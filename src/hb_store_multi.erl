%%% @doc A store implementation that wraps many other stores and dispatches
%%% operations to them in parallel. It can be configured to wait for a certain
%%% number of results before returning, or to return as soon as possible.
%%% Expects a store options message of the following form:
%%%      /stores/1..n: Sub-store definition messages.
%%%      /confirmations: Number of confirmations to require for write operations.
-module(hb_store_multi).
-behaviour(hb_store).
-export([start/1, stop/1, reset/1, scope/0, scope/1]).
-export([read/2, type/2, list/2, match/2]).
-export([write/3, make_group/2, make_link/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Initialization and teardown functions.

%% @doc Return the scope of the stores: Use the `scope' configuration if present,
%% otherwise default to `local'.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().
scope() -> local.

%% @doc Find (causing a spawn and caching of the instance data) each store.
start(StoreOpts) ->
    store_with_workers(StoreOpts).

%% @doc Stop each store and its worker process.
stop(#{ <<"stores">> := Stores }) ->
    operation(
        length(Stores),
        Stores,
        fun(XOpts) -> hb_store:stop(XOpts) end,
        []
    ),
    lists:foreach(
        fun(#{ <<"worker">> := Worker }) -> Worker ! stop end,
        Stores
    ).

%% @doc Reset each store.
reset(#{ <<"stores">> := Stores }) ->
    operation(
        length(Stores),
        Stores,
        fun(XOpts) -> hb_store:reset(XOpts) end,
        []
    ).

%%% Read operations.

%% @doc Read a key from the stores. Return the first successful result.
read(#{ <<"stores">> := Stores }, Key) ->
    case operation(1, Stores, fun(XOpts) -> hb_store:read(XOpts, Key) end, [Key]) of
        [Res] -> Res;
        _ -> not_found
    end.

%% @doc List the keys in the stores. Return the first successful result.
list(#{ <<"stores">> := Stores }, Key) ->
    case operation(1, Stores, fun(XOpts) -> hb_store:list(XOpts, Key) end, [Key]) of
        [Res] -> Res;
        _ -> not_found
    end.

%% @doc Type a key in the stores. Return the first successful result.
type(#{ <<"stores">> := Stores }, Key) ->
    case operation(1, Stores, fun(XOpts) -> hb_store:type(XOpts, Key) end, [Key]) of
        [Res] -> Res;
        _ -> not_found
    end.

%% @doc Match a key in the stores. Return the first successful result.
match(#{ <<"stores">> := Stores }, Match) ->
    MatchRes = 
        operation(
            1,
            Stores,
            fun(XOpts) -> hb_store:match(XOpts, Match) end,
            [Match]
        ),
    case MatchRes of
        [Res] -> Res;
        _ -> not_found
    end.

%%% Write operations.

%% @doc Calculate the number of confirmations to wait for on write operations.
confirmations(#{ <<"confirmations">> := Confirmations }) -> Confirmations;
confirmations(#{ <<"stores">> := Stores }) -> length(Stores).

%% @doc Write a key to the stores. By default writes to all stores, but can be
%% configured to return after only a count of `write-confirmations`, as necessary.
write(StoreOpts = #{ <<"stores">> := Stores }, Key, Value) ->
    Res = 
        operation(
            confirmations(StoreOpts),
            Stores,
            fun(XOpts) -> hb_store:write(XOpts, Key, Value) end,
            [Key, Value]
        ),
    case Res of
        {error, not_enough_results} -> not_found;
        _ -> ok
    end.

%% @doc Make a link in the stores. By default makes a link in all stores, but
%% consults the `write-confirmations' configuration to determine how many stores
%% as with `write/2`.
make_link(StoreOpts = #{ <<"stores">> := Stores }, Existing, New) ->
    Res =
        operation(
            confirmations(StoreOpts),
            Stores,
            fun(XOpts) -> hb_store:make_link(XOpts, Existing, New) end,
            [Existing, New]
        ),
    case Res of
        {error, not_enough_results} -> not_found;
        _ -> ok
    end.

%%% Group operations.

%% @doc Make a group in the stores. By default makes a group in all stores, but
%% consults the `write-confirmations' configuration to determine how many stores
%% as with `write/2`.
make_group(StoreOpts = #{ <<"stores">> := Stores }, Path) ->
    Res = operation(
        confirmations(StoreOpts),
        Stores,
        fun(XOpts) -> hb_store:make_group(XOpts, Path) end,
        [Path]
    ),
    case Res of
        {error, not_enough_results} -> not_found;
        _ -> ok
    end.

%%% Worker operations.

%% @doc Start a worker process for each store and return the updated store options.
store_with_workers(StoreOpts = #{ <<"stores">> := Stores }) ->
    StoreOpts#{
        <<"stores">> :=
            lists:map(
                fun(Store) -> Store#{ <<"worker">> := start_worker(Store) } end,
                Stores
            )
    }.

%% @doc Create a new worker process for the given store options.
start_worker(StoreOpts) ->
    spawn(
        fun() ->
            % Trigger a `find' of the store in the background on the process to
            % populate its process dictionary with the store's environment.
            hb_store:find(StoreOpts),
            % Start the server loop for this worker.
            server(StoreOpts)
        end
    ).

%% @doc Dispatch an operation across all of the stores, then return the results.
operation(Required, Stores, Function, Args) ->
    collect(
        Required,
        lists:map(
            fun(Store) -> dispatch(Store, Function, Args) end,
            Stores
        )
    ).

%% @doc Dispatch an operation to a specific worker process, returning the ref
%% that can be used to collect the result.
dispatch(#{ <<"worker">> := Worker }, Function, Args) ->
    dispatch(Worker, Function, Args);
dispatch(Worker, Function, Args) ->
    Ref = make_ref(),
    Caller = self(),
    Worker ! {operation, Ref, Caller, Function, Args},
    {Worker, {waiting, Ref}}.

%% @doc Collect result messages from worker processes, cancelling operations
%% that are no longer needed.
collect(Required, PIDRefs) when is_list(PIDRefs) ->
    collect(Required, maps:from_list(PIDRefs));
collect(0, PIDRefs) ->
    % Cancel all remaining operations and return the result values.
    maps:values(
        maps:filtermap(
            fun(PID, {waiting, Ref}) -> cancel(PID, Ref), false;
               (_PID, Res) -> {true, Res}
            end,
            PIDRefs
        )
    );
collect(Count, Refs) when Count > map_size(Refs) ->
    % Threre are more results still to gather than remaining store references.
    % Cancel the remaining operations and return an error.
    maps:map(
        fun(PID, {waiting, Ref}) -> cancel(PID, Ref);
           (_PID, _Res) -> ok
        end,
        Refs
    ),
    {error, not_enough_results};
collect(Count, Refs) ->
    receive
        {result, Ref, Res} when is_map_key(Ref, Refs) ->
            % Add new `ok' or `{ok, Res}' to the results, but remove erroring
            % store references.
            case Res of
                ok -> collect(Count - 1, maps:put(Ref, ok, Refs));
                {ok, Res} -> collect(Count - 1, maps:put(Ref, {ok, Res}, Refs));
                _ -> collect(Count, maps:remove(Ref, Refs))
            end
    end.

%% @doc Cancel an operation on a worker process.
cancel(PID, Ref) -> PID ! {cancel, Ref}.

%% @doc Server loop for a worker process. Waits for operations to perform,
%% checks that they have not been cancelled before performing them, and sends
%% the result back to the caller. Terminates on `stop' message.
server(StoreOpts) ->
    receive
        stop -> ok;
        {operation, Ref, Caller, Function, Args} ->
            receive {cancel, Ref} -> server(StoreOpts)
            after 0 ->
                Caller ! {result, Ref, apply(Function, [StoreOpts | Args])},
                server(StoreOpts)
            end
    end.