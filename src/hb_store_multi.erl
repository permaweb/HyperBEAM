%%% @doc A store implementation that wraps many other stores and dispatches
%%% operations to them in parallel. It can be configured to wait for a certain
%%% number of results before returning, or to return as soon as possible.
%%% 
%%% Expects a store options message of the following form:
%%%      /stores/1..n: Sub-store definition messages.
%%%      /confirmations: Number of confirmations to require for write operations.
%%%      /workers-per-store: Number of worker processes to spawn for each store
%%%                          (default: 3). Work is distributed evenly across each.
%%% 
%%% Each sub-store may additionally specify a specific number of store workers
%%% to spawn, overriding the 'global' store configuration for that individual
%%% case. This parameter can be specified in the store's own configuration using
%%% the `workers-per-store' key.
-module(hb_store_multi).
-behaviour(hb_store).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, type/3, list/3, match/3]).
-export([write/3, group/3, link/3]).
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_STORE_WORKERS, 3).

%%% Initialization and teardown functions.

%% @doc Return the scope of the stores: Use the `scope' configuration if present,
%% otherwise default to `local'.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_) -> scope().
scope() -> local.

%% @doc Find (causing a spawn and caching of the instance data) each store.
start(StoreOpts, _Req, _Opts) ->
    {ok, store_with_workers(StoreOpts)}.

%% @doc Stop each store and its worker process.
stop(StoreOpts, _Req, _Opts) ->
    #{ <<"stores">> := Stores } = hb_store:find(StoreOpts),
    operation(
        length(Stores),
        Stores,
        fun(XOpts, XReq, XNodeOpts) -> hb_store:stop(XOpts, XReq, XNodeOpts) end,
        [#{}, StoreOpts]
    ),
    lists:foreach(
        fun(#{ <<"workers">> := Workers }) ->
            lists:foreach(fun(Worker) -> Worker ! stop end, Workers)
        end,
        Stores
    ),
    ok.

%% @doc Reset each store.
reset(StoreOpts, _Req, _Opts) ->
    #{ <<"stores">> := Stores } = hb_store:find(StoreOpts),
    operation(
        length(Stores),
        Stores,
        fun(XOpts, XReq, XNodeOpts) -> hb_store:reset(XOpts, XReq, XNodeOpts) end,
        [#{}, StoreOpts]
    ),
    ok.

%%% Read operations.

%% @doc Read a key from the stores. Return the first successful result.
read(StoreOpts, Req, NodeOpts) ->
    #{ <<"stores">> := Stores } = hb_store:find(StoreOpts),
    case
        operation(
            1,
            Stores,
            fun(XOpts, XReq, XNodeOpts) -> hb_store:read(XOpts, XReq, XNodeOpts) end,
            [Req, NodeOpts]
        )
    of
        [Res] -> Res;
        _ -> {error, not_found}
    end.

%% @doc List the keys in the stores. Return the first successful result.
list(StoreOpts, Req, NodeOpts) ->
    #{ <<"stores">> := Stores } = hb_store:find(StoreOpts),
    case
        operation(
            1,
            Stores,
            fun(XOpts, XReq, XNodeOpts) -> hb_store:list(XOpts, XReq, XNodeOpts) end,
            [Req, NodeOpts]
        )
    of
        [Res] -> Res;
        _ -> {error, not_found}
    end.

%% @doc Type a key in the stores. Return the first successful result.
type(StoreOpts, Req, NodeOpts) ->
    #{ <<"stores">> := Stores } = hb_store:find(StoreOpts),
    case
        operation(
            1,
            Stores,
            fun(XOpts, XReq, XNodeOpts) -> hb_store:type(XOpts, XReq, XNodeOpts) end,
            [Req, NodeOpts]
        )
    of
        [Res] -> Res;
        _ -> {error, not_found}
    end.

%% @doc Match a key in the stores. Return the first successful result.
match(StoreOpts, Match, NodeOpts) ->
    #{ <<"stores">> := Stores } = hb_store:find(StoreOpts),
    MatchRes = 
        operation(
            1,
            Stores,
            fun(XOpts, XMatch, XNodeOpts) -> hb_store:match(XOpts, XMatch, XNodeOpts) end,
            [Match, NodeOpts]
        ),
    case MatchRes of
        [Res] -> Res;
        _ -> {error, not_found}
    end.

%%% Write operations.

%% @doc Calculate the number of confirmations to wait for on write operations.
confirmations(#{ <<"confirmations">> := Confirmations }) -> Confirmations;
confirmations(#{ <<"stores">> := Stores }) -> length(Stores).

%% @doc Write a key to the stores. By default writes to all stores, but can be
%% configured to return after only a count of `write-confirmations`, as necessary.
write(StoreOpts, Req, NodeOpts) ->
    StoreOptsWithWorkers = hb_store:find(StoreOpts),
    #{ <<"stores">> := Stores } = StoreOptsWithWorkers,
    Res = 
        operation(
            confirmations(StoreOptsWithWorkers),
            Stores,
            fun(XOpts, XReq, XNodeOpts) -> hb_store:write(XOpts, XReq, XNodeOpts) end,
            [Req, NodeOpts]
        ),
    case Res of
        {error, not_enough_results} -> {error, not_found};
        [_ | _] -> ok;
        _ -> {error, not_found}
    end.

%% @doc Make a link in the stores. By default makes a link in all stores, but
%% consults the `write-confirmations' configuration to determine how many stores
%% as with `write/2`.
link(StoreOpts, Req, NodeOpts) ->
    StoreOptsWithWorkers = hb_store:find(StoreOpts),
    #{ <<"stores">> := Stores } = StoreOptsWithWorkers,
    Res =
        operation(
            confirmations(StoreOptsWithWorkers),
            Stores,
            fun(XOpts, XReq, XNodeOpts) ->
                hb_store:link(XOpts, XReq, XNodeOpts)
            end,
            [Req, NodeOpts]
        ),
    case Res of
        {error, not_enough_results} -> {error, not_found};
        [_ | _] -> ok;
        _ -> {error, not_found}
    end.

%%% Group operations.

%% @doc Make a group in the stores. By default makes a group in all stores, but
%% consults the `write-confirmations' configuration to determine how many stores
%% as with `write/2`.
group(StoreOpts, Req, NodeOpts) ->
    StoreOptsWithWorkers = hb_store:find(StoreOpts),
    #{ <<"stores">> := Stores } = StoreOptsWithWorkers,
    Res = operation(
        confirmations(StoreOptsWithWorkers),
        Stores,
        fun(XOpts, XReq, XNodeOpts) -> hb_store:group(XOpts, XReq, XNodeOpts) end,
        [Req, NodeOpts]
    ),
    case Res of
        {error, not_enough_results} -> {error, not_found};
        [_ | _] -> ok;
        _ -> {error, not_found}
    end.

%%% Worker operations.

%% @doc Start a worker process for each store and return the updated store options.
%% The number of workers per store is controlled by the `num-workers' key in
%% the store options, or globally in the multi store with `num-workers-per-store' 
%% (default: 3).
store_with_workers(MultiStoreOpts = #{ <<"stores">> := Stores }) ->
    GlobalWorkersPerStore =
        maps:get(
            <<"workers-per-store">>,
            MultiStoreOpts,
            ?DEFAULT_STORE_WORKERS
        ),
    MultiStoreOpts#{
        <<"stores">> :=
            lists:map(
                fun(StoreOpts) ->
                    StoreNumWorkers =
                        case maps:get(
                            <<"workers-per-store">>,
                            StoreOpts,
                            undefined
                        ) of
                            undefined -> GlobalWorkersPerStore;
                            NumWorkersPerStore -> NumWorkersPerStore
                        end,
                    Workers = [start_worker(StoreOpts) || _ <- lists:seq(1, StoreNumWorkers)],
                    StoreOpts#{ <<"workers">> => Workers }
                end,
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

%% @doc Dispatch an operation to a worker process chosen at random from the
%% store's pool, returning the ref that can be used to collect the result.
dispatch(#{ <<"workers">> := Workers }, Function, Args) ->
    Worker = lists:nth(rand:uniform(length(Workers)), Workers),
    dispatch(Worker, Function, Args);
dispatch(Worker, Function, Args) ->
    Ref = make_ref(),
    Caller = self(),
    Worker ! {operation, Ref, Caller, Function, Args},
    {Ref, {waiting, Worker}}.

%% @doc Collect result messages from worker processes, cancelling operations
%% that are no longer needed.
collect(Required, RefStates) when is_list(RefStates) ->
    collect(Required, maps:from_list(RefStates));
collect(0, RefStates) ->
    % Cancel all remaining operations and return the result values.
    maps:values(
        maps:filtermap(
            fun(Ref, {waiting, Worker}) -> cancel(Worker, Ref), false;
               (_Ref, Res) -> {true, Res}
            end,
            RefStates
        )
    );
collect(Count, Refs) when Count > map_size(Refs) ->
    % Threre are more results still to gather than remaining store references.
    % Cancel the remaining operations and return an error.
    maps:foreach(
        fun(Ref, {waiting, Worker}) -> cancel(Worker, Ref);
           (_Ref, _Res) -> ok
        end,
        Refs
    ),
    {error, not_enough_results};
collect(Count, Refs) ->
    receive
        {result, Ref, Result} when is_map_key(Ref, Refs) ->
            % Add new `ok' or `{ok, Res}' to the results, but remove erroring
            % store references.
            case Result of
                ok -> collect(Count - 1, maps:put(Ref, ok, Refs));
                {ok, Res} -> collect(Count - 1, maps:put(Ref, {ok, Res}, Refs));
                {composite, _} = Composite ->
                    collect(Count - 1, maps:put(Ref, Composite, Refs));
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

%%% Tests

key_in_any_store_is_found_test() ->
    with_multi_store(
        fun(#{ multi_store := MultiStore, stores := Stores }) ->
            [_Store1, Store2, _Store3] = Stores,
            Key = <<"found-in-second-store">>,
            Value = <<"value-in-second-store">>,
            ok = hb_store:write(Store2, #{ Key => Value }, #{}),
            ?assertEqual({ok, Value}, hb_store:read(MultiStore, Key, #{}))
        end
    ).

write_meets_confirmation_threshold_test() ->
    with_multi_store(
        fun(#{ multi_store := MultiStore, stores := Stores }) ->
            StoreWithConfirmations = MultiStore#{ <<"confirmations">> => 2 },
            Key = <<"minimum-confirmations-key">>,
            Value = <<"minimum-confirmations-value">>,
            ?assertEqual(
                ok,
                hb_store:write(StoreWithConfirmations, #{ Key => Value }, #{})
            ),
            Copies = stores_with_key(Stores, Key, Value),
            ?assert(Copies >= 2),
            ?assert(Copies =< length(Stores))
        end
    ).

write_replicates_to_all_stores_by_default_test() ->
    with_multi_store(
        fun(#{ multi_store := MultiStore, stores := Stores }) ->
            Key = <<"all-stores-key">>,
            Value = <<"all-stores-value">>,
            ?assertEqual(ok, hb_store:write(MultiStore, #{ Key => Value }, #{})),
            ?assertEqual(length(Stores), stores_with_key(Stores, Key, Value))
        end
    ).

setup_multi_store() ->
    Unique = integer_to_binary(erlang:unique_integer([positive])),
    MultiStore =
        #{
            <<"store-module">> => ?MODULE,
            <<"name">> => <<"multi-store-", Unique/binary>>,
            <<"stores">> => Stores =
                [
                    hb_test_utils:test_store(hb_store_fs),
                    hb_test_utils:test_store(hb_store_fs),
                    hb_test_utils:test_store(hb_store_fs)
                ]
        },
    ok = hb_store:start(MultiStore),
    #{ multi_store => MultiStore, stores => Stores }.

cleanup_multi_store(#{ multi_store := MultiStore, stores := Stores }) ->
    ok = hb_store:stop(MultiStore),
    lists:foreach(
        fun(Store) -> ok = hb_store:reset(Store) end,
        Stores
    ).

with_multi_store(TestFun) ->
    Context = setup_multi_store(),
    try TestFun(Context)
    after cleanup_multi_store(Context)
    end.

stores_with_key(Stores, Key, Value) ->
    length(
        [
            Store
        ||
            Store <- Stores,
            hb_store:read(Store, Key, #{}) =:= {ok, Value}
        ]
    ).
