%%% @doc A simple abstraction layer for AO key value store operations.
%%% 
%%% This interface allows us to swap out the underlying store implementation(s)
%%% as desired, without changing the API that `hb_cache` employs. Additionally,
%%% it enables node operators to customize their configuration to maximize
%%% performance, data availability, and other factors.
%%% 
%%% Stores can be represented in a node's configuration as either a single 
%%% message, or a (`structured@1.0') list of store messages. If a list of stores
%%% is provided, the node will cycle through each until a viable store is found
%%% to execute the given function.
%%% 
%%% A valid store must implement a _subset_ of the following functions:
%%% ```
%%%     start/3:      Initialize the store.
%%%     stop/3:       Stop any processes (etc.) that manage the store.
%%%     reset/3:      Restore the store to its original, empty state.
%%%     scope/0:      A tag describing the 'scope' of a stores search: `in_memory',
%%%                   `local', `remote', `arweave', etc. Used in order to allow
%%%                   node operators to prioritize their stores for search.
%%%     group/3:      Create a new group of keys in the store using a request
%%%                   map of the form `#{<<"group">> => Path}`.
%%%     link/3:       Create links using a request map of the form
%%%                   `#{NewPath => ExistingPath}`.
%%%     type/3:       Return whether the value found at the given key is a
%%%                   `composite' (group) type, or a `simple' direct binary,
%%%                   using a request map of the form `#{<<"type">> => Path}`.
%%%     read/3:       Read the data at the given location, returning a binary
%%%                   if it is a `simple' value, or a message if it is a complex
%%%                   term, using a request map of the form `#{<<"read">> => Path}`.
%%%     write/3:      Write a request map of the form `#{Path => Value}`.
%%%     list/3:       For `composite' type keys, return child keys using a
%%%                   request map of the form `#{<<"list">> => Path}`.
%%%                   Composite read results may also return children as
%%%                   `{Key, Value}' pairs when the store can provide a child
%%%                   value without an additional read.
%%% '''
%%% Each function takes a `store' message first, containing an arbitrary set
%%% of its necessary configuration keys, as well as the `store-module' key which
%%% refers to the Erlang module that implements the store.
%%% 
%%% All functions must return `ok`, `{ok, Result}`, `{error, Reason}`, or
%%% `{failure, Reason}`, as appropriate. `{error, Reason}` results will lead to
%%% the store manager (this module) iterating to the next store message given by
%%% the user. `{failure, Reason}` results trigger retry logic before the next
%%% store is tried. If none of the given store messages are able to execute a
%%% requested service, the store manager will return the strongest terminal
%%% result observed, or `{error, not_found}`.
%%%
%%% A store message may additionally specify a normalization pipeline, called
%%% upon its paths and values during store invocation. These pipelines allow
%%% node operators to optimize their setups to store their keys with
%%% particular representations, while AO-Core and its devices remain agnostic
%%% to their specifics.
%%%
%%% Each store invocation progresses through the following pipeline:
%%%
%%% ```
%%%     Prefix matching/removal ->
%%%     Pre-normalization ->
%%%     Store invocation ->
%%%     Post-normalization ->
%%%     Re-prefixing (only for `resolve`) ->
%%%     Return.
%%% '''
%%%
%%% `Key`s and `Value`s are normalized individually -- `to` prior to store
%%% invocation, and `from` upon responses from store. To configure each, a
%%% store message may specify:
%%%
%%% ```
%%%     `prefix`:           Admit only `Key`s with the given prefix, skipping
%%%                         the store otherwise. The prefix is stripped from
%%%                         the `Key` ahead of store normalization and
%%%                         invocation, unless an additional `prefix-strip'
%%%                         store message key is explicitly set to `false'.
%%%     `[to|from]-key`:    An AO-Core path, resolved in `raw' mode with the
%%%                         (post-prefix handling) key as its `Base/body`
%%%                         message. The result is utilized as the key the
%%%                         store sees.
%%%     `[to|from]-value`:  An AO-Core path, resolved in `raw' mode with a
%%%                         successful result as the `Base/body`.
%%% '''
%%% Every path position of a request is processed through this pipeline. Every
%%% `Key` -- whether seen as an input (e.g. to `read`) or an output (as in
%%% `list`) use the `[to|from]-key` flow, and similarly `[to|from]-value` is
%%% used upon every value relatively as input or response. A `match` request
%%% carries no key or value and passes through untouched.
-module(hb_store).
-export([behavior_info/1]).
-export([
    start/1, start/2, start/3,
    stop/1, stop/2, stop/3,
    reset/1, reset/2, reset/3
]).
-export([filter/2, scope/2, sort/2]).
-export([
    type/2, type/3,
    read/2, read/3,
    write/2, write/3,
    list/2, list/3,
    match/2, match/3
]).
-export([
    group/2, group/3,
    link/2, link/3,
    resolve/2, resolve/3
]).
-export([find/1]).
-export([generate_test_suite/1, generate_test_suite/2, test_stores/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The number of write and read operations to perform in the benchmark.
-define(STORE_BENCH_WRITE_OPS, 100_000).
-define(STORE_BENCH_READ_OPS, 100_000).
-define(STORE_BENCH_LIST_KEYS, 100_000).
-define(STORE_BENCH_LIST_GROUP_SIZE, 10).
-define(STORE_BENCH_LIST_OPS, 20_000).
-define(BENCH_MSG_WRITE_OPS, 50_000).
-define(BENCH_MSG_READ_OPS, 50_000).
-define(BENCH_MSG_DATA_SIZE, 32).

behavior_info(callbacks) ->
    [
        {start, 3}, {stop, 3}, {reset, 3}, {group, 3}, {link, 3},
        {type, 3}, {read, 3}, {write, 3}, {list, 3}, {match, 3},
        {resolve, 3}
    ].

-define(DEFAULT_SCOPE, local).
-define(DEFAULT_RETRIES, 1).
-define(COMMON_POLICIES, [start, stop, scope]).

%% @doc Store access policies to function names.
-define(STORE_ACCESS_POLICIES, #{
    <<"read">> => [read, resolve, list, type, match] ++ ?COMMON_POLICIES,
    <<"write">> => [write, link, group, reset] ++ ?COMMON_POLICIES,
    <<"admin">> => [reset] ++ ?COMMON_POLICIES
}).

%% @doc The store message keys that describe a normalization pipeline.
-define(PIPELINE_KEYS, [
    <<"prefix">>, <<"to-key">>, <<"from-key">>, <<"to-value">>, <<"from-value">>
]).

%%% Store named terms registry functions.

%% @doc Set the instance options for a given store module and name combination.
set(StoreOpts, InstanceTerm) ->
    Mod = maps:get(<<"store-module">>, StoreOpts),
    set(
        Mod,
        maps:get(<<"name">>, StoreOpts, Mod),
        InstanceTerm
    ).
set(StoreMod, Name, undefined) ->
    StoreRef = {store, StoreMod, Name},
    erlang:erase(StoreRef),
    persistent_term:erase(StoreRef);
set(StoreMod, Name, InstanceTerm) ->
    StoreRef = {store, StoreMod, Name},
    put(StoreRef, InstanceTerm),
    persistent_term:put(StoreRef, InstanceTerm),
    ok.

%% @doc Find or spawn a store instance by its store opts.
-ifdef(STORE_EVENTS).
find(StoreOpts) ->
    {Time, Result} = timer:tc(fun() -> do_find(StoreOpts) end),
    hb_event:record(<<"store_duration">>, <<"find">>, #{}, Time),
    hb_event:record(<<"store">>, <<"find">>, #{}, 1),
    Result.
-else.
find(StoreOpts) ->
    do_find(StoreOpts).
-endif.

do_find(StoreOpts = #{ <<"store-module">> := Mod }) ->
    Name = maps:get(<<"name">>, StoreOpts, Mod),
    LookupName = {store, Mod, Name},
    case get(LookupName) of
        undefined ->
            try persistent_term:get(LookupName) of
                Instance1 ->
                    EnsuredInstance = ensure_instance_alive(StoreOpts, Instance1),
                    put(LookupName, EnsuredInstance),
                    EnsuredInstance
            catch
                error:badarg -> spawn_instance(StoreOpts)
            end;
        InstanceMessage ->
            ensure_instance_alive(StoreOpts, InstanceMessage)
    end.

%% @doc Create a new instance of a store and return its term.
spawn_instance(StoreOpts = #{ <<"store-module">> := Mod }) ->
    Name = maps:get(<<"name">>, StoreOpts, Mod),
    try call_store_start(Mod, StoreOpts, #{}, StoreOpts) of
        ok -> ok;
        {ok, InstanceMessage} ->
            set(Mod, Name, InstanceMessage),
            InstanceMessage;
        {error, Reason} ->
            ?event(error, {store_start_failed, {Mod, Name, Reason}}),
            throw({store_start_failed, {Mod, Name, Reason}});
        {failure, Reason} ->
            ?event(error, {store_start_failed, {Mod, Name, Reason}}),
            throw({store_start_failed, {Mod, Name, Reason}})
    catch
        error:undef ->
        ok
    end.

%% @doc Handle a found instance message. If it contains a PID, we check if it
%% is alive. If it does not, we return it as is.
ensure_instance_alive(StoreOpts, InstanceMessage = #{ <<"pid">> := Pid }) ->
    case is_process_alive(Pid) of
        true -> InstanceMessage;
        false -> spawn_instance(StoreOpts)
    end;
ensure_instance_alive(_, InstanceMessage) ->
    InstanceMessage.

%%% Library wrapper implementations.

%% @doc Ensure that a store, or list of stores, have all been started.
start(StoreOrOpts) ->
    case is_store_spec(StoreOrOpts) of
        true -> start(StoreOrOpts, #{});
        false -> start(hb_opts:get(store, [], StoreOrOpts), #{}, StoreOrOpts)
    end.
start(Store, Opts) ->
    start(Store, #{}, Opts).
start(StoreOpts, Req, Opts) when not is_list(StoreOpts) ->
    start([StoreOpts], Req, Opts);
start([], _Req, _Opts) ->
    ok;
start([StoreOpts | Rest], Req, Opts) ->
    case start_one(StoreOpts, Req, Opts) of
        ok -> start(Rest, Req, Opts);
        {ok, _} -> start(Rest, Req, Opts);
        {error, not_found} -> start(Rest, Req, Opts);
        Result -> Result
    end.

stop(StoreOrOpts) ->
    case is_store_spec(StoreOrOpts) of
        true -> stop(StoreOrOpts, #{});
        false -> stop(hb_opts:get(store, [], StoreOrOpts), #{}, StoreOrOpts)
    end.
stop(Store, Opts) ->
    stop(Store, #{}, Opts).
stop(Stores, Req, Opts) ->
    admin_call(Stores, stop, Req, Opts).

%% @doc Takes a store object and a filter function or match spec, returning a
%% new store object with only the modules that match the filter. The filter
%% function takes 2 arguments: the scope and the options. It calls the store's
%% scope function to get the scope of the module.
filter(Module, Filter) when not is_list(Module) ->
    filter([Module], Filter);
filter(Modules, Filter) ->
    lists:filter(
        fun(Store) ->
            try Filter(get_store_scope(Store), Store)
            catch _:_ -> false
            end
        end,
        Modules
    ).

%% @doc Limit the store scope to only a specific (set of) option(s).
%% Takes either an Opts message or store, and either a single scope or a list
%% of scopes.
scope(Opts, Scope) when is_map(Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        no_viable_store -> Opts;
        Store when is_list(Store) ->
            % Store is already a list, apply scope normally
            Opts#{ <<"store">> => scope(Store, Scope) };
        Store when is_map(Store) ->
            % Check if Store already has a nested 'store' key
            case maps:find(store, Store) of
                {ok, _NestedStores} ->
                    % Already has nested structure, return as-is
                    Opts;
                error ->
                    % Single store map, wrap in list before scoping
                    % This ensures consistent behavior
                    Opts#{ <<"store">> => scope([Store], Scope) }
            end
    end;
scope(Store, Scope) ->
    filter(
        Store,
        fun(StoreScope, _) ->
            StoreScope == Scope orelse
                (is_list(Scope) andalso lists:member(StoreScope, Scope))
        end
    ).

%% @doc Ask a store for its own scope. If it doesn't have one, return the
%% default scope (local).
get_store_scope(Store) ->
    case get_store_scope_result(Store) of
        not_found -> ?DEFAULT_SCOPE;
        Scope -> Scope
    end.

%% @doc Order a store by a preference of its scopes. This is useful for making
%% sure that faster (or perhaps cheaper) stores are used first. If a list is
%% provided, it will be used as a preference order. If a map is provided,
%% scopes will be ordered by the scores in the map. Any unknown scopes will
%% default to a score of 0.
sort(Stores, PreferenceOrder) when is_list(PreferenceOrder) ->
    sort(
        Stores,
        hb_maps:from_list(
            [
                {Scope, -Index}
            ||
                {Scope, Index} <-
                    lists:zip(
                        PreferenceOrder,
                        lists:seq(1, length(PreferenceOrder))
                    )
            ]
        )
    );
sort(Stores, ScoreMap) ->
    lists:sort(
        fun(Store1, Store2) ->
            hb_maps:get(get_store_scope(Store1), ScoreMap, 0) >
                hb_maps:get(get_store_scope(Store2), ScoreMap, 0)
        end,
        Stores
    ).

%%% The store interface that modules should implement.

%% @doc Read a key from the store.
read(Path, Opts) ->
    read(hb_opts:get(store, [], Opts), Path, Opts).
read(Modules, Req = #{ <<"read">> := _ }, Opts) ->
    call_function(Modules, read, [Req, Opts]);
read(Modules, Path, Opts) ->
    read(Modules, #{ <<"read">> => hb_path:to_binary(Path) }, Opts).

%% @doc Write a key with a value to the store.
write(Req, Opts) ->
    write(hb_opts:get(store, [], Opts), Req, Opts).
write(Modules, Req, Opts) ->
    call_function(Modules, write, [Req, Opts]).

%% @doc Make a group in the store. A group can be seen as a namespace or
%% 'directory' in a filesystem.
group(Path, Opts) ->
    group(hb_opts:get(store, [], Opts), Path, Opts).
group(Modules, Req = #{ <<"group">> := _ }, Opts) ->
    call_function(Modules, group, [Req, Opts]);
group(Modules, Path, Opts) ->
    group(Modules, #{ <<"group">> => hb_path:to_binary(Path) }, Opts).

%% @doc Make a link from one path to another in the store.
link(Req, Opts) ->
    link(hb_opts:get(store, [], Opts), Req, Opts).
link(Modules, Req, Opts) ->
    call_function(Modules, link, [Req, Opts]).

%% @doc Delete all of the keys in a store. Should be used with extreme
%% caution. Lost data can lose money in many/most of hyperbeam's use cases.
reset(StoreOrOpts) ->
    case is_store_spec(StoreOrOpts) of
        true -> reset(StoreOrOpts, #{});
        false -> reset(hb_opts:get(store, [], StoreOrOpts), #{}, StoreOrOpts)
    end.
reset(Store, Opts) ->
    reset(Store, #{}, Opts).
reset(Stores, Req, Opts) ->
    admin_call(Stores, reset, Req, Opts).

%% @doc Get the type of element of a given path in the store. This can be
%% a performance killer if the store is remote etc. Use only when necessary.
type(Path, Opts) ->
    type(hb_opts:get(store, [], Opts), Path, Opts).
type(Modules, Req = #{ <<"type">> := _ }, Opts) ->
    call_function(Modules, type, [Req, Opts]);
type(Modules, Path, Opts) ->
    type(Modules, #{ <<"type">> => hb_path:to_binary(Path) }, Opts).

%% @doc Follow links through the store to resolve a path to its ultimate target.
resolve(Path, Opts) ->
    resolve(hb_opts:get(store, [], Opts), Path, Opts).
resolve(Modules, Req = #{ <<"resolve">> := _ }, Opts) ->
    call_function(Modules, resolve, [Req, Opts]);
resolve(Modules, Path, Opts) ->
    resolve(Modules, #{ <<"resolve">> => hb_path:to_binary(Path) }, Opts).

%% @doc List the keys in a group in the store. Use only in debugging.
%% The hyperbeam model assumes that stores are built as efficient hash-based
%% structures, so this is likely to be very slow for most stores.
list(Path, Opts) ->
    list(hb_opts:get(store, [], Opts), Path, Opts).
list(Modules, Req = #{ <<"list">> := _ }, Opts) ->
    call_function(Modules, list, [Req, Opts]);
list(Modules, Path, Opts) ->
    list(Modules, #{ <<"list">> => hb_path:to_binary(Path) }, Opts).

%% @doc Match a series of keys and values against the store. Returns 
%% `{ok, Matches}' if the match is successful, or `not_found' if there are no
%% messages in the store that feature all of the given key-value pairs. `Matches'
%% is given as a list of IDs.
match(Match, Opts) ->
    match(hb_opts:get(store, [], Opts), Match, Opts).
match(Modules, Match, Opts) ->
    call_function(Modules, match, [Match, Opts]).

%% @doc Call a function on the first store module that succeeds. Returns its
%% result, or `not_found` if none of the stores succeed. If `TIME_CALLS` is set,
%% this function will also time the call and record the appropriate event
%% counter.
-ifdef(STORE_EVENTS).
call_function(X, Function, Args) ->
    {Time, Result} =
        timer:tc(fun() -> do_call_function(X, Function, Args, undefined, undefined) end),
    ?event(store_events,
        {store_call,
            {function, Function},
            {args, Args},
            {primary_store,
                case X of
                    [PrimaryStore | _] -> PrimaryStore;
                    _ -> X
                end
            },
            {time, Time},
            {result, Result}
        }
    ),
    hb_event:record(<<"store_duration">>, hb_util:bin(Function), #{}, Time),
    hb_event:record(<<"store">>, hb_util:bin(Function), #{}, 1),
    Result.
-else.
call_function(X, Function, Args) ->
    do_call_function(X, Function, Args, undefined, undefined).
-endif.
do_call_function(X, Function, Args, Failure, Error) when not is_list(X) ->
    do_call_function([X], Function, Args, Failure, Error);
do_call_function([], _Function, _Args, Failure, Error) ->
    terminal_result(Failure, Error);
do_call_function([Store = #{<<"access">> := Access} | Rest], Function, Args, Failure, Error) ->
    % If the store has an access controls, check if the function is allowed from
    % the stated policies.
    IsAdmissible =
        lists:any(
            fun(Group) ->
                lists:any(
                    fun(F) -> F == Function end,
                    maps:get(Group, ?STORE_ACCESS_POLICIES, [])
                )
            end,
            Access
        ),
    case IsAdmissible of
        true ->
            do_call_function(
                [maps:remove(<<"access">>, Store) | Rest],
                Function,
                Args,
                Failure,
                Error
            );
        false ->
            do_call_function(Rest, Function, Args, Failure, Error)
    end;
do_call_function([Store = #{<<"store-module">> := Mod} | Rest], Function, Args, Failure, Error) ->
    % Attempt to invoke the function. If it fails, try the next store.
    try invoke(Mod, Store, Function, Args) of
        ok ->
            ok;
        {ok, _} = Result ->
            Result;
        {composite, _} = Result ->
            Result;
        {failure, _} = Result ->
            do_call_function(
                Rest,
                Function,
                Args,
                strongest_failure(Failure, Result),
                Error
            );
        {error, not_found} ->
            do_call_function(Rest, Function, Args, Failure, Error);
        {error, _} = Result ->
            do_call_function(
                Rest,
                Function,
                Args,
                Failure,
                strongest_error(Error, Result)
            );
        Other ->
            normalize_result(Other)
    catch _:_:_ ->
        do_call_function(Rest, Function, Args, Failure, Error)
    end.

%% @doc Invoke a store function through the normalization pipeline its store
%% message describes: the request's keys and values are normalized to the
%% store, and its answer's from it. A request the store does not admit is
%% `not_found' for that store alone, and an answer that fails its
%% normalization is an error: both move the manager on to the next store.
invoke(Mod, Store, Function, Args = [Req, Opts]) ->
    case pipelined(Store) of
        false ->
            apply_store_function(Mod, Store, Function, Args);
        true ->
            maybe
                {ok, NormReq} ?= to_store(Store, Function, Req, Opts),
                from_store(
                    Store,
                    Function,
                    apply_store_function(Mod, Store, Function, [NormReq, Opts]),
                    Opts
                )
            end
    end.

%% @doc Whether a store message describes a normalization pipeline.
pipelined(Store) ->
    lists:any(fun(Key) -> is_map_key(Key, Store) end, ?PIPELINE_KEYS).

%% @doc Normalize a request's keys and values to the store: the path a
%% `read', `list', `type', `group' or `resolve' names, every key and value
%% of a `write', and both paths of a `link'. Every other request carries no
%% key or value and passes through.
to_store(Store, write, Req, Opts) ->
    to_pairs(Store, fun to_value/3, Req, Opts);
to_store(Store, link, Req, Opts) ->
    to_pairs(Store, fun to_key/3, Req, Opts);
to_store(Store, Function, Req, Opts)
        when Function =:= read orelse Function =:= list orelse
            Function =:= type orelse Function =:= group orelse
            Function =:= resolve ->
    OpKey = hb_util:bin(Function),
    maybe
        {ok, Path} ?= to_key(Store, maps:get(OpKey, Req), Opts),
        {ok, Req#{ OpKey => Path }}
    end;
to_store(_Store, _Function, Req, _Opts) ->
    {ok, Req}.

%% @doc Normalize every pair of a request to the store: its keys as paths,
%% and its values through the given function -- as paths for a `link', as
%% values for a `write'.
to_pairs(Store, Values, Req, Opts) ->
    maybe
        {ok, Pairs} ?=
            each(
                fun({Path, Value}) ->
                    maybe
                        {ok, NormPath} ?= to_key(Store, Path, Opts),
                        {ok, NormValue} ?= Values(Store, Value, Opts),
                        {ok, {NormPath, NormValue}}
                    end
                end,
                maps:to_list(Req)
            ),
        {ok, maps:from_list(Pairs)}
    end.

%% @doc Normalize a path to the store: admit it by the store's `prefix',
%% stripped from it unless `prefix-strip' is `false', then through the
%% store's `to-key'. A path without the prefix is `not_found' for the store.
to_key(Store, Path, Opts) ->
    Prefix = maps:get(<<"prefix">>, Store, <<>>),
    Size = byte_size(Prefix),
    case Path of
        <<Prefix:Size/binary, Rest/binary>> ->
            Admitted =
                case strips(Store) of
                    true -> Rest;
                    false -> Path
                end,
            normalize(<<"to-key">>, Store, Admitted, Opts);
        _ ->
            {error, not_found}
    end.

%% @doc Normalize a value to the store through its `to-value'.
to_value(Store, Value, Opts) ->
    normalize(<<"to-value">>, Store, Value, Opts).

%% @doc Whether the store's prefix is stripped from the paths it admits.
strips(Store) ->
    hb_util:bool(maps:get(<<"prefix-strip">>, Store, true)).

%% @doc Normalize a store's answer from it: a read's value, the children a
%% `list' or a composite read enumerates, and a resolved path, which regains
%% a stripped prefix. Every other answer passes through.
from_store(Store, read, {ok, Value}, Opts) ->
    normalize(<<"from-value">>, Store, Value, Opts);
from_store(Store, read, {composite, Children}, Opts) ->
    maybe
        {ok, Norm} ?= from_children(Store, Children, Opts),
        {composite, Norm}
    end;
from_store(Store, list, {ok, Children}, Opts) ->
    from_children(Store, Children, Opts);
from_store(Store, resolve, {ok, Path}, Opts) ->
    Prefix = maps:get(<<"prefix">>, Store, <<>>),
    maybe
        {ok, Norm} ?= normalize(<<"from-key">>, Store, Path, Opts),
        {ok,
            case strips(Store) of
                true -> <<Prefix/binary, Norm/binary>>;
                false -> Norm
            end}
    end;
from_store(_Store, _Function, Result, _Opts) ->
    Result.

%% @doc Normalize the children a store enumerates from it.
from_children(Store, Children, Opts) ->
    each(fun(Child) -> from_child(Store, Child, Opts) end, Children).

%% @doc Normalize a child from the store: its key through `from-key', and
%% the value of a child given as a pair through `from-value'.
from_child(Store, {Key, Value}, Opts) ->
    maybe
        {ok, NormKey} ?= normalize(<<"from-key">>, Store, Key, Opts),
        {ok, NormValue} ?= normalize(<<"from-value">>, Store, Value, Opts),
        {ok, {NormKey, NormValue}}
    end;
from_child(Store, Key, Opts) ->
    normalize(<<"from-key">>, Store, Key, Opts).

%% @doc Normalize each term of a list in order, stopping at the first error.
each(_Fun, []) -> {ok, []};
each(Fun, [Term | Rest]) ->
    maybe
        {ok, Norm} ?= Fun(Term),
        {ok, Others} ?= each(Fun, Rest),
        {ok, [Norm | Others]}
    end.

%% @doc Resolve one of the store message's normalization paths over a term
%% in `raw' mode, with the term as the `body' of the head of the path: a
%% bare `body' key would be merged into every stage of the path, clobbering
%% the values piped between them. A store message without the path leaves
%% the term as it is. The resolution sees only the stores that carry no
%% pipeline of their own, so any loads it performs -- remote devices among
%% them -- can never recurse into another normalization.
normalize(Setting, Store, Term, Opts) ->
    case maps:get(Setting, Store, []) of
        [] -> {ok, Term};
        Path ->
            hb_ao:raw(
                #{ <<"path">> => Path, <<"0.body">> => Term },
                Opts#{
                    <<"store">> => [ S || S <- stores(Opts), not pipelined(S) ]
                }
            )
    end.

%% @doc The stores of the node's options as a list.
stores(Opts) ->
    case hb_opts:get(store, [], Opts) of
        Stores when is_list(Stores) -> Stores;
        Store -> [Store]
    end.

%% @doc Apply a store function, checking if the store returns a retry request or
%% errors. If it does, attempt to start the store again and retry, up to the
%% given maximum number of times.
apply_store_function(Mod, Store, Function, Args) ->
    MaxAttempts = maps:get(<<"max-retries">>, Store, ?DEFAULT_RETRIES) + 1,
    apply_store_function(Mod, Store, Function, Args, MaxAttempts).
apply_store_function(_Mod, _Store, _Function, _Args, 0) ->
    % Too many attempts have already failed. Bail.
    {error, not_found};
apply_store_function(Mod, Store, Function, Args, AttemptsRemaining) ->
    try normalize_result(apply(Mod, Function, [Store | Args])) of
        retry ->
            retry(Mod, Store, Function, Args, AttemptsRemaining, {error, not_found});
        {failure, _} = Failure when AttemptsRemaining =< 1 ->
            Failure;
        {failure, _} = Failure ->
            retry(Mod, Store, Function, Args, AttemptsRemaining, Failure);
        Other ->
            Other
    catch Class:Reason:Stacktrace ->
        ?event(store_error,
            {store_call_failed_retrying,
                {store, Store},
                {function, Function},
                {args, Args},
                {class, Class},
                {reason, Reason},
                {stacktrace, {trace, Stacktrace}}
            }
        ),
        retry(Mod, Store, Function, Args, AttemptsRemaining, {error, not_found})
    end.

%% @doc Stop and start the store, then retry.
retry(_Mod, _Store, _Function, _Args, AttemptsRemaining, Result)
        when AttemptsRemaining =< 1 ->
    Result;
retry(Mod, Store, Function, Args, AttemptsRemaining, _Result) ->
    % Attempt to stop the store and start it again, then retry.
    try call_store_stop(Mod, Store) catch _:_ -> ignore_errors end,
    set(Store, undefined),
    find(Store),
    apply_store_function(Mod, Store, Function, Args, AttemptsRemaining - 1).

admin_call(Stores, Function, Req, Opts) when not is_list(Stores) ->
    admin_call([Stores], Function, Req, Opts);
admin_call([], _Function, _Req, _Opts) ->
    ok;
admin_call([Store | Rest], Function, Req, Opts) ->
    case call_function([Store], Function, [Req, Opts]) of
        ok ->
            admin_post_process(Function, Store),
            admin_call(Rest, Function, Req, Opts);
        {ok, _} ->
            admin_post_process(Function, Store),
            admin_call(Rest, Function, Req, Opts);
        {error, not_found} ->
            admin_call(Rest, Function, Req, Opts);
        {error, _} = Error ->
            Error;
        {failure, _} = Failure ->
            Failure;
        {composite, _} = Composite ->
            Composite
    end.

admin_post_process(stop, Store) ->
    set(Store, undefined);
admin_post_process(reset, Store) ->
    set(Store, undefined);
admin_post_process(_, _Store) ->
    ok.

start_one(Store = #{ <<"store-module">> := Mod }, Req, Opts) ->
    case is_admissible(Store, start) of
        false ->
            {error, not_found};
        true ->
            Name = maps:get(<<"name">>, Store, Mod),
            try call_store_start(Mod, Store, Req, Opts) of
                ok ->
                    ok;
                {ok, InstanceMessage} ->
                    set(Mod, Name, InstanceMessage),
                    ok;
                Other ->
                    normalize_result(Other)
            catch Class:Reason:Stacktrace ->
                ?event(store_error,
                    {store_start_failed,
                        {store, Store},
                        {class, Class},
                        {reason, Reason},
                        {stacktrace, {trace, Stacktrace}}
                    }
                ),
                {failure, {Class, Reason, Stacktrace}}
            end
    end.

call_store_start(Mod, Store, Req, Opts) ->
    %% function_exported doesn't load the module. We need to call ensure_loaded
    %% here since is the first time we call a function to load the module.
    code:ensure_loaded(Mod),
    case erlang:function_exported(Mod, start, 3) of
        true -> Mod:start(Store, Req, Opts);
        false -> Mod:start(Store)
    end.

call_store_stop(Mod, Store) ->
    case erlang:function_exported(Mod, stop, 3) of
        true -> Mod:stop(Store, #{}, Store);
        false -> Mod:stop(Store)
    end.

is_store_spec(#{ <<"store-module">> := _ }) -> true;
is_store_spec([#{ <<"store-module">> := _ } | _]) -> true;
is_store_spec([]) -> true;
is_store_spec(_) -> false.

is_admissible(#{ <<"access">> := Access }, Function) ->
    lists:any(
        fun(Group) ->
            lists:any(
                fun(F) -> F == Function end,
                maps:get(Group, ?STORE_ACCESS_POLICIES, [])
            )
        end,
        Access
    );
is_admissible(_, _) ->
    true.

get_store_scope_result(Store = #{ <<"scope">> := Scope }) ->
    normalize_scope(Scope);
get_store_scope_result(Store = #{ <<"store-module">> := Mod }) ->
    try
        code:ensure_loaded(Mod),
        case erlang:function_exported(Mod, scope, 3) of
            true -> normalize_scope(Mod:scope(Store, #{}, Store));
            false ->
                case erlang:function_exported(Mod, scope, 1) of
                    true -> normalize_scope(Mod:scope(Store));
                    false ->
                        case erlang:function_exported(Mod, scope, 0) of
                            true -> normalize_scope(Mod:scope());
                            false -> not_found
                        end
                end
        end
    catch _:_ ->
        not_found
    end.

normalize_scope({ok, Scope}) -> Scope;
normalize_scope(Scope) -> Scope.

normalize_result({ok, Result}) when Result =:= ok -> ok;
normalize_result({ok, _} = Result) -> Result;
normalize_result({error, _} = Result) -> Result;
normalize_result({failure, _} = Result) -> Result;
normalize_result({composite, _} = Result) -> Result;
normalize_result(not_found) -> {error, not_found};
normalize_result(failure) -> {failure, failure};
normalize_result(retry) -> retry;
normalize_result(ok) -> ok;
normalize_result(simple) -> {ok, simple};
normalize_result(composite) -> {ok, composite};
normalize_result(Result) -> {ok, Result}.

terminal_result(undefined, undefined) ->
    {error, not_found};
terminal_result({failure, _} = Failure, _Error) ->
    Failure;
terminal_result(undefined, {error, _} = Error) ->
    Error;
terminal_result(_Failure, {error, _} = Error) ->
    Error.

strongest_failure(undefined, Failure) -> Failure;
strongest_failure(Current, _Failure) -> Current.

strongest_error(undefined, Error) -> Error;
strongest_error(Current, _Error) -> Current.

%%% Test helpers

%% @doc Return a list of stores for testing. Additional individual functions are
%% used to generate store options for those whose drivers are not built by 
%% default into all HyperBEAM distributions.
test_stores() ->
    [
        (hb_test_utils:test_store(hb_store_fs))#{
            <<"benchmark-scale">> => 0.0001
        },
        (hb_test_utils:test_store(hb_store_lmdb))#{
            <<"benchmark-scale">> => 0.5
        },
        (hb_test_utils:test_store(hb_store_volatile))#{
            <<"benchmark-scale">> => 0.001
        }
    ] ++ rocks_stores().

-ifdef(ENABLE_ROCKSDB).
rocks_stores() ->
    [
        #{
            <<"store-module">> => hb_store_rocksdb,
            <<"name">> => <<"cache-TEST/rocksdb">>
        }
    ].
-else.
rocks_stores() -> [].
-endif.

generate_test_suite(Suite) ->
    generate_test_suite(Suite, test_stores()).
generate_test_suite(Suite, Stores) ->
    hb:init(),
    lists:map(
        fun(Store = #{<<"store-module">> := Mod}) ->
            {foreach,
                fun() ->
                    hb_store:start(Store)
                end,
                fun(_) ->
                    hb_store:reset(Store)
                    % hb_store:stop(Store)
                end,
                [
                    {
                        atom_to_list(Mod) ++ ": " ++ Desc,
                        {
                            timeout,
                            60,
                            fun() ->
                                TestResult = Test(Store),
                                TestResult
                            end
                        }
                    }
                ||
                    {Desc, Test} <- Suite
                ]
            }
        end,
        Stores
    ).

%%% Tests

write_req(Key, Value) ->
    #{ hb_path:to_binary(Key) => Value }.

link_req(New, Existing) ->
    #{ hb_path:to_binary(New) => hb_path:to_binary(Existing) }.

%% @doc Test path resolution dynamics.
simple_path_resolution_test(Store) ->
    ok = write(Store, write_req(<<"test-file">>, <<"test-data">>), #{}),
    ok = link(Store, link_req(<<"test-link">>, <<"test-file">>), #{}),
    ?assertEqual({ok, <<"test-data">>}, read(Store, <<"test-link">>, #{})).

%% @doc Ensure that we can resolve links recursively.
resursive_path_resolution_test(Store) ->
    ok = write(Store, write_req(<<"test-file">>, <<"test-data">>), #{}),
    ok = link(Store, link_req(<<"test-link">>, <<"test-file">>), #{}),
    ok = link(Store, link_req(<<"test-link2">>, <<"test-link">>), #{}),
    ?assertEqual({ok, <<"test-data">>}, read(Store, <<"test-link2">>, #{})).

%% @doc Ensure that we can resolve links through a directory.
hierarchical_path_resolution_test(Store) ->
    ok = group(Store, <<"test-dir1">>, #{}),
    ok =
        write(
            Store,
            write_req([<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
            #{}
        ),
    ok = link(Store, link_req(<<"test-link">>, [<<"test-dir1">>]), #{}),
    ?assertEqual(
        {ok, <<"test-data">>},
        read(Store, [<<"test-link">>, <<"test-file">>], #{})
    ).

store_suite_test_() ->
    generate_test_suite([
        {"simple path resolution", fun simple_path_resolution_test/1},
        {"resursive path resolution", fun resursive_path_resolution_test/1},
        {"hierarchical path resolution", fun hierarchical_path_resolution_test/1}
    ]).

benchmark_suite_test_() ->
    generate_test_suite([
        {"benchmark key read write", fun benchmark_key_read_write/1},
        {"benchmark list", fun benchmark_list/1},
        {"benchmark flat message read write", fun benchmark_flat_message_read_write/1},
        {"benchmark nested message read write", fun benchmark_nested_message_read_write/1}
    ]).

%% @doc Benchmark a store. By default, we write 10,000 keys and read 10,000
%% keys. This can be altered by setting the `STORE_BENCH_WRITE_OPS' and
%% `STORE_BENCH_READ_OPS' macros. If the `benchmark-scale' key is set in the
%% store message, we use it to scale the number of operations for only that
%% store. This allows slower stores to be tested with fewer operations.
benchmark_key_read_write(Store = #{ <<"benchmark-scale">> := Scale }) ->
    benchmark_key_read_write(
        Store,
        erlang:ceil(Scale * ?STORE_BENCH_WRITE_OPS), 
        erlang:ceil(Scale * ?STORE_BENCH_READ_OPS)
    );
benchmark_key_read_write(Store) ->
    benchmark_key_read_write(Store, ?STORE_BENCH_WRITE_OPS, ?STORE_BENCH_READ_OPS).
benchmark_key_read_write(Store, WriteOps, ReadOps) ->
    start(Store),
    timer:sleep(100),
    ?event(
        {benchmarking,
            {store, Store},
            {write_ops, WriteOps},
            {read_ops, ReadOps}
        }
    ),
    % Generate random data, keys, and store requests ahead of time.
    RandomData = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Keys =
        lists:map(
            fun(N) ->
                << "key-", (integer_to_binary(N))/binary >>
            end,
            lists:seq(1, ReadOps)
        ),
    WriteReqs =
        lists:map(
            fun(Key) ->
                write_req(Key, RandomData)
            end,
            Keys
        ),
    {WriteTime, ok} =
        timer:tc(
            fun() ->
                lists:foreach(
                    fun(Req) ->
                        ok = write(Store, Req, #{})
                    end,
                    WriteReqs
                )
            end
        ),
    % Calculate write rate.
    WriteRate = erlang:round(WriteOps / (WriteTime / 1000000)),
    hb_format:eunit_print(
        "Wrote ~s records in ~p ms (~s records/s)",
        [
            hb_util:human_int(WriteOps),
            WriteTime/1000,
            hb_util:human_int(WriteRate)
        ]
    ),
    % Generate read requests ahead of time.
    ReadReqs =
        lists:map(
            fun(_) ->
                #{
                    <<"read">> =>
                        << "key-", (integer_to_binary(rand:uniform(ReadOps)))/binary >>
                }
            end,
            lists:seq(1, ReadOps)
        ),
    % Time random reads.
    {ReadTime, NotFoundCount} =
        timer:tc(
            fun() ->
                lists:foldl(
                    fun(Req, Count) ->
                        case read(Store, Req, #{}) of
                            {ok, _} -> Count;
                            _ -> Count + 1
                        end
                    end,
                    0,
                    ReadReqs
                )
            end
        ),
    % Calculate read rate.
    ReadRate = erlang:round(ReadOps / (ReadTime / 1000000)),
    hb_format:eunit_print(
        "Read ~s records in ~p ms (~s records/s)",
        [
            hb_util:human_int(ReadOps),
            ReadTime/1000,
            hb_util:human_int(ReadRate)
        ]
    ),
    ?assertEqual(0, NotFoundCount, "Written keys not found in store.").

benchmark_list(Store = #{ <<"benchmark-scale">> := Scale }) ->
    benchmark_list(
        Store,
        erlang:ceil(Scale * ?STORE_BENCH_LIST_KEYS),
        erlang:ceil(Scale * ?STORE_BENCH_LIST_OPS),
        erlang:ceil(Scale * ?STORE_BENCH_LIST_GROUP_SIZE)
    );
benchmark_list(Store) ->
    benchmark_list(
        Store,
        ?STORE_BENCH_LIST_KEYS,
        ?STORE_BENCH_LIST_OPS,
        ?STORE_BENCH_LIST_GROUP_SIZE
    ).
benchmark_list(Store, WriteOps, ListOps, GroupSize) ->
    start(Store),
    timer:sleep(100),
    ?event(
        {benchmarking,
            {store, Store},
            {keys, hb_util:human_int(WriteOps)},
            {groups, hb_util:human_int(WriteOps div GroupSize)},
            {lists, hb_util:human_int(ListOps)}
        }
    ),
    % Generate a random message to write and the keys to read ahead of time.
    Groups =
        lists:map(
            fun(_) ->
                GroupID = hb_util:human_id(crypto:strong_rand_bytes(32)),
                {
                    GroupID,
                    lists:map(
                        fun(M) ->
                            {
                                <<"key-", (integer_to_binary(M))/binary >>,
                                <<"value-", (integer_to_binary(M))/binary >>
                            }
                        end,
                        lists:seq(1, GroupSize)
                    )
                }
            end,
            lists:seq(1, GroupCount = WriteOps div GroupSize)
        ),
    hb_format:eunit_print(
        "Generated ~s groups of ~s keys",
        [
            hb_util:human_int(GroupCount),
            hb_util:human_int(GroupSize)
        ]
    ),
    {WriteTime, _} =
        timer:tc(
            fun() ->
                lists:map(
                    fun({GroupID, KeyPairs}) ->
                        ok = group(Store, GroupID, #{}),
                        lists:foreach(
                            fun({Key, Value}) ->
                                ok =
                                    write(
                                        Store,
                                        write_req(
                                            <<GroupID/binary, "/", Key/binary>>,
                                            Value
                                        ),
                                        #{}
                                    )
                            end,
                            KeyPairs
                        )
                    end,
                    Groups
                ),
                % Perform one list operation to ensure that the write queue is
                % flushed.
                {LastGroupID, _} = lists:last(Groups),
                list(Store, LastGroupID, #{})
            end
        ),
    % Print the results. Our write time is in microseconds, so we normalize it
    % to seconds.
    hb_test_utils:benchmark_print(
        <<"Wrote and flushed">>,
        <<"keys">>,
        WriteOps,
        WriteTime / 1_000_000
    ),
    % Generate groups to read ahead of time.
    ReadGroups =
        lists:map(
            fun(_) ->
                lists:nth(rand:uniform(GroupCount), Groups)
            end,
            lists:seq(1, ListOps)
        ),
    % Time random reads.
    {ReadTime, NotFoundCount} =
        timer:tc(
            fun() ->
                lists:foldl(
                    fun({GroupID, GroupKeyValues}, Count) ->
                        ExpectedKeys =
                            [ KeyInGroup || {KeyInGroup, _} <- GroupKeyValues ],
                        case list(Store, GroupID, #{}) of
                            {ok, ListedKeys} ->
                                Res =
                                    lists:all(
                                        fun({KeyInGroup, _ExpectedValue}) ->
                                            lists:member(KeyInGroup, ListedKeys)
                                        end,
                                        GroupKeyValues
                                    ),
                                case Res of
                                    true -> Count;
                                    _ ->
                                        ?event(
                                            {list_group_not_found,
                                                {group, GroupID},
                                                {received_keys, ListedKeys},
                                                {expected_keys, ExpectedKeys}
                                            }
                                        ),
                                        Count + 1
                                end;
                            _ ->
                                ?event(
                                    {list_group_not_found,
                                        {group, GroupID},
                                        {expected_keys, ExpectedKeys}
                                    }
                                ),
                                Count + 1
                        end
                    end,
                    0,
                    ReadGroups
                )
            end
        ),
    % Print the results.
    hb_test_utils:benchmark_print(
        <<"Listed">>,
        <<"groups">>,
        ListOps,
        ReadTime / 1_000_000
    ),
    ?assertEqual(0, NotFoundCount, "Groups listed in correctly.").

benchmark_flat_message_read_write(Store) ->
    benchmark_message_read_write(Store, flat).

benchmark_nested_message_read_write(Store) ->
    benchmark_message_read_write(Store, nested).

benchmark_message_read_write(Store = #{ <<"benchmark-scale">> := Scale }, Shape) ->
    benchmark_message_read_write(
        Store,
        erlang:ceil(Scale * ?BENCH_MSG_WRITE_OPS),
        erlang:ceil(Scale * ?BENCH_MSG_READ_OPS),
        Shape
    );
benchmark_message_read_write(Store, Shape) ->
    benchmark_message_read_write(
        Store,
        ?BENCH_MSG_WRITE_OPS,
        ?BENCH_MSG_READ_OPS,
        Shape
    ).
benchmark_message_read_write(Store, WriteOps, ReadOps, Shape) ->
    start(Store),
    Opts = #{ 
        <<"store">> => Store, 
        <<"priv-wallet">> => hb:wallet()
    },
    TestDataSize = ?BENCH_MSG_DATA_SIZE * 8, % in _bits_
    timer:sleep(100),
    ?event(
        {benchmarking,
            {store, Store},
            {shape, Shape},
            {write_ops, WriteOps},
            {read_ops, ReadOps}
        }
    ),
    % Generate a random message to write and the keys to read ahead of time.
    {GenerateTime, Msgs} =
        timer:tc(
            fun() ->
                lists:map(
                    fun(N) ->
                        benchmark_message(Shape, N, TestDataSize)
                    end,
                    lists:seq(1, WriteOps)
                )
            end
        ),
    hb_test_utils:benchmark_print(
        <<"Generated">>,
        <<"messages">>,
        WriteOps,
        GenerateTime / 1_000_000
    ),
    {WriteTime, MsgPairs} =
        timer:tc(
            fun() ->
                lists:map(
                    fun(Msg) ->
                        {hb_util:ok(hb_cache:write(Msg, Opts)), Msg}
                    end,
                    Msgs
                )
            end
        ),
    % Print the results. Our write time is in microseconds, so we normalize it
    % to seconds.
    hb_test_utils:benchmark_print(
        <<"Wrote">>,
        <<"messages">>,
        WriteOps,
        WriteTime / 1_000_000
    ),
    % Generate keys to read ahead of time.
    ReadKeys =
        lists:map(
            fun(_) ->
                lists:nth(rand:uniform(length(MsgPairs)), MsgPairs)
            end,
            lists:seq(1, ReadOps)
        ),
    % Time random reads.
    {ReadTime, NotFoundCount} =
        timer:tc(
            fun() ->
                lists:foldl(
                    fun({MsgID, _Msg}, Count) ->
                        case hb_cache:read(MsgID, Opts) of
                            {ok, _CacheMsg} -> Count;
                            _ -> Count + 1
                        end
                    end,
                    0,
                    ReadKeys
                )
            end
        ),
    % Print the results.
    hb_test_utils:benchmark_print(
        <<"Read">>,
        <<"messages">>,
        ReadOps,
        ReadTime / 1_000_000
    ),
    ?assertEqual(0, NotFoundCount, "Written keys not found in store.").

benchmark_message(flat, N, TestDataSize) ->
    #{
        <<"process">> => <<0:TestDataSize, N:32>>,
        <<"slot">> => N
    };
benchmark_message(nested, N, TestDataSize) ->
    (benchmark_message(flat, N, TestDataSize))#{
        <<"message">> =>
            #{
                <<"body">> => <<"test", 0:TestDataSize, N:32>>
            }
    }.

%%% Normalization Pipeline Tests

%% @doc Test that a store with a `prefix' only admits keys bearing it --
%% writes and reads alike -- stripping the prefix ahead of invocation and
%% falling through to later stores otherwise, and that a path resolved
%% through it regains the prefix, as `hb_cache' reuses the resolution.
prefix_pipeline_test() ->
    Mounted =
        (hb_test_utils:test_store(hb_store_fs, <<"pipeline-prefix">>))#{
            <<"prefix">> => <<"mnt/">>
        },
    Plain = hb_test_utils:test_store(hb_store_fs, <<"pipeline-plain">>),
    StoreList = [Mounted, Plain],
    Ungated = maps:remove(<<"prefix">>, Mounted),
    start(StoreList),
    ?event(testing, {prefix_pipeline_test_started}),
    % A prefixed write is stripped ahead of the store, so the raw key is
    % visible once the prefix gate is removed from the store message.
    ?assertEqual(ok, write(StoreList, write_req(<<"mnt/inner">>, <<"1">>), #{})),
    ?assertEqual({ok, <<"1">>}, read(StoreList, <<"mnt/inner">>, #{})),
    ?assertEqual({ok, <<"1">>}, read([Ungated], <<"inner">>, #{})),
    ?assertEqual({error, not_found}, read([Plain], <<"mnt/inner">>, #{})),
    ?assertEqual(
        {ok, <<"mnt/inner">>},
        resolve(StoreList, <<"mnt/inner">>, #{})
    ),
    ?assertEqual(
        {ok, <<"1">>},
        hb_cache:read(
            <<"mnt/inner">>,
            #{ <<"store">> => Mounted, <<"cache-read-mode">> => raw }
        )
    ),
    ?event(testing, {prefixed_write_and_read_passed}),
    % A key without the prefix skips the mounted store entirely, for writes
    % and reads alike.
    ?assertEqual(ok, write(StoreList, write_req(<<"outer">>, <<"2">>), #{})),
    ?assertEqual({ok, <<"2">>}, read(StoreList, <<"outer">>, #{})),
    ?assertEqual({error, not_found}, read([Ungated], <<"outer">>, #{})),
    % Disabling `prefix-strip' admits the key but passes it through whole.
    StripOff = Mounted#{ <<"prefix-strip">> => <<"false">> },
    ?assertEqual(ok, write([StripOff], write_req(<<"mnt/keep">>, <<"3">>), #{})),
    ?assertEqual({ok, <<"3">>}, read([StripOff], <<"mnt/keep">>, #{})),
    ?assertEqual({ok, <<"3">>}, read([Ungated], <<"mnt/keep">>, #{})),
    ?assertEqual(ok, reset([Mounted])),
    ?assertEqual({error, not_found}, read([Ungated], <<"inner">>, #{})),
    ?assertEqual({ok, <<"2">>}, read([Plain], <<"outer">>, #{})),
    ?event(testing, {unprefixed_skip_and_strip_off_passed}).

%% @doc Test that lifecycle operations bypass path preprocessing for a store
%% carrying a prefix.
prefix_pipeline_stop_test() ->
    Store =
        (hb_test_utils:test_store(hb_store_volatile, <<"pipeline-stop">>))#{
            <<"prefix">> => <<"mnt/">>
        },
    start(Store),
    #{ <<"pid">> := Pid } = find(Store),
    Monitor = erlang:monitor(process, Pid),
    ?assertEqual(ok, stop(Store)),
    receive
        {'DOWN', Monitor, process, Pid, normal} -> ok
    after 1000 ->
        ?assert(false)
    end.

%% @doc Test that `to-key' and `to-value' rewrite a request's paths and
%% values ahead of the store -- for writes and reads alike -- and that
%% `from-key' and `from-value' normalize its answers: a read's value, each
%% child a list or a composite read enumerates, and a resolved path, which
%% regains its prefix. A path the normalization cannot decode, or an answer
%% that fails its normalization, falls through to the next store with the
%% caller's original request.
normalize_pipeline_test() ->
    Store =
        (hb_test_utils:test_store(hb_store_fs, <<"pipeline-normalize">>))#{
            <<"prefix">> => <<"b64/">>,
            <<"to-key">> => <<"~base64url@1.0/decode/body">>,
            <<"from-key">> => <<"~base64url@1.0/encode/body">>,
            <<"to-value">> => <<"~base64url@1.0/encode/body">>,
            <<"from-value">> => <<"~base64url@1.0/decode/body">>
        },
    Fallback = hb_test_utils:test_store(hb_store_fs, <<"pipeline-fallback">>),
    Raw = maps:without(?PIPELINE_KEYS, Store),
    start([Store, Fallback]),
    ?event(testing, {normalize_pipeline_test_started}),
    % A write under the canonical key is decoded ahead of the store and its
    % value encoded: both are visible with the pipeline removed from the
    % message, and the read decodes the value again.
    EncodedKey = hb_util:encode(<<"inner">>),
    ?assertEqual(
        ok,
        write([Store], write_req(<<"b64/", EncodedKey/binary>>, <<"val">>), #{})
    ),
    ?assertEqual(
        {ok, hb_util:encode(<<"val">>)},
        read([Raw], <<"inner">>, #{})
    ),
    ?assertEqual(
        {ok, <<"val">>},
        read([Store], <<"b64/", EncodedKey/binary>>, #{})
    ),
    % A single store message is also a valid `store' option.
    SingleKey = hb_util:encode(<<"single">>),
    SingleOpts = #{ <<"store">> => Store },
    ?assertEqual(
        ok,
        write(write_req(<<"b64/", SingleKey/binary>>, <<"one">>), SingleOpts)
    ),
    ?assertEqual(
        {ok, <<"one">>},
        read(<<"b64/", SingleKey/binary>>, SingleOpts)
    ),
    ?event(testing, {normalized_write_and_read_passed}),
    % Each child a list or a composite read enumerates is encoded, and a
    % resolved path is encoded and regains its prefix.
    EncodedChild = hb_util:encode(<<"g/a">>),
    ?assertEqual(
        ok,
        write([Store], write_req(<<"b64/", EncodedChild/binary>>, <<"x">>), #{})
    ),
    EncodedGroup = hb_util:encode(<<"g">>),
    ?assertEqual(
        {ok, [hb_util:encode(<<"a">>)]},
        list([Store], <<"b64/", EncodedGroup/binary>>, #{})
    ),
    ?assertEqual(
        {composite, [hb_util:encode(<<"a">>)]},
        read([Store], <<"b64/", EncodedGroup/binary>>, #{})
    ),
    ?assertEqual(
        {ok, <<"b64/", EncodedChild/binary>>},
        resolve([Store], <<"b64/", EncodedChild/binary>>, #{})
    ),
    % An undecodable key skips the store for writes and reads alike, so the
    % fallback both receives and serves the original request.
    ?assertEqual(
        ok,
        write([Store, Fallback], write_req(<<"b64/!!!">>, <<"f">>), #{})
    ),
    ?assertEqual({ok, <<"f">>}, read([Store, Fallback], <<"b64/!!!">>, #{})),
    ?event(testing, {undecodable_key_fell_through}),
    % An answer failing its normalization errs the store, falling through to
    % the next store that carries the key.
    Bad =
        (hb_test_utils:test_store(hb_store_fs, <<"pipeline-badresult">>))#{
            <<"prefix">> => <<"b64/">>,
            <<"from-value">> => <<"~base64url@1.0/decode">>
        },
    start([Bad]),
    ?assertEqual(ok, write([Bad], write_req(<<"b64/x">>, <<"!!!">>), #{})),
    ?assertEqual(ok, write([Fallback], write_req(<<"b64/x">>, <<"fb">>), #{})),
    ?assertEqual({ok, <<"fb">>}, read([Bad, Fallback], <<"b64/x">>, #{})),
    ?event(testing, {failed_result_normalization_fell_through}).

%%% Access Control Tests

%% @doc Test that read-only stores allow read operations but block write operations
read_only_access_test() ->
    TestStore = hb_test_utils:test_store(hb_store_fs, <<"access-read-only">>),
    ReadOnlyStore = TestStore#{<<"access">> => [<<"read">>]},
    WriteStore = hb_test_utils:test_store(hb_store_fs, <<"access-write">>),
    StoreList = [ReadOnlyStore, WriteStore],
    TestKey = <<"test-key">>,
    TestValue = <<"test-value">>,
    start(StoreList),
    ?event(testing, {read_only_test_started}),
    WriteResponse = write(StoreList, write_req(TestKey, TestValue), #{}),
    ?assertEqual(ok, WriteResponse),
    ?event(testing, {write_used_fallback_store, WriteResponse}),
    ReadResponse = read(StoreList, TestKey, #{}),
    ?assertEqual({ok, TestValue}, ReadResponse),
    ?event(testing, {read_succeeded, ReadResponse}),
    ReadOnlyStoreState = read([ReadOnlyStore], TestKey, #{}),
    WriteStoreState = read([WriteStore], TestKey, #{}),
    ?event(testing, {
        store_state, {read_only, ReadOnlyStoreState},{ write, WriteStoreState}
    }),
    ?assertEqual({error, not_found}, ReadOnlyStoreState),
    ?assertEqual({ok, TestValue}, WriteStoreState).

%% @doc Test that write-only stores allow write operations but block read operations  
write_only_access_test() ->
    WriteOnlyStore =
        (hb_test_utils:test_store(hb_store_fs, <<"access-write-only">>))#{
            <<"access">> => [<<"write">>]
        },
    ReadStore = hb_test_utils:test_store(hb_store_fs, <<"access-read-fallback">>),
    StoreList = [WriteOnlyStore, ReadStore],
    TestKey = <<"write-test-key">>,
    TestValue = <<"write-test-value">>,
    start(StoreList),
    ?event(testing, {write_only_test_started}),
    ?assertEqual(ok, write(StoreList, write_req(TestKey, TestValue), #{})),
    ?event(testing, {write_succeeded_on_write_only}),
    ReadStoreState = read(StoreList, TestKey, #{}),
    ?assertEqual({error, not_found}, ReadStoreState),
    ?event(testing, {read_skipped_write_only_store, ReadStoreState}),
    WriteOnlyStoreNoAccess = maps:remove(<<"access">>, WriteOnlyStore),
    ReadStoreNoAccess = read([WriteOnlyStoreNoAccess], TestKey, #{}),
    ?event(testing, {store, ReadStoreNoAccess}),
    ?assertEqual({ok, TestValue}, ReadStoreNoAccess).

%% @doc Test admin-only stores for start/stop/reset operations
admin_only_access_test() ->
    AdminOnlyStore =
        (hb_test_utils:test_store(hb_store_fs, <<"access-admin-only">>))#{
            <<"access">> => [<<"admin">>, <<"read">>, <<"write">>]
        },
    StoreList = [AdminOnlyStore],
    TestKey = <<"admin-test-key">>,
    TestValue = <<"admin-test-value">>,
    start(StoreList),
    ?assertEqual(ok, write(StoreList, write_req(TestKey, TestValue), #{})),
    ?assertEqual({ok, TestValue}, read(StoreList, TestKey, #{})),
    ?assertEqual(ok, reset(StoreList)),
    ?assertEqual(ok, start(StoreList)),
    ?assertEqual({error, not_found}, read(StoreList, TestKey, #{})).

%% @doc Test multiple access permissions
multi_access_permissions_test() ->
    ReadWriteStore =
        (hb_test_utils:test_store(hb_store_fs, <<"access-read-write">>))#{
            <<"access">> => [<<"read">>, <<"write">>]
        },
    AdminStore =
        (hb_test_utils:test_store(hb_store_fs, <<"access-admin-fallback">>))#{
            <<"access">> => [<<"admin">>]
        },
    StoreList = [ReadWriteStore, AdminStore],
    TestKey = <<"multi-access-key">>,
    TestValue = <<"multi-access-value">>,
    start(StoreList),
    ?event(testing, {multi_access_test_started}),
    ?assertEqual(ok, write(StoreList, write_req(TestKey, TestValue), #{})),
    ?event(testing, {write_succeeded_on_read_write_store}),
    ?assertEqual({ok, TestValue}, read(StoreList, TestKey, #{})),
    ?event(testing, {read_succeeded_on_read_write_store}),
    ?assertEqual(ok, reset(StoreList)),
    ?assertEqual(ok, start(StoreList)),
    ?assertEqual({error, not_found}, read(StoreList, TestKey, #{})).

%% @doc Test access control with a list of stores.
store_access_list_test() ->
    % Chain: Read-only -> Write-only -> Unrestricted
    ReadOnlyStore =
        (hb_test_utils:test_store(hb_store_fs, <<"chain-read-only">>))#{
            <<"access">> => [<<"read">>]
        },
    WriteOnlyStore =
        (hb_test_utils:test_store(hb_store_fs, <<"chain-write-only">>))#{
            <<"access">> => [<<"write">>]
        },
    UnrestrictedStore =
        hb_test_utils:test_store(hb_store_fs, <<"chain-unrestricted">>),
    StoreChain = [ReadOnlyStore, WriteOnlyStore, UnrestrictedStore],
    TestKey = <<"chain-test-key">>,
    TestValue = <<"chain-test-value">>,
    start(StoreChain),
    ?event(testing, {fallback_chain_test_started, length(StoreChain)}),
    ?assertEqual(ok, write(StoreChain, write_req(TestKey, TestValue), #{})),
    ?event(testing, {write_used_second_store_in_chain}),
    ?assertEqual({error, not_found}, read(StoreChain, TestKey, #{})),
    ?event(testing, {read_fell_through_entire_chain}),
    WriteOnlyNoAccess = maps:remove(<<"access">>, WriteOnlyStore),
    ?assertEqual({ok, TestValue}, read([WriteOnlyNoAccess], TestKey, #{})).

%% @doc Test invalid access permissions are ignored
invalid_access_permissions_test() ->
    InvalidAccessStore =
        (hb_test_utils:test_store(hb_store_fs, <<"access-invalid">>))#{
            <<"access">> => [<<"invalid-policy">>, <<"nonexistent-policy">>]
        },
    FallbackStore = hb_test_utils:test_store(hb_store_fs, <<"access-fallback">>),
    StoreList = [InvalidAccessStore, FallbackStore],
    TestKey = <<"invalid-access-key">>,
    TestValue = <<"invalid-access-value">>,
    start(StoreList),
    ?event(testing, {invalid_access_test_started}),
    ?assertEqual(ok, write(StoreList, write_req(TestKey, TestValue), #{})),
    ?event(testing, {write_used_fallback_store}),
    ?assertEqual({ok, TestValue}, read(StoreList, TestKey, #{})),
    ?event(testing, {read_used_fallback_store}),
    InvalidStoreNoAccess = maps:remove(<<"access">>, InvalidAccessStore),
    start([InvalidStoreNoAccess]),
    ?assertEqual({error, not_found}, read([InvalidStoreNoAccess], TestKey, #{})).

%% @doc Test list operations with access control
list_access_control_test() ->
    ReadOnlyStore =
        (hb_test_utils:test_store(hb_store_fs, <<"list-read-only">>))#{
            <<"access">> => [<<"read">>]
        },
    WriteStore = hb_test_utils:test_store(hb_store_fs, <<"list-write">>),
    StoreList = [ReadOnlyStore, WriteStore],
    ListGroup = <<"list-test-group">>,
    TestKey = <<"list-test-key">>,
    TestValue = <<"list-test-value">>,
    start(StoreList),
    ?event(testing, {list_access_test_started}),
    GroupResult = group(StoreList, ListGroup, #{}),
    ?assertEqual(ok, GroupResult),
    ?event(testing, {group_created, GroupResult}),
    WriteResponse = write(StoreList, write_req([ListGroup, TestKey], TestValue), #{}),
    ?assertEqual(ok, WriteResponse),
    ListResult = list(StoreList, ListGroup, #{}),
    ListValue = read(StoreList, [ListGroup, TestKey], #{}),
    ?event(testing, {list_result, ListResult, ListValue}),
    ?assertEqual({ok,[TestKey]}, ListResult),
    ?assertEqual({ok,TestValue}, ListValue).

%% @doc Test make_link operations with write access
make_link_access_test() ->
    WriteOnlyStore =
        (hb_test_utils:test_store(hb_store_fs, <<"link-write-only">>))#{
            <<"access">> => [<<"write">>,<<"read">>]
        },
    FallbackStore = hb_test_utils:test_store(hb_store_fs, <<"link-fallback">>),
    StoreList = [WriteOnlyStore, FallbackStore],
    SourceKey = <<"link-source">>,
    TargetKey = <<"link-target">>,
    TestValue = <<"link-test-value">>,
    start(StoreList),
    ?event(testing, {make_link_access_test_started}),
    ?assertEqual(ok, write(StoreList, write_req(TargetKey, TestValue), #{})),
    LinkResult = link(StoreList, link_req(SourceKey, TargetKey), #{}),
    ?event(testing, {make_link_result, LinkResult}),
    ReadResult = read(StoreList, SourceKey, #{}),
    ?event(testing, {read_linked_value, ReadResult}),
    ?assertEqual({ok, TestValue}, ReadResult),
    ?assertEqual(ok, LinkResult).

%% Prevent stores with access property to return local scope if they are defined as remote.
get_store_scope_access_test() ->
    ReadStore = #{<<"store-module">> => hb_store_remote_node, <<"access">> => [<<"read">>]},
    ?assertEqual(remote, get_store_scope(ReadStore)),
    WriteStore = #{<<"store-module">> => hb_store_remote_node, <<"access">> => [<<"write">>]},
    ?assertEqual(remote, get_store_scope(WriteStore)),
    AdminStore = #{<<"store-module">> => hb_store_remote_node, <<"access">> => [<<"admin">>]},
    ?assertEqual(remote, get_store_scope(AdminStore)).
