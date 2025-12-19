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
%%%     start/1:      Initialize the store.
%%%     stop/1:       Stop any processes (etc.) that manage the store.
%%%     reset/1:      Restore the store to its original, empty state.
%%%     scope/0:      A tag describing the 'scope' of a stores search: `in_memory',
%%%                   `local', `remote', `arweave', etc. Used in order to allow
%%%                   node operators to prioritize their stores for search.
%%%     make_group/2: Create a new group of keys in the store with the given ID.
%%%     make_link/3:  Create a link (implying one key should redirect to another)
%%%                   from `existing` to `new` (in that order).
%%%     type/2:       Return whether the value found at the given key is a
%%%                   `composite' (group) type, or a `simple' direct binary.
%%%     read/2:       Read the data at the given location, returning a binary
%%%                   if it is a `simple' value, or a message if it is a complex
%%%                   term.
%%%     write/3:      Write the given `key` with the associated `value` (in that
%%%                   order) to the store.
%%%     list/2:       For `composite' type keys, return a list of its child keys.
%%%     path/2:       Optionally transform a list of path parts into the store's
%%%                   canonical form.
%%% '''
%%% Each function takes a `store' message first, containing an arbitrary set
%%% of its necessary configuration keys, as well as the `store-module' key which
%%% refers to the Erlang module that implements the store.
%%% 
%%% All functions must return `ok` or `{ok, Result}`, as appropriate. Other 
%%% results will lead to the store manager (this module) iterating to the next
%%% store message given by the user. If none of the given store messages are 
%%% able to execute a requested service, the store manager will return 
%%% `not_found`.

-module(hb_store).
-export([behavior_info/1]).
-export([start/1, stop/1, reset/1]).
-export([filter/2, scope/2, sort/2]).
-export([type/2, read/2, write/3, list/2, match/2]).
-export([path/1, path/2, add_path/2, add_path/3, join/1]).
-export([make_group/2, make_link/3, resolve/2]).
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
-define(BENCH_MSG_WRITE_OPS, 250).
-define(BENCH_MSG_READ_OPS, 250).
-define(BENCH_MSG_DATA_SIZE, 1024).

behavior_info(callbacks) ->
    [
        {start, 1}, {stop, 1}, {reset, 1}, {make_group, 2}, {make_link, 3},
        {type, 2}, {read, 2}, {write, 3},
        {list, 2}, {match, 2}, {path, 2}, {add_path, 3}
    ].

-define(DEFAULT_SCOPE, local).
-define(DEFAULT_RETRIES, 1).

%% @doc Store access policies to function names.
-define(STORE_ACCESS_POLICIES, #{
    <<"read">> => [read, resolve, list, type, path, add_path, join],
    <<"write">> => [write, make_link, make_group, reset, path, add_path, join],
    <<"admin">> => [start, stop, reset]
}).

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
    hb_event:increment(<<"store_duration">>, <<"find">>, #{}, Time),
    hb_event:increment(<<"store">>, <<"find">>, #{}, 1),
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
    try Mod:start(StoreOpts) of
        ok -> ok;
        {ok, InstanceMessage} ->
            set(Mod, Name, InstanceMessage),
            InstanceMessage;
        {error, Reason} ->
            ?event(error, {store_start_failed, {Mod, Name, Reason}}),
            throw({store_start_failed, {Mod, Name, Reason}})
    catch error:undef ->
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
start(StoreOpts) when not is_list(StoreOpts) -> start([StoreOpts]);
start([]) -> ok;
start([StoreOpts | Rest]) ->
    find(StoreOpts),
    start(Rest).

stop(Modules) ->
    call_function(Modules, stop, []).

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
            Opts#{ store => scope(Store, Scope) };
        Store when is_map(Store) ->
            % Check if Store already has a nested 'store' key
            case maps:find(store, Store) of
                {ok, _NestedStores} ->
                    % Already has nested structure, return as-is
                    Opts;
                error ->
                    % Single store map, wrap in list before scoping
                    % This ensures consistent behavior
                    Opts#{ store => scope([Store], Scope) }
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
    case call_function(Store, scope, []) of
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

%% @doc Join a list of path components together.
join(Path) -> hb_path:to_binary(Path).

%%% The store interface that modules should implement.

%% @doc Read a key from the store.
read(Modules, Key) -> call_function(Modules, read, [Key]).

%% @doc Write a key with a value to the store.
write(Modules, Key, Value) -> call_function(Modules, write, [Key, Value]).

%% @doc Make a group in the store. A group can be seen as a namespace or
%% 'directory' in a filesystem.
make_group(Modules, Path) -> call_function(Modules, make_group, [Path]).

%% @doc Make a link from one path to another in the store.
make_link(Modules, Existing, New) ->
    call_function(Modules, make_link, [Existing, New]).

%% @doc Delete all of the keys in a store. Should be used with extreme
%% caution. Lost data can lose money in many/most of hyperbeam's use cases.
reset(Modules) -> call_function(Modules, reset, []).

%% @doc Get the type of element of a given path in the store. This can be
%% a performance killer if the store is remote etc. Use only when necessary.
type(Modules, Path) -> call_function(Modules, type, [Path]).

%% @doc Create a path from a list of path components. If no store implements
%% the path function, we return the path with the 'default' transformation (id).
path(Path) -> join(Path).
path(_, Path) -> path(Path).

%% @doc Add two path components together. If no store implements the add_path
%% function, we concatenate the paths.
add_path(Path1, Path2) -> Path1 ++ Path2.
add_path(Store, Path1, Path2) ->
    case call_function(Store, add_path, [Path1, Path2]) of
        not_found -> add_path(Path1, Path2);
        Result -> Result
    end.

%% @doc Follow links through the store to resolve a path to its ultimate target.
resolve(Modules, Path) -> call_function(Modules, resolve, [Path]).

%% @doc List the keys in a group in the store. Use only in debugging.
%% The hyperbeam model assumes that stores are built as efficient hash-based
%% structures, so this is likely to be very slow for most stores.
list(Modules, Path) -> call_function(Modules, list, [Path]).

%% @doc Match a series of keys and values against the store. Returns 
%% `{ok, Matches}' if the match is successful, or `not_found' if there are no
%% messages in the store that feature all of the given key-value pairs. `Matches'
%% is given as a list of IDs.
match(Modules, Match) -> call_function(Modules, match, [Match]).

%% @doc Call a function on the first store module that succeeds. Returns its
%% result, or `not_found` if none of the stores succeed. If `TIME_CALLS` is set,
%% this function will also time the call and increment the appropriate event
%% counter.
-ifdef(STORE_EVENTS).
call_function(X, Function, Args) ->
    {Time, Result} = timer:tc(fun() -> do_call_function(X, Function, Args) end),
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
    hb_event:increment(<<"store_duration">>, hb_util:bin(Function), #{}, Time),
    hb_event:increment(<<"store">>, hb_util:bin(Function), #{}, 1),
    Result.
-else.
call_function(X, Function, Args) ->
    do_call_function(X, Function, Args).
-endif.
do_call_function(X, _Function, _Args) when not is_list(X) ->
    do_call_function([X], _Function, _Args);
do_call_function([], _Function, _Args) ->
    not_found;
do_call_function([Store = #{<<"access">> := Access} | Rest], Function, Args) ->
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
                Args
            );
        false ->
            do_call_function(Rest, Function, Args)
    end;
do_call_function([Store = #{<<"store-module">> := Mod} | Rest], Function, Args) ->
    % Attempt to apply the function. If it fails, try the next store.
    try apply_store_function(Mod, Store, Function, Args) of
        not_found ->
            do_call_function(Rest, Function, Args);
        Result ->
            Result
    catch _:_:_ -> do_call_function(Rest, Function, Args)
    end.

%% @doc Apply a store function, checking if the store returns a retry request or
%% errors. If it does, attempt to start the store again and retry, up to the
%% given maximum number of times.
apply_store_function(Mod, Store, Function, Args) ->
    MaxAttempts = maps:get(<<"max-retries">>, Store, ?DEFAULT_RETRIES) + 1,
    apply_store_function(Mod, Store, Function, Args, MaxAttempts).
apply_store_function(_Mod, _Store, _Function, _Args, 0) ->
    % Too many attempts have already failed. Bail.
    not_found;
apply_store_function(Mod, Store, Function, Args, AttemptsRemaining) ->
    try apply(Mod, Function, [Store | Args]) of
        retry -> retry(Mod, Store, Function, Args, AttemptsRemaining);
        Other -> Other
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
        retry(Mod, Store, Function, Args, AttemptsRemaining)
    end.

%% @doc Stop and start the store, then retry.
retry(Mod, Store, Function, Args, AttemptsRemaining) ->
    % Attempt to stop the store and start it again, then retry.
    try Mod:stop(Store) catch _:_ -> ignore_errors end,
    set(Store, undefined),
    start(Store),
    apply_store_function(Mod, Store, Function, Args, AttemptsRemaining - 1).

%% @doc Call a function on all modules in the store.
call_all(X, _Function, _Args) when not is_list(X) ->
    call_all([X], _Function, _Args);
call_all([], _Function, _Args) ->
    ok;
call_all([Store = #{<<"store-module">> := Mod} | Rest], Function, Args) ->
    try apply_store_function(Mod, Function, Store, Args)
    catch
        Class:Reason:Stacktrace ->
            ?event(warning, {store_call_failed, {Class, Reason, Stacktrace}}),
            ok
    end,
    call_all(Rest, Function, Args).

%%% Test helpers

%% @doc Return a list of stores for testing. Additional individual functions are
%% used to generate store options for those whose drivers are not built by 
%% default into all HyperBEAM distributions.
test_stores() ->
    [
        (hb_test_utils:test_store(hb_store_fs))#{
            <<"benchmark-scale">> => 0.001
        },
        (hb_test_utils:test_store(hb_store_lmdb))#{
            <<"benchmark-scale">> => 0.5
        },
        (hb_test_utils:test_store(hb_store_lru))#{
            <<"persistent-store">> => [
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-TEST/lru">>
                }
            ]
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

%% @doc Test path resolution dynamics.
simple_path_resolution_test(Store) ->
    ok = hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link">>)).

%% @doc Ensure that we can resolve links recursively.
resursive_path_resolution_test(Store) ->
    hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    hb_store:make_link(Store, <<"test-link">>, <<"test-link2">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link2">>)).

%% @doc Ensure that we can resolve links through a directory.
hierarchical_path_resolution_test(Store) ->
    hb_store:make_group(Store, <<"test-dir1">>),
    hb_store:write(Store, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    hb_store:make_link(Store, [<<"test-dir1">>], <<"test-link">>),
    ?assertEqual(
        {ok, <<"test-data">>},
        hb_store:read(Store, [<<"test-link">>, <<"test-file">>])
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
        {"benchmark message read write", fun benchmark_message_read_write/1}
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
    % Generate random data to write and the keys to read ahead of time.
    RandomData = hb_util:human_id(crypto:strong_rand_bytes(32)),
    Keys =
        lists:map(
            fun(N) ->
                << "key-", (integer_to_binary(N))/binary >>
            end,
            lists:seq(1, ReadOps)
        ),
    {WriteTime, ok} =
        timer:tc(
            fun() ->
                lists:foreach(
                    fun(Key) -> ok = write(Store, Key, RandomData) end,
                    Keys
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
    % Generate keys to read ahead of time.
    ReadKeys =
        lists:map(
            fun(_) ->
                << "key-", (integer_to_binary(rand:uniform(ReadOps)))/binary >>
            end,
            lists:seq(1, ReadOps)
        ),
    % Time random reads.
    {ReadTime, NotFoundCount} =
        timer:tc(
            fun() ->
                lists:foldl(
                    fun(Key, Count) -> 
                        case read(Store, Key) of
                            {ok, _} -> Count;
                            _ -> Count + 1
                        end
                    end,
                    0,
                    ReadKeys
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
                        ok = make_group(Store, GroupID),
                        lists:foreach(
                            fun({Key, Value}) ->
                                ok =
                                    write(
                                        Store,
                                        <<GroupID/binary, "/", Key/binary >>,
                                        Value
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
                list(Store, LastGroupID)
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
                        case list(Store, GroupID) of
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

benchmark_message_read_write(Store = #{ <<"benchmark-scale">> := Scale }) ->
    benchmark_message_read_write(
        Store,
        erlang:ceil(Scale * ?BENCH_MSG_WRITE_OPS),
        erlang:ceil(Scale * ?BENCH_MSG_READ_OPS)
    );
benchmark_message_read_write(Store) ->
    benchmark_message_read_write(Store, ?BENCH_MSG_WRITE_OPS, ?BENCH_MSG_READ_OPS).
benchmark_message_read_write(Store, WriteOps, ReadOps) ->
    start(Store),
    Opts = #{ store => Store, priv_wallet => hb:wallet() },
    TestDataSize = ?BENCH_MSG_DATA_SIZE * 8, % in _bits_
    timer:sleep(100),
    ?event(
        {benchmarking,
            {store, Store},
            {write_ops, WriteOps},
            {read_ops, ReadOps}
        }
    ),
    % Generate a random message to write and the keys to read ahead of time.
    Msgs =
        lists:map(
            fun(N) ->
                #{
                    <<"process">> => hb_util:human_id(crypto:strong_rand_bytes(32)),
                    <<"slot">> => N,
                    <<"message">> =>
                        hb_message:commit(
                            #{
                                <<"body">> => <<"test", 0:TestDataSize, N:32>>
                            },
                            Opts
                        )
                }
            end,
            lists:seq(1, WriteOps)
        ),
    hb_format:eunit_print(
        "Generated ~s messages (size ~s bits)",
        [
            hb_util:human_int(WriteOps),
            hb_util:human_int(TestDataSize)
        ]
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
                    fun({MsgID, Msg}, Count) -> 
                        NormalizedMsg =
                            hb_cache:ensure_all_loaded(
                                hb_message:normalize_commitments(Msg, Opts),
                                Opts
                            ),
                        case hb_cache:read(MsgID, Opts) of
                            {ok, CacheMsg} ->
                                NormalizedCacheMsg = 
                                    hb_message:normalize_commitments(
                                        hb_cache:read_all_commitments(
                                            hb_cache:ensure_all_loaded(
                                                CacheMsg,
                                                Opts
                                            ),
                                            Opts
                                        ),
                                        Opts
                                    ),
                                case NormalizedCacheMsg of
                                    NormalizedMsg -> Count;
                                    _ -> Count + 1
                                end;
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
    WriteResponse = write(StoreList, TestKey, TestValue),
    ?assertEqual(ok, WriteResponse),
    ?event(testing, {write_used_fallback_store, WriteResponse}),
    ReadResponse = read(StoreList, TestKey),
    ?assertEqual({ok, TestValue}, ReadResponse),
    ?event(testing, {read_succeeded, ReadResponse}),
    ReadOnlyStoreState = read([ReadOnlyStore], TestKey),
    WriteStoreState = read([WriteStore], TestKey),
    ?event(testing, {
        store_state, {read_only, ReadOnlyStoreState},{ write, WriteStoreState}
    }),
    ?assertEqual(not_found, ReadOnlyStoreState),
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
    ?assertEqual(ok, write(StoreList, TestKey, TestValue)),
    ?event(testing, {write_succeeded_on_write_only}),
    ReadStoreState = read(StoreList, TestKey),
    ?assertEqual(not_found, ReadStoreState),
    ?event(testing, {read_skipped_write_only_store, ReadStoreState}),
    WriteOnlyStoreNoAccess = maps:remove(<<"access">>, WriteOnlyStore),
    ReadStoreNoAccess = read([WriteOnlyStoreNoAccess], TestKey),
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
    ?assertEqual(ok, write(StoreList, TestKey, TestValue)),
    ?assertEqual({ok, TestValue}, read(StoreList, TestKey)),
    reset(StoreList),
    ?assertEqual(ok, start(StoreList)),
    ?assertEqual(not_found, read(StoreList, TestKey)).

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
    ?assertEqual(ok, write(StoreList, TestKey, TestValue)),
    ?event(testing, {write_succeeded_on_read_write_store}),
    ?assertEqual({ok, TestValue}, read(StoreList, TestKey)),
    ?event(testing, {read_succeeded_on_read_write_store}),
    reset(StoreList),
    ?assertEqual(ok, start(StoreList)),
    ?assertEqual(not_found, read(StoreList, TestKey)).

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
    ?assertEqual(ok, write(StoreChain, TestKey, TestValue)),
    ?event(testing, {write_used_second_store_in_chain}),
    ?assertEqual(not_found, read(StoreChain, TestKey)),
    ?event(testing, {read_fell_through_entire_chain}),
    WriteOnlyNoAccess = maps:remove(<<"access">>, WriteOnlyStore),
    ?assertEqual({ok, TestValue}, read([WriteOnlyNoAccess], TestKey)).

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
    ?assertEqual(ok, write(StoreList, TestKey, TestValue)),
    ?event(testing, {write_used_fallback_store}),
    ?assertEqual({ok, TestValue}, read(StoreList, TestKey)),
    ?event(testing, {read_used_fallback_store}),
    InvalidStoreNoAccess = maps:remove(<<"access">>, InvalidAccessStore),
    start([InvalidStoreNoAccess]),
    ?assertEqual(not_found, read([InvalidStoreNoAccess], TestKey)).

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
    GroupResult = make_group(StoreList, ListGroup),
    ?assertEqual(ok, GroupResult),
    ?event(testing, {group_created, GroupResult}),
    WriteResponse = write(StoreList, [ListGroup, TestKey], TestValue),
    ?assertEqual(ok, WriteResponse),
    ListResult = list(StoreList, ListGroup),
    ListValue = read(StoreList, [ListGroup, TestKey]),
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
    ?assertEqual(ok, write(StoreList, TargetKey, TestValue)),
    LinkResult = make_link(StoreList, TargetKey, SourceKey),
    ?event(testing, {make_link_result, LinkResult}),
    ReadResult = read(StoreList, SourceKey),
    ?event(testing, {read_linked_value, ReadResult}),
    ?assertEqual({ok, TestValue}, ReadResult),
    ?assertEqual(ok, LinkResult).