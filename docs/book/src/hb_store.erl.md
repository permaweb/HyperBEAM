# hb_store

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store.erl)

A simple abstraction layer for AO key value store operations.
This interface allows us to swap out the underlying store implementation(s)
as desired, without changing the API that `hb_cache` employs. Additionally,
it enables node operators to customize their configuration to maximize
performance, data availability, and other factors.
Stores can be represented in a node's configuration as either a single 
message, or a (`structured@1.0`) list of store messages. If a list of stores
is provided, the node will cycle through each until a viable store is found
to execute the given function.
A valid store must implement a _subset_ of the following functions:
```
    start/1:      Initialize the store.
    stop/1:       Stop any processes (etc.) that manage the store.
    reset/1:      Restore the store to its original, empty state.
    scope/0:      A tag describing the 'scope' of a stores search: `in_memory`,
                  `local`, `remote`, `arweave`, etc. Used in order to allow
                  node operators to prioritize their stores for search.
    make_group/2: Create a new group of keys in the store with the given ID.
    make_link/3:  Create a link (implying one key should redirect to another)
                  from `existing` to `new` (in that order).
    type/2:       Return whether the value found at the given key is a
                  `composite` (group) type, or a `simple` direct binary.
    read/2:       Read the data at the given location, returning a binary
                  if it is a `simple` value, or a message if it is a complex
                  term.
    write/3:      Write the given `key` with the associated `value` (in that
                  order) to the store.
    list/2:       For `composite` type keys, return a list of its child keys.
    path/2:       Optionally transform a list of path parts into the store's
                  canonical form.
'''
Each function takes a `store` message first, containing an arbitrary set
of its necessary configuration keys, as well as the `store-module` key which
refers to the Erlang module that implements the store.
All functions must return `ok` or `{ok, Result}`, as appropriate. Other 
results will lead to the store manager (this module) iterating to the next
store message given by the user. If none of the given store messages are 
able to execute a requested service, the store manager will return 
`not_found`.

---

## Exported Functions

- `add_path/2`
- `add_path/3`
- `behavior_info/1`
- `filter/2`
- `find/1`
- `generate_test_suite/1`
- `generate_test_suite/2`
- `join/1`
- `list/2`
- `make_group/2`
- `make_link/3`
- `match/2`
- `path/1`
- `path/2`
- `read/2`
- `reset/1`
- `resolve/2`
- `scope/2`
- `sort/2`
- `start/1`
- `stop/1`
- `test_stores/0`
- `type/2`
- `write/3`

---

### behavior_info

A simple abstraction layer for AO key value store operations.
The number of write and read operations to perform in the benchmark.

```erlang
behavior_info(callbacks) ->
    [
        {start, 1}, {stop, 1}, {reset, 1}, {make_group, 2}, {make_link, 3},
        {type, 2}, {read, 2}, {write, 3},
        {list, 2}, {match, 2}, {path, 2}, {add_path, 3}
    ].
```

### set

Store access policies to function names.
Set the instance options for a given store module and name combination.

```erlang
set(StoreOpts, InstanceTerm) ->
    Mod = maps:get(<<"store-module">>, StoreOpts),
    set(
        Mod,
        maps:get(<<"name">>, StoreOpts, Mod),
        InstanceTerm
    ).
```

### set

```erlang
set(StoreMod, Name, undefined) ->
    StoreRef = {store, StoreMod, Name},
    erlang:erase(StoreRef),
    persistent_term:erase(StoreRef);
```

### set

```erlang
set(StoreMod, Name, InstanceTerm) ->
    StoreRef = {store, StoreMod, Name},
    put(StoreRef, InstanceTerm),
    persistent_term:put(StoreRef, InstanceTerm),
    ok.
```

### find

Find or spawn a store instance by its store opts.

```erlang
find(StoreOpts) ->
    {Time, Result} = timer:tc(fun() -> do_find(StoreOpts) end),
    hb_event:increment(<<"store_duration">>, <<"find">>, #{}, Time),
    hb_event:increment(<<"store">>, <<"find">>, #{}, 1),
    Result.
```

### find

```erlang
find(StoreOpts) ->
    do_find(StoreOpts).
```

### do_find

```erlang
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
```

### spawn_instance

Create a new instance of a store and return its term.

```erlang
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
```

### ensure_instance_alive

Handle a found instance message. If it contains a PID, we check if it

```erlang
ensure_instance_alive(StoreOpts, InstanceMessage = #{ <<"pid">> := Pid }) ->
    case is_process_alive(Pid) of
        true -> InstanceMessage;
        false -> spawn_instance(StoreOpts)
    end;
```

### ensure_instance_alive

Handle a found instance message. If it contains a PID, we check if it

```erlang
ensure_instance_alive(_, InstanceMessage) ->
    InstanceMessage.
```

### start

Ensure that a store, or list of stores, have all been started.

```erlang
start(StoreOpts) when not is_list(StoreOpts) -> start([StoreOpts]);
```

### start

Ensure that a store, or list of stores, have all been started.

```erlang
start([]) -> ok;
```

### start

Ensure that a store, or list of stores, have all been started.

```erlang
start([StoreOpts | Rest]) ->
    find(StoreOpts),
    start(Rest).
```

### stop

```erlang
stop(Modules) ->
    call_function(Modules, stop, []).
```

### filter

Takes a store object and a filter function or match spec, returning a

```erlang
filter(Module, Filter) when not is_list(Module) ->
    filter([Module], Filter);
```

### filter

Takes a store object and a filter function or match spec, returning a

```erlang
filter(Modules, Filter) ->
    lists:filter(
        fun(Store) ->
            try Filter(get_store_scope(Store), Store)
            catch _:_ -> false
            end
        end,
        Modules
    ).
```

### scope

Limit the store scope to only a specific (set of) option(s).

```erlang
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
```

### scope

Limit the store scope to only a specific (set of) option(s).

```erlang
scope(Store, Scope) ->
    filter(
        Store,
        fun(StoreScope, _) ->
            StoreScope == Scope orelse
                (is_list(Scope) andalso lists:member(StoreScope, Scope))
        end
    ).
```

### get_store_scope

Ask a store for its own scope. If it doesn't have one, return the

```erlang
get_store_scope(Store) ->
    case call_function(Store, scope, []) of
        not_found -> ?DEFAULT_SCOPE;
        Scope -> Scope
    end.
```

### sort

Order a store by a preference of its scopes. This is useful for making

```erlang
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
```

### sort

Order a store by a preference of its scopes. This is useful for making

```erlang
sort(Stores, ScoreMap) ->
    lists:sort(
        fun(Store1, Store2) ->
            hb_maps:get(get_store_scope(Store1), ScoreMap, 0) >
                hb_maps:get(get_store_scope(Store2), ScoreMap, 0)
        end,
        Stores
    ).
```

### join

Join a list of path components together.

```erlang
join(Path) -> hb_path:to_binary(Path).
%%% The store interface that modules should implement.
```

### read

Read a key from the store.
Write a key with a value to the store.

```erlang
read(Modules, Key) -> call_function(Modules, read, [Key]).
```

### write

Read a key from the store.
Write a key with a value to the store.
Make a group in the store. A group can be seen as a namespace or

```erlang
write(Modules, Key, Value) -> call_function(Modules, write, [Key, Value]).
```

### make_group

Read a key from the store.
Write a key with a value to the store.
Make a group in the store. A group can be seen as a namespace or
Make a link from one path to another in the store.

```erlang
make_group(Modules, Path) -> call_function(Modules, make_group, [Path]).
```

### make_link

Read a key from the store.
Write a key with a value to the store.
Make a group in the store. A group can be seen as a namespace or
Make a link from one path to another in the store.

```erlang
make_link(Modules, Existing, New) ->
    call_function(Modules, make_link, [Existing, New]).
```

### reset

Delete all of the keys in a store. Should be used with extreme
Get the type of element of a given path in the store. This can be

```erlang
reset(Modules) -> call_function(Modules, reset, []).
```

### type

Delete all of the keys in a store. Should be used with extreme
Get the type of element of a given path in the store. This can be
Create a path from a list of path components. If no store implements

```erlang
type(Modules, Path) -> call_function(Modules, type, [Path]).
```

### path

Delete all of the keys in a store. Should be used with extreme
Get the type of element of a given path in the store. This can be
Create a path from a list of path components. If no store implements

```erlang
path(Path) -> join(Path).
```

### path

Delete all of the keys in a store. Should be used with extreme
Get the type of element of a given path in the store. This can be
Create a path from a list of path components. If no store implements
Add two path components together. If no store implements the add_path

```erlang
path(_, Path) -> path(Path).
```

### add_path

Delete all of the keys in a store. Should be used with extreme
Get the type of element of a given path in the store. This can be
Create a path from a list of path components. If no store implements
Add two path components together. If no store implements the add_path

```erlang
add_path(Path1, Path2) -> Path1 ++ Path2.
```

### add_path

Delete all of the keys in a store. Should be used with extreme
Get the type of element of a given path in the store. This can be
Create a path from a list of path components. If no store implements
Add two path components together. If no store implements the add_path

```erlang
add_path(Store, Path1, Path2) ->
    case call_function(Store, add_path, [Path1, Path2]) of
        not_found -> add_path(Path1, Path2);
        Result -> Result
    end.
```

### resolve

Follow links through the store to resolve a path to its ultimate target.
List the keys in a group in the store. Use only in debugging.

```erlang
resolve(Modules, Path) -> call_function(Modules, resolve, [Path]).
```

### list

Follow links through the store to resolve a path to its ultimate target.
List the keys in a group in the store. Use only in debugging.
Match a series of keys and values against the store. Returns 

```erlang
list(Modules, Path) -> call_function(Modules, list, [Path]).
```

### match

Follow links through the store to resolve a path to its ultimate target.
List the keys in a group in the store. Use only in debugging.
Match a series of keys and values against the store. Returns 
Call a function on the first store module that succeeds. Returns its

```erlang
match(Modules, Match) -> call_function(Modules, match, [Match]).
-ifdef(STORE_EVENTS).
```

### call_function

```erlang
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
```

### call_function

```erlang
call_function(X, Function, Args) ->
    do_call_function(X, Function, Args).
```

### do_call_function

```erlang
do_call_function(X, _Function, _Args) when not is_list(X) ->
    do_call_function([X], _Function, _Args);
```

### do_call_function

```erlang
do_call_function([], _Function, _Args) ->
    not_found;
```

### do_call_function

```erlang
do_call_function([Store = #{<<"access">> := Access} | Rest], Function, Args) ->
    % If the store has an access controls, check if the function is allowed from
    % the stated policies.
```

### do_call_function

```erlang
do_call_function([Store = #{<<"store-module">> := Mod} | Rest], Function, Args) ->
    % Attempt to apply the function. If it fails, try the next store.
```

### apply_store_function

Apply a store function, checking if the store returns a retry request or

```erlang
apply_store_function(Mod, Store, Function, Args) ->
    MaxAttempts = maps:get(<<"max-retries">>, Store, ?DEFAULT_RETRIES) + 1,
    apply_store_function(Mod, Store, Function, Args, MaxAttempts).
```

### apply_store_function

```erlang
apply_store_function(_Mod, _Store, _Function, _Args, 0) ->
    % Too many attempts have already failed. Bail.
```

### apply_store_function

```erlang
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
```

### retry

Stop and start the store, then retry.

```erlang
retry(Mod, Store, Function, Args, AttemptsRemaining) ->
    % Attempt to stop the store and start it again, then retry.
```

### call_all

Call a function on all modules in the store.

```erlang
call_all(X, _Function, _Args) when not is_list(X) ->
    call_all([X], _Function, _Args);
```

### call_all

Call a function on all modules in the store.

```erlang
call_all([], _Function, _Args) ->
    ok;
```

### call_all

Call a function on all modules in the store.

```erlang
call_all([Store = #{<<"store-module">> := Mod} | Rest], Function, Args) ->
    try apply_store_function(Mod, Function, Store, Args)
    catch
        Class:Reason:Stacktrace ->
            ?event(warning, {store_call_failed, {Class, Reason, Stacktrace}}),
            ok
    end,
    call_all(Rest, Function, Args).
```

### test_stores

Return a list of stores for testing. Additional individual functions are

```erlang
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
```

### rocks_stores

```erlang
rocks_stores() ->
    [
        #{
            <<"store-module">> => hb_store_rocksdb,
            <<"name">> => <<"cache-TEST/rocksdb">>
        }
    ].
```

### rocks_stores

```erlang
rocks_stores() -> [].
-endif.
```

### generate_test_suite

```erlang
generate_test_suite(Suite) ->
    generate_test_suite(Suite, test_stores()).
```

### generate_test_suite

```erlang
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
```

### simple_path_resolution_test

Test path resolution dynamics.
Ensure that we can resolve links recursively.

```erlang
simple_path_resolution_test(Store) ->
    ok = hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link">>)).
```

### resursive_path_resolution_test

Test path resolution dynamics.
Ensure that we can resolve links recursively.
Ensure that we can resolve links through a directory.

```erlang
resursive_path_resolution_test(Store) ->
    hb_store:write(Store, <<"test-file">>, <<"test-data">>),
    hb_store:make_link(Store, <<"test-file">>, <<"test-link">>),
    hb_store:make_link(Store, <<"test-link">>, <<"test-link2">>),
    ?assertEqual({ok, <<"test-data">>}, hb_store:read(Store, <<"test-link2">>)).
```

### hierarchical_path_resolution_test

Test path resolution dynamics.
Ensure that we can resolve links recursively.
Ensure that we can resolve links through a directory.

```erlang
hierarchical_path_resolution_test(Store) ->
    hb_store:make_group(Store, <<"test-dir1">>),
    hb_store:write(Store, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    hb_store:make_link(Store, [<<"test-dir1">>], <<"test-link">>),
    ?assertEqual(
        {ok, <<"test-data">>},
        hb_store:read(Store, [<<"test-link">>, <<"test-file">>])
    ).
```

### store_suite_test_

```erlang
store_suite_test_() ->
    generate_test_suite([
        {"simple path resolution", fun simple_path_resolution_test/1},
        {"resursive path resolution", fun resursive_path_resolution_test/1},
        {"hierarchical path resolution", fun hierarchical_path_resolution_test/1}
    ]).
```

### benchmark_suite_test_

```erlang
benchmark_suite_test_() ->
    generate_test_suite([
        {"benchmark key read write", fun benchmark_key_read_write/1},
        {"benchmark list", fun benchmark_list/1},
        {"benchmark message read write", fun benchmark_message_read_write/1}
    ]).
```

### benchmark_key_read_write

Benchmark a store. By default, we write 10,000 keys and read 10,000

```erlang
benchmark_key_read_write(Store = #{ <<"benchmark-scale">> := Scale }) ->
    benchmark_key_read_write(
        Store,
        erlang:ceil(Scale * ?STORE_BENCH_WRITE_OPS), 
        erlang:ceil(Scale * ?STORE_BENCH_READ_OPS)
    );
```

### benchmark_key_read_write

Benchmark a store. By default, we write 10,000 keys and read 10,000

```erlang
benchmark_key_read_write(Store) ->
    benchmark_key_read_write(Store, ?STORE_BENCH_WRITE_OPS, ?STORE_BENCH_READ_OPS).
```

### benchmark_key_read_write

```erlang
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
```

### benchmark_list

```erlang
benchmark_list(Store = #{ <<"benchmark-scale">> := Scale }) ->
    benchmark_list(
        Store,
        erlang:ceil(Scale * ?STORE_BENCH_LIST_KEYS),
        erlang:ceil(Scale * ?STORE_BENCH_LIST_OPS),
        erlang:ceil(Scale * ?STORE_BENCH_LIST_GROUP_SIZE)
    );
```

### benchmark_list

```erlang
benchmark_list(Store) ->
    benchmark_list(
        Store,
        ?STORE_BENCH_LIST_KEYS,
        ?STORE_BENCH_LIST_OPS,
        ?STORE_BENCH_LIST_GROUP_SIZE
    ).
```

### benchmark_list

```erlang
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
```

### benchmark_message_read_write

```erlang
benchmark_message_read_write(Store = #{ <<"benchmark-scale">> := Scale }) ->
    benchmark_message_read_write(
        Store,
        erlang:ceil(Scale * ?BENCH_MSG_WRITE_OPS),
        erlang:ceil(Scale * ?BENCH_MSG_READ_OPS)
    );
```

### benchmark_message_read_write

```erlang
benchmark_message_read_write(Store) ->
    benchmark_message_read_write(Store, ?BENCH_MSG_WRITE_OPS, ?BENCH_MSG_READ_OPS).
```

### benchmark_message_read_write

```erlang
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
```

### read_only_access_test

Test that read-only stores allow read operations but block write operations

```erlang
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
```

### write_only_access_test

Test that write-only stores allow write operations but block read operations  

```erlang
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
```

### admin_only_access_test

Test admin-only stores for start/stop/reset operations

```erlang
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
```

### multi_access_permissions_test

Test multiple access permissions

```erlang
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
```

### store_access_list_test

Test access control with a list of stores.

```erlang
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
```

### invalid_access_permissions_test

Test invalid access permissions are ignored

```erlang
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
```

### list_access_control_test

Test list operations with access control

```erlang
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
```

### make_link_access_test

Test make_link operations with write access

```erlang
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
```

---

*Generated from [hb_store.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store.erl)*
