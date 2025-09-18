# hb_store_lru

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_lru.erl)

An in-memory store implementation, following the `hb_store` behavior
and interface. This implementation uses a least-recently-used cache first,
and offloads evicted data to a specified non-volatile store over time.
This cache is registered under `{in_memory, HTTPServerID}`, in `hb_name`
so that all processes that are executing using the HTTP server’s Opts
can find it quickly.
The least-recently-used strategy (first is the most recent used, last is the
least recently used) is implemented by keeping track of the order and bytes
 on ets tables:
- A cache table containing all the entries along with the value size and
  key index.
- A cache indexing table containing all the index pointing to the keys. The
  IDs are then sorted to ease the eviction policy.
- A cache statistics table containing all the information about the cache
  size, capacity, and indexing.

---

## Exported Functions

- `list/2`
- `make_group/2`
- `make_link/3`
- `read/2`
- `reset/1`
- `resolve/2`
- `scope/1`
- `start/1`
- `stop/1`
- `type/2`
- `write/3`

---

### start

An in-memory store implementation, following the `hb_store` behavior
The default capacity is used when no capacity is provided in the store
Maximum number of retries when fetching cache entries that aren't
Start the LRU cache.

```erlang
start(StoreOpts = #{ <<"name">> := Name }) ->
    ?event(cache_lru, {starting_lru_server, Name}),
    From = self(),
    spawn(
        fun() ->
            State = init(From, StoreOpts),
            server_loop(State, StoreOpts)
        end
    ),
    receive
        {ok, InstanceMessage} -> {ok, InstanceMessage}
    end.
```

### init

Create the `ets` tables for the LRU cache:

```erlang
init(From, StoreOpts) ->
    % Start the persistent store.
```

### stop

Stop the LRU in memory by offloading the keys in the ETS tables

```erlang
stop(Opts) ->
    ?event(cache_lru, {stopping_lru_server, Opts}),
    #{ <<"pid">> := CacheServer } = hb_store:find(Opts),
    CacheServer ! {stop, self(), Ref = make_ref()},
    receive
        {ok, Ref} -> ok
    end.
```

### scope

The LRU store is always local, for now.
Reset the store by completely cleaning the ETS tables and

```erlang
scope(_) -> local.
```

### reset

The LRU store is always local, for now.
Reset the store by completely cleaning the ETS tables and

```erlang
reset(Opts) ->
    #{ <<"pid">> := CacheServer } = hb_store:find(Opts),
    CacheServer ! {reset, self(), Ref = make_ref()},
    receive
        {ok, Ref} ->
            ?event({reset_store, {in_memory, CacheServer}}),
            case get_persistent_store(Opts) of
                no_store ->
                    ok;
                Store ->
                    hb_store:reset(Store)
            end
    end.
```

### server_loop

```erlang
server_loop(State =
                #{cache_table := CacheTable,
                  stats_table := StatsTable,
                  index_table := IndexTable},
            Opts) ->
    receive
        {sync, From} ->
            From ! {ok, self()},
            server_loop(State, Opts);
        {get_cache_table, From} ->
            From ! CacheTable;
        {put, Key, Value, From, Ref} ->
            put_cache_entry(State, Key, Value, Opts),
            ?event(debug_lru, {put, {key, Key}, {value, Value}}),
            From ! {ok, Ref};
        {link, Existing, New, From, Ref} ->
            link_cache_entry(State, Existing, New, Opts),
            From ! {ok, Ref};
        {make_group, Key, From, Ref} ->
            ?event(debug_lru, {make_group, Key}),
            ensure_dir(State, Key),
            From ! {ok, Ref};
        {update_recent, Key, Entry, From, Ref} ->
            update_recently_used(State, Key, Entry),
            From ! {ok, Ref};
        {reset, From, Ref} ->
            ets:delete_all_objects(CacheTable),
            ets:delete_all_objects(StatsTable),
            ets:delete_all_objects(IndexTable),
            From ! {ok, Ref};
        {stop, From, Ref} ->
            evict_all_entries(State, Opts),
            From ! {ok, Ref},
            exit(self(), ok)
    end,
    server_loop(State, Opts).
```

### sync

Force the caller to wait until the server has fully processed all 

```erlang
sync(Server) ->
    Server ! {sync, self()},
    receive
        {ok, Server} -> ok
    end.
```

### write

Write an entry in the cache.

```erlang
write(Opts, RawKey, Value) ->
    Key = hb_store:join(RawKey),
    #{ <<"pid">> := CacheServer } = hb_store:find(Opts),
    CacheServer ! {put, Key, Value, self(), Ref = make_ref()},
    receive
        {ok, Ref} -> ok
    end.
```

### read

Retrieve value in the cache from the given key.

```erlang
read(Opts, RawKey) ->
    #{ <<"pid">> := Server } = hb_store:find(Opts),
    Key = resolve(Opts, RawKey),
    case fetch_cache_with_retry(Opts, Key) of
        nil ->
            case get_persistent_store(Opts) of
                no_store ->
                    not_found;
                PersistentStore ->
                    % FIXME: It might happens some links can be in LRU while data on 
                    % the permanent store and resolve doesn't produce the same key.
```

### resolve

```erlang
resolve(Opts, Key) ->
    Res = resolve(Opts, "", hb_path:term_to_path_parts(hb_store:join(Key), Opts)),
    ?event({resolved, Key, Res}),
    Res.
```

### resolve

```erlang
resolve(_, CurrPath, []) ->
    hb_store:join(CurrPath);
```

### resolve

```erlang
resolve(Opts, CurrPath, [Next|Rest]) ->
    PathPart = hb_store:join([CurrPath, Next]),
    ?event(
        {resolving,
            {accumulated_path, CurrPath},
            {next_segment, Next},
            {generated_partial_path_to_test, PathPart}
        }
    ),
    case fetch_cache_with_retry(Opts, PathPart) of
        {link, Link} ->
            resolve(Opts, Link, Rest);
        _ ->
            resolve(Opts, PathPart, Rest)
    end.
```

### make_link

Make a link from a key to another in the store.

```erlang
make_link(_, Link, Link) ->
    ok;
```

### make_link

Make a link from a key to another in the store.

```erlang
make_link(Opts, RawExisting, New) ->
    #{ <<"pid">> := Server } = hb_store:find(Opts),
    ExistingKeyBin = convert_if_list(RawExisting),
    NewKeyBin = convert_if_list(New),
    case fetch_cache_with_retry(Opts, ExistingKeyBin) of
        nil ->
            case get_persistent_store(Opts) of
                no_store ->
                    not_found;
                Store ->
                    hb_store:make_link(Store, ExistingKeyBin, NewKeyBin)
            end;
        _ ->
            Server ! {link, ExistingKeyBin, NewKeyBin, self(), Ref = make_ref()},
            receive
                {ok, Ref} ->
                    ok
            end
    end.
```

### list

List all the keys registered.

```erlang
list(Opts, Path) ->
    PersistentKeys =
        case get_persistent_store(Opts) of
            no_store ->
                not_found;
            Store ->
                ResolvedPath = hb_store:resolve(Store, Path),
                case hb_store:list(Store, ResolvedPath) of
                    {ok, Keys} -> Keys;
                    not_found -> not_found
                end
        end,
    case {ets_keys(Opts, Path), PersistentKeys} of
        {not_found, not_found} ->
            not_found;
        {InMemoryKeys, not_found} ->
            {ok, InMemoryKeys};
        {not_found, PersistentKeys} ->
            {ok, PersistentKeys};
        {InMemoryKeys, PersistentKeys} ->
            {ok, hb_util:unique(InMemoryKeys ++ PersistentKeys)}
    end.
```

### ets_keys

List all of the keys in the store for a given path, supporting a special

```erlang
ets_keys(Opts, <<"">>) -> ets_keys(Opts, <<"/">>);
```

### ets_keys

List all of the keys in the store for a given path, supporting a special

```erlang
ets_keys(Opts, <<"/">>) ->
    #{ <<"cache-table">> := Table } = hb_store:find(Opts),
    table_keys(Table, undefined);
```

### ets_keys

List all of the keys in the store for a given path, supporting a special

```erlang
ets_keys(Opts, Path) ->
    case fetch_cache_with_retry(Opts, Path) of
        {group, Set} ->
            sets:to_list(Set);
        {link, Link} ->
            list(Opts, Link);
        {raw, #{value := Value}} when is_map(Value) ->
            maps:keys(Value);
        {raw, #{value := Value}} when is_list(Value) ->
            Value;
        nil ->
            not_found
    end.
```

### type

Determine the type of a key in the store.

```erlang
type(Opts, Key) ->
    case fetch_cache_with_retry(Opts, Key) of
        nil ->
            case get_persistent_store(Opts) of
                no_store ->
                    not_found;
                Store ->
                    ResolvedKey = hb_store:resolve(Store, Key),
                    hb_store:type(Store, ResolvedKey)
            end;
        {raw, _} ->
            simple;
        {link, NewKey} ->
            type(Opts, NewKey);
        {group, _Item} ->
            composite
    end.
```

### make_group

Create a directory inside the store.

```erlang
make_group(Opts, Key) ->
    #{ <<"pid">> := Server } = hb_store:find(Opts),
    Server ! {make_group, Key, self(), Ref = make_ref()},
    receive
        {ok, Ref} ->
            ok
    end.
```

### table_keys

```erlang
table_keys(TableName) ->
    table_keys(TableName, undefined).
```

### table_keys

```erlang
table_keys(TableName, Prefix) ->
    FirstKey = ets:first(TableName),
    table_keys(TableName, FirstKey, Prefix, []).
```

### table_keys

```erlang
table_keys(_TableName, '$end_of_table', _Prefix, Acc) ->
    Acc;
```

### table_keys

```erlang
table_keys(TableName, CurrentKey, Prefix, Acc) ->
    NextKey = ets:next(TableName, CurrentKey),
    case Prefix of
        undefined ->
            table_keys(TableName, NextKey, Prefix, [CurrentKey | Acc]);
        _ ->
            PrefixParts = hb_path:term_to_path_parts(Prefix),
            Key = hb_path:term_to_path_parts(CurrentKey),
            case lists:prefix(PrefixParts, Key) of
                true ->
                    Extracted = lists:nthtail(length(PrefixParts), Key),
                    table_keys(
                        TableName,
                        NextKey,
                        Prefix,
                        [hb_path:to_binary(Extracted) | Acc]
                    );
                false ->
                    table_keys(TableName, NextKey, Prefix, Acc)
            end
    end.
```

### get_cache_entry

```erlang
get_cache_entry(#{cache_table := Table}, Key) ->
    get_cache_entry(Table, Key);
```

### get_cache_entry

```erlang
get_cache_entry(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            nil;
        [{_, Entry}] ->
            Entry
    end.
```

### fetch_cache_with_retry

```erlang
fetch_cache_with_retry(Opts, Key) ->
    fetch_cache_with_retry(Opts, Key, 1).
```

### fetch_cache_with_retry

```erlang
fetch_cache_with_retry(Opts, Key, Retries) ->
    #{<<"cache-table">> := Table, <<"pid">> := Server} = hb_store:find(Opts),
    case get_cache_entry(Table, Key) of
        nil ->
            case Retries < ?RETRY_THRESHOLD of
                true ->
                    sync(Server),
                    fetch_cache_with_retry(Opts, Key, Retries + 1);
                false ->
                    nil
            end;
        Entry ->
            Entry
    end.
```

### put_cache_entry

```erlang
put_cache_entry(State, Key, Value, Opts) ->
    ValueSize = erlang:external_size(Value),
    CacheSize = cache_size(State),
    ?event(cache_lru, {putting_entry, {size, ValueSize}, {opts, Opts}, {cache_size, CacheSize}}),
    Capacity = hb_maps:get(<<"capacity">>, Opts, ?DEFAULT_LRU_CAPACITY),
    case get_cache_entry(State, Key) of
        nil ->
            % For new entries, we check if the size will the fit the full
            % capacity (even by evicting keys).
```

### handle_group

```erlang
handle_group(State, Key, Opts) ->
    case filename:dirname(hb_store:join(Key)) of
        <<".">> -> undefined ;
        BaseDir ->
            case maps:get(mode, Opts, undefined) of
                offload ->
                    Store = get_persistent_store(Opts),
                    ?event(cache_lru, {create_group, BaseDir}),
                    hb_store:make_group(Store, BaseDir),
                    BaseDir;
              undefined -> 
                    ensure_dir(State, BaseDir),
                    {group, Entry} = get_cache_entry(State, BaseDir),
                    BaseName = filename:basename(Key),
                    NewGroup = append_key_to_group(BaseName, Entry),
                    add_cache_entry(State, BaseDir, {group, NewGroup}),
                    BaseDir
            end
    end.
```

### ensure_dir

```erlang
ensure_dir(State, Path) ->
    PathParts = hb_path:term_to_path_parts(Path),
    [First | Rest] = PathParts,
    Result = ensure_dir(State, First, Rest),
    Result.
```

### ensure_dir

```erlang
ensure_dir(State, CurrentPath, []) ->
    maybe_create_dir(State, CurrentPath, nil);
```

### ensure_dir

```erlang
ensure_dir(State, CurrentPath, [Next]) ->
    maybe_create_dir(State, CurrentPath, Next),
    ensure_dir(State, hb_store:join([CurrentPath, Next]), []);
```

### ensure_dir

```erlang
ensure_dir(State, CurrentPath, [Next | Rest]) ->
    maybe_create_dir(State, CurrentPath, Next),
    ensure_dir(State, hb_store:join([CurrentPath, Next]), Rest).
```

### maybe_create_dir

```erlang
maybe_create_dir(State, DirPath, Value) ->
    CurrentValueSet =
        case get_cache_entry(State, DirPath) of
            nil ->
                sets:new();
            {group, CurrentValue} ->
                CurrentValue
        end,
    NewValueSet =
        case Value of
            nil ->
                CurrentValueSet;
            _ ->
                sets:add_element(Value, CurrentValueSet)
        end,
    ?event(cache_lru, {create_group, DirPath, sets:to_list(NewValueSet)}),
    add_cache_entry(State, DirPath, {group, NewValueSet}).
```

### append_key_to_group

```erlang
append_key_to_group(Key, Group) ->
    BaseName = filename:basename(Key),
    sets:add_element(BaseName, Group).
```

### assign_new_entry

```erlang
assign_new_entry(State, Key, Value, ValueSize, Capacity, Group, Opts) ->
    case cache_size(State) + ValueSize >= Capacity of
        true ->
            ?event(cache_lru, eviction_required),
            evict_oldest_entry(State, ValueSize, Opts);
        false ->
            ok
    end,
    ID = get_index_id(State),
    add_cache_index(State, ID, Key),
    add_cache_entry(
        State,
        Key,
        {raw,
            #{
                value => Value,
                id => ID,
                size => ValueSize,
                group => Group
            }
        }
    ),
    increase_cache_size(State, ValueSize).
```

### cache_size

```erlang
cache_size(#{stats_table := Table}) ->
    case ets:lookup(Table, size) of
        [{_, Size}] ->
            Size;
        _ ->
            0
    end.
```

### get_index_id

```erlang
get_index_id(#{stats_table := StatsTable}) ->
    ets:update_counter(StatsTable, id, {2, 1}, {0, 0}).
```

### add_cache_entry

```erlang
add_cache_entry(#{cache_table := Table}, Key, Value) ->
    ets:insert(Table, {Key, Value}).
```

### add_cache_index

```erlang
add_cache_index(#{index_table := Table}, ID, Key) ->
    ets:insert(Table, {ID, Key}).
```

### link_cache_entry

```erlang
link_cache_entry(State = #{cache_table := Table}, Existing, New, Opts) ->	
    ?event(cache_lru, {link, Existing, New}),
    % Remove the link from the previous linked entry
    clean_old_link(Table, New),
    _ = handle_group(State, New, Opts),
    ets:insert(Table, {New, {link, Existing}}),
    % Add links to the linked entry
    case ets:lookup(Table, Existing) of
        [{_, {raw, Entry}}] ->
            NewLinks =
                case Entry of
                    #{links := ExistingLinks} ->
                        [New | ExistingLinks];
                    _ ->
                        [New]
                end,
            ets:insert(Table, {Existing, {raw, Entry#{links => NewLinks}}});
        _ ->
            ignore
    end.
```

### clean_old_link

Remove the link association for the the old linked data to the given key

```erlang
clean_old_link(Table, Link) ->
    case ets:lookup(Table, Link) of
        [{_, {link, PreviousEntry}}] ->
            ?event(cache_lru, {removing_previous_link,
                {link, Link},
                {previous_entry, PreviousEntry}
            }),
            case ets:lookup(Table, PreviousEntry) of
                [{_, {raw, OldEntry}}] ->
                    Links = sets:from_list(maps:get(links, OldEntry, [])),
                    UpdatedLinks = sets:del_element(Link, Links),
                    UpdatedEntry = maps:put(
                        links,
                        sets:to_list(UpdatedLinks),
                        OldEntry
                    ),
                    ets:insert(Table, {PreviousEntry, {raw, UpdatedEntry}});
                _ ->
                    skip
            end;
        _ -> skip
    end.
```

### increase_cache_size

```erlang
increase_cache_size(#{stats_table := StatsTable}, ValueSize) ->
    ets:update_counter(StatsTable, size, {2, ValueSize}, {0, 0}).
```

### evict_oldest_entry

```erlang
evict_oldest_entry(State, ValueSize, Opts) ->
    evict_oldest_entry(State, ValueSize, 0, Opts).
```

### evict_oldest_entry

```erlang
evict_oldest_entry(_State, ValueSize, FreeSize, _Opts) when FreeSize >= ValueSize ->
    ok;
```

### evict_oldest_entry

```erlang
evict_oldest_entry(State, ValueSize, FreeSize, Opts) ->
    case cache_tail_key(State) of
        nil ->
            ok;
        TailKey ->
            Entry = #{
                size := ReclaimedSize,
                id := ID,
                value := TailValue,
                group := Group
            } = case get_cache_entry(State, TailKey) of
                nil ->
                    % Raises a runtime error as this represents
                    % a non-recoverable error. This would signifies a
                    % inconsistency between the index and the cache table.
```

### evict_all_entries

```erlang
evict_all_entries(#{cache_table := Table}, Opts) ->
    lists:foreach(
        fun(Key) ->
            [{_, {raw, Entry}}] = ets:lookup(Table, Key),
            #{ value := Value, group := Group } = Entry,
            Links = maps:get(links, Entry, []),
            offload_to_store(Key, Value, Links, Group, Opts)
        end,
        table_keys(Table)
    ).
```

### offload_to_store

```erlang
offload_to_store(TailKey, TailValue, Links, Group, Opts) ->
    ?event(lru_offload, {offloading_to_store, Opts}),
    FoundStore = get_persistent_store(Opts),
    ?event(lru_offload, {found_store, FoundStore}),
    case FoundStore of
        no_store ->
            ok;
        Store ->
            case Group of
                undefined ->
                    ignore;
                _ ->
                    hb_store:make_group(Store, Group)
            end,
            case hb_store:write(Store, TailKey, TailValue) of
                ok ->
                    lists:foreach(
                        fun(Link) ->
                            ResolvedPath = resolve(Opts, Link),
                            hb_store:make_link(Store, ResolvedPath, Link)
                        end,
                        Links
                    ),
                    ?event(cache_lru, {offloaded_key, TailKey}),
                    ok;
                Err ->
                    ?event(warning, {error_offloading_to_local_cache, Err}),
                    {error, Err}
            end
    end.
```

### cache_tail_key

```erlang
cache_tail_key(#{index_table := Table}) ->
    case ets:first(Table) of
        '$end_of_table' ->
            nil;
        FirstID ->
            [{_, Key}] = ets:lookup(Table, FirstID),
            Key
    end.
```

### delete_cache_index

```erlang
delete_cache_index(#{index_table := IndexTable}, ID) ->
    ets:delete(IndexTable, ID).
```

### delete_cache_entry

```erlang
delete_cache_entry(#{cache_table := Table}, Key) ->
    ets:delete(Table, Key),
    ?event(cache_lru, {deleted, Key}).
```

### decrease_cache_size

```erlang
decrease_cache_size(#{stats_table := Table}, Size) ->
    ets:update_counter(Table, size, {2, -Size, 0, 0}).
```

### replace_entry

```erlang
replace_entry(State, Key, Value, ValueSize, {raw, OldEntry = #{ value := OldValue}}) when Value =/= OldValue ->
    % Update entry and move the keys in the front of the cache 
    % as the most used Key
    ?event(debug_lru, {replace_entry, 
        {key, Key},
        {value, Value},
        {explicit, OldEntry}
    }),
    #{size := PreviousSize} = OldEntry,
    NewEntry = OldEntry#{value := Value, size := ValueSize},
    add_cache_entry(State, Key, {raw, NewEntry}),
    update_recently_used(State, Key, NewEntry),
    update_cache_size(State, PreviousSize, ValueSize);
```

### replace_entry

```erlang
replace_entry(_State, _Key, _Value, _ValueSize, {raw, _}) -> ok;
```

### replace_entry

```erlang
replace_entry(_State, _Key, _Value, _ValueSize, {Type, _}) ->
    % Link or group should be handle directly with `make_link` or `make_group`
    % This aim of this function is to be used along with direct data insertion.
```

### update_recently_used

```erlang
update_recently_used(State, Key, Entry) ->
    % Acquire a new ID
    NewID = get_index_id(State),
    % Update the entry's ID
    add_cache_entry(State, Key, {raw, Entry#{id := NewID}}),
    #{id := PreviousID} = Entry,
    % Delete previous ID to priorize the new NewID
    delete_cache_index(State, PreviousID),
    add_cache_index(State, NewID, Key).
```

### update_cache_size

```erlang
update_cache_size(#{stats_table := Table}, PreviousSize, NewSize) ->
    ets:update_counter(Table, size, [{2, -PreviousSize}, {2, NewSize}]).
```

### get_persistent_store

```erlang
get_persistent_store(Opts) ->
    hb_maps:get(
        <<"persistent-store">>,
        Opts,
        no_store
    ).
```

### convert_if_list

```erlang
convert_if_list(Value) when is_list(Value) ->
    join(Value);  % Perform the conversion if it's a list
```

### convert_if_list

```erlang
convert_if_list(Value) ->
    Value.
```

### join

```erlang
join(Key) when is_list(Key) ->
    KeyList = hb_store:join(Key),
    maybe_convert_to_binary(KeyList);
```

### join

```erlang
join(Key) when is_binary(Key) -> Key.
```

### maybe_convert_to_binary

```erlang
maybe_convert_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
```

### maybe_convert_to_binary

```erlang
maybe_convert_to_binary(Value) when is_binary(Value) ->
    Value.
```

### test_opts

Generate a set of options for testing. The default is to use an `fs`

```erlang
test_opts(PersistentStore) ->
    test_opts(PersistentStore, 1000000).
```

### test_opts

```erlang
test_opts(PersistentStore, Capacity) ->
    % Set the server ID to a random address.
```

### unknown_value_test

```erlang
unknown_value_test() ->
    ?assertEqual(not_found, read(test_opts(default), <<"key1">>)).
```

### cache_term_test

```erlang
cache_term_test() ->
    StoreOpts = test_opts(default),
    write(StoreOpts, <<"key1">>, <<"Hello">>),
    ?assertEqual({ok, <<"Hello">>}, read(StoreOpts, <<"key1">>)).
```

### evict_oldest_items_test

```erlang
evict_oldest_items_test() ->
    StoreOpts = test_opts(no_store, 500),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    write(StoreOpts, <<"key2">>, Binary),
    read(StoreOpts, <<"key1">>),
    write(StoreOpts, <<"key3">>, Binary),
    ?assertEqual({ok, Binary}, read(StoreOpts, <<"key1">>)),
    ?assertEqual(not_found, read(StoreOpts, <<"key2">>)).
```

### evict_items_with_insufficient_space_test

```erlang
evict_items_with_insufficient_space_test() ->
    StoreOpts = test_opts(no_store, 500),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    write(StoreOpts, <<"key2">>, Binary),
    write(StoreOpts, <<"key3">>, crypto:strong_rand_bytes(400)),
    ?assertEqual(not_found, read(StoreOpts, <<"key1">>)),
    ?assertEqual(not_found, read(StoreOpts, <<"key2">>)).
```

### evict_but_able_to_read_from_fs_store_test

```erlang
evict_but_able_to_read_from_fs_store_test() ->
    StoreOpts = test_opts(default, 500),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    write(StoreOpts, <<"key2">>, Binary),
    read(StoreOpts, <<"key1">>),
    write(StoreOpts, <<"key3">>, Binary),
    ?assertEqual({ok, Binary}, read(StoreOpts, <<"key1">>)),
    ?assertEqual({ok, Binary}, read(StoreOpts, <<"key2">>)),
    % Directly offloads if the data is more than the LRU capacity
    write(StoreOpts, <<"sub/key">>, crypto:strong_rand_bytes(600)),
    ?assertMatch({ok, _}, read(StoreOpts, <<"sub/key">>)).
```

### stop_test

```erlang
stop_test() ->
    StoreOpts = test_opts(default, 500),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    write(StoreOpts, <<"key2">>, Binary),
    #{ <<"pid">> := ServerPID } = hb_store:find(StoreOpts),
    ok = stop(StoreOpts),
    ?assertEqual(false, is_process_alive(ServerPID)),
    PersistentStore = hb_maps:get(<<"persistent-store">>, StoreOpts),
    ?assertEqual({ok, Binary}, hb_store:read(PersistentStore, <<"key1">>)),
    ?assertEqual({ok, Binary}, hb_store:read(PersistentStore, <<"key2">>)).
```

### reset_test

```erlang
reset_test() ->
    StoreOpts = test_opts(default),
    write(StoreOpts, <<"key1">>, <<"Hello">>),
    write(StoreOpts, <<"key2">>, <<"Hi">>),
    reset(StoreOpts),
    ?assertEqual(not_found, read(StoreOpts, <<"key1">>)),
    #{ <<"cache-table">> := Table } = hb_store:find(StoreOpts),
    ?assertEqual([], ets:tab2list(Table)).
```

### list_test

```erlang
list_test() ->
    StoreOpts = test_opts(default, 500),
    Binary = crypto:strong_rand_bytes(200),
    make_group(StoreOpts, <<"sub">>),
    write(StoreOpts, <<"hello">>, <<"world">>),
    write(StoreOpts, <<"sub/key1">>, Binary),
    write(StoreOpts, <<"sub/key2">>, Binary),
    {ok, Keys1} = list(StoreOpts, <<"sub">>),
    ?assertEqual([<<"key1">>, <<"key2">>], lists:sort(Keys1)),
    write(StoreOpts, <<"sub/key3">>, Binary),
    {ok, Keys2} = list(StoreOpts, <<"sub">>),
    ?assertEqual(
        [<<"key1">>, <<"key2">>, <<"key3">>],
        lists:sort(Keys2)
    ),
    write(StoreOpts, <<"sub/inner/key1">>, Binary),
    {ok, Keys3} = list(StoreOpts, <<"sub">>),
    ?assertEqual([<<"inner">>, <<"key1">>, <<"key2">>, <<"key3">>],
                 lists:sort(Keys3)),
    write(StoreOpts, <<"complex">>, #{<<"a">> => 10, <<"b">> => Binary}),
    ?assertEqual({ok, [<<"a">>, <<"b">>]}, list(StoreOpts, <<"complex">>)).
```

### type_test

```erlang
type_test() ->
    StoreOpts = test_opts(default, 500),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    ?assertEqual(simple, type(StoreOpts, <<"key1">>)),
    write(StoreOpts, <<"sub/key1">>, Binary),
    ?assertEqual(composite, type(StoreOpts, <<"sub">>)),
    make_link(StoreOpts, <<"key1">>, <<"keylink">>),
    ?assertEqual(simple, type(StoreOpts, <<"keylink">>)).
```

### replace_link_test

```erlang
replace_link_test() ->
    StoreOpts = test_opts(default),
    write(StoreOpts, <<"key1">>, <<"Hello">>),
    make_link(StoreOpts, <<"key1">>, <<"keylink">>),
    ?assertEqual({ok, <<"Hello">>}, read(StoreOpts, <<"keylink">>)),
    write(StoreOpts, <<"key2">>, <<"Hello2">>),
    make_link(StoreOpts, <<"key2">>, <<"keylink">>),
    ?assertEqual({ok, <<"Hello2">>}, read(StoreOpts, <<"keylink">>)),
    #{ <<"cache-table">> := Table } = hb_store:find(StoreOpts),
    {raw, #{links := Links }}= get_cache_entry(Table, <<"key1">>),
```

---

*Generated from [hb_store_lru.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_lru.erl)*
