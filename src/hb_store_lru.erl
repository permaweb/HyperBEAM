%%% @doc An in-memory store implementation, following the `hb_store` behavior
%%% and interface. This implementation uses a least-recently-used cache first,
%%% and offloads evicted data to a specified non-volatile store over time.
%%%
%%% This cache is registered under `{in_memory, HTTPServerID}`, in `hb_name`
%%% so that all processes that are executing using the HTTP server’s Opts
%%% can find it quickly.
%%%
%%% The least-recently-used strategy (first is the most recent used, last is the
%%% least recently used) is implemented by keeping track of the order and bytes
%%%  on ets tables:
%%% - A cache table containing all the entries along with the value size and
%%%   key index.
%%% - A cache indexing table containing all the index pointing to the keys. The
%%%   IDs are then sorted to ease the eviction policy.
%%% - A cache statistics table containing all the information about the cache
%%%   size, capacity, and indexing.
-module(hb_store_lru).
-export([start/1, stop/1, reset/1, scope/1]).
-export([write/3, read/2, list/2, type/2, make_link/3, make_group/2, resolve/2]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% @doc The default server ID is used when no server ID is provided in the
%%% store options or in the process dictionary. In such cases, every LRU cache
%%% will be shared across non-ID-specific LRU stores.
-define(DEFAULT_LRU_ID, <<"global">>).

%%% @doc The default capacity is used when no capacity is provided in the store
%%% options.
-define(DEFAULT_LRU_CAPACITY, 100_000_000).

%% @doc The server ID is either found in the `StoreOpts` or in the process
%% dictionary, using the `server_id' key. This is typically set by the HTTP 
%% server when a worker is spawned for a request, but can be overridden by 
%% explicitly setting the `server_id` in the store definition. It is expected
%% to be a binary.
find_id(StoreOpts) ->
    case hb_maps:get(<<"server-id">>, StoreOpts, get(server_id)) of
        undefined ->
            ?DEFAULT_LRU_ID;
        ServerID ->
            ServerID
    end.

%% @doc Find the Process ID of the LRU cache server.
find_pid(StoreOpts) ->
    ServerID = find_id(StoreOpts),
    PID =
        case erlang:get({cache_lru, ServerID}) of
            undefined ->
                case hb_name:lookup({in_memory, ServerID}) of
                    undefined ->
                        {ok, ServerPID} = start(StoreOpts),
                        ServerPID;
                    FoundPID ->
                        ?event(x, {registered, {server, ServerID}, {pid, FoundPID}}),
                        FoundPID
                end;
            FoundPID ->
                ?event(x, {in_process, {server, ServerID}, {pid, FoundPID}}),
                FoundPID
        end,
    cache_pid(ServerID, PID),
    PID.

%% @doc Store the PID in the process dictionary for rapid lookup.
cache_pid(ServerID, PID) ->
    erlang:put({cache_lru, ServerID}, PID).

clean_cache_pid(ServerPID) ->
    erlang:erase({cache_lru, ServerPID}).

%% @doc Start the LRU cache.
start(Opts) ->
    ServerID = find_id(Opts),
    NormOpts = Opts#{ <<"server-id">> => ServerID },
    ?event(cache_lru, {starting_lru_server, ServerID}),
    From = self(),
    spawn(
        fun() ->
            State = init(From, NormOpts),
            server_loop(State, NormOpts)
        end
    ),
    receive
        {ok, PID} -> {ok, PID}
    end.

%% @doc Create the `ets' tables for the LRU cache:
%% - The cache of data itself (public, with read concurrency enabled)
%% - A set for the LRU's stats.
%% - An ordered set for the cache's index.
init(From, Opts) ->
    ServerID = find_id(Opts),
    % Start the persistent store.
    case hb_maps:get(<<"persistent-store">>, Opts, no_store) of
        no_store -> ok;
        Store -> hb_store:start(Store)
    end,
    % Create LRU tables
    CacheTable = ets:new(hb_cache_lru, [
        set,
        protected,
        {read_concurrency, true}
    ]),
    CacheStatsTable = ets:new(hb_cache_lru_stats, [set]),
    CacheIndexTable = ets:new(hb_cache_lru_index, [ordered_set]),
    hb_name:register({in_memory, ServerID}),
    persistent_term:put(
        {in_memory_lru_cache, ServerID},
        #{cache_table => CacheTable}
    ),
    From ! {ok, self()},
    #{
        cache_table => CacheTable,
        stats_table => CacheStatsTable,
        index_table => CacheIndexTable
    }.

%% @doc Stop the LRU in memory by offloading the keys in the ETS tables
%% before exiting the process.
stop(Opts) ->
    ?event(cache_lru, {stopping_lru_server, Opts}),
    CacheServer = find_pid(Opts),
    CacheServer ! {stop, self()},
    receive
      ok ->
        clean_cache_pid(find_id(Opts)),
        ok
    end.

%% @doc The LRU store is always local, for now.
scope(_) -> local.

%% @doc Reset the store by completely cleaning the ETS tables and
%% delegate the reset to the underlying offloading store.
reset(Opts) ->
    CacheServer = find_pid(Opts),
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

server_loop(State =
                #{cache_table := CacheTable,
                  stats_table := StatsTable,
                  index_table := IndexTable},
            Opts) ->
    receive
        {get_cache_table, From} ->
            From ! CacheTable;
        {put, Key, Value, From, Ref} ->
            put_cache_entry(State, Key, Value, Opts),
            ?event(debug_lru, {put, {key, Key}, {value, Value}}),
            From ! {ok, Ref};
        {link, Existing, New, From, Ref} ->
            link_cache_entry(State, Existing, New),
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
        {stop, From} ->
            evict_all_entries(State, Opts),
            persistent_term:erase(
                {in_memory_lru_cache, find_id(Opts)}
            ),
            From ! ok,
            exit(self(), ok)
    end,
    server_loop(State, Opts).

%% @doc Write an entry in the cache.
%%
%% After writing, the LRU is updated by moving the key in the most-recently-used
%% key to cycle and re-prioritize cache entry.
write(Opts, RawKey, Value) ->
    Key = hb_store:join(RawKey),
    CacheServer = find_pid(Opts),
    CacheServer ! {put, Key, Value, self(), Ref = make_ref()},
    receive
        {ok, Ref} -> ok
    end.

%% @doc Retrieve value in the cache from the given key.
%% Because the cache uses LRU, the key is moved on the most recent used key to
%% cycle and re-prioritize cache entry.
read(Opts, RawKey) ->
    ServerID = find_id(Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    Key = resolve(CacheTables, RawKey),
    case get_cache_entry(CacheTables, Key) of
        nil ->
            case get_persistent_store(Opts) of
                no_store ->
                    not_found;
                PersistentStore ->
                    hb_cache:read(Key, #{ store => PersistentStore })
            end;
        {raw, Entry = #{value := Value}} ->
            CacheServer = find_pid(Opts),
            CacheServer ! {update_recent, Key, Entry, self(), Ref = make_ref()},
            receive
                {ok, Ref} -> {ok, Value}
            end;
        {link, Link} ->
            ?event({link_found, RawKey, Link}),
            read(Opts, Link);
        _ -> not_found
    end.

resolve(Opts, Key) ->
    Res = resolve(Opts, "", hb_path:term_to_path_parts(hb_store:join(Key), Opts)),
    ?event({resolved, Key, Res}),
    Res.

resolve(_, CurrPath, []) ->
    hb_store:join(CurrPath);
resolve(Opts, CurrPath, [Next|Rest]) ->
    PathPart = hb_store:join([CurrPath, Next]),
    ?event(
        {resolving,
            {accumulated_path, CurrPath},
            {next_segment, Next},
            {generated_partial_path_to_test, PathPart}
        }
    ),
    ServerID = find_id(Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    case get_cache_entry(CacheTables, PathPart) of
        {link, Link} ->
            resolve(Opts, Link, Rest);
        _ ->
            resolve(Opts, PathPart, Rest)
    end.

%% @doc Make a link from a key to another in the store.
make_link(_, Link, Link) ->
    ok;
make_link(Opts, RawExisting, New) ->
    Existing = hb_store:join(RawExisting),
    ServerID = find_id(Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    ExistingKeyBin = convert_if_list(RawExisting),
    NewKeyBin = convert_if_list(New),
    case get_cache_entry(CacheTables, ExistingKeyBin) of
        nil ->
            case get_persistent_store(Opts) of
                no_store ->
                    no_viable_store;
                Store ->
                    hb_store:make_link(Store, ExistingKeyBin, NewKeyBin)
            end;
        _ ->
            CacheServer = find_pid(Opts),
            CacheServer ! {link, Existing, New, self(), Ref = make_ref()},
            receive
                {ok, Ref} ->
                    ok
            end
    end.

%% @doc List all the keys registered.
list(Opts, Path) ->
    ServerID = find_id(Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    InMemoryKeys =
        case get_cache_entry(CacheTables, Path) of
            {group, Set} ->
                sets:to_list(Set);
            {link, Link} ->
                list(Opts, Link);
            {raw, #{value := Value}} when is_map(Value) ->
                maps:keys(Value);
            {raw, #{value := Value}} when is_list(Value) ->
                Value;
            nil ->
                []
        end,
    case get_persistent_store(Opts) of
        no_store ->
            {ok, InMemoryKeys};
        Store ->
            PersistentKeys =
                lists:map(
                    fun hb_util:bin/1,
                    case hb_store:list(Store, Path) of
                        {error, _} -> [];
                        {ok, Keys} -> Keys;
                        Keys -> Keys
                    end
                ),
            {ok, InMemoryKeys ++ PersistentKeys}
    end.

%% @doc Determine the type of a key in the store.
type(Opts, Key) ->
    ServerID = find_id(Opts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    case get_cache_entry(CacheTables, Key) of
        nil ->
            case get_persistent_store(Opts) of
                no_store ->
                    not_found;
                Store ->
                    hb_store:type(Store, Key)
            end;
        {raw, _} ->
            simple;
        {link, NewKey} ->
            type(Opts, NewKey);
        {group, _Item} ->
            composite
    end.

%% @doc Create a directory inside the store.
make_group(Opts, Key) ->
    CacheServer = find_pid(Opts),
    CacheServer ! {make_group, Key, self(), Ref = make_ref()},
    receive
        {ok, Ref} ->
            ok
    end.

table_keys(TableName) ->
    table_keys(TableName, undefined).

table_keys(TableName, Prefix) ->
    FirstKey = ets:first(TableName),
    table_keys(TableName, FirstKey, Prefix, []).

table_keys(_TableName, '$end_of_table', _Prefix, Acc) ->
    Acc;
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

get_cache_entry(#{cache_table := Table}, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            nil;
        [{_, Entry}] ->
            Entry
    end.

put_cache_entry(State, Key, Value, Opts) ->
    ValueSize = erlang:external_size(Value),
    ?event(cache_lru, {putting_entry, {size, ValueSize}, {opts, Opts}}),
    Capacity = hb_maps:get(<<"capacity">>, Opts, ?DEFAULT_LRU_CAPACITY),
    case ValueSize =< Capacity of
        false ->
            offload_to_store(Key, Value, [], undefined, Opts);
        true ->
            case get_cache_entry(State, Key) of
                nil ->
                    ?event(cache_lru, {assign_entry, Key, Value}),
                    Group = handle_group(State, Key),
                    assign_new_entry(
                        State,
                        Key,
                        Value,
                        ValueSize,
                        Capacity,
                        Group,
                        Opts
                    );
                Entry ->
                    ?event(cache_lru, {replace_entry, Key, Value}),
                    replace_entry(State, Key, Value, ValueSize, Entry)
            end
    end.

handle_group(State, Key) ->
    case filename:dirname(Key) of
        <<".">> -> undefined ;
        BaseDir ->
            ensure_dir(State, BaseDir),
            {group, Entry} = get_cache_entry(State, BaseDir),
            BaseName = filename:basename(Key),
            NewGroup = append_key_to_group(BaseName, Entry),
            add_cache_entry(State, BaseDir, {group, NewGroup}),
            BaseDir
    end.
ensure_dir(State, Path) ->
    PathParts = hb_path:term_to_path_parts(Path),
    [First | Rest] = PathParts,
    Result = ensure_dir(State, First, Rest),
    Result.

ensure_dir(State, CurrentPath, []) ->
    maybe_create_dir(State, CurrentPath, nil);
ensure_dir(State, CurrentPath, [Next]) ->
    maybe_create_dir(State, CurrentPath, Next),
    ensure_dir(State, hb_store:join([CurrentPath, Next]), []);
ensure_dir(State, CurrentPath, [Next | Rest]) ->
    maybe_create_dir(State, CurrentPath, Next),
    ensure_dir(State, hb_store:join([CurrentPath, Next]), Rest).

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

append_key_to_group(Key, Group) ->
    BaseName = filename:basename(Key),
    sets:add_element(BaseName, Group).

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

cache_size(#{stats_table := Table}) ->
    case ets:lookup(Table, size) of
        [{_, Size}] ->
            Size;
        _ ->
            0
    end.

get_index_id(#{stats_table := StatsTable}) ->
    ets:update_counter(StatsTable, id, {2, 1}, {0, 0}).

add_cache_entry(#{cache_table := Table}, Key, Value) ->
    ets:insert(Table, {Key, Value}).

add_cache_index(#{index_table := Table}, ID, Key) ->
    ets:insert(Table, {ID, Key}).

link_cache_entry(State = #{cache_table := Table}, Existing, New) ->	
    ?event(cache_lru, {link, Existing, New}),
    % Remove the link from the previous linked entry
    clean_old_link(Table, New),
    _ = handle_group(State, New),
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

% @doc Remove the link association for the the old linked data to the given key
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

increase_cache_size(#{stats_table := StatsTable}, ValueSize) ->
    ets:update_counter(StatsTable, size, {2, ValueSize}, {0, 0}).

evict_oldest_entry(State, ValueSize, Opts) ->
    evict_oldest_entry(State, ValueSize, 0, Opts).

evict_oldest_entry(_State, ValueSize, FreeSize, _Opts) when FreeSize >= ValueSize ->
    ok;
evict_oldest_entry(State, ValueSize, FreeSize, Opts) ->
    case cache_tail_key(State) of
        nil ->
            ok;
        TailKey ->
            {raw,
                Entry = #{
                    size := ReclaimedSize,
                    id := ID,
                    value := TailValue,
                    group := Group
                }
            } = get_cache_entry(State, TailKey),
            ?event(cache_lru, {evict, TailKey, claiming_size, ReclaimedSize}),
            delete_cache_index(State, ID),
            delete_cache_entry(State, TailKey),
            decrease_cache_size(State, ReclaimedSize),
            Links = maps:get(links, Entry, []),
            case Group of
                undefined ->
                    ignore;
                _ ->
                    {group, GroupSet} = get_cache_entry(State, Group),
                    BaseName = filename:basename(TailKey),
                    UpdatedGroupSet = sets:del_element(BaseName, GroupSet),
                    case sets:size(UpdatedGroupSet) of
                        0 ->
                            delete_cache_entry(State, Group);
                        _ ->
                            add_cache_entry(
                                State,
                                Group,
                                {group, UpdatedGroupSet}
                            )
                    end
            end,
            offload_to_store(TailKey, TailValue, Links, Group, Opts),
            evict_oldest_entry(
                State,
                ValueSize,
                FreeSize + ReclaimedSize,
                Opts
            )
    end.

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

offload_to_store(TailKey, TailValue, Links, Group, Opts) ->
    case get_persistent_store(Opts) of
        no_store ->
            ok;
        Store ->
            case Group of
                undefined ->
                    ignore;
                _ ->
                    hb_store:make_group(Store, Group)
            end,
            case hb_cache:write(TailValue, Opts#{store => Store}) of
                {ok, CacheID} ->
                    hb_cache:link(CacheID, TailKey, Opts#{store => Store}),
                    lists:foreach(
                        fun(Link) ->
                            hb_cache:link(Link, TailKey, Opts#{store => Store})
                        end,
                        Links
                    ),
                    ?event(cache_lru, {offloaded_key, TailKey}),
                    ok;
                {error, Err} ->
                    ?event(warning, {error_offloading_to_local_cache, Err}),
                    {error, Err}
            end
    end.

cache_tail_key(#{index_table := Table}) ->
    case ets:first(Table) of
        '$end_of_table' ->
            nil;
        FirstID ->
            [{_, Key}] = ets:lookup(Table, FirstID),
            Key
    end.

delete_cache_index(#{index_table := IndexTable}, ID) ->
    ets:delete(IndexTable, ID).

delete_cache_entry(#{cache_table := Table}, Key) ->
    ets:delete(Table, Key),
    ?event(cache_lru, {deleted, Key}).

decrease_cache_size(#{stats_table := Table}, Size) ->
    ets:update_counter(Table, size, {2, -Size, 0, 0}).

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
replace_entry(_State, _Key, _Value, _ValueSize, {raw, _}) -> ok;
replace_entry(_State, _Key, _Value, _ValueSize, {Type, _}) ->
    % Link or group should be handle directly with `make_link` or `make_group`
    % This aim of this function is to be used along with direct data insertion.
    throw({error, can_only_replace_raw_entry, {type, Type}}).
update_recently_used(State, Key, Entry) ->
    % Acquire a new ID
    NewID = get_index_id(State),
    #{id := PreviousID} = Entry,
    % Delete previous ID to priorize the new NewID
    delete_cache_index(State, PreviousID),
    add_cache_index(State, NewID, Key),
    % Update the entry's ID
    add_cache_entry(State, Key, {raw, Entry#{id := NewID}}).

update_cache_size(#{stats_table := Table}, PreviousSize, NewSize) ->
    ets:update_counter(Table, size, [{2, -PreviousSize}, {2, NewSize}]).

get_persistent_store(Opts) ->
    hb_maps:get(
        <<"persistent-store">>,
        Opts,
        #{
            <<"store-module">> => hb_store_fs,
            <<"prefix">> => <<"cache-mainnet">>
        }
    ).

convert_if_list(Value) when is_list(Value) ->
    join(Value);  % Perform the conversion if it's a list
convert_if_list(Value) ->
    Value.

join(Key) when is_list(Key) ->
    KeyList = hb_store:join(Key),
    maybe_convert_to_binary(KeyList);
join(Key) when is_binary(Key) -> Key.

maybe_convert_to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
maybe_convert_to_binary(Value) when is_binary(Value) ->
    Value.

%%% Tests

%% @doc Generate a set of options for testing. The default is to use an `fs`
%% store as the persistent backing.
test_opts(PersistentStore) ->
    test_opts(PersistentStore, 1000000).
test_opts(PersistentStore, Capacity) ->
    % Set the server ID to a random address.
    ServerID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    put(server_id, ServerID),
    BaseStore = #{
        <<"capacity">> => Capacity,
        <<"store-module">> => hb_store_lru,
        <<"server-id">> => ServerID
    },
    StoreOpts = 
        case PersistentStore of
            no_store ->
                BaseStore#{<<"persistent-store">> => no_store};
            default ->
                DefaultStore = [
                    #{
                        <<"store-module">> => hb_store_fs,
                        <<"prefix">> => <<"cache-TEST/lru">>
                    }
                ],
                hb_store:reset(DefaultStore),
                BaseStore#{<<"persistent-store">> => DefaultStore};
            _ ->
                hb_store:reset(PersistentStore),
                BaseStore#{<<"persistent-store">> => PersistentStore}
        end,
    % Add the default HTTP server options.
    hb_http_server:set_default_opts(#{ store => StoreOpts }).

unknown_value_test() ->
    DefaultOpts = test_opts(no_store),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
    ?assertEqual(not_found, read(StoreOpts, <<"key1">>)).

cache_term_test() ->
    DefaultOpts = test_opts(no_store),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
    write(StoreOpts, <<"key1">>, <<"Hello">>),
    ?assertEqual({ok, <<"Hello">>}, read(StoreOpts, <<"key1">>)).

evict_oldest_items_test() ->
    DefaultOpts = test_opts(no_store, 500),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    write(StoreOpts, <<"key2">>, Binary),
    read(StoreOpts, <<"key1">>),
    write(StoreOpts, <<"key3">>, Binary),
    ?assertEqual({ok, Binary}, read(StoreOpts, <<"key1">>)),
    ?assertEqual(not_found, read(StoreOpts, <<"key2">>)).

evict_items_with_insufficient_space_test() ->
    DefaultOpts = test_opts(no_store, 500),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    write(StoreOpts, <<"key2">>, Binary),
    write(StoreOpts, <<"key3">>, crypto:strong_rand_bytes(400)),
    ?assertEqual(not_found, read(StoreOpts, <<"key1">>)),
    ?assertEqual(not_found, read(StoreOpts, <<"key2">>)).

evict_but_able_to_read_from_fs_store_test() ->
    DefaultOpts = test_opts(default, 500),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    write(StoreOpts, <<"key2">>, Binary),
    read(StoreOpts, <<"key1">>),
    write(StoreOpts, <<"key3">>, Binary),
    ?assertEqual({ok, Binary}, read(StoreOpts, <<"key1">>)),
    ?assertEqual({ok, Binary}, read(StoreOpts, <<"key2">>)),
    % Directly offloads if the data is more than the LRU capacity
    write(StoreOpts, <<"sub/key">>, crypto:strong_rand_bytes(600)),
    ?assertMatch({ok, _}, read(StoreOpts, <<"sub">>)).

stop_test() ->
    Opts = test_opts(default, 500),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, Opts),
    {ok, ServerPID} = start(StoreOpts),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    write(StoreOpts, <<"key2">>, Binary),
    ok = stop(StoreOpts),
    ?assertEqual(false, is_process_alive(ServerPID)),
    PersistentStore = hb_maps:get(<<"persistent-store">>, StoreOpts),
    ?assertEqual({ok, Binary}, hb_store:read(PersistentStore, <<"key1">>)),
    ?assertEqual({ok, Binary}, hb_store:read(PersistentStore, <<"key2">>)).

reset_test() ->
    DefaultOpts = test_opts(default),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
    write(StoreOpts, <<"key1">>, <<"Hello">>),
    write(StoreOpts, <<"key2">>, <<"Hi">>),
    reset(StoreOpts),
    ?assertEqual(not_found, read(StoreOpts, <<"key1">>)),
    #{cache_table := CacheTable} =
        persistent_term:get({in_memory_lru_cache, get(server_id)}),
    ?assertEqual([], ets:tab2list(CacheTable)).

list_test() ->
    DefaultOpts = test_opts(default, 500),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
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

type_test() ->
    DefaultOpts = test_opts(default, 500),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
    Binary = crypto:strong_rand_bytes(200),
    write(StoreOpts, <<"key1">>, Binary),
    ?assertEqual(simple, type(StoreOpts, <<"key1">>)),
    write(StoreOpts, <<"sub/key1">>, Binary),
    ?assertEqual(composite, type(StoreOpts, <<"sub">>)),
    make_link(StoreOpts, <<"key1">>, <<"keylink">>),
    ?assertEqual(simple, type(StoreOpts, <<"keylink">>)).

replace_link_test() ->
    DefaultOpts = test_opts(no_store),
    StoreOpts = hb_opts:get(store, {error, store_not_found}, DefaultOpts),
    start(StoreOpts),
    write(StoreOpts, <<"key1">>, <<"Hello">>),
    make_link(StoreOpts, <<"key1">>, <<"keylink">>),
    ?assertEqual({ok, <<"Hello">>}, read(StoreOpts, <<"keylink">>)),
    write(StoreOpts, <<"key2">>, <<"Hello2">>),
    make_link(StoreOpts, <<"key2">>, <<"keylink">>),
    ?assertEqual({ok, <<"Hello2">>}, read(StoreOpts, <<"keylink">>)),
    ServerID = find_id(StoreOpts),
    CacheTables = persistent_term:get({in_memory_lru_cache, ServerID}),
    {raw, #{links := Links }}= get_cache_entry(CacheTables, <<"key1">>),
    ?assertEqual([], Links).