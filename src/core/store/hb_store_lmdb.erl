%% @doc An LMDB (Lightning Memory Database) implementation of the HyperBeam store interface.
%%
%% This module provides a persistent key-value store backend using LMDB, which is a
%% high-performance embedded transactional database. The implementation follows a
%% singleton pattern where each database environment gets its own dedicated server
%% process to manage transactions and coordinate writes.
%%
%% Key features include:
%% <ul>
%%   <li>Asynchronous writes with batched transactions for performance</li>
%%   <li>Prefix-based key listing for directory-like navigation</li>
%%   <li>Process-local caching of database handles for efficiency</li>
%% </ul>
%%
%% The module implements a dual-flush strategy: writes are accumulated in memory
%% and flushed either after an idle timeout or when explicitly requested during
%% list operations.
-module(hb_store_lmdb).

%% Public API exports
-export([start/3, stop/3, flush/3, scope/0, scope/1, reset/3]).
-export([read/3, write/3, list/3, match/3]).

%% Test framework and project includes
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Configuration constants with reasonable defaults
-define(DEFAULT_SIZE, 2 * 1024 * 1024 * 1024 * 1024). % 2TiB default database size
-define(DEFAULT_BATCH_SIZE, 5_000).             % Flush keys on every list or
                                                % every 5,000 write operations.
-define(DEFAULT_FLUSH_EVERY, 1_000).            % Commit buffered writes at
                                                % least every second.
-define(MAX_KEY_SIZE, 511).                     % LMDB's key size limit, bytes.

%% @doc Start the LMDB storage system for a given database configuration.
%%
%% This function initializes or connects to an existing LMDB database instance.
%% It uses a singleton pattern, so multiple calls with the same configuration
%% will return the same server process. The server process manages the LMDB
%% environment and coordinates all database operations.
%%
%% The StoreOpts map must contain a "prefix" key specifying the
%% database directory path. Also the required configuration includes "capacity"
%% for the maximum database size and flush timing parameters.
%%
%% @param StoreOpts A map containing database configuration options
%% @returns {ok, ServerPid} on success, {error, Reason} on failure
start(Opts = #{ <<"name">> := DataDir }, _Req, _NodeOpts) ->
    init_prometheus(),
    % Ensure the directory exists before opening LMDB environment
    DataDirPath = hb_util:list(DataDir),
    ok = ensure_dir(DataDirPath),
    EnvOpts =
        [
            {
                map_size,
                hb_util:int(maps:get(<<"capacity">>, Opts, ?DEFAULT_SIZE))
            },
            {
                batch_size,
                hb_util:int(maps:get(<<"batch-size">>, Opts, ?DEFAULT_BATCH_SIZE))
            },
            no_mem_init
        ] ++
        case maps:get(<<"sync">>, Opts, false) of
            true -> [];
            false -> [no_sync]
        end ++
        case maps:get(<<"read-ahead">>, Opts, true) of
            true -> [];
            false -> [no_readahead]
        end ++
        case maps:get(<<"read-only">>, Opts, false) of
            true -> [no_lock];
            false -> []
        end ++
        case maps:get(<<"max-readers">>, Opts, false) of
            false -> [];
            MaxReaders -> [{max_readers, hb_util:int(MaxReaders)}]
        end ++
        case maps:get(<<"lock">>, Opts, true) of
            true -> [];
            false -> [no_lock]
        end,
    % Create the LMDB environment with specified size limit
    {ok, Env} = elmdb:env_open(DataDirPath, EnvOpts),
    {ok, DBInstance} = elmdb:db_open(Env, [create]),
    Instance = #{ <<"env">> => Env, <<"db">> => DBInstance },
    start_flusher(Opts, Instance),
    {ok, Instance};
start(_Store, _Req, _NodeOpts) ->
    {error, {badarg, <<"StoreOpts must be a map">>}}.

%% @doc Start the store's flusher, one process per store, which commits the
%% writes that `elmdb' holds in memory every `flush-every' milliseconds, so that
%% a crash of the node loses at most the writes of the last interval. A
%% `flush-every' of 0 starts none, and a read-only store has no writes.
start_flusher(Opts = #{ <<"name">> := DataDir }, Instance) ->
    FlushEvery =
        hb_util:int(maps:get(<<"flush-every">>, Opts, ?DEFAULT_FLUSH_EVERY)),
    case maps:get(<<"read-only">>, Opts, false) orelse FlushEvery == 0 of
        true -> ok;
        false ->
            hb_name:singleton(
                {?MODULE, DataDir},
                fun() -> flusher(Instance, FlushEvery) end
            ),
            ok
    end.

%% @doc Commit the store's buffered writes every `FlushEvery' milliseconds,
%% until the store stops.
flusher(Instance, FlushEvery) ->
    receive
        stop -> ok
    after FlushEvery ->
        commit(Instance),
        flusher(Instance, FlushEvery)
    end.

%% @doc Ensure that the database directory exists.
ensure_dir(DataDirPath) ->
    % `filelib` interprets the last path element as a filename, so we add a 
    % dummy one, else the final directory will not be created.
    filelib:ensure_dir(filename:join(DataDirPath, "dummy.mdb")).

%% @doc Write a request map of `Path => Value' pairs to the database as one
%% batch. `elmdb' refuses the whole batch if a key is empty or longer than
%% `?MAX_KEY_SIZE' bytes, and a read-only store refuses every write.
write(#{ <<"read-only">> := true }, _Req, _NodeOpts) ->
    {error, not_found};
write(Opts, Req, _NodeOpts) ->
    #{ <<"db">> := DBInstance } = find_env(Opts),
    Pairs =
        maps:fold(
            fun(Path, Value, Acc) when is_binary(Path) -> [{Path, Value} | Acc];
               (Path, Value, Acc) -> [{hb_path:to_binary(Path), Value} | Acc]
            end,
            [],
            Req
        ),
    case elmdb:put_batch(DBInstance, Pairs) of
        ok -> ok;
        {error, Type, Description} ->
            ?event(
                error,
                {lmdb_error,
                    {type, Type},
                    {description, Description}
                }
            ),
            case Type of
                % The database failed to commit earlier writes and refuses
                % more. A restart would accept this one without committing it.
                transaction_error -> {error, Type};
                % `elmdb' refuses a batch with an empty or long key every time.
                validation_error -> {error, Type};
                _ -> retry
            end
    end.

%% @doc Read a value from the database by key. If the key has no value, but
%% has keys below it, we return those keys, each with its value if it has
%% one.
%%
%% @param Opts Database configuration map
%% @param PathReq Request of the form `#{<<"read">> => Path}`.
%% @returns `{ok, Value}` on success, `{composite, Children}` for a key with
%%          keys below it, or `{error, not_found}` on failure
read(Opts, #{ <<"read">> := Path }, _NodeOpts) when is_binary(Path) ->
    read_result(Opts, Path);
read(Opts, #{ <<"read">> := Path }, _NodeOpts) ->
    read_result(Opts, hb_path:to_binary(Path)).

read_result(Opts, Path) ->
    EnvOpts = ensure_env(Opts),
    StartTime = erlang:monotonic_time(),
    Result = 
        case read_key(EnvOpts, Path) of
            {ok, Value} ->
                {ok, Value};
            {composite, Children} ->
                {composite, Children};
            not_found ->
                {error, not_found};
            {error, Type, _} ->
                {error, Type}
        end,
    MetricStatus = 
        case Result of
            {ok, _} -> hit;
            {composite, _} -> hit;
            {error, not_found} -> miss;
            not_found -> miss;
            _ -> error
        end,
    sample_metrics(EnvOpts, StartTime, MetricStatus),
    Result.

%% @doc Read a key's value, or the rows below it. We read the value with one
%% `elmdb:get' of the key. For a key that has no value, we read the rows
%% below it with one `elmdb:read_prefix' of the key and a `/', and return the
%% key's immediate children, each with its value if it has one.
read_key(EnvOpts, Path) ->
    case read_direct(EnvOpts, Path) of
        not_found ->
            Prefix = <<Path/binary, "/">>,
            maybe
                {ok, Rows} ?= read_prefix_rows(EnvOpts, Prefix),
                {composite, hb_store_utils:immediate_children(Prefix, Rows)}
            end;
        Read ->
            Read
    end.

%% @doc Unified read function that handles LMDB reads with fallback to the
%% in-process pending writes, if necessary.
%%
%% Returns `{ok, Value}` or `not_found`.
read_direct(#{<<"db">> := DBInstance, <<"name">> := Name}, Path) ->
    read_direct(DBInstance, Name, Path);
read_direct(#{<<"db">> := DBInstance}, Path) ->
    read_direct(DBInstance, undefined, Path);
read_direct(#{<<"name">> := Name} = Opts, Path) ->
    #{ <<"db">> := DBInstance } = find_env(Opts),
    read_direct(DBInstance, Name, Path).

read_direct(DBInstance, Name, Path) ->
    case elmdb:get(DBInstance, Path) of
        {ok, Value} -> {ok, Value};
        {error, not_found} -> not_found;
        not_found -> not_found;
        {error, transaction_error, Message} = Err -> 
            ?event(lmdb_store, 
                {transaction_error, 
                    {path, Path}, 
                    {db_name, Name},
                    {message, Message}}),
            Err;
        {error, database_error, ErrorMessage} = Err ->
            ?event(lmdb_store, 
                {database_error, 
                    {path, Path}, 
                    {db_name, Name},
                    {msg, ErrorMessage}}),
            Err
    end.

%% @doc Return the scope of this storage backend.
%%
%% The LMDB implementation is always local-only and does not support distributed
%% operations. This function exists to satisfy the HyperBeam store interface
%% contract and inform the system about the storage backend's capabilities.
%%
%% @returns 'local' always
-spec scope() -> local.
scope() -> local.

%% @doc Return the scope of this storage backend (ignores parameters).
%%
%% This is an alternate form of scope/0 that ignores any parameters passed to it.
%% The LMDB backend is always local regardless of configuration.
%%
%% @param _Opts Ignored parameter
%% @returns 'local' always  
-spec scope(term()) -> local.
scope(_) -> scope().

%% @doc List all keys that start with a given prefix.
%%
%% This function provides directory-like navigation by finding all keys that
%% begin with the specified path prefix. It uses the native elmdb:list/2 function
%% to efficiently scan through the database and collect matching keys.
%%
%% The implementation returns only the immediate children of the given path,
%% not the full paths. For example, listing "colors/" will return ["red", "blue"]
%% not ["colors/red", "colors/blue"].
%%
%% This is particularly useful for implementing hierarchical data organization
%% and providing tree-like navigation interfaces in applications.
%%
%% @param StoreOpts Database configuration map
%% @param Path Binary prefix to search for
%% @returns {ok, [Key]} list of matching keys, {error, Reason} on failure
list(Opts, Req = #{ <<"list">> := Path }, _NodeOpts) ->
    EnvOpts = ensure_env(Opts),
    PathBin =
        case is_binary(Path) of
            true -> Path;
            false -> hb_path:to_binary(Path)
        end,
    % An empty page with `from' may have started past the last key.
    case list_children(EnvOpts, PathBin, Req) of
        {ok, []} when is_map_key(<<"from">>, Req) ->
            case list_children(EnvOpts, PathBin, #{ <<"limit">> => 1 }) of
                {ok, []} -> {error, not_found};
                {ok, _} -> {ok, []};
                Error -> Error
            end;
        {ok, []} -> {error, not_found};
        Result -> Result
    end.

%% @doc List the children of a path through the NIF's cursor: every one, or
%% those the request selects from its `from' in its direction, no more than
%% its limit. A `batch' uses the store's `list-batch-size' (default 256,
%% minimum 2 for inclusive cursor progress), independently of the write
%% flush threshold `batch-size'. An `all' request remains unbounded.
list_children(Opts, ResolvedPath, Req) ->
    #{
        <<"from">> := From,
        <<"limit">> := Limit,
        <<"direction">> := Direction
    } = hb_store_utils:list_request_bounds(Req, Opts),
    #{ <<"db">> := DBInstance } = find_env(Opts),
    Options =
        [ {from, From} || From =/= none ] ++
        [ {limit, Limit} || Limit =/= all ] ++
        [ {direction, case Direction of asc -> forward; desc -> backward end} ],
    Prefix = hb_store_utils:child_prefix(ResolvedPath),
    case elmdb:list(DBInstance, Prefix, Options) of
        {ok, Children} -> {ok, Children};
        {error, Type, Description} -> {error, {Type, Description}}
    end.

read_prefix_rows(Opts, Path) ->
    #{ <<"db">> := DBInstance } = find_env(Opts),
    case elmdb:read_prefix(DBInstance, Path) of
        {ok, Rows} -> {ok, Rows};
        {error, not_found} -> not_found;
        not_found -> not_found;
        {error, _Type, _Description} = Error -> Error
    end.

%% @doc Match a series of keys and values against the database. Returns 
%% `{ok, Matches}' if the match is successful, or `not_found' if there are no
%% messages in the store that feature all of the given key-value pairs. `Matches'
%% is given as a list of IDs.
match(Opts, MatchMap, _NodeOpts) when is_map(MatchMap) ->
    match(Opts, maps:to_list(MatchMap), #{});
match(Opts, MatchKVs, _NodeOpts) ->
    #{ <<"db">> := DBInstance } = find_env(Opts),
    Patterns =
        lists:map(
            fun({Key, Value}) -> {Key, hb_util:bin(Value)} end,
            MatchKVs
        ),
    ?event_debug({elmdb_match, MatchKVs}),
    match_patterns(DBInstance, Patterns).

match_patterns(DBInstance, Patterns) ->
    case elmdb:match(DBInstance, Patterns) of
        {ok, Matches} ->
            ?event_debug({elmdb_matched, Matches}),
            {ok, Matches};
        {error, not_found} -> {error, not_found};
        not_found -> {error, not_found}
    end.

%% @doc Retrieve or create the LMDB environment handle for a database.
find_env(Opts = #{ <<"db">> := _ }) -> Opts;
find_env(Opts) -> hb_store:find(Opts).

ensure_env(Opts = #{ <<"db">> := _ }) -> Opts;
ensure_env(Opts) -> maps:merge(Opts, find_env(Opts)).

%% Shutdown LMDB environment and cleanup resources
stop(#{ <<"store-module">> := ?MODULE, <<"name">> := DataDir }, _Req, _Opts) ->
    stop_flusher(DataDir),
    % Soft-close by name; refs stay valid and reopen lazily on next access.
    catch elmdb:env_close_by_name(hb_util:list(DataDir)),
    ok;
stop(_InvalidStoreOpts, _Req, _Opts) ->
    ok.

%% @doc Stop the store's flusher and wait for it to exit, as a flush after the
%% environment closes would open it again.
stop_flusher(DataDir) ->
    case hb_name:lookup({?MODULE, DataDir}) of
        undefined -> ok;
        PID ->
            Ref = erlang:monitor(process, PID),
            PID ! stop,
            receive {'DOWN', Ref, process, PID, _} -> ok end
    end.

%% @doc Commit the writes that `elmdb' holds in memory, and sync them to disk.
%% `elmdb:put_batch/2' returns before its writes are committed: writes are
%% committed in batches of `batch-size', and a halt of the node before then
%% loses them. A store with `sync' set to `true' syncs every commit to disk.
%% Any other store commits to the OS page cache only, and a loss of power can
%% lose its commits since the last flush.
flush(Opts = #{ <<"sync">> := true }, _Req, _NodeOpts) ->
    commit(find_env(Opts));
flush(Opts, _Req, _NodeOpts) ->
    Instance = #{ <<"env">> := Env } = find_env(Opts),
    maybe
        ok ?= commit(Instance),
        lmdb_result(elmdb:env_sync(Env))
    end.

%% @doc Commit the writes that `elmdb' holds in memory.
commit(#{ <<"db">> := DBInstance }) ->
    lmdb_result(elmdb:flush(DBInstance)).

%% @doc Normalize the result of an `elmdb' call. `ok' is returned as is. An
%% error is logged and returned as `{error, Type}'.
lmdb_result(Result) ->
    case Result of
        ok -> ok;
        {error, Type, Description} ->
            ?event(
                error,
                {lmdb_error,
                    {type, Type},
                    {description, Description}
                }
            ),
            {error, Type}
    end.

%% @doc Completely delete the database directory and all its contents.
%%
%% This is a destructive operation that removes all data from the specified
%% database. It first performs a graceful shutdown to ensure data consistency,
%% then uses the system shell to recursively delete the entire database
%% directory structure.
%%
%% This function is primarily intended for testing and development scenarios
%% where you need to start with a completely clean database state. It should
%% be used with extreme caution in production environments.
%%
%% @param StoreOpts Database configuration map containing the directory prefix
%% @returns 'ok' when deletion is complete
reset(Opts, _Req, _NodeOpts) ->
    case maps:get(<<"name">>, Opts, undefined) of
        undefined ->
            % No prefix specified, nothing to reset
            ok;
        DataDir ->
            % Stop the store and remove the database.
            stop(Opts, #{}, #{}),
            os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
            ensure_dir(DataDir),
            ok
    end.

%% @doc Sample roughly 1/1024 reads using the start timestamp and scale the
%% hit counter by the same factor to preserve an approximate total.
sample_metrics(_Name, StartTime, _Type) when (StartTime band 1023) =/= 0 ->
    ok;
sample_metrics(Name, StartTime, Type) ->
    ReadTime = erlang:monotonic_time() - StartTime,
    hb_prometheus:observe(ReadTime, hb_store_lmdb_duration_seconds, [read, Name]),
    case Type of
        hit -> hb_prometheus:inc(counter, hb_store_lmdb_hit, [Name], 1024);
        miss -> ok;
        error -> ok
    end.

init_prometheus() ->
    hb_prometheus:declare(histogram, [
        {name, hb_store_lmdb_duration_seconds},
        {labels, [function, store_name]},
        {buckets, [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 20]},
        {help, "Duration of lmdb operations in microseconds"}
    ]),
    hb_prometheus:declare(counter, [
        {name, hb_store_lmdb_hit},
        {labels, [name]},
        {help, "LMDB name requested"}
    ]),
    ok.

%% @doc Test suite demonstrating basic store operations.
%%
%% The following functions implement unit tests using EUnit to verify that
%% the LMDB store implementation correctly handles basic read/write operations
%% and hierarchical listing.

test_reset(StoreOpts) ->
    reset(StoreOpts, #{}, #{}).

test_stop(StoreOpts) ->
    stop(StoreOpts, #{}, #{}).

test_read(StoreOpts, Path) ->
    read_direct(StoreOpts, hb_path:to_binary(Path)).

test_list(StoreOpts, Path) ->
    list_children(StoreOpts, hb_path:to_binary(Path), #{}).

test_write(StoreOpts, Path, Value) ->
    ok = write(StoreOpts, #{ Path => Value }, #{}),
    ok.

%% @doc Basic store test - verifies fundamental read/write functionality.
%%
%% This test creates a temporary database, writes a key-value pair, reads it
%% back to verify correctness, and cleans up by stopping the database. It
%% serves as a sanity check that the basic storage mechanism is working.
basic_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-1">>
    },
    test_reset(StoreOpts),
    Res = test_write(StoreOpts, <<"Hello">>, <<"World2">>),
    ?assertEqual(ok, Res),
    {ok, Value} = test_read(StoreOpts, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    ok = test_stop(StoreOpts).

%% @doc A 511-byte key is written, a 512-byte key returns an error, and the
%% store still reads the first key afterwards.
long_key_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    MaxKey = binary:copy(<<"k">>, ?MAX_KEY_SIZE),
    ?assertEqual(ok, write(StoreOpts, #{ MaxKey => <<"held">> }, #{})),
    ?assertEqual(
        {error, validation_error},
        write(StoreOpts, #{ <<MaxKey/binary, "k">> => <<"refused">> }, #{})
    ),
    ?assertEqual({ok, <<"held">>}, test_read(StoreOpts, MaxKey)),
    ok = test_stop(StoreOpts).

%% @doc List test - verifies prefix-based key listing functionality.
%%
%% This test creates several keys with hierarchical names and verifies that
%% the list operation correctly returns only keys matching a specific prefix.
%% It demonstrates the directory-like navigation capabilities of the store.
list_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-2">>,
        <<"list-batch-size">> => 2,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    test_reset(StoreOpts),
    ?assertEqual({ok, []}, test_list(StoreOpts, <<"colors">>)),
    % Create immediate children under colors/
    test_write(StoreOpts, <<"colors/red">>, <<"1">>),
    test_write(StoreOpts, <<"colors/blue">>, <<"2">>),
    test_write(StoreOpts, <<"colors/green">>, <<"3">>),
    % Create nested directories under colors/ - these should show up as immediate children
    test_write(StoreOpts, <<"colors/multi/foo">>, <<"4">>),
    test_write(StoreOpts, <<"colors/multi/bar">>, <<"5">>),
    test_write(StoreOpts, <<"colors/primary/red">>, <<"6">>),
    test_write(StoreOpts, <<"colors/primary/blue">>, <<"7">>),
    test_write(StoreOpts, <<"colors/nested/deep/value">>, <<"8">>),
    % Create other top-level directories
    test_write(StoreOpts, <<"foo/bar">>, <<"baz">>),
    test_write(StoreOpts, <<"beep/boop">>, <<"bam">>),
    test_read(StoreOpts, <<"colors">>),
    % Test listing colors/ - should return immediate children only
    {ok, ListResult} = test_list(StoreOpts, <<"colors">>),
    ?event_debug({list_result, ListResult}),
    % Expected: red, blue, green (files) + multi, primary, nested (directories)
    % Should NOT include deeply nested items like foo, bar, deep, value
    ExpectedChildren = [<<"blue">>, <<"green">>, <<"multi">>, <<"nested">>, <<"primary">>, <<"red">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedChildren) end, ListResult)),
    % Batches are bounded in both directions; explicit limits remain independent.
    lists:foreach(
        fun({Limit, Direction, Expected}) ->
            ?assertEqual({ok, Expected}, list(StoreOpts, #{
                <<"list">> => <<"colors">>, <<"from">> => <<"multi">>,
                <<"limit">> => Limit, <<"direction">> => Direction
            }, #{}))
        end,
        [{batch, asc, [<<"multi">>, <<"nested">>]},
            {batch, desc, [<<"multi">>, <<"green">>]},
            {1, asc, [<<"multi">>]},
            {all, asc, [<<"multi">>, <<"nested">>, <<"primary">>, <<"red">>]}]
    ),
    % Test listing a nested directory - should only show immediate children
    {ok, NestedListResult} = test_list(StoreOpts, <<"colors/multi">>),
    ?event_debug({nested_list_result, NestedListResult}),
    ExpectedNestedChildren = [<<"bar">>, <<"foo">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedNestedChildren) end, NestedListResult)),
    % Test listing a deeper nested directory
    {ok, DeepListResult} = test_list(StoreOpts, <<"colors/nested">>),
    ?event_debug({deep_list_result, DeepListResult}),
    ExpectedDeepChildren = [<<"deep">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedDeepChildren) end, DeepListResult)),
    ok = test_stop(StoreOpts).

%% @doc Test cache-style usage through hb_store interface
cache_style_test() ->
    hb:init(),
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    % Start the store
    hb_store:start(StoreOpts),
    % Test writing through hb_store interface  
    ok = hb_store:write(StoreOpts, #{ <<"test-key">> => <<"test-value">> }, #{}),
    % Test reading through hb_store interface
    Result = hb_store:read(StoreOpts, <<"test-key">>, #{}),
    ?event_debug({cache_style_read_result, Result}),
    ?assertEqual({ok, <<"test-value">>}, Result),
    % If the store's directory is replaced, as by a mount over it, a write
    % goes to the new directory.
    #{ <<"name">> := Dir } = StoreOpts,
    ok = file:rename(Dir, <<Dir/binary, "-hidden">>),
    ok = ensure_dir(Dir),
    ok = hb_store:write(StoreOpts, #{ <<"mounted">> => <<"yes">> }, #{}),
    ok = hb_store:flush(StoreOpts, #{}),
    ok = hb_store:stop(StoreOpts),
    ?assertEqual({ok, <<"yes">>}, hb_store:read(StoreOpts, <<"mounted">>, #{})),
    hb_store:stop(StoreOpts).

read_prefix_composite_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    test_write(StoreOpts, <<"root/a">>, <<"1">>),
    test_write(StoreOpts, <<"root/b/c">>, <<"2">>),
    ?assertEqual(
        {composite, [{<<"a">>, <<"1">>}, <<"b">>]},
        read(StoreOpts, #{ <<"read">> => <<"root">> }, #{})
    ),
    ?assertEqual({ok, [<<"a">>, <<"b">>]}, test_list(StoreOpts, <<"root">>)),
    % If writes are pending, a read of a key with no value waits for their
    % commit on a dirty IO scheduler: with one normal scheduler online, this
    % process runs before the read returns.
    test_write(StoreOpts, <<"root/d">>, <<"3">>),
    Online = erlang:system_flag(schedulers_online, 1),
    Self = self(),
    spawn(
        fun() ->
            Self ! reading,
            read(StoreOpts, #{ <<"read">> => <<"missing">> }, #{}),
            Self ! read
        end
    ),
    receive reading -> Self ! ran end,
    First = receive Msg when Msg == ran; Msg == read -> Msg end,
    erlang:system_flag(schedulers_online, Online),
    receive Next when Next == ran; Next == read -> ok end,
    ?assertEqual(ran, First),
    test_stop(StoreOpts).
