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
%%   <li>Automatic link resolution for creating symbolic references between keys</li>
%%   <li>Group support for organizing hierarchical data structures</li>
%%   <li>Prefix-based key listing for directory-like navigation</li>
%%   <li>Process-local caching of database handles for efficiency</li>
%% </ul>
%%
%% The module implements a dual-flush strategy: writes are accumulated in memory
%% and flushed either after an idle timeout or when explicitly requested during
%% read operations that encounter cache misses.
-module(hb_store_lmdb).

%% Public API exports
-export([start/3, stop/3, scope/0, scope/1, reset/3]).
-export([read/3, write/3, list/3, match/3]).
-export([group/3, link/3, type/3, resolve/3]).

%% Test framework and project includes
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Configuration constants with reasonable defaults
-define(DEFAULT_SIZE, 2 * 1024 * 1024 * 1024 * 1024). % 2TiB default database size
-define(DEFAULT_BATCH_SIZE, 5_000).             % Flush keys on every read or 
                                                % every 5,000 write operations.
-define(MAX_REDIRECTS, 1000).                   % Only resolve 1000 links to data

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
            no_mem_init,
            no_sync
        ] ++
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
    {ok, #{ <<"env">> => Env, <<"db">> => DBInstance }};
start(_Store, _Req, _NodeOpts) ->
    {error, {badarg, <<"StoreOpts must be a map">>}}.

%% @doc Ensure that the database directory exists.
ensure_dir(DataDirPath) ->
    % `filelib` interprets the last path element as a filename, so we add a 
    % dummy one, else the final directory will not be created.
    filelib:ensure_dir(filename:join(DataDirPath, "dummy.mdb")).

type(Opts, #{ <<"type">> := Key }, _NodeOpts) ->
    case read_resolved(Opts, hb_path:to_binary(Key)) of
        {ok, _ResolvedKey, <<"group">>} -> {ok, composite};
        {ok, _ResolvedKey, _Value} -> {ok, simple};
        not_found -> {error, not_found}
    end.

write(#{ <<"read-only">> := true }, _Req, _NodeOpts) when is_map(_Req) ->
    {error, not_found};
write(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(Path, Value, ok) ->
            write(Opts, hb_path:to_binary(Path), Value);
           (_Path, _Value, Error) ->
            Error
        end,
        ok,
        Req
    );
write(#{ <<"read-only">> := true }, _PathParts, _Value) ->
    {error, not_found};
write(Opts, PathParts, Value) when is_list(PathParts) ->
    write(Opts, to_path(PathParts), Value);
write(Opts, Path, Value) ->
    #{ <<"db">> := DBInstance } = find_env(Opts),
    ?event({elmdb_write, {db, DBInstance}, {path, Path}, {value, Value}}),
    case elmdb:put(DBInstance, Path, Value) of
        ok -> ok;
        {error, Type, Description} ->
            ?event(
                error,
                {lmdb_error,
                    {type, Type},
                    {description, Description}
                }
            ),
            retry
    end.

read(Opts, #{ <<"read">> := Path }, _NodeOpts) ->
    case read_resolved(Opts, hb_path:to_binary(Path)) of
        {ok, ResolvedPath, <<"group">>} ->
            {composite, hb_util:ok(list_children(Opts, ResolvedPath))};
        {ok, _ResolvedPath, Value} ->
            {ok, Value};
        not_found ->
            {error, not_found}
    end.

read_resolved(#{<<"name">> := Name} = Opts, Path) ->
    StartTime = erlang:monotonic_time(),
    case do_read_resolved(Opts, hb_path:to_binary(Path)) of
        {ok, _ResolvedPath, _Value} = Result ->
            sample_metrics(Name, StartTime, hit),
            Result;
        not_found ->
            sample_metrics(Name, StartTime, miss),
            not_found
    end.

do_read_resolved(Opts, Path) ->
    case read_with_links(Opts, Path) of
        {ok, _ResolvedPath, _Value} = Result ->
            Result;
        not_found ->
            try
                PathParts = binary:split(Path, <<"/">>, [global, trim_all]),
                case resolve_path_links(Opts, PathParts) of
                    {ok, ResolvedPathParts} ->
                        read_with_links(Opts, to_path(ResolvedPathParts));
                    {error, _} ->
                        not_found
                end
            catch
                Class:Reason:Stacktrace ->
                    ?event(error,
                        {
                            resolve_path_links_failed, 
                            {class, Class},
                            {reason, Reason},
                            {stacktrace, {trace, Stacktrace}},
                            {path, Path}
                        }
                    ),
                    not_found
            end
    end.

%% @doc Helper function to check if a value is a link and extract the target.
is_link(Value) ->
    LinkPrefixSize = byte_size(<<"link:">>),
    case byte_size(Value) > LinkPrefixSize andalso
        binary:part(Value, 0, LinkPrefixSize) =:= <<"link:">> of
        true -> 
            Link =
                binary:part(
                    Value,
                    LinkPrefixSize,
                    byte_size(Value) - LinkPrefixSize
                ),
            {true, Link};
        false ->
            false
    end.

%% @doc Helper function to convert to a path
to_path(PathParts) ->
    hb_util:bin(lists:join(<<"/">>, PathParts)).

read_direct(#{<<"name">> := Name} = Opts, Path) ->
    #{ <<"db">> := DBInstance } = find_env(Opts),
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

read_with_links(Opts, Path) ->
    case read_direct(Opts, Path) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Link} -> 
                    do_read_resolved(Opts, Link);
                false ->
                    {ok, Path, Value}
            end;
        not_found ->
            not_found
    end.

%% @doc Resolve links in a path, checking each segment except the last.
%% Returns the resolved path where any intermediate links have been followed.
resolve_path_links(Opts, Path) ->
    resolve_path_links(Opts, Path, 0).

%% Internal helper with depth limit to prevent infinite loops
resolve_path_links(_Opts, _Path, Depth) when Depth > ?MAX_REDIRECTS ->
    % Prevent infinite loops with depth limit
    {error, too_many_redirects};
resolve_path_links(_Opts, [LastSegment], _Depth) ->
    % Base case: only one segment left, no link resolution needed
    {ok, [LastSegment]};
resolve_path_links(Opts, Path, Depth) ->
    resolve_path_links_acc(Opts, Path, [], Depth).

%% Internal helper that accumulates the resolved path
resolve_path_links_acc(_Opts, [], AccPath, _Depth) ->
    % No more segments to process
    {ok, lists:reverse(AccPath)};
resolve_path_links_acc(_, FullPath = [<<"data">>|_], [], _Depth) ->
    {ok, FullPath};
resolve_path_links_acc(Opts, [Head | Tail], AccPath, Depth) ->
    % Build the accumulated path so far
    CurrentPath = lists:reverse([Head | AccPath]),
    CurrentPathBin = to_path(CurrentPath),
    % Check if the accumulated path (not just the segment) is a link
    case read_direct(Opts, CurrentPathBin) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Link} ->
                    % The accumulated path is a link! Resolve it
                    LinkSegments = binary:split(Link, <<"/">>, [global]),
                    % Replace the accumulated path with the link target and
                    % continue with remaining segments
                    NewPath = LinkSegments ++ Tail,
                    resolve_path_links(Opts, NewPath, Depth + 1);
                false ->
                    % Not a link, continue accumulating
                    resolve_path_links_acc(Opts, Tail, [Head | AccPath], Depth)
            end;
        not_found ->
            % Path doesn't exist as a complete link, continue accumulating
            resolve_path_links_acc(Opts, Tail, [Head | AccPath], Depth)
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
%% If the Path points to a link, the function resolves the link and lists
%% the contents of the target directory instead.
%%
%% This is particularly useful for implementing hierarchical data organization
%% and providing tree-like navigation interfaces in applications.
%%
%% @param StoreOpts Database configuration map
%% @param Path Binary prefix to search for
%% @returns {ok, [Key]} list of matching keys, {error, Reason} on failure
list(Opts, #{ <<"list">> := Path }, _NodeOpts) ->
    case read_resolved(Opts, hb_path:to_binary(Path)) of
        {ok, ResolvedPath, <<"group">>} ->
            list_children(Opts, ResolvedPath);
        {ok, _ResolvedPath, _Value} ->
            {error, not_found};
        not_found ->
            {error, not_found}
    end.

list_children(Opts, ResolvedPath) ->
    SearchPath = 
        case ResolvedPath of
            <<>> -> <<>>;
            <<"/">> -> <<>>;
            _ -> 
                case binary:last(ResolvedPath) of
                    $/ -> ResolvedPath;
                    _ -> <<ResolvedPath/binary, "/">>
                end
        end,
    % Use native elmdb:list function
    #{ <<"db">> := DBInstance } = find_env(Opts),
    case elmdb:list(DBInstance, SearchPath) of
        {ok, Children} -> {ok, Children};
        {error, not_found} -> {ok, []};
        not_found -> {ok, []}
    end.

%% @doc Match a series of keys and values against the database. Returns 
%% `{ok, Matches}' if the match is successful, or `not_found' if there are no
%% messages in the store that feature all of the given key-value pairs. `Matches'
%% is given as a list of IDs.
match(Opts, MatchMap, _NodeOpts) when is_map(MatchMap) ->
    match(Opts, maps:to_list(MatchMap), #{});
match(Opts, MatchKVs, _NodeOpts) ->
    #{ <<"db">> := DBInstance } = find_env(Opts),
    WithPrefixes =
        lists:map(
            fun({Key, Path}) ->
                {Key, <<"link:", Path/binary>>}
            end,
            MatchKVs
        ),
    ?event({elmdb_match, MatchKVs}),
    case elmdb:match(DBInstance, WithPrefixes) of
        {ok, Matches} ->
            ?event({elmdb_matched, Matches}),
            {ok, Matches};
        {error, not_found} -> {error, not_found};
        not_found -> {error, not_found}
    end.


%% @doc Create a group entry that can contain other keys hierarchically.
%%
%% Groups in the HyperBeam system represent composite entries that can contain
%% child elements, similar to directories in a filesystem. This function creates
%% a group by storing the special value "group" at the specified key.
%%
%% The group mechanism allows applications to organize data hierarchically and
%% provides semantic meaning that can be used by navigation and visualization
%% tools to present appropriate user interfaces.
%%
%% Groups can be identified later using `type/3', which will return
%% 'composite' for group entries versus 'simple' for regular key-value pairs.
%%
%% @param Opts Database configuration map
%% @param GroupName Binary name for the group
%% @returns Result of the write operation
group(Opts, #{ <<"group">> := GroupName }, _NodeOpts) ->
    write(Opts, hb_path:to_binary(GroupName), <<"group">>).

%% @doc Ensure all parent groups exist for a given path.
%%
%% This function creates the necessary parent groups for a path, similar to
%% how filesystem stores use ensure_dir. For example, if the path is
%% "a/b/c/file", it will ensure groups "a", "a/b", and "a/b/c" exist.
%%
%% @param Opts Database configuration map
%% @param Path The path whose parents should exist
%% @returns ok
-spec ensure_parent_groups(map(), binary()) -> ok.
ensure_parent_groups(Opts, Path) ->
    PathParts = binary:split(Path, <<"/">>, [global]),
    case PathParts of
        [_] -> 
            % Single segment, no parents to create
            ok;
        _ ->
            % Multiple segments, create parent groups
            ParentParts = lists:droplast(PathParts),
            create_parent_groups(Opts, [], ParentParts)
    end.

%% @doc Helper function to recursively create parent groups.
create_parent_groups(_Opts, _Current, []) ->
    ok;
create_parent_groups(Opts, Current, [Next | Rest]) ->
    NewCurrent = Current ++ [Next],
    GroupPath = to_path(NewCurrent),
    % Only create group if it doesn't already exist.
    case read_direct(Opts, GroupPath) of
        not_found ->
            write(Opts, GroupPath, <<"group">>);
        {ok, _} ->
            % Already exists, skip
            ok
    end,
    create_parent_groups(Opts, NewCurrent, Rest).

%% @doc Create a symbolic link from a new key to an existing key.
%%
%% This function implements a symbolic link mechanism by storing a special
%% "link:" prefixed value at the new key location. When the new key is read,
%% the system will automatically resolve the link and return the value from
%% the target key instead.
%%
%% Links provide a way to create aliases, shortcuts, or alternative access
%% paths to the same underlying data without duplicating storage. They can
%% be chained together to create complex reference structures, though care
%% should be taken to avoid circular references.
%%
%% The link resolution happens transparently during read operations, making
%% links invisible to most application code while providing powerful
%% organizational capabilities.
%%
%% @param StoreOpts Database configuration map
%% @param Existing The key that already exists and contains the target value
%% @param New The new key that should link to the existing key
%% @returns Result of the write operation
link(Opts, Req, _NodeOpts) when is_map(Req) ->
    maps:fold(
        fun(New, Existing, ok) ->
            link(Opts, hb_path:to_binary(Existing), hb_path:to_binary(New));
           (_New, _Existing, Error) ->
            Error
        end,
        ok,
        Req
    );
link(#{ <<"read-only">> := true }, _Existing, _New) ->
    {error, not_found};
link(Opts, Existing, New) when is_list(Existing) ->
    link(Opts, to_path(Existing), New);
link(Opts, Existing, New) ->
   ExistingBin = hb_util:bin(Existing),
   ensure_parent_groups(Opts, hb_path:to_binary(New)),
   write(Opts, hb_path:to_binary(New), <<"link:", ExistingBin/binary>>).

%% @doc Resolve a path by following any symbolic links.
%%
%% For LMDB, we handle links through our own "link:" prefix mechanism.
%% This function resolves link chains in paths, similar to filesystem symlink resolution.
%% It's used by the cache to resolve paths before type checking and reading.
%%
%% @param StoreOpts Database configuration map
%% @param Path The path to resolve (binary or list)
%% @returns The resolved path as a binary
resolve(Opts, #{ <<"resolve">> := Path }, _NodeOpts) ->
    PathBin = hb_path:to_binary(Path),
    case resolve_path_links(Opts, binary:split(PathBin, <<"/">>, [global])) of
        {ok, ResolvedParts} ->
            {ok, to_path(ResolvedParts)};
        {error, _} ->
            {ok, PathBin}
    end.

%% @doc Retrieve or create the LMDB environment handle for a database.
find_env(Opts) -> hb_store:find(Opts).

%% Shutdown LMDB environment and cleanup resources
stop(#{ <<"store-module">> := ?MODULE, <<"name">> := DataDir }, _Req, _Opts) ->
    % Soft-close by name; refs stay valid and reopen lazily on next access.
    catch elmdb:env_close_by_name(hb_util:list(DataDir)),
    ok;
stop(_InvalidStoreOpts, _Req, _Opts) ->
    ok.

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
        miss -> ok
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
%% the LMDB store implementation correctly handles various scenarios including
%% basic read/write operations, hierarchical listing, group creation, link
%% resolution, and type detection.

test_reset(StoreOpts) ->
    reset(StoreOpts, #{}, #{}).

test_stop(StoreOpts) ->
    stop(StoreOpts, #{}, #{}).

test_group(StoreOpts, Path) ->
    write(StoreOpts, hb_path:to_binary(Path), <<"group">>).

test_link(StoreOpts, Existing, New) ->
    link(StoreOpts, Existing, New).

test_type(StoreOpts, Path) ->
    case read_resolved(StoreOpts, hb_path:to_binary(Path)) of
        {ok, _ResolvedPath, <<"group">>} -> composite;
        {ok, _ResolvedPath, _Value} -> simple;
        not_found -> not_found
    end.

test_read(StoreOpts, Path) ->
    case read_resolved(StoreOpts, hb_path:to_binary(Path)) of
        {ok, _ResolvedPath, <<"group">>} -> not_found;
        {ok, _ResolvedPath, Value} -> {ok, Value};
        not_found -> not_found
    end.

test_list(StoreOpts, Path) ->
    PathBin = hb_path:to_binary(Path),
    ResolvedPath =
        case read_direct(StoreOpts, PathBin) of
            {ok, Value} ->
                case is_link(Value) of
                    {true, Link} -> Link;
                    false -> PathBin
                end;
            not_found ->
                PathBin
        end,
    list_children(StoreOpts, ResolvedPath).

test_write(StoreOpts, Path, Value) ->
    ok = write(StoreOpts, Path, Value),
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

%% @doc List test - verifies prefix-based key listing functionality.
%%
%% This test creates several keys with hierarchical names and verifies that
%% the list operation correctly returns only keys matching a specific prefix.
%% It demonstrates the directory-like navigation capabilities of the store.
list_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-2">>,
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
    ?event({list_result, ListResult}),
    % Expected: red, blue, green (files) + multi, primary, nested (directories)
    % Should NOT include deeply nested items like foo, bar, deep, value
    ExpectedChildren = [<<"blue">>, <<"green">>, <<"multi">>, <<"nested">>, <<"primary">>, <<"red">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedChildren) end, ListResult)),
    % Test listing a nested directory - should only show immediate children
    {ok, NestedListResult} = test_list(StoreOpts, <<"colors/multi">>),
    ?event({nested_list_result, NestedListResult}),
    ExpectedNestedChildren = [<<"bar">>, <<"foo">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedNestedChildren) end, NestedListResult)),
    % Test listing a deeper nested directory
    {ok, DeepListResult} = test_list(StoreOpts, <<"colors/nested">>),
    ?event({deep_list_result, DeepListResult}),
    ExpectedDeepChildren = [<<"deep">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedDeepChildren) end, DeepListResult)),
    ok = test_stop(StoreOpts).

%% @doc Group test - verifies group creation and type detection.
%%
%% This test creates a group entry and verifies that it is correctly identified 
%% as a composite type and cannot be read directly (like filesystem directories).
group_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store3">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    test_reset(StoreOpts),
    test_group(StoreOpts, <<"colors">>),
    % Groups should be detected as composite types
    ?assertEqual(composite, test_type(StoreOpts, <<"colors">>)),
    % Groups should not be readable directly (like directories in filesystem)
    ?assertEqual(not_found, test_read(StoreOpts, <<"colors">>)).

%% @doc Link test - verifies symbolic link creation and resolution.
%%
%% This test creates a regular key-value pair, creates a link pointing to it,
%% and verifies that reading from the link location returns the original value.
%% This demonstrates the transparent link resolution mechanism.
link_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    test_write(StoreOpts, <<"foo/bar/baz">>, <<"Bam">>),
    test_link(StoreOpts, <<"foo/bar/baz">>, <<"foo/beep/baz">>),
    {ok, Result} = test_read(StoreOpts, <<"foo/beep/baz">>),
    ?event({ result, Result}),
    ?assertEqual(<<"Bam">>, Result).

link_fragment_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    test_write(StoreOpts, [<<"data">>, <<"bar">>, <<"baz">>], <<"Bam">>),
    test_link(StoreOpts, [<<"data">>, <<"bar">>], <<"my-link">>),
    {ok, Result} = test_read(StoreOpts, [<<"my-link">>, <<"baz">>]),
    ?event({ result, Result}),
    ?assertEqual(<<"Bam">>, Result).

%% @doc Type test - verifies type detection for both simple and composite entries.
%%
%% This test creates both a group (composite) entry and a regular (simple) entry,
%% then verifies that the type detection function correctly identifies each one.
%% This demonstrates the semantic classification system used by the store.
type_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    test_group(StoreOpts, <<"assets">>),
    Type = test_type(StoreOpts, <<"assets">>),
    ?event({type, Type}),
    ?assertEqual(composite, Type),
    test_write(StoreOpts, <<"assets/1">>, <<"bam">>),
    Type2 = test_type(StoreOpts, <<"assets/1">>),
    ?event({type2, Type2}),
    ?assertEqual(simple, Type2).

%% @doc Link key list test - verifies symbolic link creation using structured key paths.
%%
%% This test demonstrates the store's ability to handle complex key structures
%% represented as lists of binary segments, and verifies that symbolic links
%% work correctly when the target key is specified as a list rather than a
%% flat binary string.
%%
%% The test creates a hierarchical key structure using a list format (which
%% presumably gets converted to a path-like binary internally), creates a
%% symbolic link pointing to that structured key, and verifies that link
%% resolution works transparently to return the original value.
%%
%% This is particularly important for applications that organize data in
%% hierarchical structures where keys represent nested paths or categories,
%% and need to create shortcuts or aliases to deeply nested data.
link_key_list_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    test_write(StoreOpts, [ <<"parent">>, <<"key">> ], <<"value">>),
    test_link(StoreOpts, [ <<"parent">>, <<"key">> ], <<"my-link">>),
    {ok, Result} = test_read(StoreOpts, <<"my-link">>),
    ?event({result, Result}),
    ?assertEqual(<<"value">>, Result).

%% @doc Path traversal link test - verifies link resolution during path traversal.
%%
%% This test verifies that when reading a path as a list, intermediate path
%% segments that are links get resolved correctly. For example, if "link" 
%% is a symbolic link to "group", then reading ["link", "key"] should 
%% resolve to reading ["group", "key"].
%%
%% This functionality enables transparent redirection at the directory level,
%% allowing reorganization of hierarchical data without breaking existing
%% access patterns.
path_traversal_link_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    % Create the actual data at group/key
    test_write(StoreOpts, [<<"group">>, <<"key">>], <<"target-value">>),
    % Create a link from "link" to "group"
    test_link(StoreOpts, <<"group">>, <<"link">>),
    % Reading via the link path should resolve to the target value
    {ok, Result} = test_read(StoreOpts, [<<"link">>, <<"key">>]),
    ?event({path_traversal_result, Result}),
    ?assertEqual(<<"target-value">>, Result),
    ok = test_stop(StoreOpts).

%% @doc Test that matches the exact hb_store hierarchical test pattern
exact_hb_store_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    % Follow exact same pattern as hb_store test
    ?event(step1_make_group),
    test_group(StoreOpts, <<"test-dir1">>),
    ?event(step2_write_file),
    test_write(StoreOpts, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    ?event(step3_make_link),
    test_link(StoreOpts, [<<"test-dir1">>], <<"test-link">>),
    % Debug: test that the link behaves like the target (groups are unreadable)
    ?event(step4_check_link),
    LinkResult = test_read(StoreOpts, <<"test-link">>),
    ?event({link_result, LinkResult}),
    % Since test-dir1 is a group and groups are unreadable, the link should also be unreadable
    ?assertEqual(not_found, LinkResult),
    % Debug: test intermediate steps
    ?event(step5_test_direct_read),
    DirectResult = test_read(StoreOpts, <<"test-dir1/test-file">>),
    ?event({direct_result, DirectResult}),
    % This should work: reading via the link path  
    ?event(step6_test_link_read),
    Result = test_read(StoreOpts, [<<"test-link">>, <<"test-file">>]),
    ?event({final_result, Result}),
    ?assertEqual({ok, <<"test-data">>}, Result),
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
    ?event({cache_style_read_result, Result}),
    ?assertEqual({ok, <<"test-value">>}, Result),
    hb_store:stop(StoreOpts).

%% @doc Test nested map storage with cache-like linking behavior
%%
%% This test demonstrates how to store a nested map structure where:
%% 1. Each value is stored at data/{hash_of_value} 
%% 2. Links are created to compose the values back into the original map structure
%% 3. Reading the composed structure reconstructs the original nested map
nested_map_cache_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    % Clean up any previous test data
    test_reset(StoreOpts),
    % Original nested map structure
    OriginalMap = #{
        <<"target">> => <<"Foo">>,
        <<"commitments">> => #{
            <<"key1">> => #{
              <<"alg">> => <<"rsa-pss-512">>,
              <<"committer">> => <<"unique-id">>
            },
            <<"key2">> => #{
              <<"alg">> => <<"hmac">>,
              <<"commiter">> => <<"unique-id-2">>              
            }
        },
        <<"other-key">> => #{
            <<"other-key-key">> => <<"other-key-value">>
        }
    },
    ?event({original_map, OriginalMap}),
    % Step 1: Store each leaf value at data/{hash}
    TargetValue = <<"Foo">>,
    TargetHash = base64:encode(crypto:hash(sha256, TargetValue)),
    test_write(StoreOpts, <<"data/", TargetHash/binary>>, TargetValue),
    AlgValue1 = <<"rsa-pss-512">>,
    AlgHash1 = base64:encode(crypto:hash(sha256, AlgValue1)),
    test_write(StoreOpts, <<"data/", AlgHash1/binary>>, AlgValue1),
    CommitterValue1 = <<"unique-id">>,
    CommitterHash1 = base64:encode(crypto:hash(sha256, CommitterValue1)),
    test_write(StoreOpts, <<"data/", CommitterHash1/binary>>, CommitterValue1),
    AlgValue2 = <<"hmac">>,
    AlgHash2 = base64:encode(crypto:hash(sha256, AlgValue2)),
    test_write(StoreOpts, <<"data/", AlgHash2/binary>>, AlgValue2),
    CommitterValue2 = <<"unique-id-2">>,
    CommitterHash2 = base64:encode(crypto:hash(sha256, CommitterValue2)),
    test_write(StoreOpts, <<"data/", CommitterHash2/binary>>, CommitterValue2),
    OtherKeyValue = <<"other-key-value">>,
    OtherKeyHash = base64:encode(crypto:hash(sha256, OtherKeyValue)),
    test_write(StoreOpts, <<"data/", OtherKeyHash/binary>>, OtherKeyValue),
    % Step 2: Create the nested structure with groups and links
    % Create the root group
    test_group(StoreOpts, <<"root">>),
    % Create links for the root level keys
    test_link(StoreOpts, <<"data/", TargetHash/binary>>, <<"root/target">>),
    % Create the commitments subgroup
    test_group(StoreOpts, <<"root/commitments">>),
    % Create the key1 subgroup within commitments
    test_group(StoreOpts, <<"root/commitments/key1">>),
    test_link(StoreOpts, <<"data/", AlgHash1/binary>>, <<"root/commitments/key1/alg">>),
    test_link(StoreOpts, <<"data/", CommitterHash1/binary>>, <<"root/commitments/key1/committer">>),
    % Create the key2 subgroup within commitments
    test_group(StoreOpts, <<"root/commitments/key2">>),
    test_link(StoreOpts, <<"data/", AlgHash2/binary>>, <<"root/commitments/key2/alg">>),
    test_link(StoreOpts, <<"data/", CommitterHash2/binary>>, <<"root/commitments/key2/commiter">>),
    % Create the other-key subgroup
    test_group(StoreOpts, <<"root/other-key">>),
    test_link(StoreOpts, <<"data/", OtherKeyHash/binary>>, <<"root/other-key/other-key-key">>),
    % Step 3: Test reading the structure back
    % Verify the root is a composite
    ?assertEqual(composite, test_type(StoreOpts, <<"root">>)),
    % List the root contents
    {ok, RootKeys} = test_list(StoreOpts, <<"root">>),
    ?event({root_keys, RootKeys}),
    ExpectedRootKeys = [<<"commitments">>, <<"other-key">>, <<"target">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedRootKeys) end, RootKeys)),
    % Read the target directly
    {ok, TargetValueRead} = test_read(StoreOpts, <<"root/target">>),
    ?assertEqual(<<"Foo">>, TargetValueRead),
    % Verify commitments is a composite
    ?assertEqual(composite, test_type(StoreOpts, <<"root/commitments">>)),
    % Verify other-key is a composite  
    ?assertEqual(composite, test_type(StoreOpts, <<"root/other-key">>)),
    % Step 4: Test programmatic reconstruction of the nested map
    ReconstructedMap = reconstruct_map(StoreOpts, <<"root">>),
    ?event({reconstructed_map, ReconstructedMap}),
    % Verify the reconstructed map matches the original structure
    ?assert(hb_message:match(OriginalMap, ReconstructedMap)),
    test_stop(StoreOpts).

%% Helper function to recursively reconstruct a map from the store
reconstruct_map(StoreOpts, Path) ->
    case test_type(StoreOpts, Path) of
        composite ->
            % This is a group, reconstruct it as a map
            {ok, ImmediateChildren} = test_list(StoreOpts, Path),
            % The list function now correctly returns only immediate children
            ?event({path, Path, immediate_children, ImmediateChildren}),
            maps:from_list([
                {Key, reconstruct_map(StoreOpts, <<Path/binary, "/", Key/binary>>)}
                || Key <- ImmediateChildren
            ]);
        simple ->
            % This is a simple value, read it directly
            {ok, Value} = test_read(StoreOpts, Path),
            Value;
        not_found ->
            % Path doesn't exist
            undefined
    end.

%% @doc Debug test to understand cache linking behavior
cache_debug_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    % Simulate what the cache does:
    % 1. Create a group for message ID
    MessageID = <<"test_message_123">>,
    test_group(StoreOpts, MessageID),
    % 2. Store a value at data/hash
    Value = <<"test_value">>,
    ValueHash = base64:encode(crypto:hash(sha256, Value)),
    DataPath = <<"data/", ValueHash/binary>>,
    test_write(StoreOpts, DataPath, Value),
    % 3. Calculate a key hashpath (simplified version)
    KeyHashPath = <<MessageID/binary, "/", "key_hash_abc">>,
    % 4. Create link from data path to key hash path
    test_link(StoreOpts, DataPath, KeyHashPath),
    % 5. Test what the cache would see:
    ?event(debug_cache_test, {step, check_message_type}),
    MsgType = test_type(StoreOpts, MessageID),
    ?event(debug_cache_test, {message_type, MsgType}),
    ?event(debug_cache_test, {step, list_message_contents}),
    {ok, Subkeys} = test_list(StoreOpts, MessageID),
    ?event(debug_cache_test, {message_subkeys, Subkeys}),
    ?event(debug_cache_test, {step, read_key_hashpath}),
    KeyHashResult = test_read(StoreOpts, KeyHashPath),
    ?event(debug_cache_test, {key_hash_read_result, KeyHashResult}),
    % 6. Test with path as list (what cache does):
    ?event(debug_cache_test, {step, read_path_as_list}),
    PathAsList = [MessageID, <<"key_hash_abc">>],
    PathAsListResult = test_read(StoreOpts, PathAsList),
    ?event(debug_cache_test, {path_as_list_result, PathAsListResult}),
    test_stop(StoreOpts).

%% @doc Isolated test focusing on the exact cache issue
isolated_type_debug_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    % Create the exact scenario from user's description:
    % 1. A message ID with nested structure
    MessageID = <<"Base23">>,
    test_group(StoreOpts, MessageID),
    % 2. Create nested groups for "commitments" and "other-test-key"
    CommitmentsPath = <<MessageID/binary, "/commitments">>,
    OtherKeyPath = <<MessageID/binary, "/other-test-key">>,
    ?event(debug_isolated, {creating_nested_groups, CommitmentsPath, OtherKeyPath}),
    test_group(StoreOpts, CommitmentsPath),
    test_group(StoreOpts, OtherKeyPath),
    % 3. Add some actual data within those groups
    test_write(StoreOpts, <<CommitmentsPath/binary, "/sig1">>, <<"signature_data_1">>),
    test_write(StoreOpts, <<OtherKeyPath/binary, "/sub_value">>, <<"nested_value">>),
    % 4. Test type detection on the nested paths
    ?event(debug_isolated, {testing_main_message_type}),
    MainType = test_type(StoreOpts, MessageID),
    ?event(debug_isolated, {main_message_type, MainType}),
    ?event(debug_isolated, {testing_commitments_type}),
    CommitmentsType = test_type(StoreOpts, CommitmentsPath),
    ?event(debug_isolated, {commitments_type, CommitmentsType}),
    ?event(debug_isolated, {testing_other_key_type}),
    OtherKeyType = test_type(StoreOpts, OtherKeyPath),
    ?event(debug_isolated, {other_key_type, OtherKeyType}),
    % 5. Test what happens when reading these nested paths
    ?event(debug_isolated, {reading_commitments_directly}),
    CommitmentsResult = test_read(StoreOpts, CommitmentsPath),
    ?event(debug_isolated, {commitments_read_result, CommitmentsResult}),
    ?event(debug_isolated, {reading_other_key_directly}),
    OtherKeyResult = test_read(StoreOpts, OtherKeyPath),
    ?event(debug_isolated, {other_key_read_result, OtherKeyResult}),
    test_stop(StoreOpts).

%% @doc Test that list function resolves links correctly
list_with_link_test() ->
    StoreOpts = hb_test_utils:test_store(?MODULE),
    test_reset(StoreOpts),
    % Create a group with some children
    test_group(StoreOpts, <<"real-group">>),
    test_write(StoreOpts, <<"real-group/child1">>, <<"value1">>),
    test_write(StoreOpts, <<"real-group/child2">>, <<"value2">>),
    test_write(StoreOpts, <<"real-group/child3">>, <<"value3">>),
    % Create a link to the group
    test_link(StoreOpts, <<"real-group">>, <<"link-to-group">>),
    % List the real group to verify expected children
    {ok, RealGroupChildren} = test_list(StoreOpts, <<"real-group">>),
    ?event({real_group_children, RealGroupChildren}),
    ExpectedChildren = [<<"child1">>, <<"child2">>, <<"child3">>],
    ?assertEqual(ExpectedChildren, lists:sort(RealGroupChildren)),
    % List via the link - should return the same children
    {ok, LinkChildren} = test_list(StoreOpts, <<"link-to-group">>),
    ?event({link_children, LinkChildren}),
    ?assertEqual(ExpectedChildren, lists:sort(LinkChildren)),
    test_stop(StoreOpts).
