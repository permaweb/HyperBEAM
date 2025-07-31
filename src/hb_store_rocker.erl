%% @doc A RocksDB implementation of the HyperBeam store interface using the rocker NIF.
%%
%% This module provides a persistent key-value store backend using RocksDB via the
%% rocker Erlang NIF library. The implementation follows the same interface as
%% hb_store_lmdb and provides high-performance persistent storage for HyperBeam.
%%
%% Key features include:
%% <ul>
%%   <li>Direct RocksDB operations for maximum performance</li>
%%   <li>Automatic link resolution for creating symbolic references between keys</li>
%%   <li>Group support for organizing hierarchical data structures</li>
%%   <li>Prefix-based key listing for directory-like navigation</li>
%%   <li>Configurable RocksDB options for tuning performance</li>
%% </ul>
%%
%% The module uses the rocker NIF for direct RocksDB access, providing excellent
%% performance characteristics while maintaining compatibility with the HyperBeam
%% store interface.
-module(hb_store_rocker).

%% Public API exports
-export([enabled/0, start/1, stop/1, scope/0, scope/1, reset/1]).
-export([read/2, write/3, list/2]).
-export([make_group/2, make_link/3, type/2]).
-export([path/2, add_path/3, resolve/2]).

%% Test framework and project includes
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Configuration constants with reasonable defaults
-define(MAX_REDIRECTS, 1000).                   % Only resolve 1000 links to data

%%% ============================================================================
%%% Public API
%%% ============================================================================

%% @doc Returns whether the rocker store is enabled.
-ifdef(ENABLE_ROCKER).
enabled() -> true.
-else.
enabled() -> false.
-endif.

%% @doc Start the RocksDB storage system for a given database configuration.
%%
%% This function initializes or connects to an existing RocksDB database instance.
%% The StoreOpts map must contain a "name" key specifying the database directory path.
%% Uses application-level synchronization to prevent RocksDB lock conflicts.
%%
%% @param StoreOpts A map containing database configuration options
%% @returns {ok, DatabaseHandle} on success, {error, Reason} on failure
start(#{ <<"name">> := DataDir } = Opts) ->
    % Check if database is already open first (before any locking)
    StoreKey = {rocker, ?MODULE, DataDir},
    case safe_get_persistent_term(StoreKey) of
        {ok, DBHandle} ->
            % Database already opened, reuse it immediately
            ?event(debug, {rocker_reusing_existing_db, DataDir}),
            {ok, #{ <<"db">> => DBHandle }};
        not_found ->
            % Need to open database - use mutex for thread safety
            ?event(debug, {rocker_opening_new_db, DataDir}),
            MutexKey = {rocker_mutex, DataDir},
            case global:set_lock(MutexKey, [node()], 5000) of
                true ->
                    try
                        % Double-check after acquiring lock
                        case safe_get_persistent_term(StoreKey) of
                            {ok, DBHandle} ->
                                % Another process opened it while we waited
                                ?event(debug, {rocker_found_db_after_lock, DataDir}),
                                {ok, #{ <<"db">> => DBHandle }};
                            not_found ->
                                % We need to open it
                                ?event(debug, {rocker_opening_db_with_lock, DataDir}),
                                open_database(DataDir)
                        end
                    after
                        global:del_lock(MutexKey, [node()])
                    end;
                false ->
                    % Could not acquire lock, wait and retry
                    ?event(debug, {rocker_waiting_for_lock, DataDir}),
                    timer:sleep(50 + rand:uniform(50)),
                    start(Opts)
            end
    end;
start(_) ->
    {error, {badarg, <<"StoreOpts must be a map with name key">>}}.

%% @doc Internal helper to open the RocksDB database
open_database(DataDir) ->
    ?event(debug, {rocker_calling_rocker_open, DataDir}),
    DBOptions = #{
        create_if_missing => true,
        set_max_open_files => 1000
    },
    case rocker:open(DataDir, DBOptions) of
        {ok, DBHandle} ->
            ?event(debug, {rocker_open_success, DataDir, DBHandle}),
            % Store the database handle in persistent_term for later cleanup
            StoreKey = {rocker, ?MODULE, DataDir},
            persistent_term:put(StoreKey, {DBHandle, DataDir}),
            ?event(debug, {rocker_stored_in_persistent_term, DataDir, StoreKey}),
            {ok, #{ <<"db">> => DBHandle }};
        {error, Reason} ->
            ?event(error, {rocker_open_failed, DataDir, Reason}),
            {error, Reason}
    end.

%% @doc Determine whether a key represents a simple value or composite group.
%%
%% This function reads the value associated with a key and examines its content
%% to classify the entry type. Keys storing the literal binary "group" are
%% considered composite (directory-like) entries, while all other values are
%% treated as simple key-value pairs.
%%
%% @param Opts Database configuration map
%% @param Key The key to examine
%% @returns 'composite' for group entries, 'simple' for regular values, 'not_found' if key doesn't exist
-spec type(map(), binary()) -> composite | simple | not_found.
type(Opts, Key) ->
    case read_direct(Opts, Key) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Link} ->
                    % This is a link, check the target's type
                    type(Opts, Link);
                false ->
                    case Value of
                        <<"group">> -> 
                            composite;
                        _ -> 
                            simple
                    end
            end;
        not_found -> not_found
    end.

%% @doc Write a key-value pair to the database.
%%
%% This function writes a key-value pair directly to RocksDB.
%%
%% @param Opts Database configuration map
%% @param Path Binary path to write or list of path segments
%% @param Value Binary value to store
%% @returns 'ok' on success, {error, Reason} on failure
-spec write(map(), binary() | list(), binary()) -> ok | {error, term()}.
write(Opts, PathParts, Value) when is_list(PathParts) ->
    % Convert to binary
    PathBin = to_path(PathParts),
    write(Opts, PathBin, Value);
write(Opts, Path, Value) ->
    #{ <<"db">> := DBHandle } = find_env(Opts),
    case rocker:put(DBHandle, Path, Value) of
        ok -> ok;
        {error, Reason} ->
            ?event(
                error,
                {rocker_error,
                    {reason, Reason},
                    {path, Path}
                }
            ),
            {error, Reason}
    end.

%% @doc Read a value from the database by key, with automatic link resolution.
%%
%% This function attempts to read a value directly from the RocksDB database.
%% The function automatically handles link resolution: if a stored value begins
%% with the "link:" prefix, it extracts the target key and recursively reads
%% from that location instead.
%%
%% @param Opts Database configuration map  
%% @param Path Binary key or list of path segments to read
%% @returns {ok, Value} on success, not_found if key doesn't exist, {error, Reason} on failure
-spec read(map(), binary() | list()) -> {ok, binary()} | not_found | {error, term()}.
read(Opts, PathParts) when is_list(PathParts) ->
    read(Opts, to_path(PathParts));
read(Opts, Path) ->
    % Try direct read first (fast path for non-link paths)
    case read_with_links(Opts, Path) of
        {ok, Value} -> 
            {ok, Value};
        not_found ->
            try
                PathParts = binary:split(Path, <<"/">>, [global]),
                case resolve_path_links(Opts, PathParts) of
                    {ok, ResolvedPathParts} ->
                        ResolvedPathBin = to_path(ResolvedPathParts),
                        read_with_links(Opts, ResolvedPathBin);
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
                            {stacktrace, Stacktrace},
                            {path, Path}
                        }
                    ),
                    % If link resolution fails, return not_found
                    not_found
            end
    end.

%% @doc Return the scope of this storage backend.
%%
%% The RocksDB implementation is always local-only and does not support distributed
%% operations.
%%
%% @returns 'local' always
-spec scope() -> local.
scope() -> local.

%% @doc Return the scope of this storage backend (ignores parameters).
%%
%% @param _Opts Ignored parameter
%% @returns 'local' always  
-spec scope(term()) -> local.
scope(_) -> scope().

%% @doc List all keys that start with a given prefix.
%%
%% This function provides directory-like navigation by finding all keys that
%% begin with the specified path prefix. It uses RocksDB's iterator to
%% efficiently scan through the database and collect matching keys.
%%
%% @param StoreOpts Database configuration map
%% @param Path Binary prefix to search for
%% @returns {ok, [Key]} list of matching keys, {error, Reason} on failure
-spec list(map(), binary()) -> {ok, [binary()]} | {error, term()}.
list(Opts, Path) ->
    % Check if Path is a link and resolve it if necessary
    ResolvedPath =
        case read_direct(Opts, Path) of
            {ok, Value} ->
                case is_link(Value) of
                    {true, Link} ->
                        Link;
                    false ->
                        % Not a link; use original path
                        Path
                end;
            not_found ->
                Path
        end,
    SearchPath = 
        case ResolvedPath of
            <<>> -> <<"">>;   % Root paths
            <<"/">> -> <<"">>;
            _ -> <<ResolvedPath/binary, "/">>
        end,
    DBKeys =
        case matching_db_keys(SearchPath, Opts) of
            {ok, Keys} -> Keys;
            not_found -> []
        end,
    {ok, DBKeys}.

%% @doc Create a group entry that can contain other keys hierarchically.
%%
%% Groups in the HyperBeam system represent composite entries that can contain
%% child elements, similar to directories in a filesystem. This function creates
%% a group by storing the special value "group" at the specified key.
%%
%% @param Opts Database configuration map
%% @param GroupName Binary name for the group
%% @returns Result of the write operation
-spec make_group(map(), binary()) -> ok | {error, term()}.
make_group(Opts, GroupName) when is_map(Opts), is_binary(GroupName) ->
    write(Opts, GroupName, <<"group">>);
make_group(_,_) ->
    {error, {badarg, <<"StoreOps must be map and GroupName must be a binary">>}}.

%% @doc Create a symbolic link from a new key to an existing key.
%%
%% This function implements a symbolic link mechanism by storing a special
%% "link:" prefixed value at the new key location. When the new key is read,
%% the system will automatically resolve the link and return the value from
%% the target key instead.
%%
%% @param StoreOpts Database configuration map
%% @param Existing The key that already exists and contains the target value
%% @param New The new key that should link to the existing key
%% @returns Result of the write operation
-spec make_link(map(), binary() | list(), binary()) -> ok | {error, term()}.
make_link(Opts, Existing, New) when is_list(Existing) ->
    ExistingBin = to_path(Existing),
    make_link(Opts, ExistingBin, New);
make_link(Opts, Existing, New) ->
   ExistingBin = hb_util:bin(Existing),
   % Ensure parent groups exist for the new link path
   ensure_parent_groups(Opts, New),
   write(Opts, New, <<"link:", ExistingBin/binary>>). 

%% @doc Transform a path into the store's canonical form.
%% For RocksDB, paths are simply joined with "/" separators.
path(_Opts, PathParts) when is_list(PathParts) ->
    to_path(PathParts);
path(_Opts, Path) when is_binary(Path) ->
    Path.

%% @doc Add two path components together.
%% For RocksDB, this concatenates the path lists.
add_path(_Opts, Path1, Path2) when is_list(Path1), is_list(Path2) ->
    Path1 ++ Path2;
add_path(Opts, Path1, Path2) when is_binary(Path1), is_binary(Path2) ->
    % Convert binaries to lists, concatenate, then convert back
    Parts1 = binary:split(Path1, <<"/">>, [global]),
    Parts2 = binary:split(Path2, <<"/">>, [global]),
    path(Opts, Parts1 ++ Parts2);
add_path(Opts, Path1, Path2) when is_list(Path1), is_binary(Path2) ->
    Parts2 = binary:split(Path2, <<"/">>, [global]),
    path(Opts, Path1 ++ Parts2);
add_path(Opts, Path1, Path2) when is_binary(Path1), is_list(Path2) ->
    Parts1 = binary:split(Path1, <<"/">>, [global]),
    path(Opts, Parts1 ++ Path2).

%% @doc Resolve a path by following any symbolic links.
%%
%% For RocksDB, we handle links through our own "link:" prefix mechanism.
%% This function resolves link chains in paths, similar to filesystem symlink resolution.
%%
%% @param StoreOpts Database configuration map
%% @param Path The path to resolve (binary or list)
%% @returns The resolved path as a binary
-spec resolve(map(), binary() | list()) -> binary().
resolve(Opts, Path) when is_binary(Path) ->
    resolve(Opts, binary:split(Path, <<"/">>, [global]));
resolve(Opts, PathParts) when is_list(PathParts) ->
    % Handle list paths by resolving directly and converting to binary
    case resolve_path_links(Opts, PathParts) of
        {ok, ResolvedParts} ->
            to_path(ResolvedParts);
        {error, _} ->
            % If resolution fails, return original path as binary
            to_path(PathParts)
    end;
resolve(_,_) -> not_found.

%% Shutdown RocksDB database and cleanup resources
stop(#{ <<"store-module">> := ?MODULE, <<"name">> := DataDir }) ->
    ?event(debug, {rocker_stop_called, DataDir}),
    StoreKey = {rocker, ?MODULE, DataDir},
    close_database(StoreKey, DataDir);
stop(InvalidStoreOpts) ->
    ?event(debug, {rocker_stop_invalid_opts, InvalidStoreOpts}),
    ok.

%% @doc Completely delete the database directory and all its contents.
%%
%% This is a destructive operation that removes all data from the specified
%% database. It first performs a graceful shutdown to ensure data consistency,
%% then uses the system shell to recursively delete the entire database
%% directory structure.
%%
%% @param StoreOpts Database configuration map containing the directory name
%% @returns 'ok' when deletion is complete
reset(Opts) ->
    case maps:get(<<"name">>, Opts, undefined) of
        undefined ->
            % No name specified, nothing to reset
            ok;
        DataDir ->
            % Stop the store and remove the database.
            stop(Opts),
            os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
            ok
    end.

%%% ============================================================================
%%% Private Helper Functions
%%% ============================================================================

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

%% @doc Helper function to convert path parts to a path
to_path(PathParts) ->
    hb_util:bin(lists:join(<<"/">>, PathParts)).

%% @doc Read a value directly from the database without link resolution.
%% Returns {ok, Value} or not_found.
read_direct(Opts, Path) ->
    #{ <<"db">> := DBHandle } = find_env(Opts),
    case rocker:get(DBHandle, Path) of
        {ok, Value} -> {ok, Value};
        not_found -> not_found;
        {error, _Reason} -> not_found
    end.

%% @doc Read a value directly from the database with link resolution.
%% This is the internal implementation that handles actual database reads.
read_with_links(Opts, Path) ->
    case read_direct(Opts, Path) of
        {ok, Value} ->
            % Check if this value is actually a link to another key
            case is_link(Value) of
                {true, Link} -> 
                   % Extract the target key and recursively resolve the link
                   read_with_links(Opts, Link);
                false ->
                    % Check if this is a group marker - groups should not be
                    % readable as simple values
                    case Value of
                        <<"group">> -> not_found;
                        _ -> {ok, Value}
                    end
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

%% @doc Determine if a key matches a path prefix. Returns `{true, Child}'
%% if the key matches the prefix, and `false' if it does not.
match_path(Prefix, Path) when byte_size(Prefix) > byte_size(Path) ->
    false;
match_path(Prefix, Path) ->
    PathPrefix = binary:part(Path, 0, byte_size(Prefix)),
    case PathPrefix of
        Prefix ->
            % Return the part of the path after the prefix.
            {
                true,
                hd(
                    binary:split(
                        binary:part(
                            Path,
                            byte_size(Prefix),
                            byte_size(Path) - byte_size(Prefix)
                        ),
                        <<"/">>
                    )
                )
            };
        _ -> false
    end.

%% @doc Find all keys that match the given path prefix from the RocksDB database.
matching_db_keys(SearchPrefix, Opts) ->
    #{ <<"db">> := DBHandle } = find_env(Opts),
    case rocker:iterator(DBHandle, {'from', SearchPrefix, forward}) of
        {ok, Iterator} ->
            % Start collecting keys from the prefix
            collect_matching_keys_from_iterator(Iterator, SearchPrefix, []);
        {error, _Reason} ->
            {ok, []}
    end.

%% @doc Collect keys that match the prefix from the iterator
%% @doc Collect keys that match the prefix from the rocker iterator
collect_matching_keys_from_iterator(Iterator, SearchPrefix, Acc) ->
    case rocker:next(Iterator) of
        {ok, Key, _Value} ->
            case match_path(SearchPrefix, Key) of
                {true, Child} ->
                    % This key matches, collect the child name and continue
                    collect_matching_keys_from_iterator(Iterator, SearchPrefix, [Child | Acc]);
                false ->
                    % Key doesn't match prefix anymore, we're done
                    {ok, lists:usort(Acc)}
            end;
        end_of_iterator ->
            % End of iterator
            {ok, lists:usort(Acc)}
    end.

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
            make_group(Opts, GroupPath);
        {ok, _} ->
            % Already exists, skip
            ok
    end,
    create_parent_groups(Opts, NewCurrent, Rest).

%% @doc Retrieve or create the RocksDB database handle.
find_env(Opts) -> hb_store:find(Opts).

%% Close database using persistent_term lookup with fallback
close_database(StoreKey, DataDir) ->
    case safe_get_persistent_term(StoreKey) of
        {ok, DBHandle} ->
            close_and_cleanup(DBHandle, StoreKey, DataDir);
        not_found ->
            ?event(debug, {rocker_stop_not_found_in_persistent_term, DataDir})
    end,
    ok.

%% Get database handle from persistent_term without exceptions
safe_get_persistent_term(Key) ->
    case persistent_term:get(Key, undefined) of
        {DBHandle, _DataDir} -> {ok, DBHandle};
        _ -> not_found
    end.

%% Close database handle and cleanup persistent_term entry
close_and_cleanup(DBHandle, StoreKey, DataDir) ->
    CloseResult = safe_close_db(DBHandle),
    case CloseResult of
        ok -> 
            % Only erase from persistent_term if we actually closed the database
            persistent_term:erase(StoreKey),
            ?event(debug, {rocker_stop_success, DataDir});
        {error, no_close_function} ->
            % Can't close database, so keep the handle in persistent_term for reuse
            ?event(debug, {rocker_keeping_db_handle_open, DataDir});
        {error, Reason} -> 
            % Close failed for other reasons, still remove from persistent_term
            persistent_term:erase(StoreKey),
            ?event(debug, {rocker_stop_error, Reason})
    end.

%% Close database handle with error capture
safe_close_db(DBHandle) ->
    try
        % Try common RocksDB close function names
        case catch rocker:close(DBHandle) of
            {'EXIT', {undef, _}} ->
                % close/1 doesn't exist, try other names
                case catch rocker:db_close(DBHandle) of
                    {'EXIT', {undef, _}} ->
                        % No explicit close function found, just remove reference
                        ?event(debug, {rocker_no_close_function_found, DBHandle}),
                        {error, no_close_function};
                    Other -> Other
                end;
            Result -> Result
        end
    catch
        error:Reason -> {error, Reason}
    end.

%%% ============================================================================
%%% Tests
%%% ============================================================================

%% @doc Basic store test - verifies fundamental read/write functionality.
basic_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/rocker-store-1">>
    },
    reset(StoreOpts),
    {ok, _} = start(StoreOpts),
    Res = write(StoreOpts, <<"Hello">>, <<"World2">>),
    ?assertEqual(ok, Res),
    {ok, Value} = read(StoreOpts, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    ok = stop(StoreOpts).

%% @doc Group test - verifies group creation and type detection.
group_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/rocker-store-group">>
    },
    reset(StoreOpts),
    {ok, _} = start(StoreOpts),
    make_group(StoreOpts, <<"colors">>),
    % Groups should be detected as composite types
    ?assertEqual(composite, type(StoreOpts, <<"colors">>)),
    % Groups should not be readable directly (like directories in filesystem)
    ?assertEqual(not_found, read(StoreOpts, <<"colors">>)),
    ok = stop(StoreOpts).

%% @doc Link test - verifies symbolic link creation and resolution.
link_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/rocker-store-link">>
    },
    reset(StoreOpts),
    {ok, _} = start(StoreOpts),
    write(StoreOpts, <<"foo/bar/baz">>, <<"Bam">>),
    make_link(StoreOpts, <<"foo/bar/baz">>, <<"foo/beep/baz">>),
    {ok, Result} = read(StoreOpts, <<"foo/beep/baz">>),
    ?assertEqual(<<"Bam">>, Result),
    ok = stop(StoreOpts).

%% @doc List test - verifies prefix-based key listing functionality.
list_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/rocker-store-list">>
    },
    reset(StoreOpts),
    {ok, _} = start(StoreOpts),
    
    % Create immediate children under colors/
    write(StoreOpts, <<"colors/red">>, <<"1">>),
    write(StoreOpts, <<"colors/blue">>, <<"2">>),
    write(StoreOpts, <<"colors/green">>, <<"3">>),
    
    % Create nested directories under colors/
    write(StoreOpts, <<"colors/multi/foo">>, <<"4">>),
    write(StoreOpts, <<"colors/multi/bar">>, <<"5">>),
    
    % Test listing colors/ - should return immediate children only
    {ok, ListResult} = list(StoreOpts, <<"colors">>),
    
    % Expected: red, blue, green (files) + multi (directory)
    ExpectedChildren = [<<"blue">>, <<"green">>, <<"multi">>, <<"red">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedChildren) end, ListResult)),
    
    ok = stop(StoreOpts).