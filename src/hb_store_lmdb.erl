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
-export([start/1, stop/1, scope/0, scope/1, reset/1]).
-export([read/2, write/3, list/2]).
-export([make_group/2, make_link/3, type/2]).
-export([path/2, add_path/3, sync/1, resolve/2]).
-export([resolve_path_links/2, find_env/1]).

%% Test framework and project includes
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% Configuration constants with reasonable defaults
-define(DEFAULT_SIZE, 16 * 1024 * 1024 * 1024).  % 16GB default database size
-define(CONNECT_TIMEOUT, 3000).                   % 3 second timeout for server communication
-define(DEFAULT_IDLE_FLUSH_TIME, 5).              % 5ms idle time before auto-flush
-define(DEFAULT_MAX_FLUSH_TIME, 50).              % 50ms maximum time between flushes

%% @doc Start the LMDB storage system for a given database configuration.
%%
%% This function initializes or connects to an existing LMDB database instance.
%% It uses a singleton pattern, so multiple calls with the same configuration
%% will return the same server process. The server process manages the LMDB
%% environment and coordinates all database operations.
%%
%% The StoreOpts map must contain a "prefix" key specifying the
%% database directory path. Also the required configuration includes "max-size" for
%% the maximum database size and flush timing parameters.
%%
%% @param StoreOpts A map containing database configuration options
%% @returns {ok, ServerPid} on success, {error, Reason} on failure
-spec start(map()) -> {ok, pid()} | {error, term()}.
start(StoreOpts) when is_map(StoreOpts) ->
    {ok, find_or_spawn_instance(StoreOpts)};
start(_) ->
    {error, {badarg, "StoreOpts must be a map"}}.

%% @doc Determine whether a key represents a simple value or composite group.
%%
%% This function reads the value associated with a key and examines its content
%% to classify the entry type. Keys storing the literal binary "group" are
%% considered composite (directory-like) entries, while all other values are
%% treated as simple key-value pairs.
%%
%% This classification is used by higher-level HyperBeam components to understand
%% the structure of stored data and provide appropriate navigation interfaces.
%%
%% @param Opts Database configuration map
%% @param Key The key to examine
%% @returns 'composite' for group entries, 'simple' for regular values
-spec type(map(), binary()) -> composite | simple | not_found.
type(Opts, Key) ->
    ?event(debug_type_detection, {checking_type_for_key, Key}),
    case lmdb:get(find_env(Opts), Key) of
        {ok, Value} ->
            ?event(debug_type_detection, {found_value, Key, Value, byte_size(Value)}),
            LinkPrefixSize = byte_size(<<"link:">>),
            case byte_size(Value) > LinkPrefixSize andalso
                binary:part(Value, 0, LinkPrefixSize) =:= <<"link:">> of
                true ->
                    % This is a link, check the target's type
                    Link = binary:part(Value, LinkPrefixSize, byte_size(Value) - LinkPrefixSize),
                    ?event(debug_type_detection, {following_link, Key, Link}),
                    type(Opts, Link);
                false ->
                    case Value of
                        <<"group">> -> 
                            ?event(debug_type_detection, {key_is_group, Key}),
                            composite;
                        _ -> 
                            ?event(debug_type_detection, {key_is_simple, Key, Value}),
                            simple
                    end
            end;
        not_found ->
            ?event(debug_type_detection, {key_not_found_triggering_flush, Key}),
            % Key not found in committed data, trigger flush and retry
            find_or_spawn_instance(Opts) ! {flush, self(), Ref = make_ref()},
            receive
                {flushed, Ref} -> 
                    case lmdb:get(find_env(Opts), Key) of
                        {ok, Value} ->
                            ?event(debug_type_detection, {found_after_flush, Key, Value}),
                            LinkPrefixSize = byte_size(<<"link:">>),
                            case byte_size(Value) > LinkPrefixSize andalso
                                binary:part(Value, 0, LinkPrefixSize) =:= <<"link:">> of
                                true ->
                                    Link = binary:part(Value, LinkPrefixSize, byte_size(Value) - LinkPrefixSize),
                                    type(Opts, Link);
                                false ->
                                    case Value of
                                        <<"group">> -> composite;
                                        _ -> simple
                                    end
                            end;
                        not_found ->
                            ?event(debug_type_detection, {still_not_found_after_flush_checking_children, Key}),
                            % Still not found after flush, check if this is a composite by seeing if it has children
                            case list(Opts, Key) of
                                {ok, []} -> 
                                    ?event(debug_type_detection, {no_children_not_found, Key}),
                                    not_found;  % No children, doesn't exist
                                {ok, Children} -> 
                                    ?event(debug_type_detection, {has_children_composite, Key, Children}),
                                    composite;  % Has children, it's a composite
                                {error, Error} -> 
                                    ?event(debug_type_detection, {list_error_not_found, Key, Error}),
                                    not_found
                            end
                    end
            after ?CONNECT_TIMEOUT -> 
                ?event(debug_type_detection, {flush_timeout, Key}),
                not_found
            end
    end.

%% @doc Write a key-value pair to the database asynchronously.
%%
%% This function sends a write request to the database server process and returns
%% immediately without waiting for the write to be committed to disk. The server
%% accumulates writes in a transaction that is periodically flushed based on
%% timing constraints or explicit flush requests.
%%
%% The asynchronous nature provides better performance for write-heavy workloads
%% while the batching strategy ensures data consistency and reduces I/O overhead.
%% However, recent writes may not be immediately visible to readers until the
%% next flush occurs.
%%
%% @param StoreOpts Database configuration map
%% @param Key Binary key to write
%% @param Value Binary value to store
%% @returns 'ok' immediately (write happens asynchronously)
-spec write(map(), binary() | list(), binary()) -> ok.
write(StoreOpts, Key, Value) when is_list(Key) ->
    KeyBin = hb_util:bin(lists:join(<<"/">>, Key)),
    write(StoreOpts, KeyBin, Value);
write(StoreOpts, Key, Value) ->
    PID = find_or_spawn_instance(StoreOpts),
    PID ! {write, Key, Value},
    ok.

%% @doc Read a value from the database by key, with automatic link resolution.
%%
%% This function attempts to read a value directly from the committed database.
%% If the key is not found, it triggers a flush operation to ensure any pending
%% writes are committed before retrying the read.
%%
%% The function automatically handles link resolution: if a stored value begins
%% with the "link:" prefix, it extracts the target key and recursively reads
%% from that location instead. This creates a symbolic link mechanism that
%% allows multiple keys to reference the same underlying data.
%%
%% When given a list of path segments, the function first attempts a direct read
%% for optimal performance. Only if the direct read fails does it perform link
%% resolution at each level of the path except the final segment, allowing path
%% traversal through symbolic links to work transparently.
%%
%% Link resolution is transparent to the caller and can chain through multiple
%% levels of indirection, though care should be taken to avoid circular references.
%%
%% @param StoreOpts Database configuration map  
%% @param Key Binary key or list of path segments to read
%% @returns {ok, Value} on success, {error, Reason} on failure
-spec read(map(), binary() | list()) -> {ok, binary()} | {error, term()}.
read(StoreOpts, Key) when is_list(Key) ->
    % Try direct read first (fast path for non-link paths)
    DirectKeyBin = hb_util:bin(lists:join(<<"/">>, Key)),
    ?event(debug_nested_read, {path_list, Key, direct_key, DirectKeyBin}),
    case read_direct(StoreOpts, DirectKeyBin) of
        {ok, Value} -> 
            ?event(debug_nested_read_direct_success, {key, DirectKeyBin, value_size, byte_size(Value)}),
            {ok, Value};
        not_found ->
            % Direct read failed, try link resolution (slow path)
            ?event(debug_nested_read_trying_link_resolution, {path, Key}),
            try
                case resolve_path_links(StoreOpts, Key) of
                    {ok, ResolvedPath} ->
                        ResolvedKeyBin = hb_util:bin(lists:join(<<"/">>, ResolvedPath)),
                        ?event(debug_nested_read_link_resolved, {original, Key, resolved, ResolvedPath, resolved_key, ResolvedKeyBin}),
                        read_direct(StoreOpts, ResolvedKeyBin);
                    {error, _} ->
                        ?event(debug_nested_read_link_resolution_failed, {path, Key}),
                        % Convert errors to not_found for hb_store compatibility
                        not_found
                end
            catch
                Class:Reason:Stacktrace ->
                    ?event(error, {resolve_path_links_failed, Class, Reason, Stacktrace}),
                    % If link resolution fails, return not_found
                    not_found
            end;
        Error -> 
            Error
    end;
read(StoreOpts, Key) ->
    read_direct(StoreOpts, Key).

%% @doc Read a value directly from the database with link resolution.
%% This is the internal implementation that handles actual database reads.
read_direct(StoreOpts, Key) ->
    LinkPrefixSize = byte_size(<<"link:">>),
    case lmdb:get(find_env(StoreOpts), Key) of
        {ok, Value} ->
            % Check if this value is actually a link to another key
            case byte_size(Value) > LinkPrefixSize andalso
                binary:part(Value, 0, LinkPrefixSize) =:= <<"link:">> of
                true -> 
                   % Extract the target key and recursively resolve the link
                   Link = binary:part(Value, LinkPrefixSize, byte_size(Value) - LinkPrefixSize),
                   read(StoreOpts, Link);
                false ->
                    % Check if this is a group marker - groups should not be readable as simple values
                    case Value of
                        <<"group">> ->
                            ?event(debug_lmdb_read, {refusing_to_read_group_marker, Key}),
                            % Groups should be accessed via list/type, not read directly
                            % This makes LMDB behave like filesystem where directories cannot be read as files
                            not_found;
                        _ ->
                            % Regular value, return as-is
                            {ok, Value}
                    end
            end;
        not_found ->
            % Key not found in committed data, trigger flush and retry
            ?event(read_miss, {miss, Key}),
            find_or_spawn_instance(StoreOpts) ! {flush, self(), Ref = make_ref()},
            receive
                {flushed, Ref} -> 
                    case lmdb:get(find_env(StoreOpts), Key) of
                        {ok, Value} -> {ok, Value};
                        not_found -> not_found
                    end
            after ?CONNECT_TIMEOUT -> {error, timeout}
            end
    end.

%% @doc Resolve links in a path, checking each segment except the last.
%% Returns the resolved path where any intermediate links have been followed.
resolve_path_links(StoreOpts, Path) ->
    resolve_path_links(StoreOpts, Path, 0).

%% Internal helper with depth limit to prevent infinite loops
resolve_path_links(_StoreOpts, _Path, Depth) when Depth > 10 ->
    % Prevent infinite loops with depth limit
    {error, too_many_redirects};
resolve_path_links(_StoreOpts, [LastSegment], _Depth) ->
    % Base case: only one segment left, no link resolution needed
    {ok, [LastSegment]};
resolve_path_links(StoreOpts, [Head | Tail], Depth) ->
    % Check if the first segment (Head) is a link
    case lmdb:get(find_env(StoreOpts), Head) of
        {ok, Value} ->
            LinkPrefixSize = byte_size(<<"link:">>),
            case byte_size(Value) > LinkPrefixSize andalso
                binary:part(Value, 0, LinkPrefixSize) =:= <<"link:">> of
                true ->
                    % This segment is a link, resolve it
                    Link = binary:part(Value, LinkPrefixSize, byte_size(Value) - LinkPrefixSize),
                    LinkSegments = binary:split(Link, <<"/">>, [global]),
                    % Replace Head with the link target and continue resolving
                    resolve_path_links(StoreOpts, LinkSegments ++ Tail, Depth + 1);
                false ->
                    % Not a link, continue with the rest of the path
                    case resolve_path_links(StoreOpts, Tail, Depth) of
                        {ok, ResolvedTail} ->
                            {ok, [Head | ResolvedTail]};
                        {error, _} = Error ->
                            Error
                    end
            end;
        not_found ->
            % Segment doesn't exist, continue without resolution
            case resolve_path_links(StoreOpts, Tail, Depth) of
                {ok, ResolvedTail} ->
                    {ok, [Head | ResolvedTail]};
                {error, _} = Error ->
                    Error
            end
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
%% begin with the specified path prefix. It uses LMDB's fold operation to
%% efficiently scan through the database and collect matching keys.
%%
%% The implementation only returns keys that are longer than the prefix itself,
%% ensuring that the prefix acts like a directory separator. For example,
%% listing with prefix "colors" will return "colors/red" and "colors/blue"
%% but not "colors" itself.
%%
%% This is particularly useful for implementing hierarchical data organization
%% and providing tree-like navigation interfaces in applications.
%%
%% @param StoreOpts Database configuration map
%% @param Path Binary prefix to search for
%% @returns {ok, [Key]} list of matching keys, {error, Reason} on failure
-spec list(map(), binary()) -> {ok, [binary()]} | {error, term()}.
list(StoreOpts, Path) when is_map(StoreOpts), is_binary(Path) ->
    Env = find_env(StoreOpts),
    PathSize = byte_size(Path),
    try
       Children = lmdb:fold(Env, default,
           fun(Key, _Value, Acc) ->
               % Only match on keys that have the prefix and are longer than it
               case byte_size(Key) > PathSize andalso 
                    binary:part(Key, 0, PathSize) =:= Path of
                  true -> 
                      % Return key without the path prefix and separator
                      KeyWithoutPrefix = binary:part(Key, PathSize, byte_size(Key) - PathSize),
                      % Remove leading separator if present
                      CleanKey = case KeyWithoutPrefix of
                          <<"/", Rest/binary>> -> Rest;
                          Other -> Other
                      end,
                      % Only include immediate children (no nested paths)
                      case binary:match(CleanKey, <<"/">>) of
                          nomatch -> 
                              % This is an immediate child, add it if not already present
                              case lists:member(CleanKey, Acc) of
                                  true -> Acc;
                                  false -> [CleanKey | Acc]
                              end;
                          _ -> 
                              % This is a nested path, extract only the immediate child part
                              [ImmediateChild | _] = binary:split(CleanKey, <<"/">>, [global]),
                              case lists:member(ImmediateChild, Acc) of
                                  true -> Acc;
                                  false -> [ImmediateChild | Acc]
                              end
                      end;
                  false -> Acc
               end
           end,
           []
       ),
       Children
    catch
       _:Error -> {error, Error}
    end;
list(_, _) ->
    {error, {badarg, "StoreOpts must be a map and Path must be an binary"}}.

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
%% Groups can be identified later using the type/2 function, which will return
%% 'composite' for group entries versus 'simple' for regular key-value pairs.
%%
%% @param StoreOpts Database configuration map
%% @param GroupName Binary name for the group
%% @returns Result of the write operation
-spec make_group(map(), binary()) -> ok | {error, term()}.
make_group(StoreOpts, GroupName) when is_map(StoreOpts), is_binary(GroupName) ->
    write(StoreOpts, GroupName, hb_util:bin(group));
make_group(_,_) ->
    {error, {badarg, "StoreOps must be map and GroupName must be a binary"}}.

%% @doc Ensure all parent groups exist for a given path.
%%
%% This function creates the necessary parent groups for a path, similar to
%% how filesystem stores use ensure_dir. For example, if the path is
%% "a/b/c/file", it will ensure groups "a", "a/b", and "a/b/c" exist.
%%
%% @param StoreOpts Database configuration map
%% @param Path The path whose parents should exist
%% @returns ok
-spec ensure_parent_groups(map(), binary()) -> ok.
ensure_parent_groups(StoreOpts, Path) ->
    PathParts = binary:split(Path, <<"/">>, [global]),
    case PathParts of
        [_] -> 
            % Single segment, no parents to create
            ok;
        _ ->
            % Multiple segments, create parent groups
            ParentParts = lists:droplast(PathParts),
            create_parent_groups(StoreOpts, [], ParentParts)
    end.

%% @doc Helper function to recursively create parent groups.
create_parent_groups(_StoreOpts, _Current, []) ->
    ok;
create_parent_groups(StoreOpts, Current, [Next | Rest]) ->
    NewCurrent = Current ++ [Next],
    GroupPath = hb_util:bin(lists:join(<<"/">>, NewCurrent)),
    % Only create group if it doesn't already exist - use direct LMDB check to avoid recursion
    case lmdb:get(find_env(StoreOpts), GroupPath) of
        not_found ->
            make_group(StoreOpts, GroupPath);
        {ok, _} ->
            % Already exists, skip
            ok
    end,
    create_parent_groups(StoreOpts, NewCurrent, Rest).

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
-spec make_link(map(), binary() | list(), binary()) -> ok.
make_link(StoreOpts, Existing, New) when is_list(Existing) ->
    ExistingBin = hb_util:bin(lists:join(<<"/">>, Existing)),
    make_link(StoreOpts, ExistingBin, New);
make_link(StoreOpts, Existing, New) ->
   ExistingBin = hb_util:bin(Existing),
   % Ensure parent groups exist for the new link path (like filesystem ensure_dir)
   ensure_parent_groups(StoreOpts, New),
   write(StoreOpts, New, <<"link:", ExistingBin/binary>>). 

%% @doc Transform a path into the store's canonical form.
%% For LMDB, paths are simply joined with "/" separators.
path(_StoreOpts, Path) when is_list(Path) ->
    hb_util:bin(lists:join(<<"/">>, Path));
path(_StoreOpts, Path) when is_binary(Path) ->
    Path.

%% @doc Add two path components together.
%% For LMDB, this concatenates the path lists.
add_path(_StoreOpts, Path1, Path2) when is_list(Path1), is_list(Path2) ->
    Path1 ++ Path2;
add_path(StoreOpts, Path1, Path2) when is_binary(Path1), is_binary(Path2) ->
    % Convert binaries to lists, concatenate, then convert back
    Parts1 = binary:split(Path1, <<"/">>, [global]),
    Parts2 = binary:split(Path2, <<"/">>, [global]),
    path(StoreOpts, Parts1 ++ Parts2);
add_path(StoreOpts, Path1, Path2) when is_list(Path1), is_binary(Path2) ->
    Parts2 = binary:split(Path2, <<"/">>, [global]),
    path(StoreOpts, Path1 ++ Parts2);
add_path(StoreOpts, Path1, Path2) when is_binary(Path1), is_list(Path2) ->
    Parts1 = binary:split(Path1, <<"/">>, [global]),
    path(StoreOpts, Parts1 ++ Path2).

%% @doc Force an immediate flush of all pending writes to disk.
%%
%% This function synchronously forces the database server to commit any
%% pending writes in the current transaction. It blocks until the flush
%% operation is complete, ensuring that all previously written data is
%% durably stored before returning.
%%
%% This is useful when you need to ensure data is persisted immediately,
%% rather than waiting for the automatic flush timers to trigger. Common
%% use cases include critical checkpoints, before system shutdown, or
%% when preparing for read operations that must see the latest writes.
%%
%% @param StoreOpts Database configuration map
%% @returns 'ok' when flush is complete, {error, Reason} on failure
-spec sync(map()) -> ok | {error, term()}.
sync(StoreOpts) ->
    PID = find_or_spawn_instance(StoreOpts),
    PID ! {flush, self(), Ref = make_ref()},
    receive
        {flushed, Ref} -> ok
    after ?CONNECT_TIMEOUT -> {error, timeout}
    end.

%% @doc Resolve a path by following any symbolic links.
%%
%% For LMDB, we handle links through our own "link:" prefix mechanism.
%% This function resolves link chains in paths, similar to filesystem symlink resolution.
%% It's used by the cache to resolve paths before type checking and reading.
%%
%% @param StoreOpts Database configuration map
%% @param Path The path to resolve (binary or list)
%% @returns The resolved path as a binary
-spec resolve(map(), binary() | list()) -> binary().
resolve(StoreOpts, Path) when is_binary(Path) ->
    % Convert binary path to list for resolution, then back to binary
    PathParts = binary:split(Path, <<"/">>, [global]),
    ?event(debug_resolve, {resolving_binary_path, Path, path_parts, PathParts}),
    case resolve_path_links(StoreOpts, PathParts) of
        {ok, ResolvedParts} ->
            Result = hb_util:bin(lists:join(<<"/">>, ResolvedParts)),
            ?event(debug_resolve, {resolved_successfully, Path, to, Result}),
            Result;
        {error, Reason} ->
            % If resolution fails, return original path
            ?event(debug_resolve, {resolution_failed, Path, reason, Reason}),
            Path
    end;
resolve(StoreOpts, Path) when is_list(Path) ->
    % Handle list paths by resolving directly and converting to binary
    ?event(debug_resolve, {resolving_list_path, Path}),
    case resolve_path_links(StoreOpts, Path) of
        {ok, ResolvedParts} ->
            Result = hb_util:bin(lists:join(<<"/">>, ResolvedParts)),
            ?event(debug_resolve, {resolved_list_successfully, Path, to, Result}),
            Result;
        {error, Reason} ->
            % If resolution fails, return original path as binary
            OrigPath = hb_util:bin(lists:join(<<"/">>, Path)),
            ?event(debug_resolve, {list_resolution_failed, Path, reason, Reason, returning, OrigPath}),
            OrigPath
    end.

%% @doc Retrieve or create the LMDB environment handle for a database.
%%
%% This function manages the LMDB environment handles using a two-level caching
%% strategy. First, it checks the process dictionary for a cached handle. If not
%% found, it requests the handle from the singleton server process and caches
%% it locally for future use.
%%
%% The caching strategy improves performance by avoiding repeated server
%% communication for read operations while ensuring that all processes share
%% the same underlying database environment.
%%
%% Environment handles are lightweight references that can be safely shared
%% between processes and cached indefinitely as long as the server remains alive.
%%
%% @param StoreOpts Database configuration map containing the directory prefix
%% @returns LMDB environment handle or 'timeout' on communication failure
find_env(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    case get({?MODULE, DataDir}) of
        undefined ->
            % Not cached locally, request from server
            ?event(debug_process_cache, {not_in_env_cache, {?MODULE, DataDir}}),
            PID = find_or_spawn_instance(StoreOpts),
            PID ! {get_env, self(), Ref = make_ref()},
            receive
                {env, Env, Ref} ->
                    % Cache the environment handle in process dictionary
                    put({?MODULE, DataDir}, Env),
                    Env
            after ?CONNECT_TIMEOUT -> timeout
            end;
        Env -> Env
    end.

%% @doc Locate an existing server process or spawn a new one if needed.
%%
%% This function implements the singleton pattern for server processes using
%% a two-tier lookup strategy. First, it checks the local process dictionary
%% for a cached server PID. If not found, it consults the global process
%% registry (hb_name) to see if another process has already started a server
%% for this database directory.
%%
%% Only if no server exists anywhere in the system will a new one be spawned.
%% This ensures that each database directory has exactly one server process
%% regardless of how many client processes are accessing it.
%%
%% The caching in the process dictionary improves performance by avoiding
%% registry lookups on subsequent calls from the same process.
%%
%% @param StoreOpts Database configuration map containing the directory prefix
%% @returns PID of the server process (existing or newly created)
find_or_spawn_instance(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    case get({?MODULE, {server, DataDir}}) of
        undefined ->
            % Not cached locally, check global registry
            ?event(debug_process_cache, {not_in_process_cache, {?MODULE, DataDir}}),
            case hb_name:lookup({?MODULE, DataDir}) of
                undefined ->
                    % No server exists anywhere, create a new one
                    Pid = start_server(StoreOpts),
                    Pid;
                Pid -> Pid
            end;
        Pid ->
            Pid
    end.

%% @doc Gracefully shut down the database server and close the environment.
%%
%% This function performs an orderly shutdown of the database system by first
%% stopping the server process (which flushes any pending writes) and then
%% closing the LMDB environment to release system resources.
%%
%% The shutdown process ensures that no data is lost and all file handles
%% are properly closed. After calling stop, the database can be restarted
%% by calling any other function that triggers server creation.
%%
%% @param StoreOpts Database configuration map
%% @returns 'ok' when shutdown is complete
stop(StoreOpts) when is_map(StoreOpts) ->
    case maps:get(<<"prefix">>, StoreOpts, undefined) of
        undefined ->
            % No prefix specified, nothing to stop
            ok;
        DataDir ->
            PID = find_or_spawn_instance(StoreOpts),
            Env = find_env(StoreOpts),
            PID ! stop,
            % Clean up process dictionary entries for this database
            erase({?MODULE, DataDir}),
            erase({?MODULE, {server, DataDir}}),
            lmdb:env_close(Env),
            ok
    end;
stop(_) ->
    % Invalid argument, ignore
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
reset(StoreOpts) when is_map(StoreOpts) ->
    case maps:get(<<"prefix">>, StoreOpts, undefined) of
        undefined ->
            % No prefix specified, nothing to reset
            ok;
        DataDir ->
            % Only stop the store if it's actually running
            case get({?MODULE, DataDir}) of
                undefined ->
                    % Check if there's a server running without env cached
                    case hb_name:lookup({?MODULE, DataDir}) of
                        undefined -> 
                            % No server running, just remove the directory
                            ok;
                        _Pid -> 
                            % Server exists, stop it properly
                            stop(StoreOpts)
                    end;
                _Env ->
                    % Environment is cached, stop normally
                    stop(StoreOpts)
            end,
            os:cmd(binary_to_list(<< "rm -Rf ", DataDir/binary >>)),
            ok
    end;
reset(_) ->
    % Invalid argument, ignore
    ok.

%% @doc Initialize a new server process for managing database operations.
%%
%% This function creates the complete server infrastructure for a database
%% instance. It first ensures the database directory exists, then creates
%% the LMDB environment with the specified configuration parameters.
%%
%% The server architecture consists of two linked processes: the main server
%% that handles database operations and a commit manager that enforces maximum
%% flush intervals. This dual-process design ensures that data is regularly
%% committed to disk even during periods of continuous write activity.
%%
%% The server process is registered both in the local process dictionary and
%% the global process registry to enable the singleton pattern used throughout
%% the module.
%%
%% @param StoreOpts Database configuration map containing directory and options
%% @returns PID of the newly created server process
start_server(StoreOpts = #{ <<"prefix">> := DataDir }) ->
    % Ensure the database directory exists
    filelib:ensure_dir(
        binary_to_list(hb_util:bin(DataDir)) ++ "/mbd.data"
    ),
    
    % Create the LMDB environment with specified size limit
    {ok, Env} =
        lmdb:env_create(
            DataDir,
            #{
                max_mapsize => maps:get(<<"max-size">>, StoreOpts, ?DEFAULT_SIZE)
            }
        ),
    
    % Prepare server state with environment handle
    ServerOpts = StoreOpts#{ <<"env">> => Env },
    
    % Spawn the main server process with linked commit manager
    Server = 
        spawn(
            fun() ->
                spawn_link(fun() -> commit_manager(ServerOpts, self()) end),
                server(ServerOpts)
            end
        ),
    
    % Register the server in process dictionary for caching
    put({?MODULE, {server, DataDir}}, Server),
    Server.

%% @doc Main server loop that handles database operations and manages transactions.
%%
%% This function implements the core server logic using Erlang's selective receive
%% mechanism. It handles four types of messages: environment requests from readers,
%% write requests that accumulate in transactions, explicit flush requests that
%% commit pending data, and stop messages for graceful shutdown.
%%
%% The server uses a timeout-based flush strategy where it automatically commits
%% transactions after a period of inactivity. This balances write performance
%% (by batching operations) with data safety (by limiting the window of potential
%% data loss).
%%
%% The server maintains its state as a map containing the LMDB environment,
%% current transaction handle, and configuration parameters. State updates are
%% handled functionally by passing modified state maps through tail-recursive calls.
%%
%% @param State Map containing server configuration and runtime state
%% @returns 'ok' when the server terminates, otherwise recurses indefinitely
server(State) ->
    receive
        {get_env, From, Ref} ->
            % Reader requesting environment handle for direct access
            From ! {env, maps:get(<<"env">>, State), Ref},
            server(State);
        {write, Key, Value} ->
            % Write request, accumulate in current transaction
            server(server_write(State, Key, Value));
        {flush, From, Ref} ->
            % Explicit flush request, commit transaction and notify requester
            NewState = server_flush(State),
            From ! {flushed, Ref},
            server(NewState);
        stop ->
            % Shutdown request, flush final data and terminate
            server_flush(State),
            ok
    after
        % Auto-flush after idle timeout to ensure data safety
        maps:get(<<"idle-flush-time">>, State, ?DEFAULT_IDLE_FLUSH_TIME) ->
        server(server_flush(State))
    end.

%% @doc Add a key-value pair to the current transaction, creating one if needed.
%%
%% This function handles write operations by ensuring a transaction is active
%% and then adding the key-value pair to it using LMDB's native interface.
%% If no transaction exists, it creates one automatically.
%%
%% The function uses LMDB's direct NIF interface for maximum performance,
%% bypassing higher-level abstractions that might add overhead. The write
%% is added to the transaction but not committed until a flush occurs.
%%
%% @param RawState Current server state map
%% @param Key Binary key to write
%% @param Value Binary value to store
%% @returns Updated server state with the write added to the transaction
server_write(RawState, Key, Value) ->
    State = ensure_transaction(RawState),
    case {maps:get(<<"transaction">>, State, undefined), 
          maps:get(<<"instance">>, State, undefined)} of
        {undefined, _} ->
            % Transaction creation failed, return state unchanged
            ?event(error, {write_failed_no_transaction, Key}),
            State;
        {_, undefined} ->
            % Database instance missing, return state unchanged
            ?event(error, {write_failed_no_db_instance, Key}),
            State;
        {Txn, Dbi} ->
            % Valid transaction and instance, perform the write
            try
                lmdb_nif:put(Txn, Dbi, Key, Value, 0),
                State
            catch
                Class:Reason:Stacktrace ->
                    ?event(error, {put_failed, Class, Reason, Stacktrace, Key}),
                    % If put fails, the transaction may be invalid, clean it up
                    State#{ <<"transaction">> => undefined, <<"instance">> => undefined }
            end
    end.

%% @doc Commit the current transaction to disk and clean up state.
%%
%% This function handles the critical operation of persisting accumulated writes
%% to the database. If a transaction is active, it commits the transaction and
%% notifies any processes waiting for the flush to complete.
%%
%% After committing, the server state is cleaned up by removing transaction
%% references, preparing for the next batch of operations. If no transaction
%% is active, the function is a no-op.
%%
%% The notification mechanism ensures that read operations blocked on cache
%% misses can proceed once fresh data is available.
%%
%% @param RawState Current server state map  
%% @returns Updated server state with transaction cleared
server_flush(RawState) ->
    case maps:get(<<"transaction">>, RawState, undefined) of
        undefined ->
            % No active transaction, nothing to flush
            RawState;
        Txn ->
            % Commit the transaction with proper error handling
            try
                lmdb_nif:txn_commit(Txn),
                notify_flush(RawState),
                RawState#{ <<"transaction">> => undefined, <<"instance">> => undefined }
            catch
                Class:Reason:Stacktrace ->
                    ?event(error, {txn_commit_failed, Class, Reason, Stacktrace}),
                    % Even if commit fails, clean up the transaction reference
                    % to prevent trying to use an invalid handle
                    notify_flush(RawState),
                    RawState#{ <<"transaction">> => undefined, <<"instance">> => undefined }
            end
    end.

%% @doc Notify all processes waiting for a flush operation to complete.
%%
%% This function handles the coordination between the server's flush operations
%% and client processes that may be blocked waiting for data to be committed.
%% It uses a non-blocking receive loop to collect all pending flush requests
%% and respond to them immediately.
%%
%% The non-blocking nature (timeout of 0) ensures that the server doesn't get
%% stuck waiting for messages that may not exist, while still handling all
%% queued requests efficiently.
%%
%% @param State Current server state (used for context, not modified)
%% @returns 'ok' when all notifications have been sent
notify_flush(State) ->
    receive
        {flush, From, Ref} ->
            From ! {flushed, Ref},
            notify_flush(State)
    after 0 ->
        ok
    end.

%% @doc Background process that enforces maximum flush intervals.
%%
%% This function runs in a separate process linked to the main server and
%% ensures that transactions are committed within a reasonable time frame
%% even during periods of continuous write activity. It sends periodic
%% flush requests to the main server based on the configured maximum flush time.
%%
%% The commit manager provides a safety net against data loss by preventing
%% transactions from remaining uncommitted indefinitely. It works in conjunction
%% with the idle timeout mechanism to provide comprehensive data safety guarantees.
%%
%% The process runs in an infinite loop, coordinating with the main server
%% through message passing and restarting its timer after each successful flush.
%%
%% @param StoreOpts Database configuration containing timing parameters
%% @param Server PID of the main server process to send flush requests to
%% @returns Does not return under normal circumstances (infinite loop)
commit_manager(StoreOpts, Server) ->
    Time = maps:get(<<"max-flush-time">>, StoreOpts, ?DEFAULT_MAX_FLUSH_TIME),
    receive after Time ->
        % Time limit reached, request flush from main server
        Server ! {flush, self(), Ref = make_ref()},
        receive
            {flushed, Ref} ->
                % Flush completed, restart the cycle
                commit_manager(StoreOpts, Server)
        after ?CONNECT_TIMEOUT -> timeout
        end,
        commit_manager(StoreOpts, Server)
    end.

%% @doc Ensure that the server has an active LMDB transaction for writes.
%%
%% This function implements lazy transaction creation by checking if a transaction
%% already exists in the server state. If not, it creates a new read-write
%% transaction and opens the default database within it.
%%
%% The lazy approach improves efficiency by avoiding transaction overhead when
%% the server is idle, while ensuring that write operations always have a
%% transaction available when needed.
%%
%% Transactions in LMDB are lightweight but still represent a commitment of
%% resources, so creating them only when needed helps optimize memory usage
%% and system performance.
%%
%% @param State Current server state map
%% @returns Server state guaranteed to have an active transaction
ensure_transaction(State) ->
    case maps:get(<<"transaction">>, State, undefined) of
        undefined ->
            % No transaction exists, create one with error handling
            try
                {ok, Txn} =
                    lmdb_nif:txn_begin(
                        maps:get(<<"env">>, State),
                        undefined,
                        0
                    ),
                {ok, Dbi} = lmdb:open_db(Txn, default),
                State#{<<"transaction">> => Txn, <<"instance">> => Dbi}
            catch
                Class:Reason:Stacktrace ->
                    ?event(error, {txn_begin_failed, Class, Reason, Stacktrace}),
                    % If transaction creation fails, return state unchanged
                    % This will cause writes to fail but won't crash the server
                    State
            end;
        _ ->
            % Transaction already exists, return state unchanged
            State
    end.

%% @doc Test suite demonstrating basic store operations.
%%
%% The following functions implement unit tests using EUnit to verify that
%% the LMDB store implementation correctly handles various scenarios including
%% basic read/write operations, hierarchical listing, group creation, link
%% resolution, and type detection.

%% @doc Basic store test - verifies fundamental read/write functionality.
%%
%% This test creates a temporary database, writes a key-value pair, reads it
%% back to verify correctness, and cleans up by stopping the database. It
%% serves as a sanity check that the basic storage mechanism is working.
basic_test() ->
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/store-1">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    Res = write(StoreOpts, <<"Hello">>, <<"World2">>),
    ?assertEqual(ok, Res),
    {ok, Value} = read(StoreOpts, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    ok = stop(StoreOpts).

%% @doc List test - verifies prefix-based key listing functionality.
%%
%% This test creates several keys with hierarchical names and verifies that
%% the list operation correctly returns only keys matching a specific prefix.
%% It demonstrates the directory-like navigation capabilities of the store.
list_test() ->
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/store-2">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    write(StoreOpts, <<"colors/red">>, <<"1">>),
    write(StoreOpts, <<"colors/blue">>, <<"2">>),
    write(StoreOpts, <<"colors/green">>, <<"3">>),
    % write(StoreOpts, <<"colors/multi/foo">>, <<"4">>),
    write(StoreOpts, <<"foo/bar">>, <<"baz">>),
    write(StoreOpts, <<"beep/boop">>, <<"bam">>),
    % Brief delay to ensure writes are flushed
    timer:sleep(10), 
    ListResult = list(StoreOpts, <<"colors">>),
    ?event({list_result, ListResult}),
    case ListResult of
        {ok, Keys} ->
            ?assertEqual([<<"blue">>, <<"green">>, <<"red">>], lists:sort(Keys));
        Other ->
            ?event({unexpected_list_result, Other}),
            ?assert(false)
    end,
    ok = stop(StoreOpts).

%% @doc Group test - verifies group creation and type detection.
%%
%% This test creates a group entry and verifies that it is correctly identified 
%% as a composite type and cannot be read directly (like filesystem directories).
group_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store3">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    make_group(StoreOpts, <<"colors">>),
    % Groups should be detected as composite types
    ?assertEqual(composite, type(StoreOpts, <<"colors">>)),
    % Groups should not be readable directly (like directories in filesystem)
    ?assertEqual(not_found, read(StoreOpts, <<"colors">>)).

%% @doc Link test - verifies symbolic link creation and resolution.
%%
%% This test creates a regular key-value pair, creates a link pointing to it,
%% and verifies that reading from the link location returns the original value.
%% This demonstrates the transparent link resolution mechanism.
link_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store3">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    write(StoreOpts, <<"foo/bar/baz">>, <<"Bam">>),
    make_link(StoreOpts, <<"foo/bar/baz">>, <<"foo/beep/baz">>),
    {ok, Result} = read(StoreOpts, <<"foo/beep/baz">>),
    ?event({ result, Result}),
    ?assertEqual(Result, <<"Bam">>).

%% @doc Type test - verifies type detection for both simple and composite entries.
%%
%% This test creates both a group (composite) entry and a regular (simple) entry,
%% then verifies that the type detection function correctly identifies each one.
%% This demonstrates the semantic classification system used by the store.
type_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store-6">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    make_group(StoreOpts, <<"assets">>),
    Type = type(StoreOpts, <<"assets">>),
    ?event({type, Type}),
    ?assertEqual(composite, Type),
    write(StoreOpts, <<"assets/1">>, <<"bam">>),
    Type2 = type(StoreOpts, <<"assets/1">>),
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
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store-7">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    write(StoreOpts, [ <<"parent">>, <<"key">> ], <<"value">>),
    make_link(StoreOpts, [ <<"parent">>, <<"key">> ], <<"my-link">>),
    timer:sleep(100),
    {ok, Result} = read(StoreOpts, <<"my-link">>),
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
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store-8">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    % Create the actual data at group/key
    write(StoreOpts, [<<"group">>, <<"key">>], <<"target-value">>),
    % Create a link from "link" to "group"
    make_link(StoreOpts, <<"group">>, <<"link">>),
    timer:sleep(100),
    % Reading via the link path should resolve to the target value
    {ok, Result} = read(StoreOpts, [<<"link">>, <<"key">>]),
    ?event({path_traversal_result, Result}),
    ?assertEqual(<<"target-value">>, Result),
    ok = stop(StoreOpts).

%% @doc Test that matches the exact hb_store hierarchical test pattern
exact_hb_store_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store-exact">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    
    % Follow exact same pattern as hb_store test
    ?event(debug, step1_make_group),
    make_group(StoreOpts, <<"test-dir1">>),
    
    ?event(debug, step2_write_file),
    write(StoreOpts, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    
    ?event(debug, step3_make_link),
    make_link(StoreOpts, [<<"test-dir1">>], <<"test-link">>),
    
    timer:sleep(1000),

    % Debug: test that the link behaves like the target (groups are unreadable)
    ?event(debug, step4_check_link),
    LinkResult = read(StoreOpts, <<"test-link">>),
    ?event(debug, {link_result, LinkResult}),
    % Since test-dir1 is a group and groups are unreadable, the link should also be unreadable
    ?assertEqual(not_found, LinkResult),
    
    
    % Debug: test intermediate steps
    ?event(debug, step5_test_direct_read),
    DirectResult = read(StoreOpts, <<"test-dir1/test-file">>),
    ?event(debug, {direct_result, DirectResult}),
    
    % This should work: reading via the link path  
    ?event(debug, step6_test_link_read),
    Result = read(StoreOpts, [<<"test-link">>, <<"test-file">>]),
    ?event(debug, {final_result, Result}),
    ?assertEqual({ok, <<"test-data">>}, Result),
    ok = stop(StoreOpts).

%% @doc Sync test - verifies that sync forces immediate flush of writes.
%%
%% This test writes data to the store and immediately calls sync to force
%% a flush, then verifies that the data is immediately readable without
%% waiting for automatic flush timers.
sync_test() ->
    StoreOpts = #{
      <<"prefix">> => <<"/tmp/store-sync">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    
    % Write some data
    write(StoreOpts, <<"sync-key">>, <<"sync-value">>),
    
    % Force immediate flush
    ?assertEqual(ok, sync(StoreOpts)),
    
    % Data should be immediately readable
    {ok, Value} = read(StoreOpts, <<"sync-key">>),
    ?assertEqual(<<"sync-value">>, Value),
    
    ok = stop(StoreOpts).

%% @doc Test cache-style usage through hb_store interface
cache_style_test() ->
    hb:init(),
    StoreOpts = #{
      <<"store-module">> => hb_store_lmdb,
      <<"prefix">> => <<"/tmp/store-cache-style">>,
      <<"max-size">> => ?DEFAULT_SIZE
    },
    
    % Start the store
    hb_store:start(StoreOpts),
    
    % Test writing through hb_store interface  
    ok = hb_store:write(StoreOpts, <<"test-key">>, <<"test-value">>),
    
    % Wait a bit
    timer:sleep(100),
    
    % Test reading through hb_store interface
    Result = hb_store:read(StoreOpts, <<"test-key">>),
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
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/store-nested-cache">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    
    % Clean up any previous test data
    reset(StoreOpts),
    
    % Original nested map structure
    OriginalMap = #{
        <<"name">> => <<"Foo">>,
        <<"items">> => #{
            <<"bar">> => #{
              <<"beep">> => <<"baz">>
            },
            <<"count">> => <<"42">>,
            <<"active">> => <<"true">>
        }
    },
    
    ?event({original_map, OriginalMap}),
    
    % Step 1: Store each leaf value at data/{hash}
    NameHash = crypto:hash(sha256, <<"Foo">>),
    NameDataPath = <<"data/", (base64:encode(NameHash))/binary>>,
    write(StoreOpts, NameDataPath, <<"Foo">>),
    
    BeepHash = crypto:hash(sha256, <<"baz">>),
    BeepDataPath = <<"data/", (base64:encode(BeepHash))/binary>>,
    write(StoreOpts, BeepDataPath, <<"baz">>),
    
    CountHash = crypto:hash(sha256, <<"42">>),
    CountDataPath = <<"data/", (base64:encode(CountHash))/binary>>,
    write(StoreOpts, CountDataPath, <<"42">>),
    
    ActiveHash = crypto:hash(sha256, <<"true">>),
    ActiveDataPath = <<"data/", (base64:encode(ActiveHash))/binary>>,
    write(StoreOpts, ActiveDataPath, <<"true">>),
    
    % Step 2: Create the nested structure with groups and links
    
    % Create the root group
    make_group(StoreOpts, <<"root">>),
    
    % Create links for the root level keys
    make_link(StoreOpts, NameDataPath, <<"root/name">>),
    
    % Create the items subgroup
    make_group(StoreOpts, <<"root/items">>),
    
    % Create the bar subgroup within items
    make_group(StoreOpts, <<"root/items/bar">>),
    
    % Create links for the items subkeys
    make_link(StoreOpts, BeepDataPath, <<"root/items/bar/beep">>),
    make_link(StoreOpts, CountDataPath, <<"root/items/count">>),
    make_link(StoreOpts, ActiveDataPath, <<"root/items/active">>),
    
    % Force writes to be committed
    sync(StoreOpts),
    
    % Step 3: Test reading the structure back
    
    % Verify the root is a composite
    ?assertEqual(composite, type(StoreOpts, <<"root">>)),
    
    % List the root contents
    {ok, RootKeys} = list(StoreOpts, <<"root">>),
    ?event({root_keys, RootKeys}),
    
    % Read the name directly
    {ok, NameValue} = read(StoreOpts, <<"root/name">>),
    ?assertEqual(<<"Foo">>, NameValue),
    
    % Verify items is a composite
    ?assertEqual(composite, type(StoreOpts, <<"root/items">>)),
    
    % List the items contents  
    {ok, ItemsKeys} = list(StoreOpts, <<"root/items">>),
    ?event({items_keys, ItemsKeys}),
    
    % Verify bar is a composite (nested group)
    ?assertEqual(composite, type(StoreOpts, <<"root/items/bar">>)),
    
    % List the bar contents
    {ok, BarKeys} = list(StoreOpts, <<"root/items/bar">>),
    ?event({bar_keys, BarKeys}),
    
    % Read the nested value
    {ok, BeepValue} = read(StoreOpts, <<"root/items/bar/beep">>),
    ?assertEqual(<<"baz">>, BeepValue),
    
    % Read other item values
    {ok, CountValue} = read(StoreOpts, <<"root/items/count">>),
    ?assertEqual(<<"42">>, CountValue),
    
    {ok, ActiveValue} = read(StoreOpts, <<"root/items/active">>),
    ?assertEqual(<<"true">>, ActiveValue),
    
    % Step 4: Test programmatic reconstruction of the nested map
    ReconstructedMap = reconstruct_map(StoreOpts, <<"root">>),
    ?event({reconstructed_map, ReconstructedMap}),
    
    % Verify the reconstructed map matches the original structure
    ?assertEqual(<<"Foo">>, maps:get(<<"name">>, ReconstructedMap)),
    ItemsMap = maps:get(<<"items">>, ReconstructedMap),
    BarMap = maps:get(<<"bar">>, ItemsMap),
    ?assertEqual(<<"baz">>, maps:get(<<"beep">>, BarMap)),
    ?assertEqual(<<"42">>, maps:get(<<"count">>, ItemsMap)),
    ?assertEqual(<<"true">>, maps:get(<<"active">>, ItemsMap)),
    ?event({originalMap, OriginalMap}),
    ?assert(hb_message:match(OriginalMap, ReconstructedMap)),
    stop(StoreOpts).

%% Helper function to recursively reconstruct a map from the store
reconstruct_map(StoreOpts, Path) ->
    case type(StoreOpts, Path) of
        composite ->
            % This is a group, reconstruct it as a map
            {ok, ImmediateChildren} = list(StoreOpts, Path),
            % The list function now correctly returns only immediate children
            ?event({path, Path, immediate_children, ImmediateChildren}),
            maps:from_list([
                {Key, reconstruct_map(StoreOpts, <<Path/binary, "/", Key/binary>>)}
                || Key <- ImmediateChildren
            ]);
        simple ->
            % This is a simple value, read it directly
            {ok, Value} = read(StoreOpts, Path),
            Value;
        not_found ->
            % Path doesn't exist
            undefined
    end.

%% @doc Debug test to understand cache linking behavior
cache_debug_test() ->
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/cache-debug">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    
    reset(StoreOpts),
    
    % Simulate what the cache does:
    % 1. Create a group for message ID
    MessageID = <<"test_message_123">>,
    make_group(StoreOpts, MessageID),
    
    % 2. Store a value at data/hash
    Value = <<"test_value">>,
    ValueHash = base64:encode(crypto:hash(sha256, Value)),
    DataPath = <<"data/", ValueHash/binary>>,
    write(StoreOpts, DataPath, Value),
    
    % 3. Calculate a key hashpath (simplified version)
    KeyHashPath = <<MessageID/binary, "/", "key_hash_abc">>,
    
    % 4. Create link from data path to key hash path
    make_link(StoreOpts, DataPath, KeyHashPath),
    
    sync(StoreOpts),
    
    % 5. Test what the cache would see:
    ?event(debug_cache_test, {step, check_message_type}),
    MsgType = type(StoreOpts, MessageID),
    ?event(debug_cache_test, {message_type, MsgType}),
    
    ?event(debug_cache_test, {step, list_message_contents}),
    {ok, Subkeys} = list(StoreOpts, MessageID),
    ?event(debug_cache_test, {message_subkeys, Subkeys}),
    
    ?event(debug_cache_test, {step, read_key_hashpath}),
    KeyHashResult = read(StoreOpts, KeyHashPath),
    ?event(debug_cache_test, {key_hash_read_result, KeyHashResult}),
    
    % 6. Test with path as list (what cache does):
    ?event(debug_cache_test, {step, read_path_as_list}),
    PathAsList = [MessageID, <<"key_hash_abc">>],
    PathAsListResult = read(StoreOpts, PathAsList),
    ?event(debug_cache_test, {path_as_list_result, PathAsListResult}),
    
    stop(StoreOpts).

%% @doc Isolated test focusing on the exact cache issue
isolated_type_debug_test() ->
    StoreOpts = #{
        <<"prefix">> => <<"/tmp/isolated-debug">>,
        <<"max-size">> => ?DEFAULT_SIZE
    },
    
    reset(StoreOpts),
    
    % Create the exact scenario from user's description:
    % 1. A message ID with nested structure
    MessageID = <<"message123">>,
    make_group(StoreOpts, MessageID),
    
    % 2. Create nested groups for "commitments" and "other-test-key"
    CommitmentsPath = <<MessageID/binary, "/commitments">>,
    OtherKeyPath = <<MessageID/binary, "/other-test-key">>,
    
    ?event(isolated_debug, {creating_nested_groups, CommitmentsPath, OtherKeyPath}),
    make_group(StoreOpts, CommitmentsPath),
    make_group(StoreOpts, OtherKeyPath),
    
    % 3. Add some actual data within those groups
    write(StoreOpts, <<CommitmentsPath/binary, "/sig1">>, <<"signature_data_1">>),
    write(StoreOpts, <<OtherKeyPath/binary, "/sub_value">>, <<"nested_value">>),
    
    sync(StoreOpts),
    
    % 4. Test type detection on the nested paths
    ?event(isolated_debug, {testing_main_message_type}),
    MainType = type(StoreOpts, MessageID),
    ?event(isolated_debug, {main_message_type, MainType}),
    
    ?event(isolated_debug, {testing_commitments_type}),
    CommitmentsType = type(StoreOpts, CommitmentsPath),
    ?event(isolated_debug, {commitments_type, CommitmentsType}),
    
    ?event(isolated_debug, {testing_other_key_type}),
    OtherKeyType = type(StoreOpts, OtherKeyPath),
    ?event(isolated_debug, {other_key_type, OtherKeyType}),
    
    % 5. Test what happens when reading these nested paths
    ?event(isolated_debug, {reading_commitments_directly}),
    CommitmentsResult = read(StoreOpts, CommitmentsPath),
    ?event(isolated_debug, {commitments_read_result, CommitmentsResult}),
    
    ?event(isolated_debug, {reading_other_key_directly}),
    OtherKeyResult = read(StoreOpts, OtherKeyPath),
    ?event(isolated_debug, {other_key_read_result, OtherKeyResult}),
    
    stop(StoreOpts). 