# hb_store_lmdb

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_lmdb.erl)

## Exported Functions

- `add_path/3`
- `list/2`
- `make_group/2`
- `make_link/3`
- `match/2`
- `path/2`
- `read/2`
- `reset/1`
- `resolve/2`
- `scope/0`
- `scope/1`
- `start/1`
- `stop/1`
- `type/2`
- `write/3`

---

### start

An LMDB (Lightning Memory Database) implementation of the HyperBeam store interface.
Start the LMDB storage system for a given database configuration.

```erlang
start(Opts = #{ <<"name">> := DataDir }) ->
    % Ensure the directory exists before opening LMDB environment
    DataDirPath = hb_util:list(DataDir),
    ok = filelib:ensure_dir(filename:join(DataDirPath, "dummy")),
    % Create the LMDB environment with specified size limit
    {ok, Env} =
        elmdb:env_open(
            DataDirPath,
            [
                {map_size, maps:get(<<"capacity">>, Opts, ?DEFAULT_SIZE)},
                no_mem_init, no_sync
            ]
        ),
    {ok, DBInstance} = elmdb:db_open(Env, [create]),
    % Store both environment and DB instance in persistent_term for later cleanup
    StoreKey = {lmdb, ?MODULE, DataDir},
    persistent_term:put(StoreKey, {Env, DBInstance, DataDir}),
    {ok, #{ <<"env">> => Env, <<"db">> => DBInstance }};
```

### start

An LMDB (Lightning Memory Database) implementation of the HyperBeam store interface.
Start the LMDB storage system for a given database configuration.
Determine whether a key represents a simple value or composite group.
Write a key-value pair to the database asynchronously.
Read a value from the database by key, with automatic link resolution.
Helper function to check if a value is a link and extract the target.

```erlang
-spec read(map(), binary() | list()) -> {ok, binary()} | {error, term()}.
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
```

```erlang
start(_) ->
    {error, {badarg, <<"StoreOpts must be a map">>}}.
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
%%
```

### is_link

An LMDB (Lightning Memory Database) implementation of the HyperBeam store interface.
Start the LMDB storage system for a given database configuration.
Determine whether a key represents a simple value or composite group.
Write a key-value pair to the database asynchronously.
Read a value from the database by key, with automatic link resolution.
Helper function to check if a value is a link and extract the target.

```erlang
-spec read(map(), binary() | list()) -> {ok, binary()} | {error, term()}.
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
```

```erlang
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
```

### to_path

Helper function to convert to a path
Unified read function that handles LMDB reads with fallback to the 

```erlang
to_path(PathParts) ->
    hb_util:bin(lists:join(<<"/">>, PathParts)).
```

### read_direct

Helper function to convert to a path
Unified read function that handles LMDB reads with fallback to the 

```erlang
read_direct(Opts, Path) ->
    #{ <<"db">> := DBInstance } = find_env(Opts),
    case elmdb:get(DBInstance, Path) of
        {ok, Value} -> {ok, Value};
        {error, not_found} -> not_found;  % Normalize error format
        not_found -> not_found  % Handle both old and new format
    end.
```

### read_with_links

Read a value directly from the database with link resolution.

```erlang
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
```

### resolve_path_links

Resolve links in a path, checking each segment except the last.

```erlang
resolve_path_links(Opts, Path) ->
    resolve_path_links(Opts, Path, 0).
```

### resolve_path_links

```erlang
resolve_path_links(_Opts, _Path, Depth) when Depth > ?MAX_REDIRECTS ->
    % Prevent infinite loops with depth limit
    {error, too_many_redirects};
```

### resolve_path_links

```erlang
resolve_path_links(_Opts, [LastSegment], _Depth) ->
    % Base case: only one segment left, no link resolution needed
    {ok, [LastSegment]};
```

### resolve_path_links

```erlang
resolve_path_links(Opts, Path, Depth) ->
    resolve_path_links_acc(Opts, Path, [], Depth).
```

### resolve_path_links_acc

```erlang
resolve_path_links_acc(_Opts, [], AccPath, _Depth) ->
    % No more segments to process
    {ok, lists:reverse(AccPath)};
```

### resolve_path_links_acc

```erlang
resolve_path_links_acc(_, FullPath = [<<"data">>|_], [], _Depth) ->
    {ok, FullPath};
```

### resolve_path_links_acc

```erlang
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
```

### match

Match a series of keys and values against the database. Returns 

```erlang
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
    % Ensure path ends with / for elmdb:list API
    SearchPath = 
        case ResolvedPath of
            <<>> -> <<>>;      % Root path
            <<"/">> -> <<>>;   % Root path variant
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
        {error, not_found} -> {ok, []};  % Normalize new error format
        not_found -> {ok, []}  % Handle both old and new format
    end.
```

```erlang
match(Opts, MatchMap) when is_map(MatchMap) ->
    match(Opts, maps:to_list(MatchMap));
```

### match

Match a series of keys and values against the database. Returns 

```erlang
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
    % Ensure path ends with / for elmdb:list API
    SearchPath = 
        case ResolvedPath of
            <<>> -> <<>>;      % Root path
            <<"/">> -> <<>>;   % Root path variant
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
        {error, not_found} -> {ok, []};  % Normalize new error format
        not_found -> {ok, []}  % Handle both old and new format
    end.
```

```erlang
match(Opts, MatchKVs) ->
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
        {error, not_found} -> not_found;
        not_found -> not_found
    end.
```

### create_parent_groups

Helper function to recursively create parent groups.

```erlang
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
```

```erlang
create_parent_groups(_Opts, _Current, []) ->
    ok;
```

### create_parent_groups

Helper function to recursively create parent groups.

```erlang
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
```

```erlang
create_parent_groups(Opts, Current, [Next | Rest]) ->
    NewCurrent = Current ++ [Next],
    GroupPath = to_path(NewCurrent),
    % Only create group if it doesn't already exist.
```

### path

Transform a path into the store's canonical form.

```erlang
-spec make_link(map(), binary() | list(), binary()) -> ok.
make_link(Opts, Existing, New) when is_list(Existing) ->
    ExistingBin = to_path(Existing),
    make_link(Opts, ExistingBin, New);
make_link(Opts, Existing, New) ->
   ExistingBin = hb_util:bin(Existing),
   % Ensure parent groups exist for the new link path (like filesystem ensure_dir)
   ensure_parent_groups(Opts, New),
   write(Opts, New, <<"link:", ExistingBin/binary>>). 
```

```erlang
path(_Opts, PathParts) when is_list(PathParts) ->
    to_path(PathParts);
```

### path

Transform a path into the store's canonical form.

```erlang
-spec make_link(map(), binary() | list(), binary()) -> ok.
make_link(Opts, Existing, New) when is_list(Existing) ->
    ExistingBin = to_path(Existing),
    make_link(Opts, ExistingBin, New);
make_link(Opts, Existing, New) ->
   ExistingBin = hb_util:bin(Existing),
   % Ensure parent groups exist for the new link path (like filesystem ensure_dir)
   ensure_parent_groups(Opts, New),
   write(Opts, New, <<"link:", ExistingBin/binary>>). 
```

```erlang
path(_Opts, Path) when is_binary(Path) ->
    Path.
```

### add_path

Add two path components together.

```erlang
add_path(_Opts, Path1, Path2) when is_list(Path1), is_list(Path2) ->
    Path1 ++ Path2;
```

### add_path

Add two path components together.

```erlang
add_path(Opts, Path1, Path2) when is_binary(Path1), is_binary(Path2) ->
    % Convert binaries to lists, concatenate, then convert back
    Parts1 = binary:split(Path1, <<"/">>, [global]),
    Parts2 = binary:split(Path2, <<"/">>, [global]),
    path(Opts, Parts1 ++ Parts2);
```

### add_path

Add two path components together.

```erlang
add_path(Opts, Path1, Path2) when is_list(Path1), is_binary(Path2) ->
    Parts2 = binary:split(Path2, <<"/">>, [global]),
    path(Opts, Path1 ++ Parts2);
```

### add_path

Add two path components together.

```erlang
add_path(Opts, Path1, Path2) when is_binary(Path1), is_list(Path2) ->
    Parts1 = binary:split(Path1, <<"/">>, [global]),
    path(Opts, Parts1 ++ Path2).
```

### find_env

Retrieve or create the LMDB environment handle for a database.

```erlang
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
```

```erlang
find_env(Opts) -> hb_store:find(Opts).
%% Shutdown LMDB environment and cleanup resources
```

### stop

Retrieve or create the LMDB environment handle for a database.

```erlang
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
```

```erlang
stop(#{ <<"store-module">> := ?MODULE, <<"name">> := DataDir }) ->
    StoreKey = {lmdb, ?MODULE, DataDir},
    close_environment(StoreKey, DataDir);
```

### stop

Retrieve or create the LMDB environment handle for a database.

```erlang
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
```

```erlang
stop(_InvalidStoreOpts) ->
    ok.
```

### close_environment

```erlang
close_environment(StoreKey, DataDir) ->
    case safe_get_persistent_term(StoreKey) of
        {ok, {Env, DBInstance}} ->
            close_and_cleanup(Env, DBInstance, StoreKey, DataDir);
        not_found ->
            ?event({lmdb_stop_not_found_in_persistent_term, DataDir}),
            safe_close_by_name(DataDir)
    end,
    ok.
```

### safe_get_persistent_term

```erlang
safe_get_persistent_term(Key) ->
    case persistent_term:get(Key, undefined) of
        {Env, DBInstance, _DataDir} -> {ok, {Env, DBInstance}};
        {Env, _DataDir} -> {ok, {Env, undefined}};  % Backwards compatibility
        _ -> not_found
    end.
```

### close_and_cleanup

```erlang
close_and_cleanup(Env, DBInstance, StoreKey, DataDir) ->
    % Close DB instance first if it exists
    DBCloseResult = safe_close_db(DBInstance),
    ?event({db_close_result, DBCloseResult}),
    % Then close the environment
    EnvCloseResult = safe_close_env(Env),
    persistent_term:erase(StoreKey),
    case EnvCloseResult of
        ok -> ?event({lmdb_stop_success, DataDir});
        {error, Reason} -> ?event({lmdb_stop_error, Reason})
    end.
```

### safe_close_db

```erlang
safe_close_db(undefined) ->
    ok;  % No DB instance to close
```

### safe_close_db

```erlang
safe_close_db(DBInstance) ->
    try
        elmdb:db_close(DBInstance)
    catch
        error:Reason -> {error, Reason}
    end.
```

### safe_close_env

```erlang
safe_close_env(Env) ->
    try
        elmdb:env_close(Env)
    catch
        error:Reason -> {error, Reason}
    end.
```

### safe_close_by_name

```erlang
safe_close_by_name(DataDir) ->
    try
        elmdb:env_close_by_name(binary_to_list(DataDir))
    catch
        error:_ -> ok
    end.
```

### reset

Completely delete the database directory and all its contents.

```erlang
reset(Opts) ->
    case maps:get(<<"name">>, Opts, undefined) of
        undefined ->
            % No prefix specified, nothing to reset
            ok;
        DataDir ->
            % Stop the store and remove the database.
```

### basic_test

Test suite demonstrating basic store operations.
Basic store test - verifies fundamental read/write functionality.

```erlang
basic_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-1">>
    },
    reset(StoreOpts),
    Res = write(StoreOpts, <<"Hello">>, <<"World2">>),
    ?assertEqual(ok, Res),
    {ok, Value} = read(StoreOpts, <<"Hello">>),
    ?assertEqual(Value, <<"World2">>),
    ok = stop(StoreOpts).
```

### list_test

List test - verifies prefix-based key listing functionality.

```erlang
list_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-2">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    ?assertEqual(list(StoreOpts, <<"colors">>), {ok, []}),
    % Create immediate children under colors/
    write(StoreOpts, <<"colors/red">>, <<"1">>),
    write(StoreOpts, <<"colors/blue">>, <<"2">>),
    write(StoreOpts, <<"colors/green">>, <<"3">>),
    % Create nested directories under colors/ - these should show up as immediate children
    write(StoreOpts, <<"colors/multi/foo">>, <<"4">>),
    write(StoreOpts, <<"colors/multi/bar">>, <<"5">>),
    write(StoreOpts, <<"colors/primary/red">>, <<"6">>),
    write(StoreOpts, <<"colors/primary/blue">>, <<"7">>),
    write(StoreOpts, <<"colors/nested/deep/value">>, <<"8">>),
    % Create other top-level directories
    write(StoreOpts, <<"foo/bar">>, <<"baz">>),
    write(StoreOpts, <<"beep/boop">>, <<"bam">>),
    read(StoreOpts, <<"colors">>), 
    % Test listing colors/ - should return immediate children only
    {ok, ListResult} = list(StoreOpts, <<"colors">>),
    ?event({list_result, ListResult}),
    % Expected: red, blue, green (files) + multi, primary, nested (directories)
    % Should NOT include deeply nested items like foo, bar, deep, value
    ExpectedChildren = [<<"blue">>, <<"green">>, <<"multi">>, <<"nested">>, <<"primary">>, <<"red">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedChildren) end, ListResult)),
    % Test listing a nested directory - should only show immediate children
    {ok, NestedListResult} = list(StoreOpts, <<"colors/multi">>),
    ?event({nested_list_result, NestedListResult}),
    ExpectedNestedChildren = [<<"bar">>, <<"foo">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedNestedChildren) end, NestedListResult)),
    % Test listing a deeper nested directory
    {ok, DeepListResult} = list(StoreOpts, <<"colors/nested">>),
    ?event({deep_list_result, DeepListResult}),
    ExpectedDeepChildren = [<<"deep">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedDeepChildren) end, DeepListResult)),
    ok = stop(StoreOpts).
```

### group_test

Group test - verifies group creation and type detection.
Link test - verifies symbolic link creation and resolution.

```erlang
group_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store3">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    make_group(StoreOpts, <<"colors">>),
    % Groups should be detected as composite types
    ?assertEqual(composite, type(StoreOpts, <<"colors">>)),
    % Groups should not be readable directly (like directories in filesystem)
    ?assertEqual(not_found, read(StoreOpts, <<"colors">>)).
%%
```

### link_test

Group test - verifies group creation and type detection.
Link test - verifies symbolic link creation and resolution.

```erlang
link_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store3">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    write(StoreOpts, <<"foo/bar/baz">>, <<"Bam">>),
    make_link(StoreOpts, <<"foo/bar/baz">>, <<"foo/beep/baz">>),
    {ok, Result} = read(StoreOpts, <<"foo/beep/baz">>),
    ?event({ result, Result}),
    ?assertEqual(<<"Bam">>, Result).
```

### link_fragment_test

Group test - verifies group creation and type detection.
Link test - verifies symbolic link creation and resolution.
Type test - verifies type detection for both simple and composite entries.

```erlang
link_fragment_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store3">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    write(StoreOpts, [<<"data">>, <<"bar">>, <<"baz">>], <<"Bam">>),
    make_link(StoreOpts, [<<"data">>, <<"bar">>], <<"my-link">>),
    {ok, Result} = read(StoreOpts, [<<"my-link">>, <<"baz">>]),
    ?event({ result, Result}),
    ?assertEqual(<<"Bam">>, Result).
%%
```

### type_test

Group test - verifies group creation and type detection.
Link test - verifies symbolic link creation and resolution.
Type test - verifies type detection for both simple and composite entries.

```erlang
type_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-6">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    make_group(StoreOpts, <<"assets">>),
    Type = type(StoreOpts, <<"assets">>),
    ?event({type, Type}),
    ?assertEqual(composite, Type),
    write(StoreOpts, <<"assets/1">>, <<"bam">>),
    Type2 = type(StoreOpts, <<"assets/1">>),
    ?event({type2, Type2}),
    ?assertEqual(simple, Type2).
```

### link_key_list_test

Link key list test - verifies symbolic link creation using structured key paths.
Path traversal link test - verifies link resolution during path traversal.

```erlang
link_key_list_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-7">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    write(StoreOpts, [ <<"parent">>, <<"key">> ], <<"value">>),
    make_link(StoreOpts, [ <<"parent">>, <<"key">> ], <<"my-link">>),
    {ok, Result} = read(StoreOpts, <<"my-link">>),
    ?event({result, Result}),
    ?assertEqual(<<"value">>, Result).
%%
%%
```

### path_traversal_link_test

Link key list test - verifies symbolic link creation using structured key paths.
Path traversal link test - verifies link resolution during path traversal.

```erlang
path_traversal_link_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-8">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    % Create the actual data at group/key
    write(StoreOpts, [<<"group">>, <<"key">>], <<"target-value">>),
    % Create a link from "link" to "group"
    make_link(StoreOpts, <<"group">>, <<"link">>),
    % Reading via the link path should resolve to the target value
    {ok, Result} = read(StoreOpts, [<<"link">>, <<"key">>]),
    ?event({path_traversal_result, Result}),
    ?assertEqual(<<"target-value">>, Result),
    ok = stop(StoreOpts).
```

### exact_hb_store_test

Test that matches the exact hb_store hierarchical test pattern

```erlang
exact_hb_store_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-exact">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    % Follow exact same pattern as hb_store test
    ?event(step1_make_group),
    make_group(StoreOpts, <<"test-dir1">>),
    ?event(step2_write_file),
    write(StoreOpts, [<<"test-dir1">>, <<"test-file">>], <<"test-data">>),
    ?event(step3_make_link),
    make_link(StoreOpts, [<<"test-dir1">>], <<"test-link">>),
    % Debug: test that the link behaves like the target (groups are unreadable)
    ?event(step4_check_link),
    LinkResult = read(StoreOpts, <<"test-link">>),
    ?event({link_result, LinkResult}),
    % Since test-dir1 is a group and groups are unreadable, the link should also be unreadable
    ?assertEqual(not_found, LinkResult),
    % Debug: test intermediate steps
    ?event(step5_test_direct_read),
    DirectResult = read(StoreOpts, <<"test-dir1/test-file">>),
    ?event({direct_result, DirectResult}),
    % This should work: reading via the link path  
    ?event(step6_test_link_read),
    Result = read(StoreOpts, [<<"test-link">>, <<"test-file">>]),
    ?event({final_result, Result}),
    ?assertEqual({ok, <<"test-data">>}, Result),
    ok = stop(StoreOpts).
```

### cache_style_test

Test cache-style usage through hb_store interface

```erlang
cache_style_test() ->
    hb:init(),
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-cache-style">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    % Start the store
    hb_store:start(StoreOpts),
    % Test writing through hb_store interface  
    ok = hb_store:write(StoreOpts, <<"test-key">>, <<"test-value">>),
    % Test reading through hb_store interface
    Result = hb_store:read(StoreOpts, <<"test-key">>),
    ?event({cache_style_read_result, Result}),
    ?assertEqual({ok, <<"test-value">>}, Result),
    hb_store:stop(StoreOpts).
```

### nested_map_cache_test

Test nested map storage with cache-like linking behavior

```erlang
nested_map_cache_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-nested-cache">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    % Clean up any previous test data
    reset(StoreOpts),
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
    write(StoreOpts, <<"data/", TargetHash/binary>>, TargetValue),
    AlgValue1 = <<"rsa-pss-512">>,
    AlgHash1 = base64:encode(crypto:hash(sha256, AlgValue1)),
    write(StoreOpts, <<"data/", AlgHash1/binary>>, AlgValue1),
    CommitterValue1 = <<"unique-id">>,
    CommitterHash1 = base64:encode(crypto:hash(sha256, CommitterValue1)),
    write(StoreOpts, <<"data/", CommitterHash1/binary>>, CommitterValue1),
    AlgValue2 = <<"hmac">>,
    AlgHash2 = base64:encode(crypto:hash(sha256, AlgValue2)),
    write(StoreOpts, <<"data/", AlgHash2/binary>>, AlgValue2),
    CommitterValue2 = <<"unique-id-2">>,
    CommitterHash2 = base64:encode(crypto:hash(sha256, CommitterValue2)),
    write(StoreOpts, <<"data/", CommitterHash2/binary>>, CommitterValue2),
    OtherKeyValue = <<"other-key-value">>,
    OtherKeyHash = base64:encode(crypto:hash(sha256, OtherKeyValue)),
    write(StoreOpts, <<"data/", OtherKeyHash/binary>>, OtherKeyValue),
    % Step 2: Create the nested structure with groups and links
    % Create the root group
    make_group(StoreOpts, <<"root">>),
    % Create links for the root level keys
    make_link(StoreOpts, <<"data/", TargetHash/binary>>, <<"root/target">>),
    % Create the commitments subgroup
    make_group(StoreOpts, <<"root/commitments">>),
    % Create the key1 subgroup within commitments
    make_group(StoreOpts, <<"root/commitments/key1">>),
    make_link(StoreOpts, <<"data/", AlgHash1/binary>>, <<"root/commitments/key1/alg">>),
    make_link(StoreOpts, <<"data/", CommitterHash1/binary>>, <<"root/commitments/key1/committer">>),
    % Create the key2 subgroup within commitments
    make_group(StoreOpts, <<"root/commitments/key2">>),
    make_link(StoreOpts, <<"data/", AlgHash2/binary>>, <<"root/commitments/key2/alg">>),
    make_link(StoreOpts, <<"data/", CommitterHash2/binary>>, <<"root/commitments/key2/commiter">>),
    % Create the other-key subgroup
    make_group(StoreOpts, <<"root/other-key">>),
    make_link(StoreOpts, <<"data/", OtherKeyHash/binary>>, <<"root/other-key/other-key-key">>),
    % Step 3: Test reading the structure back
    % Verify the root is a composite
    ?assertEqual(composite, type(StoreOpts, <<"root">>)),
    % List the root contents
    {ok, RootKeys} = list(StoreOpts, <<"root">>),
    ?event({root_keys, RootKeys}),
    ExpectedRootKeys = [<<"commitments">>, <<"other-key">>, <<"target">>],
    ?assert(lists:all(fun(Key) -> lists:member(Key, ExpectedRootKeys) end, RootKeys)),
    % Read the target directly
    {ok, TargetValueRead} = read(StoreOpts, <<"root/target">>),
    ?assertEqual(<<"Foo">>, TargetValueRead),
    % Verify commitments is a composite
    ?assertEqual(composite, type(StoreOpts, <<"root/commitments">>)),
    % Verify other-key is a composite  
    ?assertEqual(composite, type(StoreOpts, <<"root/other-key">>)),
    % Step 4: Test programmatic reconstruction of the nested map
    ReconstructedMap = reconstruct_map(StoreOpts, <<"root">>),
    ?event({reconstructed_map, ReconstructedMap}),
    % Verify the reconstructed map matches the original structure
    ?assert(hb_message:match(OriginalMap, ReconstructedMap)),
    stop(StoreOpts).
```

### reconstruct_map

```erlang
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
```

### cache_debug_test

Debug test to understand cache linking behavior

```erlang
cache_debug_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/cache-debug">>,
        <<"capacity">> => ?DEFAULT_SIZE
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
```

### isolated_type_debug_test

Isolated test focusing on the exact cache issue

```erlang
isolated_type_debug_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/isolated-debug">>,
        <<"capacity">> => ?DEFAULT_SIZE
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
```

### list_with_link_test

Test that list function resolves links correctly

```erlang
list_with_link_test() ->
    StoreOpts = #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"/tmp/store-list-link">>,
        <<"capacity">> => ?DEFAULT_SIZE
    },
    reset(StoreOpts),
    % Create a group with some children
    make_group(StoreOpts, <<"real-group">>),
    write(StoreOpts, <<"real-group/child1">>, <<"value1">>),
    write(StoreOpts, <<"real-group/child2">>, <<"value2">>),
    write(StoreOpts, <<"real-group/child3">>, <<"value3">>),
    % Create a link to the group
    make_link(StoreOpts, <<"real-group">>, <<"link-to-group">>),
    % List the real group to verify expected children
    {ok, RealGroupChildren} = list(StoreOpts, <<"real-group">>),
    ?event({real_group_children, RealGroupChildren}),
    ExpectedChildren = [<<"child1">>, <<"child2">>, <<"child3">>],
    ?assertEqual(ExpectedChildren, lists:sort(RealGroupChildren)),
    % List via the link - should return the same children
    {ok, LinkChildren} = list(StoreOpts, <<"link-to-group">>),
    ?event({link_children, LinkChildren}),
    ?assertEqual(ExpectedChildren, lists:sort(LinkChildren)),
```

---

*Generated from [hb_store_lmdb.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_lmdb.erl)*
