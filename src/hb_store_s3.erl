%%%-----------------------------------------------------------------------------
%%% @doc S3-backed implementation of the HyperBEAM store behavior.
%%% This module provides persistent storage using Amazon S3 or compatible
%%% object storage services (MinIO, Wasabi, etc.).
%%% @end
%%%-----------------------------------------------------------------------------
-module(hb_store_s3).
-behaviour(hb_store).

%% Store behavior callbacks
-export([start/1, stop/1, reset/1, scope/0, scope/1]).
-export([read/2, write/3, list/2, type/2]).
-export([make_group/2, make_link/3, resolve/2]).
-export([path/2, add_path/3]).

%% Helper functions
-export([match/2]).

-include("include/hb.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

%% Type definitions
-type opts() :: map().
-type key() :: binary() | list().
-type value() :: binary().

%% Configuration defaults
-define(DEFAULT_REGION, <<"us-east-1">>).
-define(DEFAULT_ENDPOINT, <<"https://s3.amazonaws.com">>).
-define(MAX_LINK_DEPTH, 100).
-define(LINK_MARKER, <<"link:">>).

%%%-----------------------------------------------------------------------------
%%% Configuration and Initialization (Phase 2)
%%%-----------------------------------------------------------------------------

%% @doc Initialize the S3 store connection.
%% This function is called when the store is first accessed.
%% It validates the configuration and tests the connection.
-spec start(opts()) -> ok | {error, term()}.
start(Opts) ->
    try
        % Step 1: Validate required configuration keys
        ok = validate_config(Opts),

        % Step 2: Create erlcloud configuration
        Config = make_erlcloud_config(Opts),

        % Step 3: Test bucket access
        Bucket = maps:get(<<"bucket">>, Opts),
        ok = test_bucket_access(Bucket, Config),

        % Step 4: Store configuration for later use
        StoreRef = get_store_ref(Opts),
        persistent_term:put(StoreRef, #{
            bucket => Bucket,
            prefix => maps:get(<<"prefix">>, Opts, <<>>),
            config => Config
        }),

        ?event(store_s3, {started, {bucket, Bucket}}),
        ok
    catch
        error:Reason ->
            ?event(error, {s3_start_failed, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Validate that all required configuration keys are present.
%% Required keys: bucket, access-key-id, secret-access-key
validate_config(Opts) ->
    Required = [<<"bucket">>, <<"access-key-id">>, <<"secret-access-key">>],
    Missing = [K || K <- Required, not maps:is_key(K, Opts)],
    case Missing of
        [] ->
            ok;
        _ ->
            error({missing_config_keys, Missing})
    end.

%% @doc Build erlcloud AWS configuration from our options.
make_erlcloud_config(Opts) ->
    % Get configuration values with defaults
    AccessKey = binary_to_list(maps:get(<<"access-key-id">>, Opts)),
    SecretKey = binary_to_list(maps:get(<<"secret-access-key">>, Opts)),
    Region = binary_to_list(maps:get(<<"region">>, Opts, ?DEFAULT_REGION)),
    Endpoint = maps:get(<<"endpoint">>, Opts, ?DEFAULT_ENDPOINT),

    % Parse endpoint URL
    {Scheme, Host, Port} = parse_endpoint(Endpoint),

    % Create base configuration
    BaseConfig = erlcloud_s3:new(AccessKey, SecretKey, Host, Port),

    % Add additional settings
    BaseConfig#aws_config{
        s3_scheme = Scheme,
        s3_bucket_after_host = false,
        s3_bucket_access_method = path,
        aws_region = Region,
        http_client = httpc
    }.

%% @doc Parse an endpoint URL into scheme, host, and port.
%% Example: "https://s3.amazonaws.com" -> {"https://", "s3.amazonaws.com", 443}
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    parse_endpoint(binary_to_list(Endpoint));
parse_endpoint(Endpoint) when is_list(Endpoint) ->
    case string:split(Endpoint, "://") of
        [Scheme, HostPort] ->
            case string:split(HostPort, ":", trailing) of
                [Host, PortStr] ->
                    Port = list_to_integer(PortStr),
                    {Scheme ++ "://", Host, Port};
                [Host] ->
                    % Default port based on scheme
                    DefaultPort = case Scheme of
                        "https" -> 443;
                        "http" -> 80
                    end,
                    {Scheme ++ "://", Host, DefaultPort}
            end;
        [HostOnly] ->
            % No scheme provided, assume HTTP
            {"http://", HostOnly, 80}
    end.

%% @doc Test that we can access the configured bucket.
test_bucket_access(Bucket, Config) ->
    BucketStr = binary_to_list(Bucket),
    % Try to list objects with max-keys=1 to minimize data transfer
    case erlcloud_s3:list_objects(BucketStr, [{max_keys, 1}], Config) of
        L when is_list(L) ->
            ok;
        {error, {aws_error, {http_error, 404, _, _}}} ->
            error({bucket_not_found, Bucket});
        {error, Reason} ->
            error({bucket_access_failed, Reason})
    end.

%% @doc Get a unique reference for this store instance.
get_store_ref(Opts) ->
    Bucket = maps:get(<<"bucket">>, Opts),
    Prefix = maps:get(<<"prefix">>, Opts, <<>>),
    {?MODULE, Bucket, Prefix}.

%% @doc Get stored configuration from persistent_term.
get_config(Opts) ->
    StoreRef = get_store_ref(Opts),
    case persistent_term:get(StoreRef, undefined) of
        undefined ->
            error(store_not_started);
        Config ->
            Config
    end.

%%%-----------------------------------------------------------------------------
%%% Core Read/Write Operations (Phase 3)
%%%-----------------------------------------------------------------------------

%% @doc Write a value to a key in S3.
-spec write(opts(), key(), value()) -> ok | {error, term()}.
write(Opts, Key, Value) when is_list(Key) ->
    % Convert list paths to binary
    write(Opts, hb_store:join(Key), Value);
write(Opts, Key, Value) ->
    try
        % Get stored configuration
        #{bucket := Bucket, prefix := Prefix, config := Config} = get_config(Opts),

        % Build full S3 key with prefix
        FullKey = build_s3_key(Prefix, Key),

        % Write to S3
        BucketStr = binary_to_list(Bucket),
        KeyStr = binary_to_list(FullKey),

        ?event(store_s3, {write, {key, FullKey}, {size, byte_size(Value)}}),

        case erlcloud_s3:put_object(BucketStr, KeyStr, Value, [], Config) of
            L when is_list(L) ->
                % Success - erlcloud returns a proplist
                ok;
            {error, Reason} ->
                ?event(error, {s3_write_failed, {key, FullKey}, {reason, Reason}}),
                {error, Reason}
        end
    catch
        error:CatchReason ->
            ?event(error, {s3_write_error, {key, Key}, {reason, CatchReason}}),
            {error, CatchReason}
    end.

%% @doc Build full S3 key with optional prefix.
build_s3_key(<<>>, Key) ->
    hb_store:join(Key);
build_s3_key(Prefix, Key) ->
    Path = hb_store:join(Key),
    % Ensure prefix ends with / for proper key namespacing
    PrefixWithSlash = case binary:last(Prefix) of
        $/ -> Prefix;
        _ -> <<Prefix/binary, "/">>
    end,
    <<PrefixWithSlash/binary, Path/binary>>.

%%%-----------------------------------------------------------------------------
%%% Link Support (Phase 4)
%%%-----------------------------------------------------------------------------

%% @doc Read a value from S3, following links if necessary.
-spec read(opts(), key()) -> {ok, value()} | not_found.
read(Opts, Key) when is_list(Key) ->
    read(Opts, hb_store:join(Key));
read(Opts, Key) ->
    read_with_links(Opts, Key, 0).

%% Internal read that tracks link depth to prevent infinite loops
read_with_links(_Opts, _Key, Depth) when Depth > ?MAX_LINK_DEPTH ->
    ?event(error, {too_many_links, {depth, Depth}}),
    not_found;
read_with_links(Opts, Key, Depth) ->
    % Read the key directly
    case read_direct(Opts, Key) of
        {ok, Value} ->
            % Check if it's a link
            case is_link(Value) of
                {true, Target} ->
                    ?event(store_s3, {follow_link, {from, Key}, {to, Target}}),
                    read_with_links(Opts, Target, Depth + 1);
                false ->
                    {ok, Value}
            end;
        not_found ->
            not_found
    end.

%% Direct read without link resolution
read_direct(Opts, Key) ->
    try
        #{bucket := Bucket, prefix := Prefix, config := Config} = get_config(Opts),

        FullKey = build_s3_key(Prefix, Key),
        BucketStr = binary_to_list(Bucket),
        KeyStr = binary_to_list(FullKey),

        case erlcloud_s3:get_object(BucketStr, KeyStr, [], Config) of
            L when is_list(L) ->
                Content = proplists:get_value(content, L),
                {ok, hb_util:bin(Content)};
            {error, {aws_error, {http_error, 404, _, _}}} ->
                not_found;
            {error, _Reason} ->
                not_found
        end
    catch
        error:_CatchReason ->
            not_found
    end.

%% @doc Create a symbolic link from New to Existing.
%% Links are stored as values with "link:" prefix.
-spec make_link(opts(), key(), key()) -> ok | {error, term()}.
make_link(Opts, Existing, New) ->
    % Convert to binary if needed
    ExistingBin = hb_util:bin(hb_store:join(Existing)),

    % Build link value with marker
    LinkValue = <<?LINK_MARKER/binary, ExistingBin/binary>>,

    ?event(store_s3, {make_link, {from, New}, {to, Existing}}),

    write(Opts, New, LinkValue).

%% @doc Check if a value is a link and extract the target.
%% Returns {true, Target} or false.
is_link(Value) ->
    LinkPrefixSize = byte_size(?LINK_MARKER),
    case byte_size(Value) > LinkPrefixSize of
        true ->
            case binary:part(Value, 0, LinkPrefixSize) of
                ?LINK_MARKER ->
                    Target = binary:part(Value, LinkPrefixSize,
                                       byte_size(Value) - LinkPrefixSize),
                    {true, Target};
                _ ->
                    false
            end;
        false ->
            false
    end.

%%%-----------------------------------------------------------------------------
%%% Groups and Listing (Phase 5)
%%%-----------------------------------------------------------------------------

%% @doc Create a group (virtual directory).
%% In S3, directories don't really exist, so this is a no-op.
%% Groups are detected by listing operations.
-spec make_group(opts(), key()) -> ok.
make_group(_Opts, _Path) ->
    % S3 doesn't need explicit directory creation
    % They exist implicitly when objects are stored with that prefix
    ok.

%% @doc List immediate children under a given path.
%% Treats the path as a directory prefix.
-spec list(opts(), key()) -> {ok, [binary()]} | {error, term()}.
list(Opts, Path) when is_list(Path) ->
    list(Opts, hb_store:join(Path));
list(Opts, Path) ->
    try
        #{bucket := Bucket, prefix := Prefix, config := Config} = get_config(Opts),

        % Check if Path is a link and resolve it
        ResolvedPath = case read_direct(Opts, Path) of
            {ok, Value} ->
                case is_link(Value) of
                    {true, Target} ->
                        Target;
                    false ->
                        Path
                end;
            not_found ->
                Path
        end,

        % Build S3 prefix for listing
        FullPath = build_s3_key(Prefix, ResolvedPath),

        % Ensure path ends with / for S3 listing
        SearchPrefix = ensure_trailing_slash(FullPath),

        BucketStr = binary_to_list(Bucket),
        PrefixStr = binary_to_list(SearchPrefix),

        ?event(store_s3, {list, {prefix, SearchPrefix}}),

        % Use delimiter to get only immediate children
        ListOpts = [{prefix, PrefixStr}, {delimiter, "/"}],

        case erlcloud_s3:list_objects(BucketStr, ListOpts, Config) of
            L when is_list(L) ->
                Children = extract_children(SearchPrefix, L),
                {ok, Children};
            {error, _Reason} ->
                {ok, []}
        end
    catch
        error:Reason ->
            ?event(error, {s3_list_error, {path, Path}, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Ensure a path ends with / for S3 directory listing.
ensure_trailing_slash(<<>>) ->
    <<>>;
ensure_trailing_slash(Path) ->
    case binary:last(Path) of
        $/ -> Path;
        _ -> <<Path/binary, "/">>
    end.

%% @doc Extract immediate children from S3 list response.
%% Returns only the child names, not full paths.
%% Returns both objects (files) and common prefixes (directories), like file:list_dir().
extract_children(Prefix, S3Response) ->
    % Get regular objects (actual files)
    Contents = proplists:get_value(contents, S3Response, []),

    % Get common prefixes (subdirectories)
    CommonPrefixes = proplists:get_value(common_prefixes, S3Response, []),

    % Extract object names - only immediate children
    Objects = lists:filtermap(
        fun(Obj) ->
            Key = list_to_binary(proplists:get_value(key, Obj, "")),
            case strip_prefix(Prefix, Key) of
                <<>> -> false;
                Child ->
                    % Only include if it's an immediate child (no / in name)
                    case binary:match(Child, <<"/">>) of
                        nomatch -> {true, Child};
                        _ -> false
                    end
            end
        end,
        Contents
    ),

    % Extract directory names (common prefixes)
    Dirs = lists:filtermap(
        fun(P) ->
            PrefixBin = list_to_binary(proplists:get_value(prefix, P, "")),
            case strip_prefix(Prefix, PrefixBin) of
                <<>> -> false;
                Child ->
                    % Remove trailing slash from directory name
                    ChildName = case binary:last(Child) of
                        $/ -> binary:part(Child, 0, byte_size(Child) - 1);
                        _ -> Child
                    end,
                    {true, ChildName}
            end
        end,
        CommonPrefixes
    ),

    % Return unique sorted list (both files and directories, like file:list_dir)
    lists:usort(Objects ++ Dirs).

%% @doc Remove a prefix from a binary if it matches.
strip_prefix(Prefix, Bin) ->
    PrefixLen = byte_size(Prefix),
    case Bin of
        <<Prefix:PrefixLen/binary, Rest/binary>> -> Rest;
        _ -> Bin
    end.

%%%-----------------------------------------------------------------------------
%%% Type Detection (Phase 6)
%%%-----------------------------------------------------------------------------

%% @doc Determine if a key represents a simple value or composite group.
-spec type(opts(), key()) -> simple | composite | not_found.
type(Opts, Key) when is_list(Key) ->
    type(Opts, hb_store:join(Key));
type(Opts, Key) ->
    % Try to read the key directly
    case read_direct(Opts, Key) of
        {ok, Value} ->
            % Check if it's a link and resolve it
            case is_link(Value) of
                {true, Target} ->
                    % Recursively check the target's type
                    type(Opts, Target);
                false ->
                    % It's a simple value
                    simple
            end;
        not_found ->
            % Check if it has children (is a composite/directory)
            case has_children(Opts, Key) of
                true -> composite;
                false -> not_found
            end
    end.

%% @doc Check if a path has any children (is a directory).
has_children(Opts, Path) ->
    #{bucket := Bucket, prefix := Prefix, config := Config} = get_config(Opts),

    FullPath = build_s3_key(Prefix, Path),
    SearchPrefix = ensure_trailing_slash(FullPath),

    BucketStr = binary_to_list(Bucket),
    PrefixStr = binary_to_list(SearchPrefix),

    % List with max-keys=1 to check if anything exists
    ListOpts = [{prefix, PrefixStr}, {max_keys, 1}],

    case erlcloud_s3:list_objects(BucketStr, ListOpts, Config) of
        L when is_list(L) ->
            Contents = proplists:get_value(contents, L, []),
            length(Contents) > 0;
        _ ->
            false
    end.

%%%-----------------------------------------------------------------------------
%%% Path Resolution (Phase 7)
%%%-----------------------------------------------------------------------------

%% @doc Resolve any links in a path.
%% Follows links in each path segment except the last.
-spec resolve(opts(), key()) -> binary().
resolve(Opts, Path) when is_list(Path) ->
    resolve(Opts, hb_store:join(Path));
resolve(Opts, Path) when is_binary(Path) ->
    Parts = binary:split(Path, <<"/">>, [global]),
    case resolve_path_segments(Opts, Parts, 0) of
        {ok, Resolved} -> Resolved;
        {error, _} -> Path
    end.

%% Internal path resolution that resolves all segments including the last
resolve_path_segments(_Opts, _Path, Depth) when Depth > ?MAX_LINK_DEPTH ->
    {error, too_many_redirects};
resolve_path_segments(_Opts, [], _Depth) ->
    {ok, <<>>};
resolve_path_segments(Opts, Parts, Depth) ->
    resolve_path_accumulate(Opts, Parts, <<>>, Depth).

% Accumulator-based resolution
resolve_path_accumulate(_Opts, [], Acc, _Depth) ->
    {ok, Acc};
resolve_path_accumulate(_Opts, _Parts, _Acc, Depth) when Depth > ?MAX_LINK_DEPTH ->
    {error, too_many_redirects};
resolve_path_accumulate(Opts, [Head|Tail], Acc, Depth) ->
    % Build the current path segment
    CurrentPath = case Acc of
        <<>> -> Head;
        _ -> <<Acc/binary, "/", Head/binary>>
    end,

    % Check if current path is a link
    case read_direct(Opts, CurrentPath) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} ->
                    % It's a link - replace accumulated path with target and continue
                    resolve_path_accumulate(Opts, Tail, Target, Depth + 1);
                false ->
                    % It's a regular value, continue accumulating
                    resolve_path_accumulate(Opts, Tail, CurrentPath, Depth)
            end;
        not_found ->
            % Path segment doesn't exist as a link, continue accumulating
            resolve_path_accumulate(Opts, Tail, CurrentPath, Depth)
    end.

%% @doc Convert path to canonical form.
-spec path(opts(), key()) -> binary().
path(_Opts, Path) ->
    hb_store:join(Path).

%% @doc Add two path components together.
-spec add_path(opts(), key(), key()) -> list().
add_path(_Opts, Path1, Path2) when is_list(Path1), is_list(Path2) ->
    Path1 ++ Path2;
add_path(_Opts, Path1, Path2) ->
    P1 = case is_binary(Path1) of
        true -> binary:split(Path1, <<"/">>, [global]);
        false -> Path1
    end,
    P2 = case is_binary(Path2) of
        true -> binary:split(Path2, <<"/">>, [global]);
        false -> Path2
    end,
    P1 ++ P2.

%%%-----------------------------------------------------------------------------
%%% Remaining Functions (Phase 8)
%%%-----------------------------------------------------------------------------

%% @doc Stop the S3 store and clean up resources.
-spec stop(opts()) -> ok.
stop(Opts) ->
    StoreRef = get_store_ref(Opts),
    persistent_term:erase(StoreRef),
    ok.

%% @doc Reset the store by deleting all objects.
%% Requires "dangerous_reset" => true for safety.
-spec reset(opts()) -> ok | {error, term()}.
reset(Opts) ->
    case maps:get(<<"dangerous_reset">>, Opts, false) of
        true ->
            % Only proceed if explicitly confirmed
            delete_all_objects(Opts);
        false ->
            {error, reset_not_confirmed}
    end.

delete_all_objects(Opts) ->
    #{bucket := Bucket, prefix := Prefix, config := Config} = get_config(Opts),

    BucketStr = binary_to_list(Bucket),
    PrefixStr = binary_to_list(Prefix),

    % List all objects with prefix
    case erlcloud_s3:list_objects(BucketStr, [{prefix, PrefixStr}], Config) of
        L when is_list(L) ->
            Contents = proplists:get_value(contents, L, []),
            Keys = [proplists:get_value(key, Obj) || Obj <- Contents],

            % Delete all objects
            case Keys of
                [] -> ok;
                _ ->
                    erlcloud_s3:delete_objects(BucketStr, Keys, Config),
                    ok
            end;
        _ ->
            ok
    end.

%% @doc Return the scope of this store.
%% Defaults to local to match filesystem behavior, but can be overridden in config.
-spec scope() -> local.
scope() -> local.

-spec scope(opts()) -> local | remote.
scope(#{ <<"scope">> := Scope }) -> Scope;
scope(_Opts) -> scope().

%% @doc Match keys based on a template.
%% Simple implementation - just returns not_found for now.
-spec match(opts(), map()) -> {ok, [binary()]} | not_found.
match(_Opts, _Template) ->
    % This would require listing all objects and checking each one
    % For MVP, we'll skip this feature
    not_found.
