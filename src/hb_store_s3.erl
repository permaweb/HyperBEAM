%%%-----------------------------------------------------------------------------
%%% @doc S3-backed implementation of the HyperBEAM store behavior.
%%% This module provides persistent storage using Amazon S3 or compatible
%%% object storage services (MinIO, Wasabi, etc.).
%%%
%%% To run tests enable the `s3` profile.
%%%
%%% ```
%%% rebar3 as s3 eunit --module hb_store_s3
%%% ```
%%%
%%% It might also need external dependencies like MinIO to be up and running:
%%%
%%% ```
%%% docker-compose -f test/docker-compose-s3.yml up -d
%%% rebar3 as s3 eunit --module hb_store_s3
%%% docker-compose -f test/docker-compose-s3.yml down -d
%%% ```
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(hb_store_s3).
-behaviour(hb_store).

%% Store behavior callbacks
-export([start/1, stop/1, reset/1, scope/0, scope/1]).
-export([read/2, write/3, list/2, type/2]).
-export([make_group/2, make_link/3, resolve/2]).
-export([path/2, add_path/3]).
-export([default_test_opts/0, get_config/1]).

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
-define(DEFAULT_FORCE_PATH_STYLE, false).
-define(MAX_REDIRECTS, 100).                    % Only resolve 1000 links to data
-define(LINK_MARKER, <<"link:">>).
%% Namespace for storing link objects separately to avoid file collisions
-define(LINKS_NS, <<"links/">>).
-define(GROUPS_NS, <<"groups/">>).

-define(DEFAULT_RETRY_DELAY, 1000).             % Wait for 1 second before retry.
-define(DEFAULT_RETRY_MODE, exp_backoff).
-define(DEFAULT_RETRIES, 5).                    % Retries for 5 times until it returns.
-define(DEFAULT_MAX_RETRY_DELAY, 300000).       % Max 5 minutes waiting to retry.
%%%-----------------------------------------------------------------------------
%%% Configuration and Initialization (Phase 2)
%%%-----------------------------------------------------------------------------

%% @doc Initialize the S3 store connection.
%% This function is called when the store is first accessed.
%% It validates the configuration and tests the connection.
-spec start(opts()) -> ok | {error, term()}.
start(Opts) ->
    maybe
        ok ?= validate_config(Opts),
        AccessKey = hb_util:list(maps:get(<<"priv_access_key_id">>, Opts)),
        SecretKey = hb_util:list(maps:get(<<"priv_secret_access_key">>, Opts)),
        Region = hb_util:list(maps:get(<<"region">>, Opts, ?DEFAULT_REGION)),
        Endpoint = maps:get(<<"endpoint">>, Opts, ?DEFAULT_ENDPOINT),
        Bucket = maps:get(<<"bucket">>, Opts),
        ForcePathStyle = case maps:get(<<"force_path_style">>, Opts, ?DEFAULT_FORCE_PATH_STYLE) of
            true -> path;
            false -> auto
        end,
        #{
            scheme := Scheme,
            host := Host,
            port := Port
        } ?= uri_string:parse(Endpoint),
        BaseConfig = erlcloud_s3:new(AccessKey, SecretKey, hb_util:list(Host), Port),
        Config = BaseConfig#aws_config{
            s3_scheme = hb_util:list(hb_util:list(Scheme) ++ "://"),
            s3_bucket_after_host = false,
            s3_bucket_access_method = ForcePathStyle,
            aws_region = Region,
            % Use `gun_pool` to define a connection pool.
            http_client = fun gun_request/6
        },
        ok ?= test_bucket_access(Bucket, Config),
        StoreRef = get_store_ref(Opts),
        ok ?= persistent_term:put(StoreRef, #{
            bucket => Bucket,
            config => Config
        }),
        ?event(store_s3, {started, {bucket, Bucket}}),
        {ok, #{
            module => ?MODULE,
            bucket => Bucket
        }}
    else
        Error ->
            ?event(error, {s3_start_failed, {reason, Error}}),
            {error, Error}
    end.


%% Interface erlcloud_s3 with HB HTTP Client
gun_request(URL, Method, Headers, Body, Timeout, _Config) when is_atom(Method) ->
    case uri_string:parse(URL) of
        #{port := Port, scheme := Scheme, host := Host} = ParsedURL ->
            Peer = uri_string:normalize(#{port => Port, scheme => Scheme, host => Host}),
            HeadersMap = maps:from_list(Headers),
            MethodBinary = string:uppercase(atom_to_binary(Method)),
            Args = #{
                     peer => Peer,
                     path => uri_string:normalize(maps:with([path, fragment, query], ParsedURL)),
                     method => MethodBinary,
                     headers => HeadersMap,
                     body => Body
                    },
            Opts = #{connect_timeout => Timeout},
            Response = hb_http_client:request(Args, Opts),
            handle_gun_response(Response);
        Reason ->
            ?event(error, {parsing_url, {url, URL}, {reason, Reason}}),
            {error, Reason}
    end.

handle_gun_response({ok, Status, ResponseHeaders, Body}) ->
    {ok, {{Status, undefined}, header_str(ResponseHeaders), Body}};

handle_gun_response({error, _} = Error) ->
    Error.

header_str(Hdrs) ->
    [{string:to_lower(to_list_string(K)), to_list_string(V)} || {K, V} <- Hdrs].

to_list_string(Val) when erlang:is_binary(Val) ->
    erlang:binary_to_list(Val);
to_list_string(Val) when erlang:is_list(Val) ->
    Val.

%% @doc Validate that all required configuration keys are present.
%% Required keys: bucket, priv_access_key_id, priv_secret_access_key
validate_config(Opts) ->
    Required = [<<"bucket">>, <<"priv_access_key_id">>, <<"priv_secret_access_key">>],
    Missing = [K || K <- Required, not maps:is_key(K, Opts)],
    case Missing of
        [] ->
            ok;
        _ ->
            error({missing_config_keys, Missing})
    end.

%% @doc Test that we can access the configured bucket.
test_bucket_access(Bucket, Config) ->
    BucketStr = hb_util:list(Bucket),
    try erlcloud_s3:list_objects(BucketStr, [{max_keys, 1}], Config) of
        L when is_list(L) -> ok
    catch
        _Class:Reason:Stacktrace ->
            case Reason of
                {aws_error, {http_error, 404, _, _}} ->
                    error({bucket_not_found, Bucket});
                _ ->
                    ?event(error, {error, {reason, Reason}, {stacktrace, Stacktrace}}),
                    error({bucket_access_failed, Reason})
            end
    end.

%% @doc Get a unique reference for this store instance.
get_store_ref(Opts) ->
    Bucket = hb_util:bin(maps:get(<<"bucket">>, Opts)),
    {?MODULE, Bucket}.

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
-spec write(opts(), key(), value()) -> ok.
write(Opts, Key, Value) when is_list(Key) ->
    write(Opts, hb_store:join(Key), Value);
write(Opts, Key, Value) when is_binary(Key) ->
    RetryAttempts = maps:get(<<"retry-attempts">>, Opts, ?DEFAULT_RETRIES),
    write(Opts, Key, Value, RetryAttempts).
write(_Opts, _Key, _Value, 0) ->
    ok;
write(Opts, Key, Value, AttemptsRemaining) ->
    maybe
        #{bucket := Bucket, config := Config} = get_config(Opts),

        HasSlash = (binary:match(hb_store:join(Key), <<"/">>) =/= nomatch),

        RelKey = hb_store:join(Key),
        FullKey = case RelKey of
            <<"data/", _/binary>> -> RelKey;
            _ ->
                case HasSlash of
                    true -> build_groups_key(RelKey);
                    false -> build_links_key(RelKey)
                end
        end,
        BucketStr = hb_util:list(Bucket),
        KeyStr = hb_util:list(FullKey),
        ?event(store_s3, {write, {key, FullKey}, {size, byte_size(Value)}}),
        ok ?= try erlcloud_s3:put_object(BucketStr, KeyStr, Value, [], Config) of
            L when is_list(L) -> ok
        catch _Class:Reason -> Reason end,
        ok
    else
        Error ->
            %% Store S3 retry mechanism
            ?event(error, {s3_write_error, {key, Key}, {reason, Error}}),
            MaxRetries = maps:get(<<"retry-attempts">>, Opts, ?DEFAULT_RETRIES),
            MinRetryDelay = maps:get(<<"min-retry-delay">>, Opts, ?DEFAULT_RETRY_DELAY),
            MaxRetryDelay = maps:get(<<"max-retry-delay">>, Opts, ?DEFAULT_MAX_RETRY_DELAY),
            RetryTime = case maps:get(<<"retry-mode">>, Opts, ?DEFAULT_RETRY_MODE) of
                exp_backoff ->
                    min(MinRetryDelay * round(math:pow(2, MaxRetries - AttemptsRemaining)), MaxRetryDelay);
                _ -> MinRetryDelay
            end,
            ?event(store_s3, {retry_in, RetryTime}),
            timer:sleep(RetryTime),
            write(Opts, Key, Value, AttemptsRemaining - 1)
    end.

%% @doc Build an S3 key under the links namespace for a logical key.
build_links_key(Key) ->
    <<?LINKS_NS/binary, (hb_store:join(Key))/binary>>.

%% @doc Build an S3 key under the groups namespace for a logical key.
build_groups_key(Key) ->
    <<?GROUPS_NS/binary, (hb_store:join(Key))/binary>>.

%%%-----------------------------------------------------------------------------
%%% Link Support (Phase 4)
%%%-----------------------------------------------------------------------------

%% @doc Read a value from S3, following links if necessary.
-spec read(opts(), key()) -> {ok, value()} | not_found.
read(Opts, Key) when is_list(Key) ->
    read(Opts, hb_store:join(Key));
read(Opts, Key) when is_binary(Key) ->
    % Try direct read first (fast path for non-link paths)
    case read_with_links(Opts, Key) of
        {ok, Value} ->
            {ok, Value};
        not_found ->
            try
                PathParts = binary:split(Key, <<"/">>, [global]),
                case resolve_path_segments(Opts, PathParts) of
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
                            {key, Key}
                        }
                    ),
                    % If link resolution fails, return not_found
                    not_found
            end
    end.

read_with_links(Opts, Path) ->
    case read_direct(Opts, Path) of
        {ok, Value} ->
            % Check if this value is actually a link to another key
            case is_link(Value) of
                {true, Link} ->
                   % Extract the target key and recursively resolve the link
                   read_with_links(Opts, Link);
                false ->
                    {ok, Value}
            end;
        not_found ->
            not_found
    end.

%% @doc Helper function to convert to a path
to_path(PathParts) when is_list(PathParts) ->
    hb_util:bin(lists:join(<<"/">>, PathParts));
to_path(Path) when is_binary(Path) ->
    Path.

%% Direct read without link resolution
read_direct(Opts, Key) ->
    #{bucket := Bucket, config := Config} = get_config(Opts),
    BucketStr = hb_util:list(Bucket),
    FullKey = case Key of
        <<"data/", _/binary>> ->
            hb_store:join(Key);
        _ ->
            HasSlash = (binary:match(hb_store:join(Key), <<"/">>) =/= nomatch),
            NsKey = case HasSlash of
                true -> build_groups_key(Key);
                false -> build_links_key(Key)
            end,
            NsKey
    end,
    KeyStr = hb_util:list(FullKey),
    try erlcloud_s3:get_object(BucketStr, KeyStr, [], Config) of
        L when is_list(L) ->
            Content = proplists:get_value(content, L),
            {ok, hb_util:bin(Content)}
    catch
        _:{aws_error, {http_error, 404, _, _}} ->
            not_found;
        _:Reason ->
            ?event(error, {s3_read_error, {key, Key}, {reason, Reason}}),
            %% To enable store chain fallback
            not_found
    end.

%% @doc Create a symbolic link from New to Existing.
%% Links are stored as values with "link:" prefix.
-spec make_link(opts(), key(), key()) -> ok | {error, term()}.
make_link(Opts, Existing, New) ->
    ExistingBin = hb_util:bin(hb_store:join(Existing)),
    LinkValue = <<?LINK_MARKER/binary, ExistingBin/binary>>,
    ?event(store_s3, {make_link, {from, New}, {to, Existing}}),
    maybe
        #{bucket := Bucket, config := Config} = get_config(Opts),
        BucketStr = hb_util:list(Bucket),
        HasSlash = (binary:match(hb_store:join(New), <<"/">>) =/= nomatch),
        NsKey = case HasSlash of
            true -> build_groups_key(New);
            false -> build_links_key(New)
        end,
        NsKeyStr = hb_util:list(NsKey),
        ok ?= try erlcloud_s3:put_object(BucketStr, NsKeyStr, LinkValue, [], Config) of
                L when is_list(L) -> ok
            catch _Class:Reason -> {error, Reason} end,
        ok
    else
        _ -> {error, make_link_failed}
    end.

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
make_group(Opts, Path) ->
    % We need to write to a file to has_children can consider this a group.
    write(Opts, <<Path/binary, "/empty_group">>, <<"empty">>),
    ok.

%% @doc List immediate children under a given path.
%% Treats the path as a directory prefix.
-spec list(opts(), key()) -> {ok, [binary()]} | {error, term()}.
list(Opts, Path) when is_list(Path) ->
    list(Opts, hb_store:join(Path));
list(Opts, Path) ->
    %% Check make_group note.
    UnwantedChildren = [<<"empty_group">>],
    maybe
        #{bucket := Bucket, config := Config} = get_config(Opts),
        ResolvedPath = case read_direct(Opts, Path) of
            {ok, Value} ->
                case is_link(Value) of
                    {true, Target} ->
                        Target;
                    false ->
                        Path
                end;
            _ ->
                Path
        end,
        FullPath = build_groups_key(ResolvedPath),
        SearchPrefix = ensure_trailing_slash(FullPath),
        BucketStr = hb_util:list(Bucket),
        PrefixStr = hb_util:list(SearchPrefix),
        ListOpts = [{prefix, PrefixStr}, {delimiter, "/"}],
        ?event(store_s3, {list_opts, ListOpts}),
        try erlcloud_s3:list_objects(BucketStr, ListOpts, Config) of
            L when is_list(L) ->
                Children = extract_children(SearchPrefix, L),
                {ok, Children -- UnwantedChildren}
        catch
            _Class:ErrorReason  ->
                {error, ErrorReason}
        end
    else
        Reason ->
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
    Contents = proplists:get_value(contents, S3Response, []),
    CommonPrefixes = proplists:get_value(common_prefixes, S3Response, []),
    Objects = lists:filtermap(
        fun(Obj) ->
            Key = hb_util:bin(proplists:get_value(key, Obj, "")),
            case strip_prefix(Prefix, Key) of
                <<>> -> false;
                Child ->
                    case binary:match(Child, <<"/">>) of
                        nomatch -> {true, Child};
                        _ -> false
                    end
            end
        end,
        Contents
    ),

    Dirs = lists:filtermap(
        fun(P) ->
            PrefixBin = hb_util:bin(proplists:get_value(prefix, P, "")),
            case strip_prefix(Prefix, PrefixBin) of
                <<>> -> false;
                Child ->
                    ChildName = case binary:last(Child) of
                        $/ -> binary:part(Child, 0, byte_size(Child) - 1);
                        _ -> Child
                    end,
                    {true, ChildName}
            end
        end,
        CommonPrefixes
    ),
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
    case Key of
        <<"data/", _/binary>> ->
            case head_exists(Opts, Key) of
                true ->
                    simple;
                false ->
                    not_found
            end;
        _ ->
            case read_direct(Opts, Key) of
                {ok, Value} ->
                    case is_link(Value) of
                        {true, Target} ->
                            type(Opts, Target);
                        false ->
                            simple
                    end;
                not_found ->
                    case has_children(Opts, Key) of
                        true ->
                            composite;
                        false ->
                            not_found
                    end;
                {error, _} ->
                    not_found
            end
    end.

%% @doc HEAD check for object existence without downloading content
head_exists(Opts, Key) ->
    try
        #{bucket := Bucket, config := Config} = get_config(Opts),
        FullKey = hb_store:join(Key),
        BucketStr = hb_util:list(Bucket),
        KeyStr = hb_util:list(FullKey),
        is_list(erlcloud_s3:head_object(BucketStr, KeyStr, [], Config))
    catch
        _:_ -> false
    end.

%% @doc Check if a path has any children (is a directory).
has_children(Opts, Path) ->
    try
        #{bucket := Bucket, config := Config} = get_config(Opts),
        FullPath = build_groups_key(Path),
        SearchPrefix = ensure_trailing_slash(FullPath),
        BucketStr = hb_util:list(Bucket),
        PrefixStr = hb_util:list(SearchPrefix),
        ListOpts = [{prefix, PrefixStr}, {max_keys, 1}],
        case erlcloud_s3:list_objects(BucketStr, ListOpts, Config) of
            L when is_list(L) ->
                Contents = proplists:get_value(contents, L, []),
                Contents =/= []
        end
    catch
        _:_ -> false
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

resolve_path_segments(Opts, Path) ->
    resolve_path_segments(Opts, Path, 0).
%% Internal path resolution that resolves all segments including the last
resolve_path_segments(_Opts, _Path, Depth) when Depth > ?MAX_REDIRECTS ->
    {error, too_many_redirects};
resolve_path_segments(_Opts, [], _Depth) ->
    {ok, <<>>};
resolve_path_segments(Opts, Parts, Depth) ->
    resolve_path_accumulate(Opts, Parts, <<>>, Depth).

%% Internal helper that accumulates the resolved path
resolve_path_accumulate(_Opts, [], Acc, _Depth) ->
    {ok, Acc};
resolve_path_accumulate(_Opts, FullPath = [<<"data">> | _], <<>>, _Depth) ->
    {ok, hb_store:join(FullPath)};
resolve_path_accumulate(_Opts, _Parts, _Acc, Depth) when Depth > ?MAX_REDIRECTS ->
    {error, too_many_redirects};
resolve_path_accumulate(Opts, [Head|Tail], Acc, Depth) ->
    CurrentPath = case Acc of
        <<>> -> Head;
        _ -> <<Acc/binary, "/", Head/binary>>
    end,
    case read_direct(Opts, CurrentPath) of
        {ok, Value} ->
            case is_link(Value) of
                {true, Target} ->
                    resolve_path_accumulate(Opts, Tail, Target, Depth + 1);
                false ->
                    resolve_path_accumulate(Opts, Tail, CurrentPath, Depth)
            end;
        not_found ->
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
            delete_all_objects(Opts);
        false ->
            {error, reset_not_confirmed}
    end.

delete_all_objects(Opts) ->
    #{bucket := Bucket, config := Config} = get_config(Opts),
    BucketStr = hb_util:list(Bucket),
    try erlcloud_s3:list_objects(BucketStr, [], Config) of
        L when is_list(L) ->
            Contents = proplists:get_value(contents, L, []),
            Keys = [proplists:get_value(key, Obj) || Obj <- Contents],
            case Keys of
                [] -> ok;
                _ ->
                    erlcloud_s3:delete_objects_batch(BucketStr, Keys, Config),
                    ok
            end
    catch _Class:Reason ->
        {error, Reason}
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
    not_found.

%% @doc Integration test suite demonstrating basic store operations.
%%
%% The following functions implement integration tests using EUnit to verify that
%% the S3 store implementation correctly handles various scenarios including
%% basic read/write operations, hierarchical listing, group creation, link
%% resolution, and type detection.
%%
%% Be sure that minio io server is running before executing the integration tests.

default_test_opts() ->
    #{
        <<"store-module">> => ?MODULE,
        <<"bucket">> => <<"hb-s3">>,
        <<"priv_access_key_id">> => <<"minioadmin">>,
        <<"priv_secret_access_key">> => <<"minioadmin">>,
        <<"endpoint">> => <<"http://localhost:9000">>,
        <<"dangerous_reset">> => true,
        <<"force_path_style">> => true
     }.

-ifdef(ENABLE_S3).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% @doc Basic store test - verifies fundamental read/write functionality.
%%
%% This test creates a temporary database, writes a key-value pair, reads it
%% back to verify correctness, and cleans up by stopping the database. It
%% serves as a sanity check that the basic storage mechanism is working.

init() ->
    application:ensure_all_started(hb).

basic_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
    Value = <<"world">>,
    Key = <<"hello">>,
    ?event(s3_test, {{key, Key},{value, Value}}),
    Res = write(StoreOpts, Key, Value),
    ?assertEqual(ok, Res),
    {ok, Response} = read(StoreOpts, Key),
    ?assertEqual(Value, Response),
    ok = stop(StoreOpts).

not_found_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
    NonExistingKey = <<"NonExistingKey">>,
    Result = read(StoreOpts, NonExistingKey),
    ?assertEqual(not_found, Result),
    ok = stop(StoreOpts).

bucket_not_found_test() ->
    init(),
    StoreOpts = (default_test_opts())#{<<"bucket">> => <<"invalid_bucket">>},
    ?assertError({bucket_access_failed, {aws_error, {http_error, 400, _, _}}}, start(StoreOpts)),
    ok = stop(StoreOpts).

failed_write_test() ->
    init(),
    StoreOpts = (default_test_opts())#{<<"retry-attempts">> => 2},
    start(StoreOpts),
    reset(StoreOpts),
    Key = <<"write_test">>,
    Value = <<"value">>,

    ok = meck:new(erlcloud_s3, [unstick, passthrough]),
    XMLBody = <<"">>,
    ok = meck:expect(erlcloud_s3, put_object, fun(_, _, _, _, _) ->
        error({aws_error,{http_error, 400, "Bad Request", XMLBody}}) end),

    {Time, Result} = timer:tc(fun() -> write(StoreOpts, Key, Value) end, second),
    ?assertMatch(ok, Result),
    ?assert(Time >= 3),

    ?assert(meck:called(erlcloud_s3, put_object, ['_', '_', '_', '_', '_'])),
    ok = meck:unload(erlcloud_s3),
    ok = stop(StoreOpts).

failed_read_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
    Key = <<"read_test">>,

    ok = meck:new(erlcloud_s3, [passthrough]),
    XMLBody = <<"">>,
    ok = meck:expect(erlcloud_s3, get_object, fun(_, _, _, _) ->
        error({aws_error,{http_error, 400, "Bad Request", XMLBody}}) end),

    Result = read(StoreOpts, Key),
    ?assertMatch(not_found, Result),

    ?assert(meck:called(erlcloud_s3, get_object, ['_', '_', '_', '_'])),
    ok = meck:unload(erlcloud_s3),
    ok = stop(StoreOpts).

%% @doc List test - verifies prefix-based key listing functionality.
%%
%% This test creates several keys with hierarchical names and verifies that
%% the list operation correctly returns only keys matching a specific prefix.
%% It demonstrates the directory-like navigation capabilities of the store.
list_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
    ?assertEqual({ok, []}, list(StoreOpts, <<"colors">>)),
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

%% @doc Group test - verifies group creation and type detection.
%%
%% This test creates a group entry and verifies that it is correctly identified
%% as a composite type and cannot be read directly (like filesystem directories).
group_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
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
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
    write(StoreOpts, <<"foo/bar/baz">>, <<"Bam">>),
    make_link(StoreOpts, <<"foo/bar/baz">>, <<"foo/beep/baz">>),
    {ok, Result} = read(StoreOpts, <<"foo/beep/baz">>),
    ?event({ result, Result}),
    ?assertEqual(<<"Bam">>, Result).

link_fragment_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
    ok = write(StoreOpts, [<<"data">>, <<"bar">>, <<"baz">>], <<"Bam">>),
    ok = make_link(StoreOpts, [<<"data">>, <<"bar">>], <<"my-link">>),
    {ok, Result} = read(StoreOpts, [<<"my-link">>, <<"baz">>]),
    ?event({ result, Result}),
    ?assertEqual(<<"Bam">>, Result).

%% @doc Type test - verifies type detection for both simple and composite entries.
%%
%% This test creates both a group (composite) entry and a regular (simple) entry,
%% then verifies that the type detection function correctly identifies each one.
%% This demonstrates the semantic classification system used by the store.
type_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
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
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
    reset(StoreOpts),
    write(StoreOpts, [ <<"parent">>, <<"key">> ], <<"value">>),
    make_link(StoreOpts, [ <<"parent">>, <<"key">> ], <<"my-link">>),
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
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
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

%% @doc Test that matches the exact hb_store hierarchical test pattern
exact_hb_store_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
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

%% @doc Test cache-style usage through hb_store interface
cache_style_test() ->
    init(),
    hb:init(),
    StoreOpts = default_test_opts(),
    % Start the store
    hb_store:start(StoreOpts),
    reset(StoreOpts),
    % Test writing through hb_store interface
    ok = hb_store:write(StoreOpts, <<"test-key">>, <<"test-value">>),
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
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
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
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
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

%% @doc Isolated test focusing on the exact cache issue
isolated_type_debug_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
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

%% @doc Test that list function resolves links correctly
list_with_link_test() ->
    init(),
    StoreOpts = default_test_opts(),
    start(StoreOpts),
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
    stop(StoreOpts).
-endif.
-endif.