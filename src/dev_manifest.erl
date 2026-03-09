%%% @doc An Arweave path manifest resolution device. Follows the v1 schema:
%%% https://specs.ar.io/?tx=lXLd0OPwo-dJLB_Amz5jgIeDhiOkjXuM3-r0H_aiNj0
-module(dev_manifest).
-export([index/3, info/0, request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Use the `route/4' function as the handler for all requests, aside 
%% from `keys' and `set', which are handled by the default resolver.
info() ->
    #{
        default => fun route/4,
        excludes => [keys, set, committers]
    }.

%% @doc Return the fallback index page when the manifest itself is requested.
index(M1, M2, Opts) ->
    ?event(debug_manifest, {index_request, {base, M1}, {request, M2}}, Opts),
    case route(<<"index">>, M1, M2, Opts) of
        {ok, Index} ->
            ?event({manifest_index_returned, Index}),
            {ok, Index};
        {error, not_found} ->
            {error, not_found}
    end.

%% @doc Route a request to the associated data via its manifest.
route(<<"index">>, M1, M2, Opts) ->
    ?event({manifest_index, M1, M2}),
    case manifest(M1, M2, Opts) of
        {ok, Manifest} ->
            % Get the path to the index page from the manifest. We make
            % sure to use `hb_maps:get/4' to ensure that we do not recurse
            % on the `index' key with an `ao' resolve.
            Index =
                hb_maps:get(
                    <<"index">>,
                    Manifest,
                    #{},
                    Opts
                ),
            ?event(debug_manifest,
                {manifest_index_found,
                    {index, Index},
                    {manifest, Manifest}
                }
            ),
            Path = hb_maps:get(<<"path">>, Index, not_found, Opts),
            case Path of
                not_found ->
                    ?event({manifest_path_not_found, <<"index/path">>}),
                    {error, not_found};
                _ ->
                    ?event({manifest_path, Path}),
                    route(Path, M1, M2, Opts)
            end;
        {error, not_found} ->
            ?event(manifest_not_parsed),
            {error, not_found}
    end;
route(ID, _, _, Opts) when ?IS_ID(ID) ->
    ?event({manifest_reading_id, ID}),
    hb_cache:read(ID, Opts);
route(Key, M1, M2, Opts) ->
    ?event(debug_manifest, {manifest_lookup, {key, Key}, {m1, M1}, {m2, {explicit, M2}}}),
    {ok, Manifest} = manifest(M1, M2, Opts),
    {ok, Res} = maps:find(<<"paths">>, Manifest),
    case maps:get(Key, Res, no_path_match) of
        no_path_match ->
            % Support materialized view in some JavaScript frameworks.
            case hb_opts:get(manifest_404, fallback, Opts) of
                error ->
                    ?event({manifest_404_error, {key, Key}}),
                    {error, not_found};
                fallback ->
                    ?event({manifest_fallback, {key, Key}}),
                    route(<<"index">>, M1, M2, Opts)
            end;
        Result ->
            ?event({manifest_lookup_success, {key, Key}, {result, Result}}),
            try {ok, hb_cache:ensure_loaded(Result, Opts)}
            catch _:_:_ -> {error, not_found}
            end
    end.

%% @doc Implement the `on/request' hook for the `manifest@1.0' device, finding
%% requests for legacy (non-device-tagged) manifests and casting them to
%% `manifest@1.0' before execution. Allowing `/ID/path` style access for old data.
request(Base, Req, Opts) ->
    ?event({on_req_manifest_detector, {base, Base}, {req, Req}}),
    maybe
        {ok, [PrimaryMsg|Rest]} ?= hb_maps:find(<<"body">>, Req, Opts),
        {ok, Loaded} ?= load(PrimaryMsg, Opts),
        ?event(debug_manifest, {loaded, Loaded}),
        % Must handle three cases:
        % 1. The maybe_cast is not a manifest, so we return the *loaded* request,
        %    such that the work to load it is not wasted.
        % 2. The maybe_cast is a manifest, and there are no other elements of
        %    the path, so we add the `index' path and return.
        % 3. The maybe_cast is a manifest, and there are other elements of
        %    the path, so we return the original request sequence with the first
        %    message replaced with the casted manifest.
        case {Rest, maybe_cast_manifest(Loaded, Opts)} of
            {_, ignored} ->
                ?event(
                    debug_manifest,
                    {non_manifest_returning_loaded, {loaded, Loaded}, {rest, Rest}}),
                {ok, Req#{ <<"body">> => [Loaded|Rest] }};
            {[], {ok, Casted}} ->
                ?event(debug_manifest, {manifest_returning_index, {req, Req}}),
                {ok, Req#{ <<"body">> => [Casted, #{<<"path">> => <<"index">>}] }};
            {_, {ok, Casted}} ->
                ?event(debug_manifest, {manifest_returning_subpath, {req, Req}}),
                {ok, Req#{ <<"body">> => [Casted|Rest] }}
        end
    else
        {error, not_found} ->
            ?event(debug_manifest, {not_found_on_load, {req, Req}}),
            {
                error,
                #{
                    <<"status">> => 404,
                    <<"body">> => <<"Not Found">>
                }
            };
        Error ->
            ?event(debug_manifest, {request_ignored, {unexpected, Error}}),
            % On other errors, we return the original request.
            {ok, Req}
    end.

%% @doc Cast a message to `manifest@1.0` if it has the correct content-type but
%% no other device is specified.
load(Msg, _Opts) when is_map(Msg) -> {ok, Msg};
load(List, _Opts) when is_list(List) -> skip;
load({as, _, _}, _Opts) -> skip;
load(ID, Opts) when ?IS_ID(ID) ->
    case hb_cache:read(ID, Opts) of
        {ok, Msg} -> load(Msg, Opts);
        _ ->
            ?event(debug_maybe_cast_manifest, {message_load_failed, {id, ID}}),
            {error, not_found}
    end;
load(Msg, Opts) when ?IS_LINK(Msg) ->
    try load(hb_cache:ensure_loaded(Msg, Opts), Opts)
    catch
        _ ->
            ?event(debug_maybe_cast_manifest, {message_load_failed, {link, Msg}}),
            {error, not_found}
    end.

maybe_cast_manifest(Msg, Opts) ->
    case hb_maps:find(<<"device">>, Msg, Opts) of
        {ok, X} when X == <<"manifest@1.0">> -> {ok, Msg};
        _ ->
            case hb_maps:find(<<"content-type">>, Msg, Opts) of
                {ok, <<"application/x.arweave-manifest", _/binary>>} ->
                    ?event(debug_maybe_cast_manifest, {manifest_casting, {msg, Msg}}),
                    {ok, {as, <<"manifest@1.0">>, Msg}};
                _IgnoredContentType ->
                    ignored
            end
    end.

%% @doc Find and deserialize a manifest from the given base, returning a 
%% message with the `~manifest@1.0' device.
manifest(Base, _Req, Opts) ->
    JSON =
        hb_maps:get_first(
            [
                {Base, <<"data">>},
                {Base, <<"body">>}
            ],
            not_found,
            Opts
        ),
    FlatManifest = #{ <<"paths">> := FlatPaths } = hb_json:decode(JSON),
    {ok, DeepPaths} = dev_codec_flat:from(FlatPaths, #{}, Opts),
    LinkifiedPaths = linkify(DeepPaths, Opts),
    Structured = FlatManifest#{ <<"paths">> => LinkifiedPaths },
    {ok, Structured#{ <<"device">> => <<"manifest@1.0">> }}.

%% @doc Generate a nested message of links to content from a parsed (and
%% structured) manifest.
linkify(#{ <<"id">> := ID }, Opts) when is_binary(ID) ->
    LinkOptsBase = (maps:with([store], Opts))#{ scope => [local, remote]},
    {link, ID, LinkOptsBase#{ <<"type">> => <<"link">>, <<"lazy">> => false }};
linkify(Manifest, Opts) when is_map(Manifest) ->
    hb_maps:map(
        fun(_Key, Val) -> linkify(Val, Opts) end,
        Manifest,
        Opts
    );
linkify(Manifest, Opts) when is_list(Manifest) ->
    lists:map(
        fun(Item) -> linkify(Item, Opts) end,
        Manifest
    );
linkify(Manifest, _Opts) ->
    Manifest.

%%% Tests

resolve_test() ->
    Opts = #{
        store => hb_opts:get(store, no_viable_store, #{}),
        on => #{
            <<"request">> => #{
                <<"device">> => <<"manifest@1.0">>
            }
        }
    },
    IndexPage = #{
        <<"content-type">> => <<"text/html">>,
        <<"body">> => <<"Page 1">>
    },
    {ok, IndexID} = hb_cache:write(IndexPage, Opts),
    Page2 = #{
        <<"content-type">> => <<"text/html">>,
        <<"body">> => <<"Page 2">>
    },
    {ok, Page2ID} = hb_cache:write(Page2, Opts),
    Manifest = #{
        <<"paths">> => #{
            <<"nested">> => #{ <<"page2">> => #{ <<"id">> => Page2ID } },
            <<"page1">> => #{ <<"id">> => IndexID }
        },
        <<"index">> => #{ <<"path">> => <<"page1">> }
    },
    JSON = hb_json:encode(Manifest),
    ManifestMsg =
        #{
            <<"device">> => <<"manifest@1.0">>,
            <<"body">> => JSON
        },
    LegacyManifestWithCT =
        #{
            <<"content-type">> => <<"application/x.arweave-manifest+json">>,
            <<"body">> => JSON
        },
    {ok, ManifestID} = hb_cache:write(ManifestMsg, Opts),
    {ok, LegacyManifestID} = hb_cache:write(LegacyManifestWithCT, Opts),
    ?event({manifest_id, ManifestID}),
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, #{ <<"body">> := <<"Page 1">> }},
        hb_http:get(Node, << ManifestID/binary, "/index" >>, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"body">> := <<"Page 2">>}}, 
        hb_http:get(Node, << ManifestID/binary, "/nested/page2" >>, Opts)),
    % Making the same requests to a node with the `request' hook enabled should
    % yield the same results.
    ?event({legacy_manifest_id, LegacyManifestID}),
    ?assertMatch(
        {ok, #{ <<"body">> := <<"Page 1">> }},
        hb_http:get(Node, << LegacyManifestID/binary, "/index" >>, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"body">> := <<"Page 2">>}}, 
        hb_http:get(Node, << LegacyManifestID/binary, "/nested/page2" >>, Opts)),
    ok.

manifest_default_fallback_test() ->
    Opts = #{ store => hb_opts:get(store, no_viable_store, #{}) },
    {ok, ManifestID} = create_generic_manifest(Opts),
    ?event({manifest_id, ManifestID}),
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, #{ <<"body">> := <<"Page 1">> }},
        hb_http:get(Node, << ManifestID/binary, "/invalid_path" >>, Opts)
    ),
    ok.

manifest_404_error_test() ->
    Opts = #{
        store => hb_opts:get(store, no_viable_store, #{}),
        manifest_404 => error
    },
    {ok, ManifestID} = create_generic_manifest(Opts),
    ?event({manifest_id, ManifestID}),
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {error, not_found},
        hb_http:get(Node, << ManifestID/binary, "/invalid_path" >>, Opts)
    ),
    ok.

create_generic_manifest(Opts) ->
    IndexPage = #{
        <<"content-type">> => <<"text/html">>,
        <<"body">> => <<"Page 1">>
    },
    {ok, IndexID} = hb_cache:write(IndexPage, Opts),
    Manifest = #{
        <<"paths">> => #{
            <<"page1">> => #{ <<"id">> => IndexID }
        },
        <<"index">> => #{ <<"path">> => <<"page1">> }
    },
    JSON = hb_json:encode(Manifest),
    ManifestMsg =
        #{
            <<"device">> => <<"manifest@1.0">>,
            <<"body">> => JSON
        },
    hb_cache:write(ManifestMsg, Opts).

%% @doc Download the manifest raw data. 
%% NOTE: This test requests data to arweave node
manifest_download_via_raw_endpoint_test_ignore() ->
    Opts = #{
        arweave_index_ids => true,
        store => [
        #{
            <<"store-module">> => hb_store_arweave,
            <<"name">> => <<"arweave-store">>,
            <<"arweave-node">> => <<"https://arweave.net">>,
            <<"index-store">> => [hb_test_utils:test_store()]
        }
    ]},
    Node = hb_http_server:start_node(Opts),
    %% Force index the block that includes the manifest transaction
    _ = hb_http:get(
            Node,
            #{
                <<"path">> =>
                    <<"~copycat@1.0/arweave/?from+integer=1809222&to+integer=1809222">>
            },
            #{}
        ),
    ?assertMatch(
        {ok,
            #{
                <<"arweave-id">> := <<"42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA">>,
                <<"content-length">> := 5868
            }
        },
        hb_http:get(
            Node,
            #{<<"path">> => <<"~arweave@2.9/raw=42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA">>},
            #{}
        )
    ).

%% @doc Accessing `/TXID` of a manifest transaction should access the index key.
manifest_inner_redirect_test() ->
    %% Define the store
    LmdbStore = hb_test_utils:test_store(),
    %% Load transaction information to the store
    load_and_store(LmdbStore, <<"42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA.bin">>),
    load_and_store(LmdbStore, <<"index-Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM.bin">>),
    %% Start node
    Opts = #{store => LmdbStore},
    Node = hb_http_server:start_node(Opts),
    %% Request manifest to node.
    ?assertMatch(
        {ok, #{<<"commitments">> := #{<<"Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM">> := _ }}},
        hb_http:get(
            Node,
            #{<<"path">> => <<"/42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA">>},
            Opts
        )
    ).

%% @doc Accessing `/TXID/assets/ArticleBlock-Dtwjc54T.js` should return valid message.
access_key_path_in_manifest_test() ->
    LmdbStore = hb_test_utils:test_store(),
    load_and_store(LmdbStore, <<"42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA.bin">>),
    load_and_store(LmdbStore, <<"index-Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM.bin">>),
    load_and_store(LmdbStore, <<"item-oLnQY-EgiYRg9XyO7yZ_mC0Ehy7TFR3UiDhFvxcohC4.bin">>),
    Opts = #{store => LmdbStore},
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, #{<<"commitments">> := #{<<"oLnQY-EgiYRg9XyO7yZ_mC0Ehy7TFR3UiDhFvxcohC4">> := _ }}},
        hb_http:get(
            Node,
            #{<<"path">> => <<"/42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA/assets/ArticleBlock-Dtwjc54T.js">>},
            Opts
        )
    ).

%% This works with `not_found.js` but doesn't follow the logic if under a 
%% folder structure, like `assets/not_found.js .
manifest_should_fallback_on_not_found_path_test() ->
    LmdbStore = hb_test_utils:test_store(),
    load_and_store(LmdbStore, <<"42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA.bin">>),
    load_and_store(LmdbStore, <<"index-Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM.bin">>),
    Opts = #{store => LmdbStore},
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, #{<<"commitments">> := #{<<"Tqh6oIS2CLUaDY11YUENlvvHmDim1q16pMyXAeSKsFM">> := _ }}},
        hb_http:get(
            Node,
            #{<<"path">> => <<"/42jky7O3rzKkMOfHBXgK-304YjulzEYqHc9qyjT3efA/x.js">>},
            Opts
        )
    ).

%% @doc Load ans104 binary files to a store.
load_and_store(LmdbStore, File) ->
    Opts = #{},
    {ok, SerializedItem} =
        file:read_file(
            hb_util:bin(
                <<"test/arbundles.js/ans-104-manifest-", File/binary>>
            )
        ),
    Message = hb_message:convert(
        ar_bundles:deserialize(SerializedItem),
        <<"structured@1.0">>,
        <<"ans104@1.0">>,
        Opts
    ),
    _ = hb_cache:write(Message, #{store => LmdbStore}).