%%% @doc An Arweave path manifest resolution device. Follows the v1 schema:
%%% https://specs.ar.io/?tx=lXLd0OPwo-dJLB_Amz5jgIeDhiOkjXuM3-r0H_aiNj0
-module(dev_manifest).
-export([index/3, info/0]).
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
    ?event(debug_manifest, {index_request, {m1, M1}, {m2, M2}}),
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
    ?event(debug_manifest, {manifest_lookup, {key, Key}, {m1, M1}, {m2, M2}}),
    {ok, Manifest} = manifest(M1, M2, Opts),
    Res = hb_ao:get(
        <<"paths/", Key/binary>>,
        {as, <<"message@1.0">>, Manifest},
        Opts
    ),
    {ok, Res}.

%% @doc Find and deserialize a manifest from the given base, returning a 
%% message with the `~manifest@1.0' device.
manifest(Base, _Req, Opts) ->
    JSON =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Base}, [<<"data">>]},
                {{as, <<"message@1.0">>, Base}, [<<"body">>]}
            ],
            Opts
        ),
    FlatManifest = #{ <<"paths">> := FlatPaths } = hb_json:decode(JSON),
    {ok, DeepPaths} = dev_codec_flat:from(FlatPaths, #{}, Opts),
    LinkifiedPaths = linkify(DeepPaths, Opts),
    Structured = FlatManifest#{ <<"paths">> => LinkifiedPaths },
    {ok, Structured#{ <<"device">> => <<"manifest@1.0">> }}.

%% @doc Generate a nested message of links to content from a parsed (and
%% structured) manifest.
linkify(#{ <<"id">> := ID }, Opts) ->
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
    Opts = #{ store => hb_opts:get(store, no_viable_store, #{}) },
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
    {ok, ManifestID} = hb_cache:write(ManifestMsg, Opts),
    ?event({manifest_id, ManifestID}),
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, #{ <<"body">> := <<"Page 1">> }},
        hb_http:get(Node, << ManifestID/binary, "/index" >>, Opts)
    ),
    ?assertMatch(
        {ok, #{ <<"body">> := <<"Page 2">>}}, 
        hb_http:get(Node, << ManifestID/binary, "/nested/page2" >>, Opts)),
    ok.