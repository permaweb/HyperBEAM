%%% @doc An Arweave path manifest resolution device. Follows the v1 schema:
%%% https://specs.ar.io/?tx=lXLd0OPwo-dJLB_Amz5jgIeDhiOkjXuM3-r0H_aiNj0
-module(dev_manifest).
-export([info/0]).
-include("include/hb.hrl").

-include_lib("eunit/include/eunit.hrl").


%% @doc Use the `route/4' function as the handler for all requests, aside
%% from `keys' and `set', which are handled by the default resolver.
info() ->
    #{
      default => fun route/4,
      excludes => [keys, set, committers]
     }.


%% @doc Route a request to the associated data via its manifest.
route(<<"index">>, M1, M2, Opts) ->
    ?event({manifest_index, M1, M2}),
    case manifest(M1, M2, Opts) of
        {ok, JSONStruct} ->
            ?event({manifest_json_struct, JSONStruct}),
            % Get the path to the index page from the manifest. We make
            % sure to use `hb_maps:get/4' to ensure that we do not recurse
            % on the `index' key with an `ao' resolve.
            Index =
                hb_maps:get(
                  <<"index">>,
                  JSONStruct,
                  #{},
                  Opts),
            ?event({manifest_index_found, Index}),
            Path = hb_ao:get(<<"path">>, {as, <<"message@1.0">>, Index}, Opts),
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
route(Key, M1, M2, Opts) ->
    ?event({manifest_lookup, Key}),
    {ok, JSONStruct} = manifest(M1, M2, Opts),
    ?event({manifest_json_struct, JSONStruct}),
    case hb_ao:resolve(JSONStruct, [<<"paths">>, Key], Opts) of
        {ok, Entry} ->
            ?event({manifest_entry, Entry}),
            ID = maps:get(<<"id">>, Entry),
            ?event({manifest_serving, ID}),
            case hb_cache:read(ID, Opts) of
                {ok, Data} ->
                    ?event({manifest_data, Data}),
                    {ok, Data};
                {error, not_found} ->
                    Fallback = hb_ao:get(JSONStruct, <<"fallback">>, Opts),
                    FallbackID = maps:get(<<"id">>, Fallback),
                    ?event({manifest_serving_fallback, FallbackID}),
                    hb_cache:read(FallbackID, Opts)
            end;
        _ ->
            ?event({manifest_not_found, Key}),
            {error, not_found}
    end.


%% @doc Find and deserialize a manifest from the given base.
manifest(Base, _Req, Opts) ->
    JSON =
        hb_ao:get_first(
          [{{as, <<"message@1.0">>, Base}, [<<"data">>]},
           {{as, <<"message@1.0">>, Base}, [<<"body">>]}],
          Opts),
    ?event({manifest_json, JSON}),
    hb_ao:resolve(
      #{<<"device">> => <<"json@1.0">>, <<"body">> => JSON},
      <<"deserialize">>,
      Opts).


%%% Tests


resolve_test() ->
    IndexPage = #{
                  <<"content-type">> => <<"text/html">>,
                  <<"body">> =>
                      <<"""
                <html>
                    <body>
                        <h1>Hello, World!</h1>
                        <a href="page2">Click me</a>
                    </body>
                </html>
                """>>
                 },
    {ok, IndexID} = hb_cache:write(IndexPage, #{}),
    Page2 = #{
              <<"content-type">> => <<"text/html">>,
              <<"body">> =>
                  <<"""
                <html>
                    <body>
                        <h1>Page 2</h1>
                    </body>
                </html>
                """>>
             },
    {ok, Page2ID} = hb_cache:write(Page2, #{}),
    Manifest = #{
                 <<"paths">> => #{
                                  <<"page2">> => #{<<"id">> => Page2ID},
                                  <<"page1">> => #{<<"id">> => IndexID}
                                 },
                 <<"index">> => #{<<"path">> => <<"page1">>}
                },
    JSON = hb_message:convert(Manifest, <<"json@1.0">>, #{}),
    ManifestMsg =
        #{
          <<"device">> => <<"manifest@1.0">>,
          <<"body">> => JSON
         },
    {ok, ManifestID} = hb_cache:write(ManifestMsg, #{}),
    ?event({manifest_id, ManifestID}).
