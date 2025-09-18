# dev_manifest

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_manifest.erl)

An Arweave path manifest resolution device. Follows the v1 schema:
https://specs.ar.io/?tx=lXLd0OPwo-dJLB_Amz5jgIeDhiOkjXuM3-r0H_aiNj0

---

## Exported Functions

- `index/3`
- `info/0`

---

### info

An Arweave path manifest resolution device. Follows the v1 schema:
Use the `route/4` function as the handler for all requests, aside 

```erlang
info() ->
    #{
        default => fun route/4,
        excludes => [keys, set, committers]
    }.
```

### index

Return the fallback index page when the manifest itself is requested.

```erlang
index(M1, M2, Opts) ->
    ?event({manifest_index_request, M1, M2}),
    case route(<<"index">>, M1, M2, Opts) of
        {ok, Index} ->
            ?event({manifest_index_returned, Index}),
            {ok, Index};
        {error, not_found} ->
            {error, not_found}
    end.
```

### route

Route a request to the associated data via its manifest.

```erlang
route(<<"index">>, M1, M2, Opts) ->
    ?event({manifest_index, M1, M2}),
    case manifest(M1, M2, Opts) of
        {ok, JSONStruct} ->
            ?event({manifest_json_struct, JSONStruct}),
            % Get the path to the index page from the manifest. We make
            % sure to use `hb_maps:get/4' to ensure that we do not recurse
            % on the `index' key with an `ao' resolve.
```

### route

```erlang
route(Key, M1, M2, Opts) ->
    ?event({manifest_lookup, Key}),
    {ok, Manifest} = manifest(M1, M2, Opts),
    {ok,
        hb_ao:get(
            <<"paths/", Key/binary>>,
            {as, <<"message@1.0">>, Manifest},
            Opts
        )
    }.
```

### manifest

Find and deserialize a manifest from the given base.

```erlang
manifest(Base, _Req, Opts) ->
    JSON =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Base}, [<<"data">>]},
                {{as, <<"message@1.0">>, Base}, [<<"body">>]}
            ],
            Opts
        ),
    ?event({manifest_json, JSON}),
    Structured = 
        hb_cache:ensure_all_loaded(
            hb_message:convert(JSON, <<"structured@1.0">>, <<"json@1.0">>, Opts),
            Opts
        ),
    ?event({manifest_structured, {explicit, Structured}}),
    Linkified = linkify(Structured, Opts),
    ?event({manifest_linkified, {explicit, Linkified}}),
    {ok, Linkified}.
```

### linkify

Generate a nested message of links to content from a parsed (and

```erlang
linkify(#{ <<"id">> := ID }, Opts) ->
    LinkOptsBase = (maps:with([store], Opts))#{ scope => [local, remote]},
    {link, ID, LinkOptsBase#{ <<"type">> => <<"link">>, <<"lazy">> => false }};
```

### linkify

Generate a nested message of links to content from a parsed (and

```erlang
linkify(Manifest, Opts) when is_map(Manifest) ->
    hb_maps:map(
        fun(_Key, Val) -> linkify(Val, Opts) end,
        Manifest,
        Opts
    );
```

### linkify

Generate a nested message of links to content from a parsed (and

```erlang
linkify(Manifest, Opts) when is_list(Manifest) ->
    lists:map(
        fun(Item) -> linkify(Item, Opts) end,
        Manifest
    );
```

### linkify

Generate a nested message of links to content from a parsed (and

```erlang
linkify(Manifest, _Opts) ->
    Manifest.
```

### resolve_test

```erlang
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
    JSON = hb_message:convert(Manifest, <<"json@1.0">>, <<"structured@1.0">>, Opts),
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
    {ok, Res} = hb_http:get(Node, << ManifestID/binary, "/nested/page2" >>, Opts),
    ?event({manifest_resolve_test, Res}),
    ?assertEqual(<<"Page 2">>, hb_maps:get(<<"body">>, Res, <<"NO BODY">>, Opts)),
```

---

*Generated from [dev_manifest.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_manifest.erl)*
