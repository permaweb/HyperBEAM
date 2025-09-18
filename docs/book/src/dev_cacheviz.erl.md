# dev_cacheviz

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_cacheviz.erl)

A device that generates renders (or renderable dot output) of a node's
cache.

---

## Exported Functions

- `dot/3`
- `index/3`
- `js/3`
- `json/3`
- `svg/3`

---

### dot

A device that generates renders (or renderable dot output) of a node's
Output the dot representation of the cache, or a specific path within
Output the SVG representation of the cache, or a specific path within

```erlang
dot(_, Req, Opts) ->
    Target = hb_ao:get(<<"target">>, Req, all, Opts),
    Dot =
        hb_cache_render:cache_path_to_dot(
            Target,
            #{
                render_data =>
                    hb_util:atom(
                        hb_ao:get(<<"render-data">>, Req, false, Opts)
                    )
            },
            Opts
        ),
    {ok, #{ <<"content-type">> => <<"text/vnd.graphviz">>, <<"body">> => Dot }}.
```

### svg

A device that generates renders (or renderable dot output) of a node's
Output the dot representation of the cache, or a specific path within
Output the SVG representation of the cache, or a specific path within
Return a JSON representation of the cache graph, suitable for use with

```erlang
svg(Base, Req, Opts) ->
    {ok, #{ <<"body">> := Dot }} = dot(Base, Req, Opts),
    ?event(cacheviz, {dot, Dot}),
    Svg = hb_cache_render:dot_to_svg(Dot),
    {ok, #{ <<"content-type">> => <<"image/svg+xml">>, <<"body">> => Svg }}.
```

### json

A device that generates renders (or renderable dot output) of a node's
Output the dot representation of the cache, or a specific path within
Output the SVG representation of the cache, or a specific path within
Return a JSON representation of the cache graph, suitable for use with

```erlang
json(Base, Req, Opts) ->
    ?event({json, {base, Base}, {req, Req}}),
    Target =
        case hb_ao:get(<<"target">>, Req, Opts) of
            not_found -> 
                case map_size(maps:without([<<"device">>], hb_private:reset(Base))) of
                    0 ->
                        all;
                    _ ->
                        ?event({writing_base_for_rendering, Base}),
                        {ok, Path} = hb_cache:write(Base, Opts),
                        ?event({wrote_message, Path}),
                        ID = hb_message:id(Base, all, Opts),
                        ?event({generated_id, ID}),
                        ID
                end;
            <<".">> -> all;
            ReqTarget -> ReqTarget
        end,
    MaxSize = hb_util:int(hb_ao:get(<<"max-size">>, Req, 250, Opts)),
    ?event({max_size, MaxSize}),
    ?event({generating_json_for, {target, Target}}),
    Res = hb_cache_render:get_graph_data(Target, MaxSize, Opts),
    ?event({graph_data, Res}),
    Res.
```

### index

Return a renderer in HTML form for the JSON format.
Return a JS library that can be used to render the JSON format.

```erlang
index(Base, _, _Opts) ->
    ?event({cacheviz_index, {base, Base}}),
    dev_hyperbuddy:return_file(<<"cacheviz@1.0">>, <<"graph.html">>).
```

### js

Return a renderer in HTML form for the JSON format.
Return a JS library that can be used to render the JSON format.

```erlang
js(_, _, _Opts) ->
```

---

*Generated from [dev_cacheviz.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_cacheviz.erl)*
