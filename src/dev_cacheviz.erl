%%% @doc A device that generates renders (or renderable dot output) of a node's
%%% cache.
-module(dev_cacheviz).
-export([dot/3, svg/3, json/3, index/3, js/3]).
-include("include/hb.hrl").


%% @doc Output the dot representation of the cache, or a specific path within
%% the cache set by the `target' key in the request.
dot(_, Req, Opts) ->
    Target = hb_ao:get(<<"target">>, Req, all, Opts),
    Dot =
        hb_cache_render:cache_path_to_dot(
          Target,
          #{
            render_data =>
                hb_util:atom(
                  hb_ao:get(<<"render-data">>, Req, false, Opts))
           },
          Opts),
    {ok, #{<<"content-type">> => <<"text/vnd.graphviz">>, <<"body">> => Dot}}.


%% @doc Output the SVG representation of the cache, or a specific path within
%% the cache set by the `target' key in the request.
svg(Base, Req, Opts) ->
    {ok, #{<<"body">> := Dot}} = dot(Base, Req, Opts),
    ?event(cacheviz, {dot, Dot}),
    Svg = hb_cache_render:dot_to_svg(Dot),
    {ok, #{<<"content-type">> => <<"image/svg+xml">>, <<"body">> => Svg}}.


%% @doc Return a JSON representation of the cache graph, suitable for use with
%% the `graph.js' library. If the request specifies a `target' key, we use that
%% target. Otherwise, we generate a new target by writing the message to the
%% cache and using the ID of the written message.
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


%% @doc Return a renderer in HTML form for the JSON format.
index(Base, _, _Opts) ->
    ?event({cacheviz_index, {base, Base}}),
    dev_hyperbuddy:return_file(<<"cacheviz@1.0">>, <<"graph.html">>).


%% @doc Return a JS library that can be used to render the JSON format.
js(_, _, _Opts) ->
    dev_hyperbuddy:return_file(<<"cacheviz@1.0">>, <<"graph.js">>).
