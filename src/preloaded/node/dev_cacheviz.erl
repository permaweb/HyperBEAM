%%% @doc A device that generates renders (or renderable dot output) of a node's
%%% cache.
-module(dev_cacheviz).
-export([dot/3, svg/3, json/3, index/3, js/3]).
-include("include/hb.hrl").

%% @doc Output the dot representation of the cache, or a specific path within
%% the cache set by the `target' key in the request.
-spec dot(_, #{ target => binary(), 'render-data' => boolean(), _ => _ }, _) -> _.
dot(_, Req, Opts) ->
    Target = maps:get(<<"target">>, Req, all),
    Dot =
        hb_cache_render:cache_path_to_dot(
            Target,
            #{
                render_data => maps:get(<<"render-data">>, Req, false)
            },
            Opts
        ),
    {ok, #{ <<"content-type">> => <<"text/vnd.graphviz">>, <<"body">> => Dot }}.

%% @doc Output the SVG representation of the cache, or a specific path within
%% the cache set by the `target' key in the request.
-spec svg(_, _, _) -> _.
svg(Base, Req, Opts) ->
    {ok, #{ <<"body">> := Dot }} = dot(Base, Req, Opts),
    ?event(cacheviz, {dot, Dot}),
    Svg = hb_cache_render:dot_to_svg(Dot),
    {ok, #{ <<"content-type">> => <<"image/svg+xml">>, <<"body">> => Svg }}.

%% @doc Return a JSON representation of the cache graph, suitable for use with
%% the `graph.js' library. If the request specifies a `target' key, we use that
%% target. Otherwise, we generate a new target by writing the message to the
%% cache and using the ID of the written message.
-spec json(_, #{ target => binary(), 'max-size' => integer(), _ => _ }, _) -> _.
json(Base, Req, Opts) ->
    ?event({json, {base, Base}, {req, Req}}),
    Target =
        case maps:get(<<"target">>, Req, not_found) of
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
    MaxSize = maps:get(<<"max-size">>, Req, 250),
    ?event({max_size, MaxSize}),
    ?event({generating_json_for, {target, Target}}),
    Res = hb_cache_render:get_graph_data(Target, MaxSize, Opts),
    ?event({graph_data, Res}),
    Res.

%% @doc Return a renderer in HTML form for the JSON format.
-spec index(_, _, _) -> _.
index(Base, _, _Opts) ->
    ?event({cacheviz_index, {base, Base}}),
    hb_http_server:static(<<"cacheviz@1.0">>, <<"graph.html">>, Opts).

%% @doc Return a JS library that can be used to render the JSON format.
-spec js(_, _, _) -> _.
js(_, _, _Opts) ->
    dev_hyperbuddy:return_file(<<"cacheviz@1.0">>, <<"graph.js">>).
