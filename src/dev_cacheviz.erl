-module(dev_cacheviz).
-moduledoc """
A device that generates renders (or renderable dot output) of a node's
cache.
""".
-export([dot/3, svg/3]).
-include("include/hb.hrl").

-doc """
Output the dot representation of the cache, or a specific path within
the cache set by the `target` key in the request.
""".
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

-doc """
Output the SVG representation of the cache, or a specific path within
the cache set by the `target` key in the request.
""".
svg(Base, Req, Opts) ->
    {ok, #{ <<"body">> := Dot }} = dot(Base, Req, Opts),
    ?event(cacheviz, {dot, Dot}),
    Svg = hb_cache_render:dot_to_svg(Dot),
    {ok, #{ <<"content-type">> => <<"image/svg+xml">>, <<"body">> => Svg }}.
