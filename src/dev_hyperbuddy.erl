%%% @doc A device that renders a REPL-like interface for AO-Core via HTML.
-module(dev_hyperbuddy).
-export([info/0, format/3, metrics/3]).
-include_lib("include/hb.hrl").

%% @doc Export an explicit list of files via http.
info() ->
    #{
        default => fun serve/4,
        routes => #{
            <<"index">> => <<"index.html">>,
            <<"console">> => <<"console.html">>,
            <<"graph">> => <<"graph.html">>,
			<<"styles.css">> => <<"styles.css">>,
			<<"metrics.js">> => <<"metrics.js">>,
			<<"devices.js">> => <<"devices.js">>,
			<<"utils.js">> => <<"utils.js">>,
			<<"main.js">> => <<"main.js">>,
			<<"graph.js">> => <<"graph.js">>
        }
    }.

%% @doc The main HTML page for the REPL device.
metrics(_, Req, Opts) ->
    case hb_opts:get(prometheus, not hb_features:test(), Opts) of
        true ->
            {_, HeaderList, Body} =
            prometheus_http_impl:reply(
                #{path => true,
                headers => 
                    fun(Name, Default) ->
                        hb_ao:get(Name, Req, Default, Opts)
                    end,
                registry => prometheus_registry:exists(<<"default">>),
                standalone => false}
            ),
            RawHeaderMap = maps:from_list(prometheus_cowboy:to_cowboy_headers(HeaderList)),
            Headers = maps:map(fun(_, Value) -> hb_util:bin(Value) end, RawHeaderMap),
            {ok, Headers#{ <<"body">> => Body }};
        false ->
            {ok, #{ <<"body">> => <<"Prometheus metrics disabled.">> }}
    end.

%% @doc Employ HyperBEAM's internal pretty printer to format a message.
format(Base, _, _) ->
    {ok, #{ <<"body">> => hb_util:bin(hb_message:format(Base)) }}.

%% @doc Serve a file from the priv directory. Only serves files that are explicitly
%% listed in the `routes' field of the `info/0' return value.
serve(<<"keys">>, M1, _M2, _Opts) -> dev_message:keys(M1);
serve(<<"set">>, M1, M2, Opts) -> dev_message:set(M1, M2, Opts);
serve(<<"graph-data">>, _, _, Opts) -> hb_cache_render:get_graph_data(Opts);
serve(Key, _, _, _) ->
    ?event({hyperbuddy_serving, Key}),
    case maps:get(Key, maps:get(routes, info(), no_routes), undefined) of
        undefined -> {error, not_found};
        Filename -> return_file(Filename)
    end.

%% @doc Read a file from disk and serve it as a static HTML page.
return_file(Name) ->
    Base = hb_util:bin(code:priv_dir(hb)),
    Filename = <<Base/binary, "/html/hyperbuddy@1.0/", Name/binary >>,
    ?event({hyperbuddy_serving, Filename}),
    {ok, Body} = file:read_file(Filename),
    {ok, #{
        <<"body">> => Body,
        <<"content-type">> =>
            case filename:extension(Filename) of
                <<".html">> -> <<"text/html">>;
                <<".js">> -> <<"text/javascript">>;
                <<".css">> -> <<"text/css">>;
                <<".png">> -> <<"image/png">>;
                <<".ico">> -> <<"image/x-icon">>
            end
    }}.