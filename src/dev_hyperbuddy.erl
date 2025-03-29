%%% @doc A device that renders a REPL-like interface for AO-Core via HTML.
-module(dev_hyperbuddy).
-export([index/3, console/3, format/3, metrics/3]).
-include_lib("include/hb.hrl").

%% @doc Render the dashboard for a node as a HTML page.
index(_, _, _Opts) ->
    return_file("index").

%% @doc The main HTML page for the REPL device.
console(_, _, _Opts) ->
    return_file("console").

%% @doc The main HTML page for the REPL device.
metrics(_, Req, Opts) ->
    {_, HeaderList, Body} =
        prometheus_http_impl:reply(
            #{path => true,
            headers => 
                fun(Name, Default) ->
                    hb_converge:get(Name, Req, Default, Opts)
                end,
            registry => prometheus_registry:exists(<<"default">>),
            standalone => false}
        ),
    RawHeaderMap = maps:from_list(prometheus_cowboy:to_cowboy_headers(HeaderList)),
    Headers = maps:map(fun(_, Value) -> hb_util:bin(Value) end, RawHeaderMap),
    {ok, Headers#{ <<"body">> => Body }}.

format(Base, _, _) ->
    {ok, #{ <<"body">> => hb_util:bin(hb_message:format(Base)) }}.

return_file(Name) ->
    {ok, Body} = file:read_file(code:priv_dir(hb) ++ "/html/hyperbuddy@1.0/" ++ Name ++ ".html"),
    {ok, #{
        <<"body">> => Body,
        <<"content-type">> => <<"text/html">>
    }}.