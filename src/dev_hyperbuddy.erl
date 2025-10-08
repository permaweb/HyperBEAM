%%% @doc A device that renders a REPL-like interface for AO-Core via HTML.
-module(dev_hyperbuddy).
-export([info/0, format/3, return_file/2, return_error/2]).
-export([metrics/3, events/3]).
-export([throw/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Export an explicit list of files via http.
info() ->
    #{
        default => fun serve/4,
        routes => #{
            % Default message viewer page:
            <<"index">> => <<"index.html">>,
            % HyperBEAM default homepage:
            <<"dashboard">> => <<"dashboard.html">>,
            % Interactive REPL:
            <<"console">> => <<"console.html">>,
            <<"graph">> => <<"graph.html">>,
            % Styling and scripts:
			<<"styles.css">> => <<"styles.css">>,
			<<"metrics.js">> => <<"metrics.js">>,
			<<"devices.js">> => <<"devices.js">>,
			<<"utils.js">> => <<"utils.js">>,
			<<"dashboard.js">> => <<"dashboard.js">>,
			<<"graph.js">> => <<"graph.js">>,
            <<"404.html">> => <<"404.html">>
        },
        excludes => [<<"return_file">>]
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
            RawHeaderMap =
                hb_maps:from_list(
                    prometheus_cowboy:to_cowboy_headers(HeaderList)
                ),
            Headers =
                hb_maps:map(
                    fun(_, Value) -> hb_util:bin(Value) end,
                    RawHeaderMap,
					Opts
                ),
            {ok, Headers#{ <<"body">> => Body }};
        false ->
            {ok, #{ <<"body">> => <<"Prometheus metrics disabled.">> }}
    end.

%% @doc Return the current event counters as a message.
events(_, _Req, _Opts) ->
    {ok, hb_event:counters()}.

%% @doc Employ HyperBEAM's internal pretty printer to format a message.
%% 
%% The request and node message can also be printed if desired by changing the
%% `format` key in the `format` call. This can be achieved easily using the
%% default key semantics:
%% ```
%% GET /.../~hyperbuddy@1.0/format=request
%% ```
%% Or a list of environment components:
%% ```
%% GET /.../~hyperbuddy@1.0/format+list=request,node
%% ```
%% Valid components are `base`, `request`, and `node`. The string `all` can also
%% be used to quickly include all of the components.
%% 
%% The `truncate-keys` key can also be used to truncate the number of keys
%% printed for each component. The default value is `infinity` (print all keys).
%% ```
%% GET /.../~hyperbuddy@1.0/format=request?truncate-keys=20
%% ```
format(Base, Req, Opts) ->
    % Find the scope of the environment that should be printed.
    Scope =
        lists:map(
            fun hb_util:bin/1,
            case hb_maps:get(<<"format">>, Req, <<"base">>, Opts) of
                <<"all">> -> [<<"base">>, <<"request">>, <<"node">>];
                Messages when is_list(Messages) -> Messages;
                SingleScope -> [SingleScope]
            end
        ),
    ?event(debug_format, {using_scope, Scope}),
    CombinedMsg =
        hb_maps:with(
            Scope,
            #{
                <<"base">> => maps:without([<<"device">>], hb_private:reset(Base)),
                <<"request">> => maps:without([<<"path">>], hb_private:reset(Req)),
                <<"node">> => hb_private:reset(Opts)
            },
            Opts
        ),
    MsgBeforeLoad =
        if map_size(CombinedMsg) == 1 ->
            hb_maps:get(hd(maps:keys(CombinedMsg)), CombinedMsg, #{}, Opts);
        true ->
            CombinedMsg
        end,
    MsgLoaded = hb_cache:ensure_all_loaded(MsgBeforeLoad, Opts),
    TruncateKeys =
        hb_maps:get(
            <<"truncate-keys">>,
            Req,
            hb_opts:get(debug_print_truncate, infinity, Opts),
            Opts
        ),
    ?event(debug_format, {using_truncation, TruncateKeys}),
    {ok,
        #{
            <<"body">> =>
                hb_util:bin(
                    hb_format:message(
                        MsgLoaded,
                        Opts#{
                            linkify_mode => discard,
                            cache_control => [<<"no-cache">>, <<"no-store">>],
                            debug_print_truncate => TruncateKeys
                        }
                    )
                )
        }
    }.

%% @doc Test key for validating the behavior of the `500` HTTP response.
throw(_Msg, _Req, Opts) ->
    case hb_opts:get(mode, prod, Opts) of
        prod -> {error, <<"Forced-throw unavailable in `prod` mode.">>};
        debug -> throw({intentional_error, Opts})
    end.

%% @doc Serve a file from the priv directory. Only serves files that are explicitly
%% listed in the `routes' field of the `info/0' return value.
serve(<<"keys">>, M1, _M2, Opts) -> dev_message:keys(M1, Opts);
serve(<<"set">>, M1, M2, Opts) -> dev_message:set(M1, M2, Opts);
serve(Key, _, _, Opts) ->
    ?event({hyperbuddy_serving, Key}),
    Routes = hb_maps:get(routes, info(), no_routes, Opts),
    case hb_maps:get(Key, Routes, undefined, Opts) of
        undefined -> {error, not_found};
        Filename -> return_file(Filename)
    end.

%% @doc Read a file from disk and serve it as a static HTML page.
return_file(Name) ->
    return_file(<<"hyperbuddy@1.0">>, Name, #{}).
return_file(Device, Name) ->
    return_file(Device, Name, #{}).
return_file(Device, Name, Template) ->
    Base = hb_util:bin(code:priv_dir(hb)),
    Filename = <<Base/binary, "/html/", Device/binary, "/", Name/binary >>,
    ?event({hyperbuddy_serving, Filename}),
    case file:read_file(Filename) of
        {ok, RawBody} ->
            Body = apply_template(RawBody, Template),
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
                }
            };
        {error, _} ->
            {error, not_found}
    end.

%% @doc Return an error page, with the `{{error}}` template variable replaced.
return_error(Error, Opts) when not is_map(Error) ->
    return_error(#{ <<"body">> => Error }, Opts);
return_error(ErrorMsg, Opts) ->
    return_file(
        <<"hyperbuddy@1.0">>,
        <<"500.html">>,
        #{ <<"error">> => hb_format:error(ErrorMsg, Opts) }
    ).

%% @doc Apply a template to a body.
apply_template(Body, Template) when is_map(Template) ->
    apply_template(Body, maps:to_list(Template));
apply_template(Body, []) ->
    Body;
apply_template(Body, [{Key, Value} | Rest]) ->
    apply_template(
        re:replace(
            Body,
            <<"\\{\\{", Key/binary, "\\}\\}">>,
            hb_util:bin(Value),
            [global, {return, binary}]
        ),
        Rest
    ).

%%% Tests

return_templated_file_test() ->
    {ok, #{ <<"body">> := Body }} =
        return_file(
            <<"hyperbuddy@1.0">>,
            <<"500.html">>,
            #{
                <<"error">> => <<"This is an error message.">>
            }
        ),
    ?assertNotEqual(
        binary:match(Body, <<"This is an error message.">>),
        nomatch
    ).