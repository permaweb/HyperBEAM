%%% @doc A device that renders a REPL-like interface for AO-Core via HTML.
-module(dev_hyperbuddy).
-export([info/1, format/3, return_file/2, return_error/2]).
-export([metrics/3, events/3]).
-export([throw/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Export an explicit list of files via http. Filenames added to the 
%% `hyperbuddy-serve' key of the node message will be served as static files.
%% Each filename must point to a path relative to the HyperBEAM instance's
%% build subdirectory as follows: `priv/html/hyperbuddy@1.0'.
info(Opts) ->
    ServedRoutes = hb_opts:get(<<"hyperbuddy-serve">>, #{}, Opts),
    #{
        default => fun serve/4,
        serve => ServedRoutes#{
            % Default message viewer page:
            <<"index">> => <<"index.html">>,
            <<"bundle.js">> => <<"bundle.js">>,
            <<"fonts.css">> => <<"fonts.css">>,
            <<"favicon-light.png">> => <<"favicon-light.png">>,
            <<"favicon-dark.png">> => <<"favicon-dark.png">>,
            <<"font-dm-sans-italic.ttf">> => <<"font-dm-sans-italic.ttf">>,
            <<"font-dm-sans-variable.ttf">> => <<"font-dm-sans-variable.ttf">>,
            <<"font-geist-mono-variable.ttf">> => <<"font-geist-mono-variable.ttf">>,
            % Error pages:
            <<"404.html">> => <<"404.html">>,
            <<"500.html">> => <<"500.html">>,
            <<"styles.css">> => <<"styles.css">>,
            <<"script.js">> => <<"script.js">>
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
        case hb_maps:get(<<"truncate-keys">>, Req, infinity, Opts) of
            infinity -> infinity;
            Value -> hb_util:int(Value)
        end,
    ?event(debug_format, {using_truncation, TruncateKeys}),
    {ok,
        #{
            <<"body">> =>
                hb_util:bin(
                    hb_format:message(
                        MsgLoaded,
                        Opts#{
                            <<"linkify-mode">> => discard,
                            <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
                            <<"debug-print-truncate">> => TruncateKeys
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
%% listed in the `routes' field of the `info/1' return value.
serve(<<"keys">>, M1, _M2, Opts) ->
    hb_ao:raw(<<"message@1.0">>, <<"keys">>, M1, #{}, Opts);
serve(<<"set">>, M1, M2, Opts) ->
    hb_ao:raw(<<"message@1.0">>, <<"set">>, M1, M2, Opts);
serve(Key, _, _, Opts) ->
    ?event({hyperbuddy_serving, Key}),
    ServeRoutes = hb_maps:get(serve, info(Opts), #{}, Opts),
    case hb_maps:find(Key, ServeRoutes, Opts) of
        {ok, Filename} -> return_file(<<"hyperbuddy@1.0">>, Filename, #{});
        error -> {error, not_found}
    end.

%% @doc Read a file from disk and serve it as a static HTML page.
return_file(Device, Name) ->
    return_file(Device, Name, #{}).
return_file(Device, Name, Template) ->
    hb_http_server:static(Device, Name, Template, #{}).

%% @doc Return an error page, with the `{{error}}` template variable replaced.
return_error(Error, Opts) when not is_map(Error) ->
    return_error(#{ <<"body">> => Error }, Opts);
return_error(ErrorMsg, Opts) ->
    return_file(
        <<"hyperbuddy@1.0">>,
        <<"500.html">>,
        #{ <<"error">> => hb_format:error(ErrorMsg, Opts) }
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

return_custom_json_test() ->
    Base = hb_util:bin(code:priv_dir(hb)),
    Filename = <<Base/binary, "/html/hyperbuddy@1.0/test.json">>,
    ok = file:write_file(Filename, <<"{\"status\":\"ok\"}">>),
    try
        ?assertMatch(
            {ok,
                #{
                    <<"body">> := JSONBin,
                    <<"content-type">> := <<"application/json">>
                }
            } when byte_size(JSONBin) > 0,
            hb_ao:resolve(
                #{
                    <<"device">> => <<"hyperbuddy@1.0">>
                },
                <<"custom.json">>,
                #{
                    <<"hyperbuddy-serve">> => #{
                        <<"custom.json">> => <<"test.json">>
                    }
                }
            )
        )
    after
        file:delete(Filename)
    end.