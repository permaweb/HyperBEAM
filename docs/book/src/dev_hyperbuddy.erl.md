# dev_hyperbuddy

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_hyperbuddy.erl)

A device that renders a REPL-like interface for AO-Core via HTML.

---

## Exported Functions

- `events/3`
- `format/3`
- `info/0`
- `metrics/3`
- `return_error/2`
- `return_file/2`
- `throw/3`

---

### info

A device that renders a REPL-like interface for AO-Core via HTML.
Export an explicit list of files via http.

```erlang
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
```

### metrics

The main HTML page for the REPL device.

```erlang
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
```

### events

Return the current event counters as a message.

```erlang
events(_, _Req, _Opts) ->
    {ok, hb_event:counters()}.
```

### format

Employ HyperBEAM's internal pretty printer to format a message.

```erlang
format(Base, Req, Opts) ->
    LoadedBase = hb_cache:ensure_all_loaded(Base, Opts),
    LoadedReq = hb_cache:ensure_all_loaded(Req, Opts),
    {ok,
        #{
            <<"body">> =>
                hb_util:bin(
                    hb_format:message(
                        #{
                            <<"base">> =>
                                maps:without(
                                    [<<"device">>],
                                    hb_private:reset(LoadedBase)),
                            <<"request">> =>
                                maps:without(
                                    [<<"path">>],
                                    hb_private:reset(LoadedReq)
                                )
                        },
                        Opts#{
                            linkify_mode => discard,
                            cache_control => [<<"no-cache">>, <<"no-store">>]
                        }
                    )
                )
        }
    }.
```

### throw

Test key for validating the behavior of the `500` HTTP response.

```erlang
throw(_Msg, _Req, Opts) ->
    case hb_opts:get(mode, prod, Opts) of
        prod -> {error, <<"Forced-throw unavailable in `prod` mode.">>};
        debug -> throw({intentional_error, Opts})
    end.
```

### serve

Serve a file from the priv directory. Only serves files that are explicitly

```erlang
serve(<<"keys">>, M1, _M2, Opts) -> dev_message:keys(M1, Opts);
```

### serve

Serve a file from the priv directory. Only serves files that are explicitly

```erlang
serve(<<"set">>, M1, M2, Opts) -> dev_message:set(M1, M2, Opts);
```

### serve

Serve a file from the priv directory. Only serves files that are explicitly

```erlang
serve(Key, _, _, Opts) ->
    ?event({hyperbuddy_serving, Key}),
    Routes = hb_maps:get(routes, info(), no_routes, Opts),
    case hb_maps:get(Key, Routes, undefined, Opts) of
        undefined -> {error, not_found};
        Filename -> return_file(Filename)
    end.
```

### return_file

Read a file from disk and serve it as a static HTML page.

```erlang
return_file(Name) ->
    return_file(<<"hyperbuddy@1.0">>, Name, #{}).
```

### return_file

Read a file from disk and serve it as a static HTML page.

```erlang
return_file(Device, Name) ->
    return_file(Device, Name, #{}).
```

### return_file

```erlang
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
```

### return_error

Return an error page, with the `{{error}}` template variable replaced.

```erlang
return_error(Error, Opts) when not is_map(Error) ->
    return_error(#{ <<"body">> => Error }, Opts);
```

### return_error

Return an error page, with the `{{error}}` template variable replaced.

```erlang
return_error(ErrorMsg, Opts) ->
    return_file(
        <<"hyperbuddy@1.0">>,
        <<"500.html">>,
        #{ <<"error">> => hb_format:error(ErrorMsg, Opts) }
    ).
```

### apply_template

Apply a template to a body.

```erlang
apply_template(Body, Template) when is_map(Template) ->
    apply_template(Body, maps:to_list(Template));
```

### apply_template

Apply a template to a body.

```erlang
apply_template(Body, []) ->
    Body;
```

### apply_template

Apply a template to a body.

```erlang
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
```

### return_templated_file_test

```erlang
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
```

---

*Generated from [dev_hyperbuddy.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_hyperbuddy.erl)*
