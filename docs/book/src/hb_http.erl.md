# hb_http

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_http.erl)

Hyperbeam's core HTTP request/reply functionality. The functions in this
module generally take a message request from their caller and return a
response in message form, as granted by the peer. This module is mostly
used by hb_client, but can also be used by other modules that need to make
HTTP requests.

---

## Exported Functions

- `accept_to_codec/2`
- `get/2`
- `get/3`
- `message_to_request/2`
- `post/3`
- `post/4`
- `reply/4`
- `req_to_tabm_singleton/3`
- `request/2`
- `request/4`
- `request/5`
- `start/0`

---

### start

```erlang
start() ->
    httpc:set_options([{max_keep_alive_length, 0}]),
    ok.
```

### get

Gets a URL via HTTP and returns the resulting message in deserialized

```erlang
get(Node, Opts) -> get(Node, <<"/">>, Opts).
```

### get

Gets a URL via HTTP and returns the resulting message in deserialized

```erlang
get(Node, PathBin, Opts) when is_binary(PathBin) ->
    get(Node, #{ <<"path">> => PathBin }, Opts);
```

### get

Gets a URL via HTTP and returns the resulting message in deserialized

```erlang
get(Node, Message, Opts) ->
    request(
        <<"GET">>,
        Node,
        hb_ao:get(<<"path">>, Message, <<"/">>, Opts),
        Message,
        Opts
    ).
```

### post

Posts a message to a URL on a remote peer via HTTP. Returns the

```erlang
post(Node, Path, Opts) when is_binary(Path) ->
    post(Node, #{ <<"path">> => Path }, Opts);
```

### post

Posts a message to a URL on a remote peer via HTTP. Returns the

```erlang
post(Node, Message, Opts) ->
    post(Node,
        hb_ao:get(
            <<"path">>,
            Message,
            <<"/">>,
            Opts#{ topic => ao_internal }
        ),
        Message,
        Opts
    ).
```

### post

```erlang
post(Node, Path, Message, Opts) ->
    case request(<<"POST">>, Node, Path, Message, Opts) of
        {ok, Res} ->
            ?event(http, {post_response, Res}),
            {ok, Res};
        Error -> Error
    end.
```

### request

Posts a binary to a URL on a remote peer via HTTP, returning the raw

```erlang
request(Message, Opts) ->
    % Special case: We are not given a peer and a path, so we need to
    % preprocess the URL to find them.
```

### request

```erlang
request(Method, Peer, Path, Opts) ->
    request(Method, Peer, Path, #{}, Opts).
```

### request

```erlang
request(Method, Config = #{ <<"nodes">> := Nodes }, Path, Message, Opts) when is_list(Nodes) ->
    % The request has a `route' (see `dev_router' for more details), so we use the
    % `multirequest' functionality, rather than a single request.
```

### request

```erlang
request(Method, #{ <<"opts">> := ReqOpts, <<"uri">> := URI }, _Path, Message, Opts) ->
    % The request has a set of additional options, so we apply them to the
    % request.
```

### request

```erlang
request(Method, Peer, Path, RawMessage, Opts) ->
    ?event({request, {method, Method}, {peer, Peer}, {path, Path}, {message, RawMessage}}),
    Req =
        prepare_request(
            hb_maps:get(
                <<"codec-device">>,
                RawMessage,
                <<"httpsig@1.0">>,
                Opts
            ),
            Method,
            Peer,
            Path,
            RawMessage,
            Opts
        ),
    StartTime = os:system_time(millisecond),
    % Perform the HTTP request.
```

### response_status_to_atom

Convert a HTTP status code to a status atom.

```erlang
response_status_to_atom(Status) ->
    case Status of
        201 -> created;
        X when X < 400 -> ok;
        X when X < 500 -> error;
        _ -> failure
    end.
```

### outbound_result_to_message

Convert an HTTP response to a message.

```erlang
outbound_result_to_message(<<"ans104@1.0">>, Status, Headers, Body, Opts) ->
    ?event(http_outbound,
        {result_is_ans104, {headers, Headers}, {body, Body}},
        Opts
    ),
    try ar_bundles:deserialize(Body) of
        Deserialized ->
            {
                response_status_to_atom(Status),
                hb_message:convert(
                    Deserialized,
                    <<"structured@1.0">>,
                    <<"ans104@1.0">>,
                    Opts
                )
            }
    catch
      _Class:ExceptionPattern:Stacktrace ->
        % The response message had a `codec-device: ans104@1.0', but we
        % failed to deserialize it, so we fallback to HTTPSig.
```

### outbound_result_to_message

```erlang
outbound_result_to_message(<<"httpsig@1.0">>, Status, Headers, Body, Opts) ->
    ?event(http_outbound, {result_is_httpsig, {body, Body}}, Opts),
    {
        response_status_to_atom(Status),
        http_response_to_httpsig(Status, Headers, Body, Opts)
    }.
```

### http_response_to_httpsig

Convert a HTTP response to a httpsig message.
Given a message, return the information needed to make the request.

```erlang
http_response_to_httpsig(Status, HeaderMap, Body, Opts) ->
    (hb_message:convert(
        hb_maps:merge(
            HeaderMap#{ <<"status">> => hb_util:bin(Status) },
            case Body of
                <<>> -> #{};
                _ -> #{ <<"body">> => Body }
            end,
			Opts
        ),
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        <<"httpsig@1.0">>,
        Opts
    ))#{ <<"status">> => hb_util:int(Status) }.
```

### message_to_request

Convert a HTTP response to a httpsig message.
Given a message, return the information needed to make the request.

```erlang
message_to_request(M, Opts) ->
    % Get the route for the message
    Res = route_to_request(M, RouteRes = dev_router:route(M, Opts), Opts),
    ?event(debug_http, {route_res, {route_res, RouteRes}, {full_res, Res}, {msg, M}}),
    Res.
```

### route_to_request

Parse a `dev_router:route` response and return a tuple of request

```erlang
route_to_request(M, {ok, URI}, Opts) when is_binary(URI) ->
    route_to_request(M, {ok, #{ <<"uri">> => URI, <<"opts">> => #{} }}, Opts);
```

### route_to_request

Parse a `dev_router:route` response and return a tuple of request

```erlang
route_to_request(M, {ok, #{ <<"uri">> := XPath, <<"opts">> := ReqOpts}}, Opts) ->
    % The request is a direct HTTP URL, so we need to split the path into a
    % host and path.
```

### route_to_request

```erlang
route_to_request(M, {ok, Routes}, Opts) ->
    ?event(http_outbound, {found_routes, {req, M}, {routes, Routes}}),
    % The result is a route, so we leave it to `request' to handle it.
```

### route_to_request

```erlang
route_to_request(M, {error, Reason}, _Opts) ->
    {error, {no_viable_route, {reason, Reason}, {message, M}}}.
```

### prepare_request

Turn a set of request arguments into a request message, formatted in the

```erlang
prepare_request(Format, Method, Peer, Path, RawMessage, Opts) ->
    Message = hb_ao:normalize_keys(RawMessage, Opts),
    % Generate a `cookie' key for the message, if an unencoded cookie is
    % present.
```

### reply

Reply to the client's HTTP request with a message.

```erlang
reply(Req, TABMReq, Message, Opts) ->
    Status =
        case hb_ao:get(<<"status">>, Message, Opts) of
            not_found -> 200;
            S-> S
        end,
    reply(Req, TABMReq, Status, Message, Opts).
```

### reply

```erlang
reply(Req, TABMReq, BinStatus, RawMessage, Opts) when is_binary(BinStatus) ->
    reply(Req, TABMReq, binary_to_integer(BinStatus), RawMessage, Opts);
```

### reply

```erlang
reply(InitReq, TABMReq, Status, RawMessage, Opts) ->
    KeyNormMessage = hb_ao:normalize_keys(RawMessage, Opts),
    {ok, Req, Message} = reply_handle_cookies(InitReq, KeyNormMessage, Opts),
    {ok, HeadersBeforeCors, EncodedBody} =
        encode_reply(
            Status,
            TABMReq,
            Message,
            Opts
        ),
    % Get the CORS request headers from the message, if they exist.
```

### reply_handle_cookies

Handle replying with cookies if the message contains them. Returns the

```erlang
reply_handle_cookies(Req, Message, Opts) ->
    {ok, Cookies} = dev_codec_cookie:extract(Message, #{}, Opts),
    ?event(debug_cookie, {encoding_reply_cookies, {explicit, Cookies}}),
    case Cookies of
        NoCookies when map_size(NoCookies) == 0 -> {ok, Req, Message};
        _ ->
            % The internal values of the `cookie' field will be stored in the
            % `priv_store' by default, so we let `dev_codec_cookie:opts/1'
            % reset the options.
```

### add_cors_headers

Add permissive CORS headers to a message, if the message has not already

```erlang
add_cors_headers(Msg, ReqHdr, Opts) ->
    CorHeaders = #{
        <<"access-control-allow-origin">> => <<"*">>,
        <<"access-control-allow-methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>,
        <<"access-control-expose-headers">> => <<"*">>
    },
     WithAllowHeaders = case ReqHdr of
        <<>> -> CorHeaders;
        _ -> CorHeaders#{
             <<"access-control-allow-headers">> => ReqHdr
        }
    end,
    % Keys in the given message will overwrite the defaults listed below if 
    % included, due to `hb_maps:merge''s precidence order.
```

### encode_reply

Generate the headers and body for a HTTP response message.

```erlang
encode_reply(Status, TABMReq, Message, Opts) ->
    Codec = accept_to_codec(TABMReq, Message, Opts),
    ?event(http, {encoding_reply, {codec, Codec}, {message, Message}}),
    BaseHdrs =
        hb_maps:merge(
            #{
                <<"codec-device">> => Codec
            },
            case codec_to_content_type(Codec, Opts) of
                    undefined -> #{};
                    CT -> #{ <<"content-type">> => CT }
            end,
			Opts
        ),
    AcceptBundle =
        hb_util:atom(
            hb_maps:get(<<"accept-bundle">>, TABMReq, false, Opts)
        ),
    ?event(http,
        {encoding_reply,
            {status, Status},
            {codec, Codec},
            {should_bundle, AcceptBundle},
            {response_message, Message}
        }
    ),
    % Codecs generally do not need to specify headers outside of the content-type,
    % aside the default `httpsig@1.0' codec, which expresses its form in HTTP
    % documents, and subsequently must set its own headers.
```

### accept_to_codec

Calculate the codec name to use for a reply given the original parsed 

```erlang
accept_to_codec(OriginalReq, Opts) ->
    accept_to_codec(OriginalReq, undefined, Opts).
```

### accept_to_codec

```erlang
accept_to_codec(#{ <<"require-codec">> := RequiredCodec }, _Reply, Opts) ->
    mime_to_codec(RequiredCodec, Opts);
```

### accept_to_codec

```erlang
accept_to_codec(_OriginalReq, #{ <<"content-type">> := _ }, _Opts) ->
    <<"httpsig@1.0">>;
```

### accept_to_codec

```erlang
accept_to_codec(OriginalReq, _, Opts) ->
    Accept = hb_maps:get(<<"accept">>, OriginalReq, <<"*/*">>, Opts),
    ?event(debug_accept,
        {accept_to_codec,
            {original_req, OriginalReq},
            {accept, Accept}
        }
    ),
    mime_to_codec(Accept, Opts).
```

### mime_to_codec

Find a codec name from a mime-type.

```erlang
mime_to_codec(<<"application/", Mime/binary>>, Opts) ->
    Name =
        case binary:match(Mime, <<"@">>) of
            nomatch -> << Mime/binary, "@1.0" >>;
            _ -> Mime
        end,
    case hb_ao:load_device(Name, Opts) of
        {ok, _} -> Name;
        {error, _} ->
            Default = default_codec(Opts),
            ?event(http,
                {codec_parsing_error,
                    {given, Name},
                    {defaulting_to, Default}
                }
            ),
            Default
    end;
```

### mime_to_codec

Find a codec name from a mime-type.

```erlang
mime_to_codec(<<"device/", Name/binary>>, _Opts) -> Name;
```

### mime_to_codec

Find a codec name from a mime-type.

```erlang
mime_to_codec(Device, Opts) ->
    case binary:match(Device, <<"@">>) of
        nomatch -> default_codec(Opts);
        _ -> Device
    end.
```

### default_codec

Return the default codec for the given options.
Call the `content-type` key on a message with the given codec, using

```erlang
default_codec(Opts) ->
    hb_opts:get(default_codec, <<"httpsig@1.0">>, Opts).
```

### codec_to_content_type

Return the default codec for the given options.
Call the `content-type` key on a message with the given codec, using

```erlang
codec_to_content_type(Codec, Opts) ->
    FastOpts =
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-cache">>, <<"no-store">>],
            cache_lookup_hueristics => false,
            load_remote_devices => false,
            error_strategy => continue
        },
    case hb_ao:get(<<"content-type">>, #{ <<"device">> => Codec }, FastOpts) of
        not_found -> undefined;
        CT -> CT
    end.
```

### req_to_tabm_singleton

Convert a cowboy request to a normalized message. We first parse the

```erlang
req_to_tabm_singleton(Req, Body, Opts) ->
    FullPath =
        <<
            (cowboy_req:path(Req))/binary,
            "?",
            (cowboy_req:qs(Req))/binary
        >>,
    Headers = cowboy_req:headers(Req),
    {ok, _Path, QueryKeys} = hb_singleton:from_path(FullPath),
    PrimitiveMsg = maps:merge(Headers, QueryKeys),
    Codec =
        case hb_maps:find(<<"codec-device">>, PrimitiveMsg, Opts) of
            {ok, ExplicitCodec} -> ExplicitCodec;
            error ->
                case hb_maps:find(<<"content-type">>, PrimitiveMsg, Opts) of
                    {ok, ContentType} -> mime_to_codec(ContentType, Opts);
                    error -> default_codec(Opts)
                end
        end,
    ?event(http,
        {parsing_req,
            {path, FullPath},
            {query, QueryKeys},
            {headers, Headers},
            {primitive_message, PrimitiveMsg}
        }
    ),
    ?event({req_to_tabm_singleton, {codec, Codec}}),
    case Codec of
        <<"httpsig@1.0">> ->
			?event(
                {req_to_tabm_singleton,
                    {request, {explicit, Req},
                    {body, {string, Body}}
                }}
            ),
            httpsig_to_tabm_singleton(PrimitiveMsg, Req, Body, Opts);
        <<"ans104@1.0">> ->
            Item = ar_bundles:deserialize(Body),
            ?event(debug_accept,
                {deserialized_ans104,
                    {item, Item},
                    {exact, {explicit, Item}}
                }
            ),
            case ar_bundles:verify_item(Item) of
                true ->
                    ?event(ans104, {valid_ans104_signature, Item}),
                    ANS104 =
                        hb_message:convert(
                            Item,
                            <<"structured@1.0">>,
                            <<"ans104@1.0">>,
                            Opts
                        ),
                    normalize_unsigned(PrimitiveMsg, Req, ANS104, Opts);
                false ->
                    throw({invalid_ans104_signature, Item})
            end;
        Codec ->
            % Assume that the codec stores the encoded message in the `body' field.
```

### httpsig_to_tabm_singleton

HTTPSig messages are inherently mixed into the transport layer, so they

```erlang
httpsig_to_tabm_singleton(PrimMsg, Req, Body, Opts) ->
    {ok, Decoded} =
        hb_message:with_only_committed(
            hb_message:convert(
                PrimMsg#{ <<"body">> => Body },
                <<"structured@1.0">>,
                <<"httpsig@1.0">>,
                Opts
            ),
            Opts
        ),
    ?event(http, {decoded, Decoded}, Opts),
    ForceSignedRequests = hb_opts:get(force_signed_requests, false, Opts),
    case (not ForceSignedRequests) orelse hb_message:verify(Decoded, all, Opts) of
        true ->
            ?event(http_verify, {verified_signature, Decoded}),
            Signers = hb_message:signers(Decoded, Opts),
            case Signers =/= [] andalso hb_opts:get(store_all_signed, false, Opts) of
                true ->
                    ?event(http_verify, {storing_signed_from_wire, Decoded}),
                    {ok, _} =
                        hb_cache:write(Decoded,
                            Opts#{
                                store =>
                                    #{
                                        <<"store-module">> => hb_store_fs,
                                        <<"name">> => <<"cache-http">>
                                    }
                            }
                        );
                false ->
                    do_nothing
            end,
            normalize_unsigned(PrimMsg, Req, Decoded, Opts);
        false ->
            ?event(http_verify,
                {invalid_signature,
                    {signed, Decoded},
                    {force, ForceSignedRequests}
                }
            ),
            throw({invalid_commitments, Decoded})
    end.
```

### normalize_unsigned

Add the method and path to a message, if they are not already present.

```erlang
normalize_unsigned(PrimMsg, Req = #{ headers := RawHeaders }, Msg, Opts) ->
    ?event({adding_method_and_path_from_request, {explicit, Req}}),
    Method = cowboy_req:method(Req),
    MsgPath =
        hb_maps:get(
            <<"path">>,
            Msg,
            hb_maps:get(
                <<"path">>, 
                RawHeaders,
                iolist_to_binary(
                    cowboy_req:uri(
                        Req,
                        #{
                            host => undefined,
                            port => undefined,
                            scheme => undefined
                        }
                    )
                ),
                Opts
            ),
            Opts
        ),
    FilterKeys = hb_opts:get(http_inbound_filter_keys, ?DEFAULT_FILTER_KEYS, Opts),
    FilteredMsg = hb_message:without_unless_signed(FilterKeys, Msg, Opts),
    BaseMsg =
        FilteredMsg#{
            <<"method">> => Method,
            <<"path">> => MsgPath,
            <<"accept-bundle">> =>
                maps:get(
                    <<"accept-bundle">>,
                    Msg,
                    maps:get(
                        <<"accept-bundle">>,
                        PrimMsg,
                        maps:get(<<"accept-bundle">>, RawHeaders, false)
                    )
                ),
            <<"accept">> =>
                Accept = maps:get(
                    <<"accept">>,
                    Msg,
                    maps:get(
                        <<"accept">>,
                        PrimMsg,
                        maps:get(<<"accept">>, RawHeaders, <<"*/*">>)
                    )
                )
        },
    ?event(debug_accept, {normalize_unsigned, {accept, Accept}}),
    % Parse and add the cookie from the request, if present. We reinstate the
    % `cookie' field in the message, as it is not typically signed, yet should
    % be honored by the node anyway.
```

### simple_ao_resolve_unsigned_test

```erlang
simple_ao_resolve_unsigned_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"/key1">>, <<"key1">> => <<"Value1">> },
    ?assertEqual({ok, <<"Value1">>}, post(URL, TestMsg, #{})).
```

### simple_ao_resolve_signed_test

```erlang
simple_ao_resolve_signed_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"/key1">>, <<"key1">> => <<"Value1">> },
    Wallet = hb:wallet(),
    {ok, Res} =
        post(
            URL,
            hb_message:commit(TestMsg, Wallet),
            #{}
        ),
    ?assertEqual(<<"Value1">>, Res).
```

### nested_ao_resolve_test

```erlang
nested_ao_resolve_test() ->
    URL = hb_http_server:start_node(),
    Wallet = hb:wallet(),
    {ok, Res} =
        post(
            URL,
            hb_message:commit(#{
                <<"path">> => <<"/key1/key2/key3">>,
                <<"key1">> =>
                    #{<<"key2">> =>
                        #{
                            <<"key3">> => <<"Value2">>
                        }
                    }
            }, Wallet),
            #{}
        ),
    ?assertEqual(<<"Value2">>, Res).
```

### wasm_compute_request

```erlang
wasm_compute_request(ImageFile, Func, Params) ->
    wasm_compute_request(ImageFile, Func, Params, <<"">>).
```

### wasm_compute_request

```erlang
wasm_compute_request(ImageFile, Func, Params, ResultPath) ->
    {ok, Bin} = file:read_file(ImageFile),
    Wallet = hb:wallet(),
    hb_message:commit(#{
        <<"path">> => <<"/init/compute/results", ResultPath/binary>>,
        <<"device">> => <<"wasm-64@1.0">>,
        <<"function">> => Func,
        <<"parameters">> => Params,
        <<"body">> => Bin
    }, Wallet).
```

### run_wasm_unsigned_test

```erlang
run_wasm_unsigned_test() ->
    Node = hb_http_server:start_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0]),
    {ok, Res} = post(Node, Msg, #{}),
    ?event({res, Res}),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, #{})).
```

### run_wasm_signed_test

```erlang
run_wasm_signed_test() ->
    Opts = #{ priv_wallet => hb:wallet() },
    URL = hb_http_server:start_node(#{force_signed => true}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>),
    {ok, Res} = post(URL, hb_message:commit(Msg, Opts), Opts),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, #{})).
```

### get_deep_unsigned_wasm_state_test

```erlang
get_deep_unsigned_wasm_state_test() ->
    URL = hb_http_server:start_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>),
    {ok, Res} = post(URL, Msg, #{}),
    ?assertEqual(6.0, hb_ao:get(<<"/output/1">>, Res, #{})).
```

### get_deep_signed_wasm_state_test

```erlang
get_deep_signed_wasm_state_test() ->
    URL = hb_http_server:start_node(#{force_signed => true}),
    Msg =
        wasm_compute_request(
            <<"test/test-64.wasm">>,
            <<"fac">>,
            [3.0],
            <<"/output">>
        ),
    {ok, Res} = post(URL, Msg, #{}),
    ?assertEqual(6.0, hb_ao:get(<<"1">>, Res, #{})).
```

### cors_get_test

```erlang
cors_get_test() ->
    URL = hb_http_server:start_node(),
    {ok, Res} = get(URL, <<"/~meta@1.0/info">>, #{}),
    ?assertEqual(
        <<"*">>,
        hb_ao:get(<<"access-control-allow-origin">>, Res, #{})
    ).
```

### ans104_wasm_test

```erlang
ans104_wasm_test() ->
    TestStore = [hb_test_utils:test_store()],
    TestOpts =
        #{
            force_signed => true,
            store => TestStore,
            priv_wallet => ar_wallet:new()
        },
    ClientStore = [hb_test_utils:test_store()],
    ClientOpts = #{ store => ClientStore, priv_wallet => hb:wallet() },
    URL = hb_http_server:start_node(TestOpts),
    {ok, Bin} = file:read_file(<<"test/test-64.wasm">>),
    Msg =
        hb_message:commit(
            #{
                <<"require-codec">> => <<"ans104@1.0">>,
                <<"codec-device">> => <<"ans104@1.0">>,
                <<"device">> => <<"wasm-64@1.0">>,
                <<"function">> => <<"fac">>,
                <<"parameters">> => [3.0],
                <<"body">> => Bin
            },
            ClientOpts,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true }
        ),
    ?assert(hb_message:verify(Msg, all, ClientOpts)),
    ?event({msg, Msg}),
    {ok, Res} =
        post(
            URL,
            Msg#{ <<"path">> => <<"/init/compute/results">> },
            ClientOpts
        ),
    ?event({res, Res}),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, ClientOpts)).
```

### send_large_signed_request_test

```erlang
send_large_signed_request_test() ->
    % Note: If the signature scheme ever changes, we will need to run the 
    % following to get a freshly signed request.
```

### index_test

```erlang
index_test() ->
    NodeURL = hb_http_server:start_node(),
    {ok, Res} =
        get(
            NodeURL,
            #{
                <<"path">> => <<"/~test-device@1.0/load">>,
                <<"accept-bundle">> => false
            },
            #{}
        ),
    ?assertEqual(<<"i like turtles!">>, hb_ao:get(<<"body">>, Res, #{})).
```

### index_request_test

```erlang
index_request_test() ->
    URL = hb_http_server:start_node(),
    {ok, Res} =
        get(
            URL,
            #{
                <<"path">> => <<"/~test-device@1.0/load?name=dogs">>,
                <<"accept-bundle">> => false
            },
            #{}
        ),
    ?assertEqual(<<"i like dogs!">>, hb_ao:get(<<"body">>, Res, #{})).
```

---

*Generated from [hb_http.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_http.erl)*
