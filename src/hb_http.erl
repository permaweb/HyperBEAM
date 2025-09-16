%%% Hyperbeam's core HTTP request/reply functionality. The functions in this
%%% module generally take a message request from their caller and return a
%%% response in message form, as granted by the peer. This module is mostly
%%% used by hb_client, but can also be used by other modules that need to make
%%% HTTP requests.
-module(hb_http).
-export([start/0]).
-export([get/2, get/3, post/3, post/4, request/2, request/4, request/5]).
-export([message_to_request/2, reply/4, accept_to_codec/2]).
-export([req_to_tabm_singleton/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEFAULT_FILTER_KEYS, [<<"content-length">>]).

start() ->
    httpc:set_options([{max_keep_alive_length, 0}]),
    ok.

%% @doc Gets a URL via HTTP and returns the resulting message in deserialized
%% form.
get(Node, Opts) -> get(Node, <<"/">>, Opts).
get(Node, PathBin, Opts) when is_binary(PathBin) ->
    get(Node, #{ <<"path">> => PathBin }, Opts);
get(Node, Message, Opts) ->
    request(
        <<"GET">>,
        Node,
        hb_ao:get(<<"path">>, Message, <<"/">>, Opts),
        Message,
        Opts
    ).

%% @doc Posts a message to a URL on a remote peer via HTTP. Returns the
%% resulting message in deserialized form.
post(Node, Path, Opts) when is_binary(Path) ->
    post(Node, #{ <<"path">> => Path }, Opts);
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
post(Node, Path, Message, Opts) ->
    case request(<<"POST">>, Node, Path, Message, Opts) of
        {ok, Res} ->
            ?event(http, {post_response, Res}),
            {ok, Res};
        Error -> Error
    end.

%% @doc Posts a binary to a URL on a remote peer via HTTP, returning the raw
%% binary body.
request(Message, Opts) ->
    % Special case: We are not given a peer and a path, so we need to
    % preprocess the URL to find them.
    {ok, Method, Peer, Path, MessageToSend, NewOpts} =
        message_to_request(Message, Opts),
    request(Method, Peer, Path, MessageToSend, NewOpts).
request(Method, Peer, Path, Opts) ->
    request(Method, Peer, Path, #{}, Opts).
request(Method, Config = #{ <<"nodes">> := Nodes }, Path, Message, Opts) when is_list(Nodes) ->
    % The request has a `route' (see `dev_router' for more details), so we use the
    % `multirequest' functionality, rather than a single request.
    hb_http_multi:request(Config, Method, Path, Message, Opts);
request(Method, #{ <<"opts">> := ReqOpts, <<"uri">> := URI }, _Path, Message, Opts) ->
    % The request has a set of additional options, so we apply them to the
    % request.
    MergedOpts =
        hb_maps:merge(
            Opts,
            hb_opts:mimic_default_types(ReqOpts, new_atoms, Opts),
            Opts
        ),
    % We also recalculate the request. The order of precidence here is subtle:
    % We favor the args given to the function, but the URI rules take precidence
    % over that.
    {ok, NewMethod, Node, NewPath, NewMsg, NewOpts} =
        message_to_request(
            Message#{ <<"path">> => URI, <<"method">> => Method },
            MergedOpts
        ),
    request(NewMethod, Node, NewPath, NewMsg, NewOpts);
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
    {_ErlStatus, Status, Headers, Body} = hb_http_client:req(Req, Opts),
    % Process the response.
    EndTime = os:system_time(millisecond),
    ?event(http_outbound,
        {
            http_response,
            {req, Req},
            {response,
                #{
                    status => Status,
                    headers => Headers,
                    body => Body
                }
            }
        },
        Opts
    ),
    % Convert the set-cookie headers into a cookie message, if they are present.
    % We do this by extracting the set-cookie headers and converting them into a
    % cookie message if they are present.
    SetCookieLines =
        [
            KeyVal
        ||
            {<<"set-cookie">>, KeyVal} <- Headers
        ],
    MaybeSetCookie =
        case SetCookieLines of
            [] -> #{};
            _ ->
                ?event(
                    debug_cookie,
                    {normalizing_setcookie_headers,
                        {set_cookie_lines, [ {string, Line} || Line <- SetCookieLines ]}
                    },
                    Opts
                ),
                {ok, MsgWithCookies} =
                    dev_codec_cookie:from(
                        #{ <<"set-cookie">> => SetCookieLines },
                        #{},
                        Opts
                    ),
                ?event(debug_cookie, {msg_with_cookies, MsgWithCookies}),
                MsgWithCookies
        end,
    % Merge the set-cookie message into the header map, which itself is
    % constructed from the header key-value pair list.
    HeaderMap = hb_maps:merge(hb_maps:from_list(Headers), MaybeSetCookie, Opts),
    NormHeaderMap = hb_ao:normalize_keys(HeaderMap, Opts),
    ?event(http_outbound,
        {normalized_response_headers, {norm_header_map, NormHeaderMap}},
        Opts
    ),
    ?event(http_short,
        {received,
            {status, Status},
            {duration, EndTime - StartTime},
            {method, Method},
            {peer, Peer},
            {path, {string, Path}},
            {body_size, byte_size(Body)}
        }),
    ReturnAOResult =
        hb_opts:get(http_only_result, true, Opts) andalso
        hb_maps:get(<<"ao-result">>, NormHeaderMap, false, Opts),
    case ReturnAOResult of
        Key when is_binary(Key) ->
            Msg = http_response_to_httpsig(Status, NormHeaderMap, Body, Opts),
            ?event(
                http_outbound,
                {result_is_single_key, {key, Key}, {msg, Msg}},
                Opts
            ),
            case {Key, hb_maps:get(Key, Msg, undefined, Opts)} of
                {<<"body">>, undefined} ->
                    {response_status_to_atom(Status), <<>>};
                {_, undefined} ->
                    {failure,
                        <<
                            "Result key '",
                            Key/binary,
                            "' not found in response from '",
                            Peer/binary,
                            "' for path '",
                            Path/binary,
                            "': ",
                            Body/binary
                        >>
                    };
                {_, Value} ->
                    {response_status_to_atom(Status), Value}
            end;
        false ->
            % Find the codec device from the headers, if set.
            CodecDev =
                hb_maps:get(
                    <<"codec-device">>,
                    NormHeaderMap,
                    <<"httpsig@1.0">>,
                    Opts
                ),
            outbound_result_to_message(
                CodecDev,
                Status,
                NormHeaderMap,
                Body,
                Opts
            )
    end.

%% @doc Convert a HTTP status code to a status atom.
response_status_to_atom(Status) ->
    case Status of
        201 -> created;
        X when X < 400 -> ok;
        X when X < 500 -> error;
        _ -> failure
    end.

%% @doc Convert an HTTP response to a message.
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
        ?event(http_outbound,
            {failed_to_deserialize_ans104_attempting_httpsig,
                {headers, Headers},
                {body, Body},
                {error, ExceptionPattern},
                {stacktrace, {trace, Stacktrace}}
            },
            Opts
        ),
        outbound_result_to_message(<<"httpsig@1.0">>, Status, Headers, Body, Opts)
    end;
outbound_result_to_message(<<"httpsig@1.0">>, Status, Headers, Body, Opts) ->
    ?event(http_outbound, {result_is_httpsig, {body, Body}}, Opts),
    {
        response_status_to_atom(Status),
        http_response_to_httpsig(Status, Headers, Body, Opts)
    }.

%% @doc Convert a HTTP response to a httpsig message.
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

%% @doc Given a message, return the information needed to make the request.
message_to_request(M, Opts) ->
    % Get the route for the message
    Res = route_to_request(M, RouteRes = dev_router:route(M, Opts), Opts),
    ?event(debug_http, {route_res, {route_res, RouteRes}, {full_res, Res}, {msg, M}}),
    Res.

%% @doc Parse a `dev_router:route' response and return a tuple of request
%% parameters.
route_to_request(M, {ok, URI}, Opts) when is_binary(URI) ->
    route_to_request(M, {ok, #{ <<"uri">> => URI, <<"opts">> => #{} }}, Opts);
route_to_request(M, {ok, #{ <<"uri">> := XPath, <<"opts">> := ReqOpts}}, Opts) ->
    % The request is a direct HTTP URL, so we need to split the path into a
    % host and path.
    URI = uri_string:parse(XPath),
    ?event(http_outbound, {parsed_uri, {uri, {explicit, URI}}}),
    Method = hb_ao:get(<<"method">>, M, <<"GET">>, Opts),
    % We must remove the path and host from the message, because they are not
    % valid for outbound requests. The path is retrieved from the route, and
    % the host should already be known to the caller.
    MsgWithoutMeta = hb_maps:without([<<"path">>, <<"host">>], M, Opts),
    Port =
        case maps:get(port, URI, undefined) of
            undefined ->
                % If no port is specified, use 80 for HTTP and 443
                % for HTTPS.
                case XPath of
                    <<"https", _/binary>> -> <<"443">>;
                    _ -> <<"80">>
                end;
            X -> integer_to_binary(X)
        end,
    Protocol = maps:get(scheme, URI, <<"https">>),
    Host = maps:get(host, URI, <<"localhost">>),
    Node = << Protocol/binary, "://", Host/binary, ":", Port/binary  >>,
    PathParts = [maps:get(path, URI, <<"/">>)] ++
        case maps:get(query, URI, <<>>) of
            <<>> -> [];
            Query -> [<<"?", Query/binary>>]
        end,
    Path = iolist_to_binary(PathParts),
    ?event(http_outbound, {parsed_req, {node, Node}, {method, Method}, {path, Path}}),
    {ok, Method, Node, Path, MsgWithoutMeta, hb_util:deep_merge(Opts, ReqOpts, Opts)};
route_to_request(M, {ok, Routes}, Opts) ->
    ?event(http_outbound, {found_routes, {req, M}, {routes, Routes}}),
    % The result is a route, so we leave it to `request' to handle it.
    Path = hb_ao:get(<<"path">>, M, <<"/">>, Opts),
    Method = hb_ao:get(<<"method">>, M, <<"GET">>, Opts),
    % We must remove the path and host from the message, because they are not
    % valid for outbound requests. The path is retrieved from the route, and
    % the host should already be known to the caller.
    MsgWithoutMeta = hb_maps:without([<<"path">>, <<"host">>], M, Opts),
    {ok, Method, Routes, Path, MsgWithoutMeta, Opts};
route_to_request(M, {error, Reason}, _Opts) ->
    {error, {no_viable_route, {reason, Reason}, {message, M}}}.

%% @doc Turn a set of request arguments into a request message, formatted in the
%% preferred format. This function honors the `accept-bundle' option, if it is
%% already present in the message, and sets it to `true' if it is not.
prepare_request(Format, Method, Peer, Path, RawMessage, Opts) ->
    Message = hb_ao:normalize_keys(RawMessage, Opts),
    % Generate a `cookie' key for the message, if an unencoded cookie is
    % present.
    {MaybeCookie, WithoutCookie} =
        case dev_codec_cookie:extract(Message, #{}, Opts) of
            {ok, NoCookies} when map_size(NoCookies) == 0 ->
                {#{}, Message};
            {ok, _Cookies} ->
                {ok, #{ <<"cookie">> := CookieLines }} =
                    dev_codec_cookie:to(
                        Message,
                        #{ <<"format">> => <<"cookie">> },
                        Opts
                    ),
                {ok, CookieReset} = dev_codec_cookie:reset(Message, Opts),
                ?event(http, {cookie_lines, CookieLines}),
                {
                    #{ <<"cookie">> => CookieLines },
                    CookieReset
                }
        end,
    % Remove the private components from the message, if they are present.
    WithoutPriv = hb_private:reset(WithoutCookie),
    % Add the `accept-bundle: true' key to the message, if the caller has not
    % set an explicit preference.
    WithAcceptBundle =
        case hb_maps:get(<<"accept-bundle">>, Message, not_found, Opts) of
            not_found -> WithoutPriv#{ <<"accept-bundle">> => true };
            _ -> WithoutPriv
        end,
    % Determine the `ao-peer-port' from the message to send or the node message.
    % `port_external' can be set in the node message to override the port that
    % the peer node should receive. This allows users to proxy requests to their
    % HB node from another port.
    WithSelfPort =
        WithAcceptBundle#{
            <<"ao-peer-port">> =>
                hb_maps:get(
                    <<"ao-peer-port">>,
                    WithAcceptBundle,
                    hb_opts:get(
                        port_external,
                        hb_opts:get(port, undefined, Opts),
                        Opts
                    ),
                    Opts
                )
        },
    BinPeer = if is_binary(Peer) -> Peer; true -> list_to_binary(Peer) end,
    BinPath = hb_path:normalize(hb_path:to_binary(Path)),
    ReqBase = #{ peer => BinPeer, path => BinPath, method => Method },
    case Format of
        <<"httpsig@1.0">> ->
            FullEncoding =
                hb_message:convert(
                    WithSelfPort,
                    #{
                        <<"device">> => <<"httpsig@1.0">>,
                        <<"bundle">> => true
                    },
                    Opts
                ),
            Body = hb_maps:get(<<"body">>, FullEncoding, <<>>, Opts),
            Headers = hb_maps:without([<<"body">>], FullEncoding, Opts),
			?event(http, {request_headers, {explicit, {headers, Headers}}}),
			?event(http, {request_body, {explicit, {body, Body}}}),
            hb_maps:merge(
                ReqBase,
                #{ headers => maps:merge(MaybeCookie, Headers), body => Body },
                Opts
            );
        <<"ans104@1.0">> ->
            ?event(debug_accept, {request_message, {message, Message}}),
            {ok, FilteredMessage} =
                case hb_message:signers(Message, Opts) of
                    [] -> WithSelfPort;
                    _ ->
                        hb_message:with_only_committed(WithSelfPort, Opts)
                end,
            ReqBase#{
                headers =>
                    MaybeCookie#{
                        <<"codec-device">> => <<"ans104@1.0">>,
                        <<"content-type">> => <<"application/ans104">>,
                        <<"accept-bundle">> =>
                            hb_util:bin(
                                hb_ao:get(
                                    <<"accept-bundle">>,
                                    WithSelfPort,
                                    true,
                                    Opts
                                )
                            )
                    },
                body =>
                    ar_bundles:serialize(
                        hb_message:convert(
                            FilteredMessage,
                            #{
                                <<"device">> => <<"ans104@1.0">>,
                                <<"bundle">> => true
                            },
                            Opts
                        )
                    )
            };
        _ ->
            ReqBase#{
                headers =>
                    maps:merge(MaybeCookie, maps:without([<<"body">>], Message)),
                body => maps:get(<<"body">>, Message, <<>>)
            }
    end.

%% @doc Reply to the client's HTTP request with a message.
reply(Req, TABMReq, Message, Opts) ->
    Status =
        case hb_ao:get(<<"status">>, Message, Opts) of
            not_found -> 200;
            S-> S
        end,
    reply(Req, TABMReq, Status, Message, Opts).
reply(Req, TABMReq, BinStatus, RawMessage, Opts) when is_binary(BinStatus) ->
    reply(Req, TABMReq, binary_to_integer(BinStatus), RawMessage, Opts);
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
    ReqHdr = cowboy_req:header(<<"access-control-request-headers">>, Req, <<"">>),
    HeadersWithCors = add_cors_headers(HeadersBeforeCors, ReqHdr, Opts),
    EncodedHeaders = hb_private:reset(HeadersWithCors),
    ?event(http,
        {http_replying,
            {status, {explicit, Status}},
            {path, hb_maps:get(<<"path">>, Req, undefined_path, Opts)},
            {raw_message, RawMessage},
            {enc_headers, {explicit, EncodedHeaders}},
            {enc_body, EncodedBody}
        }
    ),
    ReqBeforeStream = Req#{ resp_headers => EncodedHeaders },
    PostStreamReq = cowboy_req:stream_reply(Status, #{}, ReqBeforeStream),
    cowboy_req:stream_body(EncodedBody, nofin, PostStreamReq),
    EndTime = os:system_time(millisecond),
    ?event(http, {reply_headers, {explicit, PostStreamReq}}),
    ?event(http_short,
        {sent,
            {status, Status},
            {duration, EndTime - hb_maps:get(start_time, Req, undefined, Opts)},
            {method, cowboy_req:method(Req)},
            {path,
                {string,
                    uri_string:percent_decode(
                        hb_maps:get(<<"path">>, TABMReq, <<"[NO PATH]">>, Opts)
                    )
                }
            },
            {body_size, byte_size(EncodedBody)}
        }
    ),
    {ok, PostStreamReq, no_state}.

%% @doc Handle replying with cookies if the message contains them. Returns the
%% new Cowboy `Req` object, and the message with the cookies removed. Both
%% `set-cookie' and `cookie' fields are treated as viable sources of cookies.
reply_handle_cookies(Req, Message, Opts) ->
    {ok, Cookies} = dev_codec_cookie:extract(Message, #{}, Opts),
    ?event(debug_cookie, {encoding_reply_cookies, {explicit, Cookies}}),
    case Cookies of
        NoCookies when map_size(NoCookies) == 0 -> {ok, Req, Message};
        _ ->
            % The internal values of the `cookie' field will be stored in the
            % `priv_store' by default, so we let `dev_codec_cookie:opts/1'
            % reset the options.
            {ok, #{ <<"set-cookie">> := SetCookieLines }} =
                dev_codec_cookie:to(
                    Message,
                    #{ <<"format">> => <<"set-cookie">> },
                    Opts
                ),
            ?event(debug_cookie, {outbound_set_cookie_lines, SetCookieLines}),
            % Add the cookies to the response headers.
            FinalReq =
                lists:foldl(
                    fun(FullCookieLine, ReqAcc) ->
                        [CookieRef, _] = binary:split(FullCookieLine, <<"=">>),
                        RespCookies = maps:get(resp_cookies, ReqAcc, #{}),
                        % Note: Cowboy handles cookies peculiarly. The key
                        % given in the `resp_cookies' map is not used directly
                        % in the response headers. Nonetheless, we use the
                        % key parsed from the cookie line as the key, but do not
                        % be surprised if while debugging you see a different
                        % key created by Cowboy in the response headers.
                        ReqAcc#{
                            resp_cookies =>
                                RespCookies#{ CookieRef => FullCookieLine }
                        }
                    end,
                    Req,
                    SetCookieLines
                ),
            {ok, CookieReset} = dev_codec_cookie:reset(Message, Opts),
            {
                ok,
                FinalReq,
                CookieReset
            }
    end.

%% @doc Add permissive CORS headers to a message, if the message has not already
%% specified CORS headers.
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
    hb_maps:merge(WithAllowHeaders, Msg, Opts).

%% @doc Generate the headers and body for a HTTP response message.
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
    case {Status, Codec, AcceptBundle} of
        {500, <<"httpsig@1.0">>, false} ->
            ?event(debug_accept,
                {returning_500_error,
                    {status, Status},
                    {codec, Codec},
                    {bundle, AcceptBundle}
                }
            ),
            {ok, ErrMsg} =
                dev_hyperbuddy:return_error(Message, Opts),
            {ok,
                maps:without([<<"body">>], ErrMsg),
                maps:get(<<"body">>, ErrMsg, <<>>)
            };
        {404, <<"httpsig@1.0">>, false} ->
            {ok, ErrMsg} =
                dev_hyperbuddy:return_file(
                    <<"hyperbuddy@1.0">>,
                    <<"404.html">>
                ),
            {ok,
                maps:without([<<"body">>], ErrMsg),
                maps:get(<<"body">>, ErrMsg, <<>>)
            };
        {_, <<"httpsig@1.0">>, _} ->
            TABM =
                hb_message:convert(
                    Message,
                    tabm,
                    <<"structured@1.0">>,
                    Opts#{ topic => ao_internal }
                ),
            {ok, EncMessage} =
                dev_codec_httpsig:to(
                    TABM,
                    case AcceptBundle of
                        true ->
                            #{
                                <<"bundle">> => true
                            };
                        false ->
                            #{
                                <<"index">> =>
                                    hb_opts:get(generate_index, true, Opts)
                            }
                    end,
                    Opts
                ),
            {
                ok,
                hb_maps:without([<<"body">>], EncMessage, Opts),
                hb_maps:get(<<"body">>, EncMessage, <<>>, Opts)
            };
        {_, <<"ans104@1.0">>, _} ->
            % The `ans104@1.0' codec is a binary format, so we must serialize
            % the message to a binary before sending it.
            {
                ok,
                BaseHdrs,
                ar_bundles:serialize(
                    hb_message:convert(
                        hb_message:with_only_committers(
                            Message,
                            hb_message:signers(Message, Opts),
							Opts
                        ),
                        #{
                            <<"device">> => <<"ans104@1.0">>,
                            <<"bundle">> =>
                                hb_util:atom(
                                    hb_ao:get(
                                        <<"accept-bundle">>,
                                        {as, <<"message@1.0">>, TABMReq},
                                        true,
                                        Opts
                                    )
                                )
                        },
                        <<"structured@1.0">>,
                        Opts#{ topic => ao_internal }
                    )
                )
            };
        _ ->
            % Other codecs are already in binary format, so we can just convert
            % the message to the codec. We also include all of the top-level 
            % fields, except for maps and lists, in the message and return them 
            % as headers.
            ExtraHdrs =
                hb_maps:filter(
                    fun(Key, V) ->
                        not is_map(V)
                            andalso not is_list(V)
                            andalso Key =/= <<"body">>
                            andalso Key =/= <<"data">>
                    end,
                    Message,
                    Opts
                ),
            % Encode all header values as strings.
            EncodedExtraHdrs =
                maps:map(
                    fun(_K, V) -> hb_util:bin(V) end,
                    ExtraHdrs
                ),
            {ok,
                hb_maps:merge(EncodedExtraHdrs, BaseHdrs, Opts),
                hb_message:convert(
                    Message,
                    #{ <<"device">> => Codec, <<"bundle">> => AcceptBundle },
                    <<"structured@1.0">>,
                    Opts#{ topic => ao_internal }
                )
            }
    end.

%% @doc Calculate the codec name to use for a reply given the original parsed 
%% singleton TABM request and the response message. The precidence
%% order for finding the codec is:
%% 1. If the `content-type' field is present in the response message, we always
%%    use `httpsig@1.0', as the device is expected to have already encoded the
%%    message and the `body' field.
%% 2. The `accept-codec' field in the original request.
%% 3. The `accept' field in the original request.
%% 4. The default codec
%% Options can be specified in mime-type format (`application/*') or in
%% AO device format (`device@1.0').
accept_to_codec(OriginalReq, Opts) ->
    accept_to_codec(OriginalReq, undefined, Opts).
accept_to_codec(#{ <<"require-codec">> := RequiredCodec }, _Reply, Opts) ->
    mime_to_codec(RequiredCodec, Opts);
accept_to_codec(_OriginalReq, #{ <<"content-type">> := _ }, _Opts) ->
    <<"httpsig@1.0">>;
accept_to_codec(OriginalReq, _, Opts) ->
    Accept = hb_maps:get(<<"accept">>, OriginalReq, <<"*/*">>, Opts),
    ?event(debug_accept,
        {accept_to_codec,
            {original_req, OriginalReq},
            {accept, Accept}
        }
    ),
    mime_to_codec(Accept, Opts).

%% @doc Find a codec name from a mime-type.
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
mime_to_codec(<<"device/", Name/binary>>, _Opts) -> Name;
mime_to_codec(Device, Opts) ->
    case binary:match(Device, <<"@">>) of
        nomatch -> default_codec(Opts);
        _ -> Device
    end.

%% @doc Return the default codec for the given options.
default_codec(Opts) ->
    hb_opts:get(default_codec, <<"httpsig@1.0">>, Opts).

%% @doc Call the `content-type' key on a message with the given codec, using
%% a fast-path for options that are not needed for this one-time lookup.
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

%% @doc Convert a cowboy request to a normalized message. We first parse the
%% `primitive' message from the request: A message (represented as an Erlang
%% map) of binary keys and values for the request headers and query parameters.
%% We then determine the codec to use for the request, decode it, and merge it
%% overriding the keys of the `primitive' message.
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
            ?event(http, {decoding_body, {codec, Codec}, {body, {string, Body}}}),
            Decoded =
                hb_message:convert(
                    Body,
                    <<"structured@1.0">>,
                    Codec,
                    Opts
                ),
            ReqMessage = hb_maps:merge(PrimitiveMsg, Decoded, Opts),
            ?event(
                {verifying_encoded_message,
                    {codec, Codec},
                    {body, {string, Body}},
                    {decoded, ReqMessage}
                }
            ),
            case hb_message:verify(ReqMessage, all) of
                true ->
                    normalize_unsigned(PrimitiveMsg, Req, ReqMessage, Opts);
                false ->
                    throw({invalid_commitment, ReqMessage})
            end
    end.

%% @doc HTTPSig messages are inherently mixed into the transport layer, so they
%% require special handling in order to be converted to a normalized message.
%% In particular, the signatures are verified if present and required by the 
%% node configuration. Additionally, non-committed fields are removed from the
%% message if it is signed, with the exception of the `path' and `method' fields.
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

%% @doc Add the method and path to a message, if they are not already present.
%% Remove browser-added fields that are unhelpful during processing (for example,
%% `content-length').
%% The precidence order for finding the path is:
%% 1. The path in the message
%% 2. The path in the request URI
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
    {ok, WithCookie} =
        case maps:get(<<"cookie">>, RawHeaders, undefined) of
            undefined -> {ok, BaseMsg};
            Cookie ->
                dev_codec_cookie:from(
                    BaseMsg#{ <<"cookie">> => Cookie },
                    Req,
                    Opts
                )
        end,
    % If the body is empty and unsigned, we remove it.
    NormalBody =
        case hb_maps:get(<<"body">>, WithCookie, undefined, Opts) of
            <<"">> -> hb_message:without_unless_signed(<<"body">>, WithCookie, Opts);
            _ -> WithCookie
        end,
    case hb_maps:get(<<"ao-peer-port">>, NormalBody, undefined, Opts) of
        undefined -> NormalBody;
        P2PPort ->
            % Calculate the peer address from the request. We honor the 
            % `x-real-ip' header if it is present.
            RealIP =
                case hb_maps:get(<<"x-real-ip">>, RawHeaders, undefined, Opts) of
                    undefined ->
                        {{A, B, C, D}, _} = cowboy_req:peer(Req),
                        hb_util:bin(
                            io_lib:format(
                                "~b.~b.~b.~b",
                                [A, B, C, D]
                            )
                        );
                    IP -> IP
                end,
            Peer = <<RealIP/binary, ":", (hb_util:bin(P2PPort))/binary>>,
            (hb_message:without_unless_signed(<<"ao-peer-port">>, NormalBody, Opts))#{
                <<"ao-peer">> => Peer
            }
    end.

%%% Tests

simple_ao_resolve_unsigned_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"/key1">>, <<"key1">> => <<"Value1">> },
    ?assertEqual({ok, <<"Value1">>}, post(URL, TestMsg, #{})).

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

wasm_compute_request(ImageFile, Func, Params) ->
    wasm_compute_request(ImageFile, Func, Params, <<"">>).
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

run_wasm_unsigned_test() ->
    Node = hb_http_server:start_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0]),
    {ok, Res} = post(Node, Msg, #{}),
    ?event({res, Res}),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, #{})).

run_wasm_signed_test() ->
    Opts = #{ priv_wallet => hb:wallet() },
    URL = hb_http_server:start_node(#{force_signed => true}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>),
    {ok, Res} = post(URL, hb_message:commit(Msg, Opts), Opts),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, #{})).

get_deep_unsigned_wasm_state_test() ->
    URL = hb_http_server:start_node(#{force_signed => false}),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>),
    {ok, Res} = post(URL, Msg, #{}),
    ?assertEqual(6.0, hb_ao:get(<<"/output/1">>, Res, #{})).

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

cors_get_test() ->
    URL = hb_http_server:start_node(),
    {ok, Res} = get(URL, <<"/~meta@1.0/info">>, #{}),
    ?assertEqual(
        <<"*">>,
        hb_ao:get(<<"access-control-allow-origin">>, Res, #{})
    ).

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

send_large_signed_request_test() ->
    % Note: If the signature scheme ever changes, we will need to run the 
    % following to get a freshly signed request.
    %    file:write_file(
    %        "test/large-message.eterm",
    %        hb_util:bin(
    %            io_lib:format(
    %               "~p.", 
    %                [
    %                    hb_cache:ensure_all_loaded(hb_message:commit(
    %                        hb_message:uncommitted(hd(hb_util:ok(
    %                            file:consult(<<"test/large-message.eterm">>)
    %                       ))),
    %                        #{ priv_wallet => hb:wallet() }
    %                    ))
    %                ]
    %            )
    %        )
    %    ).
    {ok, [Req]} = file:consult(<<"test/large-message.eterm">>),
    % Get the short trace length from the node message in the large, stored
    % request. 
    ?assertMatch(
        {ok, 5},
        post(
            hb_http_server:start_node(),
            <<"/node-message/short_trace_len">>,
            Req,
            #{ http_client => gun }
        )
    ).

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
