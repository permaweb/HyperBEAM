%%% @doc Hyperbeam's outbound HTTP requests. The functions in this module take a
%%% message request from their caller, send it to a peer, and return the peer's
%%% response in message form. Inbound requests and their replies are handled by
%%% `hb_http_server'.
%%%
%%% We send a request message in the codec named by its `codec-device' key, or
%%% in `httpsig@1.0' if it has none. We add `accept-bundle: true' to the
%%% message, unless the caller has set it. The cookies of the message, from
%%% `priv/cookie' and from its `cookie' and `set-cookie' keys, are sent in a
%%% `cookie' header.
%%%
%%% We read the response in the codec named by its `codec-device' header. If
%%% the response has commitments, we keep only the keys that at least one of
%%% them covers. For example, the `hashpath' of a computed response is kept
%%% when the node's signature covers it. A response without commitments keeps
%%% all of its keys. We add the HTTP status to the message as `status', if it
%%% has no such key. The cookies that the response sets are placed in
%%% `priv/cookie'. If `http-only-result' is set, as it is by default, we return
%%% only the value of the key that the response's `ao-result' names.
-module(hb_http).
-export([start/0]).
-export([get/2, get/3, post/3, post/4, request/2, request/4, request/5]).
-export([message_to_request/2, status_code/2, cookie/4]).
-export([only_result/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

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
            Opts#{ <<"topic">> => ao_internal }
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
    % over that. The URI is the route itself, so the message is not routed
    % again: its own `path' stays as it is, in case it is a committed key.
    {ok, NewMethod, Node, NewPath, NewMsg, NewOpts} =
        route_to_request(
            Message#{ <<"method">> => Method },
            {ok, URI},
            MergedOpts
        ),
    request(NewMethod, Node, NewPath, NewMsg, NewOpts);
request(Method, Peer, Path, RawMessage, Opts) ->
    ?event({request, {method, Method}, {peer, Peer}, {path, Path}, {priv_message, RawMessage}}),
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
    Response = hb_http_client:request(Req, Opts),
    % Process the response.
    EndTime = os:system_time(millisecond),
    Duration = EndTime - StartTime,
    case Response of
        {_ErlStatus, Status, Headers, Body} ->
            ?event(http_outbound,
                {
                    http_response,
                    {priv_req, Req},
                    {priv_response,
                        #{
                            status => Status,
                            headers => Headers,
                            body => Body
                        }
                    }
                },
                Opts
            ),
            request_response(Method, Peer, Path, Response, Duration, Opts);
        Error ->
            Error
    end.

request_response(Method, Peer, Path, Response, Duration, Opts) ->
    {_ErlStatus, Status, RawHeaders, Body} = Response,
    Headers = [{hb_util:to_lower(Key), Value} || {Key, Value} <- RawHeaders],
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
                    cookie(
                        <<"from">>,
                        #{ <<"set-cookie">> => SetCookieLines },
                        #{},
                        Opts
                    ),
                ?event(debug_cookie, {msg_with_cookies, MsgWithCookies}),
                MsgWithCookies
        end,
    HeaderMap = hb_maps:from_list(Headers),
    % A reply to a `HEAD' request, or with a 204 or 304 status, carries no
    % body. Its `content-digest' is that of the body that it omits, so we remove
    % it: the reply decodes without a body, rather than with an empty one.
    BodyHeaderMap =
        case Method == <<"HEAD">> orelse Status == 204 orelse Status == 304 of
            true -> hb_maps:without([<<"content-digest">>], HeaderMap, Opts);
            false -> HeaderMap
        end,
    NormHeaderMap = hb_ao:normalize_keys(BodyHeaderMap, Opts),
    ?event(debug_http_outbound,
        {normalized_response_headers, {norm_header_map, NormHeaderMap}},
        Opts
    ),
    ?event(http_client_short,
        {received,
            {status, Status},
            {duration, Duration},
            {method, Method},
            {peer, Peer},
            {path, {string, Path}},
            {body_size, byte_size(Body)}
        }),
    % Find the codec device from the headers, if set.
    CodecDev =
        hb_maps:get(
            <<"codec-device">>,
            NormHeaderMap,
            <<"httpsig@1.0">>,
            Opts
        ),
    {ResponseStatus, Msg} =
        outbound_result_to_message(
            CodecDev,
            Status,
            NormHeaderMap,
            Body,
            Opts
        ),
    % A reply keeps the keys that any of its commitments covers. The keys that
    % no commitment covers, such as the reply's headers and the node's index
    % page, are removed.
    {ok, Committed} = hb_message:with_only_committed(Msg, any, Opts),
    % A reply in any codec keeps the `status' that it carries, and gets the
    % HTTP status as its `status' if it carries none, and the cookies that it
    % sets.
    WithStatus =
        case is_map(Msg) of
            true ->
                maps:merge(
                    #{ <<"status">> => hb_util:int(Status) },
                    hb_maps:merge(Committed, MaybeSetCookie, Opts)
                );
            false -> Msg
        end,
    % The result may be a link to a message that the peer holds.
    only_result(
        add_peer_stores({ResponseStatus, WithStatus}, Peer, Opts),
        Opts
    ).

%% @doc Return the value that a reply names in its `ao-result' key, if the
%% caller asks for the result alone (`http-only-result', the default). Return
%% any other reply as it is.
only_result({ResponseStatus, Msg}, Opts) ->
    ReturnAOResult =
        hb_opts:get(http_only_result, true, Opts) andalso
        is_map(Msg) andalso
        hb_maps:get(<<"ao-result">>, Msg, false, Opts),
    case ReturnAOResult of
        Key when is_binary(Key) ->
            ?event(
                debug_http_outbound,
                {result_is_single_key, {key, Key}, {msg, Msg}},
                Opts
            ),
            case {Key, hb_maps:get(Key, Msg, undefined, Opts)} of
                {<<"body">>, undefined} ->
                    {ResponseStatus, <<>>};
                {_, undefined} ->
                    {failure,
                        <<
                            "Result key '",
                            Key/binary,
                            "' not found in response."
                        >>
                    };
                {_, Value} ->
                    {ResponseStatus, Value}
            end;
        false ->
            {ResponseStatus, Msg}
    end.

%% @doc Give every link in a response the stores needed to resolve it: the
%% client's own local stores, followed by the peer that served the response.
%% We additionally add the local stores as a cache that the read links should
%% be replicated to on this node, if they are resolved.
add_peer_stores({Status, Res}, Peer, Opts) ->
    LocalStore = hb_opts:get(store, [], hb_store:scope(Opts, local)),
    {
        Status,
        set_link_stores(
            Res,
            LocalStore ++ [
                #{
                    <<"store-module">> => hb_store_remote_node,
                    <<"node">> => Peer,
                    <<"access">> => [<<"read">>],
                    <<"local-store">> => LocalStore
                }
            ]
        )
    }.

%% @doc Add's the given `Stores` value to every link in a message, recursively.
set_link_stores({link, ID, LinkOpts}, Stores) ->
    {link,
        ID,
        LinkOpts#{
            <<"store">> => Stores,
            <<"scope">> => [local, remote]
        }
    };
set_link_stores(Msg, Stores) when is_map(Msg) ->
    maps:map(fun(_Key, Value) -> set_link_stores(Value, Stores) end, Msg);
set_link_stores(Msg, Stores) when is_list(Msg) ->
    lists:map(fun(Value) -> set_link_stores(Value, Stores) end, Msg);
set_link_stores(Value, _Stores) ->
    Value.

%% @doc Convert an HTTP response to a message.
outbound_result_to_message(<<"ans104@1.0">>, Status, Headers, Body, Opts) ->
    ?event(debug_http_outbound,
        {result_is_ans104, {headers, Headers}, {body, Body}},
        Opts
    ),
    try ar_bundles:deserialize(Body) of
        Deserialized ->
            {
                hb_http_client:response_status_to_atom(Status),
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
outbound_result_to_message(<<"tx@1.0">>, Status, _Headers, Body, Opts) ->
    {ok, TABM} = hb_ao:raw(<<"tx@1.0">>, <<"deserialize">>, Body, #{}, Opts),
    {
        hb_http_client:response_status_to_atom(Status),
        hb_message:convert(TABM, <<"structured@1.0">>, tabm, Opts)
    };
outbound_result_to_message(<<"httpsig@1.0">>, Status, Headers, Body, Opts) ->
    ?event(debug_http_outbound, {result_is_httpsig, {body, Body}}, Opts),
    {
        hb_http_client:response_status_to_atom(Status),
        http_response_to_httpsig(Headers, Body, Opts)
    };
outbound_result_to_message(Codec, Status, Headers, Body, Opts) ->
    ?event(debug, {headers, Headers}),
    {
        hb_http_client:response_status_to_atom(Status),
        hb_message:convert(
            Body,
            <<"structured@1.0">>,
            Codec,
            Opts
        )
    }.

%% @doc Convert a HTTP response to a httpsig message.
http_response_to_httpsig(HeaderMap, Body, Opts) ->
    BodyMap = case byte_size(Body) of
        0 when not is_map_key(<<"content-digest">>, HeaderMap) -> #{};
        _ -> #{ <<"body">> => Body }
    end,
    ConvertFrom = 
        hb_maps:merge(
            HeaderMap,
            BodyMap,
			Opts
        ),
    hb_message:convert(
        ConvertFrom,
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        <<"httpsig@1.0">>,
        Opts
    ).

%% @doc The HTTP status of a result: its message's `status' if that is an
%% integer from 100 to 599 or the binary encoding of one. Otherwise an `error'
%% or `failure' whose `body' is an atom with a code of its own takes that code,
%% and any other result the code of its Erlang status.
status_code({ErlStatus, Msg}, Opts) ->
    ParsedStatus =
        hb_util:safe_int(hb_maps:get(<<"status">>, Msg, undefined, Opts)),
    case {ParsedStatus, Msg} of
        {{ok, Status}, _} when Status >= 100, Status =< 599 ->
            Status;
        {_, #{ <<"body">> := Error }} when is_atom(Error), ErlStatus == error;
                is_atom(Error), ErlStatus == failure ->
            case status_code(Error, Opts) of
                200 -> status_code(ErlStatus, Opts);
                Status -> Status
            end;
        _ -> status_code(ErlStatus, Opts)
    end;
status_code(ok, _Opts) -> 200;
status_code(error, _Opts) -> 400;
status_code(created, _Opts) -> 201;
status_code(not_found, _Opts) -> 404;
status_code(client_error, _Opts) -> 400;
status_code(no_viable_responses, _Opts) -> 400;
status_code(failure, _Opts) -> 500;
status_code(unavailable, _Opts) -> 503;
status_code(unauthorized, _Opts) -> 401;
status_code(forbidden, _Opts) -> 403;
status_code(not_authorized, _Opts) -> 403;
status_code(_, _Opts) -> 200.

%% @doc Given a message, return the information needed to make the request.
message_to_request(M, Opts) ->
    % Get the route for the message.
    RouteRes = hb_ao:raw(<<"router@1.0">>, <<"route">>, #{}, M, Opts),
    Res = route_to_request(M, RouteRes, Opts),
    ?event(debug_http, {route_res, {route_res, RouteRes}, {full_res, Res}, {msg, M}}),
    Res.

%% @doc Invoke the cookie device directly.
cookie(Key, Msg, Req, Opts) ->
    hb_ao:raw(<<"cookie@1.0">>, Key, Msg, Req, Opts).

%% @doc Parse a `~router@1.0/route' response and return a tuple of request
%% parameters.
route_to_request(M, {ok, URI}, Opts) when is_binary(URI) ->
    route_to_request(M, {ok, #{ <<"uri">> => URI, <<"opts">> => #{} }}, Opts);
route_to_request(M, {ok, #{ <<"uri">> := XPath, <<"opts">> := ReqOpts}}, Opts) ->
    % The request is a direct HTTP URL, so we need to split the path into a
    % host and path.
    URI = uri_string:parse(XPath),
    ?event(debug_http_outbound, {parsed_uri, {uri, {explicit, URI}}}),
    Method = hb_ao:get(<<"method">>, M, <<"GET">>, Opts),
    % We must remove the path and host from the message, because they are not
    % valid for outbound requests. The path is retrieved from the route, and
    % the host should already be known to the caller. A committed path or host
    % stays in the message: the peer verifies the request as it was signed.
    MsgWithoutMeta =
        hb_message:without_unless_signed([<<"path">>, <<"host">>], M, Opts),
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
    ?event(debug_http_outbound, {parsed_req, {node, Node}, {method, Method}, {path, Path}}),
    {ok, Method, Node, Path, MsgWithoutMeta, hb_util:deep_merge(Opts, ReqOpts, Opts)};
route_to_request(M, {ok, Routes}, Opts) ->
    ?event(debug_http_outbound, {found_routes, {req, M}, {routes, Routes}}),
    % The result is a route, so we leave it to `request' to handle it.
    Path = hb_ao:get(<<"path">>, M, <<"/">>, Opts),
    Method = hb_ao:get(<<"method">>, M, <<"GET">>, Opts),
    % We must remove the path and host from the message, because they are not
    % valid for outbound requests. The path is retrieved from the route, and
    % the host should already be known to the caller. A committed path or host
    % stays in the message: the peer verifies the request as it was signed.
    MsgWithoutMeta =
        hb_message:without_unless_signed([<<"path">>, <<"host">>], M, Opts),
    {ok, Method, Routes, Path, MsgWithoutMeta, Opts};
route_to_request(M, {error, Reason}, _Opts) ->
    {error, {no_viable_route, {reason, Reason}, {message, M}}}.

%% @doc Turn a set of request arguments into a request message, formatted in the
%% preferred format. This function honors the `accept-bundle' option, if it is
%% already present in the message, and sets it to `true' if it is not.
prepare_request(Format, Method, Peer, Path, RawMessage, Opts) ->
    Message = hb_ao:normalize_keys(RawMessage, Opts),
    % Verify the outbound message if paranoid mode is enabled for http_request
    hb_message:paranoid_verify(http_request, Message, Opts),
    % Generate a `cookie' key for the message, if an unencoded cookie is
    % present. The message keeps the cookie keys a commitment covers.
    {MaybeCookie, WithoutCookie} =
        case cookie(<<"extract">>, Message, #{}, Opts) of
            {ok, NoCookies} when map_size(NoCookies) == 0 ->
                {#{}, Message};
            {ok, _Cookies} ->
                {ok, #{ <<"cookie">> := CookieLines }} =
                    cookie(
                        <<"to">>,
                        Message,
                        #{ <<"format">> => <<"cookie">> },
                        Opts
                    ),
                ?event(debug_http, {cookie_lines, CookieLines}),
                {
                    #{ <<"cookie">> => CookieLines },
                    hb_message:without_unless_signed(
                        [<<"cookie">>, <<"set-cookie">>], Message, Opts
                    )
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
    % `port-external' can be set in the node message to override the port that
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
    BinPath = request_path(Path),
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
			?event(debug_http, {request_headers, {explicit, {headers, Headers}}}),
			?event(debug_http, {request_body, {explicit, {body, Body}}}),
            hb_maps:merge(
                ReqBase,
                #{ headers => maps:merge(MaybeCookie, Headers), body => Body },
                Opts
            );
        <<"ans104@1.0">> ->
            ?event(debug_accept, {request_message, {message, Message}}),
            {ok, FilteredMessage} =
                case hb_message:signers(Message, Opts) of
                    [] -> {ok, WithoutPriv};
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

%% @doc Give the request target of a path. An empty first path part (`//')
%% is kept: it is the carried message (see `hb_singleton').
request_path(<<"//", _/binary>> = Path) ->
    <<"/", (hb_path:normalize(hb_path:to_binary(Path)))/binary>>;
request_path(Path) ->
    hb_path:normalize(hb_path:to_binary(Path)).

%%% Tests

test_opts() ->
    #{ <<"store">> => hb_test_utils:test_store(), <<"priv-wallet">> => hb:wallet() }.

isolated_test_opts() ->
    #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new()
    }.

simple_ao_resolve_unsigned_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"//key1">>, <<"key1">> => <<"Value1">> },
    ?assertEqual({ok, <<"Value1">>}, post(URL, TestMsg, test_opts())),
    % The atom value of a successful result is its body, not its status.
    ?assertEqual(
        {ok, forbidden},
        post(URL, #{ <<"path">> => <<"//key2">>, <<"key2">> => forbidden }, test_opts())
    ).

%% @doc An empty body is preserved in signed HTTP requests and responses.
empty_body_http_test() ->
    Origin = isolated_test_opts(),
    Client = isolated_test_opts(),
    Signed = hb_message:commit(#{ <<"body">> => <<>> }, Origin),
    URL = hb_http_server:start_node(Origin),
    {ok, ID} = hb_cache:write(Signed, Origin),
    {ok, Reply} = get(URL, ID, Client#{ <<"http-only-result">> => false }),
    ?assertEqual(<<>>, hb_maps:get(<<"body">>, Reply, missing, Client)),
    ?assertEqual(true, hb_message:deep_verify(Reply, Client)),
    % A reply to `accept-bundle: false' keeps its empty body or its list body
    % as a link: no index page.
    ?assertMatch(
        {ok, #{ <<"body">> := <<>> }},
        get(URL, #{ <<"path">> => ID, <<"accept-bundle">> => false }, Client)
    ),
    {ok, ListID} = hb_cache:write([], Origin),
    ?assertEqual(
        {ok, []},
        get(
            URL,
            #{ <<"path">> => ListID, <<"accept-bundle">> => false },
            Client
        )
    ),
    ?assertEqual({ok, <<>>}, post(URL, <<"//body">>, Signed, Client)).

simple_ao_resolve_signed_test() ->
    URL = hb_http_server:start_node(),
    TestMsg = #{ <<"path">> => <<"//key1">>, <<"key1">> => <<"Value1">> },
    {ok, Res} =
        post(
            URL,
            hb_message:commit(TestMsg, test_opts()),
            test_opts()
        ),
    ?assertEqual(<<"Value1">>, Res).

%% @doc A request signed by the client and relayed through a router node
%% reaches the peer with its commitment intact. The peer stores every signed
%% request it receives, so the stored copy must read back and verify with its
%% committed `path' in place.
relayed_signed_request_test() ->
    Peer =
        hb_http_server:start_node(
            (isolated_test_opts())#{ <<"store-all-signed">> => true }
        ),
    Router =
        hb_http_server:start_node(
            (isolated_test_opts())#{
                <<"routes">> => [
                    #{
                        <<"template">> => <<"/key1">>,
                        <<"nodes">> => [#{ <<"prefix">> => Peer }]
                    }
                ],
                <<"on">> => #{
                    <<"request">> => #{ <<"device">> => <<"relay@1.0">> }
                }
            }
        ),
    ClientOpts = isolated_test_opts(),
    Request =
        hb_message:commit(
            #{ <<"path">> => <<"//key1">>, <<"key1">> => <<"Value1">> },
            ClientOpts
        ),
    ?assertEqual({ok, <<"Value1">>}, post(Router, Request, ClientOpts)),
    % Read the request back from the store that the peer writes signed
    % requests to, by the ID of the client's signature.
    {ok, SignedID, _} =
        hb_message:commitment(
            #{ <<"type">> => <<"rsa-pss-sha512">> },
            Request,
            ClientOpts
        ),
    StoreOpts =
        #{
            <<"store">> =>
                #{
                    <<"store-module">> => hb_store_fs,
                    <<"name">> => <<"cache-http">>
                }
        },
    {ok, Stored} = hb_cache:read(SignedID, StoreOpts),
    Loaded = hb_cache:ensure_all_loaded(Stored, StoreOpts),
    ?assertEqual(
        <<"//key1">>,
        hb_maps:get(<<"path">>, Loaded, not_found, StoreOpts)
    ),
    ?assert(hb_message:verify(Loaded, all, StoreOpts)),
    ?assertEqual(
        hb_message:signers(Request, ClientOpts),
        hb_message:signers(Loaded, StoreOpts)
    ).
%% @doc Routing must retain signed fields even when the HTTP destination differs.
signed_routing_fields_test() ->
    Opts = test_opts(),
    Node = hb_http_server:start_node(),
    Signed = hb_message:commit(#{
        <<"path">> => <<"//value">>,
        <<"host">> => <<"application.example">>,
        <<"method">> => <<"POST">>,
        <<"value">> => <<"preserved">>
    }, Opts),
    Route = #{ <<"uri">> => <<Node/binary, "//value">>, <<"opts">> => #{} },
    lists:foreach(
        fun(Routing) ->
            {ok, _, _, _, Prepared, _} =
                route_to_request(Signed, {ok, Routing}, Opts),
            ?assert(hb_message:verify(Prepared, all, Opts)),
            ?assertEqual(<<"//value">>, maps:get(<<"path">>, Prepared)),
            ?assertEqual(<<"application.example">>, maps:get(<<"host">>, Prepared))
        end,
        [Route, #{ <<"nodes">> => [Route] }]
    ),
    ?assertEqual({ok, <<"preserved">>},
        request(<<"POST">>, Route, <<"/ignored">>, Signed, Opts)).

nested_ao_resolve_test() ->
    URL = hb_http_server:start_node(),
    Opts = #{ <<"store">> => hb_test_utils:test_store(), <<"priv-wallet">> => hb:wallet() },
    {ok, Res} =
        post(
            URL,
            hb_message:commit(
                #{
                    <<"path">> => <<"//key1/key2/key3">>,
                    <<"key1">> =>
                        #{<<"key2">> =>
                            #{
                                <<"key3">> => <<"Value2">>
                            }
                        }
                },
                Opts
            ),
            Opts
        ),
    ?assertEqual(<<"Value2">>, Res).

wasm_compute_request(ImageFile, Func, Params, Opts) ->
    wasm_compute_request(ImageFile, Func, Params, <<"">>, Opts).
wasm_compute_request(ImageFile, Func, Params, ResultPath, Opts) ->
    {ok, Bin} = file:read_file(ImageFile),
    hb_message:commit(
        #{
            <<"path">> => <<"//init/compute/results", ResultPath/binary>>,
            <<"device">> => <<"wasm-64@1.0">>,
            <<"function">> => Func,
            <<"parameters">> => Params,
            <<"body">> => Bin
        },
        Opts
    ).

run_wasm_unsigned_test() ->
    Node = hb_http_server:start_node(#{ <<"force-signed">> => false }),
    LocalOpts = test_opts(),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], LocalOpts),
    {ok, Res} = post(Node, Msg, LocalOpts),
    ?event({res, Res}),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, LocalOpts)).

run_wasm_signed_test() ->
    Opts = test_opts(),
    URL = hb_http_server:start_node(#{ <<"force-signed">> => true }),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>, Opts),
    {ok, Res} = post(URL, hb_message:commit(Msg, Opts), Opts),
    ?assertEqual(6.0, hb_ao:get(<<"output/1">>, Res, Opts)).

get_deep_unsigned_wasm_state_test() ->
    URL = hb_http_server:start_node(#{ <<"force-signed">> => false }),
    LocalOpts = test_opts(),
    Msg = wasm_compute_request(<<"test/test-64.wasm">>, <<"fac">>, [3.0], <<"">>, LocalOpts),
    {ok, Res} = post(URL, Msg, LocalOpts),
    ?assertEqual(6.0, hb_ao:get(<<"/output/1">>, Res, LocalOpts)).

get_deep_signed_wasm_state_test() ->
    URL = hb_http_server:start_node(#{ <<"force-signed">> => true }),
    LocalOpts = test_opts(),
    Msg =
        wasm_compute_request(
            <<"test/test-64.wasm">>,
            <<"fac">>,
            [3.0],
            <<"/output">>,
            LocalOpts
        ),
    {ok, Res} = post(URL, Msg, LocalOpts),
    ?assertEqual(6.0, hb_ao:get(<<"1">>, Res, LocalOpts)).

ans104_wasm_test() ->
    ServerStore = [hb_test_utils:test_store()],
    ServerOpts =
        #{
            <<"force-signed">> => true,
            <<"store">> => ServerStore,
            <<"priv-wallet">> => ar_wallet:new()
        },
    ClientStore = [hb_test_utils:test_store()],
    ClientOpts = #{ <<"store">> => ClientStore, <<"priv-wallet">> => hb:wallet() },
    URL = hb_http_server:start_node(ServerOpts),
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
    ?assertMatch(
        {ok, #{ <<"output">> := [6.0] }},
        post(
            URL,
            Msg#{ <<"path">> => <<"//init/compute/results">> },
            ClientOpts
        )
    ).

nested_signed_bundle_over_http_test() ->
    Parent = self(),
    ServerOpts =
        (isolated_test_opts())#{
            <<"port">> => 0,
            <<"on">> => #{
                <<"request">> => #{
                    <<"device">> => #{
                        <<"request">> =>
                            fun(_, HookReq, HookOpts) ->
                                Parent ! {nested_httpsig_request, HookReq},
                                {error, #{
                                    <<"status">> => 200,
                                    <<"response">> =>
                                        nested_signed_response(HookOpts)
                                }}
                            end
                    }
                }
            }
        },
    ClientOpts =
        (isolated_test_opts())#{ <<"http-only-result">> => false },
    Inner = nested_signed_response(ClientOpts),
    Request =
        hb_message:commit(
            #{
                <<"accept-bundle">> => true,
                <<"codec-device">> => <<"httpsig@1.0">>,
                <<"require-codec">> => <<"httpsig@1.0">>,
                <<"response">> => Inner
            },
            ClientOpts,
            #{
                <<"commitment-device">> => <<"httpsig@1.0">>,
                <<"bundle">> => true
            }
        ),
    {ok, Reply} =
        post(
            hb_http_server:start_node(ServerOpts),
            <<"/~meta@1.0/info">>,
            Request,
            ClientOpts
        ),
    Received =
        receive
            {nested_httpsig_request, #{ <<"request">> := Req }} -> Req
        after 5_000 ->
            error(nested_httpsig_request_timeout)
        end,
    ?assert(hb_message:verify(Received, all, ServerOpts)),
    ReceivedInner = maps:get(<<"response">>, Received),
    % A bundle holds the signed commitments of its nested messages alone.
    ?assertEqual(
        hb_message:with_only_committers(
            Inner,
            hb_message:signers(Inner, ClientOpts),
            ClientOpts
        ),
        ReceivedInner
    ),
    ?assert(hb_message:verify(ReceivedInner, all, ServerOpts)),
    ?assertEqual(
        ReceivedInner,
        maps:get(
            <<"response">>,
            hb_cache:ensure_all_loaded(Received, ServerOpts)
        )
    ),
    ReplyInner = maps:get(<<"response">>, Reply),
    ?assert(hb_message:verify(Reply, all, ClientOpts)),
    ?assert(hb_message:verify(ReplyInner, all, ClientOpts)),
    ?assertEqual(
        ReplyInner,
        hb_cache:ensure_all_loaded(ReplyInner, ClientOpts)
    ).

nested_signed_response(Opts) ->
    Response = #{
        <<"body">> => #{ <<"nested">> => #{ <<"value">> => <<"x">> } },
        <<"status">> => 200
    },
    {ok, _} = hb_cache:write(Response, Opts),
    Unsigned =
        hb_cache:ensure_all_loaded(
            hb_message:commit(
                Response,
                Opts,
                #{
                    <<"commitment-device">> => <<"httpsig@1.0">>,
                    <<"type">> => <<"unsigned">>
                }
            ),
            Opts
        ),
    hb_message:commit(
        Unsigned,
        Opts,
        #{
            <<"commitment-device">> => <<"httpsig@1.0">>,
            <<"bundle">> => true
        }
    ).

%% @doc Ensure that linked submessages in a response can be resolved by
%% reading them back from the peer that served it.
remote_response_links_test() ->
    ServerOpts = #{ <<"store">> => hb_test_utils:test_store() },
    ClientOpts = #{ <<"store">> => hb_test_utils:test_store() },
    {ok, ID} = hb_cache:write(#{ <<"a">> => #{ <<"b">> => 123 } }, ServerOpts),
    {ok, Res} =
        get(
            hb_http_server:start_node(ServerOpts),
            #{
                <<"path">> => ID,
                <<"accept-bundle">> => false
            },
            ClientOpts
        ),
    % The reply's unsigned commitment gives it the ID of the message read.
    ?assertEqual(ID, hb_message:id(Res, unsigned, ClientOpts)),
    % The reply keeps the keys of the message read, and gets its HTTP status.
    ?assertEqual(
        [<<"a">>, <<"commitments">>, <<"status">>],
        lists:sort(maps:keys(Res))
    ),
    ?assertEqual(123, hb_ao:get(<<"a/b">>, Res, ClientOpts)).

send_large_signed_request_test() ->
    Opts = #{ <<"priv-wallet">> => hb:wallet() },
    Req =
        hb_cache:ensure_all_loaded(
            hb_message:commit(
                #{
                    <<"node-message">> => #{
                        <<"short-trace-len">> => 5,
                        <<"large-payload">> =>
                            maps:from_list(
                                [
                                    {
                                        <<"key-", (integer_to_binary(N))/binary>>,
                                        crypto:strong_rand_bytes(1024)
                                    }
                                ||
                                    N <- lists:seq(1, 64)
                                ]
                            )
                    }
                },
                Opts
            ),
            Opts
        ),
    % Get the short trace length from the node message in the large, stored
    % request. 
    ?assertMatch(
        {ok, 5},
        post(
            hb_http_server:start_node(),
            <<"//node-message/short-trace-len">>,
            Req,
            #{ <<"http-client">> => gun }
        )
    ).

%% Test parallel requests
parallel_request_test() ->
    Routes = [
        #{
            % Routes for GraphQL requests to use a remote GraphQL API.
            <<"template">> => <<"/graphql">>,
            <<"parallel">> => true,
            <<"nodes">> =>
                [
                    #{
                        <<"prefix">> => <<"https://ao-search-gateway.goldsky.com">>,
                        <<"opts">> =>
                            #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                    },
                    #{
                        <<"prefix">> => <<"https://arweave-search.goldsky.com">>,
                        <<"opts">> =>
                            #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                    },
                    #{
                        <<"prefix">> => <<"https://arweave.net">>,
                        <<"opts">> =>
                            #{ <<"http-client">> => gun, <<"protocol">> => http2 }
                    }
                ]
            },
            #{
                % Routes for raw data requests to use a remote gateway.
                <<"template">> => <<"/arweave/raw">>,
                <<"node">> =>
                    #{
                        <<"match">> => <<"^/arweave">>,
                        <<"with">> => <<"https://arweave.net">>,
                        <<"opts">> =>
                            #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                    }
            }
    ],
    Store = [
        hb_test_utils:test_store(),
        #{
          <<"store-module">> => hb_store_gateway,
          %% Routes need to be defined in the store, otherwise the code
          %% will fetch the hb_opts:default_message which doesn't have
          %% parallel property.
          <<"routes">> => Routes
         }
    ],
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Node = hb_http_server:start_node(Opts),
    ?assertMatch(
        {ok, #{<<"data">> := <<"1984">>}},
        hb_http:get(
            Node,
            #{<<"path">> => <<"/BOogk_XAI3bvNWnxNxwxmvOfglZt17o4MOVAdPNZ_ew">>},
            Opts
        )
    ).

request_error_handling_test() ->
    Opts = #{},
    NonExistentDomain = <<"http://nonexistent.invalid:80">>,
    Result = hb_http:request(
        <<"GET">>,
        NonExistentDomain,
        <<"/">>,
        #{},
        Opts
    ),
    % The result should be an error tuple, not crash with badmatch
    ?assertMatch({error, _}, Result).
