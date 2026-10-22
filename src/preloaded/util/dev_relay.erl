%%% @doc This module implements the relay device, which is responsible for
%%% relaying messages between nodes and other HTTP(S) endpoints.
%%%
%%% It can be called in either `call' or `cast' mode. In `call' mode, it
%%% returns a `{ok, Result}' tuple, where `Result' is the response from the 
%%% remote peer to the message sent. In `cast' mode, the invocation returns
%%% immediately, and the message is relayed asynchronously. No response is given
%%% and the device returns `{ok, <<"OK">>}'.
%%% 
%%% Example usage:
%%% 
%%% <pre>
%%%     curl /~relay@.1.0/call?method=GET?0.path=https://www.arweave.net/
%%% </pre>
-module(dev_relay).
-specification("../../../specs/relay@1.0.md").
%%% Execute synchronous and asynchronous relay requests.
-export([call/3, cast/3]).
%%% Re-route requests that would be executed locally to other peers, according
%%% to the node's routing table.
-export([request/3]).
-include("include/hb.hrl").
-include("include/hb_opts.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Execute a `call' request using a node's routes.
%% 
%% Supports the following options:
%% - `target': The target message to relay. Defaults to the original message.
%% - `relay-path': The path to relay the message to. Defaults to the original path.
%% - `method': The method to use for the request. Defaults to the original method.
%% - `commit-request': Whether the request should be committed before dispatching.
%% Defaults to `false'.
-spec call(
    #{ _ => _ },
    #{
        target => binary(),
        'relay-path' => binary(),
        method => binary(),
        peer => binary(),
        _ => _
    },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
call(M1, RawM2, Opts) ->
    ?event({relay_call, {m1, M1}, {raw_m2, RawM2}}),
    {ok, BaseTarget} = hb_message:find_target(M1, RawM2, Opts),
    ?event({relay_call, {message_to_relay, BaseTarget}}),
    RelayPath =
        hb_ao:get_first(
            [
                {M1, <<"path">>},
                {{as, <<"message@1.0">>, BaseTarget}, <<"path">>},
                {RawM2, <<"relay-path">>},
                {M1, <<"relay-path">>}
            ],
            undefined,
            Opts
        ),
    do_call(RelayPath, BaseTarget, M1, RawM2, Opts).

%% @doc Perform the full relay call, refusing it if the host it would reach is
%% blocked.
do_call(RelayPath, BaseTarget, M1, RawM2, Opts) ->
    RelayDevice =
        hb_ao:get_first(
            [
                {M1, <<"relay-device">>},
                {{as, <<"message@1.0">>, BaseTarget}, <<"relay-device">>},
                {RawM2, <<"relay-device">>}
            ],
            Opts
        ),
    % A `peer' named in the request is checked against the host policy. A
    % `relay-route' is resolved through the node's own routes, so it names a
    % destination the node's configuration chose and is reached without a
    % check. The message relayed does not choose its own `peer': for the
    % `request' hook it is the inbound request, and the node's routes choose
    % its destination.
    RequestPeer =
        hb_ao:get_first(
            [
                {M1, <<"peer">>},
                {RawM2, <<"peer">>}
            ],
            Opts
        ),
    RelayRoute =
        hb_ao:get_first(
            [
                {M1, <<"relay-route">>},
                {{as, <<"message@1.0">>, BaseTarget}, <<"relay-route">>},
                {RawM2, <<"relay-route">>}
            ],
            not_found,
            Opts
        ),
    RelayMethod =
        hb_ao:get_first(
            [
                {M1, <<"method">>},
                {{as, <<"message@1.0">>, BaseTarget}, <<"method">>},
                {RawM2, <<"relay-method">>},
                {M1, <<"relay-method">>},
                {RawM2, <<"method">>}
            ],
            <<"GET">>,
            Opts
        ),
    RelayBody =
        hb_ao:get_first(
            [
                {M1, <<"body">>},
                {{as, <<"message@1.0">>, BaseTarget}, <<"body">>},
                {RawM2, <<"relay-body">>},
                {M1, <<"relay-body">>},
                {RawM2, <<"body">>}
            ],
            Opts
        ),
    TargetMod1 =
        if RelayBody == not_found -> BaseTarget;
        true -> BaseTarget#{<<"body">> => RelayBody}
        end,
    TargetMod2 =
        TargetMod1#{
            <<"method">> => RelayMethod,
            <<"path">> => RelayPath
        },
    TargetMod3 =
        case RelayDevice of
            not_found -> hb_maps:without([<<"device">>], TargetMod2);
            _ -> TargetMod2#{<<"device">> => RelayDevice}
        end,
    % A relayed request's `multirequest-*' keys configure this node's own
    % multirequest, so remove them: the route alone chooses the peers reached.
    TargetMod4 =
        hb_maps:without(
            hb_http_multi:multirequest_keys(TargetMod3, Opts),
            strip_cookies(TargetMod3, Opts),
            Opts
        ),
    Commit =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, BaseTarget}, <<"commit-request">>},
                {RawM2, <<"relay-commit-request">>},
                {M1, <<"relay-commit-request">>},
                {RawM2, <<"commit-request">>},
                {M1, <<"commit-request">>}
            ],
            false,
            Opts
        ),
    TargetMod5 =
        case hb_util:atom(Commit) of
            true ->
                case hb_opts:get(relay_allow_commit_request, false, Opts) of
                    true ->
                        Committed = hb_message:commit(TargetMod4, Opts),
                        ?event(debug_relay, {relay_recommitted, Committed}, Opts),
                        true = hb_message:verify(Committed, all),
                        Committed;
                    false ->
                        throw(relay_commit_request_not_allowed)
                end;
            false -> TargetMod4
        end,
    ?event(debug_relay, {relay_call, {without_http_params, TargetMod4}}),
    ?event(debug_relay, {relay_call, {with_http_params, TargetMod5}}),
    true = hb_message:verify(TargetMod5),
    ?event(debug_relay, {relay_call, {verified, true}}),
    Client = hb_opts:get(
        relay_http_client,
        hb_opts:get(http_client, ?DEFAULT_HTTP_CLIENT, Opts),
        Opts
    ),
    % `hb_http:request/2' finds the peer and dispatches the request, unless the
    % peer is explicitly given. Redirects are not followed, so the host checked
    % here is the only host contacted.
    HTTPOpts =
        Opts#{
            <<"http-client">> => Client,
            <<"http-only-result">> => false,
            <<"http-redirects">> => 0
        },
    % Decide where the request goes and whether the host was chosen by the
    % node. A `relay-route' the node resolves through its own routes is trusted.
    % A request `peer' or absolute `relay-path' is checked; a relative
    % `relay-path' names no host and is routed by `hb_http:request/2', which
    % fails closed when no route matches.
    {RelayPeer, Destination, RouteOpts} =
        case RelayRoute of
            not_found ->
                {RequestPeer, relay_destination(RequestPeer, RelayPath), HTTPOpts};
            _ ->
                {Node, NodeOpts} = route_node(RelayRoute, RelayMethod, HTTPOpts),
                {Node, undefined, NodeOpts}
        end,
    RequestOpts = RouteOpts#{ <<"http-redirects">> => 0 },
    case is_blocked_host(Destination, Opts) of
        true -> {error, blocked_host};
        false ->
            Res =
                case RelayPeer of
                    not_found ->
                        hb_http:request(TargetMod5, RequestOpts);
                    _ ->
                        ?event(debug_relay, {relaying_to_peer, RelayPeer}),
                        hb_http:request(
                            RelayMethod,
                            RelayPeer,
                            RelayPath,
                            TargetMod5,
                            RequestOpts
                        )
                end,
            case Res of
                {ok, R} ->
                    % A reply that carries commitments is the message that the
                    % peer sent: the result keeps the keys that they cover, so
                    % the `status' that the client adds to the reply is not one.
                    {ok, Sent} = hb_message:with_only_committed(R, any, Opts),
                    % The reply's links name messages in the peer's stores, and
                    % a client reads them from this node: load them here.
                    Loaded = hb_cache:ensure_all_loaded(Sent, Opts),
                    {ok, strip_cookies(Loaded, Opts)};
                Err -> Err
            end
    end.

%% @doc The host the relay will contact: the `peer' when one is named,
%% otherwise an absolute `relay-path'. A relative path names no host, so the
%% node routes it through its own routes and there is nothing for
%% `is_blocked_host/2' to vet: return `undefined'.
relay_destination(not_found, <<"http://", _/binary>> = Path) -> Path;
relay_destination(not_found, <<"https://", _/binary>> = Path) -> Path;
relay_destination(not_found, _RelativePath) -> undefined;
relay_destination(Peer, _RelayPath) -> Peer.

%% @doc The node that the node's own routes resolve `Route' to: the destination
%% the node's configuration chose for that path, with its request options.
%% Returns `{not_found, Opts}' when no route matches, leaving no peer to contact.
route_node(Route, Method, Opts) ->
    case hb_http:message_to_request(
            #{ <<"path">> => Route, <<"method">> => Method },
            Opts
        ) of
        {ok, _Method, Node, _Path, _Msg, NodeOpts} -> {Node, NodeOpts};
        {error, _} -> {not_found, Opts}
    end.

%% @doc Ensure that cookies are not forwarded either to or from the relayed
%% node.
strip_cookies(Msg, Opts) ->
    hb_private:set(
        hb_maps:without([<<"cookie">>, <<"set-cookie">>], Msg, Opts),
        <<"cookie">>,
        unset,
        Opts
    ).

%% @doc Returns `true` if the host named by `URI` is blocked by the relay's
%% allowed hosts configuration, or if `URI' names a host that cannot be
%% determined. `undefined' names no host to vet and is not blocked.
%%
%% The configuration supports:
%% 1. Blocking internal hosts (e.g. `localhost`, `127.0.0.1`, etc.) if the
%%    `relay-block-internal` option is set to `true` (default: `true`).
%% 2. Allowing access to a list of specific hosts by hostname or IP address,
%%    provided by the `relay-allowed-hosts` option.
is_blocked_host(URI, Opts) ->
    maybe
        true ?= (URI =/= undefined) orelse skip,
        {ok, Host} ?= hb_hostname:uri_host(URI),
        AllowedHosts = hb_opts:get(relay_allowed_hosts, any, Opts),
        true ?=
            (AllowedHosts =:= any) orelse
                lists:any(
                    fun(Entry) -> host_matches(Host, Entry) end,
                    AllowedHosts
                ),
        true ?= hb_opts:get(relay_block_internal, true, Opts) orelse skip,
        try not hb_hostname:is_public(Host, Opts)
        catch _:_ -> true
        end
    else
        skip -> false;
        _ -> true
    end.

%% @doc Ensure that a given hostname either fully matches, or matches a
%% namespace-delimited suffix.
host_matches(_Host, Entry) when not is_binary(Entry) ->
    throw(relay_invalid_allowed_host);
host_matches(Host, Entry) ->
    case hb_hostname:normalize(Entry) of
        SuffixSeg = <<".", _/binary>> ->
            binary:longest_common_suffix([Host, SuffixSeg])
                =:= byte_size(SuffixSeg);
        NormalEntry ->
            Host =:= NormalEntry
    end.

%% @doc Execute a request in the same way as `call/3', but asynchronously. Always
%% returns `<<"OK">>'.
-spec cast(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) -> {ok, binary()}.
cast(M1, M2, Opts) ->
    spawn(fun() -> call(M1, M2, Opts) end),
    {ok, <<"OK">>}.

%% @doc Preprocess a request to check if it should be relayed to a different node.
-spec request(#{ _ => _ }, #{ request := #{ _ => _ }, _ => _ }, #{ _ => _ }) ->
    {ok, #{ body := [#{ _ => _ }], _ => _ }}.
request(_Base, Req, Opts) ->
    {ok,
        #{
            <<"body">> =>
                [
                    #{ <<"device">> => <<"relay@1.0">> },
                    #{
                        <<"path">> => <<"call">>,
                        <<"target">> => <<"proxy-message">>,
                        <<"proxy-message">> =>
                            hb_util:ok(hb_maps:find(<<"request">>, Req, Opts))
                    }
                ]
        }
    }.


%%% Tests

internal_host_block_test() ->
    lists:foreach(
        fun(URL) -> ?assert(is_blocked_host(URL, #{})) end,
        [
            <<"http://localhost/">>,
            <<"http://localhost./">>,
            <<"http://127.0.0.1/">>,
            <<"http://127.1/">>,
            <<"http://2130706433/">>,
            <<"http://0x7f000001/">>,
            <<"http://0177.0.0.1/">>,
            <<"http://0/">>,
            <<"http://0.0.0.0/">>,
            <<"http://10.0.0.1/">>,
            <<"http://172.16.0.1/">>,
            <<"http://192.168.0.1/">>,
            <<"http://169.254.169.254/">>,
            <<"http://[::]/">>,
            <<"http://[::1]/">>,
            <<"http://[::ffff:127.0.0.1]/">>,
            <<"http://[fd00:ec2::254]/">>,
            <<"http://[fd20:ce::254]/">>,
            <<"http://[fe80::1]/">>
        ]
    ),
    ?assertEqual(false, is_blocked_host(<<"https://1.1.1.1/">>, #{})),
    ?assertEqual(false, is_blocked_host(<<"https://[2606:4700:4700::1111]/">>, #{})),
    % `undefined' names no host to vet, so it is not blocked: a relative
    % `relay-path' is routed by the node, not checked here. A value that names
    % no resolvable host fails closed.
    ?assertEqual(false, is_blocked_host(undefined, #{})),
    ?assert(is_blocked_host(<<"/arweave/info">>, #{})),
    ?assertEqual(
        false,
        is_blocked_host(
            <<"http://localhost/">>,
            #{ <<"relay-block-internal">> => false }
        )
    ).

relay_host_allowlist_test() ->
    ?assertNot(
        is_blocked_host(
            <<"https://example.com/">>,
            #{ <<"relay-allowed-hosts">> => [<<"example.com">>] }
        )
    ),
    ?assertNot(
        is_blocked_host(
            <<"https://www.example.com/">>,
            #{ <<"relay-allowed-hosts">> => [<<".example.com">>] }
        )
    ),
    ?assert(
        is_blocked_host(
            <<"https://example.com/">>,
            #{ <<"relay-allowed-hosts">> => [<<"arweave.net">>] }
        )
    ),
    ?assert(
        is_blocked_host(
            <<"https://example.com/">>,
            #{ <<"relay-allowed-hosts">> => [<<"https://example.com">>] }
        )
    ),
    ?assert(
        is_blocked_host(
            <<"http://127.0.0.1/">>,
            #{ <<"relay-allowed-hosts">> => [<<"127.0.0.1">>] }
        )
    ).

call_get_test() ->
    application:ensure_all_started([hb]),
    {ok, #{<<"body">> := Body}} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"relay@1.0">>,
                <<"method">> => <<"GET">>,
                <<"path">> => <<"https://www.google.com/">>
            },
            <<"call">>,
            #{ <<"protocol">> => http2 }
        ),
    ?assert(byte_size(Body) > 10_000).

relay_nearest_test() ->
    Peer1 = hb_http_server:start_node(#{ <<"priv-wallet">> => W1 = ar_wallet:new() }),
    Peer2 = hb_http_server:start_node(#{ <<"priv-wallet">> => W2 = ar_wallet:new() }),
    Address1 = hb_util:human_id(ar_wallet:to_address(W1)),
    Address2 = hb_util:human_id(ar_wallet:to_address(W2)),
    Peers = [Address1, Address2],
    Node =
        hb_http_server:start_node(Opts = #{
            <<"store">> => hb_opts:get(store),
            <<"priv-wallet">> => ar_wallet:new(),
            <<"routes">> => [
                #{
                    <<"template">> => <<"/.*">>,
                    <<"strategy">> => <<"Nearest">>,
                    <<"nodes">> => [
                        #{
                            <<"prefix">> => Peer1,
                            <<"wallet">> => Address1
                        },
                        #{
                            <<"prefix">> => Peer2,
                            <<"wallet">> => Address2
                        }
                    ]
                }
            ]
        }),
    {ok, RelayRes} =
        hb_http:get(
            Node,
            <<"/~relay@1.0/call?relay-path=/~meta@1.0/info/address">>,
            Opts#{ <<"http-only-result">> => false }
        ),
    ?event(
        {relay_res,
            {response, RelayRes},
            {signer, hb_message:signers(RelayRes, Opts)},
            {peers, Peers}
        }
    ),
    ?assert(lists:member(hb_ao:get(<<"body">>, RelayRes, Opts), Peers)).

%% @doc Test that a `relay@1.0/call' correctly commits requests as specified.
%% We validate this by configuring two nodes: One that will execute a given
%% request from a user, but only if the request is committed. The other node
%% re-routes all requests to the first node, using `call`'s `commit-request'
%% key to sign the request during proxying. The initial request is not signed,
%% such that the first node would otherwise reject the request outright.
commit_request_test() ->
    Port = 10000 + rand:uniform(10000),
    Wallet = ar_wallet:new(),
    Executor =
        hb_http_server:start_node(
            #{ <<"port">> => Port }
        ),
    Node =
        hb_http_server:start_node(#{
            <<"priv-wallet">> => Wallet,
            <<"relay-allow-commit-request">> => true,
            <<"routes">> =>
                [
                    #{
                        <<"template">> => <<"/test-key">>,
                        <<"strategy">> => <<"Nearest">>,
                        <<"nodes">> => [
                            #{
                                <<"wallet">> => hb:address(Wallet),
                                <<"prefix">> => Executor
                            }
                        ]
                    }
                ],
            <<"on">> => #{
                <<"request">> =>
                    #{
                        <<"device">> => <<"router@1.0">>,
                        <<"path">> => <<"preprocess">>,
                        <<"commit-request">> => true
                    }
                }
        }),
    {ok, Res} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"//test-key">>,
                <<"test-key">> => <<"value">>
            },
            #{}
        ),
    ?event({res, Res}),
    ?assertEqual(<<"value">>, Res).

%% @doc Start a node to relay to, returning its location and address. The node
%% binds to an internal host, so the relay blocks it unless it is permitted.
start_relay_target() ->
    Wallet = ar_wallet:new(),
    Node = hb_http_server:start_node(#{ <<"priv-wallet">> => Wallet }),
    {Node, hb_util:human_id(ar_wallet:to_address(Wallet))}.

%% @doc Resolve a `relay@1.0/call' with the given request fields and node
%% options, returning the relay's response without the HTTP-only wrapping.
relay_call(Fields, Opts) ->
    hb_ao:resolve(
        Fields#{ <<"device">> => <<"relay@1.0">> },
        <<"call">>,
        Opts#{ <<"http-only-result">> => false }
    ).

%% @doc A `call' with an internal `peer' is refused by default: the peer names
%% the host the request will reach.
relay_peer_internal_blocked_test() ->
    {Target, _Address} = start_relay_target(),
    ?assertEqual(
        {error, blocked_host},
        relay_call(
            #{
                <<"peer">> => Target,
                <<"relay-path">> => <<"/~meta@1.0/info/address">>
            },
            #{}
        )
    ).

%% @doc The same internal `peer' is reached once internal hosts are permitted.
relay_peer_allowed_test() ->
    {Target, Address} = start_relay_target(),
    {ok, Res} =
        relay_call(
            #{
                <<"peer">> => Target,
                <<"relay-path">> => <<"/~meta@1.0/info/address">>
            },
            #{ <<"relay-block-internal">> => false }
        ),
    ?assertEqual(Address, hb_ao:get(<<"body">>, Res, #{})),
    % The result is the peer's message, without the `status' the client adds.
    ?assertNot(maps:is_key(<<"status">>, Res)).

%% @doc A `call' whose `relay-path' is an absolute URL on an internal host is
%% refused by default.
relay_absolute_internal_blocked_test() ->
    {Target, _Address} = start_relay_target(),
    ?assertEqual(
        {error, blocked_host},
        relay_call(
            #{ <<"relay-path">> => <<Target/binary, "~meta@1.0/info/address">> },
            #{}
        )
    ).

%% @doc The same absolute URL is reached once internal hosts are permitted.
relay_absolute_allowed_test() ->
    {Target, Address} = start_relay_target(),
    {ok, Res} =
        relay_call(
            #{ <<"relay-path">> => <<Target/binary, "~meta@1.0/info/address">> },
            #{ <<"relay-block-internal">> => false }
        ),
    ?assertEqual(Address, hb_ao:get(<<"body">>, Res, #{})).

%% @doc A relative `relay-path' names no host: the node routes it through its
%% own routes. A route to an internal host is the operator's own, so the
%% request is reached even with internal hosts blocked.
relay_relative_routed_test() ->
    {Target, Address} = start_relay_target(),
    Relay =
        hb_http_server:start_node(#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"routes">> =>
                [
                    #{
                        <<"template">> => <<"/.*">>,
                        <<"node">> => #{ <<"prefix">> => Target }
                    }
                ]
        }),
    {ok, Res} =
        hb_http:get(
            Relay,
            <<"/~relay@1.0/call?relay-path=/~meta@1.0/info/address">>,
            #{ <<"http-only-result">> => false }
        ),
    ?assertEqual(Address, hb_ao:get(<<"body">>, Res, #{})).

%% @doc A relayed request's `multirequest-*' keys configure the node's own
%% multirequest, so the relay removes them: a request on a `nodes' route cannot
%% choose the peers it is sent to, only the route's own nodes are reached.
relay_relative_multirequest_nodes_blocked_test() ->
    {Target, Address} = start_relay_target(),
    {Evil, _EvilAddress} = start_relay_target(),
    {ok, Res} =
        relay_call(
            #{
                <<"relay-path">> => <<"/~meta@1.0/info/address">>,
                <<"multirequest-nodes">> =>
                    [#{
                        <<"uri">> => <<Evil/binary, "~meta@1.0/info/address">>,
                        <<"opts">> => #{ <<"http-client">> => <<"httpc">> }
                    }]
            },
            #{
                <<"routes">> =>
                    [#{
                        <<"template">> => <<"/.*">>,
                        <<"nodes">> => [#{ <<"prefix">> => Target }]
                    }]
            }
        ),
    ?assertEqual(Address, hb_ao:get(<<"body">>, Res, #{})).

%% @doc A relative `relay-path' that matches no route reaches no host: the
%% request is refused rather than relayed.
relay_relative_unroutable_refused_test() ->
    Relay =
        hb_http_server:start_node(#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"routes">> =>
                [
                    #{
                        <<"template">> => <<"/only-this">>,
                        <<"node">> => #{ <<"prefix">> => <<"https://arweave.net">> }
                    }
                ]
        }),
    Res =
        hb_http:get(
            Relay,
            <<"/~relay@1.0/call?relay-path=/no-matching-route">>,
            #{ <<"http-only-result">> => false }
        ),
    ?assertMatch({failure, _}, Res).

%% @doc A `call' whose `peer' names no resolvable host is refused: the check
%% fails closed rather than contacting an unknown host.
relay_undeterminable_host_blocked_test() ->
    ?assertEqual(
        {error, blocked_host},
        relay_call(
            #{
                <<"peer">> => <<"http://">>,
                <<"relay-path">> => <<"/~meta@1.0/info/address">>
            },
            #{}
        )
    ).
