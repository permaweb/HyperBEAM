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
%%% Execute synchronous and asynchronous relay requests.
-export([call/3, cast/3]).
%%% Re-route requests that would be executed locally to other peers, according
%%% to the node's routing table.
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Execute a `call' request using a node's routes.
%% 
%% Supports the following options:
%% - `target': The target message to relay. Defaults to the original message.
%% - `relay-path': The path to relay the message to. Defaults to the original path.
%% - `method': The method to use for the request. Defaults to the original method.
%% - `commit-request': Whether the request should be committed before dispatching.
%% Defaults to `false'.
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
            Opts
        ),
    RelayDevice =
        hb_ao:get_first(
            [
                {M1, <<"relay-device">>},
                {{as, <<"message@1.0">>, BaseTarget}, <<"relay-device">>},
                {RawM2, <<"relay-device">>}
            ],
            Opts
        ),
    RelayPeer =
        hb_ao:get_first(
            [
                {M1, <<"peer">>},
                {{as, <<"message@1.0">>, BaseTarget}, <<"peer">>},
                {RawM2, <<"peer">>}
            ],
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
    TargetMod4 = 
        hb_maps:without(
            [<<"commitments">>],
            TargetMod3,
            Opts
        ),
    TargetMod5 =
        case Commit of
            true ->
                case hb_opts:get(relay_allow_commit_request, false, Opts) of
                    true ->
                        ?event(debug_relay, {recommitting, TargetMod4}, Opts),
                        Committed = hb_message:commit(TargetMod4, Opts),
                        ?event(debug_relay, {relay_call, {committed, Committed}}, Opts),
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
    RequestMethod =
        hb_maps:get(<<"method">>, TargetMod5, RelayMethod, Opts),
    Client =
        case hb_maps:get(<<"http-client">>, BaseTarget, not_found, Opts) of
            not_found -> hb_opts:get(relay_http_client, Opts);
            RequestedClient -> RequestedClient
        end,
    % Let `hb_http:request/2' handle finding the peer and dispatching the
    % request, unless the peer is explicitly given.
    HTTPOpts = Opts#{ http_client => Client, http_only_result => false },
    Res = case RelayPeer of
        not_found ->
            hb_http:request(TargetMod5, HTTPOpts);
        _ ->
            case hb_ao:get(<<"nodes">>, RelayPeer, not_found, Opts) of
                not_found ->
                    ?event(debug_relay, {relaying_to_peer, RelayPeer}),
                    hb_http:request(
                        RequestMethod,
                        RelayPeer,
                        RelayPath,
                        TargetMod5,
                        HTTPOpts
                    );
                Nodes when is_list(Nodes) ->
                    relay_nodes_in_order(
                        hb_util:message_to_ordered_list(Nodes, Opts),
                        RequestMethod,
                        RelayPath,
                        TargetMod5,
                        HTTPOpts,
                        Opts
                    );
                _ ->
                    ?event(debug_relay, {relaying_to_peer, RelayPeer}),
                    hb_http:request(
                        RequestMethod,
                        RelayPeer,
                        RelayPath,
                        TargetMod5,
                        HTTPOpts
                    )
            end
    end,
    case Res of
        {ok, R} ->
            {ok, hb_maps:without([<<"set-cookie">>], R)};
        Err -> Err
    end.


%% @doc Execute a request in the same way as `call/3', but asynchronously. Always
%% returns `<<"OK">>'.
cast(M1, M2, Opts) ->
    spawn(fun() -> call(M1, M2, Opts) end),
    {ok, <<"OK">>}.

%% @doc Preprocess a request to check if it should be relayed to a different node.
request(_Base, Req, Opts) ->
    {ok,
        #{
            <<"body">> =>
                [
                    #{ <<"device">> => <<"relay@1.0">> },
                    #{
                        <<"path">> => <<"call">>,
                        <<"target">> => <<"body">>,
                        <<"body">> =>
                            hb_ao:get(<<"request">>, Req, Opts#{ hashpath => ignore })
                    }
                ]
        }
    }.

%% @doc Try each node in order, respecting per-node HTTP timeouts. Stops at the
%% first admissible response or when all nodes fail/time out.
relay_nodes_in_order([], _Method, _Path, _Message, _HTTPOpts, _Opts) ->
    {error, no_viable_responses};
relay_nodes_in_order(
        [Node|Rest],
        Method,
        Path,
        Message,
        HTTPOpts,
        Opts
    ) ->
    case hb_ao:get(<<"prefix">>, Node, not_found, Opts) of
        not_found ->
            relay_nodes_in_order(
                Rest,
                Method,
                Path,
                Message,
                HTTPOpts,
                Opts
            );
        Peer ->
            {PeerTimeout, HTTPOpts1} = peer_http_opts(Node, HTTPOpts, Opts),
            ?event(debug_relay, {relaying_to_peer, Peer}),
            RequestFun =
                fun() ->
                    hb_http:request(Method, Peer, Path, Message, HTTPOpts1)
                end,
            case relay_request_with_timeout(RequestFun, PeerTimeout) of
                {ok, Res} ->
                    case relay_response_ok(Res, Opts) of
                        true -> {ok, Res};
                        false ->
                            relay_nodes_in_order(
                                Rest,
                                Method,
                                Path,
                                Message,
                                HTTPOpts,
                                Opts
                            )
                    end;
                {error, _Reason} ->
                    relay_nodes_in_order(
                        Rest,
                        Method,
                        Path,
                        Message,
                        HTTPOpts,
                        Opts
                    )
            end
    end.

relay_response_ok(Res, Opts) ->
    Status = hb_util:int(hb_ao:get(<<"status">>, Res, 500, Opts)),
    Status < 400.

%% @doc Run a request with an optional hard timeout. When no timeout is provided
%% the request executes in the caller; otherwise we spawn and kill the worker if
%% it exceeds the limit.
relay_request_with_timeout(
        RequestFun,
        Timeout
    ) when Timeout == not_found; Timeout == undefined ->
    RequestFun();
relay_request_with_timeout(RequestFun, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Worker =
        spawn(fun() ->
            Parent ! {Ref, RequestFun()}
        end),
    receive
        {Ref, Res} -> Res
    after Timeout ->
        exit(Worker, kill),
        {error, relay_peer_timeout}
    end.

peer_http_opts(Node, HTTPOpts, Opts) ->
    NodeOpts =
        case hb_maps:get(<<"opts">>, Node, #{}, Opts) of
            Map when is_map(Map) -> Map;
            _ -> #{}
        end,
    Normalized = hb_opts:mimic_default_types(NodeOpts, new_atoms, Opts),
    case peer_timeout(Node, NodeOpts, Opts) of
        not_found ->
            {not_found, maps:merge(HTTPOpts, Normalized)};
        Timeout ->
            TimeoutMs = hb_util:int(Timeout),
            {
                TimeoutMs,
                maps:merge(
                    HTTPOpts,
                    Normalized#{
                        http_request_send_timeout => TimeoutMs,
                        http_connect_timeout => TimeoutMs
                    }
                )
            }
    end.

peer_timeout(Node, NodeOpts, Opts) ->
    case hb_ao:get(<<"http-timeout">>, Node, not_found, Opts) of
        not_found ->
            hb_maps:get(<<"http-timeout">>, NodeOpts, not_found, Opts);
        Timeout -> Timeout
    end.


%%% Tests

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
            #{ protocol => http2 }
        ),
    ?assertEqual(true, byte_size(Body) > 10_000).

relay_nearest_test() ->
    Peer1 = hb_http_server:start_node(#{ priv_wallet => W1 = ar_wallet:new() }),
    Peer2 = hb_http_server:start_node(#{ priv_wallet => W2 = ar_wallet:new() }),
    Address1 = hb_util:human_id(ar_wallet:to_address(W1)),
    Address2 = hb_util:human_id(ar_wallet:to_address(W2)),
    Peers = [Address1, Address2],
    Node =
        hb_http_server:start_node(Opts = #{
            store => hb_opts:get(store),
            priv_wallet => ar_wallet:new(),
            routes => [
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
            <<"/~relay@1.0/call?relay-path=/~meta@1.0/info">>,
            Opts#{ http_only_result => false }
        ),
    ?event(
        {relay_res,
            {response, RelayRes},
            {signer, hb_message:signers(RelayRes, Opts)},
            {peers, Peers}
        }
    ),
    HasValidSigner =
        lists:any(
            fun(Peer) ->
                lists:member(Peer, hb_message:signers(RelayRes, Opts))
            end,
            Peers
        ),
    ?assert(HasValidSigner).

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
            #{
                port => Port,
                force_signed_requests => true
            }
        ),
    Node =
        hb_http_server:start_node(#{
            priv_wallet => Wallet,
            relay_allow_commit_request => true,
            routes =>
                [
                    #{
                        <<"template">> => <<"/test-key">>,
                        <<"strategy">> => <<"Nearest">>,
                        <<"nodes">> => [
                            #{
                                <<"wallet">> => hb_util:human_id(Wallet),
                                <<"prefix">> => Executor
                            }
                        ]
                    }
                ],
            on => #{
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
                <<"path">> => <<"test-key">>,
                <<"test-key">> => <<"value">>
            },
            #{}
        ),
    ?event({res, Res}),
    ?assertEqual(<<"value">>, Res).

relay_failover_test() ->
    application:ensure_all_started([hb]),
    PeerWallet = ar_wallet:new(),
    RelayWallet = ar_wallet:new(),
    Peer = hb_http_server:start_node(#{ priv_wallet => PeerWallet }),
    Node =
        hb_http_server:start_node(NodeOpts = #{
            relay_allow_commit_request => true,
            priv_wallet => RelayWallet,
            routes =>
                [
                    #{
                        <<"template">> => <<"/~meta@1.0/info.*">>,
                        <<"nodes">> => [
                            #{
                                % Remote peer used to exercise timeout-driven
                                % failover. When Google one day runs HB, we can
                                % lower this again.
                                <<"prefix">> => <<"http://google.com/">>,
                                <<"http-timeout">> => 10000
                            },
                            #{
                                <<"prefix">> => <<"http://doesnotroute.invalid/">>,
                                <<"http-timeout">> => 2000
                            },
                            #{
                                % Local peer that should eventually succeed.
                                <<"prefix">> => Peer,
                                <<"http-timeout">> => 5000
                            }
                        ]
                    }
                ],
            on => #{
                <<"request">> =>
                    #{
                        <<"device">> => <<"router@1.0">>,
                        <<"path">> => <<"preprocess">>,
                        <<"commit-request">> => true
                    }
                }
        }),
    % Validate that the server can forward requests through the `hb_http:get` API.
    {ok, DirectRecvdAddr} =
        hb_http:request(
            #{ <<"path">> => <<"~meta@1.0/info/address">> },
            NodeOpts
        ),
    ?assertEqual(hb_util:human_id(PeerWallet), DirectRecvdAddr),
    % Validate that the relay device is able to forward requests to the peer.
    {ok, RelayRecvdAddr} =
        hb_http:get(
            Node,
            <<"~relay@1.0/call?relay-path=~meta@1.0/info/address">>,
            #{}
        ),
    ?assertEqual(hb_util:human_id(PeerWallet), RelayRecvdAddr),
    ?hr(),
    timer:sleep(100),
    % Validate that the server forwards requests from clients to the peer.
    {ok, ClientRecvdAddr} = hb_http:get(Node, <<"~meta@1.0/info/address">>, #{}),
    ?assertEqual(hb_util:human_id(PeerWallet), ClientRecvdAddr).