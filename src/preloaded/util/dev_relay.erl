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
    TargetMod4 = sanitize_request(TargetMod3, Opts),
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
    maybe_block_internal_host(RelayPath, Opts),
    Client = hb_opts:get(relay_http_client, Opts),
    % Let `hb_http:request/2' handle finding the peer and dispatching the
    % request, unless the peer is explicitly given.
    HTTPOpts = Opts#{ <<"http-client">> => Client, <<"http-only-result">> => false },
    Res = case RelayPeer of
        not_found ->
            hb_http:request(TargetMod5, HTTPOpts);
        _ ->
            ?event(debug_relay, {relaying_to_peer, RelayPeer}),
            hb_http:request(
                RelayMethod,
                RelayPeer,
                RelayPath,
                TargetMod5,
                HTTPOpts
            )
    end,
    case Res of
        {ok, R} ->
            {ok, sanitize_response(R, Opts)};
        Err -> Err
    end.


sanitize_request(Msg, Opts) ->
    hb_private:set(
        hb_maps:without([<<"commitments">>, <<"cookie">>, <<"set-cookie">>], Msg, Opts),
        <<"cookie">>,
        unset,
        Opts
    ).

sanitize_response(Msg, Opts) ->
    hb_private:set(
        hb_maps:without([<<"cookie">>, <<"set-cookie">>], Msg, Opts),
        <<"cookie">>,
        unset,
        Opts
    ).

maybe_block_internal_host(URI, Opts) ->
    Default = hb_opts:get(relay_block_localhost, true, Opts),
    case hb_opts:get(relay_block_internal_hosts, Default, Opts) of
        true -> block_internal_host(URI, Opts);
        _ -> ok
    end.

block_internal_host(not_found, _Opts) -> ok;
block_internal_host(URI, Opts) when is_binary(URI) ->
    case hb_hostname:uri_host(URI) of
        not_found -> ok;
        Host ->
            maybe_allow_host(Host, Opts),
            try hb_hostname:is_public(Host, Opts) of
                true -> ok;
                false -> throw(relay_internal_host_not_allowed)
            catch
                throw:invalid_dns_server -> throw(relay_invalid_dns_resolver)
            end
    end;
block_internal_host(_, _Opts) -> ok.

maybe_allow_host(Host, Opts) ->
    case hb_opts:get(relay_allowed_hosts, false, Opts) of
        false -> ok;
        Hosts ->
            case
                lists:any(
                    fun(Entry) -> host_matches(Host, Entry) end,
                    host_list(Hosts)
                )
            of
                true -> ok;
                false -> throw(relay_host_not_allowed)
            end
    end.

host_list(Host) when is_binary(Host) -> [Host];
host_list(Hosts) when is_list(Hosts) -> Hosts;
host_list(_) -> throw(relay_invalid_allowed_host).

host_matches(_Host, Entry) when not is_binary(Entry) ->
    throw(relay_invalid_allowed_host);
host_matches(Host, Entry) ->
    case hb_hostname:normalize(Entry) of
        <<".", Suffix/binary>> ->
            Host =:= Suffix orelse has_host_suffix(Host, Suffix);
        NormalEntry ->
            Host =:= NormalEntry
    end.

has_host_suffix(Host, Suffix) when byte_size(Host) =< byte_size(Suffix) ->
    false;
has_host_suffix(Host, Suffix) ->
    Pos = byte_size(Host) - byte_size(Suffix) - 1,
    binary:part(Host, Pos, byte_size(Suffix) + 1) =:= <<".", Suffix/binary>>.

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
                            hb_ao:get(<<"request">>, Req, Opts#{ <<"hashpath">> => ignore })
                    }
                ]
        }
    }.


%%% Tests

internal_host_block_test() ->
    lists:foreach(
        fun(URL) ->
            ?assertThrow(
                relay_internal_host_not_allowed,
                block_internal_host(URL, #{})
            )
        end,
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
    ?assertEqual(ok, block_internal_host(<<"https://1.1.1.1/">>, #{})),
    ?assertEqual(
        ok,
        block_internal_host(<<"https://[2606:4700:4700::1111]/">>, #{})
    ),
    ?assertEqual(ok, block_internal_host(<<"/arweave/info">>, #{})),
    ?assertEqual(
        ok,
        maybe_block_internal_host(
            <<"http://localhost/">>,
            #{ <<"relay-block-internal-hosts">> => false }
        )
    ).

relay_host_allowlist_test() ->
    ?assertEqual(
        ok,
        block_internal_host(
            <<"https://example.com/">>,
            #{ <<"relay-allowed-hosts">> => [<<"example.com">>] }
        )
    ),
    ?assertEqual(
        ok,
        block_internal_host(
            <<"https://www.example.com/">>,
            #{ <<"relay-allowed-hosts">> => [<<".example.com">>] }
        )
    ),
    ?assertThrow(
        relay_host_not_allowed,
        block_internal_host(
            <<"https://example.com/">>,
            #{ <<"relay-allowed-hosts">> => [<<"arweave.net">>] }
        )
    ),
    ?assertThrow(
        relay_host_not_allowed,
        block_internal_host(
            <<"https://example.com/">>,
            #{ <<"relay-allowed-hosts">> => [<<"https://example.com">>] }
        )
    ),
    ?assertThrow(
        relay_internal_host_not_allowed,
        block_internal_host(
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
    ?assertEqual(true, byte_size(Body) > 10_000).

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
                <<"port">> => Port,
                <<"force-signed-requests">> => true
            }
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
                                <<"wallet">> => hb_util:human_id(Wallet),
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
                <<"path">> => <<"test-key">>,
                <<"test-key">> => <<"value">>
            },
            #{}
        ),
    ?event({res, Res}),
    ?assertEqual(<<"value">>, Res).
