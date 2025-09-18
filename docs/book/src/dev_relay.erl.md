# dev_relay

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_relay.erl)

This module implements the relay device, which is responsible for
relaying messages between nodes and other HTTP(S) endpoints.
It can be called in either `call` or `cast` mode. In `call` mode, it
returns a `{ok, Result}` tuple, where `Result` is the response from the 
remote peer to the message sent. In `cast` mode, the invocation returns
immediately, and the message is relayed asynchronously. No response is given
and the device returns `{ok, <<"OK">>}`.
Example usage:
<pre>
    curl /~relay@.1.0/call?method=GET?0.path=https://www.arweave.net/
</pre>

---

## Exported Functions

- `call/3`
- `cast/3`
- `request/3`

---

### call

This module implements the relay device, which is responsible for
Execute a `call` request using a node's routes.

```erlang
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
        case Commit of
            true ->
                case hb_opts:get(relay_allow_commit_request, false, Opts) of
                    true ->
                        ?event(debug_relay, {recommitting, TargetMod3}, Opts),
                        Committed = hb_message:commit(TargetMod3, Opts),
                        ?event(debug_relay, {relay_call, {committed, Committed}}, Opts),
                        true = hb_message:verify(Committed, all),
                        Committed;
                    false ->
                        throw(relay_commit_request_not_allowed)
                end;
            false -> TargetMod3
        end,
    ?event(debug_relay, {relay_call, {without_http_params, TargetMod3}}),
    ?event(debug_relay, {relay_call, {with_http_params, TargetMod4}}),
    true = hb_message:verify(TargetMod4),
    ?event(debug_relay, {relay_call, {verified, true}}),
    Client =
        case hb_maps:get(<<"http-client">>, BaseTarget, not_found, Opts) of
            not_found -> hb_opts:get(relay_http_client, Opts);
            RequestedClient -> RequestedClient
        end,
    % Let `hb_http:request/2' handle finding the peer and dispatching the
    % request, unless the peer is explicitly given.
```

### cast

Execute a request in the same way as `call/3`, but asynchronously. Always
Preprocess a request to check if it should be relayed to a different node.

```erlang
cast(M1, M2, Opts) ->
    spawn(fun() -> call(M1, M2, Opts) end),
    {ok, <<"OK">>}.
```

### request

Execute a request in the same way as `call/3`, but asynchronously. Always
Preprocess a request to check if it should be relayed to a different node.

```erlang
request(_Msg1, Msg2, Opts) ->
    {ok,
        #{
            <<"body">> =>
                [
                    #{ <<"device">> => <<"relay@1.0">> },
                    #{
                        <<"path">> => <<"call">>,
                        <<"target">> => <<"body">>,
                        <<"body">> =>
                            hb_ao:get(<<"request">>, Msg2, Opts#{ hashpath => ignore })
                    }
                ]
        }
    }.
```

### call_get_test

```erlang
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
```

### relay_nearest_test

```erlang
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
```

### commit_request_test

Test that a `relay@1.0/call` correctly commits requests as specified.

```erlang
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
```

---

*Generated from [dev_relay.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_relay.erl)*
