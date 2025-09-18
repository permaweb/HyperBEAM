# dev_p4

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_p4.erl)

The HyperBEAM core payment ledger. This module allows the operator to
specify another device that can act as a pricing mechanism for transactions
on the node, as well as orchestrating a payment ledger to calculate whether
the node should fulfil services for users.
The device requires the following node message settings in order to function:
- `p4_pricing-device`: The device that will estimate the cost of a request.
- `p4_ledger-device`: The device that will act as a payment ledger.
The pricing device should implement the following keys:
<pre>
            `GET /estimate?type=pre|post&body=[...]&request=RequestMessage`
            `GET /price?type=pre|post&body=[...]&request=RequestMessage`
</pre>
The `body` key is used to pass either the request or response messages to the
device. The `type` key is used to specify whether the inquiry is for a request
(pre) or a response (post) object. Requests carry lists of messages that will
be executed, while responses carry the results of the execution. The `price`
key may return `infinity` if the node will not serve a user under any
circumstances. Else, the value returned by the `price` key will be passed to
the ledger device as the `amount` key.
A ledger device should implement the following keys:
<pre>
            `POST /credit?message=PaymentMessage&request=RequestMessage`
            `POST /charge?amount=PriceMessage&request=RequestMessage`
            `GET /balance?request=RequestMessage`
</pre>
The `type` key is optional and defaults to `pre`. If `type` is set to `post`,
the charge must be applied to the ledger, whereas the `pre` type is used to
check whether the charge would succeed before execution.

---

## Exported Functions

- `balance/3`
- `request/3`
- `response/3`

---

### request

The HyperBEAM core payment ledger. This module allows the operator to
Estimate the cost of a transaction and decide whether to proceed with

```erlang
request(State, Raw, NodeMsg) ->
    PricingDevice = hb_ao:get(<<"pricing-device">>, State, false, NodeMsg),
    LedgerDevice = hb_ao:get(<<"ledger-device">>, State, false, NodeMsg),
    Messages = hb_ao:get(<<"body">>, Raw, NodeMsg#{ hashpath => ignore }),
    Request = hb_ao:get(<<"request">>, Raw, NodeMsg),
    IsChargable = is_chargable_req(Request, NodeMsg),
    ?event(payment,
        {preprocess_with_devices,
            PricingDevice,
            LedgerDevice,
            {chargable, IsChargable}
        }
    ),
    case {IsChargable, (PricingDevice =/= false) and (LedgerDevice =/= false)} of
        {false, _} ->
            ?event(payment, non_chargable_route),
            {ok, #{ <<"body">> => Messages }};
        {true, false} ->
            ?event(payment, {p4_pre_pricing_response, {error, <<"infinity">>}}),
            {ok, #{ <<"body">> => Messages }};
        {true, true} ->
            PricingMsg = State#{ <<"device">> => PricingDevice },
            LedgerMsg = State#{ <<"device">> => LedgerDevice },
            PricingReq = #{
                <<"path">> => <<"estimate">>,
                <<"request">> => Request,
                <<"body">> => Messages
            },
            ?event({p4_pricing_request, {devmsg, PricingMsg}, {req, PricingReq}}),
            case hb_ao:resolve(PricingMsg, PricingReq, NodeMsg) of
                {ok, <<"infinity">>} ->
                    % The device states that under no circumstances should we
                    % proceed with the request.
```

### response

Postprocess the request after it has been fulfilled.

```erlang
response(State, RawResponse, NodeMsg) ->
    PricingDevice = hb_ao:get(<<"pricing-device">>, State, false, NodeMsg),
    LedgerDevice = hb_ao:get(<<"ledger-device">>, State, false, NodeMsg),
    Response =
        hb_ao:get(
            <<"body">>,
            RawResponse,
            NodeMsg#{ hashpath => ignore }
        ),
    Request = hb_ao:get(<<"request">>, RawResponse, NodeMsg),
    ?event(payment, {post_processing_with_devices, PricingDevice, LedgerDevice}),
    ?event({response_hook, {request, Request}, {response, Response}}),
    case ((PricingDevice =/= false) and (LedgerDevice =/= false)) andalso
            is_chargable_req(Request, NodeMsg) of
        false ->
            {ok, #{ <<"body">> => Response }};
        true ->
            PricingMsg = State#{ <<"device">> => PricingDevice },
            LedgerMsg = State#{ <<"device">> => LedgerDevice },
            PricingReq = #{
                <<"path">> => <<"price">>,
                <<"request">> => Request,
                <<"body">> => Response
            },
            ?event({post_pricing_request, PricingReq}),
            PricingRes =
                case hb_ao:resolve(PricingMsg, PricingReq, NodeMsg) of
                    {error, _Error} ->
                        % The pricing device is unable to give us a cost for
                        % the request, so we try to estimate it instead.
```

### balance

Get the balance of a user in the ledger.

```erlang
balance(_, Req, NodeMsg) ->
    case dev_hook:find(<<"request">>, NodeMsg) of
        [] ->
            {error, <<"No request hook found.">>};
        [Handler] ->
            LedgerDevice =
                hb_ao:get(<<"ledger-device">>, Handler, false, NodeMsg),
            LedgerMsg = Handler#{ <<"device">> => LedgerDevice },
            LedgerReq = #{
                <<"path">> => <<"balance">>,
                <<"request">> => Req
            },
            ?event({ledger_message, {ledger_msg, LedgerMsg}}),
            case hb_ao:resolve(LedgerMsg, LedgerReq, NodeMsg) of
                {ok, Balance} ->
                    {ok, Balance};
                {error, Error} ->
                    {error, Error}
            end
    end.
```

### is_chargable_req

The node operator may elect to make certain routes non-chargable, using 

```erlang
is_chargable_req(Req, NodeMsg) ->
    NonChargableRoutes =
        hb_opts:get(
            p4_non_chargable_routes,
            ?DEFAULT_NON_CHARGABLE_ROUTES,
            NodeMsg
        ),
    Matches =
        dev_router:match(
            #{ <<"routes">> => NonChargableRoutes },
            Req,
            NodeMsg
        ),
    ?event(
        {
            is_chargable,
            {non_chargable_routes, NonChargableRoutes},
            {req, Req},
            {matches, Matches}
        }
    ),
    case Matches of
        {error, no_matching_route} -> true;
        _ -> false
    end.
```

### test_opts

```erlang
test_opts(Opts) ->
    test_opts(Opts, <<"faff@1.0">>).
```

### test_opts

```erlang
test_opts(Opts, PricingDev) ->
    test_opts(Opts, PricingDev, <<"faff@1.0">>).
```

### test_opts

```erlang
test_opts(Opts, PricingDev, LedgerDev) ->
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"pricing-device">> => PricingDev,
            <<"ledger-device">> => LedgerDev
        },
    Opts#{
        on => #{
            <<"request">> => ProcessorMsg,
            <<"response">> => ProcessorMsg
        }
    }.
```

### faff_test

Simple test of p4's capabilities with the `faff@1.0` device.

```erlang
faff_test() ->
    GoodWallet = ar_wallet:new(),
    BadWallet = ar_wallet:new(),
    Node = hb_http_server:start_node(
       test_opts(
            #{
                faff_allow_list =>
                    [hb_util:human_id(ar_wallet:to_address(GoodWallet))]
            }
        )
    ),
    Req = #{
        <<"path">> => <<"/greeting">>,
        <<"greeting">> => <<"Hello, world!">>
    },
    GoodSignedReq = hb_message:commit(Req, GoodWallet),
    ?event({req, GoodSignedReq}),
    BadSignedReq = hb_message:commit(Req, BadWallet),
    ?event({req, BadSignedReq}),
    {ok, Res} = hb_http:get(Node, GoodSignedReq, #{}),
    ?event(payment, {res, Res}),
    ?assertEqual(<<"Hello, world!">>, Res),
    ?assertMatch({error, _}, hb_http:get(Node, BadSignedReq, #{})).
```

### non_chargable_route_test

Test that a non-chargable route is not charged for.

```erlang
non_chargable_route_test() ->
    Wallet = ar_wallet:new(),
    Processor =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>
        },
    Node = hb_http_server:start_node(
        #{
            p4_non_chargable_routes =>
                [
                    #{ <<"template">> => <<"/~p4@1.0/balance">> },
                    #{ <<"template">> => <<"/~meta@1.0/*/*">> }
                ],
            on => #{
                <<"request">> => Processor,
                <<"response">> => Processor
            },
            operator => hb:address()
        }
    ),
    Req = #{
        <<"path">> => <<"/~p4@1.0/balance">>
    },
    GoodSignedReq = hb_message:commit(Req, Wallet),
    Res = hb_http:get(Node, GoodSignedReq, #{}),
    ?event({res1, Res}),
    ?assertMatch({ok, 0}, Res),
    Req2 = #{ <<"path">> => <<"/~meta@1.0/info/operator">> },
    GoodSignedReq2 = hb_message:commit(Req2, Wallet),
    Res2 = hb_http:get(Node, GoodSignedReq2, #{}),
    ?event({res2, Res2}),
    OperatorAddress = hb_util:human_id(hb:address()),
    ?assertEqual({ok, OperatorAddress}, Res2),
    Req3 = #{ <<"path">> => <<"/~scheduler@1.0">> },
    BadSignedReq3 = hb_message:commit(Req3, Wallet),
    Res3 = hb_http:get(Node, BadSignedReq3, #{}),
    ?event({res3, Res3}),
    ?assertMatch({error, _}, Res3).
```

### hyper_token_ledger_test_

Ensure that Lua scripts can be used as pricing and ledger devices. Our

```erlang
hyper_token_ledger_test_() ->
    {timeout, 60, fun hyper_token_ledger/0}.
```

### hyper_token_ledger

```erlang
hyper_token_ledger() ->
    % Create the wallets necessary and read the files containing the scripts.
```

---

*Generated from [dev_p4.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_p4.erl)*
