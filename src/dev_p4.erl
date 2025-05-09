%%% @doc The HyperBEAM core payment ledger. This module allows the operator to
%%% specify another device that can act as a pricing mechanism for transactions
%%% on the node, as well as orchestrating a payment ledger to calculate whether
%%% the node should fulfil services for users.
%%%
%%% The device requires the following node message settings in order to function:
%%%
%%% - `p4_pricing-device': The device that will estimate the cost of a request.
%%% - `p4_ledger-device': The device that will act as a payment ledger.
%%%
%%% The pricing device should implement the following keys:
%%% <pre>
%%%             `GET /estimate?type=pre|post&body=[...]&request=RequestMessage'
%%%             `GET /price?type=pre|post&body=[...]&request=RequestMessage'
%%% </pre>
%%% 
%%% The `body' key is used to pass either the request or response messages to the
%%% device. The `type' key is used to specify whether the inquiry is for a request
%%% (pre) or a response (post) object. Requests carry lists of messages that will
%%% be executed, while responses carry the results of the execution. The `price'
%%% key may return `infinity' if the node will not serve a user under any
%%% circumstances. Else, the value returned by the `price' key will be passed to
%%% the ledger device as the `amount' key.
%%%
%%% A ledger device should implement the following keys:
%%% <pre>
%%%             `POST /credit?message=PaymentMessage&request=RequestMessage'
%%%             `POST /debit?amount=PriceMessage&request=RequestMessage'
%%%             `GET /balance?request=RequestMessage'
%%% </pre>
%%%
%%% The `type' key is optional and defaults to `pre'. If `type' is set to `post',
%%% the debit must be applied to the ledger, whereas the `pre' type is used to
%%% check whether the debit would succeed before execution.
-module(dev_p4).
-export([request/3, response/3, balance/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The default list of routes that should not be charged for.
-define(DEFAULT_NON_CHARGABLE_ROUTES, [
    #{ <<"template">> => <<"/~p4@1.0/balance">> },
    #{ <<"template">> => <<"/~meta@1.0/*">> }
]).

%% @doc Estimate the cost of a transaction and decide whether to proceed with
%% a request. The default behavior if `pricing-device' or `p4_balances' are
%% not set is to proceed, so it is important that a user initialize them.
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
                {error, Error} ->
                    % The device is unable to estimate the cost of the request,
                    % so we don't proceed.
                    {error, {error_calculating_price, Error}};
                {ok, <<"infinity">>} ->
                    % The device states that under no circumstances should we
                    % proceed with the request.
                    ?event(payment, {p4_pre_pricing_response, {error, <<"infinity">>}}),
                    {error,
                        <<"Node will not service this request "
                            "under any circumstances.">>};
                {ok, 0} ->
                    % The device has estimated the cost of the request to be
                    % zero, so we proceed.
                    {ok, #{ <<"body">> => Messages }};
                {ok, Price} ->
                    % The device has estimated the cost of the request. We check
                    % the user's balance to see if they have enough funds to
                    % service the request.
                    LedgerReq = #{
                        <<"path">> => <<"balance">>,
                        <<"target">> =>
                            case hb_message:signers(Request) of
                                [Signer] -> Signer;
                                [] -> <<"unknown">>;
                                Multiple -> Multiple
                            end,
                        <<"request">> => Request
                    },
                    ?event(payment, {p4_pre_pricing_estimate, Price}),
                    case hb_ao:resolve(LedgerMsg, LedgerReq, NodeMsg) of
                        {ok, Sufficient} when
                                Sufficient =:= true orelse
                                Sufficient =:= <<"infinity">> ->
                            % The ledger device has confirmed that the user has
                            % enough funds for the request, so we proceed.
                            ?event(payment,
                                {p4_pre_ledger_response,
                                    {balance_check, guaranteed}
                                }
                            ),
                            {ok, #{ <<"body">> => Messages }};
                        {ok, Balance} when Balance >= Price ->
                            % The user has enough funds to service the request,
                            % so we proceed.
                            ?event(payment,
                                {p4_pre_ledger_response,
                                    {balance_check, sufficient}
                                }
                            ),
                            {ok, #{ <<"body">> => Messages }};
                        {ok, Balance} ->
                            % The user does not have enough funds to service
                            % the request, so we don't proceed.
                            ?event(payment,
                                {insufficient_funds,
                                    {balance, Balance},
                                    {price, Price}
                                }
                            ),
                            {error, #{
                                <<"status">> => 429,
                                <<"body">> => <<"Insufficient funds">>,
                                <<"price">> => Price,
                                <<"balance">> => Balance
                            }};
                        {error, Error} ->
                            % The ledger device is unable to process the request,
                            % so we don't proceed.
                            ?event(payment,
                                {pre_ledger_validation,
                                    {error, Error},
                                    {base, LedgerMsg},
                                    {req, LedgerReq}
                                }
                            ),
                            {error, #{
                                <<"status">> => 500,
                                <<"body">> => <<"Error checking ledger balance.">>
                            }}
                    end
            end
    end.

%% @doc Postprocess the request after it has been fulfilled.
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
                        EstimateReq = PricingReq#{ <<"path">> => <<"estimate">> },
                        hb_ao:resolve(PricingMsg, EstimateReq, NodeMsg);
                    {ok, P} -> {ok, P}
                end,
            ?event(payment, {p4_post_pricing_response, PricingRes}),
            case PricingRes of
                {ok, Price} ->
                    % We have successfully determined the cost of the request,
                    % so we proceed to debit the user's account. We sign the
                    % request with the node's private key, as it is the node
                    % that is performing the debit, not the user.
                    LedgerReq =
                        hb_message:commit(
                            #{
                                <<"path">> => <<"debit">>,
                                <<"quantity">> => Price,
                                <<"account">> =>
                                    case hb_message:signers(Request) of
                                        [Signer] -> Signer;
                                        [] -> <<"unknown">>;
                                        Multiple -> Multiple
                                    end,
                                <<"request">> => Request
                            },
                            hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg)
                        ),
                    ?event({p4_ledger_request, LedgerReq}),
                    case hb_ao:resolve(LedgerMsg, LedgerReq, NodeMsg) of
                        {ok, _} ->
                            ?event(payment, {p4_post_ledger_response, {ok, Price}}),
                            % Return the original response.
                            {ok, #{ <<"body">> => Response }};
                        {error, Error} ->
                            ?event(payment, {p4_post_ledger_response, {error, Error}}),
                            % The debit failed, so we return the error from the
                            % ledger device.
                            {error, Error}
                    end;
                {error, PricingError} ->
                    % The pricing device is unable to process the request,
                    % so we don't proceed.
                    {error, PricingError}
            end
    end.

%% @doc Get the balance of a user in the ledger.
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
            ?event(debug, {ledger_message, {ledger_msg, LedgerMsg}}),
            case hb_ao:resolve(LedgerMsg, LedgerReq, NodeMsg) of
                {ok, Balance} ->
                    {ok, Balance};
                {error, Error} ->
                    {error, Error}
            end
    end.

%% @doc The node operator may elect to make certain routes non-chargable, using 
%% the `routes' syntax also used to declare routes in `router@1.0'.
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

%%% Tests

test_opts(Opts) ->
    test_opts(Opts, <<"faff@1.0">>).
test_opts(Opts, PricingDev) ->
    test_opts(Opts, PricingDev, <<"faff@1.0">>).
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

%% @doc Simple test of p4's capabilities with the `faff@1.0' device.
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

%% @doc Test that a non-chargable route is not charged for.
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

%% @doc Ensure that Lua scripts can be used as pricing and ledger devices. Our
%% scripts come in two parts:
%% - A `process' script which is executed as a persistent `local-process' on the
%%   node, and which maintains the state of the ledger.
%% - A `client' script, which is executed as a `p4@1.0' device, marshalling
%%   requests to the `process' script.
lua_pricing_test() ->
    HostWallet = ar_wallet:new(),
    ClientWallet = ar_wallet:new(),
    {ok, ProcessScript} = file:read_file("scripts/p4-payment-process.lua"),
    {ok, ClientScript} = file:read_file("scripts/p4-payment-client.lua"),
    Processor =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"lua@5.3a">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>,
            <<"script">> => #{
                <<"content-type">> => <<"text/x-lua">>,
                <<"module">> => <<"scripts/p4-payment-client.lua">>,
                <<"body">> => ClientScript
            },
            <<"ledger-path">> => <<"/ledger~node-process@1.0">>
        },
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => HostWallet,
                p4_non_chargable_routes =>
                    [
                        #{
                            <<"template">> =>
                                <<"/*~node-process@1.0/*">>
                        }
                    ],
                on => #{
                    <<"request">> => Processor,
                    <<"response">> => Processor
                },
                operator => ar_wallet:to_address(HostWallet),
                node_processes => #{
                    <<"ledger">> => #{
                        <<"device">> => <<"process@1.0">>,
                        <<"execution-device">> => <<"lua@5.3a">>,
                        <<"scheduler-device">> => <<"scheduler@1.0">>,
                        <<"script">> => #{
                            <<"content-type">> => <<"text/x-lua">>,
                            <<"module">> => <<"scripts/p4-payment-process.lua">>,
                            <<"body">> => ProcessScript
                        },
                        <<"operator">> =>
                            hb_util:human_id(ar_wallet:to_address(HostWallet))
                    }
                }
            }
        ),
    Req = #{
        <<"path">> => <<"/greeting">>,
        <<"greeting">> => <<"Hello, world!">>
    },
    SignedReq = hb_message:commit(Req, ClientWallet),
    Res = hb_http:get(Node, SignedReq, #{}),
    ?event({expected_failure, Res}),
    ?assertMatch({error, _}, Res),
    {ok, TopupRes} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/ledger~node-process@1.0/schedule">>,
                    <<"body">> =>
                        hb_message:commit(
                            #{
                                <<"path">> => <<"credit-notice">>,
                                <<"quantity">> => 100,
                                <<"recipient">> =>
                                    hb_util:human_id(
                                        ar_wallet:to_address(ClientWallet)
                                    )
                            },
                            HostWallet
                        )
                },
                HostWallet
            ),
            #{}
        ),
    ?event({topup_res, TopupRes}),
    ResAfterTopup = hb_http:get(Node, SignedReq, #{}),
    ?event({res_after_topup, ResAfterTopup}),
    ?assertMatch({ok, <<"Hello, world!">>}, ResAfterTopup),
    {ok, Balance} =
        hb_http:get(
            Node,
            <<
                "/ledger~node-process@1.0/now/balance/",
                    (hb_util:human_id(ar_wallet:to_address(ClientWallet)))/binary
            >>,
            #{}
        ),
    ?event({balance, Balance}),
    ?assertMatch(#{ <<"body">> := <<"98">> }, Balance).