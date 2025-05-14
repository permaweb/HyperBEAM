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
%%%             `POST /charge?amount=PriceMessage&request=RequestMessage'
%%%             `GET /balance?request=RequestMessage'
%%% </pre>
%%%
%%% The `type' key is optional and defaults to `pre'. If `type' is set to `post',
%%% the charge must be applied to the ledger, whereas the `pre' type is used to
%%% check whether the charge would succeed before execution.
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
                {ok, 0} ->
                    % The pricing device has estimated the cost of the request
                    % to be zero, so we proceed.
                    {ok, #{ <<"body">> => Response }};
                {ok, Price} ->
                    % We have successfully determined the cost of the request,
                    % so we proceed to charge the user's account. We sign the
                    % request with the node's private key, as it is the node
                    % that is performing the charge, not the user.
                    LedgerReq =
                        hb_message:commit(
                            #{
                                <<"path">> => <<"charge">>,
                                <<"quantity">> => Price,
                                <<"account">> =>
                                    case hb_message:signers(Request) of
                                        [Signer] -> Signer;
                                        Multiple -> Multiple
                                    end,
                                <<"recipient">> =>
                                    hb_util:human_id(
                                        hb_opts:get(operator, <<0:256>>, NodeMsg)
                                    ),
                                <<"request">> => Request
                            },
                            hb_opts:get(priv_wallet, no_viable_wallet, NodeMsg)
                        ),
                    ?event(payment,
                        {post_charge,
                            {msg, LedgerMsg},
                            {req, LedgerReq}
                        }
                    ),
                    case hb_ao:resolve(LedgerMsg, LedgerReq, NodeMsg) of
                        {ok, _} ->
                            ?event(payment, {p4_post_ledger_response, {ok, Price}}),
                            % Return the original response.
                            {ok, #{ <<"body">> => Response }};
                        {error, Error} ->
                            ?event(
                                payment,
                                {p4_post_ledger_response,
                                    {error,
                                        {string, maps:get(<<"body">>, Error, Error)}
                                    }
                                }
                            ),
                            % The charge failed, so we return the error from the
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
            ?event({ledger_message, {ledger_msg, LedgerMsg}}),
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

%% @doc Ensure that Lua scripts can be used as pricing and ledger devices. This
%% configuration comes in three components:
%% 1. A `hyper-token` root process, acting as the main token ledger.
%% 2. A `process' script which is executed as a persistent `local-process' on the
%%   node, and which maintains the state of our node's sub-ledger. This process
%%   runs `hyper-token.lua' as its base, then adds the logic of `hyper-token-p4'
%%   to it. This secondary script implements the `charge' function that `p4@1.0'
%%   will call to charge a user's account on our sub-ledger. The node's key is
%%   added as an `admin' to the sub-ledger, allowing the `~p4@1.0/charge' call
%%   to identify itself as the node itself. In a TEE-secured environment, this
%%   key will not be exposed to the user. Subsequently, the `admin' functionality
%%   will be as secure as the TEE itself.
%% 3. A `client' script, which is executed as a `p4@1.0' ledger device, which
%%   uses `~push@1.0' to send requests to the local ledger `process'.
hyper_token_ledger_test_() ->
    {timeout, 60, fun hyper_token_ledger/0}.
hyper_token_ledger() ->
    % Create the wallets necessary and read the files containing the scripts.
    HostWallet = ar_wallet:new(),
    HostAddress = hb_util:human_id(HostWallet),
    OperatorWallet = ar_wallet:new(),
    OperatorAddress = hb_util:human_id(OperatorWallet),
    AliceWallet = ar_wallet:new(),
    AliceAddress = hb_util:human_id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddress = hb_util:human_id(BobWallet),
    {ok, TokenScript} = file:read_file("scripts/hyper-token.lua"),
    {ok, ProcessScript} = file:read_file("scripts/hyper-token-p4.lua"),
    {ok, ClientScript} = file:read_file("scripts/hyper-token-p4-client.lua"),
    Store = [
        #{
            <<"prefix">> => <<"cache-mainnet">>,
            <<"store-module">> => hb_store_fs
        }
    ],
	% Create the main (component 1) ledger, which will act as the token for
    % the sub-ledger. We initialize it with 100 tokens for Alice.
    HyperToken =
        hb_message:commit(
            #{
                <<"device">> => <<"process@1.0">>,
                <<"type">> => <<"Process">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"scheduler">> => hb_util:human_id(HostWallet),
                <<"execution-device">> => <<"lua@5.3a">>,
                <<"authority">> => hb_util:human_id(HostWallet),
                <<"script">> => #{
                    <<"content-type">> => <<"text/x-lua">>,
                    <<"module">> => <<"scripts/hyper-token.lua">>,
                    <<"body">> => TokenScript
                },
                <<"balance">> => #{
                    AliceAddress => 100
                }
            },
            HostWallet
        ),
    TokenID = hb_message:id(HyperToken, all),
    % We store it such that it will be available to the node later. In practice,
    % this step is not required as the node will be using an external process
    % for its token.
    {ok, TokenPath} = hb_cache:write(HyperToken, #{ store => Store }),
    % Create the processor device, which contains component (3): The script that 
    % pushes requests to the ledger `process'.
    Processor =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"lua@5.3a">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>,
            <<"script">> => #{
                <<"content-type">> => <<"text/x-lua">>,
                <<"module">> => <<"scripts/hyper-token-p4-client.lua">>,
                <<"body">> => ClientScript
            },
            <<"ledger-path">> => <<"/ledger~node-process@1.0">>
        },
    % Start the node with the processor and the `local-process' ledger 
    % (component 2), running the `hyper-token.lua' and `hyper-token-p4.lua'
    % scripts. `hyper-token.lua' implements the core token ledger, while
    % `hyper-token-p4.lua' implements the `charge' function that `p4@1.0' will
    % call to charge a user's account upon charges.
    % We set the `token' key to the ID of the `hyper-token' process, which
    % informs the local process that it will be running in `sub-ledger' mode and
    % accepting deposits from the main token ledger.
    Node =
        hb_http_server:start_node(
            #{
                store => Store,
                priv_wallet => HostWallet,
                p4_non_chargable_routes =>
                    [
                        #{
                            <<"template">> => <<"/*~node-process@1.0/*">>
                        },
                        #{
                            <<"template">> =>
                                <<"/~node-process@1.0/ledger&return=id">>
                        }
                    ],
                on => #{
                    <<"request">> => Processor,
                    <<"response">> => Processor
                },
                operator => OperatorAddress,
                node_processes => #{
                    <<"ledger">> => #{
                        <<"device">> => <<"process@1.0">>,
                        <<"execution-device">> => <<"lua@5.3a">>,
                        <<"scheduler-device">> => <<"scheduler@1.0">>,
                        <<"script">> => [
                            #{
                                <<"content-type">> => <<"text/x-lua">>,
                                <<"module">> => <<"scripts/hyper-token.lua">>,
                                <<"body">> => TokenScript
                            },
                            #{
                                <<"content-type">> => <<"text/x-lua">>,
                                <<"module">> => <<"scripts/hyper-token-p4.lua">>,
                                <<"body">> => ProcessScript
                            }
                        ],
                        <<"balance">> => #{ AliceAddress => 100 },
                        <<"admin">> => HostAddress,
                        <<"token">> => TokenID
                    }
                }
            }
        ),
    % To start, we attempt a request from Bob, which should fail because he
    % has no tokens.
    Req = #{
        <<"path">> => <<"/greeting">>,
        <<"greeting">> => <<"Hello, world!">>
    },
    SignedReq = hb_message:commit(Req, BobWallet),
    Res = hb_http:get(Node, SignedReq, #{}),
    ?event({expected_failure, Res}),
    ?assertMatch({error, _}, Res),
    % We then move 50 tokens from Alice to Bob from the main token ledger to the
    % sub-ledger.
    % First, we get the ID of the sub-ledger from the local process.
    {ok, SubLedgerID} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~node-process@1.0/ledger&return=id">>
                },
                HostWallet
            ),
            #{}
        ),
    ?event(debug_charge, {subledger_id, SubLedgerID}),
    % We then move 50 tokens from Alice to Bob from the main token ledger to the
    % sub-ledger. The `route' key specifies that the request should be sent to
    % the sub-ledger. Internally, upon receiving the transfer, the main ledger
    % will debit Alice's balance, but instead of crediting Bob directly, it will
    % credit the sub-ledger's account. The sub-ledger will then receive this
    % `Credit-Notice', validate that it is from the root ledger, and credit Bob's
    % account.
    {ok, TopupRes} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => << TokenPath/binary, "/push">>,
                    <<"body">> =>
                        hb_message:commit(
                            #{
                                <<"path">> => <<"transfer">>,
                                <<"quantity">> => 50,
                                <<"recipient">> => BobAddress,
                                <<"route">> => SubLedgerID
                            },
                            AliceWallet
                        )
                },
                OperatorWallet
            ),
            #{}
        ),
    % We now attempt Bob's request again, which should succeed.
    ?event({topup_res, TopupRes}),
    ResAfterTopup = hb_http:get(Node, SignedReq, #{}),
    ?event({res_after_topup, ResAfterTopup}),
    ?assertMatch({ok, <<"Hello, world!">>}, ResAfterTopup),
    % Check the balance of Bob. It should have been charged 2 tokens from
    % the 50 Alice sent him.
    {ok, Balances} =
        hb_http:get(
            Node,
            <<"/ledger~node-process@1.0/now/balance">>,
            #{}
        ),
    ?event(debug_charge, {balances, Balances}),
    ?assertMatch(48, hb_ao:get(BobAddress, Balances, #{})),
    % Check the balance of the operator. It should be 2 tokens, the amount that
    % was charged from Alice.
    ?assertMatch(2, hb_ao:get(OperatorAddress, Balances, #{})).