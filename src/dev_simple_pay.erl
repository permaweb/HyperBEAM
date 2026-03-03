%%% @doc A simple device that allows the operator to specify a price for a
%%% request and then charge the user for it, on a per route and optionally
%%% per message basis.
%%% 
%%% The device's pricing rules are as follows:
%%%
%%% 1. If the request is from the operator, the cost is 0.
%%% 2. If the request matches one of the `router_opts/offered' routes, the
%%%    explicit price of the route is used.
%%% 3. Else, the price is calculated by counting the number of messages in the
%%%    request, and multiplying by the `simple_pay_price' node option, plus the
%%%    price of the apply subrequest if applicable. Subrequests are priced by
%%%    recursively calling `estimate/3' upon them. In the case of an `apply@1.0'
%%%    subrequest, the two initiating apply messages are not counted towards the
%%%    message count price.
%%% 
%%% The device's ledger is stored in the node message at `simple_pay_ledger',
%%% and can be topped-up by either the operator, or an external device. The 
%%% price is specified in the node message at `simple_pay_price'.
%%% This device acts as both a pricing device and a ledger device, by p4's
%%% definition.
-module(dev_simple_pay).
-export([estimate/3, charge/3, balance/3, topup/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Estimate the cost of the request, using the rules outlined in the
%% moduledoc.
estimate(_Base, EstimateReq, NodeMsg) ->
    Req = hb_ao:get(<<"request">>, EstimateReq, NodeMsg#{ hashpath => ignore }),
    case is_operator(Req, NodeMsg) of
        true ->
            ?event(payment,
                {estimate_preprocessing, caller_is_operator}
            ),
            {ok, 0};
        false ->
            ?event(payment, {starting_estimate, {req, Req}}),
            ReqSequence = hb_singleton:from(Req, NodeMsg),
            ?event(payment,
                {estimating_cost,
                    {singleton, Req},
                    {request_sequence, ReqSequence}
                }
            ),
            % Get the user's request to match against router registration options
            case price_from_routes(Req, NodeMsg) of
                no_matches ->
                    {ok, ApplyPrice, SeqWithoutApply} = apply_price(ReqSequence, NodeMsg),
                    MessageCountPrice = price_from_count(SeqWithoutApply, NodeMsg),
                    Price = MessageCountPrice + ApplyPrice,
                    ?event(payment,
                        {calculated_generic_route_price,
                            {price, Price},
                            {message_count_price, MessageCountPrice},
                            {apply_price, ApplyPrice}
                        }),
                    {ok, Price};
                Price ->
                    ?event(payment,
                        {calculated_specific_route_price,
                            {price, Price}
                        }
                    ),
                    {ok, Price}
            end
    end.

%% @doc If the request is for the `apply@1.0' device, we should price the
%% inner request in addition to the price of the outer request.
apply_price([{as, Device, Msg} | Rest], NodeMsg) ->
    apply_price([Msg#{ <<"device">> => Device } | Rest], NodeMsg);
apply_price(
        [Req = #{ <<"device">> := <<"apply@1.0">> }, #{ <<"path">> := Path } | Rest],
        NodeMsg
    ) ->
    UserPath = hb_maps:get(Path, Req, <<"">>, NodeMsg),
    UserMessage =
        case hb_maps:find(<<"source">>, Req, NodeMsg) of
            {ok, Source} -> hb_maps:get(Source, Req, Req, NodeMsg);
            error -> Req
        end,
    UserRequest =
        hb_maps:without(
            [<<"device">>],
            UserMessage#{ <<"path">> => UserPath }
        ),
    ?event(payment, {estimating_price_of_subrequest, {req, UserRequest}}),
    {ok, Price} = estimate(#{}, #{ <<"request">> => UserRequest }, NodeMsg),
    ?event(payment, {price_of_apply_subrequest, {price, Price}}),
    {ok, Price, Rest};
apply_price(Seq, _) ->
    {ok, 0, Seq}.

%% @doc Calculate the price of a request based on the offered routes, if
%% applicable.
price_from_routes(UserRequest, NodeMsg) ->
    RouterOpts = hb_opts:get(<<"router_opts">>, #{}, NodeMsg),
    Routes = hb_maps:get(<<"offered">>, RouterOpts, [], NodeMsg),
    MatchRes =
        dev_router:match(
            #{ <<"routes">> => Routes },
            UserRequest,
            NodeMsg
        ),
    case MatchRes of
        {ok, OfferedRoute} ->
            Price = hb_maps:get(<<"price">>, OfferedRoute, 0, NodeMsg),
            ?event(payment, {price_from_routes, {price, Price}}),
            Price;
        _ ->
            no_matches
    end.

%% @doc Calculate the price of a request based on the number of messages in
%% the request, if the node is configured to do so.
price_from_count(Messages, NodeMsg) ->
    Price =
        hb_util:int(hb_opts:get(simple_pay_price, 1, NodeMsg))
            * length(Messages),
    ?event(payment, {price_from_count, {price, Price}, {count, length(Messages)}}),
    Price.

%% @doc Preprocess a request by checking the ledger and charging the user. We 
%% can charge the user at this stage because we know statically what the price
%% will be
charge(_, RawReq, NodeMsg) ->
    ?event(payment, {charge, RawReq}),
    Req = hb_ao:get(<<"request">>, RawReq, NodeMsg#{ hashpath => ignore }),
    case hb_message:signers(Req, NodeMsg) of
        [] ->
            ?event(payment, {charge, {error, <<"No signers">>}}),
            {ok, false};
        [Signer] ->
            UserBalance = get_balance(Signer, NodeMsg),
            Price = hb_ao:get(<<"quantity">>, RawReq, 0, NodeMsg),
            ?event(payment,
                {charge,
                    {user, Signer},
                    {balance, UserBalance},
                    {price, Price}
                }),
            {ok, _} =
                set_balance(
                    Signer,
                    NewBalance = UserBalance - Price,
                    NodeMsg
                ),
            case NewBalance >= 0 of
                true ->
                    {ok, true};
                false ->
                    ?event(payment,
                        {charge,
                            {user, Signer},
                            {balance, UserBalance},
                            {price, Price}
                        }
                    ),
                    {error, #{
                        <<"status">> => 402,
                        <<"body">> => <<"Insufficient funds. "
                            "User balance before charge: ",
                                (hb_util:bin(UserBalance))/binary,
                            ". Price of request: ",
                                (hb_util:bin(Price))/binary,
                            ". New balance: ",
                                (hb_util:bin(NewBalance))/binary,
                            ".">>
                    }}
            end;
        MultipleSigners ->
            ?event(payment, {charge, {error_multiple_signers, MultipleSigners}}),
            {error, #{
                <<"status">> => 400,
                <<"body">> => <<"Multiple signers in charge.">>
            }}
    end.

%% @doc Get the balance of a user in the ledger.
balance(_, RawReq, NodeMsg) ->
    Target =
        case hb_ao:get(<<"request">>, RawReq, NodeMsg#{ hashpath => ignore }) of
            not_found ->
                case hb_message:signers(RawReq, NodeMsg) of
                    [] -> hb_ao:get(<<"target">>, RawReq, undefined, NodeMsg);
                    [Signer] -> Signer
                end;
            Req -> hd(hb_message:signers(Req, NodeMsg))
        end,
    {ok, get_balance(Target, NodeMsg)}.

%% @doc Adjust a user's balance, normalizing their wallet ID first.
set_balance(Signer, Amount, NodeMsg) ->
    NormSigner = hb_util:human_id(Signer),
    Ledger = hb_opts:get(simple_pay_ledger, #{}, NodeMsg),
    ?event(payment,
        {modifying_balance,
            {user, NormSigner},
            {amount, Amount},
            {ledger_before, Ledger}
        }
    ),
    hb_http_server:set_opts(
        #{},
        NewMsg = NodeMsg#{
            simple_pay_ledger =>
                hb_ao:set(
                    Ledger,
                    NormSigner,
                    Amount,
                    NodeMsg
                )
        }
    ),
    {ok, NewMsg}.

%% @doc Get the balance of a user in the ledger.
get_balance(Signer, NodeMsg) ->
    NormSigner = hb_util:human_id(Signer),
    Ledger = hb_opts:get(simple_pay_ledger, #{}, NodeMsg),
    hb_ao:get(NormSigner, Ledger, 0, NodeMsg).

%% @doc Top up the user's balance in the ledger.
topup(_, Req, NodeMsg) ->
    ?event({topup, {req, Req}, {node_msg, NodeMsg}}),
    case is_operator(Req, NodeMsg) of
        false -> {error, <<"Unauthorized">>};
        true ->
            Amount = hb_ao:get(<<"amount">>, Req, 0, NodeMsg),
            Recipient = hb_ao:get(<<"recipient">>, Req, undefined, NodeMsg),
            CurrentBalance = get_balance(Recipient, NodeMsg),
            ?event(payment,
                {topup,
                    {amount, Amount},
                    {recipient, Recipient},
                    {balance, CurrentBalance},
                    {expected_new_balance, CurrentBalance + Amount}
                }),
            {ok, NewNodeMsg} =
                set_balance(
                    Recipient,
                    CurrentBalance + Amount,
                    NodeMsg
                ),
            % Briefly wait for the ledger to be updated.
            receive after 100 -> ok end,
            {ok, get_balance(Recipient, NewNodeMsg)}
    end.

%% @doc Check if the request is from the operator.
is_operator(Req, NodeMsg) ->
    is_operator(Req, NodeMsg, hb_opts:get(operator, undefined, NodeMsg)).

is_operator(Req, NodeMsg, OperatorAddr) when ?IS_ID(OperatorAddr) ->
    Signers = hb_message:signers(Req, NodeMsg),
    HumanOperatorAddr = hb_util:human_id(OperatorAddr),
    lists:any(
        fun(Signer) ->
            HumanOperatorAddr =:= hb_util:human_id(Signer)
        end,
        Signers
    );
is_operator(_, _, _) ->
    false.

%%% Tests

test_opts(Ledger) ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>
        },
    {
        Address,
        Wallet,
        #{
            simple_pay_ledger => Ledger,
            simple_pay_price => 10,
            operator => Address,
            on => #{
                <<"request">> => ProcessorMsg,
                <<"response">> => ProcessorMsg
            }
        }
    }.

get_balance_and_top_up_test() ->
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    {HostAddress, HostWallet, Opts} = test_opts(#{ ClientAddress => 100 }),
    Node = hb_http_server:start_node(Opts),
    ?event({host_address, HostAddress}),
    ?event({client_address, ClientAddress}),
    {ok, Res} =
        hb_http:get(
            Node,
            Req = hb_message:commit(
                #{<<"path">> => <<"/~simple-pay@1.0/balance">>},
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),
    ?event({req_signers, hb_message:signers(Req, Opts)}),
    % Balance is given during the request, before the charge is made, so we 
    % should expect to see the original balance.
    ?assertEqual(100, Res),
    % The balance should now be 80, as the check will have charged us 20.
    {ok, NewBalance} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~simple-pay@1.0/topup">>,
                    <<"amount">> => 100,
                    <<"recipient">> => ClientAddress
                },
                Opts#{ priv_wallet => HostWallet }
            ),
            Opts
        ),
    % The balance should now be 180, as the topup will have been added and will
    % not have generated a charge in itself. The top-up did not generate a charge
    % because it is the operator that performed it, and not a user.
    ?assertEqual(180, NewBalance),
    {ok, Res2} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{<<"path">> => <<"/~p4@1.0/balance">>},
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),
    ?assertEqual(180, Res2).

apply_price_test() ->
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    ClientOpts = #{ priv_wallet => ClientWallet },
    {HostAddress, _HostWallet, Opts} =
        test_opts(#{ ClientAddress => 100 }),
    Node = hb_http_server:start_node(Opts),
    ?event({host_address, HostAddress}),
    ?event({client_address, ClientAddress}),
    % The balance should now be 80, as the check will have charged us 20.
    {ok, _} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~apply@1.0/user-path">>,
                    <<"user-path">> => <<"/~scheduler@1.0/status/keys/1">>,
                    <<"user-message">> => #{ <<"a">> => 1 }
                },
                ClientOpts
            ),
            ClientOpts
        ),
    {ok, Res2} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{ <<"path">> => <<"/~p4@1.0/balance">> },
                Opts#{ priv_wallet => ClientWallet }
            ),
            Opts
        ),
    ?assertEqual(60, Res2).