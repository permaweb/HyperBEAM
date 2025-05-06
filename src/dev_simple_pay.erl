%%% @doc A simple device that allows the operator to specify a price for a
%%% request and then charge the user for it, on a per message basis.
%%% The device's ledger is stored in the node message at `simple_pay_ledger',
%%% and can be topped-up by either the operator, or an external device. The 
%%% price is specified in the node message at `simple_pay_price'.
%%% This device acts as both a pricing device and a ledger device, by p4's
%%% definition.
-module(dev_simple_pay).
-export([estimate/3, debit/3, balance/3, topup/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Estimate the cost of a request by counting the number of messages in
%% the request, then multiplying by the per-message price. The operator does
%% not pay for their own requests.
estimate(_, EstimateReq, NodeMsg) ->
    Req = hb_ao:get(<<"request">>, EstimateReq, NodeMsg#{ hashpath => ignore }),
    ReqType = hb_ao:get(<<"type">>, EstimateReq, undefined, NodeMsg),
    case {is_operator(Req, NodeMsg), ReqType} of
        {true, _} -> {ok, 0};
        {_, <<"post">>} ->
            % We do not charge after the request has been processed, as balances
            % in the ledger are updated in the pre-processing step.
            {ok, 0};
        {_, <<"pre">>} ->
            Messages = hb_ao:get(<<"body">>, EstimateReq, NodeMsg#{ hashpath => ignore }),
            {ok, length(Messages) * hb_opts:get(simple_pay_price, 1, NodeMsg)}
    end.

%% @doc Preprocess a request by checking the ledger and charging the user. We 
%% can charge the user at this stage because we know statically what the price
%% will be
debit(_, RawReq, NodeMsg) ->
    case hb_ao:get(<<"type">>, RawReq, undefined, NodeMsg) of
        <<"post">> -> {ok, true};
        <<"pre">> ->
            ?event(payment, {debit_preprocessing, RawReq}),
            Req = hb_ao:get(<<"request">>, RawReq, NodeMsg#{ hashpath => ignore }),
            case hb_message:signers(Req) of
                [] -> {ok, false};
                [Signer] ->
                    UserBalance = get_balance(Signer, NodeMsg),
                    Price = hb_ao:get(<<"amount">>, RawReq, 0, NodeMsg),
                    ?event(payment,
                        {debit,
                            {user, Signer},
                            {balance, UserBalance},
                            {price, Price}
                        }),
                    case UserBalance >= Price of
                        true ->
                            set_balance(Signer, UserBalance - Price, NodeMsg),
                            {ok, true};
                        false -> {ok, false}
                    end
            end
    end.

%% @doc Get the balance of a user in the ledger.
balance(_, RawReq, NodeMsg) ->
    Target =
        case hb_ao:get(<<"request">>, RawReq, NodeMsg#{ hashpath => ignore }) of
            not_found -> hd(hb_message:signers(RawReq));
            Req -> hd(hb_message:signers(Req))
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
    Signers = hb_message:signers(Req),
    OperatorAddr = hb_util:human_id(hb_opts:get(operator, undefined, NodeMsg)),
    lists:any(
        fun(Signer) ->
            OperatorAddr =:= hb_util:human_id(Signer)
        end,
        Signers
    ).

%%% Tests

test_opts() -> test_opts(#{}).
test_opts(Ledger) ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger_device">> => <<"simple-pay@1.0">>,
            <<"pricing_device">> => <<"simple-pay@1.0">>
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
    {_HostAddress, HostWallet, Opts} = test_opts(#{ClientAddress => 100}),
    Node = hb_http_server:start_node(Opts),
    {ok, Res} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{<<"path">> => <<"/~simple-pay@1.0/balance">>},
                ClientWallet
            ),
            #{}
        ),
    ?assertEqual(80, Res),
    {ok, NewBalance} =
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~simple-pay@1.0/topup">>,
                    <<"amount">> => 100,
                    <<"recipient">> => ClientAddress
                },
                HostWallet
            ),
            #{}
        ),
    ?assertEqual(180, NewBalance),
    {ok, Res2} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{<<"path">> => <<"/~simple-pay@1.0/balance">>},
                ClientWallet
            ),
            #{}
        ),
    ?assertEqual(160, Res2).
