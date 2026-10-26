%%% @doc A simple device that allows the operator to specify a price for a
%%% request and then charge the user for it, on a per route and optionally
%%% per message basis.
%%% 
%%% The device's pricing rules are as follows:
%%%
%%% 1. If the request is from the operator, the cost is 0.
%%% 2. If the request matches one of the `router-opts/offered' routes, the
%%%    explicit price of the route is used.
%%% 3. Else, the price is calculated by counting the number of messages in the
%%%    request, and multiplying by the `simple_pay_price' node option, plus the
%%%    price of the apply subrequest if applicable. Subrequests are priced by
%%%    recursively calling `estimate/3' upon them. In the case of an `apply@1.0'
%%%    subrequest, the two initiating apply messages are not counted towards the
%%%    message count price.
%%% 
%%% The node keeps the device's balances in a `lib_volatile_ledger', which
%%% starts from the balances in the node message at `simple_pay_ledger' and
%%% applies one charge or top-up at a time. The operator can top up balances.
%%% When the node's VM restarts, the balances return to `simple_pay_ledger'.
%%% The price is specified in the node message at `simple_pay_price'.
%%% This device acts as both a pricing device and a ledger device, by p4's
%%% definition.
-module(dev_simple_pay).
-device_libraries([lib_volatile_ledger, lib_meta, lib_process]).
-export([estimate/3, charge/3, balance/3, topup/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Estimate the cost of the request, using the rules outlined in the
%% moduledoc.
-spec estimate(#{ _ => _ }, #{ request => #{ _ => _ }, _ => _ }, #{ _ => _ }) ->
    {ok, non_neg_integer()}.
estimate(_Base, EstimateReq, NodeMsg) ->
    Req =
        hb_ao:get(
            <<"request">>,
            EstimateReq,
            NodeMsg#{ <<"hashpath">> => ignore }
        ),
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
apply_price([{as, Device, Msg} | Rest], NodeMsg) when is_map(Msg) ->
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
    RouterOpts = hb_opts:get(<<"router-opts">>, #{}, NodeMsg),
    Routes = hb_maps:get(<<"offered">>, RouterOpts, [], NodeMsg),
    % Match the path without its query, as `dev_p4' matches its non-chargable
    % routes.
    RoutePath =
        case hb_maps:get(<<"path">>, UserRequest, no_path, NodeMsg) of
            no_path -> no_path;
            Path -> hd(binary:split(hb_util:bin(Path), <<"?">>))
        end,
    MatchRes =
        hb_ao:resolve(
            #{
                <<"device">> => <<"router@1.0">>,
                <<"routes">> => Routes
            },
            UserRequest#{
                <<"path">> => <<"match">>,
                <<"route-path">> => RoutePath
            },
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
-spec charge(
    #{ _ => _ },
    #{ request => #{ _ => _ }, quantity => non_neg_integer(), _ => _ },
    #{ _ => _ }
) ->
    {ok, boolean()}
    | {error, #{ status := integer(), body := binary(), _ => _ }}.
charge(_, RawReq, NodeMsg) ->
    ?event(payment, {charge, RawReq}),
    % Only the node may charge: p4 signs its charge with the node's key. The
    % charge reads only the keys that the node signed, so the node's receipt
    % over a `hashpath' or a result it pushed for a caller does not charge.
    case lib_meta:authorized(hb_opts:committer(NodeMsg), RawReq, NodeMsg) of
        {ok, Charge} -> do_charge(Charge, NodeMsg);
        {error, not_authorized} ->
            {error, #{
                <<"status">> => 403,
                <<"body">> => <<"Charge not made by the node.">>
            }}
    end.

%% @doc Debit the signer of the request's `request' by its `quantity'.
do_charge(RawReq, NodeMsg) ->
    Req =
        hb_ao:get(
            <<"request">>,
            RawReq,
            #{},
            NodeMsg#{ <<"hashpath">> => ignore }
        ),
    case hb_message:signers(Req, NodeMsg) of
        [] ->
            ?event(payment, {charge, {error, <<"No signers">>}}),
            {error, #{
                <<"status">> => 400,
                <<"body">> => <<"No signers in charge.">>
            }};
        [Signer] ->
            Price = hb_ao:get(<<"quantity">>, RawReq, 0, NodeMsg),
            NewBalance = debit(Signer, Price, NodeMsg),
            UserBalance = NewBalance + Price,
            ?event(payment,
                {charge,
                    {user, Signer},
                    {balance, UserBalance},
                    {price, Price}
                }),
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
-spec balance(
    #{ _ => _ },
    #{ request => #{ _ => _ }, target => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, integer()}.
balance(_, RawReq, NodeMsg) ->
    Req =
        hb_ao:get(
            <<"request">>,
            RawReq,
            RawReq,
            NodeMsg#{ <<"hashpath">> => ignore }
        ),
    Target =
        case hb_ao:get(<<"target">>, RawReq, undefined, NodeMsg) of
            undefined ->
                case hb_message:signers(Req, NodeMsg) of
                    [] -> undefined;
                    [Signer | _] -> Signer
                end;
            Explicit -> hb_committer:uri(Explicit, NodeMsg)
        end,
    {ok, get_balance(Target, NodeMsg)}.

%% @doc Debit a user's balance, keyed by their committer, and return the new
%% balance. A negative amount credits the user.
debit(Signer, Amount, NodeMsg) ->
    lib_volatile_ledger:debit(
        ledger(NodeMsg),
        Signer,
        Amount,
        NodeMsg
    ).

%% @doc The node's ledger of balances, starting from `simple_pay_ledger'.
ledger(NodeMsg) ->
    #{
        <<"name">> => <<"simple-pay@1.0">>,
        <<"balances">> =>
            maps:fold(
                fun(Account, Balance, Balances) ->
                    Balances#{ hb_committer:uri(Account, NodeMsg) => Balance }
                end,
                #{},
                hb_opts:get(simple_pay_ledger, #{}, NodeMsg)
            )
    }.

%% @doc Get the balance of a user in the ledger. A request with no signer and
%% no `target' gives no user, and has a balance of 0.
get_balance(undefined, _NodeMsg) -> 0;
get_balance(Signer, NodeMsg) ->
    lib_volatile_ledger:balance(
        ledger(NodeMsg),
        Signer,
        NodeMsg
    ).

%% @doc Top up the user's balance in the ledger. We read the `amount' and the
%% `recipient' only from the keys that the operator signed.
-spec topup(
    #{ _ => _ },
    #{ amount => integer(), recipient => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, integer()} | {error, binary() | map()}.
topup(_, Req, NodeMsg) ->
    ?event({topup, {req, Req}, {node_msg, NodeMsg}}),
    case lib_meta:authorized(lib_meta:role(operator, NodeMsg), Req, NodeMsg) of
        {error, _} -> {error, <<"Unauthorized">>};
        {ok, Signed} ->
            Amount = hb_ao:get(<<"amount">>, Signed, 0, NodeMsg),
            case is_integer(Amount) andalso Amount > 0 of
                false -> {error, <<"Top-up amount must be positive.">>};
                true ->
                    Recipient =
                        hb_committer:uri(
                            hb_ao:get(<<"recipient">>, Signed, undefined, NodeMsg),
                            NodeMsg
                        ),
                    % The top-up's unsigned ID is its nonce, so the ledger
                    % applies each signed top-up once.
                    case
                        lib_volatile_ledger:debit(
                            ledger(NodeMsg),
                            Recipient,
                            -Amount,
                            hb_message:id(Signed, NodeMsg),
                            NodeMsg
                        )
                    of
                        {error, already_applied} ->
                            {error, #{
                                <<"status">> => 409,
                                <<"body">> => <<"Top-up already applied.">>
                            }};
                        NewBalance ->
                            ?event(payment,
                                {topup,
                                    {amount, Amount},
                                    {recipient, Recipient},
                                    {balance, NewBalance}
                                }),
                            {ok, NewBalance}
                    end
            end
    end.

%% @doc Check if the request is from the operator, as `meta@1.0' finds it.
is_operator(Req, NodeMsg) ->
    {ok, IsOperator} =
        hb_ao:resolve(
            #{ <<"device">> => <<"meta@1.0">> },
            #{ <<"path">> => <<"is-operator">>, <<"body">> => Req },
            NodeMsg#{ <<"hashpath">> => ignore }
        ),
    IsOperator.

%%% Tests

test_opts(Ledger) ->
    Wallet = ar_wallet:new(),
    Address = hb_test_utils:committer(Wallet),
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
            <<"simple-pay-ledger">> => Ledger,
            <<"simple-pay-price">> => 10,
            <<"operator">> => Address,
            <<"on">> => #{
                <<"request">> => ProcessorMsg,
                <<"response">> => ProcessorMsg
            }
        }
    }.

get_balance_and_top_up_test() ->
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_test_utils:committer(ClientWallet),
    % The ledger keys the client lowercase, as a config file loads it.
    {HostAddress, HostWallet, Opts} =
        test_opts(#{ hb_util:lowercase_key(ClientAddress) => 100 }),
    Node = hb_http_server:start_node(Opts),
    ?event({host_address, HostAddress}),
    ?event({client_address, ClientAddress}),
    {ok, Res} =
        hb_http:get(
            Node,
            Req = hb_message:commit(
                #{<<"path">> => <<"/~simple-pay@1.0/balance">>},
                Opts#{ <<"priv-wallet">> => ClientWallet }
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
                Opts#{ <<"priv-wallet">> => HostWallet }
            ),
            Opts
        ),
    % The balance should now be 180, as the topup will have been added and will
    % not have generated a charge in itself. The top-up did not generate a charge
    % because it is the operator that performed it, and not a user.
    ?assertEqual(180, NewBalance),
    % A node with no `operator' takes a top-up signed by its own wallet.
    ?assertEqual(
        {ok, 100},
        hb_http:post(
            hb_http_server:start_node(#{ <<"priv-wallet">> => HostWallet }),
            <<"/~simple-pay@1.0/topup">>,
            hb_message:commit(
                #{ <<"amount">> => 100, <<"recipient">> => ClientAddress },
                Opts#{ <<"priv-wallet">> => HostWallet }
            ),
            Opts
        )
    ),
    % A charge of a negative quantity is refused, even from the operator.
    ?assertMatch(
        {error, #{ <<"status">> := 400, <<"key">> := <<"quantity">> }},
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~simple-pay@1.0/charge">>,
                    <<"quantity">> => -1000,
                    <<"request">> => Req
                },
                Opts#{ <<"priv-wallet">> => HostWallet }
            ),
            Opts
        )
    ),
    {ok, Res2} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{<<"path">> => <<"/~p4@1.0/balance">>},
                Opts#{ <<"priv-wallet">> => ClientWallet }
            ),
            Opts
        ),
    ?assertEqual(180, Res2),
    % A query parameter does not change the amount that the operator signed.
    ?assertEqual(
        {ok, 280},
        hb_http:post(
            Node,
            <<"/~simple-pay@1.0/topup?amount+integer=1">>,
            hb_message:commit(
                #{ <<"amount">> => 100, <<"recipient">> => ClientAddress },
                Opts#{ <<"priv-wallet">> => HostWallet }
            ),
            Opts
        )
    ),
    % A top-up to the client's Arweave address credits its committer's account.
    ?assertEqual(
        {ok, 380},
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~simple-pay@1.0/topup">>,
                    <<"amount">> => 100,
                    <<"recipient">> =>
                        hb_util:human_id(ar_wallet:to_address(ClientWallet))
                },
                Opts#{ <<"priv-wallet">> => HostWallet }
            ),
            Opts
        )
    ),
    % A balance whose `target' is that address reads its committer's account.
    ?assertEqual(
        {ok, 380},
        hb_http:get(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~simple-pay@1.0/balance">>,
                    <<"target">> =>
                        hb_util:human_id(ar_wallet:to_address(ClientWallet))
                },
                Opts#{ <<"priv-wallet">> => HostWallet }
            ),
            Opts
        )
    ),
    % A client's direct charge is refused, whatever request it carries.
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        hb_http:post(
            Node,
            hb_message:commit(
                #{
                    <<"path">> => <<"/~simple-pay@1.0/charge">>,
                    <<"quantity">> => 10,
                    <<"request">> => Req
                },
                Opts#{ <<"priv-wallet">> => ClientWallet }
            ),
            Opts
        )
    ).

apply_price_test() ->
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_test_utils:committer(ClientWallet),
    ClientOpts = #{ <<"priv-wallet">> => ClientWallet },
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
                Opts#{ <<"priv-wallet">> => ClientWallet }
            ),
            Opts
        ),
    ?assertEqual(60, Res2).
