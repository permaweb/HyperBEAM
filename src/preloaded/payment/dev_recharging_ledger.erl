%%% @doc A P4-compatible ledger with balances that recharge over time.
%%%
%%% Accounts accrue units continuously up to a configured cap. `p4@1.0' can
%%% query the current effective balance and charge metered usage against it.
%%% Balances are integer ledger units. Operators should choose units fine-grained
%%% enough for their pricing and recharge policy.
%%% Defaults provide a 24-hour bucket that recharges at 1 unit per second.
%%% An optional `recharging-ledger-rates' message can provide account-specific
%%% recharge rates.
%%% An optional `recharging-ledger-grace' allows small post-metering negative
%%% balance drift without making that grace appear as spendable balance.
-module(dev_recharging_ledger).
-export([balance/3, charge/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOOKUP_TIMEOUT, 1000).
-define(DEFAULT_MAX, 86_400).
-define(DEFAULT_RECHARGE, 1).
-define(DEFAULT_PERIOD, 1).
-define(DEFAULT_GRACE, 3_600).

account_id(Address) ->
    hb_util:human_id(Address).

%% @doc Get the current effective balance for a P4 account. P4 supplies the
%% `target' key during pre-request checks and the signed `request' key when its
%% balance endpoint delegates to the ledger. This function normalizes that
%% account to the key used by the ledger server, then asks the server for a
%% read-only balance. The server returns `infinity' for exempt accounts, the
%% configured max balance for accounts it has not seen before, or the stored
%% balance plus elapsed recharge for existing accounts.
balance(_, Req, Opts) ->
    case balance_account(Req, Opts) of
        {ok, AccountID} -> {ok, get_balance(AccountID, Opts)};
        {error, Error} -> {error, Error}
    end.

%% @doc Charge metered usage against a P4 account. P4 supplies the `account'
%% key on the post-response commit path and the `quantity' to deduct. The charge
%% succeeds only when the current effective balance can cover the quantity, plus
%% any configured negative-balance grace.
charge(_, Req, Opts) ->
    Account = hb_ao:get(<<"account">>, Req, Opts),
    case hb_util:safe_int(hb_ao:get(<<"quantity">>, Req, 0, Opts)) of
        {ok, Quantity} ->
            charge_balance(account_id(Account), Quantity, Opts);
        {error, _} ->
            {error, #{
                <<"status">> => 400,
                <<"body">> => <<"Invalid charge quantity.">>
            }}
    end.

balance_account(Req, Opts) ->
    case hb_ao:get(<<"target">>, Req, not_found, Opts) of
        not_found ->
            request_account(
                hb_ao:get(
                    <<"request">>,
                    Req,
                    not_found,
                    Opts#{ <<"hashpath">> => ignore }
                ),
                Opts
            );
        Target ->
            {ok, account_id(Target)}
    end.

request_account(not_found, _Opts) ->
    {error, #{
        <<"status">> => 400,
        <<"body">> => <<"Balance request must include target or signed request.">>
    }};
request_account(Request, Opts) ->
    case hb_message:signers(Request, Opts) of
        [Signer] ->
            {ok, account_id(Signer)};
        [] ->
            {error, #{
                <<"status">> => 400,
                <<"body">> => <<"Balance request has no signer.">>
            }};
        _ ->
            {error, #{
                <<"status">> => 400,
                <<"body">> => <<"Balance request has multiple signers.">>
            }}
    end.

get_balance(AccountID, Opts) ->
    PID = ensure_server_started(Opts),
    PID ! {balance, self(), AccountID, Opts},
    receive
        {balance, Balance} ->
            Balance
    after ?LOOKUP_TIMEOUT ->
        ?event(warning, {recharging_ledger_timeout, restarting}),
        hb_name:unregister(server_id(Opts)),
        get_balance(AccountID, Opts)
    end.

charge_balance(_AccountID, Quantity, _Opts) when Quantity < 0 ->
    {error, #{
        <<"status">> => 400,
        <<"body">> => <<"Charge quantity must be non-negative.">>
    }};
charge_balance(AccountID, Quantity, Opts) ->
    PID = ensure_server_started(Opts),
    PID ! {charge, self(), AccountID, Quantity, Opts},
    receive
        {charged, Result} ->
            Result
    after ?LOOKUP_TIMEOUT ->
        ?event(warning, {recharging_ledger_timeout, restarting}),
        hb_name:unregister(server_id(Opts)),
        charge_balance(AccountID, Quantity, Opts)
    end.

server_id(Opts) ->
    PrivWallet = hb_opts:get(priv_wallet, undefined, Opts),
    {?MODULE, account_id(PrivWallet)}.

ensure_server_started(Opts) ->
    ServerID = server_id(Opts),
    hb_name:singleton(
        ServerID,
        fun() -> start_server(ServerID, Opts) end
    ).

start_server(ServerID, Opts) ->
    Max = hb_opts:get(recharging_ledger_max, ?DEFAULT_MAX, Opts),
    Recharge =
        hb_opts:get(
            recharging_ledger_recharge,
            ?DEFAULT_RECHARGE,
            Opts
        ),
    Period = hb_opts:get(recharging_ledger_period, ?DEFAULT_PERIOD, Opts),
    Grace = hb_opts:get(recharging_ledger_grace, ?DEFAULT_GRACE, Opts),
    RatesMessage = hb_opts:get(recharging_ledger_rates, undefined, Opts),
    Exempt = hb_opts:get(recharging_ledger_exempt, [], Opts),
    ?event(
        recharging_ledger,
        {started_recharging_ledger,
            {server_id, ServerID},
            {max, Max},
            {recharge, Recharge},
            {period, Period},
            {grace, Grace},
            {rates, RatesMessage},
            {exempt, Exempt}
        }
    ),
    server_loop(
        #{
            max => Max,
            recharge => Recharge,
            period => Period,
            grace => Grace,
            rates => RatesMessage,
            accounts => #{ account_id(Account) => infinity || Account <- Exempt }
        }
    ).

server_loop(State) ->
    receive
        {balance, PID, AccountID, Opts} ->
            PID ! {balance, account_balance(AccountID, State, Opts)},
            server_loop(State);
        {charge, PID, AccountID, Quantity, Opts} ->
            {Result, NewState} =
                charge_account(
                    AccountID,
                    Quantity,
                    State,
                    erlang:system_time(millisecond),
                    Opts
                ),
            PID ! {charged, Result},
            server_loop(NewState)
    end.

charge_account(
        AccountID,
        Quantity,
        State = #{ accounts := Accounts },
        Now,
        Opts
    ) ->
    Grace = maps:get(grace, State, ?DEFAULT_GRACE),
    case account_balance(AccountID, State, Now, Opts) of
        infinity ->
            {{ok, true}, State};
        Balance when Balance - Quantity >= -Grace ->
            {{ok, true},
                State#{
                    accounts =>
                        Accounts#{
                            AccountID =>
                                #{
                                    balance => Balance - Quantity,
                                    last => Now
                                }
                        }
                }};
        _Balance ->
            {{error, #{
                <<"status">> => 402,
                <<"body">> => <<"Insufficient recharging ledger balance.">>
            }}, State}
    end.

account_balance(AccountID, State, Time) when is_integer(Time) ->
    account_balance(AccountID, State, Time, #{});
account_balance(AccountID, State, Opts) ->
    account_balance(AccountID, State, erlang:system_time(millisecond), Opts).
account_balance(
        AccountID,
        State = #{ max := Max, recharge := Recharge, period := Period, accounts := Accounts },
        Time,
        Opts
    ) ->
    case maps:get(AccountID, Accounts, not_found) of
        infinity -> infinity;
        not_found -> Max;
        #{ balance := Balance, last := LastInteraction } ->
            AccountRecharge = account_recharge(AccountID, Recharge, State, Opts),
            PeriodMs = Period * 1000,
            Elapsed = Time - LastInteraction,
            RechargedSinceLast = (Elapsed * AccountRecharge) div PeriodMs,
            min(Max, Balance + RechargedSinceLast)
    end.

account_recharge(AccountID, DefaultRecharge, State, Opts) ->
    case maps:get(rates, State, undefined) of
        undefined -> DefaultRecharge;
        RatesMessage ->
            % Ask the optional rates provider for this account's recharge rate.
            case
                hb_util:safe_int(
                    hb_ao:get(AccountID, RatesMessage, DefaultRecharge, Opts)
                )
            of
                {ok, Recharge} when Recharge >= 0 -> Recharge;
                _ -> DefaultRecharge
            end
    end.

%%% Tests

balance_new_target_returns_max_test() ->
    Target = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100
    },
    ?assertEqual(
        {ok, 100},
        balance(#{}, #{ <<"target">> => Target }, Opts)
    ).

default_bucket_is_one_day_at_one_unit_per_second_test() ->
    Account = <<"account">>,
    State = #{
        max => ?DEFAULT_MAX,
        recharge => ?DEFAULT_RECHARGE,
        period => ?DEFAULT_PERIOD,
        accounts => #{ Account => #{ balance => 0, last => 0 } }
    },
    ?assertEqual(86_400, account_balance(<<"new">>, State, 0, #{})),
    ?assertEqual(1, account_balance(Account, State, 1000, #{})).

charge_new_account_deducts_units_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100,
        <<"recharging-ledger-recharge">> => 0
    },
    ?assertEqual(
        {ok, true},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => 25 }, Opts)
    ),
    ?assertEqual(
        {ok, 75},
        balance(#{}, #{ <<"target">> => Account }, Opts)
    ).

p4_balance_request_uses_signed_request_signer_test() ->
    Wallet = ar_wallet:new(),
    Account = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100,
        <<"recharging-ledger-recharge">> => 0
    },
    {ok, true} =
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => 25 }, Opts),
    Request =
        hb_message:commit(
            #{ <<"path">> => <<"/greeting">> },
            #{ <<"priv-wallet">> => Wallet }
        ),
    ?assertEqual(
        {ok, 75},
        balance(#{}, #{ <<"request">> => Request }, Opts)
    ).

balance_request_without_signer_returns_400_test() ->
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100,
        <<"recharging-ledger-recharge">> => 0
    },
    ?assertMatch(
        {error, #{ <<"status">> := 400 }},
        balance(#{}, #{ <<"request">> => #{ <<"path">> => <<"/greeting">> } }, Opts)
    ).

insufficient_balance_returns_402_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 10,
        <<"recharging-ledger-recharge">> => 0,
        <<"recharging-ledger-grace">> => 0
    },
    ?assertMatch(
        {error, #{ <<"status">> := 402 }},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => 25 }, Opts)
    ).

insufficient_balance_does_not_mutate_balance_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 10,
        <<"recharging-ledger-recharge">> => 0,
        <<"recharging-ledger-grace">> => 0
    },
    ?assertMatch(
        {error, #{ <<"status">> := 402 }},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => 25 }, Opts)
    ),
    ?assertEqual(
        {ok, 10},
        balance(#{}, #{ <<"target">> => Account }, Opts)
    ).

grace_allows_small_negative_balance_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100,
        <<"recharging-ledger-recharge">> => 0,
        <<"recharging-ledger-grace">> => 10
    },
    ?assertEqual(
        {ok, true},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => 105 }, Opts)
    ),
    ?assertEqual(
        {ok, -5},
        balance(#{}, #{ <<"target">> => Account }, Opts)
    ).

grace_rejects_beyond_negative_limit_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100,
        <<"recharging-ledger-recharge">> => 0,
        <<"recharging-ledger-grace">> => 10
    },
    ?assertMatch(
        {error, #{ <<"status">> := 402 }},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => 111 }, Opts)
    ),
    ?assertEqual(
        {ok, 100},
        balance(#{}, #{ <<"target">> => Account }, Opts)
    ).

negative_quantity_returns_400_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 10,
        <<"recharging-ledger-recharge">> => 10
    },
    ?assertMatch(
        {error, #{ <<"status">> := 400 }},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => -1 }, Opts)
    ).

invalid_quantity_returns_400_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 10,
        <<"recharging-ledger-recharge">> => 10
    },
    ?assertMatch(
        {error, #{ <<"status">> := 400 }},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => <<"bad">> }, Opts)
    ).

exempt_account_returns_infinity_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100,
        <<"recharging-ledger-recharge">> => 100,
        <<"recharging-ledger-exempt">> => [Account]
    },
    ?assertEqual(
        {ok, infinity},
        balance(#{}, #{ <<"target">> => Account }, Opts)
    ).

exempt_account_charge_does_not_mutate_balance_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100,
        <<"recharging-ledger-recharge">> => 100,
        <<"recharging-ledger-exempt">> => [Account]
    },
    {ok, true} =
        charge(
            #{},
            #{ <<"account">> => Account, <<"quantity">> => 1000 },
            Opts
        ),
    ?assertEqual(
        {ok, infinity},
        balance(#{}, #{ <<"target">> => Account }, Opts)
    ).

recharge_restores_balance_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    State = #{
        max => 100,
        recharge => 10,
        period => 1,
        accounts => #{ Account => #{ balance => 20, last => 0 } }
    },
    ?assertEqual(25, account_balance(Account, State, 500)).

recharge_restores_negative_balance_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    State = #{
        max => 100,
        recharge => 10,
        period => 1,
        accounts => #{ Account => #{ balance => -5, last => 0 } }
    },
    ?assertEqual(0, account_balance(Account, State, 500)).

recharge_caps_at_max_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    State = #{
        max => 100,
        recharge => 10,
        period => 1,
        accounts => #{ Account => #{ balance => 95, last => 0 } }
    },
    ?assertEqual(100, account_balance(Account, State, 1000)).

rates_message_overrides_default_recharge_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    State = #{
        max => 100,
        recharge => 10,
        period => 1,
        rates => #{ Account => 20 },
        accounts => #{ Account => #{ balance => 20, last => 0 } }
    },
    ?assertEqual(30, account_balance(Account, State, 500)).

rates_message_missing_account_uses_default_recharge_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    State = #{
        max => 100,
        recharge => 10,
        period => 1,
        rates => #{ <<"other-account">> => 20 },
        accounts => #{ Account => #{ balance => 20, last => 0 } }
    },
    ?assertEqual(25, account_balance(Account, State, 500)).

rates_message_invalid_account_rate_uses_default_recharge_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    State = #{
        max => 100,
        recharge => 10,
        period => 1,
        rates => #{ Account => <<"bad">> },
        accounts => #{ Account => #{ balance => 20, last => 0 } }
    },
    ?assertEqual(25, account_balance(Account, State, 500)).

account_balance_uses_rates_device_message_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    RatesMessage = #{
        <<"device">> => <<"recharging-ledger-rates@1.0">>,
        <<"rates">> => #{ Account => 20 }
    },
    State = #{
        max => 100,
        recharge => 10,
        period => 1,
        rates => RatesMessage,
        accounts => #{ Account => #{ balance => 20, last => 0 } }
    },
    ?assertEqual(30, account_balance(Account, State, 500, #{})).

recharging_ledger_rates_device_integration_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 100,
        <<"recharging-ledger-recharge">> => 0,
        <<"recharging-ledger-period">> => 1,
        <<"recharging-ledger-rates">> => #{
            <<"device">> => <<"recharging-ledger-rates@1.0">>,
            <<"rates">> => #{ Account => 10_000 }
        }
    },
    ?assertEqual(
        {ok, true},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => 90 }, Opts)
    ),
    timer:sleep(20),
    ?assertEqual(
        {ok, 100},
        balance(#{}, #{ <<"target">> => Account }, Opts)
    ).

p4_metering_bundler_charges_recharging_ledger_bytes_test() ->
    HostWallet = ar_wallet:new(),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Rate = 2,
    Item =
        hb_message:commit(
            #{
                <<"data">> => <<"metered-recharging-ledger-bundler-item">>,
                <<"test">> => <<"p4-recharging-ledger-metering">>
            },
            #{ <<"priv-wallet">> => ar_wallet:new() }
        ),
    {ServerHandle, GatewayOpts} =
        hb_mock_server:start_arweave_gateway(
            #{
                price => {200, <<"12345">>},
                tx_anchor => {200, hb_util:encode(rand:bytes(32))}
            }
        ),
    Processor = #{
        <<"device">> => <<"p4@1.0">>,
        <<"ledger-device">> => <<"recharging-ledger@1.0">>,
        <<"pricing-device">> => <<"metering@1.0">>
    },
    BaseOpts =
        GatewayOpts#{
            <<"priv-wallet">> => HostWallet,
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"metering-rates">> => #{
                <<"arweave-bytes">> => Rate,
                <<"beam-reductions">> => 0
            },
            <<"operator">> => ar_wallet:to_address(HostWallet),
            <<"recharging-ledger-recharge">> => 0,
            <<"on">> => #{
                <<"request">> => Processor,
                <<"response">> => Processor
            }
        },
    ItemSize =
        byte_size(
            ar_bundles:serialize(
                hb_message:convert(
                    Item,
                    #{
                        <<"device">> => <<"ans104@1.0">>,
                        <<"bundle">> => true
                    },
                    <<"structured@1.0">>,
                    BaseOpts
                )
            )
        ),
    Opts =
        BaseOpts#{
            <<"recharging-ledger-max">> => (ItemSize * Rate) + 50
        },
    try
        Node = hb_http_server:start_node(Opts),
        UploadReq =
            hb_message:commit(
                #{
                    <<"path">> => <<"/~bundler@1.0/tx">>,
                    <<"bundler-subject">> => <<"body">>,
                    <<"body">> => Item
                },
                Opts#{ <<"priv-wallet">> => Wallet }
            ),
        ?assertMatch({ok, _}, hb_http:post(Node, UploadReq, Opts)),
        [_] = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(
            {ok, 50},
            balance(#{}, #{ <<"target">> => Address }, Opts)
        )
    after
        hb_mock_server:stop(ServerHandle)
    end.

p4_charges_recharging_ledger_simple_pay_test() ->
    HostWallet = ar_wallet:new(),
    OperatorWallet = ar_wallet:new(),
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    Processor = #{
        <<"device">> => <<"p4@1.0">>,
        <<"pricing-device">> => <<"simple-pay@1.0">>,
        <<"ledger-device">> => <<"recharging-ledger@1.0">>
    },
    Opts = #{
        <<"priv-wallet">> => HostWallet,
        <<"operator">> => ar_wallet:to_address(OperatorWallet),
        <<"simple-pay-price">> => 0,
        <<"recharging-ledger-max">> => 10,
        <<"recharging-ledger-recharge">> => 0,
        <<"router-opts">> => #{
            <<"offered">> => [
                #{
                    <<"template">> => <<"/greeting">>,
                    <<"price">> => 3
                }
            ]
        },
        <<"on">> => #{
            <<"request">> => Processor,
            <<"response">> => Processor
        }
    },
    Node = hb_http_server:start_node(Opts),
    Req =
        hb_message:commit(
            #{
                <<"path">> => <<"/greeting">>,
                <<"greeting">> => <<"Hello from P4">>
            },
            #{ <<"priv-wallet">> => ClientWallet }
        ),
    ?assertEqual({ok, <<"Hello from P4">>}, hb_http:get(Node, Req, #{})),
    {ok, Balance} =
        hb_http:get(
            Node,
            hb_message:commit(
                #{ <<"path">> => <<"/~p4@1.0/balance">> },
                #{ <<"priv-wallet">> => ClientWallet }
            ),
            #{ <<"priv-wallet">> => ClientWallet }
        ),
    ?assertEqual(7, Balance),
    ?assertEqual({ok, 7}, balance(#{}, #{ <<"target">> => ClientAddress }, Opts)).
