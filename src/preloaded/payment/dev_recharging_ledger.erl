%%% @doc A P4-compatible ledger with balances that recharge over time.
%%%
%%% Accounts accrue units continuously up to a configured cap. `p4@1.0' can
%%% query the current effective balance and charge metered usage against it.
-module(dev_recharging_ledger).
-export([balance/3, charge/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOOKUP_TIMEOUT, 1000).
-define(DEFAULT_MAX, 1_000).
-define(DEFAULT_RECHARGE, 1_000).
-define(DEFAULT_PERIOD, 60).

account_id(Address) ->
    hb_util:human_id(Address).

%% @doc Get the current effective balance for a P4 target account. P4 supplies
%% the `target' key during its pre-request balance check. This function
%% normalizes that target to the account key used by the ledger server, then
%% asks the server for a read-only balance. The server returns `infinity' for
%% exempt accounts, the configured max balance for accounts it has not seen
%% before, or the stored balance plus elapsed recharge for existing accounts.
balance(_, Req, Opts) ->
    Target = hb_ao:get(<<"target">>, Req, Opts),
    {ok, get_balance(account_id(Target), Opts)}.

%% @doc Charge metered usage against a P4 account. P4 supplies the `account'
%% key on the post-response commit path and the `quantity' to deduct. The charge
%% succeeds only when the current effective balance can cover the quantity.
charge(_, Req, Opts) ->
    Account = hb_ao:get(<<"account">>, Req, Opts),
    Quantity = hb_util:int(hb_ao:get(<<"quantity">>, Req, 0, Opts)),
    charge_balance(account_id(Account), Quantity, Opts).

get_balance(AccountID, Opts) ->
    PID = ensure_server_started(Opts),
    PID ! {balance, self(), AccountID},
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
    PID ! {charge, self(), AccountID, Quantity},
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
    Exempt = hb_opts:get(recharging_ledger_exempt, [], Opts),
    ?event(
        recharging_ledger,
        {started_recharging_ledger,
            {server_id, ServerID},
            {max, Max},
            {recharge, Recharge},
            {period, Period},
            {exempt, Exempt}
        }
    ),
    server_loop(
        #{
            max => Max,
            recharge => Recharge,
            period => Period,
            accounts => #{ account_id(Account) => infinity || Account <- Exempt }
        }
    ).

server_loop(State) ->
    receive
        {balance, PID, AccountID} ->
            PID ! {balance, account_balance(AccountID, State)},
            server_loop(State);
        {charge, PID, AccountID, Quantity} ->
            {Result, NewState} =
                charge_account(
                    AccountID,
                    Quantity,
                    State,
                    erlang:system_time(millisecond)
                ),
            PID ! {charged, Result},
            server_loop(NewState)
    end.

charge_account(AccountID, Quantity, State = #{ accounts := Accounts }, Now) ->
    case account_balance(AccountID, State, Now) of
        infinity ->
            {{ok, true}, State};
        Balance when Balance >= Quantity ->
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

account_balance(AccountID, State) ->
    account_balance(AccountID, State, erlang:system_time(millisecond)).
account_balance(
        AccountID,
        #{ max := Max, recharge := Recharge, period := Period, accounts := Accounts },
        Time
    ) ->
    case maps:get(AccountID, Accounts, not_found) of
        infinity -> infinity;
        not_found -> Max;
        #{ balance := Balance, last := LastInteraction } ->
            RechargeRate = Recharge / (Period * 1000),
            RechargedSinceLast = (Time - LastInteraction) * RechargeRate,
            min(Max, Balance + RechargedSinceLast)
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
        {ok, 75.0},
        balance(#{}, #{ <<"target">> => Account }, Opts)
    ).

insufficient_balance_returns_402_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 10,
        <<"recharging-ledger-recharge">> => 0
    },
    ?assertMatch(
        {error, #{ <<"status">> := 402 }},
        charge(#{}, #{<<"account">> => Account, <<"quantity">> => 25}, Opts)).

insufficient_balance_does_not_mutate_balance_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Opts = #{
        <<"priv-wallet">> => ar_wallet:new(),
        <<"recharging-ledger-max">> => 10,
        <<"recharging-ledger-recharge">> => 0
    },
    ?assertMatch(
        {error, #{ <<"status">> := 402}},
        charge(#{}, #{ <<"account">> => Account, <<"quantity">> => 25 }, Opts)
    ),
    ?assertEqual(
        {ok, 10},
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
        {error, #{ <<"status">> := 400}},
        charge(#{}, #{<<"account">> => Account, <<"quantity">> => -1}, Opts)
    ).