%%% @doc A P4-compatible ledger with balances that recharge over time.
%%%
%%% Accounts accrue units continuously up to a configured cap. `p4@1.0' can 
%%% query the current effective balance and charge metered usage against it.
-module(dev_recharging_ledger).
-export([balance/3]).
-include("include/hb.hrl").

-define(LOOKUP_TIMEOUT, 1000).
-define(DEFAULT_MAX, 1_000).
-define(DEFAULT_MIN, -1_000).
-define(DEFAULT_RECHARGE, 1_000).
-define(DEFAULT_PERIOD, 60).

account_id(Address) ->
    hb_util:human_id(Address).

%% @doc Get the current effective balance for a P4 target account.
balance(_, Req, Opts) ->
    Target = hb_ao:get(<<"target">>, Req, Opts),
    {ok, get_balance(account_id(Target), Opts)}.

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
    Min = hb_opts:get(recharging_ledger_min, ?DEFAULT_MIN, Opts),
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
            {min, Min},
            {recharge, Recharge},
            {period, Period},
            {exempt, Exempt}
        }
    ),
    server_loop(
        #{
            max => Max,
            min => Min,
            recharge => Recharge,
            period => Period,
            accounts => #{ account_id(Account) => infinity || Account <- Exempt }
        }
    ).

server_loop(State) ->
    receive
        {balance, PID, AccountID} ->
            PID ! {balance, account_balance(AccountID, State)},
            server_loop(State)
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