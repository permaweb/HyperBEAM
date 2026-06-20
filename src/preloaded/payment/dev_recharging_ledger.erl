%%% @doc A P4-compatible ledger with balances that recharge over time.
%%%
%%% Accounts accrue units continuously up to a configured cap. `p4@1.0' can 
%%% query the current effective balance and charge metered usage against it.
-module(dev_recharging_ledger).
-include("include/hb.hrl").

-define(LOOKUP_TIMEOUT, 1000).
-define(DEFAULT_MAX, 1_000).
-define(DEFAULT_MIN, -1_000).
-define(DEFAULT_RECHARGE, 1_000).
-define(DEFAULT_PERIOD, 60).