%%% @doc A static recharge-rate provider for `recharging-ledger@1.0'.
%%%
%%% Messages using this device store account-specific integer recharge rates
%%% under `rates'. Unknown accounts fall back to the caller's default.
-module(dev_recharging_ledger_rates).
-implements(<<"recharging-ledger-rates@1.0">>).
-export([info/1, default/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

info(_) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"remove">>, <<"rates">>],
        default => fun default/4
    }.

%% @doc Return the configured recharge rate for an account key.
default(AccountID, Base, _Req, Opts) ->
    Rates = hb_maps:get(<<"rates">>, Base, #{}, Opts),
    case hb_maps:find(AccountID, Rates, Opts) of
        {ok, Rate} ->
            rate_result(Rate);
        error ->
            {error, not_found}
    end.

rate_result(Rate) ->
    case hb_util:safe_int(Rate) of
        {ok, Int} when Int >= 0 -> {ok, Int};
        _ -> {error, invalid_rate}
    end.

%%% Tests

configured_rate_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Msg = #{
        <<"device">> => <<"recharging-ledger-rates@1.0">>,
        <<"rates">> => #{ Account => <<"20">> }
    },
    ?assertEqual(20, hb_ao:get(Account, Msg, 10, #{})).

missing_account_uses_caller_default_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Msg = #{
        <<"device">> => <<"recharging-ledger-rates@1.0">>,
        <<"rates">> => #{ <<"other-account">> => 20 }
    },
    ?assertEqual(10, hb_ao:get(Account, Msg, 10, #{})).

invalid_rate_uses_caller_default_test() ->
    Account = hb_util:human_id(ar_wallet:to_address(ar_wallet:new())),
    Msg = #{
        <<"device">> => <<"recharging-ledger-rates@1.0">>,
        <<"rates">> => #{ Account => <<"bad">> }
    },
    ?assertEqual(10, hb_ao:get(Account, Msg, 10, #{})).
