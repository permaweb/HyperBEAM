-module(dev_pot_math_props).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(DEFAULT_RUNS, 100).

exponentiation_test() ->
    ok = hb_prop:forall(
        #{
            runs => ?DEFAULT_RUNS,
            states => fun(_) -> {hb_prop:int(tiny), hb_prop:int(8)} end,
            requests =>
                fun({X, Y}, _Opts) ->
                    {ok, dev_pot_math:bignum_exp(X, Y)}
                end,
            properties =>
                [
                    fun({X, Y}, _Req, Result, _Opts) ->
                        trunc(math:pow(X, Y)) =:= Result
                    end
                ]
        }
    ).

large_exponentiation_test() ->
    ok = hb_prop:forall(
        #{
            runs => ?DEFAULT_RUNS,
            states => fun(_) -> {hb_prop:int(200), hb_prop:int(200)} end,
            requests =>
                fun({X, Y}, _Opts) ->
                    {ok, dev_pot_math:bignum_exp(X, Y)}
                end,
            properties =>
                [
                    fun({X, Y}, _Req, Result, _Opts) ->
                        reference_crypto_pow(X, Y) =:= Result
                    end
                ]
        }
    ).

reference_crypto_pow(X, Y) ->
    crypto:bytes_to_integer(
        crypto:mod_pow(
            X,
            Y,
            2 bsl 4096
        )
    ).