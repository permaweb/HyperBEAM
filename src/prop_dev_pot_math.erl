-module(prop_dev_pot_math).
-include_lib("proper/include/proper.hrl").
-include("include/hb.hrl").

prop_individual_mints_match_batch_mints() ->
    ?FORALL(
        {
            Steps,
            Max,
            PropN,
            PropD
        },
        {
            integer(100, 1_000),
            integer(100_000_000, 1_000_000_000_000),
            integer(1000, 10_000_000),
            integer(1000, 10_000_000)
        },
        ?IMPLIES(
            PropN < (PropD - 1),
            equals(
                mint_individually(0, Max, PropN, PropD, 0, Steps),
                dev_pot_math:minted_between(0, Max, PropN, PropD, 0, Steps)
            )
        )
    ).

prop_exponentiation() ->
    ?FORALL(
        {X, Y},
        {integer(1, 10), integer(1, 10)},
        equals(
            trunc(math:pow(X, Y)),
            dev_pot_math:bignum_exp(X, Y)
        )
    ).

prop_large_exponentiation() ->
    ?FORALL(
        {X, Y},
        {integer(1, 200), integer(1, 200)},
        equals(
            crypto_exp(X, Y),
            dev_pot_math:bignum_exp(X, Y)
        )
    ).

mint_individually(Minted, Max, PropN, PropD, T, T) -> Minted;
mint_individually(Minted, Max, PropN, PropD, LastT, T) ->
    NewMinted =
        Minted +
        dev_pot_math:minted_between(Minted, Max, PropN, PropD, LastT, T),
    mint_individually(NewMinted, Max, PropN, PropD, LastT + 1, T).

crypto_exp(X, Y) -> crypto_exp(X, Y, dev_pot_math:bignum_exp(2, 4096)).
crypto_exp(X, Y, Mod) ->
    crypto:bytes_to_integer(
        crypto:mod_pow(
            X,
            Y,
            Mod
        )
    ).