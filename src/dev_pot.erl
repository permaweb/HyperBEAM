%%% @doc An experimental real-time on-demand minting model, based largely on
%%% notions from the MakerDAO DSR and MCD rate accumulation system.
%%% 
%%% The theory is this:
%%% 1. The period between changes in the state of deposits provides periods in
%%%    which the state of the mint (and thus the rate of yield accrual) is stable.
%%% 2. Rather than eagerly at every mint timestep, yield accrual can thus be 
%%%    calculated instead at the end of each stable period. Even though the rate
%%%    of yield decays slightly per timestep, the summation of that yield should
%%%    be nonetheless calculable efficiently.
%%% 3. By normalizing balances against a common "chi" factor, all balances can
%%%    be effectively updated by only updating one factor. Upon balance requests
%%%    and token movements, the explicit balance per address can be renormalized.
%%% 4. `Sub-mints' can be created by setting access rights on addresses proportionate
%%%    to accrued yield during delegations to that address. For example, Alice
%%%    may delegate her yield to ProjectA. While delegated, ProjectA receives
%%%    the yield accrued from the `base' mint, while Alice's balance in the 
%%%    `sub-mint' increases according to its separate rate.
-module(dev_pot).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

core_test() ->
    Addr1 = <<"addr1">>,
    Addr2 = <<"addr2">>,
    Chi0 = 1,
    S0 = #{
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => Chi0,
        <<"rate">> => 1.1,
        <<"balances">> => #{ },
        <<"total">> => 0
    },
    S1 = modify(Addr1, 1, S0),
    S2 = modify(Addr2, 1, S1),
    report(S2),
    S3 = drip(S2#{ <<"t">> => 1 }),
    report(S3),
    S4 = drip(S3#{ <<"t">> => 2 }),
    report(S4),
    S5 = modify(Addr1, 3, S4),
    S6 = drip(S5#{ <<"t">> => 3 }),
    report(S6),
    S7 = modify(Addr1, -2, S6),
    report(S7),
    S8 = drip(S7#{ <<"t">> => 4 }).

report(S) ->
    ?event(
        {report,
            {t, maps:get(<<"t">>, S)},
            {supply, supply(S)},
            {balances, balances(S)},
            {state, S}
        }
    ).

%%% Pot Model.

pie(Human, Chi) -> Human / Chi.
unpie(Normalized, ChiN) -> Normalized * ChiN.

drip(S = #{ <<"t">> := T, <<"last-drip">> := Last }) when T =:= Last -> S;
drip(S) ->
    % TODO: Can this be an integral of the rate function?
    drip(drip_once(S)).

drip_once(S = #{ <<"chi">> := Chi, <<"last-drip">> := Last }) ->
    Supply = supply(S),
    NewTokens = rate(S),
    Multiplier = NewTokens / Supply,
    NewChi = Chi * (1 + Multiplier),
    S#{ <<"chi">> => NewChi, <<"last-drip">> => Last + 1 }.

rate(_) ->
    % Statically mint one new token proportionate to ownership, for now.
    1.

%%% Helpers.

balance(Addr, #{ <<"chi">> := Chi, <<"balances">> := Balances }) ->
    maps:get(Addr, Balances, 0) * Chi.

supply(#{ <<"total">> := Total, <<"chi">> := Chi }) ->
    unpie(Total, Chi).

balances(S) ->
    maps:map(
        fun(Addr, _) -> balance(Addr, S) end,
        maps:get(<<"balances">>, S, #{})
    ).

modify(Addr, Amount, S = #{ <<"chi">> := Chi }) ->
    ExistingBalance = balance(Addr, NewS = drip(S)),
    Supply = supply(NewS),
    NewBalance = ExistingBalance + Amount,
    ?event(
        {modify,
            {supply, Supply},
            {addr, Addr},
            {existing_balance, ExistingBalance},
            {amount, Amount},
            {new_balance, NewBalance}
        }
    ),
    NewS#{
        <<"balances">> =>
            maps:put(
                Addr,
                pie(NewBalance, maps:get(<<"chi">>, NewS)),
                maps:get(<<"balances">>, NewS, #{})
            ),
        <<"total">> => pie(Supply + Amount, Chi)
    }.