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
    S8 = drip(S7#{ <<"t">> => 4 }),
    report(S8),
    S9 = set_rate(2.0, S8),
    report(S9),
    S10 = drip(S9#{ <<"t">> => 5 }),
    report(S10),
    S11 = set_rate(1.0, S10),
    report(S11).

report(S) ->
    ?event(
        {report,
            {t, maps:get(<<"t">>, S)},
            {balances, balances(S)},
            {state, S}
        }
    ).

%%% Pot Model.

pie(Balance, Chi) ->
    Balance / Chi.

% balance(Normalized, ChiN) ->
%     Normalized * ChiN.

set_rate(Rate, S) ->
    (drip(S))#{ <<"rate">> => Rate }.

drip(S = #{
        <<"last-drip">> := Last,
        <<"chi">> := Chi,
        <<"t">> := T,
        <<"rate">> := Rate }) ->
    NewChi = Chi * math:pow(Rate, T - Last),
    S#{ <<"chi">> => NewChi, <<"last-drip">> => T }.

%%% Mint Model.

% mint(#{ <<"t">> := _ }) ->
%     % TODO: Static rate for now.
%     1.

% mint_between(Start, End, S) ->
%     % TODO: Can this be an integral of the rate function?
%     lists:sum(
%         lists:map(fun(T) -> mint(S#{ <<"t">> => T }) end,
%         lists:seq(Start, End))
%     ).

%%% Helpers.

balance(Addr, #{ <<"chi">> := Chi, <<"balances">> := Balances }) ->
    maps:get(Addr, Balances, 0) * Chi.

balances(S) ->
    maps:map(
        fun(Addr, _) -> balance(Addr, S) end,
        maps:get(<<"balances">>, S, #{})
    ).

supply(#{ <<"total">> := Total, <<"chi">> := Chi }) ->
    Total * Chi.

modify(Addr, Amount, S) ->
    ExistingBalance = balance(Addr, NewS = drip(S)),
    NewBalance = ExistingBalance + Amount,
    ?event(
        {modify,
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
        <<"total">> => maps:get(<<"total">>, NewS, 0) + Amount
    }.