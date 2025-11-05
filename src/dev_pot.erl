%%% @doc An experimental real-time on-demand minting model, based largely on
%%% notions from the MakerDAO DSR and MCD rate accumulation system.
%%% 
%%% The theory is this:
%%% 1. The period between changes in the state of deposits represent opportunities
%%%    to calculate multiple yield periods in a single operation.
%%% 2. Rather than eagerly at every mint timestep, yield accrual can be 
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
        <<"balances">> => #{ }
    },
    S1 = modify(Addr1, 10, S0),
    S2 = modify(Addr2, 10, S1),
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
    _S8 = drip(S7#{ <<"t">> => 4 }).

report(S) ->
    ?event(
        {report,
            {t, maps:get(<<"t">>, S)},
            {balances, balances(deposit, S)},
            {reward_balances, balances(reward, S)},
            {state, S}
        }
    ).

%%% Pot Model.

pie(Human, Chi) -> Human / Chi.
unpie(Normalized, Chi) -> Normalized * Chi.

drip(S = #{ <<"t">> := T, <<"last-drip">> := Last }) when T =:= Last -> S;
drip(S) ->
    % TODO: Can this be an integral of the rate function?
    drip(drip_once(S)).

drip_once(S = #{ <<"chi">> := Chi, <<"last-drip">> := Last }) ->
    NewChi = Chi * (1 + rate(S)),
    S#{ <<"chi">> => NewChi, <<"last-drip">> => Last + 1 }.

rate(_S = #{ <<"last-drip">> := _ }) ->
    % Statically mint one new token proportionate to ownership, for now.
    0.1.

%%% Helpers.

balance(deposit, Addr, #{ <<"balances">> := Balances }) ->
    case maps:find(Addr, Balances) of
        {ok, #{ <<"deposit">> := Deposit }} -> Deposit;
        error -> 0
    end;
balance(reward, Addr, #{ <<"balances">> := Balances, <<"chi">> := ChiN }) ->
    case maps:find(Addr, Balances) of
        {ok, Entry = #{ <<"deposit">> := Deposit, <<"chi0">> := Chi0 }} ->
            % Calculate the acrued yield and add it to the existing reward balance.
            Yield = unpie(pie(Deposit, Chi0), ChiN) - Deposit,
            Yield + maps:get(<<"reward">>, Entry, 0);
        error -> 0
    end.

balances(Token, S) ->
    maps:map(
        fun(Addr, _) -> balance(Token, Addr, S) end,
        maps:get(<<"balances">>, S, #{})
    ).

modify(Addr, Amount, S) ->
    ExistingDeposit = balance(deposit, Addr, NewS = drip(S)),
    ExistingReward = balance(reward, Addr, NewS),
    NewDeposit = ExistingDeposit + Amount,
    ?event(
        {modify,
            {addr, Addr},
            {existing_reward, ExistingReward},
            {existing_deposit, ExistingDeposit},
            {amount, Amount},
            {new_deposit, NewDeposit}
        }
    ),
    CurrentChi = maps:get(<<"chi">>, NewS),
    NewEntry = #{
        <<"deposit">> => NewDeposit,
        <<"chi0">> => CurrentChi,
        <<"reward">> => ExistingReward
    },
    NewBalances = maps:put(
        Addr,
        NewEntry,
        maps:get(<<"balances">>, NewS, #{})
    ),
    NewS#{
        <<"balances">> => NewBalances
    }.