%%% @doc An experimental real-time on-demand minting model. Instead of minting
%%% all tokens eagerly, this model mints tokens only on-demand. In doing so,
%%% it significantly reduces the computational and message-passing complexity of the
%%% system.
%%% 
%%% h/t to MakerDAO's DSR and MCD rate accumulation system for some inspiration.
%%% 
%%% The core is this:
%%% 1. Maintain a list of all balances for `resources` that lead to the minting
%%%    of tokens.
%%% 2. With each balance, store the `chi` factor at the time of creation.
%%% 3. When the `drip` function is called, for each time-step since the last `drip`,
%%%    calculate the yield that would have accrued to a balance holding one unit
%%%    of the resource at that time: `rate(TimeStep) * (1/sum(deposits))`.
%%% 4. When balances are requested or utilized, calculate the accrued yield by
%%%    subtracting the current `chi` factor and the initial one. Multiply this
%%%    by the number of units in the deposit. Count this with the existing reward
%%%    balance: `total-balance = (chi - chi0) * deposit + existing-balance`.
%%% 5. When the balance or deposit is modified in any way, first accrue the yield
%%%    to the existing balance. Then perform the operation.
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
    S5 = modify(Addr1, 30, S4),
    S6 = drip(S5#{ <<"t">> => 3 }),
    report(S6),
    S7 = modify(Addr1, -30, S6),
    S8 = drip(S7#{ <<"t">> => 4 }),
    report(S8).

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

drip(S = #{ <<"t">> := T, <<"last-drip">> := Last }) when T =:= Last -> S;
drip(S) ->
    % TODO: Can this be an integral of the rate function?
    drip(drip_once(S)).

drip_once(S = #{ <<"chi">> := Chi, <<"last-drip">> := Last }) ->
    NewChi = Chi + reward_per_unit(S),
    S#{ <<"chi">> => NewChi, <<"last-drip">> => Last + 1 }.

reward_per_unit(_S = #{ <<"last-drip">> := _, <<"total">> := Total }) ->
    % Statically mint one new token proportionate to ownership, for now.
    1 * (1 / Total).

%%% Helpers.

balance(deposit, Addr, #{ <<"balances">> := Balances }) ->
    case maps:find(Addr, Balances) of
        {ok, #{ <<"deposit">> := Deposit }} -> Deposit;
        error -> 0
    end;
balance(reward, Addr, #{ <<"balances">> := Balances, <<"chi">> := ChiN }) ->
    case maps:find(Addr, Balances) of
        {ok, #{ <<"deposit">> := Deposit, <<"chi0">> := Chi0, <<"reward">> := Existing }} ->
            Yield = (ChiN - Chi0) * Deposit,
            Existing + Yield;
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
        <<"balances">> => NewBalances,
        <<"total">> => maps:get(<<"total">>, NewS, 0) + Amount
    }.