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
    S0 = #{
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => 0,
        <<"deposits">> => #{ },
        <<"balances">> => #{ }
    },
    S1 = modify(Addr1, 10, S0),
    S2 = modify(Addr2, 10, S1),
    report(S2),
    S3 = drip(S2#{ <<"t">> => 1 }),
    report(S3),
    S4 = drip(S3#{ <<"t">> => 2 }),
    report(S4),
    S5 = modify(Addr1, 20, S4),
    S6 = drip(S5#{ <<"t">> => 3 }),
    report(S6),
    S7 = modify(Addr1, -20, S6),
    S8 = drip(S7#{ <<"t">> => 4 }),
    report(S8).

report(S) ->
    ?event(
        {report,
            {t, maps:get(<<"t">>, S)},
            {balances, balances(S)},
            {deposits, deposits(S)},
            {state, S}
        }
    ).

%%% Pot Model.

drip(S = #{ <<"t">> := T, <<"last-drip">> := Last }) when T =:= Last -> S;
drip(S) ->
    % TODO: Can this be an integral of the rate function?
    drip(drip_once(S)).

drip_once(S = #{ <<"chi">> := Chi, <<"last-drip">> := Last }) ->
    NewChi = Chi + reward_per_unit_per_timestep(S),
    S#{ <<"chi">> => NewChi, <<"last-drip">> => Last + 1 }.

reward_per_unit_per_timestep(#{ <<"total">> := Total }) ->
    % Statically mint one new token proportionate to ownership, for now.
    1 * (1 / Total).

modify(Addr, Amount, S) ->
    NewS = #{
        <<"balances">> := Balances,
        <<"deposits">> := Deposits,
        <<"chi">> := CurrentChi
    } = drip(S),
    ExistingDeposit = deposit(Addr, NewS),
    NewDeposit = ExistingDeposit + Amount,
    Balance = balance(Addr, NewS),
    ?event(
        {modify,
            {addr, Addr},
            {balance, Balance},
            {deposit, ExistingDeposit},
            {amount, Amount},
            {new_deposit, NewDeposit}
        }
    ),
    NewS#{
        <<"deposits">> =>
            Deposits#{
                Addr => #{
                    <<"deposit">> => NewDeposit,
                    <<"chi0">> => CurrentChi
                }
            },
        <<"balances">> => Balances#{ Addr => Balance },
        <<"total">> => maps:get(<<"total">>, NewS, 0) + Amount
    }.

%%% Helpers.

deposit(Addr, #{ <<"deposits">> := Ds }) ->
    maps:get(<<"deposit">>, maps:get(Addr, Ds, #{ <<"deposit">> => 0 }), 0).

balance(Addr, #{ <<"balances">> := Bs, <<"deposits">> := Ds, <<"chi">> := ChiN }) ->
    ExistingBalance = maps:get(Addr, Bs, 0),
    case maps:find(Addr, Ds) of
        error -> ExistingBalance;
        {ok, #{ <<"deposit">> := Deposit, <<"chi0">> := Chi0 }} ->
            ?no_prod("Remove all floating point arithmetic."),
            Yield = (ChiN - Chi0) * Deposit,
            ExistingBalance + Yield
    end.

balances(S = #{ <<"balances">> := Bs }) ->
    maps:map(fun(Addr, _) -> balance(Addr, S) end, Bs).

deposits(S = #{ <<"deposits">> := Ds }) ->
    maps:map(fun(Addr, _) -> deposit(Addr, S) end, Ds).