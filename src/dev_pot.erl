%%% @doc An experimental real-time on-demand minting model. Instead of minting
%%% all tokens eagerly, this model mints tokens only on-demand. In doing so,
%%% it significantly reduces the computational and message-passing complexity of the
%%% system.
%%% 
%%% h/t to MakerDAO's DSR and MCD rate accumulation system for some inspiration.
%%% 
%%% The core minting model is this:
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
%%% 
%%% This device supports delegating resources to other addresses, allowing for
%%% mechanisms like yield-swaps etc to be created downstream. Each delegation
%%% triggers a `Delegation-Notice` message to be sent to the recipient of the
%%% delegation, as well as a proportional increase in the recipient's `deposit`
%%% value. Reciprocally, the delegator's `deposit` value is decreased by the same
%%% amount, while the delegation itself is recorded in the `delegations`
%%% message. When a delegation is revoked, this setup is reversed and a new
%%% `Delegation-Notice` message is sent with `quantity` set to zero.
%%% 
%%% This structure allows downstream minting processes to credit `Delegation-Notice`s
%%% as deposits in their own mechanism. By tracking the delegators and performing
%%% their own mints using the same `pot` functionality as the parent, depositors
%%% in the original process can earn their yield in the form of `child` mints.
%%% Each mint can operate asynchronously and in real-time.
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
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"deposits">> => #{ },
        <<"balances">> => #{ }
    },
    S1 = modify_deposit(Addr1, 10, S0),
    S2 = modify_deposit(Addr2, 10, S1),
    report(S2),
    S3 = drip(S2#{ <<"t">> => 1 }),
    report(S3),
    S4 = drip(S3#{ <<"t">> => 2 }),
    report(S4),
    S5 = modify_deposit(Addr1, 20, S4),
    S6 = drip(S5#{ <<"t">> => 3 }),
    report(S6),
    S7 = modify_deposit(Addr1, -20, S6),
    S8 = drip(S7#{ <<"t">> => 4 }),
    report(S8).

drip_test() ->
    ?assertEqual(50.0, units_minted_between(0, 100, 0.5, 0, 1)),
    ?assertEqual(75.0, units_minted_between(0, 100, 0.5, 0, 2)),
    ?assertEqual(87.5, units_minted_between(0, 100, 0.5, 0, 3)),
    Period1 = units_minted_between(0, 100, 0.5, 0, 2),
    Period2 = units_minted_between(Period1, 100, 0.5, 2, 3),
    ?assertEqual(87.5, Period1 + Period2).

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
drip(S = #{
        <<"chi">> := Chi,
        <<"t">> := T,
        <<"mint-cap">> := Max,
        <<"mint-prop">> := Proportion
    }) ->
    Minted = maps:get(<<"minted">>, S, 0),
    LastT = maps:get(<<"last-drip">>, S, 0),
    ToMint = units_minted_between(Minted, Max, Proportion, LastT, T),
    S#{
        <<"chi">> => Chi + reward_units_per_resource_unit(ToMint, S),
        <<"last-drip">> => T,
        <<"minted">> => Minted + ToMint
    }.

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

reward_units_per_resource_unit(ToMint, S) ->
    ToMint * (1 / maps:get(<<"total-deposits">>, S, 0)).

modify_deposit(Addr, Amount, S) ->
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
        <<"total-deposits">> => maps:get(<<"total-deposits">>, NewS, 0) + Amount
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