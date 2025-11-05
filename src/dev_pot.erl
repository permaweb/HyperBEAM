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

delegate(FromAddr, TargetAddr, Qty, S) ->
    % Accrue before modifying deposits
    S1 = drip(S),
    Delegations0 = maps:get(<<"delegations">>, S1, #{}),
    FromDelegations0 = maps:get(FromAddr, Delegations0, #{}),
    ExistingQty = maps:get(TargetAddr, FromDelegations0, 0),
    Delta = Qty - ExistingQty,
    % Move Delta from FromAddr to TargetAddr (can be negative to reverse)
    S2 = modify_deposit(FromAddr, -Delta, S1),
    S3 = modify_deposit(TargetAddr, Delta, S2),
    ?event({delegation_notice, {from, FromAddr}, {to, TargetAddr}, {quantity, Qty}}),
    UpdatedFromDelegations =
        case Qty of
            0 -> maps:remove(TargetAddr, FromDelegations0);
            _ -> FromDelegations0#{ TargetAddr => Qty }
        end,
    UpdatedDelegations =
        case map_size(UpdatedFromDelegations) of
            0 -> maps:remove(FromAddr, Delegations0);
            _ -> Delegations0#{ FromAddr => UpdatedFromDelegations }
        end,
    S3#{ <<"delegations">> => UpdatedDelegations }.

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