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
%%% This device will support delegating resources to other addresses, allowing for
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
%%% 
%%% The structure of the state is as follows:
%%% 
%%% /resources/ID/chi: The current chi factor for every deposit in the resource.
%%% /resources/ID/weight: The weight of the resource in the minting process.
%%% /resources/ID/total-deposits: The total quantity of units deposited of the
%%% resource.
%%% /resources/ID/deposits/ADDR/quantity: The quantity of the resource deposited
%%% by a specific address.
%%% /resources/ID/deposits/ADDR/chi0: The initial chi factor at the time of the
%%% deposit.
%%% /balances/ADDR: The current minted asset balance of an address.
%%% /minted: The total number of units minted.
%%% /mint-cap: The maximum number of units that can be minted.
%%% /mint-prop: The proportion of the mint-cap that is minted per time-step.
%%% /last-drip: The last time the drip function was called.
%%% /t: The current time-step.
%%% 
%%% TODO:
%%% - Add support for multiple resources: Each should have a chi, a set of 
%%%   deposits, and a resource-weight.
%%% - Implement support for delegations, as described in the documentation above.
%%% - Add `secure-set` (set guarded by address) for resource-weights and 
%%%   supported resources.
%%% - 
-module(dev_pot).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([drip/3]).

%% @doc Demonstrate minting using the chi-proportional model and a single resource.
chi_proportional_mint_test() ->
    Addr1 = <<"addr1">>,
    Addr2 = <<"addr2">>,
    ResourceID = <<"resource1">>,
    Opts = #{},
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"resources">> => #{
            ResourceID => #{
                <<"chi">> => 0,
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = modify_deposit(Addr1, ResourceID, 10, S0, Opts),
    S2 = modify_deposit(Addr2, ResourceID, 10, S1, Opts),
    {ok, S3} = hb_ao:resolve(S2, <<"drip">>, Opts),
    report(S3),
    ?assertEqual(25.0, balance(Addr1, S3)),
    ?assertEqual(25.0, balance(Addr2, S3)),
    S4 = drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4),
    ?assertEqual(37.5, balance(Addr1, S4)),
    ?assertEqual(37.5, balance(Addr2, S4)),
    % Set Addr1 to have 75% of the total deposits.
    S5 = modify_deposit(Addr1, ResourceID, 20, S4, Opts),
    NewExpectedB1 = ((25 / 2) * (3 / 4)) + 37.5,
    S6 = drip(S5, #{ <<"t">> => 3 }, Opts),
    report(S6),
    ?assertEqual(NewExpectedB1, balance(Addr1, S6)),
    % Set both to be equal again.
    S7 = modify_deposit(Addr1, ResourceID, -20, S6, Opts),
    report(S7),
    Addr1BalPreFinal = balance(Addr1, S7),
    Addr2BalPreFinal = balance(Addr2, S7),
    S8 = drip(S7, #{ <<"t">> => 4 }, Opts),
    % Ensure that they were again minted equal quantities.
    Addr1Diff = balance(Addr1, S8) - Addr1BalPreFinal,
    Addr2Diff = balance(Addr2, S8) - Addr2BalPreFinal,
    ?assertEqual(Addr1Diff, Addr2Diff).

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
            {t, hb_maps:get(<<"t">>, S)},
            {last_drip, hb_maps:get(<<"last-drip">>, S)},
            {chi, hb_maps:get(<<"chi">>, S)},
            {balances, balances(S)},
            {deposits, deposits(S)},
            {minted, hb_maps:get(<<"minted">>, S)}
        }
    ).

%%% Pot Model.

drip(State, _Req, Opts) ->
    {ok, drip(State, Opts)}.

drip(S = #{ <<"t">> := T, <<"last-drip">> := Last }, _Opts) when T =:= Last -> S;
drip(S = #{
        <<"chi">> := Chi,
        <<"t">> := T,
        <<"mint-cap">> := Max,
        <<"mint-prop">> := Proportion
    }, Opts) ->
    Minted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    ToMint = units_minted_between(Minted, Max, Proportion, LastT, T),
    S#{
        <<"chi">> => Chi + reward_units_per_resource_unit(ToMint, S, Opts),
        <<"last-drip">> => T,
        <<"minted">> => Minted + ToMint
    }.

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

reward_units_per_resource_unit(ToMint, S, Opts) ->
    ToMint * (1 / hb_maps:get(<<"total-deposits">>, S, 0, Opts)).

modify_deposit(Addr, ResourceID, Amount, S, Opts) ->
    NewS = #{
        <<"balances">> := Balances,
        <<"chi">> := CurrentChi,
        <<"resources">> := Resources
    } = drip(S, Opts),
    % GET /resources/ID/deposits
    Deposits =
        hb_ao:get(
            <<"/resources/", ResourceID/binary, "/deposits">>,
            NewS,
            #{},
            Opts
        ),
    ExistingTotalDeposits =
        hb_ao:get(
            <<"/resources/", ResourceID/binary, "/total-deposits">>,
            S,
            0,
            Opts
        ),
    ExistingDeposit = deposit(Addr, ResourceID, NewS),
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
            Resources#{
                ResourceID => #{
                    <<"deposits">> =>
                        Deposits#{
                            Addr => #{
                                <<"deposit">> => NewDeposit,
                                <<"chi0">> => CurrentChi
                            }
                        },
                    <<"total-deposits">> => ExistingTotalDeposits + Amount
                }
            },
        <<"balances">> => Balances#{ Addr => Balance }
    }.

%%% Helpers.

deposit(Addr, ResourceID, S) ->
    hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        S,
        #{},
        #{}
    ).

balance(Addr, #{ <<"balances">> := Bs, <<"deposits">> := Ds, <<"chi">> := ChiN }) ->
    ExistingBalance = hb_maps:get(Addr, Bs, 0),
    case hb_maps:find(Addr, Ds) of
        error -> ExistingBalance;
        {ok, #{ <<"deposit">> := Deposit, <<"chi0">> := Chi0 }} ->
            ?no_prod("Remove all floating point arithmetic."),
            Yield = (ChiN - Chi0) * Deposit,
            ExistingBalance + Yield
    end.

balances(S = #{ <<"balances">> := Bs }) ->
    hb_maps:map(fun(Addr, _) -> balance(Addr, S) end, Bs).

deposits(S) ->
    hb_maps:map(fun(ResourceID, _) -> deposits(ResourceID, S) end, S).
deposits(ResourceID, S = #{ <<"deposits">> := Ds }) ->
    hb_maps:map(fun(Addr, _) -> deposit(Addr, ResourceID, S) end, Ds).