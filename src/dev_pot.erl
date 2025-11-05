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
single_resource_test() ->
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
        <<"tw">> => 0,
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
    % Calculate the expected balance for Addr1. It is 50% of the remaining supply
    % to mint (25 units), multiplied by the proportion of the total deposits that
    % Addr1 has (3/4), plus the existing balance (37.5).
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

%% @doc Demonstrate minting using the chi-proportional model and a single resource.
multiple_resources_test() ->
    Addr1 = <<"addr1">>,
    Addr2 = <<"addr2">>,
    ResourceID = <<"resource1">>,
    ResourceID2 = <<"resource2">>,
    Opts = #{},
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"tw">> => 0,
        <<"resources">> => #{
            ResourceID => #{
                <<"chi">> => 0,
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            },
            ResourceID2 => #{
                <<"chi">> => 0,
                <<"weight">> => 9,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = modify_deposit(Addr1, ResourceID, 10, S0, Opts),
    S1b = modify_deposit(Addr2, ResourceID, 10, S1, Opts),
    S2 = modify_deposit(Addr1, ResourceID2, 10, S1b, Opts),
    S2b = modify_deposit(Addr2, ResourceID2, 10, S2, Opts),
    {ok, S3} = hb_ao:resolve(S2b, <<"drip">>, Opts),
    report(S3),
    ?assertEqual(25.0, balance(Addr1, S3)),
    ?assertEqual(25.0, balance(Addr2, S3)),
    S4 = drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4),
    ?assertEqual(37.5, balance(Addr1, S4)),
    ?assertEqual(37.5, balance(Addr2, S4)),
    % Set Addr1 to have 75% of the total deposits.
    S5a = modify_deposit(Addr1, ResourceID2, -10, S4, Opts),
    S5b = modify_deposit(Addr2, ResourceID, -10, S5a, Opts),
    % Calculate the expected balance for Addr1. It is 50% of the remaining supply
    % to mint (25 units), multiplied by the proportion of the total deposits that
    % Addr1 has (3/4), multiplied by the weight of the resource over the total
    % weight (1/10), plus the existing balance (37.5).
    NewExpectedB1 = (((25 / 2) * (1 / 1)) * (1 / 10)) + 37.5,
    S6 = drip(S5b, #{ <<"t">> => 3 }, Opts),
    report(S6),
    ?assertEqual(NewExpectedB1, balance(Addr1, S6)),
    % Set both to be equal again.
    S7a = modify_deposit(Addr1, ResourceID2, 10, S6, Opts),
    S7b = modify_deposit(Addr2, ResourceID, 10, S7a, Opts),
    report(S7b),
    Addr1BalPreFinal = balance(Addr1, S7b),
    Addr2BalPreFinal = balance(Addr2, S7b),
    S8 = drip(S7b, #{ <<"t">> => 4 }, Opts),
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
            {chi, resource_chis(S)},
            {tw, hb_maps:get(<<"tw">>, S)},
            {balances, balances(S)},
            {deposits, deposits(S)},
            {minted, hb_maps:get(<<"minted">>, S)},
            {state, S}
        }
    ).

%%% Pot Model.

drip(State, Req, Opts) ->
    SWithTime =
        case is_map(Req) andalso hb_maps:find(<<"t">>, Req) of
            {ok, TReq} -> State#{ <<"t">> => TReq };
            _ -> State#{ <<"t">> => hb_maps:get(<<"t">>, State, 0) + 1 }
        end,
    drip(SWithTime, Opts).

drip(S = #{ <<"t">> := T, <<"last-drip">> := Last }, _Opts) when T =:= Last -> S;
drip(S = #{
        <<"t">> := T,
        <<"mint-cap">> := Max,
        <<"mint-prop">> := Proportion,
        <<"resources">> := _Resources
    }, Opts) ->
    Minted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    Steps = max(T - LastT, 0),
    case Steps =:= 0 of
        true -> S;
        false ->
            ToMint = units_minted_between(Minted, Max, Proportion, LastT, T),
            TW = hb_maps:get(<<"tw">>, S),
            DeltaM = case TW of 0 -> 0; _ -> ToMint / TW end,
            M0 = hb_maps:get(<<"chi">>, S, 0, Opts),
            S#{
                <<"chi">> => M0 + DeltaM,
                <<"last-drip">> => T,
                <<"minted">> => Minted + ToMint
            }
    end.

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

modify_deposit(Addr, ResourceID, Amount, S, Opts) ->
    NewS0 = drip(S, Opts),
    #{ <<"balances">> := Balances0, <<"resources">> := Resources0 } = NewS0,
    ExistingDeposit = deposit(Addr, ResourceID, NewS0),
    NewDepositQty = ExistingDeposit + Amount,
    RealizedBalance = balance(Addr, NewS0),
    % Reset chi0 for this address across all resources to current chi
    MNow = hb_maps:get(<<"chi">>, NewS0, 0),
    ResourcesReset =
        hb_maps:map(
            fun(_ResID, Res) ->
                ResDeposits = hb_maps:get(<<"deposits">>, Res, #{}),
                case hb_maps:find(Addr, ResDeposits) of
                    error -> Res;
                    {ok, Entry} ->
                        ResChiBase = hb_maps:get(<<"chi">>, Res, 0),
                        ResWeight = hb_maps:get(<<"weight">>, Res, 0),
                        ResM0 = hb_maps:get(<<"m0">>, Res, 0),
                        ResChi = ResChiBase + ResWeight * (MNow - ResM0),
                        Res#{
                            <<"deposits">> =>
                                ResDeposits#{
                                    Addr =>
                                        Entry#{
                                            <<"chi0">> => ResChi
                                        }
                                }
                        }
                end
            end,
            Resources0
        ),
    ResR0 = hb_maps:get(ResourceID, ResourcesReset, #{}),
    ResRDeposits0 = hb_maps:get(<<"deposits">>, ResR0, #{}),
    ResRChi =
        hb_maps:get(<<"chi">>, ResR0, 0) +
        hb_maps:get(<<"weight">>, ResR0, 0) *
        (MNow - hb_maps:get(<<"m0">>, ResR0, 0)),
    ResRTotal0 = hb_maps:get(<<"total-deposits">>, ResR0, 0),
    ResR1 = ResR0#{
        <<"deposits">> =>
            ResRDeposits0#{
                Addr => #{
                    <<"quantity">> => NewDepositQty,
                    <<"chi0">> => ResRChi
                }
            },
        <<"total-deposits">> => ResRTotal0 + Amount
    },
    Resources1 = ResourcesReset#{ ResourceID => ResR1 },
    WeightR = hb_maps:get(<<"weight">>, ResR0, 0),
    Tw0 = hb_maps:get(<<"tw">>, NewS0),
    NewS0#{
        <<"resources">> => Resources1,
        <<"tw">> => Tw0 + (WeightR * Amount),
        <<"balances">> => Balances0#{ Addr => RealizedBalance }
    }.

%%% Helpers.

deposit(Addr, ResourceID, S) ->
    hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits/", Addr/binary, "/quantity">>,
        S,
        0,
        #{}
    ).

balance(Addr, S) ->
    ExistingBalance = hb_maps:get(Addr, hb_maps:get(<<"balances">>, S, #{}), 0),
    Resources = hb_maps:get(<<"resources">>, S, #{}),
    MNow = hb_maps:get(<<"chi">>, S, 0),
    Yield =
        lists:sum(
            lists:map(
                fun(Res) ->
                    ResBase = hb_maps:get(<<"chi">>, Res, 0),
                    ResW = hb_maps:get(<<"weight">>, Res, 0),
                    M0 = hb_maps:get(<<"m0">>, Res, 0),
                    ChiEff = ResBase + ResW * (MNow - M0),
                    Deposits = hb_maps:get(<<"deposits">>, Res, #{}),
                    case hb_maps:find(Addr, Deposits) of
                        error -> 0;
                        {ok, #{ <<"quantity">> := Qty, <<"chi0">> := Chi0 }} ->
                            ?no_prod("Remove all floating point arithmetic."),
                            (ChiEff - Chi0) * Qty
                    end
                end,
                hb_maps:values(Resources)
            )
        ),
    ExistingBalance + Yield.

balances(S = #{ <<"balances">> := Bs }) ->
    hb_maps:map(fun(Addr, _) -> balance(Addr, S) end, Bs).

deposits(S = #{ <<"resources">> := Resources }) ->
    hb_maps:map(fun(ResourceID, _) -> deposits(ResourceID, S) end, Resources).
deposits(ResourceID, S) ->
    Ds = hb_ao:get(
        <<"/resources/", ResourceID/binary, "/deposits">>,
        S,
        #{},
        #{}
    ),
    hb_maps:map(fun(Addr, _) -> deposit(Addr, ResourceID, S) end, Ds).

resource_chis(S = #{ <<"resources">> := Resources }) ->
    MNow = hb_maps:get(<<"chi">>, S, 0),
    hb_maps:map(
        fun(_ResID, Res) ->
            Base = hb_maps:get(<<"chi">>, Res, 0),
            W = hb_maps:get(<<"weight">>, Res, 0),
            M0 = hb_maps:get(<<"m0">>, Res, 0),
            Base + W * (MNow - M0)
        end,
        Resources
    ).