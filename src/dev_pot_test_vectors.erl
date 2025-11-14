%%% @doc Test vectors for the `~pot@1.0` device.
-module(dev_pot_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

drip_global(Acc, ToMint, TotalWeightedUnits) ->
    Acc + (ToMint / TotalWeightedUnits).

drip_resource(ResourceAcc, GlobalAcc, LastGlobalAcc, Weight) ->
    ResourceAcc + ((GlobalAcc - LastGlobalAcc) * Weight).

drip_user(Balance, ResourceAcc, LastResourceAcc, UserQty) ->
    Balance + ((ResourceAcc - LastResourceAcc) * UserQty).

cascade_drip_test() ->
    % Initial state.
    UserBalance0 = 0,
    UserQty0 = 1,
    ResourceWeight0 = 1,
    ResourceAcc0 = 0,
    ResourceLastGlobalAcc0 = 0,
    GlobalAcc0 = 0,
    GlobalToMint0 = 50,
    GlobalTWU0 = 1,
    % ~~~~~~~~~~~~~ Epoch 1 ~~~~~~~~~~~~~
    % Drip the global.
    GlobalAcc1 = drip_global(GlobalAcc0, GlobalToMint0, GlobalTWU0),
    % Drip the resource.
    ResourceAcc1 = drip_resource(ResourceAcc0, GlobalAcc1, ResourceLastGlobalAcc0, ResourceWeight0),
    % Drip the user.
    UserBalance1 = drip_user(UserBalance0, ResourceAcc1, ResourceAcc0, UserQty0),
    ?event({results,
        {global_accumulator, GlobalAcc1},
        {resource_accumulator, ResourceAcc1},
        {user_balance, UserBalance1}
    }),
    % ~~~~~~~~~~~~~ Epoch 2 ~~~~~~~~~~~~~
    % Set changed parameters.
    GlobalToMint1 = 25,
    GlobalTWU1 = 10,
    ResourceWeight1 = 10,
    UserQty1 = 1,
    % Drip the global.
    GlobalAcc2 = drip_global(GlobalAcc1, GlobalToMint1, GlobalTWU1),
    % Drip the resource.
    ResourceAcc2 = drip_resource(ResourceAcc1, GlobalAcc2, GlobalAcc1, ResourceWeight1),
    % Drip the user.
    UserBalance2 = drip_user(UserBalance1, ResourceAcc2, ResourceAcc1, UserQty1),
    ?event({results,
        {global_accumulator, GlobalAcc2},
        {resource_accumulator, ResourceAcc2},
        {user_balance, UserBalance2}
    }),
    ok.

time_weighted_average_test() ->
    T0 = 0,
    Acc0 = 100,
    Deposit1Time0 = T0,
    Deposit1AccumulatedWeightTime0 = Acc0,
    Weight0 = 10,
    % Calculate the accumulated weight for t=1, while weight is 10.
    Weight1 = Weight0,
    T1 = T0 + 1,
    Acc1 = dev_pot_math:accumulate_resource_weight(T0, T1, Weight1, Acc0),
    % Calculate the accumulated weight for t=2, while weight is 20.
    Weight2 = 20,
    T2 = T1 + 20,
    Acc2 = dev_pot_math:accumulate_resource_weight(T1, T2, Weight2, Acc1),
    Deposit2Time0 = T2,
    Deposit2AccumulatedWeightTime0 = Acc2,
    % Calculate the accumulated weight for t=3, while weight is 30.
    Weight3 = 30,
    T3 = T2 + 10,
    Acc3 = dev_pot_math:accumulate_resource_weight(T2, T3, Weight3, Acc2),
    % Calculate the accumulated weight for t=4, while weight is 50.
    Weight4 = 50,
    T4 = T3 + 10,
    Acc4 = dev_pot_math:accumulate_resource_weight(T3, T4, Weight4, Acc3),
    % Calculate the accumulated weight for 1,000 timesteps while weight is 2.
    Weight5 = 2,
    T5 = T4 + 100000,
    Acc5 = dev_pot_math:accumulate_resource_weight(T4, T5, Weight5, Acc4),
    % Calculate the average weight for the period deposit period (T0 -> T4).
    Deposit1AvgWeight =
        dev_pot_math:user_resource_weight(
            Deposit1Time0,
            T5,
            Deposit1AccumulatedWeightTime0,
            Acc5
        ),
    ?event({deposit1_average_weight, Deposit1AvgWeight}),
    Deposit2AvgWeight =
        dev_pot_math:user_resource_weight(
            Deposit2Time0,
            T5,
            Deposit2AccumulatedWeightTime0,
            Acc5
        ),
    ?event({deposit2_average_weight, Deposit2AvgWeight}).

mint_quantity_test() ->
    ?assertEqual(50.0, dev_pot_math:units_minted_between(0, 100, 0.5, 0, 1)),
    ?assertEqual(75.0, dev_pot_math:units_minted_between(0, 100, 0.5, 0, 2)),
    ?assertEqual(87.5, dev_pot_math:units_minted_between(0, 100, 0.5, 0, 3)),
    Period1 = dev_pot_math:units_minted_between(0, 100, 0.5, 0, 2),
    Period2 = dev_pot_math:units_minted_between(Period1, 100, 0.5, 2, 3),
    ?assertEqual(87.5, Period1 + Period2).

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
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"resources">> => #{
            ResourceID => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:modify_deposit(Addr1, ResourceID, 10, S0, Opts),
    S2 = dev_pot:modify_deposit(Addr2, ResourceID, 10, S1, Opts),
    report(S2),
    S3 = dev_pot:drip(S2, #{ <<"t">> => 1 }, Opts),
    report(S3),
    ?assertEqual(25.0, dev_pot:balance(Addr1, S3)),
    ?assertEqual(25.0, dev_pot:balance(Addr2, S3)),
    S4 = dev_pot:drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4),
    ?assertEqual(37.5, dev_pot:balance(Addr1, S4)),
    ?assertEqual(37.5, dev_pot:balance(Addr2, S4)),
    % Set Addr1 to have 75% of the total deposits.
    S5 = dev_pot:modify_deposit(Addr1, ResourceID, 20, S4, Opts),
    report(S5),
    % Calculate the expected balance for Addr1. It is 50% of the remaining supply
    % to mint (25 units), multiplied by the proportion of the total deposits that
    % Addr1 has (3/4), plus the existing balance (37.5).
    NewExpectedB1 = ((25 / 2) * (3 / 4)) + 37.5,
    S6 = dev_pot:drip(S5, #{ <<"t">> => 3 }, Opts),
    report(S6),
    ?assertEqual(NewExpectedB1, dev_pot:balance(Addr1, S6)),
    % Set both to be equal again.
    S7 = dev_pot:modify_deposit(Addr1, ResourceID, -20, S6, Opts),
    report(S7),
    Addr1BalPreFinal = dev_pot:balance(Addr1, S7),
    Addr2BalPreFinal = dev_pot:balance(Addr2, S7),
    S8 = dev_pot:drip(S7, #{ <<"t">> => 4 }, Opts),
    % Ensure that they were again minted equal quantities.
    Addr1Diff = dev_pot:balance(Addr1, S8) - Addr1BalPreFinal,
    Addr2Diff = dev_pot:balance(Addr2, S8) - Addr2BalPreFinal,
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
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            },
            ResourceID2 => #{
                <<"weight">> => 9,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:modify_deposit(Addr1, ResourceID, 10, S0, Opts),
    S1b = dev_pot:modify_deposit(Addr2, ResourceID, 10, S1, Opts),
    S2 = dev_pot:modify_deposit(Addr1, ResourceID2, 10, S1b, Opts),
    S2b = dev_pot:modify_deposit(Addr2, ResourceID2, 10, S2, Opts),
    {ok, S3} = hb_ao:resolve(S2b, <<"drip">>, Opts),
    report(S3),
    ?assertEqual(25.0, dev_pot:balance(Addr1, S3)),
    ?assertEqual(25.0, dev_pot:balance(Addr2, S3)),
    S4 = dev_pot:drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4),
    ?assertEqual(37.5, dev_pot:balance(Addr1, S4)),
    ?assertEqual(37.5, dev_pot:balance(Addr2, S4)),
    % Set Addr1 to have 75% of the total deposits.
    S5a = dev_pot:modify_deposit(Addr1, ResourceID2, -10, S4, Opts),
    S5b = dev_pot:modify_deposit(Addr2, ResourceID, -10, S5a, Opts),
    % Calculate the expected balance for Addr1. It is 50% of the remaining supply
    % to mint (25 units), multiplied by the proportion of the total deposits that
    % Addr1 has (3/4), multiplied by the weight of the resource over the total
    % weight (1/10), plus the existing balance (37.5).
    NewExpectedB1 = (((25 / 2) * (1 / 1)) * (1 / 10)) + 37.5,
    S6 = dev_pot:drip(S5b, #{ <<"t">> => 3 }, Opts),
    report(S6),
    ?assertEqual(NewExpectedB1, dev_pot:balance(Addr1, S6)),
    % Set both to be equal again.
    S7a = dev_pot:modify_deposit(Addr1, ResourceID2, 10, S6, Opts),
    S7b = dev_pot:modify_deposit(Addr2, ResourceID, 10, S7a, Opts),
    report(S7b),
    Addr1BalPreFinal = dev_pot:balance(Addr1, S7b),
    Addr2BalPreFinal = dev_pot:balance(Addr2, S7b),
    S8 = dev_pot:drip(S7b, #{ <<"t">> => 4 }, Opts),
    % Ensure that they were again minted equal quantities.
    Addr1Diff = dev_pot:balance(Addr1, S8) - Addr1BalPreFinal,
    Addr2Diff = dev_pot:balance(Addr2, S8) - Addr2BalPreFinal,
    ?assertEqual(Addr1Diff, Addr2Diff).

%% Modify Qty or Weight:
%%     NormalizedReward = Reward * (OldWeightedUnits / NewWeightedUnits)
%% Drip:
%%     Rewardn+1 = NormalizedReward + (Minted / NewWeightedUnits)
%% Balance:
%%     (Reward/RewardAtStart) * ResourceWeight * Qty
scaled_reward_test() ->
    % Constants.
    UserQuantity = 1,
    % Per-step state.
    StepReward0 = 50,
    RewardAcc0 = 1,
    GlobalWU0 = 1,
    ResourceW0 = 1,
    % First drip.
    NewlyMintedPerWeightedUnit = StepReward0 / GlobalWU0,
    RewardAcc1 = RewardAcc0 + NewlyMintedPerWeightedUnit,
    ?event(
        {results,
            {reward_accumulator, RewardAcc1},
            {newly_minted_per_weighted_unit, NewlyMintedPerWeightedUnit}
        }
    ),
    % Set the resource weight to 10, scaling the reward accumulator.
    StepReward1 = 25,
    ResourceW1 = 10,
    GlobalWU1 = ResourceW1 * UserQuantity,
    RewardAcc1b = RewardAcc1 * (ResourceW0 / ResourceW1),
    NewlyMintedPerWeightedUnit1 = StepReward1 / GlobalWU1,
    RewardAcc2 = RewardAcc1b + NewlyMintedPerWeightedUnit1,
    % Calculate the user reward.
    RenormalizedReward = RewardAcc2 / RewardAcc0,
    UserReward = RenormalizedReward * UserQuantity * ResourceW1,
    ?event(
        {results,
            {renormalized_reward, RenormalizedReward},
            {user_reward, UserReward},
            {newly_minted_per_weighted_unit, NewlyMintedPerWeightedUnit}
        }
    ),
    ?assertEqual(75, UserReward),
    ok.

single_resource_modified_weight_test() ->
    Opts = #{},
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:modify_deposit(<<"alice">>, <<"oxygen">>, 1, S0, Opts),
    {ok, S3} = hb_ao:resolve(S1, <<"drip">>, Opts),
    ?hr(),
    S4 = dev_pot:set_weight(<<"oxygen">>, 10, S3, Opts),
    ?hr(),
    report(S4),
    {ok, S5} = hb_ao:resolve(S4, <<"drip">>, Opts),
    report(S5),
    ?assertEqual(75.0, dev_pot:balance(<<"alice">>, S5)),
    ok.

multiresource_modified_weight_test() ->
    Opts = #{},
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            },
            <<"hydrogen">> => #{
                <<"weight">> => 0,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:modify_deposit(<<"alice">>, <<"oxygen">>, 1, S0, Opts),
    %S1b = dev_pot:modify_deposit(<<"bob">>, <<"oxygen">>, 10, S1, Opts),
    S2 = dev_pot:modify_deposit(<<"bob">>, <<"hydrogen">>, 1, S1, Opts),
    %S2b = dev_pot:modify_deposit(<<"alice">>, <<"hydrogen">>, 10, S2, Opts),
    {ok, S3} = hb_ao:resolve(S2, <<"drip">>, Opts),
    ?hr(),
    % report(S3),
    % ?assertEqual(25.0, dev_pot:balance(<<"alice">>, S3)),
    % ?assertEqual(25.0, dev_pot:balance(<<"bob">>, S3)),
    S4 = dev_pot:set_weight(<<"hydrogen">>, 1, S3, Opts),
    ?hr(),
    report(S4),
    {ok, S5} = hb_ao:resolve(S4, <<"drip">>, Opts),
    report(S5),
    ok.

delegate_test() ->
    AddrAlice = <<"alice">>,
    AddrBob = <<"bob">>,
    ResourceHydrogen = <<"hydrogen">>,
    ResourceOxygen = <<"oxygen">>,
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
            ResourceHydrogen => #{
                <<"chi">> => 0,
                <<"weight">> => 1,
                <<"total-deposits">> => 200,
                <<"deposits">> => #{ 
                    AddrAlice => #{
                        <<"quantity">> => 200,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    AddrBob => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    }
                }
            },
            ResourceOxygen => #{
                <<"chi">> => 0,
                <<"weight">> => 1,
                <<"total-deposits">> => 50,
                <<"deposits">> => #{
                    AddrAlice => #{
                        <<"quantity">> => 25,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    AddrBob => #{
                        <<"quantity">> => 25,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:delegate(AddrAlice, AddrBob, ResourceHydrogen, 20, S0, Opts),
    ?assertEqual(
        hb_ao:get(
            <<
                "/resources/", 
                ResourceHydrogen/binary, 
                "/deposits/", 
                AddrAlice/binary, 
                "/delegations/", 
                AddrBob/binary
            >>,
            S1,
            0,
            Opts
        ),
        20
    ),
    ?assertEqual(
        hb_ao:get(
            <<
                "/resources/", 
                ResourceHydrogen/binary, 
                "/deposits/", 
                AddrAlice/binary,
                "/quantity"
            >>,
            S1,
            0,
            Opts
        ),
        180
    ),
    ?assertEqual(
        20,
        hb_ao:get(
            <<
                "/resources/", 
                ResourceHydrogen/binary, 
                "/deposits/", 
                AddrBob/binary,
                "/quantity"
            >>,
            S1,
            0,
            Opts
        )
    ),
    S2 = dev_pot:delegate(AddrAlice, AddrBob, ResourceHydrogen, -10, S1, Opts),
    ?assertEqual(
        10,
        hb_ao:get(
            <<
                "/resources/", 
                ResourceHydrogen/binary, 
                "/deposits/", 
                AddrAlice/binary, 
                "/delegations/", 
                AddrBob/binary
            >>,
            S2,
            0,
            Opts
        )
    ),
    ?assertEqual(
        190,
        hb_ao:get(
            <<
                "/resources/", 
                ResourceHydrogen/binary, 
                "/deposits/", 
                AddrAlice/binary,
                "/quantity"
            >>,
            S2,
            0,
            Opts
        )
    ),
    ?assertEqual(
        10,
        hb_ao:get(
            <<
                "/resources/", 
                ResourceHydrogen/binary, 
                "/deposits/", 
                AddrBob/binary,
                "/quantity"
            >>,
            S2,
            0,
            Opts
        )
    ),
    S3 = dev_pot:delegate(AddrBob, AddrAlice, ResourceOxygen, 21, S2, Opts),
    ?assertEqual(
        21,
        hb_ao:get(
            <<
                "/resources/", 
                ResourceOxygen/binary, 
                "/deposits/", 
                AddrBob/binary, 
                "/delegations/", 
                AddrAlice/binary
            >>,
            S3,
            0,
            Opts
        )
    ),
    ?assertEqual(46, dev_pot:deposit(AddrAlice, ResourceOxygen, S3)),
    ?assertEqual(4, dev_pot:deposit(AddrBob, ResourceOxygen, S3)).

liquidate_delegations_test() ->
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"tw">> => 0,
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"chi">> => 0,
                <<"weight">> => 1,
                <<"total-deposits">> => 1,
                <<"deposits">> => #{ 
                    <<"alice">> => #{
                        <<"quantity">> => 1,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 1, S0, #{}),
    S2 = dev_pot:delegate(<<"bob">>, <<"charlie">>, <<"oxygen">>, 1, S1, #{}),
    report(S2),
    S3 = dev_pot:delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, -1, S2, #{}),
    ?assertEqual(1, dev_pot:deposit(<<"alice">>, <<"oxygen">>, S3)),
    ?assertEqual(0, dev_pot:deposit(<<"bob">>, <<"oxygen">>, S3)),
    ?assertEqual(0, dev_pot:deposit(<<"charlie">>, <<"oxygen">>, S3)).

multiple_delegations_liquidation_test() ->
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"tw">> => 0,
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"chi">> => 0,
                <<"weight">> => 1,
                <<"total-deposits">> => 2,
                <<"deposits">> => #{ 
                    <<"alice">> => #{
                        <<"quantity">> => 2,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    <<"denis">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 2, S0, #{}),
    S2 = dev_pot:delegate(<<"bob">>, <<"charlie">>, <<"oxygen">>, 1, S1, #{}),
    S3 = dev_pot:delegate(<<"bob">>, <<"denis">>, <<"oxygen">>, 1, S2, #{}),
    S4 = dev_pot:delegate(<<"denis">>, <<"alice">>, <<"oxygen">>, 1, S3, #{}),
    report(S4),
    ?assertEqual(1, dev_pot:deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(1, dev_pot:deposit(<<"charlie">>, <<"oxygen">>, S4)),
    S5 = dev_pot:delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, -2, S4, #{}),
    ?assertEqual(2, dev_pot:deposit(<<"alice">>, <<"oxygen">>, S5)),
    ?assertEqual(0, dev_pot:deposit(<<"bob">>, <<"oxygen">>, S5)),
    ?assertEqual(0, dev_pot:deposit(<<"charlie">>, <<"oxygen">>, S5)),
    ?assertEqual(0, dev_pot:deposit(<<"denis">>, <<"oxygen">>, S5)).

cyclic_delegation_test() ->
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"tw">> => 0,
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"chi">> => 0,
                <<"weight">> => 1,
                <<"total-deposits">> => 1,
                <<"deposits">> => #{ 
                    <<"alice">> => #{
                        <<"quantity">> => 1,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 1, S0, #{}),
    S2 = dev_pot:delegate(<<"bob">>, <<"alice">>, <<"oxygen">>, 1, S1, #{}),
    S3 = dev_pot:delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 1, S2, #{}),
    S4 = dev_pot:delegate(<<"bob">>, <<"alice">>, <<"oxygen">>, 1, S3, #{}),
    ?assertEqual(1, dev_pot:deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(0, dev_pot:deposit(<<"bob">>, <<"oxygen">>, S4)),
    report(S4),
    S5 = dev_pot:delegate(<<"bob">>, <<"alice">>, <<"oxygen">>, -2, S4, #{}),
    report(S5),
    ?assertEqual(0, dev_pot:deposit(<<"alice">>, <<"oxygen">>, S5)),
    ?assertEqual(1, dev_pot:deposit(<<"bob">>, <<"oxygen">>, S5)).

remove_deposit_while_delegated_test() ->
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"chi">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => 0.5,
        <<"tw">> => 0,
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"chi">> => 0,
                <<"weight">> => 1,
                <<"total-deposits">> => 3,
                <<"deposits">> => #{ 
                    <<"alice">> => #{
                        <<"quantity">> => 3,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0,
                        <<"accumulated-weight-at-deposit">> => 0,
                        <<"time-at-deposit">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 3, S0, #{}),
    S2 = dev_pot:delegate(<<"bob">>, <<"charlie">>, <<"oxygen">>, 2, S1, #{}),
    S3 = dev_pot:delegate(<<"charlie">>, <<"alice">>, <<"oxygen">>, 1, S2, #{}),
    report(S1),
    ?assertEqual(1, dev_pot:deposit(<<"alice">>, <<"oxygen">>, S3)),
    ?assertEqual(1, dev_pot:deposit(<<"bob">>, <<"oxygen">>, S3)),
    ?assertEqual(1, dev_pot:deposit(<<"charlie">>, <<"oxygen">>, S3)),
    S4 = dev_pot:modify_deposit(<<"alice">>, <<"oxygen">>, -3, S3, #{}),
    report(S4),
    ?assertEqual(0, dev_pot:deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(0, dev_pot:deposit(<<"bob">>, <<"oxygen">>, S4)),
    ?assertEqual(0, dev_pot:deposit(<<"charlie">>, <<"oxygen">>, S4)).

inverted_index_test() ->
    AddrAlice = <<"alice">>,
    AddrBob = <<"bob">>,
    ResourceHydrogen = <<"hydrogen">>,
    ResourceOxygen = <<"oxygen">>,
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
            <<"hydrogen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            },
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:modify_deposit(AddrAlice, ResourceHydrogen, 5, S0, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 5 } },
            dev_pot:user(AddrAlice, S1, Opts),
            primary
        )
    ),
    S2 = dev_pot:modify_deposit(AddrAlice, ResourceOxygen, 2, S1, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 5, ResourceOxygen => 2 } },
            dev_pot:user(AddrAlice, S2, Opts),
            primary
        )
    ),
    S3 = dev_pot:modify_deposit(AddrBob, ResourceHydrogen, 777, S2, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 777 } },
            dev_pot:user(AddrBob, S3, Opts),
            primary
        )
    ),
    S4 = dev_pot:modify_deposit(AddrAlice, ResourceHydrogen, -4, S3, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 1, ResourceOxygen => 2 } },
            dev_pot:user(AddrAlice, S4, Opts),
            primary
        )
    ),
    S5 = dev_pot:modify_deposit(AddrAlice, ResourceHydrogen, -1, S4, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ <<"oxygen">> => 2 } },
            dev_pot:user(AddrAlice, S5, Opts),
            primary
        )
    ),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 777 } },
            dev_pot:user(AddrBob, S5, Opts),
            primary
        )
    ),
    S6 = dev_pot:modify_deposit(AddrBob, ResourceHydrogen, -777, S5, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{} },
            dev_pot:user(AddrBob, S6, Opts),
            primary
        )
    ).

report(S) ->
    ?event(
        {report,
            {t, hb_maps:get(<<"t">>, S)},
            {last_drip, hb_maps:get(<<"last-drip">>, S)},
            {tw, hb_maps:get(<<"tw">>, S)},
            {balances, dev_pot:balances(S)},
            {deposits, dev_pot:deposits(S)},
            {minted, hb_maps:get(<<"minted">>, S)},
            {state, S}
        }
    ).