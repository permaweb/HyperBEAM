%%% @doc Test vectors for the `~pot@1.0` device.
-module(dev_pot_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% HyperBEAM AO mint checkpoints for the current millisecond-based pot curve.
-define(AO_TOKEN_DENOMINATION, 1_000_000_000_000).
-define(AO_TOTAL_SUPPLY, 21_000_000 * ?AO_TOKEN_DENOMINATION).
-define(AO_MINT_PROP_DENOMINATOR, 1_000_000_000_000_000).
-define(AO_ONE_DAY_MS, 86_400_000).
-define(AO_MS_STEP_NUMERATOR, 5_492).
-define(AO_CURRENT_MS_FIRST_DAY_MINTED, 9_962_321_008_608_663).
%% Daily flooring means the lazy curve becomes unable to mint the final
%% 2,107 raw units when evaluated once per day.
-define(AO_CURRENT_MS_EFFECTIVE_CAP_DAYS, 93_762).
-define(AO_CURRENT_MS_DAY_BY_DAY_FINAL_REMAINDER, 2_107).
-define(POT_QUANTITY_SCALE, 1_000_000_000_000_000_000). % 1e18
-define(POT_REWARD_SCALE, 1_000_000_000_000_000_000). % 1e18
-define(POT_PRICE_SCALE, 1_000_000). % 1e6
-define(POT_ACCUMULATOR_SCALE, (?POT_REWARD_SCALE * ?POT_PRICE_SCALE)).

%%% Test Helper Functions

%% @doc Create a pot state with one user
%% Example: pot_state(Alice, ResourceOxygen, 10)
pot_state(User, Resource, Quantity) ->
    pot_state(User, Resource, Quantity, 1, 100, 1, 2).

pot_state(User, Resource, Quantity, Weight, MintCap, MintPropN, MintPropD) ->
    Deposits = #{
        User => #{
            <<"quantity">> => Quantity,
            <<"last-resource-accumulator">> => 0
        }
    },
    Resources = #{
        Resource => #{
            <<"weight">> => Weight,
            <<"total-deposits">> => Quantity,
            <<"deposits">> => Deposits
        }
    },
    TWU = Weight * Quantity,
    #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => MintCap,
        <<"mint-prop-numerator">> => MintPropN,
        <<"mint-prop-denominator">> => MintPropD,
        <<"resources">> => Resources,
        <<"balances">> => #{},
        <<"total-weighted-units">> => TWU
    }.

%% @doc Create a pot state with multiple users on same resource
%% Example: pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 5}])
pot_state_multi(Resource, UserQuantities) ->
    pot_state_multi(Resource, UserQuantities, 1, 100, 1, 2).

pot_state_multi(Resource, UserQuantities, Weight, MintCap, MintPropN, MintPropD) ->
    Deposits = maps:from_list([
        {User, #{
            <<"quantity">> => Qty,
            <<"last-resource-accumulator">> => 0
        }}
        || {User, Qty} <- UserQuantities
    ]),
    TotalDeposits = lists:sum([Qty || {_, Qty} <- UserQuantities]),
    Resources = #{
        Resource => #{
            <<"weight">> => Weight,
            <<"total-deposits">> => TotalDeposits,
            <<"deposits">> => Deposits
        }
    },
    TWU = Weight * TotalDeposits,
    #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => MintCap,
        <<"mint-prop-numerator">> => MintPropN,
        <<"mint-prop-denominator">> => MintPropD,
        <<"resources">> => Resources,
        <<"balances">> => #{},
        <<"total-weighted-units">> => TWU
    }.

%% @doc Create an empty pot state (no users, optionally with empty resources)
%% Example: pot_state_empty() or pot_state_empty([ResourceOxygen])
pot_state_empty(EmptyResources) ->
    pot_state_empty(EmptyResources, 100, 1, 2).

pot_state_empty(EmptyResources, MintCap, MintPropN, MintPropD) ->
    Resources = maps:from_list([
        {R, #{
            <<"weight">> => 1,
            <<"total-deposits">> => 0,
            <<"deposits">> => #{}
        }}
        || R <- EmptyResources
    ]),
    #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => MintCap,
        <<"mint-prop-numerator">> => MintPropN,
        <<"mint-prop-denominator">> => MintPropD,
        <<"resources">> => Resources,
        <<"balances">> => #{}
    }.

delegate(FromAddr, ToAddr, ResourceID, Amount, State, Opts) ->
    case dev_pot:delegate(FromAddr, ToAddr, ResourceID, Amount, State, Opts) of
        {ok, NewState} -> NewState;
        Other -> Other
    end.

undelegate(FromAddr, ToAddr, ResourceID, Amount, State, Opts) ->
    case dev_pot:undelegate(FromAddr, ToAddr, ResourceID, Amount, State, Opts) of
        {ok, NewState} -> NewState;
        Other -> Other
    end.

mint_quantity_test() ->
    ?assertEqual(50, dev_pot_math:minted_between(0, 100, 1, 2, 0, 1)),
    ?assertEqual(75, dev_pot_math:minted_between(0, 100, 1, 2, 0, 2)),
    ?assertEqual(87, dev_pot_math:minted_between(0, 100, 1, 2, 0, 3)),
    Period1 = dev_pot_math:minted_between(0, 100, 1, 2, 0, 2),
    Period2 = dev_pot_math:minted_between(Period1, 100, 1, 2, 2, 3),
    ?assertEqual(87, Period1 + Period2).

mint3_current_ms_day_test() ->
    Opts = #{},
    S0 =
        (pot_state_empty(
            [],
            ?AO_TOTAL_SUPPLY,
            ?AO_MS_STEP_NUMERATOR,
            ?AO_MINT_PROP_DENOMINATOR
        ))#{
            <<"t-source">> => <<"timestamp">>
        },
    {ok, S1} = dev_pot:mint(S0, #{<<"timestamp">> => ?AO_ONE_DAY_MS}, Opts),
    ?assertEqual(
        ?AO_CURRENT_MS_FIRST_DAY_MINTED,
        hb_maps:get(<<"minted">>, S1, 0, Opts)
    ),
    ?assertEqual(?AO_ONE_DAY_MS, hb_maps:get(<<"last-drip">>, S1, 0, Opts)).

mint3_current_ms_256_year_inactive_gap_test() ->
    Opts = #{},
    InactiveT = 256 * 365 * ?AO_ONE_DAY_MS,
    S0 =
        (pot_state_empty(
            [],
            ?AO_TOTAL_SUPPLY,
            ?AO_MS_STEP_NUMERATOR,
            ?AO_MINT_PROP_DENOMINATOR
        ))#{
            <<"t-source">> => <<"timestamp">>
        },
    {ok, S1} = dev_pot:mint(S0, #{<<"timestamp">> => InactiveT}, Opts),
    ?assertEqual(2, ?AO_TOTAL_SUPPLY - hb_maps:get(<<"minted">>, S1, 0, Opts)),
    ?assertEqual(InactiveT, hb_maps:get(<<"last-drip">>, S1, 0, Opts)).

mint3_current_ms_day_by_day_effective_cap_test() ->
    FinalMinted = simulate_mint3_days(?AO_CURRENT_MS_EFFECTIVE_CAP_DAYS),
    ?assertEqual(
        ?AO_CURRENT_MS_DAY_BY_DAY_FINAL_REMAINDER,
        ?AO_TOTAL_SUPPLY - FinalMinted
    ),
    NextT = (?AO_CURRENT_MS_EFFECTIVE_CAP_DAYS + 1) * ?AO_ONE_DAY_MS,
    LastT = ?AO_CURRENT_MS_EFFECTIVE_CAP_DAYS * ?AO_ONE_DAY_MS,
    ?assertEqual(
        0,
        dev_pot_math:minted_between(
            FinalMinted,
            ?AO_TOTAL_SUPPLY,
            ?AO_MS_STEP_NUMERATOR,
            ?AO_MINT_PROP_DENOMINATOR,
            LastT,
            NextT
        )
    ).

simulate_mint3_days(Days) ->
    simulate_mint3_days(0, 1, Days).

simulate_mint3_days(Minted, Day, Days) when Day > Days ->
    Minted;
simulate_mint3_days(Minted, Day, Days) ->
    LastT = (Day - 1) * ?AO_ONE_DAY_MS,
    T = Day * ?AO_ONE_DAY_MS,
    ToMint =
        dev_pot_math:minted_between(
            Minted,
            ?AO_TOTAL_SUPPLY,
            ?AO_MS_STEP_NUMERATOR,
            ?AO_MINT_PROP_DENOMINATOR,
            LastT,
            T
        ),
    simulate_mint3_days(Minted + ToMint, Day + 1, Days).

scaled_accumulator(Numerator, Denominator) ->
    (Numerator * ?POT_ACCUMULATOR_SCALE) div Denominator.

%% @doc Demonstrate minting using the proportional model and a single resource.
single_resource_test() ->
    Addr1 = <<"addr1">>,
    Addr2 = <<"addr2">>,
    ResourceID = <<"resource1">>,
    Opts = #{},
    S0 = pot_state_empty([ResourceID]),
    S1 = dev_pot:deposit(Addr1, ResourceID, 10, S0, Opts),
    S2 = dev_pot:deposit(Addr2, ResourceID, 10, S1, Opts),
    report(S2, Opts),
    S3 = dev_pot:test_drip(S2, #{ <<"t">> => 1 }, Opts),
    report(S3, Opts),
    % At t=1, there are 20 pot units and 50 minted to distribute. The scaled
    % accumulator preserves 50 / 20 = 2.5, so each address receives 25.
    ?assertEqual(25, dev_pot:balance(Addr1, S3, Opts)),
    ?assertEqual(25, dev_pot:balance(Addr2, S3, Opts)),
    S4 = dev_pot:test_drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4, Opts),
    % At t=2, cumulative distribution per address is floor((50 + 25) / 2) = 37.
    ?assertEqual(37, dev_pot:balance(Addr1, S4, Opts)),
    ?assertEqual(37, dev_pot:balance(Addr2, S4, Opts)),
    % Set Addr1 to have 75% of the total deposits.
    S5 = dev_pot:deposit(Addr1, ResourceID, 20, S4, Opts),
    report(S5, Opts),
    % Addr1 materializes 37 before increasing its deposit. At t=3 it receives
    % floor(12 * 30 / 40) = 9 more.
    NewExpectedB1 = 46,
    S6 = dev_pot:test_drip(S5, #{ <<"t">> => 3 }, Opts),
    report(S6, Opts),
    ?assertEqual(NewExpectedB1, dev_pot:balance(Addr1, S6, Opts)),
    % Set both to be equal again.
    S7 = dev_pot:withdraw(Addr1, ResourceID, 20, S6, Opts),
    report(S7, Opts),
    Addr1BalPreFinal = dev_pot:balance(Addr1, S7, Opts),
    Addr2BalPreFinal = dev_pot:balance(Addr2, S7, Opts),
    S8 = dev_pot:test_drip(S7, #{ <<"t">> => 4 }, Opts),
    % Ensure that they were again minted equal quantities.
    Addr1Diff = dev_pot:balance(Addr1, S8, Opts) - Addr1BalPreFinal,
    Addr2Diff = dev_pot:balance(Addr2, S8, Opts) - Addr2BalPreFinal,
    ?assertEqual(Addr1Diff, Addr2Diff).

%% @doc Demonstrate minting using the proportional model and multiple resources.
multiple_resources_test() ->
    Addr1 = <<"addr1">>,
    Addr2 = <<"addr2">>,
    Resource1 = <<"resource1">>,
    Resource2 = <<"resource2">>,
    Opts =#{},
    S0 =
        pot_state_empty(
            [Resource1, Resource2],
            1000,
            1,
            2
        ),
    % Set resource2 weight to 9
    S0Updated = 
        hb_ao:set(
            S0, 
            <<"/resources/",Resource2/binary,"/weight">>, 
            9, 
            Opts
        ),
    S1 = dev_pot:deposit(Addr1, Resource1, 1, S0Updated, Opts),
    S1b = dev_pot:deposit(Addr2, Resource1, 1, S1, Opts),
    S2 = dev_pot:deposit(Addr1, Resource2, 1, S1b, Opts),
    S2b = dev_pot:deposit(Addr2, Resource2, 1, S2, Opts),
    S3 = dev_pot:test_drip(S2b, #{ <<"t">> => 1 }, Opts),
    report(S3, Opts),
    % 20 pot units, 500 minted. 500 div 20 = 25. Each user: 10 units * 25 = 250
    ?assertEqual(250, dev_pot:balance(Addr1, S3, Opts)),
    ?assertEqual(250, dev_pot:balance(Addr2, S3, Opts)),
    S4 = dev_pot:test_drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4, Opts),
    % 20 pot units, 250 minted. The scaled accumulator preserves 250 / 20 = 12.5.
    % Each user receives floor(37.5) from resource1 and floor(337.5) from resource2.
    ?assertEqual(374, dev_pot:balance(Addr1, S4, Opts)),
    ?assertEqual(374, dev_pot:balance(Addr2, S4, Opts)),
    % Withdraw to make Addr1 have 1/10 of pot units
    S5a = dev_pot:withdraw(Addr1, Resource2, 1, S4, Opts),
    S5b = dev_pot:withdraw(Addr2, Resource1, 1, S5a, Opts),
    NewExpectedB1 = 387,
    S6 = dev_pot:test_drip(S5b, #{ <<"t">> => 3 }, Opts),
    report(S6, Opts),
    ?assertEqual(NewExpectedB1, dev_pot:balance(Addr1, S6, Opts)),
    % Make both equal again
    S7a = dev_pot:deposit(Addr1, Resource2, 1, S6, Opts),
    S7b = dev_pot:deposit(Addr2, Resource1, 1, S7a, Opts),
    report(S7b, Opts),
    Addr1BalPreFinal = dev_pot:balance(Addr1, S7b, Opts),
    Addr2BalPreFinal = dev_pot:balance(Addr2, S7b, Opts),
    S8 = dev_pot:test_drip(S7b, #{ <<"t">> => 4 }, Opts),
    % Both should mint equal quantities
    Addr1Diff = dev_pot:balance(Addr1, S8, Opts) - Addr1BalPreFinal,
    Addr2Diff = dev_pot:balance(Addr2, S8, Opts) - Addr2BalPreFinal,
    ?assertEqual(Addr1Diff, Addr2Diff).

single_resource_modified_weight_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    S0 = pot_state_empty([ResourceOxygen]),
    S1 = dev_pot:deposit(Alice, <<"oxygen">>, 1, S0, Opts),
    % There's 1 pot unit and 50 minted, alice accumulates 50.
    S3 = dev_pot:test_drip(S1, #{}, Opts),
    S4 = dev_pot:register_resource(<<"oxygen">>, 10, S3, Opts),
    report(S4, Opts),
    % There's 10 pot units and 25 minted, and the scaled accumulator preserves
    % 25 / 10 = 2.5 across Alice's weighted units.
    S5 = dev_pot:test_drip(S4, #{}, Opts),
    report(S5, Opts),
    ?assertEqual(75, dev_pot:balance(Alice, S5, Opts)),
    ok.

multiresource_modified_weight_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    Opts =#{},
    S0 = pot_state_empty([ResourceOxygen, ResourceHydrogen]),
    % Set hydrogen weight to 0
    S0Updated = 
        hb_ao:set(
            S0, 
            <<"/resources/",ResourceHydrogen/binary,"/weight">>, 
            0, 
            Opts
        ),
    S1 = dev_pot:deposit(Alice, ResourceOxygen, 1, S0Updated, Opts),
    S2 = dev_pot:deposit(Bob, ResourceHydrogen, 1, S1, Opts),
    S3 = dev_pot:test_drip(S2, #{}, Opts),
    ?assertEqual(50, dev_pot:balance(Alice, S3, Opts)),
    ?assertEqual(0, dev_pot:balance(Bob, S3, Opts)),
    S4 = dev_pot:register_resource(ResourceHydrogen, 1, S3, Opts),
    % 25 minted, 2 pot units. 25 div 2 = 12
    S5 = dev_pot:test_drip(S4, #{}, Opts),
    ?assertEqual(62, dev_pot:balance(Alice, S5, Opts)),
    ?assertEqual(12, dev_pot:balance(Bob, S5, Opts)),
    ok.

weight_authority_match_requires_multiple_signers_test() ->
    Admin = <<"admin">>,
    WeightA = <<"weight-a">>,
    WeightB = <<"weight-b">>,
    Resource = <<"oxygen">>,
    Opts = #{},
    S0 = (pot_state_empty([Resource]))#{ <<"mint-authority">> => Admin },
    S1 =
        dev_pot:register(
            S0,
            #{
                <<"body">> => #{
                    <<"resource">> => Resource,
                    <<"weight">> => 100,
                    <<"from">> => Admin,
                    <<"weight-authority">> => [WeightA, WeightB],
                    <<"weight-authority-match">> => 2
                }
            },
            Opts
        ),
    ?assert(is_map(S1)),
    ?assertEqual(100, hb_ao:get(<<"/resources/oxygen/weight">>, S1, 0, Opts)),
    ?assertMatch(
        {error, _},
        dev_pot:register(
            S1,
            #{
                <<"body">> => #{
                    <<"resource">> => Resource,
                    <<"weight">> => 200,
                    <<"from">> => WeightA
                }
            },
            Opts
        )
    ),
    ?assertEqual(100, hb_ao:get(<<"/resources/oxygen/weight">>, S1, 0, Opts)),
    S2 =
        dev_pot:register(
            S1,
            #{
                <<"body">> => #{
                    <<"resource">> => Resource,
                    <<"weight">> => 200,
                    <<"from">> => [WeightA, WeightB]
                }
            },
            Opts
        ),
    ?assert(is_map(S2)),
    ?assertEqual(200, hb_ao:get(<<"/resources/oxygen/weight">>, S2, 0, Opts)).

weight_authority_can_update_quantity_scale_test() ->
    Admin = <<"admin">>,
    WeightAuthority = <<"weight-authority">>,
    Alice = <<"alice">>,
    Resource = <<"oxygen">>,
    Opts = #{},
    Weight = 100,
    Quantity = 10,
    AssetScale = 100_000_000, % 1e8
    S0 =
        (pot_state(Alice, Resource, Quantity, Weight, 100, 1, 2))#{
            <<"mint-authority">> => Admin
        },
    S1 =
        dev_pot:register(
            S0,
            #{
                <<"body">> => #{
                    <<"resource">> => Resource,
                    <<"weight">> => Weight,
                    <<"from">> => Admin,
                    <<"weight-authority">> => WeightAuthority
                }
            },
            Opts
        ),
    InitialTWU = hb_maps:get(<<"total-weighted-units">>, S1, 0, Opts),
    S2 =
        dev_pot:register(
            S1,
            #{
                <<"body">> => #{
                    <<"resource">> => Resource,
                    <<"quantity-scale">> => AssetScale,
                    <<"from">> => WeightAuthority
                }
            },
            Opts
        ),
    ?assert(is_map(S2)),
    ?assertEqual(
        AssetScale,
        hb_ao:get(<<"/resources/oxygen/quantity-scale">>, S2, 0, Opts)
    ),
    ?assertEqual(Weight, hb_ao:get(<<"/resources/oxygen/weight">>, S2, 0, Opts)),
    ?assertEqual(InitialTWU, hb_maps:get(<<"total-weighted-units">>, S2, 0, Opts)).

resource_authority_required_signer_is_enforced_test() ->
    Admin = <<"admin">>,
    Alice = <<"alice">>,
    ResourceA = <<"resource-a">>,
    ResourceB = <<"resource-b">>,
    Resource = <<"oxygen">>,
    Opts = #{},
    S0 = (pot_state_empty([Resource]))#{ <<"mint-authority">> => Admin },
    S1 =
        dev_pot:register(
            S0,
            #{
                <<"body">> => #{
                    <<"resource">> => Resource,
                    <<"weight">> => 100,
                    <<"from">> => Admin,
                    <<"resource-authority">> => [ResourceA, ResourceB],
                    <<"resource-authority-required">> => [ResourceA],
                    <<"resource-authority-match">> => 1
                }
            },
            Opts
        ),
    ?assert(is_map(S1)),
    ?assertMatch(
        {error, _},
        dev_pot:deposit(
            S1,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => Resource,
                    <<"quantity">> => 10,
                    <<"from">> => ResourceB
                }
            },
            Opts
        )
    ),
    ?assertEqual(0, dev_pot:get_deposit(Alice, Resource, S1, Opts)),
    S2 =
        dev_pot:deposit(
            S1,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => Resource,
                    <<"quantity">> => 10,
                    <<"from">> => [ResourceA, ResourceB]
                }
            },
            Opts
        ),
    ?assert(is_map(S2)),
    ?assertEqual(10, dev_pot:get_deposit(Alice, Resource, S2, Opts)).

simple_delegation_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceHydrogen = <<"hydrogen">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Setup: Alice has 200 hydrogen and 25 oxygen, Bob has 0 hydrogen and 25 oxygen
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop-numerator">> => 1,
        <<"mint-prop-denominator">> => 2,
        <<"resources">> => #{
            ResourceHydrogen => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 200,
                <<"deposits">> => #{
                    Alice => #{
                        <<"quantity">> => 200, 
                        <<"last-resource-accumulator">> => 0
                    },
                    Bob => #{
                        <<"quantity">> => 0, 
                        <<"last-resource-accumulator">> => 0
                    }
                }
            },
            ResourceOxygen => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 50,
                <<"deposits">> => #{
                    Alice => #{
                        <<"quantity">> => 25, 
                        <<"last-resource-accumulator">> => 0
                    },
                    Bob => #{
                        <<"quantity">> => 25, 
                        <<"last-resource-accumulator">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{},
        <<"total-weighted-units">> => 250
    },
    % Alice delegates 20 hydrogen to Bob
    S1 = delegate(Alice, Bob, ResourceHydrogen, 20, S0, Opts),
    ?assertEqual(
        20,
        hb_ao:get(
            <<"/resources/",
            ResourceHydrogen/binary,
            "/deposits/",
            Alice/binary,
            "/delegations/",
            Bob/binary>>,
            S1,
            0,
            Opts
        )
    ),
    ?assertEqual(
        180,
        hb_ao:get(
            <<"/resources/",
            ResourceHydrogen/binary,
            "/deposits/",
            Alice/binary,
            "/quantity">>,
            S1,
            0,
            Opts
        )
    ),
    ?assertEqual(
        20,
        hb_ao:get(
            <<"/resources/",
            ResourceHydrogen/binary,
            "/deposits/",
            Bob/binary,
            "/quantity">>,
            S1,
            0,
            Opts
        )
    ),
    % Alice undelegates 10 hydrogen from Bob
    S2 = undelegate(Alice, Bob, ResourceHydrogen, 10, S1, Opts),
    ?assertEqual(
        10,
        hb_ao:get(
            <<"/resources/",
            ResourceHydrogen/binary,
            "/deposits/",
            Alice/binary,
            "/delegations/",
            Bob/binary>>,
            S2,
            0,
            Opts
        )
    ),
    ?assertEqual(
        190,
        hb_ao:get(
            <<"/resources/",
            ResourceHydrogen/binary,
            "/deposits/",
            Alice/binary,
            "/quantity">>,
            S2,
            0,
            Opts
        )
    ),
    ?assertEqual(
        10,
        hb_ao:get(
            <<"/resources/",
            ResourceHydrogen/binary,
            "/deposits/",
            Bob/binary,
            "/quantity">>,
            S2,
            0,
            Opts
        )
    ),
    % Bob delegates 21 oxygen to Alice
    S3 = delegate(Bob, Alice, ResourceOxygen, 21, S2, Opts),
    ?assertEqual(
        21,
        hb_ao:get(
            <<"/resources/",
            ResourceOxygen/binary,
            "/deposits/",
            Bob/binary,
            "/delegations/",
            Alice/binary>>,
            S3,
            0,
            Opts
        )
    ),
    ?assertEqual(46, dev_pot:get_deposit(Alice, ResourceOxygen, S3, Opts)),
    ?assertEqual(4, dev_pot:get_deposit(Bob, ResourceOxygen, S3, Opts)).

delegation_liquidation_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    Charlie = <<"charlie">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    S0 = 
        pot_state_multi(
            ResourceOxygen, 
            [
                {Alice, 1}, 
                {Bob, 0}, 
                {Charlie, 0}
            ]
        ),
    S1 = delegate(Alice, Bob, ResourceOxygen, 1, S0, Opts),
    S2 = delegate(Bob, Charlie, ResourceOxygen, 1, S1, Opts),
    report(S2, Opts),
    S3 = undelegate(Alice, Bob, ResourceOxygen, 1, S2, Opts),
    ?assertEqual(1, dev_pot:get_deposit(Alice, ResourceOxygen, S3, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Bob, ResourceOxygen, S3, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Charlie, ResourceOxygen, S3, Opts)).

multiple_delegations_liquidation_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    Charlie = <<"charlie">>,
    Denis = <<"denis">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    S0 = 
        pot_state_multi(
            ResourceOxygen, 
            [
                {Alice, 2}, 
                {Bob, 0}, 
                {Charlie, 0}, 
                {Denis, 0}
            ]
        ),
    S1 = delegate(Alice, Bob, ResourceOxygen, 2, S0, Opts),
    S2 = delegate(Bob, Charlie, ResourceOxygen, 1, S1, Opts),
    S3 = delegate(Bob, Denis, ResourceOxygen, 1, S2, Opts),
    S4 = delegate(Denis, Alice, ResourceOxygen, 1, S3, Opts),
    report(S4, Opts),
    ?assertEqual(1, dev_pot:get_deposit(Alice, ResourceOxygen, S4, Opts)),
    ?assertEqual(1, dev_pot:get_deposit(Charlie, ResourceOxygen, S4, Opts)),
    S5 = undelegate(Alice, Bob, ResourceOxygen, 2, S4, Opts),
    ?assertEqual(2, dev_pot:get_deposit(Alice, ResourceOxygen, S5, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Bob, ResourceOxygen, S5, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Charlie, ResourceOxygen, S5, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Denis, ResourceOxygen, S5, Opts)).

cyclic_delegations_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    S0 = 
        pot_state_multi(
            ResourceOxygen, 
            [
                {Alice, 1}, 
                {Bob, 0}
            ]
        ),
    S1 = delegate(Alice, Bob, ResourceOxygen, 1, S0, Opts),
    S2 = delegate(Bob, Alice, ResourceOxygen, 1, S1, Opts),
    S3 = delegate(Alice, Bob, ResourceOxygen, 1, S2, Opts),
    S4 = delegate(Bob, Alice, ResourceOxygen, 1, S3, Opts),
    ?assertEqual(1, dev_pot:get_deposit(Alice, ResourceOxygen, S4, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Bob, ResourceOxygen, S4, Opts)),
    report(S4, Opts),
    S5 = undelegate(Bob, Alice, ResourceOxygen, 1, S4, Opts),
    report(S5, Opts),
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S5, Opts)),
    ?assertEqual(1, dev_pot:get_deposit(Bob, ResourceOxygen, S5, Opts)).

deposit_removal_while_delegated_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    Charlie = <<"charlie">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    S0 = 
        pot_state_multi(
            ResourceOxygen, 
            [
                {Alice, 3}, 
                {Bob, 0}, 
                {Charlie, 0}
            ]
        
        ),
    S1 = delegate(Alice, Bob, ResourceOxygen, 3, S0, Opts),
    S2 = delegate(Bob, Charlie, ResourceOxygen, 2, S1, Opts),
    S3 = delegate(Charlie, Alice, ResourceOxygen, 1, S2, Opts),
    report(S1, Opts),
    ?assertEqual(1, dev_pot:get_deposit(Alice, ResourceOxygen, S3, Opts)),
    ?assertEqual(1, dev_pot:get_deposit(Bob, ResourceOxygen, S3, Opts)),
    ?assertEqual(1, dev_pot:get_deposit(Charlie, ResourceOxygen, S3, Opts)),
    S4 = dev_pot:withdraw(Alice, ResourceOxygen, 3, S3, Opts),
    report(S4, Opts),
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S4, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Bob, ResourceOxygen, S4, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Charlie, ResourceOxygen, S4, Opts)).

inverted_index_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    Opts =#{},
    S0 = pot_state_empty([ResourceHydrogen, ResourceOxygen]),
    S1 = dev_pot:deposit(Alice, ResourceHydrogen, 5, S0, Opts),
    ?assert(
        hb_message:match(
            #{
                <<"deposits">> => #{
                    ResourceHydrogen => 5
                }
            }, 
            dev_pot:user(Alice, S1, Opts), 
            primary
        )
    ),
    S2 = dev_pot:deposit(Alice, ResourceOxygen, 2, S1, Opts),
    ?assert(
        hb_message:match(
            #{
                <<"deposits">> => #{
                    ResourceHydrogen => 5, 
                    ResourceOxygen => 2
                }
            }, 
            dev_pot:user(Alice, S2, Opts), 
            primary
        )
    ),
    S3 = dev_pot:deposit(Bob, ResourceHydrogen, 777, S2, Opts),
    ?assert(
        hb_message:match(
            #{
                <<"deposits">> => #{
                    ResourceHydrogen => 777
                }
            }, 
            dev_pot:user(Bob, S3, Opts), 
            primary
        )
    ),
    S4 = dev_pot:withdraw(Alice, ResourceHydrogen, 4, S3, Opts),
    ?assert(
        hb_message:match(
            #{
                <<"deposits">> => #{
                    ResourceHydrogen => 1, 
                    ResourceOxygen => 2
                }
            }, 
            dev_pot:user(Alice, S4, Opts), 
            primary
        )
    ),
    S5 = dev_pot:withdraw(Alice, ResourceHydrogen, 1, S4, Opts),
    ?assert(
        hb_message:match(
            #{
                <<"deposits">> => #{
                    ResourceOxygen => 2
                }
            }, 
            dev_pot:user(Alice, S5, Opts), 
            primary
        )
    ),
    ?assert(
        hb_message:match(
            #{
                <<"deposits">> => #{
                    ResourceHydrogen => 777
                }
            }, 
            dev_pot:user(Bob, S5, Opts), 
            primary
        )
    ),
    S6 = dev_pot:withdraw(Bob, ResourceHydrogen, 777, S5, Opts),
    ?assert(
        hb_message:match(
            #{
                <<"deposits">> => #{}
            }, 
            dev_pot:user(Bob, S6, Opts), 
            primary
        )
    ).
%%% Division by Zero Guard Tests

drip_with_zero_total_weighted_units_test() ->
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    % Test that dripping with TWU = 0 doesn't crash with division by zero
    S0 = pot_state_empty([ResourceOxygen]),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 1}, Opts),
    ?assertEqual(0, hb_maps:get(<<"total-weighted-units">>, S1, 0)).

drip_resource_with_zero_weight_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Test that a resource with weight = 0 doesn't accumulate yield
    S0 = pot_state(Alice, ResourceOxygen, 10, 0, 100, 1, 2),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 1}, Opts),
    ?assertEqual(0, dev_pot:balance(Alice, S1, Opts)).

drip_user_with_zero_quantity_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Test that a user with 0 deposits gets 0 yield
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 1}, Opts),
    % Claim yield by performing a deposit
    S1_claim = dev_pot:deposit(Alice, ResourceOxygen, 1, S1, Opts),
    ?assertEqual(0, dev_pot:balance(Bob, S1_claim, Opts)),
    ?assert(dev_pot:balance(Alice, S1_claim, Opts) > 0).

raw_18_decimal_price_weight_deposit_distributes_scaled_reward_test() ->
    Alice = <<"alice">>,
    Resource18Decimal = <<"18-decimal-asset">>,
    Opts = #{},
    Quantity = 10_000_000_000_000_000, % 0.01 at 1e18 scale
    Weight = 2300 * ?POT_PRICE_SCALE, % $2300
    S0 =
        pot_state_empty(
            [Resource18Decimal],
            ?AO_TOTAL_SUPPLY,
            ?AO_MS_STEP_NUMERATOR,
            ?AO_MINT_PROP_DENOMINATOR
        ),
    S1 = dev_pot:register_resource(Resource18Decimal, Weight, S0, Opts),
    S2 = dev_pot:deposit(Alice, Resource18Decimal, Quantity, S1, Opts),
    S3 = dev_pot:test_drip(S2, #{ <<"t">> => ?AO_ONE_DAY_MS }, Opts),
    Minted = hb_maps:get(<<"minted">>, S3, 0, Opts),
    TotalWeightedUnits = hb_maps:get(<<"total-weighted-units">>, S3, 0, Opts),
    ?assertEqual(0, Minted div TotalWeightedUnits),
    ?assert(hb_maps:get(<<"accumulator">>, S3, 0, Opts) > 0),
    ?assert(dev_pot:balance(Alice, S3, Opts) > 0).

%%% Minting Boundary Condition Tests

minting_with_zero_proportion_test() ->
    % mint-prop = 0 should mint 0 tokens
    ?assertEqual(0, dev_pot_math:minted_between(0, 100, 0, 1, 0, 1)),
    ?assertEqual(0, dev_pot_math:minted_between(0, 100, 0, 1, 0, 10)).

minting_with_full_proportion_test() ->
    % mint-prop = 1.0 should mint entire remaining cap in one step
    ?assertEqual(100, dev_pot_math:minted_between(0, 100, 1, 1, 0, 1)),
    ?assertEqual(50, dev_pot_math:minted_between(50, 100, 1, 1, 1, 2)).

minting_at_cap_test() ->
    % When already at cap, should mint 0
    ?assertEqual(0, dev_pot_math:minted_between(100, 100, 1, 2, 0, 1)),
    ?assertEqual(0, dev_pot_math:minted_between(100, 100, 1, 2, 0, 100)).

minting_with_negative_time_test() ->
    % Time going backwards should mint 0 (or be handled gracefully)
    ?assertEqual(0, dev_pot_math:minted_between(50, 100, 1, 2, 5, 3)),
    ?assertEqual(0, dev_pot_math:minted_between(0, 100, 1, 2, 10, 0)).

minting_with_zero_time_delta_test() ->
    % Same timestamp should mint 0
    ?assertEqual(0, dev_pot_math:minted_between(0, 100, 1, 2, 5, 5)),
    ?assertEqual(0, dev_pot_math:minted_between(50, 100, 1, 2, 10, 10)).

%%% Deposit/Withdrawal Edge Cases

deposit_zero_amount_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Depositing 0 should be a no-op
    S0 = pot_state(Alice, ResourceOxygen, 10),
    % deposit/5 has guard `when Amount > 0`, so calling with 0 should not match
    ?assertMatch(
        {error, _}, 
        dev_pot:deposit(Alice, ResourceOxygen, 0, S0, Opts)
    ).

withdraw_exact_balance_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Withdrawing exact deposit should result in 0
    S0 = pot_state(Alice, ResourceOxygen, 10),
    S1 = dev_pot:withdraw(Alice, ResourceOxygen, 10, S0, Opts),
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S1, Opts)).

%%% Delegation Edge Cases

delegate_zero_amount_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Delegating 0 should not match the function guard
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    ?assertMatch(
        {error, _}, 
        delegate(Alice, Bob, ResourceOxygen, 0, S0, Opts)
    ).

delegate_to_self_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Alice delegates to herself - should work but is a no-op in practice
    S0 = pot_state(Alice, ResourceOxygen, 10),
    S1 = delegate(Alice, Alice, ResourceOxygen, 5, S0, Opts),
    % After delegating to self, deposit should still be 10 (5 removed, 5 added back)
    ?assertEqual(10, dev_pot:get_deposit(Alice, ResourceOxygen, S1, Opts)),
    % Self delegation is a noop, so no delegation record should be written
    Delegation = hb_ao:get(
        <<"/resources/",
        ResourceOxygen/binary,
        "/deposits/",
        Alice/binary,
        "/delegations/",
        Alice/binary>>,
        S1,
        0,
        Opts
    ),
    ?assertEqual(0, Delegation).

%% @doc Regression test: public `delegate/3` must use assignment time, not
%% caller-controlled `body.t`.
public_delegate_uses_assignment_timestamp_not_body_t_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    Assignment =
        #{
            <<"timestamp">> => 1,
            <<"body">> =>
                #{
                    <<"from">> => Alice,
                    <<"address">> => Bob,
                    <<"resource">> => ResourceOxygen,
                    <<"quantity">> => 1,
                    <<"t">> => 100
                }
        },
    {ok, S1} = dev_pot:delegate(S0, Assignment, Opts),
    TotalMinted =
        hb_maps:get(<<"minted">>, S1, 0, Opts)
        + hb_maps:get(<<"undistributed-mint">>, S1, 0, Opts),
    ?assertEqual(1, hb_maps:get(<<"t">>, S1, undefined, Opts)),
    ?assertEqual(1, hb_maps:get(<<"last-drip">>, S1, undefined, Opts)),
    ?assertEqual(9, dev_pot:get_deposit(Alice, ResourceOxygen, S1, Opts)),
    ?assertEqual(1, dev_pot:get_deposit(Bob, ResourceOxygen, S1, Opts)),
    ?assertEqual(
        1,
        hb_ao:get(
            <<
                "/resources/",
                ResourceOxygen/binary,
                "/deposits/",
                Alice/binary,
                "/delegations/",
                Bob/binary
            >>,
            S1,
            0,
            Opts
        )
    ),
    ?assertEqual(50, TotalMinted).

delegate_entire_balance_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Delegate 100% of deposits
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    S1 = delegate(Alice, Bob, ResourceOxygen, 10, S0, Opts),
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S1, Opts)),
    ?assertEqual(10, dev_pot:get_deposit(Bob, ResourceOxygen, S1, Opts)).

delegate_exceeds_available_balance_rejected_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    ?assertMatch(
        {error, <<"Delegation amount exceeds available deposit.">>},
        delegate(Alice, Bob, ResourceOxygen, 15, S0, Opts)
    ).

%%% Delegation Chain Tests

deep_delegation_chain_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    Charlie = <<"charlie">>,
    Denis = <<"denis">>,
    Eve = <<"eve">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Create a 5-level delegation chain: A→B→C→D→E
    S0 = pot_state_multi(ResourceOxygen, [
        {Alice, 10},
        {Bob, 0},
        {Charlie, 0},
        {Denis, 0},
        {Eve, 0}
    ]),
    S1 = delegate(Alice, Bob, ResourceOxygen, 10, S0, Opts),
    S2 = delegate(Bob, Charlie, ResourceOxygen, 10, S1, Opts),
    S3 = delegate(Charlie, Denis, ResourceOxygen, 10, S2, Opts),
    S4 = delegate(Denis, Eve, ResourceOxygen, 10, S3, Opts),
    % Final state: Alice:0, Bob:0, Charlie:0, Denis:0, Eve:10
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S4, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Bob, ResourceOxygen, S4, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Charlie, ResourceOxygen, S4, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Denis, ResourceOxygen, S4, Opts)),
    ?assertEqual(10, dev_pot:get_deposit(Eve, ResourceOxygen, S4, Opts)).

wide_delegation_tree_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    Charlie = <<"charlie">>,
    Denis = <<"denis">>,
    Eve = <<"eve">>,
    ResourceOxygen = <<"oxygen">>,
    Frank = <<"frank">>,
    Opts =#{},
    % Alice delegates to 5 different addresses
    S0 = pot_state_multi(ResourceOxygen, [
        {Alice, 10}, {Bob, 0}, {Charlie, 0},
        {Denis, 0}, {Eve, 0}, {Frank, 0}
    ]),
    S1 = delegate(Alice, Bob, ResourceOxygen, 2, S0, Opts),
    S2 = delegate(Alice, Charlie, ResourceOxygen, 2, S1, Opts),
    S3 = delegate(Alice, Denis, ResourceOxygen, 2, S2, Opts),
    S4 = delegate(Alice, Eve, ResourceOxygen, 2, S3, Opts),
    S5 = delegate(Alice, Frank, ResourceOxygen, 2, S4, Opts),
    % Alice should have 0 left, each delegate has 2
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S5, Opts)),
    ?assertEqual(2, dev_pot:get_deposit(Bob, ResourceOxygen, S5, Opts)),
    ?assertEqual(2, dev_pot:get_deposit(Charlie, ResourceOxygen, S5, Opts)),
    ?assertEqual(2, dev_pot:get_deposit(Denis, ResourceOxygen, S5, Opts)),
    ?assertEqual(2, dev_pot:get_deposit(Eve, ResourceOxygen, S5, Opts)),
    ?assertEqual(2, dev_pot:get_deposit(Frank, ResourceOxygen, S5, Opts)).

%%% Conservation Law Tests

total_deposits_conservation_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    % Ensure sum of all deposits remains constant through operations
    S0 = pot_state_empty([ResourceOxygen]),
    S1 = dev_pot:deposit(Alice, ResourceOxygen, 10, S0, Opts),
    S2 = dev_pot:deposit(Bob, ResourceOxygen, 20, S1, Opts),
    TotalAfterDeposits = 30,
    ?assertEqual(TotalAfterDeposits,
        dev_pot:get_deposit(Alice, ResourceOxygen, S2, Opts) +
        dev_pot:get_deposit(Bob, ResourceOxygen, S2, Opts)),
    % Delegate
    S3 = delegate(Alice, Bob, ResourceOxygen, 5, S2, Opts),
    ?assertEqual(TotalAfterDeposits,
        dev_pot:get_deposit(Alice, ResourceOxygen, S3, Opts) +
        dev_pot:get_deposit(Bob, ResourceOxygen, S3, Opts)),
    % Withdraw
    S4 = dev_pot:withdraw(Bob, ResourceOxygen, 10, S3, Opts),
    ?assertEqual(TotalAfterDeposits - 10,
        dev_pot:get_deposit(Alice, ResourceOxygen, S4, Opts) +
        dev_pot:get_deposit(Bob, ResourceOxygen, S4, Opts)).

mint_cap_never_exceeded_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Even with many drips, minted should never exceed cap
    S0 = pot_state(Alice, ResourceOxygen, 10),
    SFinal = lists:foldl(
        fun(T, S) -> dev_pot:test_drip(S, #{<<"t">> => T}, Opts) end,
        S0,
        lists:seq(1, 100)
    ),
    Minted = hb_maps:get(<<"minted">>, SFinal, 0, Opts),
    UndistributedMint = hb_maps:get(<<"undistributed-mint">>, SFinal, 0, Opts),
    TotalMinted = Minted + UndistributedMint,
    % With integer division, some tokens accumulate in undistributed-mint
    % Check that total minted (distributed + undistributed) approaches cap
    ?assert(Minted =< 100),
    ?assert(TotalMinted >= 99).

%%% Liquidation Edge Cases

liquidate_partial_delegation_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Overdraw is less than largest delegation - should partially liquidate
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    S1 = delegate(Alice, Bob, ResourceOxygen, 10, S0, Opts),
    % Alice tries to withdraw 3, but has 0 deposits. Should liquidate 3 from Bob
    S2 = dev_pot:withdraw(Alice, ResourceOxygen, 3, S1, Opts),
    report(S2, Opts),
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S2, Opts)),
    ?assertEqual(7, dev_pot:get_deposit(Bob, ResourceOxygen, S2, Opts)),
    % Alice should still have 7 delegated to Bob
    ?assertEqual(
        7, 
        hb_ao:get(
            <<
                "/resources/", 
                ResourceOxygen/binary, 
                "/deposits/", 
                Alice/binary, 
                "/delegations/", 
                Bob/binary
            >>, 
            S2, 
            0, 
            Opts
        )
    ).

liquidate_exact_delegation_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Overdraw equals largest delegation - should fully liquidate one
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    S1 = delegate(Alice, Bob, ResourceOxygen, 10, S0, Opts),
    S2 = dev_pot:withdraw(Alice, ResourceOxygen, 10, S1, Opts),
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S2, Opts)),
    ?assertEqual(0, dev_pot:get_deposit(Bob, ResourceOxygen, S2, Opts)),
    % Delegation should be fully undone
    ?assertEqual(
        0, 
        hb_ao:get(
            <<
                "/resources/", 
                ResourceOxygen/binary, 
                "/deposits/", 
                Alice/binary, 
                "/delegations/", 
                Bob/binary
            >>, 
            S2, 
            0, 
            Opts
        )
    ).

liquidate_requiring_multiple_delegations_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    Charlie = <<"charlie">>,
    Denis = <<"denis">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    % Overdraw requires liquidating all delegations
    S0 = 
        pot_state_multi(
            ResourceOxygen, 
            [
                {Alice, 15}, 
                {Bob, 0}, 
                {Charlie, 0}, 
                {Denis, 0}
            ]
        ),
    S1 = delegate(Alice, Bob, ResourceOxygen, 5, S0, Opts),
    S2 = delegate(Alice, Charlie, ResourceOxygen, 5, S1, Opts),
    S3 = delegate(Alice, Denis, ResourceOxygen, 5, S2, Opts),
    % Alice has 0 deposits, 3 delegations of 5 each. Try to withdraw 12
    S4 = dev_pot:withdraw(Alice, ResourceOxygen, 12, S3, Opts),
    % After withdrawal, alice should have 0 (withdrew everything)
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S4, Opts)),
    % Total should be 3 (15 - 12 withdrawn)
    Total = dev_pot:get_deposit(Alice, ResourceOxygen, S4, Opts) +
            dev_pot:get_deposit(Bob, ResourceOxygen, S4, Opts) +
            dev_pot:get_deposit(Charlie, ResourceOxygen, S4, Opts) +
            dev_pot:get_deposit(Denis, ResourceOxygen, S4, Opts),
    ?assertEqual(3, Total).

liquidate_insufficient_delegations_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    % Overdraw exceeds total delegations - what happens?
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    S1 = delegate(Alice, Bob, ResourceOxygen, 10, S0, Opts),
    % Alice has 0 deposits, 10 delegated to Bob. Try to withdraw 15 - impossible
    ?assertMatch(
        {error, <<"Insufficient delegated balance to liquidate.">>},
        dev_pot:withdraw(Alice, ResourceOxygen, 15, S1, Opts)
    ).

undelegate_more_than_delegated_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    % Try to revoke more than was delegated
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    S1 = delegate(Alice, Bob, ResourceOxygen, 5, S0, Opts),
    % Delegation record shows 5, try to undelegate 10
    ?assertMatch(
        {error, <<"Undelegation amount exceeds existing delegation.">>},
        undelegate(Alice, Bob, ResourceOxygen, 10, S1, Opts)
    ).

undelegate_exceeds_delegated_rejected_with_recipient_buffer_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Bob starts with own deposits, this should still reject over undelegation
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 20}]),
    S1 = delegate(Alice, Bob, ResourceOxygen, 5, S0, Opts),
    ?assertMatch(
        {error, <<"Undelegation amount exceeds existing delegation.">>},
        undelegate(Alice, Bob, ResourceOxygen, 10, S1, Opts)
    ).

%%% Weight Change Scenarios

change_weight_with_deposits_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Changing weight should affect future yield distribution
    % Use larger mint cap to ensure enough tokens are minted
    S0 = pot_state(Alice, ResourceOxygen, 10, 1, 1000, 1, 2),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 1}, Opts),
    % Claim yield by performing a deposit
    S1_claim = dev_pot:deposit(Alice, ResourceOxygen, 1, S1, Opts),
    BalanceT1 = dev_pot:balance(Alice, S1_claim, Opts),
    % Change weight from 1 to 10
    S2 = dev_pot:register_resource(ResourceOxygen, 10, S1_claim, Opts),
    ?assertEqual(110, hb_maps:get(<<"total-weighted-units">>, S2, 0)),
    S3 = dev_pot:test_drip(S2, #{<<"t">> => 2}, Opts),
    % Claim yield by performing another deposit
    S3_claim = dev_pot:deposit(Alice, ResourceOxygen, 1, S3, Opts),
    BalanceT2 = dev_pot:balance(Alice, S3_claim, Opts),
    % Yield should still accrue
    ?assert(BalanceT2 > BalanceT1).

rapid_weight_changes_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Changing weight multiple times in succession
    S0 = pot_state(Alice, ResourceOxygen, 10),
    S1 = dev_pot:register_resource(ResourceOxygen, 5, S0, Opts),
    S2 = dev_pot:register_resource(ResourceOxygen, 10, S1, Opts),
    S3 = dev_pot:register_resource(ResourceOxygen, 2, S2, Opts),
    S4 = dev_pot:register_resource(ResourceOxygen, 1, S3, Opts),
    % Final TWU should be 1 * 10 = 10
    ?assertEqual(10, hb_maps:get(<<"total-weighted-units">>, S4, 0, Opts)).

%%% Time Handling Edge Cases

drip_with_large_time_jump_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Large time jump should still work correctly
    S0 = pot_state(Alice, ResourceOxygen, 10),
    % Jump 10000 time periods
    S1 = dev_pot:test_drip(S0, #{ <<"t">> => 10000 }, Opts),
    Minted = hb_maps:get(<<"minted">>, S1, 0, Opts),
    UndistributedMint = hb_maps:get(<<"undistributed-mint">>, S1, 0, Opts),
    TotalMinted = Minted + UndistributedMint,
    ?assert(TotalMinted >= 99),
    ?assert(Minted =< 100).

drip_same_timestamp_idempotent_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Dripping at same timestamp multiple times should be idempotent
    S0 = pot_state(Alice, ResourceOxygen, 10),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 1}, Opts),
    Minted1 = hb_maps:get(<<"minted">>, S1, 0, Opts),
    S2 = dev_pot:test_drip(S1, #{<<"t">> => 1}, Opts),
    Minted2 = hb_maps:get(<<"minted">>, S2, 0, Opts),
    % Should not mint additional tokens
    ?assertEqual(Minted1, Minted2).

drip_backward_timestamp_does_not_remint_test() ->
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    S0 = pot_state_empty([ResourceOxygen], 100, 1, 2),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 100}, Opts),
    Minted1 = hb_maps:get(<<"minted">>, S1, 0, Opts),
    {ok, S2} = dev_pot:mint(S1, #{<<"timestamp">> => 50}, Opts),
    ?assertEqual(Minted1, hb_maps:get(<<"minted">>, S2, 0, Opts)),
    ?assertEqual(100, hb_maps:get(<<"last-drip">>, S2, 0, Opts)),
    ?assertEqual(50, hb_maps:get(<<"t">>, S2, 0, Opts)).

%%% Accumulator Precision Tests

accumulator_over_many_periods_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    % Test accumulator precision over 1000 time steps
    S0 = pot_state(Alice, ResourceOxygen, 100, 1, 1000, 1, 100),
    SFinal = lists:foldl(
        fun(T, S) -> dev_pot:test_drip(S, #{<<"t">> => T}, Opts) end,
        S0,
        lists:seq(1, 1000)
    ),
    SFinalClaim = dev_pot:deposit(Alice, ResourceOxygen, 1, SFinal, Opts),
    Balance = dev_pot:balance(Alice, SFinalClaim, Opts),
    % Due to integer division, balance may be exactly 900
    ?assert(Balance >= 900),
    ?assert(Balance =< 1000).

very_small_deposit_yield_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % 1 unit deposit with large TWU - tests precision
    % Use larger MintCap (100000) so integer division produces results: 50000 / 1000 = 50
    S0 = 
        pot_state_multi(
            ResourceOxygen, 
            [{Alice, 1}, {Bob, 999}], 
            1, 
            100000, 
            1,
            2
        ),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 1}, Opts),
    % Claim yield by performing minimal deposits
    S1AliceClaim = dev_pot:deposit(Alice, ResourceOxygen, 1, S1, Opts),
    S1BobClaim = dev_pot:deposit(Bob, ResourceOxygen, 1, S1AliceClaim, Opts),
    AliceBalance = dev_pot:balance(Alice, S1BobClaim, Opts),
    BobBalance = dev_pot:balance(Bob, S1BobClaim, Opts),
    % Alice should get 50 (1/1000 of 50000), Bob should get 49950 (999/1000 of 50000)
    ?assert(AliceBalance >= 50),
    ?assert(AliceBalance < 100),
    ?assert(BobBalance >= 49900),
    ?assert(BobBalance < 50000).

frequent_materialization_drops_sub_raw_yield_test() ->
    Alice = <<"alice">>,
    ResourceDust = <<"dust-resource">>,
    Opts = #{},
    S0 = dev_pot:deposit(Alice, ResourceDust, 1, pot_state_empty([ResourceDust]), Opts),
    SPassive =
        hb_ao:set(
            S0,
            <<"/resources/", ResourceDust/binary, "/accumulator">>,
            ?POT_ACCUMULATOR_SCALE,
            Opts
        ),
    ?assertEqual(1, dev_pot:balance(Alice, SPassive, Opts)),
    SHalf =
        hb_ao:set(
            S0,
            <<"/resources/", ResourceDust/binary, "/accumulator">>,
            ?POT_ACCUMULATOR_SCALE div 2,
            Opts
        ),
    {ok, SMaterializedHalf} =
        dev_pot:mint(SHalf, #{<<"subject">> => Alice}, Opts),
    ?assertEqual(0, dev_pot:balance(Alice, SMaterializedHalf, Opts)),
    SFull =
        hb_ao:set(
            SMaterializedHalf,
            <<"/resources/", ResourceDust/binary, "/accumulator">>,
            ?POT_ACCUMULATOR_SCALE,
            Opts
        ),
    {ok, SMaterializedFull} =
        dev_pot:mint(SFull, #{<<"subject">> => Alice}, Opts),
    ?assertEqual(0, dev_pot:balance(Alice, SMaterializedFull, Opts)).

%%% Yield Claiming Tests

deposit_then_immediate_withdraw_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Deposit then immediately withdraw should preserve balance (minus any yield)
    S0 = pot_state_empty([ResourceOxygen]),
    S1 = dev_pot:deposit(Alice, ResourceOxygen, 10, S0, Opts),
    S2 = dev_pot:withdraw(Alice, ResourceOxygen, 10, S1, Opts),
    ?assertEqual(0, dev_pot:get_deposit(Alice, ResourceOxygen, S2, Opts)),
    ?assertEqual(0, dev_pot:balance(Alice, S2, Opts)).

multiple_deposits_same_resource_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Multiple deposits should be additive
    S0 = pot_state_empty([ResourceOxygen]),
    S1 = dev_pot:deposit(Alice, ResourceOxygen, 5, S0, Opts),
    S2 = dev_pot:deposit(Alice, ResourceOxygen, 3, S1, Opts),
    S3 = dev_pot:deposit(Alice, ResourceOxygen, 2, S2, Opts),
    ?assertEqual(10, dev_pot:get_deposit(Alice, ResourceOxygen, S3, Opts)).

%%% Input Validation Tests

deposit_with_negative_amount_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Negative deposits should be rejected
    S0 = pot_state_empty([ResourceOxygen]),
    ?assertMatch(
        {error, _},
        dev_pot:deposit(Alice, ResourceOxygen, -10, S0, Opts)
    ).

withdraw_with_negative_amount_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Negative withdrawals should be rejected
    S0 = pot_state(Alice, ResourceOxygen, 10),
    ?assertMatch(
        {error, _},
        dev_pot:withdraw(Alice, ResourceOxygen, -5, S0, Opts)
    ).

deposit_non_integer_quantity_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Non-integer quantities should be rejected
    S0 = pot_state_empty([ResourceOxygen]),
    ?assertMatch({error, _}, dev_pot:deposit(Alice, ResourceOxygen, 10.5, S0, Opts)),
    ?assertMatch({error, _}, dev_pot:deposit(Alice, ResourceOxygen, ten, S0, Opts)),
    ?assertMatch({error, _}, dev_pot:deposit(Alice, ResourceOxygen, "10", S0, Opts)).

deposit_non_binary_address_test() ->
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Non-binary addresses should fail
    S0 = pot_state_empty([ResourceOxygen]),
    ?assertMatch({error, _}, dev_pot:deposit(12345, ResourceOxygen, 10, S0, Opts)),
    ?assertMatch({error, _}, dev_pot:deposit(alice, ResourceOxygen, 10, S0, Opts)).

delegate_negative_amount_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Negative delegation amount should be rejected
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    ?assertMatch(
        {error, _},
        delegate(Alice, Bob, ResourceOxygen, -5, S0, Opts)
    ).

deposit_to_nonexistent_resource_test() ->
    Alice = <<"alice">>,
    ResourceHydrogen = <<"hydrogen">>,
    Opts =#{},
    % Depositing to a resource that doesn't exist should auto-create it
    S0 = pot_state_empty([]),
    % Deposit to non-existent hydrogen resource
    S1 = dev_pot:deposit(Alice, ResourceHydrogen, 10, S0, Opts),
    % Verify the resource was created and deposit succeeded
    ?assertEqual(10, dev_pot:get_deposit(Alice, ResourceHydrogen, S1, Opts)).

%%% Delegation Notice Tests

delegation_notice_message_format_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Verify delegation notice has correct format
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    S0WithOutbox = S0#{ <<"results">> => #{ <<"outbox">> => [] } },
    S1 = delegate(Alice, Bob, ResourceOxygen, 5, S0WithOutbox, Opts),
    Outbox = hb_ao:get(<<"results/outbox">>, S1, [], Opts),
    ?assertEqual(1, length(Outbox)),
    [Notice] = Outbox,
    ?assertEqual(
        Bob, 
        hb_maps:get(<<"target">>, Notice, Opts)
    ),
    ?assertEqual(
        <<"deposit">>, 
        hb_maps:get(<<"action">>, Notice, Opts)
    ),
    ?assertEqual(5, hb_maps:get(<<"quantity">>, Notice, not_found, Opts)),
    ?assertEqual(ResourceOxygen, hb_maps:get(<<"resource">>, Notice, not_found, Opts)).

undelegate_notice_has_positive_quantity_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Undelegation notice should encode direction via `action', with a
    % positive quantity so downstream `withdraw` handlers can accept it.
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 10}, {Bob, 0}]),
    S0WithOutbox = S0#{ <<"results">> => #{ <<"outbox">> => [] } },
    S1 = delegate(Alice, Bob, ResourceOxygen, 5, S0WithOutbox, Opts),
    S2 = undelegate(Alice, Bob, ResourceOxygen, 5, S1, Opts),
    Outbox = hb_ao:get(<<"results/outbox">>, S2, [], Opts),
    ?assertEqual(2, length(Outbox)),
    % Outbox is newest first, so undelegate notice is first
    [UndelegateNotice, _] = Outbox,
    ?assertEqual(Bob, hb_maps:get(<<"target">>, UndelegateNotice, not_found, Opts)),
    ?assertEqual(<<"withdraw">>, hb_maps:get(<<"action">>, UndelegateNotice, not_found, Opts)),
    ?assertEqual(Alice, hb_maps:get(<<"address">>, UndelegateNotice, not_found, Opts)),
    ?assertEqual(5, hb_maps:get(<<"quantity">>, UndelegateNotice, not_found, Opts)),
    ?assertEqual(ResourceOxygen, hb_maps:get(<<"resource">>, UndelegateNotice, not_found, Opts)).

multiple_delegations_outbox_order_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    Charlie = <<"charlie">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Multiple delegations should appear in outbox in order
    S0 = pot_state_multi(ResourceOxygen, [{Alice, 20}, {Bob, 0}, {Charlie, 0}]),
    S0WithOutbox = S0#{ <<"results">> => #{ <<"outbox">> => [] } },
    S1 = delegate(Alice, Bob, ResourceOxygen, 5, S0WithOutbox, Opts),
    S2 = delegate(Alice, Charlie, ResourceOxygen, 5, S1, Opts),
    Outbox = hb_ao:get(<<"results/outbox">>, S2, [], Opts),
    ?assertEqual(2, length(Outbox)),
    % Outbox is newest first: [Charlie (S2), Bob (S1)]
    [Notice1, Notice2] = Outbox,
    ?assertEqual(Charlie, hb_maps:get(<<"target">>, Notice1, not_found, Opts)),
    ?assertEqual(Bob, hb_maps:get(<<"target">>, Notice2, not_found, Opts)).

notify_rejects_non_parent_source_test() ->
    Parent = <<"parent">>,
    Mallory = <<"mallory">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    State = (pot_state_empty([ResourceOxygen]))#{ <<"parent">> => Parent },
    Assignment =
        #{
            <<"body">> =>
                #{
                    <<"from">> => Mallory,
                    <<"x-action">> => <<"register">>,
                    <<"x-resource">> => ResourceOxygen,
                    <<"x-weight">> => 2
                }
        },
    ?assertMatch(
        {error, <<"Invalid notification source">>},
        dev_pot:notify(State, Assignment, Opts)
    ).

notify_rejects_non_register_forward_action_test() ->
    Parent = <<"parent">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    State = (pot_state_empty([ResourceOxygen]))#{ <<"parent">> => Parent },
    Assignment =
        #{
            <<"body">> =>
                #{
                    <<"from">> => Parent,
                    <<"x-action">> => <<"deposit">>
                }
        },
    ?assertMatch(
        {error, <<"Unsupported notification action">>},
        dev_pot:notify(State, Assignment, Opts)
    ).

quantity_scale_register_update_and_notify_test() ->
    Admin = <<"admin">>,
    Parent = <<"parent">>,
    Alice = <<"alice">>,
    Resource = <<"scale-config-resource">>,
    Opts = #{},
    Weight = 7,
    Quantity = 10,
    AssetScale = 100_000_000, % 1e8
    S0 =
        (pot_state(Alice, Resource, Quantity, Weight, 100, 1, 2))#{
            <<"mint-authority">> => Admin
        },
    InitialTWU = hb_maps:get(<<"total-weighted-units">>, S0, 0, Opts),
    S1 =
        dev_pot:register(
            S0,
            #{
                <<"body">> => #{
                    <<"resource">> => Resource,
                    <<"quantity-scale">> => AssetScale,
                    <<"from">> => Admin
                }
            },
            Opts
        ),
    ?assert(is_map(S1)),
    ?assertEqual(
        AssetScale,
        hb_ao:get(<<"/resources/", Resource/binary, "/quantity-scale">>, S1, 0, Opts)
    ),
    ?assertEqual(Weight, hb_ao:get(<<"/resources/", Resource/binary, "/weight">>, S1, 0, Opts)),
    ?assertEqual(InitialTWU, hb_maps:get(<<"total-weighted-units">>, S1, 0, Opts)),
    ChildState = (pot_state_empty([Resource]))#{ <<"parent">> => Parent },
    ForwardedRegister =
        dev_process_outbox:original_from_forwarded(
            #{
                <<"x-action">> => <<"register">>,
                <<"x-resource">> => Resource,
                <<"x-weight">> => Weight,
                <<"x-quantity-scale">> => AssetScale
            },
            Opts
        ),
    ChildAfterNotify =
        dev_pot:register(
            ChildState,
            #{
                <<"type">> => <<"notification">>,
                <<"body">> =>
                    maps:merge(
                        ForwardedRegister,
                        #{
                            <<"from">> => Parent,
                            <<"resource-authority">> => Parent,
                            <<"weight-authority">> => Parent
                        }
                    )
            },
            Opts
        ),
    ?assert(is_map(ChildAfterNotify)),
    ?assertEqual(
        AssetScale,
        hb_ao:get(
            <<"/resources/", Resource/binary, "/quantity-scale">>,
            ChildAfterNotify,
            0,
            Opts
        )
    ),
    ?assertEqual(
        Weight,
        hb_ao:get(<<"/resources/", Resource/binary, "/weight">>, ChildAfterNotify, 0, Opts)
    ).

%%% Empty/Zero State Tests

zero_mint_cap_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Mint-cap = 0 should mint nothing
    S0 = pot_state(Alice, ResourceOxygen, 10, 1, 0, 1, 2),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 1}, Opts),
    Minted = hb_maps:get(<<"minted">>, S1, 0, Opts),
    ?assertEqual(0, Minted),
    ?assertEqual(0, dev_pot:balance(Alice, S1, Opts)).

deposit_with_zero_mint_cap_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Can still deposit even with zero mint cap
    S0 = pot_state_empty([ResourceOxygen], 0, 1, 2),
    S1 = dev_pot:deposit(Alice, ResourceOxygen, 10, S0, Opts),
    ?assertEqual(10, dev_pot:get_deposit(Alice, ResourceOxygen, S1, Opts)).

resource_with_no_deposits_test() ->
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    % Resource with weight but no deposits
    S0 = pot_state_empty([ResourceOxygen]),
    % Manually set weight to 5 for testing
    S0Updated = hb_ao:set(S0, <<"/resources/oxygen/weight">>, 5, Opts),
    % Should not crash when dripping
    S1 = dev_pot:test_drip(S0Updated, #{<<"t">> => 1}, Opts),
    ?assertEqual(0, hb_maps:get(<<"total-weighted-units">>, S1, 0, Opts)).

%%% Multi-Resource Coordination Tests

deposit_to_multiple_resources_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    Opts = #{},
    % Same user deposits to multiple resources
    S0 = pot_state_empty([ResourceOxygen, ResourceHydrogen]),
    S1 = dev_pot:deposit(Alice, ResourceOxygen, 10, S0, Opts),
    S2 = dev_pot:deposit(Alice, ResourceHydrogen, 20, S1, Opts),
    ?assertEqual(10, dev_pot:get_deposit(Alice, ResourceOxygen, S2, Opts)),
    ?assertEqual(20, dev_pot:get_deposit(Alice, ResourceHydrogen, S2, Opts)),
    ?assertEqual(30, hb_maps:get(<<"total-weighted-units">>, S2, 0, Opts)).

resource_isolation_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    Opts = #{},
    % Changes to one resource shouldn't affect another
    S0 = pot_state_empty([ResourceOxygen, ResourceHydrogen]),
    S1 = dev_pot:deposit(Alice, ResourceOxygen, 10, S0, Opts),
    S2 = dev_pot:deposit(Bob, ResourceHydrogen, 10, S1, Opts),
    InitialOxygenDeposit = dev_pot:get_deposit(Alice, ResourceOxygen, S2, Opts),
    % Modify hydrogen
    S3 = dev_pot:withdraw(Bob, ResourceHydrogen, 5, S2, Opts),
    % Oxygen deposit should be unchanged
    ?assertEqual(
        InitialOxygenDeposit, 
        dev_pot:get_deposit(Alice, ResourceOxygen, S3, Opts)
    ).

weighted_distribution_across_resources_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    Opts = #{},
    % Different weights should result in proportional yield
    S0 = pot_state_empty([ResourceOxygen, ResourceHydrogen]),
    S1 = dev_pot:deposit(Alice, ResourceOxygen, 10, S0, Opts),
    S2 = dev_pot:deposit(Bob, ResourceHydrogen, 10, S1, Opts),
    % Set hydrogen weight to 3
    S3 = dev_pot:register_resource(ResourceHydrogen, 3, S2, Opts),
    % Drip at t=1
    S4 = dev_pot:test_drip(S3, #{<<"t">> => 1}, Opts),
    AliceBalance = dev_pot:balance(Alice, S4, Opts),
    BobBalance = dev_pot:balance(Bob, S4, Opts),
    % Bob should get 3x Alice's yield (3x weight)
    ?assert(BobBalance > AliceBalance * 2.5),
    ?assert(BobBalance < AliceBalance * 3.5).

%%% Balance Overflow Tests

very_large_deposit_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % Near max integer deposit
    S0 = pot_state_empty([ResourceOxygen]),
    LargeAmount = 999999999999999, % ~10^15
    S1 = dev_pot:deposit(Alice, ResourceOxygen, LargeAmount, S0, Opts),
    ?assertEqual(LargeAmount, dev_pot:get_deposit(Alice, ResourceOxygen, S1, Opts)),
    ?assertEqual(LargeAmount, hb_maps:get(<<"total-weighted-units">>, S1, 0, Opts)).

quantity_scale_normalizes_very_large_deposit_test() ->
    Alice = <<"alice">>,
    Oracle = <<"oracle">>,
    ResourceLarge8Decimal = <<"large-8-decimal-asset">>,
    Opts = #{},
    AssetScale = 100_000_000, % 1e8
    FullAssetSupply = 21_000_000 * AssetScale, % 21M supply
    LargePriceWeight = 1_000_000 * ?POT_PRICE_SCALE, % $1M
    NormalizedFullAssetSupply = 21_000_000 * ?POT_QUANTITY_SCALE,
    NormalizedOneAsset = ?POT_QUANTITY_SCALE,
    S0 = pot_state_empty([ResourceLarge8Decimal]),
    S1 =
        hb_ao:set(
            S0,
            <<"/resources/large-8-decimal-asset/authority">>,
            [Oracle],
            Opts
        ),
    S2 =
        dev_pot:register_resource(
            ResourceLarge8Decimal,
            LargePriceWeight,
            AssetScale,
            S1,
            Opts
        ),
    S3 =
        dev_pot:deposit(
            S2,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => ResourceLarge8Decimal,
                    <<"quantity">> => FullAssetSupply,
                    <<"from">> => Oracle
                }
            },
            Opts
        ),
    ?assert(is_map(S3)),
    ?assertEqual(
        NormalizedFullAssetSupply,
        dev_pot:get_deposit(Alice, ResourceLarge8Decimal, S3, Opts)
    ),
    ?assertEqual(
        NormalizedFullAssetSupply * LargePriceWeight,
        hb_maps:get(<<"total-weighted-units">>, S3, 0, Opts)
    ),
    S4 =
        dev_pot:withdraw(
            S3,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => ResourceLarge8Decimal,
                    <<"quantity">> => AssetScale,
                    <<"from">> => Oracle
                }
            },
            Opts
        ),
    ?assert(is_map(S4)),
    ?assertEqual(
        NormalizedFullAssetSupply - NormalizedOneAsset,
        dev_pot:get_deposit(Alice, ResourceLarge8Decimal, S4, Opts)
    ),
    ?assertEqual(
        (NormalizedFullAssetSupply - NormalizedOneAsset) * LargePriceWeight,
        hb_maps:get(<<"total-weighted-units">>, S4, 0, Opts)
    ).

inline_quantity_scale_is_rejected_test() ->
    Alice = <<"alice">>,
    Oracle = <<"oracle">>,
    Resource8Decimal = <<"inline-scale-rejected-asset">>,
    Opts = #{},
    AssetScale = 100_000_000, % 1e8
    S0 = pot_state_empty([Resource8Decimal]),
    S1 =
        hb_ao:set(
            S0,
            <<"/resources/inline-scale-rejected-asset/authority">>,
            [Oracle],
            Opts
        ),
    S2 =
        dev_pot:register_resource(
            Resource8Decimal,
            ?POT_PRICE_SCALE,
            AssetScale,
            S1,
            Opts
        ),
    ?assertMatch(
        {error, <<"quantity-scale must be configured on the resource.">>},
        dev_pot:deposit(
            S2,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => Resource8Decimal,
                    <<"quantity">> => AssetScale,
                    <<"quantity-scale">> => AssetScale,
                    <<"from">> => Oracle
                }
            },
            Opts
        )
    ),
    S3 =
        dev_pot:deposit(
            S2,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => Resource8Decimal,
                    <<"quantity">> => AssetScale,
                    <<"from">> => Oracle
                }
            },
            Opts
        ),
    ?assert(is_map(S3)),
    ?assertMatch(
        {error, <<"quantity-scale must be configured on the resource.">>},
        dev_pot:withdraw(
            S3,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => Resource8Decimal,
                    <<"quantity">> => AssetScale,
                    <<"quantity-scale">> => AssetScale,
                    <<"from">> => Oracle
                }
            },
            Opts
        )
    ).

quantity_scale_delegation_uses_normalized_units_test() ->
    Alice = <<"alice">>,
    Bob = <<"bob">>,
    Oracle = <<"oracle">>,
    Resource8Decimal = <<"delegated-8-decimal-asset">>,
    Opts = #{},
    AssetScale = 100_000_000, % 1e8
    RawTenAssets = 10 * AssetScale,
    NormalizedTenAssets = 10 * ?POT_QUANTITY_SCALE,
    NormalizedThreeAssets = 3 * ?POT_QUANTITY_SCALE,
    NormalizedPointOneAsset = ?POT_QUANTITY_SCALE div 10,
    NormalizedOneAsset = ?POT_QUANTITY_SCALE,
    S0 = pot_state_empty([Resource8Decimal]),
    S1 =
        hb_ao:set(
            S0,
            <<"/resources/delegated-8-decimal-asset/authority">>,
            [Oracle],
            Opts
        ),
    S2 =
        dev_pot:register_resource(
            Resource8Decimal,
            ?POT_PRICE_SCALE,
            AssetScale,
            S1,
            Opts
        ),
    S3 =
        dev_pot:deposit(
            S2,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => Resource8Decimal,
                    <<"quantity">> => RawTenAssets,
                    <<"from">> => Oracle
                }
            },
            Opts
        ),
    ?assertEqual(
        NormalizedTenAssets,
        dev_pot:get_deposit(Alice, Resource8Decimal, S3, Opts)
    ),
    {ok, S4} =
        dev_pot:delegate(
            S3,
            #{
                <<"body">> => #{
                    <<"from">> => Alice,
                    <<"address">> => Bob,
                    <<"resource">> => Resource8Decimal,
                    <<"quantity">> => NormalizedThreeAssets
                }
            },
            Opts
        ),
    ?assertEqual(
        NormalizedTenAssets - NormalizedThreeAssets,
        dev_pot:get_deposit(Alice, Resource8Decimal, S4, Opts)
    ),
    ?assertEqual(
        NormalizedThreeAssets,
        dev_pot:get_deposit(Bob, Resource8Decimal, S4, Opts)
    ),
    ?assertEqual(
        NormalizedThreeAssets,
        hb_ao:get(
            <<
                "/resources/",
                Resource8Decimal/binary,
                "/deposits/",
                Alice/binary,
                "/delegations/",
                Bob/binary
            >>,
            S4,
            0,
            Opts
        )
    ),
    {ok, S5} =
        dev_pot:delegate(
            S4,
            #{
                <<"body">> => #{
                    <<"from">> => Alice,
                    <<"address">> => Bob,
                    <<"resource">> => Resource8Decimal,
                    <<"quantity">> => NormalizedPointOneAsset
                }
            },
            Opts
        ),
    ?assertEqual(
        NormalizedTenAssets - NormalizedThreeAssets - NormalizedPointOneAsset,
        dev_pot:get_deposit(Alice, Resource8Decimal, S5, Opts)
    ),
    ?assertEqual(
        NormalizedThreeAssets + NormalizedPointOneAsset,
        dev_pot:get_deposit(Bob, Resource8Decimal, S5, Opts)
    ),
    ?assertEqual(
        NormalizedThreeAssets + NormalizedPointOneAsset,
        hb_ao:get(
            <<
                "/resources/",
                Resource8Decimal/binary,
                "/deposits/",
                Alice/binary,
                "/delegations/",
                Bob/binary
            >>,
            S5,
            0,
            Opts
        )
    ),
    {ok, S6} =
        dev_pot:undelegate(
            S5,
            #{
                <<"body">> => #{
                    <<"from">> => Alice,
                    <<"address">> => Bob,
                    <<"resource">> => Resource8Decimal,
                    <<"quantity">> => NormalizedOneAsset
                }
            },
            Opts
        ),
    ?assertEqual(
        NormalizedTenAssets
            - NormalizedThreeAssets
            - NormalizedPointOneAsset
            + NormalizedOneAsset,
        dev_pot:get_deposit(Alice, Resource8Decimal, S6, Opts)
    ),
    ?assertEqual(
        NormalizedThreeAssets + NormalizedPointOneAsset - NormalizedOneAsset,
        dev_pot:get_deposit(Bob, Resource8Decimal, S6, Opts)
    ),
    ?assertEqual(
        NormalizedThreeAssets + NormalizedPointOneAsset - NormalizedOneAsset,
        hb_ao:get(
            <<
                "/resources/",
                Resource8Decimal/binary,
                "/deposits/",
                Alice/binary,
                "/delegations/",
                Bob/binary
            >>,
            S6,
            0,
            Opts
        )
    ).

sub_unit_price_8_decimal_asset_distributes_reward_test() ->
    Alice = <<"alice">>,
    Oracle = <<"oracle">>,
    ResourceSubUnit8Decimal = <<"sub-unit-8-decimal-asset">>,
    Opts = #{},
    AssetScale = 100_000_000, % 1e8
    LargeSupply = 150_000_000_000 * AssetScale, % 150B supply
    SubUnitPriceWeight = 1_500, % $0.0015
    NormalizedLargeSupply = 150_000_000_000 * ?POT_QUANTITY_SCALE,
    S0 =
        pot_state_empty(
            [ResourceSubUnit8Decimal],
            ?AO_TOTAL_SUPPLY,
            ?AO_MS_STEP_NUMERATOR,
            ?AO_MINT_PROP_DENOMINATOR
        ),
    S1 =
        hb_ao:set(
            S0,
            <<"/resources/sub-unit-8-decimal-asset/authority">>,
            [Oracle],
            Opts
        ),
    S2 =
        dev_pot:register_resource(
            ResourceSubUnit8Decimal,
            SubUnitPriceWeight,
            AssetScale,
            S1,
            Opts
        ),
    S3 =
        dev_pot:deposit(
            S2,
            #{
                <<"body">> => #{
                    <<"address">> => Alice,
                    <<"resource">> => ResourceSubUnit8Decimal,
                    <<"quantity">> => LargeSupply,
                    <<"from">> => Oracle
                }
            },
            Opts
        ),
    ?assert(is_map(S3)),
    ?assertEqual(
        NormalizedLargeSupply,
        dev_pot:get_deposit(Alice, ResourceSubUnit8Decimal, S3, Opts)
    ),
    ?assertEqual(
        NormalizedLargeSupply * SubUnitPriceWeight,
        hb_maps:get(<<"total-weighted-units">>, S3, 0, Opts)
    ),
    S4 = dev_pot:test_drip(S3, #{ <<"t">> => ?AO_ONE_DAY_MS }, Opts),
    Minted = hb_maps:get(<<"minted">>, S4, 0, Opts),
    TotalWeightedUnits = hb_maps:get(<<"total-weighted-units">>, S4, 0, Opts),
    ?assertEqual(0, Minted div TotalWeightedUnits),
    ?assert(hb_maps:get(<<"accumulator">>, S4, 0, Opts) > 0),
    ?assert(dev_pot:balance(Alice, S4, Opts) > 0).

ten_million_user_billion_tvl_aggregate_math_test() ->
    %% This intentionally does not allocate 10M deposit records. It validates
    %% the equivalent aggregate TWU and per-user arithmetic for an even split.
    Users = 10_000_000,

    %% Scenario:
    %% - 1M units of a high-price 18-decimal asset at $2,300.
    %% - 5B units of stable asset A at $1.
    %% - 2B units of stable asset B at $1.
    %% All quantities are normalized to POT_QUANTITY_SCALE and prices use
    %% POT_PRICE_SCALE, so the aggregate TVL is $9.3B:
    %%   (1M * 2300) + 5B + 2B = 9.3B USD.
    HighPriceTotalQty = 1_000_000 * ?POT_QUANTITY_SCALE,
    StableATotalQty = 5_000_000_000 * ?POT_QUANTITY_SCALE,
    StableBTotalQty = 2_000_000_000 * ?POT_QUANTITY_SCALE,
    HighPriceWeight = 2_300 * ?POT_PRICE_SCALE,
    StableWeight = ?POT_PRICE_SCALE,
    ExpectedTotalWeightedUnits = 9_300_000_000 * ?POT_QUANTITY_SCALE * ?POT_PRICE_SCALE,
    TotalWeightedUnits =
        (HighPriceTotalQty * HighPriceWeight)
            + (StableATotalQty * StableWeight)
            + (StableBTotalQty * StableWeight),

    %% First-day AO mint under the millisecond curve. This is the same checkpoint
    %% asserted in mint3_current_ms_day_test/0.
    ToMint =
        dev_pot_math:minted_between(
            0,
            ?AO_TOTAL_SUPPLY,
            ?AO_MS_STEP_NUMERATOR,
            ?AO_MINT_PROP_DENOMINATOR,
            0,
            ?AO_ONE_DAY_MS
        ),
    ?assertEqual(?AO_CURRENT_MS_FIRST_DAY_MINTED, ToMint),
    ?assertEqual(ExpectedTotalWeightedUnits, TotalWeightedUnits),

    %% GlobalAcc is the scaled per-weighted-unit reward delta for the day:
    %%   floor(ToMint * POT_ACCUMULATOR_SCALE / TotalWeightedUnits).
    %% The non-zero remainder proves sub-accumulator dust is carried forward
    %% globally instead of being treated as raw AO supply.
    {GlobalAcc, 0, AccumulatorRemainder} =
        dev_pot_math:drip_global(0, ToMint, 0, TotalWeightedUnits),
    ?assertEqual(1_071_217, GlobalAcc),
    ?assert(AccumulatorRemainder > 0),

    %% Even split across 10M users:
    %% - each user gets 0.1 high-price asset
    %% - each user gets 500 stable A
    %% - each user gets 200 stable B
    HighPriceUserQty = HighPriceTotalQty div Users,
    StableAUserQty = StableATotalQty div Users,
    StableBUserQty = StableBTotalQty div Users,

    %% Resource accumulators multiply the global accumulator by the resource's
    %% price weight. User yield then converts the scaled accumulator back to raw
    %% AO units with drip_user/3.
    HighPriceResAcc =
        dev_pot_math:drip_resource(0, GlobalAcc, 0, HighPriceWeight),
    StableResAcc =
        dev_pot_math:drip_resource(0, GlobalAcc, 0, StableWeight),
    HighPriceYield = dev_pot_math:drip_user(HighPriceResAcc, 0, HighPriceUserQty),
    StableAYield = dev_pot_math:drip_user(StableResAcc, 0, StableAUserQty),
    StableBYield = dev_pot_math:drip_user(StableResAcc, 0, StableBUserQty),
    ?assertEqual(246_379_910, HighPriceYield),
    ?assertEqual(535_608_500, StableAYield),
    ?assertEqual(214_243_400, StableBYield),

    %% Aggregate all representative users and verify no inflation. The gap below
    %% ToMint is raw AO dust/rounding and remains below one AO.
    PerUserYield = HighPriceYield + StableAYield + StableBYield,
    AggregateUserYield = PerUserYield * Users,
    ?assertEqual(996_231_810, PerUserYield),
    ?assertEqual(9_962_318_100_000_000, AggregateUserYield),
    ?assert(AggregateUserYield =< ToMint),
    ?assert(ToMint - AggregateUserYield < ?AO_TOKEN_DENOMINATION),

    %% Tiny user edge case: a $0.10 stable position (deposit) passively accrues raw AO over
    %% the full day, but if materialized at each single global accumulator tick
    %% it floors to zero each time. This documents the current no per-user
    %% remainder tradeoff.
    TinyUserQty = ?POT_QUANTITY_SCALE div 10,
    TinyPassiveYield = dev_pot_math:drip_user(StableResAcc, 0, TinyUserQty),
    TinySingleTickYield =
        dev_pot_math:drip_user(
            dev_pot_math:drip_resource(0, 1, 0, StableWeight),
            0,
            TinyUserQty
        ),
    ?assertEqual(107_121, TinyPassiveYield),
    ?assertEqual(0, TinySingleTickYield),
    ?assertEqual(1_071_210_000_000, TinyPassiveYield * Users).

very_large_minted_amount_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts =#{},
    % Very large mint cap
    LargeMintCap = 999999999999999, % ~10^15
    S0 = pot_state(Alice, ResourceOxygen, 100, 1, LargeMintCap, 1, 2),
    S1 = dev_pot:test_drip(S0, #{<<"t">> => 1}, Opts),
    Minted = hb_maps:get(<<"minted">>, S1, 0, Opts),
    % Should mint large amount without overflow
    ?assert(Minted > 1000000000000),
    % Claim yield by performing a deposit
    S1_claim = dev_pot:deposit(Alice, ResourceOxygen, 1, S1, Opts),
    Balance = dev_pot:balance(Alice, S1_claim, Opts),
    ?assert(Balance > 1000000000000).

%%% State Corruption/Recovery Tests

missing_accumulator_field_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % State missing accumulator field
    S0 = pot_state(Alice, ResourceOxygen, 10),
    % Remove accumulator field to test missing field handling
    S0NoAcc = maps:remove(<<"accumulator">>, S0),
    % Should handle gracefully (default to 0)
    S1 = dev_pot:test_drip(S0NoAcc, #{ <<"t">> => 1 }, Opts),
    ?assert(is_map(S1)).

missing_total_weighted_units_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % State missing TWU field
    S0 = pot_state(Alice, ResourceOxygen, 10),
    % Remove TWU field to test missing field handling
    S0NoTWU = maps:remove(<<"total-weighted-units">>, S0),
    S1 = dev_pot:test_drip(S0NoTWU, #{ <<"t">> => 1 }, Opts),
    % All minted tokens should go to undistributed-mint (since TWU=0)
    Minted = hb_maps:get(<<"minted">>, S1, 0, Opts),
    UndistributedMint = hb_maps:get(<<"undistributed-mint">>, S1, 0, Opts),
    ?assertEqual(Minted, UndistributedMint).

missing_balances_field_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    % State missing balances field
    S0 = pot_state_empty([ResourceOxygen]),
    % Remove balances field to test missing field handling
    S0NoBalances = maps:remove(<<"balances">>, S0),
    % Deposit should fail or auto-create balances
    ?assertError(_, dev_pot:deposit(Alice, ResourceOxygen, 10, S0NoBalances, Opts)).

mint_distribution_test() ->
    Alice = <<"alice">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    S0 = pot_state(Alice, ResourceOxygen, 20),
    % Tick 0: mint = 50, pot units = 20, accumulate 2.5 scaled with no
    % undistributed mint.
    S1 = dev_pot:test_drip(S0, #{ <<"t">> => 1 }, Opts),
    ?assertEqual(
        50,
        hb_maps:get(<<"minted">>, S1, not_found, Opts)
    ),
    ?assertEqual(
        0,
        hb_maps:get(<<"undistributed-mint">>, S1, not_found, Opts)
    ),
    ?assertEqual(
        scaled_accumulator(5, 2),
        hb_maps:get(<<"accumulator">>, S1, not_found, Opts)
    ),
    % Tick 1: mint = 25, pot units = 20, accumulate 1.25 scaled with no
    % undistributed mint.
    S2 = dev_pot:test_drip(S1, #{ <<"t">> => 2 }, Opts),
    ?assertEqual(
        75,
        hb_maps:get(<<"minted">>, S2, not_found, Opts)
    ),
    ?assertEqual(
        0,
        hb_maps:get(<<"undistributed-mint">>, S2, not_found, Opts)
    ),
    ?assertEqual(
        scaled_accumulator(15, 4),
        hb_maps:get(<<"accumulator">>, S2, not_found, Opts)
    ),
    % Tick 3: mint = 12, pot units = 20, accumulate 0.6 scaled with no
    % undistributed mint.
    S3 = dev_pot:test_drip(S2, #{ <<"t">> => 3 }, Opts),
    ?assertEqual(
        87,
        hb_maps:get(<<"minted">>, S3, not_found, Opts)
    ),
    ?assertEqual(
        0,
        hb_maps:get(<<"undistributed-mint">>, S3, not_found, Opts)
    ),
    ?assertEqual(
        scaled_accumulator(87, 20),
        hb_maps:get(<<"accumulator">>, S3, not_found, Opts)
    ),
    % Tick 4: mint = 6, pot units = 20, accumulate 0.3 scaled with no
    % undistributed mint.
    S4 = dev_pot:test_drip(S3, #{ <<"t">> => 4 }, Opts),
    ?assertEqual(
        93,
        hb_maps:get(<<"minted">>, S4, not_found, Opts)
    ),
    ?assertEqual(
        0,
        hb_maps:get(<<"undistributed-mint">>, S4, not_found, Opts)
    ),
    ?assertEqual(
        scaled_accumulator(93, 20),
        hb_maps:get(<<"accumulator">>, S4, not_found, Opts)
    ),
    % Tick 5: mint = 3, pot units = 20, accumulate 0.15 scaled with no
    % undistributed mint.
    S5 = dev_pot:test_drip(S4, #{ <<"t">> => 5 }, Opts),
    ?assertEqual(
        96,
        hb_maps:get(<<"minted">>, S5, not_found, Opts)
    ),
    ?assertEqual(
        0,
        hb_maps:get(<<"undistributed-mint">>, S5, not_found, Opts)
    ),
    ?assertEqual(
        scaled_accumulator(24, 5),
        hb_maps:get(<<"accumulator">>, S5, not_found, Opts)
    ),
    S6 = dev_pot:withdraw(Alice, ResourceOxygen, 10, S5, Opts),
    % Tick 6: mint 2, pot units = 10, accumulate 0.2 scaled with no
    % undistributed mint.
    S7 = dev_pot:test_drip(S6, #{ <<"t">> => 6 }, Opts),
    ?assertEqual(
        98,
        hb_maps:get(<<"minted">>, S7, not_found, Opts)
    ),
    ?assertEqual(
        0,
        hb_maps:get(<<"undistributed-mint">>, S7, not_found, Opts)
    ),
    ?assertEqual(
        scaled_accumulator(5, 1),
        hb_maps:get(<<"accumulator">>, S7, not_found, Opts)
    ).

report(S, Opts) ->
    ?event(
        {report,
            {t, hb_maps:get(<<"t">>, S, no_timestamp, Opts)},
            {last_drip, hb_maps:get(<<"last-drip">>, S, undefined, Opts)},
            {balances, dev_pot:balances(S, Opts)},
            {deposits, dev_pot:get_deposits(S, Opts)},
            {minted, hb_maps:get(<<"minted">>, S, not_found, Opts)},
            {state, S}
        }
    ).
