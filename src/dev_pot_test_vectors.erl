%%% @doc Test vectors for the `~pot@1.0` device.
-module(dev_pot_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

mint_quantity_test() ->
    ?assertEqual(50, dev_pot_math:minted_between(0, 100, 1, 2, 0, 1)),
    ?assertEqual(75, dev_pot_math:minted_between(0, 100, 1, 2, 0, 2)),
    ?assertEqual(87, dev_pot_math:minted_between(0, 100, 1, 2, 0, 3)),
    Period1 = dev_pot_math:minted_between(0, 100, 1, 2, 0, 2),
    Period2 = dev_pot_math:minted_between(Period1, 100, 1, 2, 2, 3),
    ?assertEqual(87, Period1 + Period2).

%% @doc Demonstrate minting using the proportional model and a single resource.
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
        <<"mint-prop">> => {1, 2},
        <<"resources">> => #{
            ResourceID => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:deposit(Addr1, ResourceID, 10, S0, Opts),
    S2 = dev_pot:deposit(Addr2, ResourceID, 10, S1, Opts),
    report(S2),
    S3 = dev_pot:drip(S2, #{ <<"t">> => 1 }, Opts),
    report(S3),
    % At t=1, there are 20 pot units and 50 minted to distribute, 50 div 20 = 2,
    % so it's 20 to each address with 10 undistributed
    ?assertEqual(20, dev_pot:balance(Addr1, S3)),
    ?assertEqual(20, dev_pot:balance(Addr2, S3)),
    S4 = dev_pot:drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4),
    % At t=2, there are 20 pot units and 25 + 10 minted to distribute, 35 div 20 = 1,
    % so it's 10 to each address with 15 undistributed
    ?assertEqual(30, dev_pot:balance(Addr1, S4)),
    ?assertEqual(30, dev_pot:balance(Addr2, S4)),
    % Set Addr1 to have 75% of the total deposits.
    S5 = dev_pot:deposit(Addr1, ResourceID, 20, S4, Opts),
    report(S5),
    % Calculate the expected balance for Addr1. At this step we mint 12 and have
    % 15 undistributed, and there are 40 total pot units. 27 div 40 = 0, and
    % we advance 27 undistributed.
    NewExpectedB1 = 30,
    S6 = dev_pot:drip(S5, #{ <<"t">> => 3 }, Opts),
    report(S6),
    ?assertEqual(NewExpectedB1, dev_pot:balance(Addr1, S6)),
    % Set both to be equal again.
    S7 = dev_pot:withdraw(Addr1, ResourceID, 20, S6, Opts),
    report(S7),
    Addr1BalPreFinal = dev_pot:balance(Addr1, S7),
    Addr2BalPreFinal = dev_pot:balance(Addr2, S7),
    S8 = dev_pot:drip(S7, #{ <<"t">> => 4 }, Opts),
    % Ensure that they were again minted equal quantities.
    Addr1Diff = dev_pot:balance(Addr1, S8) - Addr1BalPreFinal,
    Addr2Diff = dev_pot:balance(Addr2, S8) - Addr2BalPreFinal,
    ?assertEqual(Addr1Diff, Addr2Diff).

%% @doc Demonstrate minting using the proportional model and multiple resources.
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
        <<"mint-cap">> => 1000,
        <<"mint-prop">> => {1, 2},
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
    S1 = dev_pot:deposit(Addr1, ResourceID, 1, S0, Opts),
    S1b = dev_pot:deposit(Addr2, ResourceID, 1, S1, Opts),
    S2 = dev_pot:deposit(Addr1, ResourceID2, 1, S1b, Opts),
    S2b = dev_pot:deposit(Addr2, ResourceID2, 1, S2, Opts),
    {ok, S3} = hb_ao:resolve(S2b, <<"drip">>, Opts),
    report(S3),
    % There are 20 pot units at this step, and 500 units minted. 500 div 20 = 25.
    % Each user holds 10 pot units, so accumulated yield is 250 per user with
    % 0 undistributed carried forward.
    ?assertEqual(250, dev_pot:balance(Addr1, S3)),
    ?assertEqual(250, dev_pot:balance(Addr2, S3)),
    S4 = dev_pot:drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4),
    % 20 pot units at this step, 250 minted units to distribute.
    % 250 div 20 = 12. Each user accumulates +120 yield with 10 undistributed.
    ?assertEqual(370, dev_pot:balance(Addr1, S4)),
    ?assertEqual(370, dev_pot:balance(Addr2, S4)),
    % Set Addr1 to have 1/10 of the pot units
    S5a = dev_pot:withdraw(Addr1, ResourceID2, 1, S4, Opts),
    S5b = dev_pot:withdraw(Addr2, ResourceID, 1, S5a, Opts),
    % Calculate the expected balance for Addr1. We mint 125 (half of 250) at this
    % step plus 10. There are now 10 pot units. 135 div 10 = 13. Addr1 holds 1 pot
    % unit. They accumulate +13 yield, and there's 5 undistributed.
    NewExpectedB1 = 13 + 370,
    S6 = dev_pot:drip(S5b, #{ <<"t">> => 3 }, Opts),
    report(S6),
    ?assertEqual(NewExpectedB1, dev_pot:balance(Addr1, S6)),
    % Set both to be equal again.
    S7a = dev_pot:deposit(Addr1, ResourceID2, 1, S6, Opts),
    S7b = dev_pot:deposit(Addr2, ResourceID, 1, S7a, Opts),
    report(S7b),
    Addr1BalPreFinal = dev_pot:balance(Addr1, S7b),
    Addr2BalPreFinal = dev_pot:balance(Addr2, S7b),
    S8 = dev_pot:drip(S7b, #{ <<"t">> => 4 }, Opts),
    % Ensure that they were again minted equal quantities.
    Addr1Diff = dev_pot:balance(Addr1, S8) - Addr1BalPreFinal,
    Addr2Diff = dev_pot:balance(Addr2, S8) - Addr2BalPreFinal,
    ?assertEqual(Addr1Diff, Addr2Diff).

single_resource_modified_weight_test() ->
    Opts = #{},
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:deposit(<<"alice">>, <<"oxygen">>, 1, S0, Opts),
    % There's 1 pot unit and 50 minted, alice accumulates 50.
    {ok, S3} = hb_ao:resolve(S1, <<"drip">>, Opts),
    S4 = dev_pot:set_weight(<<"oxygen">>, 10, S3, Opts),
    report(S4),
    % There's 10 pot units and 25 minted, alice accumulates 20.
    {ok, S5} = hb_ao:resolve(S4, <<"drip">>, Opts),
    report(S5),
    ?assertEqual(70, dev_pot:balance(<<"alice">>, S5)),
    ok.

multiresource_modified_weight_test() ->
    Opts = #{},
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
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
    S1 = dev_pot:deposit(<<"alice">>, <<"oxygen">>, 1, S0, Opts),
    S2 = dev_pot:deposit(<<"bob">>, <<"hydrogen">>, 1, S1, Opts),
    {ok, S3} = hb_ao:resolve(S2, <<"drip">>, Opts),
    ?assertEqual(50, dev_pot:balance(<<"alice">>, S3)),
    ?assertEqual(0, dev_pot:balance(<<"bob">>, S3)),
    S4 = dev_pot:set_weight(<<"hydrogen">>, 1, S3, Opts),
    % 25 minted at this step, 2 pot units. 25 div 2 = 12.
    {ok, S5} = hb_ao:resolve(S4, <<"drip">>, Opts),
    ?assertEqual(62, dev_pot:balance(<<"alice">>, S5)),
    ?assertEqual(12, dev_pot:balance(<<"bob">>, S5)),
    ok.

simple_delegation_test() ->
    AddrAlice = <<"alice">>,
    AddrBob = <<"bob">>,
    ResourceHydrogen = <<"hydrogen">>,
    ResourceOxygen = <<"oxygen">>,
    Opts = #{},
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
        <<"resources">> => #{
            ResourceHydrogen => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 200,
                <<"deposits">> => #{ 
                    AddrAlice => #{
                        <<"quantity">> => 200,
                        <<"last-resource-accumulator">> => 0
                    },
                    AddrBob => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
                    }
                }
            },
            ResourceOxygen => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 50,
                <<"deposits">> => #{
                    AddrAlice => #{
                        <<"quantity">> => 25,
                        <<"last-resource-accumulator">> => 0
                    },
                    AddrBob => #{
                        <<"quantity">> => 25,
                        <<"last-resource-accumulator">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:delegate(AddrAlice, AddrBob, ResourceHydrogen, 20, S0, Opts),
    ?assertEqual(
        20,
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
        )
    ),
    ?assertEqual(
        180,
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
        )
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
    S2 = dev_pot:undelegate(AddrAlice, AddrBob, ResourceHydrogen, 10, S1, Opts),
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
    ?assertEqual(46, dev_pot:get_deposit(AddrAlice, ResourceOxygen, S3)),
    ?assertEqual(4, dev_pot:get_deposit(AddrBob, ResourceOxygen, S3)).

delegation_liquidation_test() ->
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 1,
                <<"deposits">> => #{ 
                    <<"alice">> => #{
                        <<"quantity">> => 1,
                        <<"last-resource-accumulator">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 1, S0, #{}),
    S2 = dev_pot:delegate(<<"bob">>, <<"charlie">>, <<"oxygen">>, 1, S1, #{}),
    report(S2),
    S3 = dev_pot:undelegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 1, S2, #{}),
    ?assertEqual(1, dev_pot:get_deposit(<<"alice">>, <<"oxygen">>, S3)),
    ?assertEqual(0, dev_pot:get_deposit(<<"bob">>, <<"oxygen">>, S3)),
    ?assertEqual(0, dev_pot:get_deposit(<<"charlie">>, <<"oxygen">>, S3)).

multiple_delegations_liquidation_test() ->
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 2,
                <<"deposits">> => #{ 
                    <<"alice">> => #{
                        <<"quantity">> => 2,
                        <<"last-resource-accumulator">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
                    },
                    <<"denis">> => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
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
    ?assertEqual(1, dev_pot:get_deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(1, dev_pot:get_deposit(<<"charlie">>, <<"oxygen">>, S4)),
    S5 = dev_pot:undelegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 2, S4, #{}),
    ?assertEqual(2, dev_pot:get_deposit(<<"alice">>, <<"oxygen">>, S5)),
    ?assertEqual(0, dev_pot:get_deposit(<<"bob">>, <<"oxygen">>, S5)),
    ?assertEqual(0, dev_pot:get_deposit(<<"charlie">>, <<"oxygen">>, S5)),
    ?assertEqual(0, dev_pot:get_deposit(<<"denis">>, <<"oxygen">>, S5)).

cyclic_delegations_test() ->
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 1,
                <<"deposits">> => #{ 
                    <<"alice">> => #{
                        <<"quantity">> => 1,
                        <<"last-resource-accumulator">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
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
    ?assertEqual(1, dev_pot:get_deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(0, dev_pot:get_deposit(<<"bob">>, <<"oxygen">>, S4)),
    report(S4),
    S5 = dev_pot:undelegate(<<"bob">>, <<"alice">>, <<"oxygen">>, 1, S4, #{}),
    report(S5),
    ?assertEqual(0, dev_pot:get_deposit(<<"alice">>, <<"oxygen">>, S5)),
    ?assertEqual(1, dev_pot:get_deposit(<<"bob">>, <<"oxygen">>, S5)).

deposit_removal_while_delegated_test() ->
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
        <<"resources">> => #{
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 3,
                <<"deposits">> => #{ 
                    <<"alice">> => #{
                        <<"quantity">> => 3,
                        <<"last-resource-accumulator">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"last-resource-accumulator">> => 0
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
    ?assertEqual(1, dev_pot:get_deposit(<<"alice">>, <<"oxygen">>, S3)),
    ?assertEqual(1, dev_pot:get_deposit(<<"bob">>, <<"oxygen">>, S3)),
    ?assertEqual(1, dev_pot:get_deposit(<<"charlie">>, <<"oxygen">>, S3)),
    S4 = dev_pot:withdraw(<<"alice">>, <<"oxygen">>, 3, S3, #{}),
    report(S4),
    ?assertEqual(0, dev_pot:get_deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(0, dev_pot:get_deposit(<<"bob">>, <<"oxygen">>, S4)),
    ?assertEqual(0, dev_pot:get_deposit(<<"charlie">>, <<"oxygen">>, S4)).

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
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
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
    S1 = dev_pot:deposit(AddrAlice, ResourceHydrogen, 5, S0, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 5 } },
            dev_pot:user(AddrAlice, S1, Opts),
            primary
        )
    ),
    S2 = dev_pot:deposit(AddrAlice, ResourceOxygen, 2, S1, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 5, ResourceOxygen => 2 } },
            dev_pot:user(AddrAlice, S2, Opts),
            primary
        )
    ),
    S3 = dev_pot:deposit(AddrBob, ResourceHydrogen, 777, S2, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 777 } },
            dev_pot:user(AddrBob, S3, Opts),
            primary
        )
    ),
    S4 = dev_pot:withdraw(AddrAlice, ResourceHydrogen, 4, S3, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 1, ResourceOxygen => 2 } },
            dev_pot:user(AddrAlice, S4, Opts),
            primary
        )
    ),
    S5 = dev_pot:withdraw(AddrAlice, ResourceHydrogen, 1, S4, Opts),
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
    S6 = dev_pot:withdraw(AddrBob, ResourceHydrogen, 777, S5, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{} },
            dev_pot:user(AddrBob, S6, Opts),
            primary
        )
    ).

mint_distribution_test() ->
    ResourceOxygen = <<"oxygen">>,
    AddrAlice = <<"alice">>,
    Opts = #{},
    S0 = #{
        <<"device">> => <<"pot@1.0">>,
        <<"t">> => 0,
        <<"last-drip">> => 0,
        <<"mint-cap">> => 100,
        <<"mint-prop">> => {1, 2},
        <<"total-weighted-units">> => 20,
        <<"resources">> => #{
            ResourceOxygen => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 20,
                <<"deposits">> => #{
                    AddrAlice => #{
                        <<"quantity">> => 20,
                        <<"last-resource-accumulator">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    % Tick 0: mint = 50, pot units = 20, accumulate 2 with an undistributed mint of 10
    S1 = dev_pot:drip(S0, #{ <<"t">> => 1 }, Opts),
    ?assertEqual(
        50,
        hb_maps:get(<<"minted">>, S1)
    ),
    ?assertEqual(
        10,
        hb_maps:get(<<"undistributed-mint">>, S1)
    ),
    ?assertEqual(
        2,
        hb_maps:get(<<"accumulator">>, S1)
    ),
    % Tick 1: mint = 25 + 10, pot units = 20, accumulate 1 with an undistributed mint of 15
    S2 = dev_pot:drip(S1, #{ <<"t">> => 2 }, Opts),
    ?assertEqual(
        75,
        hb_maps:get(<<"minted">>, S2)
    ),
    ?assertEqual(
        15,
        hb_maps:get(<<"undistributed-mint">>, S2)
    ),
    ?assertEqual(
        3,
        hb_maps:get(<<"accumulator">>, S2)
    ),
    % Tick 3: mint = 12 + 15, pot units = 20, accumulate 1 with an undistributed mint of 7
    S3 = dev_pot:drip(S2, #{ <<"t">> => 3 }, Opts),
    ?assertEqual(
        87,
        hb_maps:get(<<"minted">>, S3)
    ),
    ?assertEqual(
        7,
        hb_maps:get(<<"undistributed-mint">>, S3)
    ),
    ?assertEqual(
        4,
        hb_maps:get(<<"accumulator">>, S3)
    ),
    % Tick 4: mint = 6 + 7, pot units = 20, accumulate 0 with an undistributed mint of 13
    S4 = dev_pot:drip(S3, #{ <<"t">> => 4 }, Opts),
    ?assertEqual(
        93,
        hb_maps:get(<<"minted">>, S4)
    ),
    ?assertEqual(
        13,
        hb_maps:get(<<"undistributed-mint">>, S4)
    ),
    ?assertEqual(
        4,
        hb_maps:get(<<"accumulator">>, S4)
    ),
    % Tick 5: mint = 3 + 13, pot units = 20, accumulate 0 with an undistributed mint of 16
    S5 = dev_pot:drip(S4, #{ <<"t">> => 5 }, Opts),
    ?assertEqual(
        96,
        hb_maps:get(<<"minted">>, S5)
    ),
    ?assertEqual(
        16,
        hb_maps:get(<<"undistributed-mint">>, S5)
    ),
    ?assertEqual(
        4,
        hb_maps:get(<<"accumulator">>, S5)
    ),
    S6 = dev_pot:withdraw(AddrAlice, ResourceOxygen, 10, S5, Opts),
    % Tick 6: mint 2 + 16, pot units = 10, accumulate 1 with an undistributed mint of 8
    S7 = dev_pot:drip(S6, #{ <<"t">> => 6 }, Opts),
    ?assertEqual(
        98,
        hb_maps:get(<<"minted">>, S7)
    ),
    ?assertEqual(
        8,
        hb_maps:get(<<"undistributed-mint">>, S7)
    ),
    ?assertEqual(
        5,
        hb_maps:get(<<"accumulator">>, S7)
    ).

report(S) ->
    ?event(
        {report,
            {t, hb_maps:get(<<"t">>, S)},
            {last_drip, hb_maps:get(<<"last-drip">>, S)},
            {balances, dev_pot:balances(S)},
            {deposits, dev_pot:get_deposits(S)},
            {minted, hb_maps:get(<<"minted">>, S)},
            {state, S}
        }
    ).
