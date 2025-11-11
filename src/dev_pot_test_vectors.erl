%%% @doc Test vectors for the `~pot@1.0` device.
-module(dev_pot_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

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
    ?hr(),
    S3 = dev_pot:drip(S2, #{ <<"t">> => 1 }, Opts),
    report(S3),
    ?hr(),
    ?assertEqual(25.0, dev_pot:balance(Addr1, S3)),
    ?assertEqual(25.0, dev_pot:balance(Addr2, S3)),
    ?hr(),?hr(),
    S4 = dev_pot:drip(S3, #{ <<"t">> => 2 }, Opts),
    report(S4),
    ?assertEqual(37.5, dev_pot:balance(Addr1, S4)),
    ?assertEqual(37.5, dev_pot:balance(Addr2, S4)),
    % Set Addr1 to have 75% of the total deposits.
    S5 = dev_pot:modify_deposit(Addr1, ResourceID, 20, S4, Opts),
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

multiresource_modified_weight_test() ->
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
            <<"oxygen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            },
            <<"hydrogen">> => #{
                <<"weight">> => 1,
                <<"total-deposits">> => 0,
                <<"deposits">> => #{ }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = dev_pot:modify_deposit(<<"alice">>, <<"oxygen">>, 10, S0, Opts),
    S1b = dev_pot:modify_deposit(<<"bob">>, <<"oxygen">>, 10, S1, Opts),
    S2 = dev_pot:modify_deposit(<<"alice">>, <<"hydrogen">>, 10, S1b, Opts),
    S2b = dev_pot:modify_deposit(<<"bob">>, <<"hydrogen">>, 10, S2, Opts),
    {ok, S3} = hb_ao:resolve(S2b, <<"drip">>, Opts),
    report(S3),
    ?assertEqual(25.0, dev_pot:balance(<<"alice">>, S3)),
    ?assertEqual(25.0, dev_pot:balance(<<"bob">>, S3)),
    S4 = dev_pot:set_weight(<<"oxygen">>, 10, S3, Opts),
    {ok, S5} = hb_ao:resolve(S4, <<"drip">>, Opts),
    report(S5).

drip_test() ->
    ?assertEqual(50.0, dev_pot:units_minted_between(0, 100, 0.5, 0, 1)),
    ?assertEqual(75.0, dev_pot:units_minted_between(0, 100, 0.5, 0, 2)),
    ?assertEqual(87.5, dev_pot:units_minted_between(0, 100, 0.5, 0, 3)),
    Period1 = dev_pot:units_minted_between(0, 100, 0.5, 0, 2),
    Period2 = dev_pot:units_minted_between(Period1, 100, 0.5, 2, 3),
    ?assertEqual(87.5, Period1 + Period2).

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
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    AddrBob => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
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
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    AddrBob => #{
                        <<"quantity">> => 25,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
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
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
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
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    <<"denis">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
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
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
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
                        <<"minted-per-weighted-unit-at-deposit">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"minted-per-weighted-unit-at-deposit">> => 0
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