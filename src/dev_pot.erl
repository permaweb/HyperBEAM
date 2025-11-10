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
%%% /chi: Global meta-chi accumulator M used to derive effective per-resource chi.
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
%%% /tw: The total weighted deposits (sum over resources of weight * total-deposits).
%%% 
%%% TODO:
%%% - Add `secure-set` (set guarded by address) for resource-weights and 
%%%   supported resources.
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
                        <<"chi0">> => 0
                    },
                    AddrBob => #{
                        <<"quantity">> => 0,
                        <<"chi0">> => 0
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
                        <<"chi0">> => 0
                    },
                    AddrBob => #{
                        <<"quantity">> => 25,
                        <<"chi0">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = delegate(AddrAlice, AddrBob, ResourceHydrogen, 20, S0, Opts),
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
    S2 = delegate(AddrAlice, AddrBob, ResourceHydrogen, -10, S1, Opts),
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
    S3 = delegate(AddrBob, AddrAlice, ResourceOxygen, 21, S2, Opts),
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
    ?assertEqual(46, deposit(AddrAlice, ResourceOxygen, S3)),
    ?assertEqual(4, deposit(AddrBob, ResourceOxygen, S3)).

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
                        <<"chi0">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"chi0">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"chi0">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 1, S0, #{}),
    S2 = delegate(<<"bob">>, <<"charlie">>, <<"oxygen">>, 1, S1, #{}),
    report(S2),
    S3 = delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, -1, S2, #{}),
    ?assertEqual(1, deposit(<<"alice">>, <<"oxygen">>, S3)),
    ?assertEqual(0, deposit(<<"bob">>, <<"oxygen">>, S3)),
    ?assertEqual(0, deposit(<<"charlie">>, <<"oxygen">>, S3)).

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
                        <<"chi0">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"chi0">> => 0
                    },
                    <<"charlie">> => #{
                        <<"quantity">> => 0,
                        <<"chi0">> => 0
                    },
                    <<"denis">> => #{
                        <<"quantity">> => 0,
                        <<"chi0">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 2, S0, #{}),
    S2 = delegate(<<"bob">>, <<"charlie">>, <<"oxygen">>, 1, S1, #{}),
    S3 = delegate(<<"bob">>, <<"denis">>, <<"oxygen">>, 1, S2, #{}),
    S4 = delegate(<<"denis">>, <<"alice">>, <<"oxygen">>, 1, S3, #{}),
    report(S4),
    ?assertEqual(1, deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(1, deposit(<<"charlie">>, <<"oxygen">>, S4)),
    S5 = delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, -2, S4, #{}),
    ?assertEqual(2, deposit(<<"alice">>, <<"oxygen">>, S5)),
    ?assertEqual(0, deposit(<<"bob">>, <<"oxygen">>, S5)),
    ?assertEqual(0, deposit(<<"charlie">>, <<"oxygen">>, S5)),
    ?assertEqual(0, deposit(<<"denis">>, <<"oxygen">>, S5)).

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
                        <<"chi0">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"chi0">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 1, S0, #{}),
    S2 = delegate(<<"bob">>, <<"alice">>, <<"oxygen">>, 1, S1, #{}),
    S3 = delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 1, S2, #{}),
    S4 = delegate(<<"bob">>, <<"alice">>, <<"oxygen">>, 1, S3, #{}),
    ?assertEqual(1, deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(0, deposit(<<"bob">>, <<"oxygen">>, S4)),
    report(S4),
    S5 = delegate(<<"bob">>, <<"alice">>, <<"oxygen">>, -2, S4, #{}),
    report(S5),
    ?assertEqual(0, deposit(<<"alice">>, <<"oxygen">>, S5)),
    ?assertEqual(1, deposit(<<"bob">>, <<"oxygen">>, S5)).

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
                        <<"chi0">> => 0
                    },
                    <<"bob">> => #{
                        <<"quantity">> => 0,
                        <<"chi0">> => 0
                    }
                }
            }
        },
        <<"balances">> => #{ }
    },
    S1 = delegate(<<"alice">>, <<"bob">>, <<"oxygen">>, 3, S0, #{}),
    S2 = delegate(<<"bob">>, <<"charlie">>, <<"oxygen">>, 2, S1, #{}),
    S3 = delegate(<<"charlie">>, <<"alice">>, <<"oxygen">>, 1, S2, #{}),
    report(S1),
    ?assertEqual(1, deposit(<<"alice">>, <<"oxygen">>, S3)),
    ?assertEqual(1, deposit(<<"bob">>, <<"oxygen">>, S3)),
    ?assertEqual(1, deposit(<<"charlie">>, <<"oxygen">>, S3)),
    S4 = modify_deposit(<<"alice">>, <<"oxygen">>, -3, S3, #{}),
    report(S4),
    ?assertEqual(0, deposit(<<"alice">>, <<"oxygen">>, S4)),
    ?assertEqual(0, deposit(<<"bob">>, <<"oxygen">>, S4)),
    ?assertEqual(0, deposit(<<"charlie">>, <<"oxygen">>, S4)).

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
        <<"resources">> => #{ },
        <<"balances">> => #{ }
    },
    S1 = modify_deposit(AddrAlice, ResourceHydrogen, 5, S0, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 5 } },
            user(AddrAlice, S1, Opts),
            primary
        )
    ),
    S2 = modify_deposit(AddrAlice, ResourceOxygen, 2, S1, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 5, ResourceOxygen => 2 } },
            user(AddrAlice, S2, Opts),
            primary
        )
    ),
    S3 = modify_deposit(AddrBob, ResourceHydrogen, 777, S2, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 777 } },
            user(AddrBob, S3, Opts),
            primary
        )
    ),
    S4 = modify_deposit(AddrAlice, ResourceHydrogen, -4, S3, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 1, ResourceOxygen => 2 } },
            user(AddrAlice, S4, Opts),
            primary
        )
    ),
    S5 = modify_deposit(AddrAlice, ResourceHydrogen, -1, S4, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ <<"oxygen">> => 2 } },
            user(AddrAlice, S5, Opts),
            primary
        )
    ),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{ ResourceHydrogen => 777 } },
            user(AddrBob, S5, Opts),
            primary
        )
    ),
    S6 = modify_deposit(AddrBob, ResourceHydrogen, -777, S5, Opts),
    ?assert(
        hb_message:match(
            #{ <<"deposits">> => #{} },
            user(AddrBob, S6, Opts),
            primary
        )
    ).

report(S) ->
    ?event(
        {report,
            {t, hb_maps:get(<<"t">>, S)},
            {last_drip, hb_maps:get(<<"last-drip">>, S)},
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
        <<"mint-prop">> := Proportion
    }, Opts) ->
    Minted = hb_maps:get(<<"minted">>, S, 0, Opts),
    LastT = hb_maps:get(<<"last-drip">>, S, 0, Opts),
    Steps = max(T - LastT, 0),
    case Steps =:= 0 of
        true -> S;
        false ->
            ToMint = units_minted_between(Minted, Max, Proportion, LastT, T),
            TW = hb_maps:get(<<"tw">>, S),
            ?event({minting, {to_mint, ToMint}, {total_weight, TW}}),
            DeltaM = case TW of 0 -> 0; _ -> ToMint / TW end,
            M0 = hb_maps:get(<<"chi">>, S, 0, Opts),
            R = S#{
                <<"chi">> => M0 + DeltaM,
                <<"last-drip">> => T,
                <<"minted">> => Minted + ToMint
            },
            ?event({new_state, R}),
            R
    end.

units_minted_between(Minted, Max, Proportion, LastT, T) ->
    Steps = max(T - LastT, 0),
    Remaining = Max - Minted,
    Remaining * (1 - math:pow(1 - Proportion, Steps)).

modify_deposit(Addr, ResourceID, Amount, S0, Opts) ->
    S1 = drip(S0, Opts),
    #{ <<"balances">> := Balances0, <<"resources">> := Resources0 } = S1,
    ExistingDeposit = deposit(Addr, ResourceID, S1),
    NewDepositQty = ExistingDeposit + Amount,
    RealizedBalance = balance(Addr, S1),
    % Reset chi0 for this address across all resources to current chi
    Chi = hb_maps:get(<<"chi">>, S1, 0, Opts),
    NewResources =
        hb_maps:map(
            fun(XResID, Res) ->
                IsDepositRes = XResID =:= ResourceID,
                ResDeposits = hb_maps:get(<<"deposits">>, Res, #{}),
                Entry = hb_maps:get(Addr, ResDeposits, #{}, Opts),
                case IsDepositRes orelse not ?IS_EMPTY_MESSAGE(Entry) of
                    false -> Res;
                    true ->
                        ResWeight = hb_maps:get(<<"weight">>, Res, 0),
                        NewChi0 = ResWeight * Chi,
                        TotalDeposits = hb_maps:get(<<"total-deposits">>, Res, 0, Opts),
                        Res#{
                            <<"deposits">> =>
                                ResDeposits#{
                                    Addr =>
                                        Entry#{
                                            <<"chi0">> => NewChi0,
                                            <<"quantity">> =>
                                                if IsDepositRes -> NewDepositQty;
                                                true ->
                                                    hb_maps:get(<<"quantity">>, Entry, 0, Opts)
                                                end
                                        }
                                },
                            <<"total-deposits">> =>
                                TotalDeposits +
                                    if IsDepositRes -> Amount;
                                    true -> 0
                                    end
                        }
                end
            end,
            Resources0
        ),
    ?event({new_resources, NewResources}),
    WeightR = hb_ao:get(<<ResourceID/binary, "/weight">>, NewResources, 0, Opts),
    Tw0 = hb_maps:get(<<"tw">>, S1),
    S2 =
        S1#{
            <<"resources">> => NewResources,
            <<"tw">> => Tw0 + (WeightR * Amount),
            <<"balances">> => Balances0#{ Addr => RealizedBalance }
        },
    S3 =
        maybe_liquidate_delegations(
            Addr,
            ResourceID,
            S2,
            Opts
        ),
    set_user_deposit(Addr, ResourceID, deposit(Addr, ResourceID, S3), S3, Opts).

delegate(FromAddr, ToAddr, ResourceID, Amount, S, Opts) ->
    ?event(
        {delegating,
            {from_addr, FromAddr},
            {to_addr, ToAddr},
            {resource_id, ResourceID},
            {amount, Amount}
        }
    ),
    S0 = modify_deposit(FromAddr, ResourceID, -Amount, S, Opts),
    S1Unnormalized = modify_deposit(ToAddr, ResourceID, Amount, S0, Opts),
    S1 =
        maybe_liquidate_delegations(
            deposit(ToAddr, ResourceID, S1Unnormalized),
            ToAddr,
            ResourceID,
            S1Unnormalized,
            Opts
        ),
    ExistingQuantity =
        hb_ao:get(
            <<
                "/resources/", 
                ResourceID/binary, 
                "/deposits/", 
                FromAddr/binary, 
                "/delegations/", 
                ToAddr/binary
            >>,
            S1,
            0,
            Opts
        ),
    NewS1 =
        hb_ao:set(
            S1,
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                FromAddr/binary,
                "/delegations/",
                ToAddr/binary
            >>,
            ExistingQuantity + Amount,
            Opts
        ),
    maybe_liquidate_delegations(
        FromAddr,
        ResourceID,
        NewS1,
        Opts
    ).

%% @doc Recursively liquidate delegations as necessary until the deposit for
%% a delegating address is non-negative.
maybe_liquidate_delegations(Addr, ResourceID, S, Opts) ->
    maybe_liquidate_delegations(
        deposit(Addr, ResourceID, S),
        Addr,
        ResourceID,
        S,
        Opts
    ).
maybe_liquidate_delegations(Deposit, Addr, _Res, S, _Opts) when Deposit >= 0 ->
    ?event({no_liquidation_necessary, {deposit, Deposit}, {addr, Addr}}),
    S;
maybe_liquidate_delegations(Deposit, Addr, ResourceID, S, Opts) ->
    Overdraw = abs(Deposit),
    % Find the existing delegations for this address.
    ExistingDelegations =
        hb_ao:get(
            <<
                "/resources/", 
                ResourceID/binary, 
                "/deposits/", 
                Addr/binary, 
                "/delegations">>,
            S,
            #{},
            Opts
        ),
    % Determine the largest delegation to liquidate.
    LargestDelegation =
        lists:max(
            hb_maps:values(
                hb_private:reset(ExistingDelegations)
            )
        ),
    {LargestDelegationAddr, _} =
        lists:keyfind(
            LargestDelegation,
            2,
            hb_maps:to_list(ExistingDelegations)
        ),
    RevokeAmount = min(Overdraw, LargestDelegation),
    ?event(
        {liquidating_delegation,
            {addr, Addr},
            {overdrawn, Overdraw},
            {recouping, RevokeAmount},
            {largest_delegation, LargestDelegation},
            {delegated_to, LargestDelegationAddr}
        }
    ),
    % Revoke the largest delegation.
    NewS =
        delegate(
            Addr,
            LargestDelegationAddr,
            ResourceID,
            -RevokeAmount,
            S,
            Opts
        ),
    % Recursively liquidate the remaining quantity.
    maybe_liquidate_delegations(
        Deposit + RevokeAmount,
        Addr,
        ResourceID,
        NewS,
        Opts
    ).

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
    Chi = hb_maps:get(<<"chi">>, S, 0),
    Yield =
        lists:sum(
            lists:map(
                fun(Res) ->
                    ResW = hb_maps:get(<<"weight">>, Res, 0),
                    ChiEff = ResW * Chi,
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

user(Addr, S, Opts) ->
    hb_ao:get(<<"/users/", Addr/binary>>, S, #{}, Opts).

set_user_deposit(Addr, ResourceID, Quantity, S, Opts) ->
    Delegations =
        hb_ao:get(
            <<
                "/resources/",
                ResourceID/binary,
                "/deposits/",
                Addr/binary,
                "/delegations"
            >>,
            S,
            #{},
            Opts
        ),
    hb_ao:set(
        S,
        <<"users/", Addr/binary, "/deposits/", ResourceID/binary>>,
        if Quantity == 0 andalso ?IS_EMPTY_MESSAGE(Delegations) -> unset;
        true -> Quantity
        end,
        Opts
    ).

set_user_delegations(Addr, Delegations, S, Opts) ->
    U = user(Addr, S, Opts),
    NewU =
        U#{
            <<"delegations">> =>
                Delegations
        },
    hb_ao:set(
        S,
        <<"/users/", Addr/binary>>,
        NewU,
        Opts
    ).