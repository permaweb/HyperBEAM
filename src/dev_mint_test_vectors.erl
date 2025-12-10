%%% @doc Test utilities and vectors for the `~mint@1.0` device.
-module(dev_mint_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(AO_TO_ARMS(X), (trunc((X * 1_000_000_000_000)))).
-define(ARMS_TO_AO(X), X div 1_000_000_000_000).
-define(DEFAULT_MAX_WEIGHT, 10_000_000).
-define(DEFAULT_TOLERANCE_ARMS, 100_000_000_000).

%%% Test helpers.

%% @doc Mint a given number of cycles, given either a parameter map or a state.
mint(Params) ->
    mint(Params, 1).
mint(Params, Cycles) ->
    mint(Params, Cycles, #{}).
mint(Params, Cycles, Opts) when not is_map_key(<<"device">>, Params) ->
    ?event(debug_test, {generating_state, {params, Params}}),
    State = generate_state(Params, Opts),
    ?event(debug_test, {generated_state, {bytes, erlang:external_size(State)}}),
    mint(State, Cycles, Opts);
mint(State, 0, _Opts) -> State;
mint(State, CyclesRemaining, Opts) ->
    {ok, NewS} = dev_mint_math:mint(State, #{}, Opts),
    mint(NewS, CyclesRemaining - 1, Opts).

within_tolerance(Expected, Actual) ->
    within_tolerance(Expected, Actual, ?DEFAULT_TOLERANCE_ARMS).
within_tolerance(Expected, Actual, Tolerance) ->
    abs(Expected - Actual) =< Tolerance.

%% @doc Return random ID(s) in human-readable (43-character) format.
random_id() -> hb_util:human_id(crypto:strong_rand_bytes(32)).
random_ids(N) -> [ random_id() || _ <- lists:seq(1, N) ].

%% @doc Generate a random resource with a random weight and oracle ID.
generate_resource(ID, undefined) ->
    generate_resource(ID, rand:uniform(?DEFAULT_MAX_WEIGHT));
generate_resource(ID, Weight) when is_integer(Weight) ->
    generate_resource(ID, #{ <<"weight">> => Weight });
generate_resource(ID, Res) ->
    {
        ID,
        Res#{
            <<"weight">> =>
                hb_maps:get(
                    <<"weight">>,
                    Res
                ),
            <<"oracle">> =>
                hb_maps:get(<<"oracle">>, Res, random_id())
        }
    }.

%% @doc Generate a set of resources given a specification, either in the form of
%% specific resources to employ, or a number of resources to generate, or a
%% list of resource IDs.
generate_resources(N) ->
    generate_resources(N, undefined).
generate_resources(NumResources, GlobalSpec) when is_integer(NumResources) ->
    generate_resources(random_ids(NumResources), GlobalSpec);
generate_resources(ResourceIDs, GlobalSpec) when is_list(ResourceIDs) ->
    generate_resources(
        hb_maps:from_list([ {ID, GlobalSpec} || ID <- ResourceIDs ]),
        undefined
    );
generate_resources(Resources, GlobalSpec) when is_map(Resources) ->
    hb_maps:map(
        fun(ID, ResourceSpec) ->
            element(2, generate_resource(
                ID,
                if is_map(ResourceSpec) andalso is_map(GlobalSpec) ->
                    hb_maps:merge(GlobalSpec, ResourceSpec);
                ResourceSpec =/= undefined ->
                    ResourceSpec;
                true ->
                    GlobalSpec
                end
            ))
        end,
        Resources
    ).

%% @doc Normalize account specifications into a per-resource map.
%% Handles three input formats:
%% 1. undefined: Generate random accounts for each resource
%% 2. List of account IDs: Use same accounts for all resources
%% 3. Map: Either per-resource #{ResID => Accounts} or global #{AccID => Details}
normalize_accounts_for_resources(undefined, Resources, DefaultCount, _Opts) ->
    DefaultAccounts = random_ids(DefaultCount),
    hb_maps:map(fun(_, _) -> DefaultAccounts end, Resources);
normalize_accounts_for_resources(Accounts, Resources, _DefaultCount, _Opts)
    when is_list(Accounts) ->
    hb_maps:map(fun(_, _) -> Accounts end, Resources);
normalize_accounts_for_resources(AccountMap, Resources, _DefaultCount, Opts)
    when is_map(AccountMap) ->
    ResourceIDs = maps:keys(Resources),
    IsPerResource = lists:any(
        fun(ResID) -> maps:is_key(ResID, AccountMap) end,
        ResourceIDs
    ),
    if IsPerResource ->
        AccountMap;
    true ->
        hb_maps:map(fun(_, _) -> AccountMap end, Resources, Opts)
    end.

%% @doc For each resource, generate a set of balances for each account.
%% AccountsPerResource must be in normalized format: #{ResourceID => Accounts}.
generate_all_balances(Resources, AccountsPerResource, Opts) ->
    hb_maps:map(
        fun(ResourceID, _ResourceDetails) ->
            Accounts = maps:get(ResourceID, AccountsPerResource),
            generate_balances(Accounts, 1_000_000_000, Opts)
        end,
        Resources
    ).

%% @doc Generate a set of initial balances for a given set or number of accounts.
%% The resulting message may be used as a sub-message of the top-level balances
%% message, attached to a specific resource ID. For example,
%% `balances/resourceID/accountID`.
generate_balances(AccountIDs, Max, Opts) when is_list(AccountIDs) ->
    generate_balances(
        hb_maps:from_list([ {ID, #{}} || ID <- AccountIDs ]),
        Max,
        Opts
    );
generate_balances(Accounts, Max, Opts) when is_map(Accounts) ->
    hb_util:ok(hb_ao:resolve(
        #{ <<"device">> => <<"trie@1.0">> },
        (hb_maps:map(
            fun(AccountID, Account) ->
                Account#{
                    <<"quantity">> =>
                        hb_maps:get(
                            <<"quantity">>,
                            Account,
                            ?AO_TO_ARMS(rand:uniform(Max)),
                            Opts
                        ),
                    <<"recipient">> =>
                        hb_maps:get(
                            <<"recipient">>,
                            Account,
                            AccountID,
                            Opts
                        )
                }
            end,
            Accounts
        ))#{ <<"path">> => <<"set">> },
        Opts
    )).

%% @doc Generate an initial state for `~mint@1.0` execution with randomized
%% parameters within given bounds. The `Params` map is used to override the
%% default generated values:
%% - `resources`: The resources to use for the initial state. Either a list of
%%   resource IDs or a map of resource IDs to resource definition messages.
%% - `balances`: The balances for each resource.
%% - `resource_count`: The number of resources to generate.
%% - `account_count`: The number of accounts to generate for each resource. This
%%   will result in `account * resources` total accounts.
%% - `max_resource_weight`: The maximum weight to generate for each resource.
%% - `max_balance`: The maximum balance to generate for each account.
%% - `client`: The client ID that should be the target of mint messages.
%% - `total_supply`: The total supply of the token.
%% - `cycle_proportion`: The proportion of the total supply to mint each cycle.
%% - `cycle_proportion_denominator`: The denominator of the cycle proportion.
%% By default, the returned state will mint 100% of its total supply of 1 token
%% in a single cycle.
generate_state(Params, Opts) ->
    Resources =
        generate_resources(
            maps:get(
                resources,
                Params,
                maps:get(resource_count, Params, 2)
            )
        ),
    % Generate or use explicit balances
    Balances =
        case maps:find(balances, Params) of
            {ok, ExplicitBalances} ->
                ExplicitBalances;
            error ->
                % Generate balances from account specifications
                % Normalize account specifications to per-resource format
                AccountsPerResource =
                    normalize_accounts_for_resources(
                        maps:get(accounts, Params, undefined),
                        Resources,
                        maps:get(account_count, Params, 10),
                        Opts
                    ),
                generate_all_balances(Resources, AccountsPerResource, Opts)
        end,
    ?event(generating_state, { balances, Balances}),
    #{
        <<"device">> => <<"mint@1.0">>,
        <<"cycle">> => maps:get(cycle, Params, 0),
        <<"cycle-proportion">> =>
            maps:get(cycle_proportion, Params, 1),
        <<"cycle-proportion-denominator">> =>
            maps:get(cycle_proportion_denominator, Params, 1),
        <<"mint-total">> =>
            ?AO_TO_ARMS(maps:get(total_supply, Params, 1_000_000_000)),
        <<"minted">> => ?AO_TO_ARMS(maps:get(minted, Params, 0)),
        <<"client">> => maps:get(client, Params, random_id()),
        <<"resources">> => Resources,
        <<"balances">> => Balances,
        <<"total-supply">> =>
            ?AO_TO_ARMS(maps:get(total_supply, Params, 1_000_000_000)),
        <<"period">> => 1000,
        <<"start-time">> => os:system_time(millisecond)
    }.

%% @doc Find a distribution for a specific recipient.
find_distribution(Distributions, Recipient) ->
    case [D || D <- Distributions, maps:get(<<"recipient">>, D) == Recipient] of
        [Dist] -> Dist;
        [] -> undefined;
        Multiple -> hd(Multiple)
    end.

%% @doc Get all recipients from distributions.
get_recipients(State) ->
    [maps:get(<<"recipient">>, D) || D <- dev_mint_stats:get_distributions(State)].

%% @doc Find a distribution and extract its quantity.
find_quantity(State, Recipient) ->
    Dist = find_distribution(dev_mint_stats:get_distributions(State), Recipient),
    maps:get(<<"quantity">>, Dist).

%%% Tests

single_account_test() ->
    hb:init(),
    State =
        mint(
            #{
                resources => [random_id()],
                accounts => [Acc = random_id()],
                total_supply => 1
            }
        ),
    ?event(debug_test, {after_mint, State}),
    Expected = ?AO_TO_ARMS(1),
    ?assertMatch(
        #{
            <<"quantity">> := Expected,
            <<"recipient">> := Acc
        },
        hb_ao:get(<<"results/distributions/1">>, State, #{})
    ).

multiple_equal_accounts_test() ->
    hb:init(),
    ResID = random_id(),
    State =
        mint(
            #{
                resources => [ResID],
                accounts =>
                    #{
                        ResID => #{
                            Acc1 = random_id() => #{
                                <<"quantity">> => ?AO_TO_ARMS(1)
                            },
                            Acc2 = random_id() => #{
                                <<"quantity">> => ?AO_TO_ARMS(1)
                            }
                        }
                    },
                total_supply => 2
            }
        ),
    ?event(debug_test, {after_mint, State}),
    ?assert(lists:member(Acc1, get_recipients(State))),
    ?assert(lists:member(Acc2, get_recipients(State))),
    ?assertEqual(
        [?AO_TO_ARMS(1), ?AO_TO_ARMS(1)], 
        lists:sort(dev_mint_stats:get_quantities(State))
    ).

%% Test: Multiple resources with weighted distribution (70/30 split).
multiple_resources_test() ->
    hb:init(),
    Res1 = random_id(),
    Res2 = random_id(),
    Acc1 = random_id(),
    Acc2 = random_id(),
    State =
        mint(
            #{
                resources =>
                    #{
                        Res1 => #{<<"weight">> => 70},
                        Res2 => #{<<"weight">> => 30}
                    },
                accounts =>
                    #{
                        Res1 => #{Acc1 => #{<<"quantity">> => ?AO_TO_ARMS(1)}},
                        Res2 => #{Acc2 => #{<<"quantity">> => ?AO_TO_ARMS(1)}}
                    },
                total_supply => 100
            }
        ),
    ?assertEqual(?AO_TO_ARMS(70), find_quantity(State, Acc1)),
    ?assertEqual(?AO_TO_ARMS(30), find_quantity(State, Acc2)).

%% Test: Unequal balance distribution (70/30 split within one resource).
unequal_balances_test() ->
    hb:init(),
    ResID = random_id(),
    Acc1 = random_id(),
    Acc2 = random_id(),
    State =
        mint(
            #{
                resources => [ResID],
                accounts =>
                    #{
                        ResID => #{
                            Acc1 => #{<<"quantity">> => ?AO_TO_ARMS(70)},
                            Acc2 => #{<<"quantity">> => ?AO_TO_ARMS(30)}}
                    },
                total_supply => 100
            }
        ),
    ?assertEqual(
        [?AO_TO_ARMS(70), ?AO_TO_ARMS(30)], 
        [find_quantity(State, Acc1), find_quantity(State, Acc2)]
    ).

%% Test: Dust handling with 3-way split of 10 atomic units.
dust_handling_test() ->
    hb:init(),
    ResID = random_id(),
    Acc1 = random_id(),
    Acc2 = random_id(),
    Acc3 = random_id(),
    State =
        mint(
            #{
                resources => [ResID],
                accounts =>
                    #{
                        ResID => #{
                            Acc1 => #{<<"quantity">> => 1},
                            Acc2 => #{<<"quantity">> => 1},
                            Acc3 => #{<<"quantity">> => 1}
                        }
                    },
                total_supply => 10
            }
        ),
    TotalAllocated = lists:sum(dev_mint_stats:get_quantities(State)),
    Minted = hb_ao:get(<<"minted">>, State, #{}),
    ?assertEqual(TotalAllocated, Minted),
    ?assert(TotalAllocated =< ?AO_TO_ARMS(10)).

%% Test: Complex scenario with 2 resources, 4 accounts, varied weights and
%% balances.
complex_distribution_test() ->
    hb:init(),
    ResA = random_id(),
    ResB = random_id(),
    Alice = random_id(),
    Bob = random_id(),
    Charlie = random_id(),
    Dave = random_id(),
    State =
        mint(
            #{
                resources =>
                    #{
                        ResA => #{<<"weight">> => 60},
                        ResB => #{<<"weight">> => 40}
                    },
                accounts =>
                    #{
                        ResA => #{
                            Alice => #{<<"quantity">> => ?AO_TO_ARMS(70)},
                            Bob => #{<<"quantity">> => ?AO_TO_ARMS(30)}
                        },
                        ResB => #{
                            Charlie => #{<<"quantity">> => ?AO_TO_ARMS(50)},
                            Dave => #{<<"quantity">> => ?AO_TO_ARMS(50)}
                        }
                    },
                total_supply => 1000
            }
        ),
    ?assertEqual(?AO_TO_ARMS(420), find_quantity(State, Alice)),
    ?assertEqual(?AO_TO_ARMS(180), find_quantity(State, Bob)),
    ?assertEqual(?AO_TO_ARMS(200), find_quantity(State, Charlie)),
    ?assertEqual(?AO_TO_ARMS(200), find_quantity(State, Dave)).

%% Test: Zero balance account receives nothing.
zero_balance_test() ->
    hb:init(),
    ResID = random_id(),
    Acc1 = random_id(),
    Acc2 = random_id(),
    State =
        mint(
            #{
                resources => [ResID],
                accounts =>
                    #{
                        ResID => #{
                            Acc1 => #{ <<"quantity">> => ?AO_TO_ARMS(100) },
                            Acc2 => #{ <<"quantity">> => 0 }
                        }
                    },
                total_supply => 100
            }
        ),
    ?assertEqual(?AO_TO_ARMS(100), find_quantity(State, Acc1)),
    ?assertEqual(?AO_TO_ARMS(0), find_quantity(State, Acc2)).

%% Test: Single token unit per account.
single_unit_test() ->
    hb:init(),
    ResID = random_id(),
    Accounts = [{random_id(), 1} || _ <- lists:seq(1, 10)],
    State =
        mint(
            #{
                resources => [ResID],
                accounts =>
                    #{
                        ResID =>
                            maps:from_list(
                                [
                                    {Acc, #{ <<"quantity">> => Qty }}
                                ||
                                    {Acc, Qty} <- Accounts
                                ]
                            )
                    },
                total_supply => 11
            }
        ),
    ?assert(
        lists:all(
            fun(Qty) -> Qty == ?AO_TO_ARMS(1.1) end,
            dev_mint_stats:get_quantities(State)
        )
    ),
    ?assert(lists:sum(dev_mint_stats:get_quantities(State)) == ?AO_TO_ARMS(11)).

%% Test: Max supply boundary - nothing left to mint
max_supply_test() ->
    hb:init(),
    TotalSupply = 1000,
    Acc = random_id(),
    State1 =
        mint(
            #{
                resources => [random_id()],
                accounts => [Acc],
                total_supply => TotalSupply,
                minted => 0
            }
        ),
    ?assertEqual(?AO_TO_ARMS(TotalSupply), hb_ao:get(<<"minted">>, State1, #{})),
    State2 = mint(State1),
    Minted2 = hb_ao:get(<<"minted">>, State2, #{}),
    ?assertEqual(?AO_TO_ARMS(TotalSupply), Minted2),
    ?assert(Minted2 =< ?AO_TO_ARMS(TotalSupply)).

supply_to_mint_test_() ->
    {timeout, 600, fun supply_to_mint/0}.
supply_to_mint() ->
    hb:init(),
    Num = 1,
    Den = 2048, % Emit 1/(2^11) of the remaining supply in each cycle.
    TotalSupply = 21_000_000,  % in tokens; will be converted via ?AO_TO_ARMS/1
    Tolerance = ?AO_TO_ARMS(1/1_000_000_000), % Use 1 billionth of a token as tolerance.
    Years = 5, % The number of years worth of mint cycles to test.
    Params =
        #{
            resources => [random_id()],
            account_count => 1,
            total_supply => TotalSupply,
            cycle_proportion => Num,
            cycle_proportion_denominator => Den
        },
    % Initialize state once (no cycles yet).
    State0 = mint(Params, 0),
    TotalUnits = ?AO_TO_ARMS(TotalSupply),
    RemainingUnits0 = TotalUnits,
    Minted0 = hb_ao:get(<<"minted">>, State0, #{}),
    % Check per-day minted and cumulative minted for the specified number of years.
    lists:foldl(
        fun(_Day, {S, RemainingUnits, MintedPrev}) ->
            NewState = mint(S, 1),
            MintedNew = hb_ao:get(<<"minted">>, NewState, #{}),
            DailyActual = MintedNew - MintedPrev,
            DailyExpected = RemainingUnits div Den,
            NewRemainingUnits = RemainingUnits - DailyExpected,
            MintedExpected = TotalUnits - NewRemainingUnits,
            ?assert(within_tolerance(DailyExpected, DailyActual, Tolerance)),
            ?assert(within_tolerance(MintedExpected, MintedNew, Tolerance)),
            {NewState, NewRemainingUnits, MintedNew}
        end,
        {State0, RemainingUnits0, Minted0},
        lists:seq(1, 365 * Years)
    ),
    ok.