%%% @doc Test vectors and benchmarks for the `~token@1.0` device.
-module(dev_token_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(OWNER, <<"test-owner-id">>).
-define(MINTER, <<"test-minter-id">>).
-define(ALICE, <<"alice-id">>).
-define(BOB, <<"bob-id">>).
-define(CHARLIE, <<"charlie-id">>).

%%% Test Utilities
%% @doc Generate generic isolated node messages for testing.
test_opts() ->
    hb:init(),
    #{
        priv_wallet => ar_wallet:new(),
        store => [hb_test_utils:test_store()]
    }.

%% @doc Generate a random ID, or an 'ID' value of the correct length starting
%% with the given binary and padded with zeros.
id() -> hb_util:human_id(crypto:strong_rand_bytes(32)).
id(Bin) when is_binary(Bin) ->
    BitSize = byte_size(Bin) * 8,
    Suffix = << 0:(256 - BitSize) >>,
    << Bin/binary, Suffix/binary >>;
id(Other) -> hb_util:human_id(Other).

%% @doc Generate a base token state with default configuration.
generate_token_base_state(Opts) ->
    generate_token_base_state(#{}, Opts).
generate_token_base_state(Params, Opts) ->
    ?event({generate_token_base_state, {params, Params}}),
    DefaultBalances = #{ <<"device">> => <<"trie@1.0">> },
    InitialBalances = maps:get(initial_balances, Params, #{}),
    Total =
        case maps:find(total_supply, Params) of
            {ok, TotalSupply} -> TotalSupply;
            error -> lists:sum(hb_maps:values(InitialBalances, Opts))
        end,
    ?event({initial_supply, {total, Total}, {balances, InitialBalances}}),
    {ok, Balances} =
        hb_ao:resolve(
            DefaultBalances,
            InitialBalances#{
                <<"path">> => <<"set">>
            },
            Opts
        ),
    DefaultState = #{
        <<"device">> => <<"token@1.0">>,
        <<"owner">> => maps:get(owner, Params, ?OWNER),
        <<"mint-authority">> => maps:get(mint_authority, Params, ?MINTER),
        <<"name">> => <<"Test Token">>,
        <<"ticker">> => <<"TEST">>,
        <<"denomination">> => 12,
        <<"total-supply">> => Total,
        <<"balances">> => Balances
    },
    FinalState = maps:merge(DefaultState, maps:get(extra, Params, #{})),
    ?event({final_state_generated, {balances_size, map_size(Balances)}}),
    FinalState.

generate_process_state(BaseState, Opts) ->
    Addr = id(hb_opts:get(priv_wallet, no_wallet, Opts)),
    ?event({process_state, {base, hb_message:commit(BaseState, Opts)}}),
    Base =
        BaseState#{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"execution-device">> => <<"token@1.0">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"push-device">> => <<"push@1.0">>,
            <<"scheduler">> => Addr,
            <<"authority">> => Addr
        },
    hb_message:commit(Base, Opts).

%% @doc Return a signed token process with a `pot@1.0` mint device.
generate_pot_token_process_state(PotFields, TokenFields, Opts) ->
    PotBase = generate_pot_fields(PotFields, Opts),
    ?event({pot_base, PotBase}),
    TokenBase = 
        generate_token_base_state(
            TokenFields#{
                extra => PotBase},
            Opts
        ),
    ?event({token_base, TokenBase}),
    generate_process_state(TokenBase, Opts).

%% @doc Create a request message.
generate_assignment(Action, Body, From) ->
    Req = #{
        <<"path">> => <<"compute">>,
        <<"body">> => Body#{ <<"from">> => From, <<"action">> => Action }
    },
    ?event({make_request, {action, Action}, {from, From}}),
    Req.

schedule_request(State, Action, Body, Wallet, Opts) ->
    From = id(Wallet),
    ?event({scheduling_request, {action, Action}, {from, From}}),
    Signed =
        hb_message:commit(
            Body#{ <<"from">> => From, <<"action">> => Action },
            Opts#{ priv_wallet => Wallet }
        ),
    ?event({signed_request, Signed}),
    Req =
        Signed#{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>
        },
    ?event({scheduling_request, Req}),
    {ok, Res} = hb_ao:resolve(State, Req, Opts),
    ?event({schedule_result, Res}),
    Res#{ <<"timestamp">> => 1 }.

%% @doc Get balance for an account.
get_balance(State, Account) ->
    get_balance(State, Account, #{}).
get_balance(State, Account, Opts) when is_binary(Account) ->
    get_balance(State, #{ <<"balance">> => Account }, Opts);
get_balance(State, Req, Opts) ->
    Res =
        case hb_maps:get(<<"device">>, State, <<"token@1.0">>, #{}) of
            <<"token@1.0">> ->
                hb_ao:resolve(
                    State,
                    Req#{ <<"path">> => <<"balance">> },
                    Opts
                );
            _ ->
                hb_ao:resolve_many(
                    [
                        State,
                        #{
                            <<"path">> => <<"as">>,
                            <<"as">> => <<"execution">>
                        },
                        Req#{ <<"path">> => <<"balance">> }
                    ],
                    Opts
                )
        end,
    case Res of
        {ok, B} -> B;
        {error, not_found} -> 0
    end.

%%% Integration Test Helpers (Token + Pot)

%% @doc Generate pot fields for integration testing
%% Returns a map of pot state fields that can be merged into token state
%% Note: Only includes initial fields. Dynamic fields (minted, accumulator,
%% undistributed-mint) are created by drip_global()
generate_pot_fields(Params, Opts) ->
    MintCap = hb_maps:get(mint_cap, Params, 10000, Opts),
    MintPropN = hb_maps:get(mint_prop_numerator, Params, 1, Opts),
    MintPropD = hb_maps:get(mint_prop_denominator, Params, 2, Opts),
    T = hb_maps:get(t, Params, 0, Opts),
    LastDrip = hb_maps:get(last_drip, Params, 0, Opts),
    ?event({generate_pot_fields, {params, Params}}),
    #{
        <<"mint-device">> => <<"pot@1.0">>,
        <<"mint-cap">> => MintCap,
        <<"mint-prop-numerator">> => MintPropN,
        <<"mint-prop-denominator">> => MintPropD,
        <<"total-weighted-units">> => 0,
        <<"resources">> => #{},
        <<"t">> => T,
        <<"last-drip">> => LastDrip
    }.

%% @doc Helper to create a pot resource with deposits
%% Example: pot_resource(100, [{?ALICE, 10}, {?BOB, 5}])
pot_resource(Weight, UserDeposits) when is_list(UserDeposits) ->
    pot_resource(Weight, hb_maps:from_list(UserDeposits));
pot_resource(Weight, RawDeposits) when is_map(RawDeposits) ->
    Deposits =
        #{
            User => #{
                <<"quantity">> => Qty,
                <<"last-resource-accumulator">> => 0
            }
        ||
            User := Qty <- RawDeposits
        },
    TotalDeposits =
        lists:sum(
            [ Qty || _ := #{ <<"quantity">> := Qty } <- Deposits ]
        ),
    #{
        <<"weight">> => Weight,
        <<"accumulator">> => 0,
        <<"last-global-accumulator">> => 0,
        <<"total-deposits">> => TotalDeposits,
        <<"deposits">> => Deposits
    }.
pot_deposit_resource({Resource, Weight, UserDeposits}, Base, Opts) ->
    PotWeightSet = dev_pot:set_weight(Resource, Weight, Base, Opts),
    ?event({weight_set, PotWeightSet}),
    lists:foldl(
        fun({Addr, Qty}, PotAcc) ->
            dev_pot:deposit(Addr, Resource, Qty, PotAcc, Opts)
        end,
        PotWeightSet,
        UserDeposits
    ).

%% @doc Generate integrated token+pot state
generate_integrated_state(Params, Opts) ->
    PotParams =
        hb_maps:with(
            [mint_cap, mint_prop, t, last_drip, resources],
            Params,
            Opts
        ),
    PotFields = generate_pot_fields(PotParams, Opts),
    % Generate base token state with pot fields merged in
    ExistingExtra = hb_maps:get(extra, Params, #{}, Opts),
    MergedExtra = hb_maps:merge(ExistingExtra, PotFields, Opts),
    generate_token_base_state(Params#{ extra => MergedExtra }, Opts).

simple_process_test() ->
    hb:init(),
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = id(BobWallet),
    Base =
        generate_process_state(
            generate_token_base_state(
                #{
                    initial_balances =>
                        #{ AliceAddr => 1_000_000_000 }
                },
                Opts
            ),
            Opts
        ),
    ?event({base_state, Base}),
    SchedRes =
        schedule_request(
            Base,
            <<"transfer">>,
            #{
                <<"recipient">> => BobAddr,
                <<"quantity">> => 1
            },
            AliceWallet,
            Opts
        ),
    {ok, State} =
        hb_ao:resolve(
            Base,
            #{
                <<"path">> => <<"compute">>,
                <<"slot">> =>
                    hb_maps:get(
                        <<"slot">>,
                        SchedRes,
                        none,
                        Opts
                    )
            },
            Opts
        ),
    ?assertEqual(999_999_999, get_balance(State, AliceAddr)),
    ?assertEqual(1, get_balance(State, BobAddr)),
    ?assertEqual(1_000_000_000, hb_ao:get(<<"total-supply">>, State, Opts)).

%% @doc Basic test to see what happens when transfer is called with mint-device=pot
simple_pot_process_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = id(BobWallet),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2
    },
    TokenFields = #{
        initial_balances => #{ AliceAddr => 1000 },
        total_supply => 1000
    },
    Base = generate_pot_token_process_state(PotFields, TokenFields, Opts),
    ?event({base_state, Base}),
    NewBase = 
        pot_deposit_resource(
            {ResourceOxygen, 100, [{AliceAddr, 10}]}, 
            Base, 
            Opts
        ),
    MintSchedRes =
        schedule_request(
            NewBase,
            <<"mint">>,
            #{
                <<"from">> => AliceAddr
            },
            AliceWallet,
            Opts
        ),
    ?event({mint_sched_res, MintSchedRes}),
    TransferSchedRes =
        schedule_request(
            NewBase,
            <<"transfer">>,
            #{
                <<"recipient">> => BobAddr,
                <<"quantity">> => 1
            },
            AliceWallet,
            Opts
        ),
    ?event({transfer_sched_res, TransferSchedRes}),
    {ok, State} =
        hb_ao:resolve(
            NewBase,
            #{
                <<"path">> => <<"compute">>,
                <<"slot">> => 1
            },
            Opts
        ),
    ?event(debug_test, {state, State}, Opts),
    ?assertEqual(1, get_balance(State, BobAddr, Opts)),
    ?assertEqual(999_999_999, get_balance(State, AliceAddr, Opts)),
    ?assertEqual(1_000_000_000, hb_ao:get(<<"total-supply">>, State, Opts)).

%% @doc Test that transfer works when balance is insufficient but 
%% balance + unclaimed_yield is sufficient
%% This validates that normalize_mint properly claims yields before transfer
transfer_with_unclaimed_yield_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = id(BobWallet),
    ResourceOxygen = <<"oxygen">>,
    % Alice has 500 tokens in balance
    % Alice has deposits in pot that will yield tokens
    % Alice wants to transfer 700 tokens
    % Should succeed because: balance + yield > 700
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{AliceAddr => 500},
        total_supply => 500
    },
    Base = generate_pot_token_process_state(PotFields, TokenFields, Opts),
    NewBase = 
        pot_deposit_resource(
            {ResourceOxygen, 100, [{AliceAddr, 10}]}, 
            Base, 
            Opts
        ),
    ?event({initial_state, Base}),
    % Advance time to generate yield
    % With mint_cap=10000, mint_prop={1,2}, going from t=0 to t=1:
    % ToMint = 10000 * (2^1 - 1^1) / 2^1 = 10000 * 1 / 2 = 5000
    % GlobalAcc = 0 + (5000 / 1000) = 5 (per weighted unit)
    % ResourceAcc = 0 + (5 * 100) = 500
    % Alice's yield = (500 - 0) * 10 = 5000 tokens!
    Balance =
        get_balance(
            NewBase,
            #{ <<"balance">> => AliceAddr, <<"timestamp">> => 1 },
            Opts
        ),
    ?assertEqual(5500, Balance),
    ?event({alice_balance, Balance}),
    % % Try to transfer 700 tokens
    % % Should fail without normalize_mint (500 < 700)
    % % Should succeed with normalize_mint (500 + 5000 = 5500 > 700)
    TransferSchedRes =
        schedule_request(
            NewBase,
            <<"transfer">>,
            #{
                <<"timestamp">> => 1,
                <<"recipient">> => BobAddr,
                <<"quantity">> => 700
            },
            AliceWallet,
            Opts
        ),
    ?event({transfer_sched_res, TransferSchedRes}),
    {ok, Result} =
        hb_ao:resolve(
            NewBase,
            #{
                <<"path">> => <<"compute">>,
                <<"slot">> => 0
            },
            Opts
        ),
    ?event({transfer_result, Result}),
    AliceBalance = get_balance(Result, ?ALICE),
    BobBalance = get_balance(Result, ?BOB),
    ?event({final_balances, {alice, AliceBalance}, {bob, BobBalance}}),
    % Alice should have: (500 + 5000) - 700 = 4800
    % Bob should have: 700
    ?assertEqual(4800, AliceBalance),
    ?assertEqual(700, BobBalance),
    % Total supply should be updated
    % Initial: 500, Minted: 5000, New total: 5500
    ?assertEqual(5500, hb_ao:get(<<"total-supply">>, Result, #{})).

%% @doc Test direct claim_yield functionality from a single resource
claim_yield_single_resource_test() ->
    Opts = test_opts(),
    ResourceOxygen = <<"oxygen">>,
    % Alice has deposits in pot but hasn't claimed yet
    Base =
        generate_integrated_state(
            #{
                initial_balances => #{?ALICE => 1000},
                total_supply => 1000,
                mint_cap => 10000,
                mint_prop => {1, 2},
                t => 0,
                last_drip => 0,
                resources => #{
                    ResourceOxygen => pot_resource(100, #{ ?ALICE => 10 })
                }
            },
            Opts
        ),
    ?event({initial_alice_balance, get_balance(Base, ?ALICE, Opts)}),
    % Advance time to generate yield
    % ToMint = 10000 * (2^1 - 1) / 2 = 5000
    % GlobalAcc = 5000 / 1000 = 5
    % ResourceAcc = 5 * 100 = 500
    % Alice's yield = 500 * 10 = 5000
    % Call claim_yield directly
    {ok, ResultAfterClaim} = 
        hb_ao:resolve(
            Base,
            #{
                <<"path">> => <<"mint">>,
                <<"subject">> => ?ALICE,
                <<"timestamp">> => 1
            },
            Opts
        ),
    ?event({after_claim, ResultAfterClaim}),
    AliceBalanceAfterClaim = get_balance(ResultAfterClaim, ?ALICE, Opts),
    ?assertEqual(6000, AliceBalanceAfterClaim),
    ?assertEqual(6000, hb_ao:get(<<"total-supply">>, ResultAfterClaim, Opts)),
    {ok, ResultSecondClaim} =
        hb_ao:resolve(
            ResultAfterClaim,
            #{
                <<"path">> => <<"mint">>,
                <<"subject">> => ?ALICE,
                <<"timestamp">> => 2
            },
            Opts
        ),
    ?assertEqual(6000, get_balance(ResultSecondClaim, ?ALICE, Opts)).

%% @doc Test claim_yield across multiple resources
claim_yield_multiple_resources_test() ->
    Opts = test_opts(),
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    % Alice has deposits in two different resources
    Base = generate_integrated_state(
        #{
            initial_balances => #{?ALICE => 500},
            total_supply => 500,
            mint_cap => 10000,
            mint_prop => {1, 2},
            t => 0,
            last_drip => 0,
            resources => #{
                ResourceOxygen => pot_resource(100, [{?ALICE, 10}]),
                ResourceHydrogen => pot_resource(50, [{?ALICE, 5}])
            }
        },
        Opts
    ),
    % Advance time to t=1
    % ToMint = 5000
    % GlobalAcc = 5000 / 1500 = 3.333... (TWU = 100*10 + 50*5 = 1250)
    % Wait, TWU should be weight * total-deposits
    % Oxygen: weight=100, deposits=10, weighted=1000
    % Hydrogen: weight=50, deposits=5, weighted=250
    % TWU = 1250
    % GlobalAcc = 5000 / 1250 = 4
    % OxygenAcc = 4 * 100 = 400
    % HydrogenAcc = 4 * 50 = 200
    % Alice oxygen yield = 400 * 10 = 4000
    % Alice hydrogen yield = 200 * 5 = 1000
    % Total yield = 5000
    BaseWithTime = Base#{ <<"t">> => 1 },
    % Claim all yields at once using claim_yield/3
    {ok, ResultAfterClaimAll} =
        hb_ao:resolve(
            BaseWithTime,
            #{
                <<"path">> => <<"mint">>,
                <<"subject">> => ?ALICE
            },
            Opts
        ),
    AliceBalance = get_balance(ResultAfterClaimAll, ?ALICE),
    % Alice should have: 500 + 5000 = 5500
    ?assertEqual(5500, AliceBalance),
    % Total supply should be updated
    ?assertEqual(5500, hb_ao:get(<<"total-supply">>, ResultAfterClaimAll, Opts)).

%% @doc Test claim_yield when address has no deposits (edge case)
claim_yield_no_deposits_test() ->
    Opts = test_opts(),
    ResourceOxygen = <<"oxygen">>,
    % Charlie has no deposits, only balance
    Base= generate_integrated_state(
        #{
            initial_balances => #{ ?CHARLIE => 100 },
            total_supply => 100,
            mint_cap => 10000,
            mint_prop => {1, 2},
            t => 0,
            last_drip => 0,
            resources => #{
                ResourceOxygen => pot_resource(100, [{?ALICE, 10}])  
            }
        },
        Opts
    ),
    BaseWithTime = Base#{ <<"t">> => 1 },
    % Charlie tries to claim yield 
    {ok, ResultAfterClaim} =
        hb_ao:resolve(
            BaseWithTime,
            #{ <<"path">> => <<"mint">>, <<"subject">> => ?CHARLIE },
            Opts
        ),
    % Charlie's balance should be unchanged (still 100)
    ?assertEqual(100, get_balance(ResultAfterClaim, ?CHARLIE)),
    % Total supply should be unchanged (no new minting for Charlie)
    {ok, ResultAfterClaimAll} =
        hb_ao:resolve(
            BaseWithTime,
            #{ <<"path">> => <<"mint">>, <<"subject">> => ?CHARLIE },
            Opts
        ),
    ?assertEqual(100, hb_ao:get(<<"total-supply">>, ResultAfterClaim, Opts)),
    ?assertEqual(100, get_balance(ResultAfterClaimAll, ?CHARLIE)),
    ?assertEqual(100, hb_ao:get(<<"total-supply">>, ResultAfterClaimAll, Opts)).

%%% Benchmark Tests
benchmark_transfers_test() ->
    Opts = test_opts(),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 1_000,
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base = generate_token_base_state(
        #{
        initial_balances =>
            hb_maps:from_list(
                [
                    {?ALICE, 1_000_000_000}
                ] ++
                [
                        {
                            hb_util:human_id(crypto:strong_rand_bytes(32)),
                            1_000_000_000
                        }
                    ||
                        _ <- lists:seq(1, Accounts - 1)
                ]
            )
        },
        Opts
    ),
    Reqs =
        [
            generate_assignment(
                <<"transfer">>,
                #{
                    <<"from">> => ?ALICE,
                    <<"recipient">> => ?BOB,
                    <<"quantity">> => 1,
                    <<"transfer-number">> => I
                },
                ?ALICE
            )
            ||
                I <- lists:seq(1, Transfers)
        ],
    AOCoreStartTime = erlang:monotonic_time(millisecond),
    AOCoreInvokedState =
        lists:foldl(
            fun(Req, State) ->
                {ok, NewState} = hb_ao:resolve(State, Req, Opts),
                NewState#{ <<"results">> => #{} }
            end,
            Base,
            Reqs
        ),
    AOCoreEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"AOCore invoked transfers">>,
        <<"transfers">>,
        Transfers,
        (AOCoreEndTime - AOCoreStartTime) / 1000
    ),
    DirectStartTime = erlang:monotonic_time(millisecond),
    DirectlyInvokedState =
        lists:foldl(
            fun(Req, State) ->
                {ok, NewState} = hb_ao:resolve(State, Req, Opts),
                NewState#{ <<"results">> => #{} }
            end,
            Base,
            Reqs
        ),
    DirectEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"Directly invoked transfers">>,
        <<"transfers">>,
        Transfers,
        (DirectEndTime - DirectStartTime) / 1000
    ),
    % Verify correctness
    ?assertEqual(
        1_000_000_000 - Transfers,
        get_balance(DirectlyInvokedState, ?ALICE)
    ),
    ?assertEqual(
        Transfers,
        get_balance(DirectlyInvokedState, ?BOB)
    ),
    ?assert(hb_message:match(DirectlyInvokedState, AOCoreInvokedState, strict, #{})).

benchmark_process_transfers_test_() ->
    {timeout, 180, fun benchmark_process_transfers/0}.
benchmark_process_transfers() ->
    hb:init(),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 3_000,
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()]
        },
    AliceWallet = ar_wallet:new(),
    AliceAddr = hb_util:human_id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = hb_util:human_id(BobWallet),
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base =
        generate_process_state(
            #{
                initial_balances =>
                    hb_maps:from_list(
                        [
                            {AliceAddr, 1_000_000_000}
                        ] ++
                        [
                            {
                                hb_util:human_id(crypto:strong_rand_bytes(32)),
                                1_000_000_000
                            }
                            ||
                                _ <- lists:seq(1, Accounts - 1)
                        ]
                    )
            },
            Opts
        ),
    lists:foreach(
        fun(I) ->
            schedule_request(
                Base,
                <<"transfer">>,
                #{
                    <<"from">> => AliceAddr,
                    <<"recipient">> => BobAddr,
                    <<"quantity">> => 1,
                    <<"transfer-number">> => I
                },
                AliceWallet,
                Opts
            )
        end,
        lists:seq(1, Transfers)
    ),
    NowStartTime = erlang:monotonic_time(millisecond),
    {ok, State} = hb_ao:resolve(Base, #{ <<"path">> => <<"now">> }, Opts),
    NowEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"Process transfers">>,
        <<"transfers">>,
        Transfers,
        (NowEndTime - NowStartTime) / 1000
    ),
    ?assertEqual(1_000_000_000 - Transfers, get_balance(State, AliceAddr)),
    ?assertEqual(Transfers, get_balance(State, BobAddr)),
    ?assertEqual(
        1_000_000_000 * Accounts,
        hb_ao:get(<<"total-supply">>, State, #{})
    ).

benchmark_batch_mint_test() ->
    Opts = test_opts(),
    NumRecipients = 10_000,  
    Base = generate_token_base_state(Opts),
    % Create batch with NumRecipients recipients
    Recipients = [
        list_to_binary("recipient-" ++ integer_to_list(I))
        || I <- lists:seq(1, NumRecipients)
    ],
    Quantities = maps:from_list([{R, 1000} || R <- Recipients]),
    Req = generate_assignment(
        <<"mint">>,
        #{
            <<"quantities">> => Quantities,
            <<"mode">> => <<"batch">>
        },
        ?MINTER
    ),
    % Benchmark batch mint
    StartTime = erlang:monotonic_time(millisecond),
    {ok, NewState} = hb_ao:resolve(Base, Req#{ <<"path">> => <<"compute">>}, Opts),
    EndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"Token Batch Mint">>,
        <<"recipients">>,
        NumRecipients,
        (EndTime - StartTime) / 1000
    ),
    ?assertEqual(
        NumRecipients * 1000, 
        hb_ao:get(<<"total-supply">>, NewState, #{})
    ).