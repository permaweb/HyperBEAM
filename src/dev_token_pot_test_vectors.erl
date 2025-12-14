%%% @doc Test vectors and benchmarks for the `~token@1.0` device.
-module(dev_token_pot_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

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

slot(SchedRes, Opts) -> hb_maps:get(<<"slot">>, SchedRes, none, Opts).

resolve_now(Base, Opts) ->
    {ok, NewBase} = hb_ao:resolve(Base, #{ <<"path">> => <<"now">> }, Opts),
    ?event(post_resolve,
        {now,
            {new_base, NewBase}
        },
        Opts
    ),
    NewBase.

set_weight_req(Resource, Weight) ->
    #{
        <<"action">> => <<"set_weight">>,
        <<"resource-id">> => Resource,
        <<"weight">> => Weight
    }.

deposit_req(Resource, Addr, Qty) ->
    #{
        <<"action">> => <<"deposit">>,
        <<"resource-id">> => Resource,
        <<"address">> => Addr,
        <<"amount">> => Qty
    }.

transfer_req(Addr, Qty) ->
    transfer_req(Addr, Qty, #{}).
transfer_req(Addr, Qty, Params) ->
    Params#{
        <<"action">> => <<"transfer">>,
        <<"recipient">> => Addr,
        <<"quantity">> => Qty
    }.
%% @doc Generate a base token state with default configuration.
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
    Owner = id(hb_maps:get(priv_wallet, Opts, <<"owner">>, Opts)),
    DefaultState = #{
        <<"device">> => <<"token@1.0">>,
        <<"t-source">> => <<"slot">>,
        <<"owner">> => maps:get(owner, Params, Owner),
        <<"mint-authority">> => maps:get(mint_authority, Params, Owner),
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
generate_integrated_process_state(PotFields, TokenFields, Opts) ->
    PotBase = generate_pot_fields(PotFields, Opts),
    TokenBase = 
        generate_token_base_state(
            TokenFields#{
                extra => PotBase},
            Opts
        ),
    generate_process_state(TokenBase, Opts).

%% @doc Create a request message.
schedule_request(State, Body, Wallet, Opts) ->
    ?event({scheduling_request, {body, Body}}),
    Signed =
        hb_message:commit(
            Body,
            Opts#{ priv_wallet => Wallet }
        ),
    ?event({signed_request, Signed}),
    Req =
        Signed#{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>
        },
    ?event(schedule_request,
        {scheduling_request, 
            {state, State}, 
            {req, Req}
        },
        Opts
    ),
    {ok, Res} = hb_ao:resolve(State, Req, Opts),
    ?event({schedule_result, Res}),
    Res.

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

%% @doc Generate pot fields for integration testing
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
schedule_resource({Resource, Weight, UserDeposits}, Base, Opts) ->
    #{ priv_wallet := Wallet } = Opts,
    schedule_request(
        Base,
        set_weight_req(Resource, Weight),
        Wallet,
        Opts
    ),
    lists:foreach(
        fun({UserWallet, Qty}) ->
            Addr = id(UserWallet),
            schedule_request(
                Base,
                deposit_req(Resource, Addr, Qty),
                UserWallet,
                Opts
            )
        end,
        UserDeposits
    ).
pot_deposit_resource(Resources, Base, Opts) when is_list(Resources) ->
    lists:foreach(
        fun(Resource) ->
            schedule_resource(Resource, Base, Opts)
        end,
        Resources
    ),
    resolve_now(Base, Opts);
pot_deposit_resource(Resource, Base, Opts) ->
    schedule_resource(Resource, Base, Opts),
    resolve_now(Base, Opts).

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
    schedule_request(
        Base,
        transfer_req(BobAddr, 1),
        AliceWallet,
        Opts
    ),
    State = resolve_now(Base, Opts),
    ?assertEqual(999_999_999, get_balance(State, AliceAddr, Opts)),
    ?assertEqual(1, get_balance(State, BobAddr, Opts)),
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
    Base = generate_integrated_process_state(PotFields, TokenFields, Opts),
    ?event({base_state, Base}),
    ModifiedBase =
        pot_deposit_resource(
            {ResourceOxygen, 100, [{AliceWallet, 10}]},
            Base,
            Opts
        ), 
    ?event({simple_pot, {modified, ModifiedBase}}),
    ?event({simple_pot, {modfied, ModifiedBase}}),
    schedule_request(
        ModifiedBase,
        #{
            <<"action">> => <<"mint">>
        },
        AliceWallet,
        Opts
    ),
    schedule_request(
        ModifiedBase,
        transfer_req(BobAddr, 1),
        AliceWallet,
        Opts
    ),
    State = resolve_now(ModifiedBase, Opts),
    ?event(debug_test, {state, State}, Opts),
    ?assertEqual(1, get_balance(State, BobAddr,Opts)),
    ?assertEqual(8999, get_balance(State, AliceAddr,Opts)),
    ?assertEqual(9000, hb_ao:get(<<"total-supply">>, State, Opts)).

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
    Base = generate_integrated_process_state(PotFields, TokenFields, Opts),
    NewBase = 
        pot_deposit_resource(
            {ResourceOxygen, 100, [{AliceWallet, 10}]}, 
            Base, 
            Opts
        ),
    ?event({initial_state, NewBase}),
    % Advance time to generate yield
    % With mint_cap=10000, mint_prop={1,2}, going from t=0 to t=1:
    % ToMint = 10000 * (2^1 - 1^1) / 2^1 = 10000 * 1 / 2 = 5000
    % GlobalAcc = 0 + (5000 / 1000) = 5 (per weighted unit)
    % ResourceAcc = 0 + (5 * 100) = 500
    % Alice's yield = (500 - 0) * 10 = 5000 tokens!
    Balance = get_balance( NewBase, AliceAddr, Opts),
    ?event({alice_balance, Balance}),
    ?assertEqual(500, Balance),
    % % Try to transfer 700 tokens
    % % Should fail without normalize_mint (500 < 700)
    % % Should succeed with normalize_mint (500 + 5000 = 5500 > 700)
    schedule_request(
        NewBase,
        transfer_req(BobAddr, 700),
        AliceWallet,
        Opts
    ),
    Result = resolve_now(NewBase, Opts),
    ?event({transfer_result, Result}),
    % Alice should have: (500 + 5000) - 700 = 4800
    % Bob should have: 700
    ?assertEqual(6800, get_balance(Result, AliceAddr,Opts)),
    ?assertEqual(700, get_balance(Result, BobAddr,Opts)),
    % Total supply should be updated
    % Initial: 500, Minted: 5000, New total: 5500
    ?assertEqual(7500, hb_ao:get(<<"total-supply">>, Result, Opts)).

%% @doc Test direct claim_yield functionality from a single resource
claim_yield_single_resource_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{AliceAddr => 1000},
        total_supply => 1000
    },
    Base = generate_integrated_process_state(PotFields, TokenFields, Opts),
    NewBase = 
        pot_deposit_resource(
            {ResourceOxygen, 100, [{AliceWallet, 10}]}, 
            Base, 
            Opts
        ),
    ?event(pot_claim, {new_pot, NewBase}, Opts),
    schedule_request(
        NewBase,
        #{
            <<"action">> => <<"mint">>
        },
        AliceWallet,
        Opts
    ),
    BaseAfterClaim = resolve_now(NewBase, Opts),
    ?event({after_claim, BaseAfterClaim}),
    ?assertEqual(8000, get_balance(BaseAfterClaim, AliceAddr,Opts)).

%% @doc Test claim_yield across multiple resources
claim_yield_multiple_resources_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    % Alice has deposits in two different resources
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{AliceAddr => 1000},
        total_supply => 1000
    },
    Base = generate_integrated_process_state(PotFields, TokenFields, Opts),
    NewBase = 
        pot_deposit_resource(
            [
                {ResourceOxygen, 100, [{AliceWallet, 10}]},
                {ResourceHydrogen, 50, [{AliceWallet, 5}]}
            ], 
            Base, 
            Opts
        ),
    ?event(pot_mutli_claim, {new_pot, NewBase}, Opts),
    schedule_request(
        NewBase,
        #{
            <<"action">> => <<"mint">>
        },
        AliceWallet,
        Opts
    ),
    BaseAfterClaim = resolve_now(NewBase, Opts),
    ?assertEqual(9750, get_balance(BaseAfterClaim, AliceAddr,Opts)),
    ?assertEqual(9750, hb_ao:get(<<"total-supply">>, BaseAfterClaim, Opts)).

%% @doc Test claim_yield when address has no deposits (edge case)
claim_yield_no_deposits_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    TokenFields = #{
        initial_balances => #{AliceAddr => 100},
        total_supply => 100
    },
    Base = generate_integrated_process_state(PotFields, TokenFields, Opts),
    schedule_request(
        Base,
        #{
            <<"action">> => <<"mint">>
        },
        AliceWallet,
        Opts
    ),
    BaseAfterClaim = resolve_now(Base, Opts),
    % Alice's balance should be unchanged (still 100)
    ?assertEqual(100, get_balance(BaseAfterClaim, AliceAddr,Opts)),
    ?assertEqual(100, hb_ao:get(<<"total-supply">>, BaseAfterClaim, Opts)).

%%% Benchmark Tests
benchmark_transfers_process_test() ->
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = id(BobWallet),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 1_000,
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base = generate_process_state(
        generate_token_base_state(
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
        Opts
    ),
    Reqs =
        [
            schedule_request(
                Base,
                transfer_req(BobAddr, 1, #{ <<"transfer-number">> => I }),
                AliceWallet,
                Opts
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
    % Verify correctness
    ?assertEqual(
        1_000_000_000 - Transfers,
        get_balance(AOCoreInvokedState, AliceAddr, Opts)
    ),
    ?assertEqual(
        Transfers,
        get_balance(AOCoreInvokedState, BobAddr, Opts)
    ).

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
            generate_token_base_state(
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
            Opts
        ),
    lists:foreach(
        fun(I) ->
            schedule_request(
                Base,
                transfer_req(BobAddr, 1, #{ <<"transfer-number">> => I }),
                AliceWallet,
                Opts
            )
        end,
        lists:seq(1, Transfers)
    ),
    NowStartTime = erlang:monotonic_time(millisecond),
    State = resolve_now(Base, Opts),
    NowEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"Process transfers">>,
        <<"transfers">>,
        Transfers,
        (NowEndTime - NowStartTime) / 1000
    ),
    ?assertEqual(Transfers, get_balance(State, BobAddr, Opts)),
    ?assertEqual(1_000_000_000 - Transfers, get_balance(State, AliceAddr, Opts)),
    ?assertEqual(
        1_000_000_000 * Accounts,
        hb_ao:get(<<"total-supply">>, State, Opts)
    ).