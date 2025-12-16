%%% @doc Test vectors and benchmarks for configurations of `~token@1.0',
%%% using the `~pot@1.0' mint device, as a `~process@1.0' message.
-module(dev_token_pot_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Test Helpers: State Accessors.
%%% ----------------------------------------------------------------------------

%% @doc Get balance for an account.
balance(Process, Wallet, Opts) when is_tuple(Wallet) ->
    balance(Process, id(Wallet), Opts);
balance(Process, Account, Opts) when is_binary(Account) ->
    balance(Process, #{ <<"balance">> => Account }, Opts);
balance(Process, Req, Opts) ->
    CurrentSlot = hb_ao:get(<<"slot/current">>, Process, Opts),
    ?event(debug_test, {current_slot, CurrentSlot}, Opts),
    Res =
        hb_ao:resolve_many(
            [
                Process,
                #{ <<"path">> => <<"now">> },
                #{
                    <<"path">> => <<"as">>,
                    <<"as">> => <<"execution">>
                },
                Req#{
                    <<"path">> => <<"balance">>,
                    <<"slot">> => hb_cache:ensure_loaded(CurrentSlot, Opts)
                }
            ],
            Opts
        ),
    case Res of
        {ok, B} -> B;
        {error, not_found} -> 0
    end.

%% @doc Return the deposit quantity for the given resource and address.
deposit(Process, Resource, Address, Opts) ->
    deposit(Process, Resource, Address, <<>>, Opts).
deposit(Process, Resource, Address, SubPath, Opts) ->
    hb_ao:get(
        <<
            "now/resources/",
            Resource/binary,
            "/deposits/",
            Address/binary,
            "/",
            SubPath/binary
        >>,
        Process,
        Opts
    ).

%%% Test Helpers: Generators.
%%% ----------------------------------------------------------------------------


%% @doc Generate generic isolated node messages for testing.
test_opts() ->
    hb:init(),
    #{
        priv_wallet => ar_wallet:new(),
        store => [hb_test_utils:test_store()]
    }.

%% @doc Generate a random ID, or an 'ID' value of the correct length starting
%% with the given binary and padded with zeros.
id(AlreadyID) when is_binary(AlreadyID) -> AlreadyID;
id(Bin) when is_binary(Bin) ->
    BitSize = byte_size(Bin) * 8,
    Suffix = << 0:(256 - BitSize) >>,
    << Bin/binary, Suffix/binary >>;
id(Other) -> hb_util:human_id(Other).


now(Process, Opts) ->
    {ok, State} = hb_ao:resolve(Process, #{ <<"path">> => <<"now">> }, Opts),
    ?event(debug_test, {now_result, State}, Opts),
    State.

set_weight_req(Resource, Weight) ->
    #{
        <<"action">> => <<"set-weight">>,
        <<"resource">> => Resource,
        <<"weight">> => Weight
    }.

deposit_req(Resource, Addr, Qty) ->
    #{
        <<"action">> => <<"deposit">>,
        <<"resource">> => Resource,
        <<"address">> => Addr,
        <<"quantity">> => Qty
    }.

delegate_req(Resource, Addr, Qty) ->
    #{
        <<"action">> => <<"delegate">>,
        <<"resource">> => Resource,
        <<"address">> => Addr,
        <<"quantity">> => Qty
    }.

undelegate_req(Resource, Addr, Qty) ->
    #{
        <<"action">> => <<"undelegate">>,
        <<"resource">> => Resource,
        <<"address">> => Addr,
        <<"quantity">> => Qty
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
generate_token_state(Params, Opts) ->
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
            InitialBalances#{ <<"path">> => <<"set">> },
            Opts
        ),
    DefaultState = #{
        <<"device">> => <<"token@1.0">>,
        <<"set-authority">> => id(hb_opts:get(priv_wallet, no_wallet, Opts)),
        <<"t-source">> => <<"slot">>,
        <<"name">> => <<"Test Token">>,
        <<"ticker">> => <<"TEST">>,
        <<"denomination">> => 12,
        <<"total-supply">> => Total,
        <<"balances">> => Balances
    },
    FinalState = maps:merge(DefaultState, maps:get(extra, Params, #{})),
    ?event({final_state_generated, {balances_size, map_size(Balances)}}),
    FinalState.

%% @doc Helper to generate a generic process state with the given extra keys
%% added. The result is returned committed with the base wallet of the given
%% `Opts'.
generate_base_process_state(ExtraKeys, Opts) ->
    Addr = id(hb_opts:get(priv_wallet, no_wallet, Opts)),
    ProcessBase =
        ExtraKeys#{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"execution-device">> => <<"token@1.0">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"push-device">> => <<"push@1.0">>,
            <<"scheduler">> => Addr,
            <<"authority">> => Addr
        },
    Proc = hb_message:commit(ProcessBase, Opts),
    hb_cache:write(Proc, Opts),
    Proc.

%% @doc Generate pot state for integration testing
generate_pot_state(Params, Opts) ->
    MintCap = hb_maps:get(mint_cap, Params, 10000, Opts),
    MintPropN = hb_maps:get(mint_prop_numerator, Params, 1, Opts),
    MintPropD = hb_maps:get(mint_prop_denominator, Params, 2, Opts),
    T = hb_maps:get(t, Params, 0, Opts),
    LastDrip = hb_maps:get(last_drip, Params, 0, Opts),
    ?event({generate_pot_fields, {params, Params}}),
    MaybeParent =
        case hb_maps:get(parent, Params, not_found, Opts) of
            not_found -> #{};
            Parent -> #{ <<"parent">> => Parent }
        end,
    MaybeParent#{
        <<"mint-device">> => <<"pot@1.0">>,
        <<"mint-cap">> => MintCap,
        <<"mint-prop-numerator">> => MintPropN,
        <<"mint-prop-denominator">> => MintPropD,
        <<"total-weighted-units">> => 0,
        <<"resources">> => #{},
        <<"t">> => T,
        <<"last-drip">> => LastDrip
    }.

%% @doc Return a signed token process with a `pot@1.0` mint device.
generate_process(PotFields, Opts) ->
    generate_process(PotFields, #{}, Opts).
generate_process(PotFields, TokenFields, Opts) ->
    PotBase = generate_pot_state(PotFields, Opts),
    TokenBase = 
        generate_token_state(
            TokenFields#{
                extra => PotBase
            },
            Opts
        ),
    generate_base_process_state(TokenBase, Opts).

%% @doc Create a request message.
schedule_request(Process, Body, Opts) ->
    schedule_request(
        Process,
        Body,
        hb_opts:get(priv_wallet, no_wallet, Opts),
        Opts
    ).
schedule_request(Process, Body, Wallet, Opts) ->
    ?event(debug_test, {scheduling_request, {body, Body}}, Opts),
    Signed =
        hb_message:commit(
            Body,
            Opts#{ priv_wallet => Wallet }
        ),
    ?event(debug_test, {signed_request, Signed}, Opts),
    Req =
        Signed#{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>
        },
    ?event(schedule_request,
        {scheduling_request, 
            {process, Process}, 
            {req, Req}
        },
        Opts
    ),
    {ok, Res} = hb_ao:resolve(Process, Req, Opts),
    ?event(debug_test, {schedule_result, Res}, Opts),
    Res.

push_request(Process, Body, Opts) ->
    push_request(
        Process,
        Body,
        hb_opts:get(priv_wallet, no_wallet, Opts),
        Opts
    ).
push_request(Process, Body, Wallet, Opts) ->
    PushReq =
        hb_message:commit(#{
            <<"path">> => <<"push">>,
            <<"body">> =>
                hb_message:commit(
                    Body,
                    Opts#{ priv_wallet => Wallet }
                )
            },
            Opts#{ priv_wallet => Wallet }
        ),
    hb_ao:resolve(
        Process,
        PushReq,
        Opts#{ priv_wallet => Wallet }
    ).

schedule_set_weight(Process, Resource, Weight, Opts) ->
    Assignment = schedule_request(
        Process,
        set_weight_req(Resource, Weight),
        Opts
    ),
    ?no_prod("Only to debug no comms in assignment issue"),
    CachedAssignemnts =  
        dev_scheduler_cache:read(
            dev_process_lib:process_id(Process, Opts), 
            0, 
            Opts
        ),
    ?event(fix, 
        {
            weight_req, 
                {req, Assignment},
                {signers, hb_message:signers(Assignment, Opts)}, 
                {cached_assignemnt,CachedAssignemnts},
                {cached_signers, hb_message:signers(CachedAssignemnts, Opts)} 
        }, Opts),
    now(Process, Opts).

%% @doc Helper to create a pot resource with deposits
schedule_modify_resource(Process, Resource, UserDeposits, Opts) ->
    hb_maps:map(
        fun(UserWallet, Qty) ->
            schedule_request(
                Process,
                deposit_req(Resource, id(UserWallet), Qty),
                Opts
            )
        end,
        UserDeposits
    ),
    now(Process, Opts).

schedule_deposit(Process, Resource, User, Qty, Opts) ->
    schedule_modify_resource(Process, Resource, #{ id(User) => Qty }, Opts).

push_delegate(Process, Resource, User, ToAddr, Qty, Opts) ->
    push_request(
        Process,
        delegate_req(Resource, ToAddr, Qty),
        User,
        Opts
    ),
    now(Process, Opts).

schedule_undelegate(Process, Wallet, FromAddr, Resource, Qty, Opts) ->
    schedule_request(
        Process,
        undelegate_req(Resource, FromAddr, Qty),
        Wallet,
        Opts
    ),
    now(Process, Opts).

%%% Test Cases.
%%% ----------------------------------------------------------------------------

simple_process_test() ->
    hb:init(),
    Opts = test_opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = id(BobWallet),
    Base =
        generate_base_process_state(
            generate_token_state(
                #{
                    initial_balances => #{ AliceAddr => 1_000_000_000 }
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
    State = now(Base, Opts),
    ?assertEqual(999_999_999, balance(State, AliceAddr, Opts)),
    ?assertEqual(1, balance(State, BobAddr, Opts)),
    ?assertEqual(1_000_000_000, hb_ao:get(<<"total-supply">>, State, Opts)).

%% @doc Basic test to see what happens when transfer is called with mint-device=pot
simple_pot_process_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2
    },
    TokenFields = #{
        initial_balances => #{ id(Alice) => 1000 },
        total_supply => 1000
    },
    Process = generate_process(PotFields, TokenFields, Opts),
    ?event({process, Process}),
    schedule_set_weight(Process, ResourceOxygen, 100, Opts),
    schedule_deposit(
        Process,
        ResourceOxygen,
        Alice,
        10,
        Opts
    ),
    schedule_request(
        Process,
        #{ <<"action">> => <<"mint">> },
        Alice,
        Opts
    ),
    schedule_request(
        Process,
        transfer_req(id(Bob), 1),
        Alice,
        Opts
    ),
    ?event(debug_test, {state, Process}, Opts),
    ?assertEqual(1, balance(Process, id(Bob),Opts)),
    ?assertEqual(8999, balance(Process, id(Alice), Opts)),
    ?assertEqual(9000, hb_ao:get(<<"now/total-supply">>, Process, Opts)).

pot_delegation_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Resource = <<"oxygen">>,
    Process =
        generate_process(
            #{
                mint_cap => 10000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    schedule_set_weight(Process, Resource, 100, Opts),
    schedule_deposit(Process, Resource, Alice, 100, Opts),
    push_delegate(Process, Resource, Alice, id(Bob), 10, Opts),
    ?assertEqual(
        10,
        deposit(Process, Resource, id(Bob), <<"quantity">>, Opts)
    ),
    ?assertEqual(
        90,
        deposit(Process, Resource, id(Alice), <<"quantity">>, Opts)
    ),
    ?assertEqual(100,
        hb_ao:get(
            <<"now/resources/", Resource/binary, "/total-deposits">>,
            Process,
            Opts
        )
    ).

balance_without_explicit_mint_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    PotFields = #{
        mint_cap => 10000
    },
    Process = generate_process(PotFields, Opts),
    schedule_set_weight(Process, ResourceOxygen, 100, Opts),
    schedule_deposit(Process, ResourceOxygen, Alice, 10, Opts),
    ?event(debug_test, 
        {processes, 
            {balance, balance(Process, id(Alice), Opts)}, 
            {post_deposit, now(Process, Opts)}
        },
        Opts
    ),
    ?assert(balance(Process, Alice, Opts) > 0).

%% @doc Test that transfer works when balance is insufficient but 
%% balance + unclaimed_yield is sufficient
%% This validates that normalize_mint properly claims yields before transfer
transfer_with_unclaimed_yield_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob  = ar_wallet:new(),
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
        initial_balances => #{ id(Alice) => 500 }
    },
    Process = generate_process(PotFields, TokenFields, Opts),
    schedule_set_weight(Process, ResourceOxygen, 100, Opts),
    schedule_deposit(
        Process,
        ResourceOxygen,
        Alice,
        10,
        Opts
    ),
    % Advance time to generate yield
    % With mint_cap=10000, mint_prop={1,2}, going from t=0 to t=1:
    % ToMint = 10000 * (2^1 - 1^1) / 2^1 = 10000 * 1 / 2 = 5000
    % GlobalAcc = 0 + (5000 / 1000) = 5 (per weighted unit)
    % ResourceAcc = 0 + (5 * 100) = 500
    % Alice's yield = (500 - 0) * 10 = 5000 tokens!
    %?assertEqual(500, balance(Process, id(Alice), Opts)),
    % % Try to transfer 700 tokens
    % % Should fail without normalize_mint (500 < 700)
    % % Should succeed with normalize_mint (500 + 5000 = 5500 > 700)
    schedule_request(
        Process,
        transfer_req(id(Bob), 700),
        Alice,
        Opts
    ),
    % Alice should have: (500 + 5000) - 700 = 4800
    % Bob should have: 700
    ?assertEqual(6800, balance(Process, id(Alice), Opts)),
    ?assertEqual(700, balance(Process, id(Bob), Opts)),
    % Total supply should be updated
    % Initial: 500, Minted: 5000, New total: 5500
    ?assertEqual(7500, hb_ao:get(<<"now/total-supply">>, Process, Opts)).

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
    Process = generate_process(PotFields, TokenFields, Opts),
    schedule_set_weight(Process, ResourceOxygen, 100, Opts),
    NewBase = 
        schedule_deposit(
            Process,
            ResourceOxygen,
            AliceWallet,
            10,
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
    BaseAfterClaim = now(NewBase, Opts),
    ?event({after_claim, BaseAfterClaim}),
    ?assertEqual(8000, balance(BaseAfterClaim, AliceAddr,Opts)).

%% @doc Test claim_yield across multiple resources
claim_yield_multiple_resources_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    % Alice has deposits in two different resources
    PotFields = #{
        mint_cap => 10_000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2
    },
    Process = generate_process(PotFields, Opts),
    State = now(Process, Opts),
    schedule_set_weight(State, ResourceOxygen, 100, Opts),
    schedule_set_weight(State, ResourceHydrogen, 50, Opts),
    schedule_deposit(State, ResourceOxygen, Alice, 10, Opts),
    schedule_deposit(State, ResourceHydrogen, Alice, 5, Opts),
    schedule_request(
        State,
        #{
            <<"action">> => <<"mint">>
        },
        Alice,
        Opts
    ),
    State2 = now(State, Opts),
    ?assertEqual(8750, balance(State2, id(Alice), Opts)).

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
    Base = generate_process(PotFields, TokenFields, Opts),
    schedule_request(
        Base,
        #{
            <<"action">> => <<"mint">>
        },
        AliceWallet,
        Opts
    ),
    BaseAfterClaim = now(Base, Opts),
    % Alice's balance should be unchanged (still 100)
    ?assertEqual(100, balance(BaseAfterClaim, AliceAddr,Opts)),
    ?assertEqual(100, hb_ao:get(<<"total-supply">>, BaseAfterClaim, Opts)).

pot_subscriptions_test() ->
    Opts = test_opts(),
    Resource = <<"oxygen">>,
    ParentProcess =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2
            },
            Opts
        ),
    ParentID = dev_process_lib:process_id(ParentProcess, Opts),
    ChildProcess =
        generate_process(
            #{
                mint_cap => 10_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2,
                parent => ParentID
            },
            Opts
        ),
    ChildID = dev_process_lib:process_id(ChildProcess, Opts),
    ?event(
        debug_test,
        {test_processes,
            {parent, ParentID},
            {child, ChildID}
        },
        Opts
    ),
    Res =
        push_request(
            ChildProcess,
            #{ <<"action">> => <<"mint">> },
            Opts
        ),
    ?event(debug_test, {push_mint_result, Res}, Opts),
    ChildState = now(ChildProcess, Opts),
    ?assertEqual(
        [dev_process_lib:process_id(ChildProcess, Opts)],
        dev_process_outbox:subscribers(ParentProcess, <<"set-weight">>, Opts)
    ),
    schedule_set_weight(ChildProcess, Resource, 100, Opts),
    ?assertEqual(
        [dev_process_lib:process_id(ParentProcess, Opts)],
        hb_ao:get(
            <<
                "now/resources/",
                Resource/binary,
                "/weight"
            >>,
            ChildState,
            Opts
        )
    ).

nested_pot_process_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    % Create the parent mint, which will deliver units to the child mint.
    StETH = <<"stETH">>,
    ParentPotParams = #{
        mint_cap => 1_000_000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    ParentToken = generate_process(ParentPotParams, Opts),
    ParentID = dev_process_lib:process_id(ParentToken, Opts),
    ?event(process, {parent_mint, ParentID}, Opts),
    % Create the child mint, which will receive units from the parent mint in
    % exchange for its own tokens.
    ChildPotParams = #{
        mint_cap => 1_000_000,
        mint_prop_numerator => 1,
        mint_prop_denominator => 2,
        t => 0,
        last_drip => 0
    },
    ChildToken = generate_process(ChildPotParams, Opts),
    ChildID = dev_process_lib:process_id(ChildToken, Opts),
    % Set the weights mints such that all units in the parent are given for
    % providing `stETH', and all units in the child are given for providing
    % `Parent'.
    schedule_set_weight(ParentToken, StETH, 1, Opts),
    schedule_set_weight(ChildToken, StETH, 1, Opts),
    % Deposit units of the resource into the parent mint for Alice.
    schedule_deposit(ParentToken, StETH, Alice, 2, Opts),
    % Delegate half of Alice's units in the parent mint to the child mint.
    push_delegate(ParentToken, StETH, Alice, ChildID, 1, Opts),
    % Check that tokens are being minted in the parent for both the child token
    % and Alice.
    schedule_request(
        ParentToken,
        #{ <<"action">> => <<"mint">> },
        Alice,
        Opts
    ),
    schedule_request(
        ChildToken,
        #{ <<"action">> => <<"mint">> },
        Alice,
        Opts
    ),
    ParentState = now(ParentToken, Opts),
    ChildState = now(ChildToken, Opts),
    ?event(debug_test, {parent_state, ParentState}, Opts),
    ?assert(balance(ParentState, Alice, Opts) > 0),
    ?assert(balance(ParentState, ChildID, Opts) > 0),
    % Check that Alice has received tokens in the child mint.
    ?assert(balance(ChildState, Alice, Opts) > 0).

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
    Base = generate_base_process_state(
        generate_token_state(
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
        balance(AOCoreInvokedState, AliceAddr, Opts)
    ),
    ?assertEqual(
        Transfers,
        balance(AOCoreInvokedState, BobAddr, Opts)
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
        generate_base_process_state(
            generate_token_state(
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
    State = now(Base, Opts),
    NowEndTime = erlang:monotonic_time(millisecond),
    hb_test_utils:benchmark_print(
        <<"Process transfers">>,
        <<"transfers">>,
        Transfers,
        (NowEndTime - NowStartTime) / 1000
    ),
    ?assertEqual(Transfers, balance(State, BobAddr, Opts)),
    ?assertEqual(1_000_000_000 - Transfers, balance(State, AliceAddr, Opts)),
    ?assertEqual(
        1_000_000_000 * Accounts,
        hb_ao:get(<<"total-supply">>, State, Opts)
    ).