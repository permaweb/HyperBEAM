%%% @doc Test vectors and benchmarks for configurations of `~token@1.0',
%%% using the `~pot@1.0' mint device, as a `~process@1.0' message.
-module(dev_token_pot_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Test Helpers: State Accessors.
%%% ----------------------------------------------------------------------------

%% @doc Get balance for an account.
balance(Process, Req, Opts) ->
    dev_pot_lib:balance(Process, Req, Opts).

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
id(Wallet) -> dev_process_lib:wallet_id(Wallet).

%% @doc Return a signed token process with a `pot@1.0` mint device.
generate_token_pot(Opts) ->
    dev_pot_lib:pot(Opts).
generate_token_pot(ProcMsg, Opts) ->
    dev_pot_lib:pot(ProcMsg, Opts).

%% @doc Helper to create a pot resource with deposits
push_set_weight(Process, Resource, Weight, Opts) ->
    dev_pot_lib:register(Process, Resource, Weight, Opts).

push_transfer(Process, Sender, Recipient, Qty, Opts) ->
    dev_token_lib:transfer(Process, Sender, Recipient, Qty, Opts).

push_deposit(Process, Resource, User, Qty, Opts) ->
    dev_pot_lib:deposit(Process, Resource, User, Qty, Opts).

push_delegate(Process, Resource, User, ToAddr, Qty, Opts) ->
    dev_pot_lib:delegate(Process, Resource, User, ToAddr, Qty, Opts).

push_undelegate(Process, Resource, User, FromAddr, Qty, Opts) ->
    dev_pot_lib:delegate(Process, Resource, User, FromAddr, Qty, Opts).

%%% Test Cases.
%%% ----------------------------------------------------------------------------
%% @doc Basic test to see what happens when transfer is called with mint-device=pot
simple_pot_process_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    Process = 
        generate_token_pot(
            #{
                <<"balances">> => #{ id(Alice) => 1000 }
            },
            Opts
        ),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(Process, ResourceOxygen, Alice, 10, Opts),
    dev_token_lib:mint(Process, Opts),
    push_transfer(Process, Alice, Bob, 1, Opts),
    ?event(debug_test, {state, Process}, Opts),
    ?assertEqual(1, balance(Process, id(Bob),Opts)),
    ?assertEqual(8999, balance(Process, id(Alice), Opts)),
    ?assertEqual(9000, hb_ao:get(<<"now/total-supply">>, Process, Opts)).

pot_delegation_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Resource = <<"oxygen">>,
    Process = generate_token_pot(Opts),
    push_set_weight(Process, Resource, 100, Opts),
    push_deposit(Process, Resource, Alice, 100, Opts),
    push_delegate(Process, Resource, Alice, id(Bob), 10, Opts),
    ?assertEqual(10, dev_pot_lib:get_deposit(Process, Resource, Bob, Opts)),
    ?assertEqual(90, dev_pot_lib:get_deposit(Process, Resource, id(Alice), Opts)),
    ?assertEqual(100, dev_pot_lib:get_total_deposit(Process, Resource, Opts)).

balance_without_explicit_mint_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    Process = generate_token_pot(Opts),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(Process, ResourceOxygen, Alice, 10, Opts),
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
    Base = #{ <<"balances">> => #{ id(Alice) => 500 } },
    Process = generate_token_pot(Base, Opts),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(Process, ResourceOxygen, Alice, 10, Opts),
    % Advance time to generate yield
    % With mint_cap=10000, mint_prop={1,2}, going from t=0 to t=2:
    % ToMint = 10000 * (2^2 - 1^2) / 2^2 = 10000 * 3 / 4 = 7500
    % GlobalAcc = 0 + (5000 / 1000) = 7 (per weighted unit)
    % ResourceAcc = 0 + (7 * 100) = 700
    % Alice's yield = (700 - 0) * 10 = 7000 tokens!
    % % Try to transfer 700 tokens
    push_transfer(Process, Alice, Bob, 700, Opts),
    % Alice should have: (500 + 7000) - 700 = 6800
    % Bob should have: 700
    ?assertEqual(6800, balance(Process, id(Alice), Opts)),
    ?assertEqual(700, balance(Process, id(Bob), Opts)),
    % Total supply should be updated
    % Initial: 500, Minted: 7000, New total: 7500
    ?assertEqual(7500, hb_ao:get(<<"now/total-supply">>, Process, Opts)).

%% @doc Test direct claim_yield functionality from a single resource
claim_yield_single_resource_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    ProcBase = #{ <<"balances">> => #{ id(Alice) => 1000 } },
    Process = generate_token_pot(ProcBase, Opts),
    push_set_weight(Process, ResourceOxygen, 100, Opts),
    push_deposit(Process, ResourceOxygen, Alice, 10, Opts),
    dev_token_lib:mint(Process, Opts),
    BaseAfterClaim = dev_token_lib:now(Process, Opts),
    ?event({after_claim, BaseAfterClaim}),
    ?assertEqual(8000, balance(BaseAfterClaim, Alice, Opts)).

%% @doc Test claim_yield across multiple resources
claim_yield_multiple_resources_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    % Alice has deposits in two different resources
    Process = generate_token_pot(Opts),
    NewState = dev_token_lib:now(Process, Opts),
    push_set_weight(NewState, ResourceOxygen, 100, Opts),
    push_set_weight(NewState, ResourceHydrogen, 50, Opts),
    push_deposit(NewState, ResourceOxygen, Alice, 10, Opts),
    push_deposit(NewState, ResourceHydrogen, Alice, 5, Opts),
    dev_token_lib:mint(Process, Opts),
    FinalState = dev_token_lib:now(NewState, Opts),
    ?assertEqual(8750, balance(FinalState, id(Alice), Opts)).

%% @doc Test claim_yield when address has no deposits (edge case)
claim_yield_no_deposits_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    ProcBase = #{ <<"balances">> => #{ id(Alice) => 100 } },
    Process = generate_token_pot(ProcBase, Opts),
    dev_token_lib:mint(Process, Opts),
    BaseAfterClaim = dev_token_lib:now(Process, Opts),
    % Alice's balance should be unchanged (still 100)
    ?assertEqual(100, balance(BaseAfterClaim, Alice,Opts)),
    ?assertEqual(100, hb_ao:get(<<"total-supply">>, BaseAfterClaim, Opts)).

pot_subscriptions_test() ->
    Opts = test_opts(),
    Resource = <<"oxygen">>,
    % Generate a parent mint process and a child mint process.
    ParentProcess = generate_token_pot(Opts),
    ParentID = dev_process_lib:process_id(ParentProcess, Opts),
    % Generate a child mint with the parent ID.
    ChildProcess = generate_token_pot(#{ <<"parent">> => ParentID }, Opts),
    ChildID = dev_process_lib:process_id(ChildProcess, Opts),
    ?event(
        debug_test,
        {test_processes,
            {parent, ParentID},
            {child, ChildID}
        },
        Opts
    ),
    % Push an action on the child mint to initialize it, subsribing to all 
    % messages on the parent mint's set-weight action.
    dev_token_lib:mint(ChildProcess, Opts),
    ?assertEqual(
        [dev_process_lib:process_id(ChildProcess, Opts)],
        dev_process_lib:subscribers(ParentProcess, <<"register">>, Opts)
    ),
    % Push set-weight actions on the parent mint and verify that the child mint
    % also updates accordingly.
    push_set_weight(ParentProcess, Resource, 100, Opts),
    ?assertEqual(100, dev_pot_lib:get_weight(ParentProcess, Resource, Opts)),
    ?assertEqual(100, dev_pot_lib:get_weight(ChildProcess, Resource, Opts)),
    push_set_weight(ParentProcess, Resource, 200, Opts),
    ?assertEqual(200, dev_pot_lib:get_weight(ParentProcess, Resource, Opts)),
    ?assertEqual(200, dev_pot_lib:get_weight(ChildProcess, Resource, Opts)).

child_pot_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    % Create the parent mint, which will deliver units to the child mint.
    Resource = <<"stETH">>,
    ParentToken = generate_token_pot(#{ <<"mint-cap">> => 1_000_000 }, Opts),
    ParentID = dev_process_lib:process_id(ParentToken, Opts),
    ?event(process, {parent_mint, ParentID}, Opts),
    % Create the child mint, which will receive units from the parent mint in
    % exchange for its own tokens.
    ChildBase = #{ <<"mint-cap">> => 1_000_000,  <<"parent">> => ParentID },
    ChildToken = generate_token_pot(ChildBase, Opts),
    ChildID = dev_process_lib:process_id(ChildToken, Opts),
    dev_token_lib:mint(ChildToken, Opts),
    % Set the weights mints such that all units in the parent are given for
    % providing `stETH', and all units in the child are given for providing
    % `Parent'.
    push_set_weight(ParentToken, Resource, 1, Opts),
    % Deposit units of the resource into the parent mint for Alice.
    push_deposit(ParentToken, Resource, Alice, 2, Opts),
    % Delegate half of Alice's units in the parent mint to the child mint.
    Res = push_delegate(ParentToken, Resource, Alice, ChildID, 1, Opts),
    ?event(debug_test, {delegate_result, Res}, Opts),
    % Check that tokens are being minted in the parent for both the child token
    % and Alice.
    dev_token_lib:mint(ParentToken, Opts),
    dev_token_lib:mint(ChildToken, Opts),
    ParentState = dev_token_lib:now(ParentToken, Opts),
    ChildState = dev_token_lib:now(ChildToken, Opts),
    ?assert(balance(ParentState, Alice, Opts) > 0),
    ?assert(balance(ParentState, ChildID, Opts) > 0),
    % Check that Alice has received tokens in the child mint.
    ?event(debug_test,
        {states_after_mint,
            {parent, ParentState},
            {child, ChildState}
        },
        Opts
    ),
    ?assert(balance(ChildState, Alice, Opts) > 0).

%% @doc Test the viability of the `~mint-index@1.0` device, replicating delegation
%% choices of other users.
child_pots_with_index_test() ->
    Opts = test_opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Resource = <<"oxygen">>,
    % Create the parent mint, which will deliver units to the child mint.
    Parent = generate_token_pot(#{ <<"name">> => <<"Parent">> },Opts),
    ParentID = dev_process_lib:process_id(Parent, Opts),
    ?event(process, {parent_mint, ParentID}, Opts),
    % Create two child mints, which will receive units from the parent mint in
    % exchange for their own tokens.
    ChildA = 
        generate_token_pot(
            #{
                <<"name">> => <<"Child A">>,
                <<"parent">> => ParentID
            },
            Opts
        ),
    ChildAID = dev_process_lib:process_id(ChildA, Opts),
    dev_token_lib:mint(ChildA, Opts),
    ChildB = 
        generate_token_pot(
            #{
                <<"name">> => <<"Child B">>,
                <<"parent">> => ParentID
            },
            Opts
        ),
    ChildBID = dev_process_lib:process_id(ChildB, Opts),
    dev_token_lib:mint(ChildB, Opts),
    % Spawn the mint index, tracking delegations to `ChildAID' and `ChildBID'.
    IndexBase =
        #{
            <<"provider-mint-device">> => <<"mint-index@1.0">>,
            <<"indexed-mints">> => [ChildAID, ChildBID],
            <<"name">> => <<"Index">>,
            <<"parent">> => ParentID
        },
    Index = generate_token_pot(IndexBase, Opts),
    IndexID = dev_process_lib:process_id(Index, Opts),
    dev_token_lib:mint(Index, Opts),
    dev_token_lib:mint(ChildA, Opts),
    dev_token_lib:mint(ChildB, Opts),
    ?hr(),
    ?event(
        {network_map,
            {parent, ParentID},
            {child_a, ChildAID},
            {child_b, ChildBID},
            {index, IndexID},
            {alice, id(Alice)},
            {bob, id(Bob)}
        }
    ),
    ?hr(),
    ParentState1 = dev_token_lib:now(Parent, Opts),
    ?event(debug_test, {parent_state_after_index_init, ParentState1}, Opts),
    ?hr("DEPOSITING FOR ALICE AND BOB"),
    % Alice and Bob both deposit 10 of the resource.
    push_set_weight(Parent, Resource, 100, Opts),
    push_deposit(Parent, Resource, Alice, 10, Opts),
    push_deposit(Parent, Resource, Bob, 10, Opts),
    ?hr("ESTABLISHING DELEGATIONS"),
    % Let Alice delegate completely to the index, Bob splits equally between
    % ChildA and ChildB.
    push_delegate(Parent, Resource, Alice, IndexID, 10, Opts),
    push_delegate(Parent, Resource, Bob, ChildAID, 5, Opts),
    push_delegate(Parent, Resource, Bob, ChildBID, 5, Opts),
    ?hr("MINTING"),
    % Push a `mint` operation to the parent to force a mint with the new
    % delegations.
    dev_token_lib:mint(Parent, Opts),
    dev_token_lib:mint(ChildA, Opts),
    dev_token_lib:mint(ChildB, Opts),
    dev_token_lib:mint(Index, Opts),
    ?hr("VERIFYING"),
    % ParentState2 = dev_token_lib:now(Parent, Opts),
    % IndexState = dev_token_lib:now(Index, Opts),
    % ChildAState = dev_token_lib:now(ChildA, Opts),
    % ChildBState = dev_token_lib:now(ChildB, Opts),
    % ?event(
    %     debug_test,
    %     {
    %         final_network_state,
    %         {parent, ParentState2},
    %         {index, IndexState},
    %         {child_a, ChildAState},
    %         {child_b, ChildBState}
    %     }
    % ),
    % Ensure that the index process minted tokens in both of the child mints.
    ?assert(balance(ChildAID, IndexID, Opts) > 0),
    ?assert(balance(ChildBID, IndexID, Opts) > 0).