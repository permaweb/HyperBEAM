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
%% @doc Generate a base token state with default configuration.
generate_base_state() ->
    generate_base_state(#{}).
generate_base_state(Overrides) ->
    generate_base_state(Overrides, #{}).

generate_base_state(Params, Opts) ->
    ?event({generate_base_state, {params, Params}}),
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

generate_process_state(Params, Opts) ->
    Addr = hb_util:human_id(hb_opts:get(priv_wallet, no_wallet, Opts)),
    Extra = maps:get(extra, Params, #{}),
    ExtraWithProcBase =
        Extra#{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"execution-device">> => <<"token@1.0">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"push-device">> => <<"push@1.0">>,
            <<"scheduler">> => Addr,
            <<"authority">> => Addr
        },
    Base =
        generate_base_state(
            Params#{ extra => ExtraWithProcBase },
            Opts
        ),
    hb_message:commit(Base, Opts).

%% @doc Create a request message.
make_request(Action, Body) ->
    make_request(Action, Body, #{}).

make_request(Action, Body, Opts) ->
    From = maps:get(from, Opts, ?OWNER),
    Req = #{
        <<"path">> => <<"compute">>,
        <<"body">> => Body#{ <<"from">> => From, <<"action">> => Action }
    },
    ?event({make_request, {action, Action}, {from, From}}),
    Req.

schedule_request(State, Action, Body, Wallet, Opts) ->
    From = hb_util:human_id(Wallet),
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
    Res.

%% @doc Get balance for an account.
get_balance(State, Account) ->
    get_balance(State, Account, #{}).
get_balance(State, Account, Opts) ->
    Balances = hb_ao:get(<<"balances">>, State, Opts),
    hb_ao:get(Account, Balances, 0, Opts).

%%% Integration Test Helpers (Token + Pot)

%% @doc Generate pot fields for integration testing
%% Returns a map of pot state fields that can be merged into token state
%% Note: Only includes initial fields. Dynamic fields (minted, accumulator,
%% undistributed-mint) are created by drip_global()
generate_pot_fields(Params) ->
    generate_pot_fields(Params, #{}).

generate_pot_fields(Params, Opts) ->
    MintCap = hb_maps:get(mint_cap, Params, 10000, Opts),
    MintProp = hb_maps:get(mint_prop, Params, {1, 2}, Opts),
    Resources = hb_maps:get(resources, Params, #{}, Opts),
    % Calculate total weighted units from resources
    TWU = hb_maps:fold(
        fun(_ResourceID, Resource, Acc) ->
            Weight = hb_maps:get(<<"weight">>, Resource, 0, Opts),
            TotalDeposits = hb_maps:get(<<"total-deposits">>, Resource, 0, Opts),
            Acc + (Weight * TotalDeposits)
        end,
        0,
        Resources,
        Opts
    ),
    #{
        <<"mint-device">> => <<"pot@1.0">>,
        <<"t">> => hb_maps:get(t, Params, 0, Opts),
        <<"last-drip">> => hb_maps:get(last_drip, Params, 0, Opts),
        <<"mint-cap">> => MintCap,
        <<"mint-prop">> => MintProp,
        <<"total-weighted-units">> => TWU,
        <<"resources">> => Resources
    }.

%% @doc Helper to create a pot resource with deposits
%% Example: pot_resource(100, [{?ALICE, 10}, {?BOB, 5}])
pot_resource(Weight, UserDeposits) ->
    Deposits = hb_maps:from_list([
        {User, #{
            <<"quantity">> => Qty,
            <<"last-resource-accumulator">> => 0
        }}
        || {User, Qty} <- UserDeposits
    ]),
    TotalDeposits = lists:sum([Qty || {_User, Qty} <- UserDeposits]),
    #{
        <<"weight">> => Weight,
        <<"accumulator">> => 0,
        <<"last-global-accumulator">> => 0,
        <<"total-deposits">> => TotalDeposits,
        <<"deposits">> => Deposits
    }.

%% @doc Generate integrated token+pot state
generate_integrated_state(Params) ->
    generate_integrated_state(Params, #{}).

generate_integrated_state(Params, Opts) ->
    PotResources = hb_maps:get(pot_resources, Params, #{}, Opts),
    PotParams = hb_maps:with(
        [mint_cap, mint_prop, t, last_drip],
        Params,
        Opts
    ),
    PotFields = generate_pot_fields(PotParams#{resources => PotResources}, Opts),
    % Generate base token state with pot fields merged in
    ExistingExtra = hb_maps:get(extra, Params, #{}, Opts),
    MergedExtra = hb_maps:merge(ExistingExtra, PotFields, Opts),
    generate_base_state(Params#{extra => MergedExtra}, Opts).

%%% Transfer Tests

transfer_basic_test() ->
    ?event({starting_test, transfer_basic_test}),
    hb:init(),
    % Setup: Alice has 1000 tokens
    ?event({generating_base_state}),
    Base = generate_base_state(#{
        total_supply => 1000,
        initial_balances => #{?ALICE => 1000}
    }),
    ?event({base_state_ready}),
    % Transfer 300 from Alice to Bob
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 300
        },
        #{from => ?ALICE}
    ),
    ?event({calling_compute}),
    ?event({base_state_balances, 
        hb_ao:get(<<"balances">>, Base, #{}), 
        Base, 
        Req
    }),
    ComputeResult = dev_token:compute(Base, Req, #{}),
    ?event({compute_result, ComputeResult}),
    {ok, NewState} = ComputeResult,
    % Verify balances
    ?event({verifying_balances}),
    ?assertEqual(700, get_balance(NewState, ?ALICE)),
    ?assertEqual(300, get_balance(NewState, ?BOB)),
    % Verify total supply unchanged
    ?assertEqual(1000, hb_ao:get(<<"total-supply">>, NewState, #{})),
    ?event({test_complete, transfer_basic_test}).

transfer_insufficient_balance_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 100}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 200
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, <<"Insufficient balance.">>}, Result).

transfer_to_self_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 500
        },
        #{from => ?ALICE}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1000, get_balance(NewState, ?ALICE)).

%%% Mint Tests

mint_single_authorized_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 1000
        },
        #{from => ?MINTER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1000, get_balance(NewState, ?ALICE)),
    ?assertEqual(1000, hb_ao:get(<<"total-supply">>, NewState, #{})).

mint_batch_test() ->
    hb:init(),
    Base = generate_base_state(#{
        total_supply => 100,
        initial_balances => #{?ALICE => 100}
    }),
    Quantities = #{
            ?ALICE => 400,
            ?BOB => 300,
            ?CHARLIE => 200
        },
    Req = make_request(
        <<"mint">>,
        #{
            <<"quantities">> => Quantities,
            <<"mode">> => <<"batch">>
        },
        #{from => ?MINTER}
    ),
    ?event({base, Base, req, Req}),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(500, get_balance(NewState, ?ALICE)),
    ?assertEqual(300, get_balance(NewState, ?BOB)),
    ?assertEqual(200, get_balance(NewState, ?CHARLIE)),
    ?assertEqual(1000, hb_ao:get(<<"total-supply">>, NewState, #{})).

mint_to_existing_balance_test() ->
    hb:init(),
    Base = generate_base_state(#{
        total_supply => 1000,
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 500
        },
        #{from => ?MINTER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1500, get_balance(NewState, ?ALICE)),
    ?assertEqual(1500, hb_ao:get(<<"total-supply">>, NewState, #{})).

%%% Secure Set Tests

secure_set_by_owner_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"set">>,
        #{
            <<"name">> => <<"New Token Name">>,
            <<"ticker">> => <<"NEW">>
        },
        #{from => ?OWNER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(<<"New Token Name">>, hb_ao:get(<<"name">>, NewState, #{})),
    ?assertEqual(<<"NEW">>, hb_ao:get(<<"ticker">>, NewState, #{})).

secure_set_by_minter_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"set">>,
        #{
            <<"description">> => <<"Token description">>
        },
        #{from => ?MINTER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(
        <<"Token description">>,
        hb_ao:get(<<"description">>, NewState, #{})
    ).

secure_set_unauthorized_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"set">>,
        #{
            <<"name">> => <<"Hacked Name">>
        },
        #{from => ?BOB}  % Bob is neither owner nor minter
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, <<"Set authority mismatch.">>}, Result).

%%% Edge Cases and Error Handling

transfer_zero_amount_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 0
        },
        #{from => ?ALICE}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1000, get_balance(NewState, ?ALICE)),
    ?assertEqual(0, get_balance(NewState, ?BOB)).

mint_zero_amount_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 0
        },
        #{from => ?MINTER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(0, get_balance(NewState, ?ALICE)),
    ?assertEqual(0, hb_ao:get(<<"total-supply">>, NewState, #{})).

unsupported_action_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"burn">>,  % Unsupported action
        #{<<"quantity">> => 100},
        #{from => ?OWNER}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({ok, Base}, Result).

%%% Additional Edge Cases - Transfer Boundaries
transfer_to_new_account_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?CHARLIE,
            <<"quantity">> => 250
        },
        #{from => ?ALICE}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(750, get_balance(NewState, ?ALICE)),
    ?assertEqual(250, get_balance(NewState, ?CHARLIE)).

transfer_exact_balance_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 500}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 500
        },
        #{from => ?ALICE}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(0, get_balance(NewState, ?ALICE)),
    ?assertEqual(500, get_balance(NewState, ?BOB)).

transfer_exceeds_by_one_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 100}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 101
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, <<"Insufficient balance.">>}, Result).

transfer_one_token_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 100}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 1
        },
        #{from => ?ALICE}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(99, get_balance(NewState, ?ALICE)),
    ?assertEqual(1, get_balance(NewState, ?BOB)).

transfer_large_amount_test() ->
    hb:init(),
    LargeAmount = 1_000_000_000_000,
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => LargeAmount}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => LargeAmount div 2
        },
        #{from => ?ALICE}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(LargeAmount div 2, get_balance(NewState, ?ALICE)),
    ?assertEqual(LargeAmount div 2, get_balance(NewState, ?BOB)).

%%% Sequential Transfer Operations
transfer_multiple_sequential_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    % Transfer 1: Alice → Bob
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 300
        },
        #{from => ?ALICE}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(700, get_balance(State1, ?ALICE)),
    ?assertEqual(300, get_balance(State1, ?BOB)),
    % Transfer 2: Alice → Charlie
    Req2 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?CHARLIE,
            <<"quantity">> => 200
        },
        #{from => ?ALICE}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    ?assertEqual(500, get_balance(State2, ?ALICE)),
    ?assertEqual(300, get_balance(State2, ?BOB)),
    ?assertEqual(200, get_balance(State2, ?CHARLIE)),
    % Transfer 3: Bob → Charlie
    Req3 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?BOB,
            <<"recipient">> => ?CHARLIE,
            <<"quantity">> => 100
        },
        #{from => ?BOB}
    ),
    {ok, State3} = dev_token:compute(State2, Req3, #{}),
    ?assertEqual(500, get_balance(State3, ?ALICE)),
    ?assertEqual(200, get_balance(State3, ?BOB)),
    ?assertEqual(300, get_balance(State3, ?CHARLIE)).

transfer_all_then_fail_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 100}
    }),
    % Transfer all balance
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(0, get_balance(State1, ?ALICE)),
    ?assertEqual(100, get_balance(State1, ?BOB)),
    % Try to transfer again - should fail
    Req2 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 1
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(State1, Req2, #{}),
    ?assertMatch({error, <<"Insufficient balance.">>}, Result).

transfer_circular_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{
            ?ALICE => 300,
            ?BOB => 300,
            ?CHARLIE => 300
        }
    }),
    % Alice → Bob
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    % Bob → Charlie
    Req2 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?BOB,
            <<"recipient">> => ?CHARLIE,
            <<"quantity">> => 100
        },
        #{from => ?BOB}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    % Charlie → Alice (completing circle)
    Req3 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?CHARLIE,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 100
        },
        #{from => ?CHARLIE}
    ),
    {ok, State3} = dev_token:compute(State2, Req3, #{}),
    % All should be back to 300
    ?assertEqual(300, get_balance(State3, ?ALICE)),
    ?assertEqual(300, get_balance(State3, ?BOB)),
    ?assertEqual(300, get_balance(State3, ?CHARLIE)).

%% @doc  Mint Edge Cases
mint_to_new_account_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?CHARLIE,
            <<"quantity">> => 500
        },
        #{from => ?MINTER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(500, get_balance(NewState, ?CHARLIE)),
    ?assertEqual(500, hb_ao:get(<<"total-supply">>, NewState, #{})).

mint_one_token_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 1
        },
        #{from => ?MINTER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1, get_balance(NewState, ?ALICE)),
    ?assertEqual(1, hb_ao:get(<<"total-supply">>, NewState, #{})).

mint_large_quantity_test() ->
    hb:init(),
    Base = generate_base_state(),
    LargeQuantity = 1_000_000_000_000_000,
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => LargeQuantity
        },
        #{from => ?MINTER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(LargeQuantity, get_balance(NewState, ?ALICE)),
    ?assertEqual(LargeQuantity, hb_ao:get(<<"total-supply">>, NewState, #{})).

mint_multiple_to_same_account_test() ->
    hb:init(),
    Base = generate_base_state(),
    % First mint
    Req1 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 500
        },
        #{from => ?MINTER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(500, get_balance(State1, ?ALICE)),
    ?assertEqual(500, hb_ao:get(<<"total-supply">>, State1, #{})),
    % Second mint
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 300
        },
        #{from => ?MINTER}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    ?assertEqual(800, get_balance(State2, ?ALICE)),
    ?assertEqual(800, hb_ao:get(<<"total-supply">>, State2, #{})),
    % Third mint
    Req3 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 200
        },
        #{from => ?MINTER}
    ),
    {ok, State3} = dev_token:compute(State2, Req3, #{}),
    ?assertEqual(1000, get_balance(State3, ?ALICE)),
    ?assertEqual(1000, hb_ao:get(<<"total-supply">>, State3, #{})).

mint_batch_single_recipient_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"batch">>,
            <<"quantities">> => #{?ALICE => 1000}
        },
        #{from => ?MINTER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1000, get_balance(NewState, ?ALICE)),
    ?assertEqual(1000, hb_ao:get(<<"total-supply">>, NewState, #{})).

%%% Combined Operations
mint_then_transfer_test() ->
    hb:init(),
    Base = generate_base_state(),
    % Mint to Alice
    MintReq = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 1000
        },
        #{from => ?MINTER}
    ),
    {ok, State1} = dev_token:compute(Base, MintReq, #{}),
    ?assertEqual(1000, get_balance(State1, ?ALICE)),
    % Transfer from Alice to Bob
    TransferReq = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 400
        },
        #{from => ?ALICE}
    ),
    {ok, State2} = dev_token:compute(State1, TransferReq, #{}),
    ?assertEqual(600, get_balance(State2, ?ALICE)),
    ?assertEqual(400, get_balance(State2, ?BOB)),
    ?assertEqual(1000, hb_ao:get(<<"total-supply">>, State2, #{})).

transfer_then_mint_test() ->
    hb:init(),
    Base = generate_base_state(#{
        total_supply => 1000,
        initial_balances => #{?ALICE => 1000}
    }),
    % Transfer from Alice to Bob
    TransferReq = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 300
        },
        #{from => ?ALICE}
    ),
    {ok, State1} = dev_token:compute(Base, TransferReq, #{}),
    ?assertEqual(700, get_balance(State1, ?ALICE)),
    ?assertEqual(300, get_balance(State1, ?BOB)),
    % Mint to Alice
    MintReq = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 500
        },
        #{from => ?MINTER}
    ),
    {ok, State2} = dev_token:compute(State1, MintReq, #{}),
    ?assertEqual(1200, get_balance(State2, ?ALICE)),
    ?assertEqual(300, get_balance(State2, ?BOB)),
    ?assertEqual(1500, hb_ao:get(<<"total-supply">>, State2, #{})).

account_lifecycle_test() ->
    hb:init(),
    Base = generate_base_state(),
    % Start: Account has 0
    ?assertEqual(0, get_balance(Base, ?ALICE)),
    % Mint to account
    Req1 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 1000
        },
        #{from => ?MINTER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(1000, get_balance(State1, ?ALICE)),
    % Transfer out all
    Req2 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 1000
        },
        #{from => ?ALICE}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    ?assertEqual(0, get_balance(State2, ?ALICE)),
    % Mint again
    Req3 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 500
        },
        #{from => ?MINTER}
    ),
    {ok, State3} = dev_token:compute(State2, Req3, #{}),
    ?assertEqual(500, get_balance(State3, ?ALICE)),
    ?assertEqual(1500, hb_ao:get(<<"total-supply">>, State3, #{})).

%% @doc  Secure Set Edge Cases
secure_set_multiple_fields_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"set">>,
        #{
            <<"name">> => <<"Updated Token">>,
            <<"ticker">> => <<"UPD">>,
            <<"denomination">> => 18,
            <<"description">> => <<"A comprehensive test token">>
        },
        #{from => ?OWNER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(<<"Updated Token">>, hb_ao:get(<<"name">>, NewState, #{})),
    ?assertEqual(<<"UPD">>, hb_ao:get(<<"ticker">>, NewState, #{})),
    ?assertEqual(18, hb_ao:get(<<"denomination">>, NewState, #{})),
    ?assertEqual(<<"A comprehensive test token">>,
                 hb_ao:get(<<"description">>, NewState, #{})).

secure_set_custom_fields_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"set">>,
        #{
            <<"logo">> => <<"https://example.com/logo.png">>,
            <<"website">> => <<"https://example.com">>,
            <<"social-twitter">> => <<"@testtoken">>
        },
        #{from => ?OWNER}
    ),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(<<"https://example.com/logo.png">>,
                 hb_ao:get(<<"logo">>, NewState, #{})),
    ?assertEqual(<<"https://example.com">>,
                 hb_ao:get(<<"website">>, NewState, #{})),
    ?assertEqual(<<"@testtoken">>,
                 hb_ao:get(<<"social-twitter">>, NewState, #{})).

secure_set_sequential_test() ->
    hb:init(),
    Base = generate_base_state(),
    % First update
    Req1 = make_request(
        <<"set">>,
        #{
            <<"name">> => <<"First Name">>
        },
        #{from => ?OWNER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(<<"First Name">>, hb_ao:get(<<"name">>, State1, #{})),
    % Second update
    Req2 = make_request(
        <<"set">>,
        #{
            <<"name">> => <<"Second Name">>
        },
        #{from => ?OWNER}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    ?assertEqual(<<"Second Name">>, hb_ao:get(<<"name">>, State2, #{})).

%% @doc  State Consistency Tests
total_supply_consistency_test() ->
    hb:init(),
    InitialSupply = 10000,
    Base = generate_base_state(#{
        total_supply => InitialSupply,
        initial_balances => #{
            ?ALICE => 5000,
            ?BOB => 3000,
            ?CHARLIE => 2000
        }
    }),
    % Multiple transfers
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 1000
        },
        #{from => ?ALICE}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(InitialSupply, hb_ao:get(<<"total-supply">>, State1, #{})),
    Req2 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?CHARLIE,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 500
        },
        #{from => ?CHARLIE}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    ?assertEqual(InitialSupply, hb_ao:get(<<"total-supply">>, State2, #{})),
    Req3 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?BOB,
            <<"recipient">> => ?CHARLIE,
            <<"quantity">> => 2000
        },
        #{from => ?BOB}
    ),
    {ok, State3} = dev_token:compute(State2, Req3, #{}),
    ?assertEqual(InitialSupply, hb_ao:get(<<"total-supply">>, State3, #{})).

total_supply_increases_with_mint_test() ->
    hb:init(),
    Base = generate_base_state(#{
        total_supply => 1000,
        initial_balances => #{?ALICE => 1000}
    }),
    % First mint
    Req1 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?BOB,
            <<"quantity">> => 500
        },
        #{from => ?MINTER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(1500, hb_ao:get(<<"total-supply">>, State1, #{})),
    % Second mint
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?CHARLIE,
            <<"quantity">> => 300
        },
        #{from => ?MINTER}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    ?assertEqual(1800, hb_ao:get(<<"total-supply">>, State2, #{})),
    % Batch mint
    Req3 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"batch">>,
            <<"quantities">> => #{
                ?ALICE => 100,
                ?BOB => 100
            }
        },
        #{from => ?MINTER}
    ),
    {ok, State3} = dev_token:compute(State2, Req3, #{}),
    ?assertEqual(2000, hb_ao:get(<<"total-supply">>, State3, #{})).

%% @doc Sum of all balances must equal minted supply at all times
sum_of_balances_equals_minted_supply_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000, ?BOB => 500},
        total_supply => 2000
    }),
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE, 
            <<"recipient">> => ?CHARLIE, 
            <<"quantity">> => 300
        },
        #{from => ?ALICE}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    Sum1 = 
        get_balance(State1, ?ALICE) + 
        get_balance(State1, ?BOB) + 
        get_balance(State1, ?CHARLIE),
    ?assertEqual(1500, Sum1),
    ?assertEqual(2000, hb_ao:get(<<"total-supply">>, State1, #{})),
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => <<"dave-id">>, 
            <<"quantity">> => 500
        },
        #{from => ?MINTER}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    Sum2 = 
        get_balance(State2, ?ALICE) + 
        get_balance(State2, ?BOB) + 
        get_balance(State2, ?CHARLIE) + 
        get_balance(State2, <<"dave-id">>),
    ?assertEqual(2000, Sum2),
    ?assertEqual(2500, hb_ao:get(<<"total-supply">>, State2, #{})).

%% @doc Operations with very large numbers maintain exact precision
no_precision_loss_large_numbers_test() ->
    hb:init(),
    LargeAmount = 1_000_000_000_000_000,
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => LargeAmount},
        total_supply => LargeAmount
    }),
    TransferAmount = 999_999_999_999_999,
    Req = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => TransferAmount
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1, get_balance(State, ?ALICE)),
    ?assertEqual(TransferAmount, get_balance(State, ?BOB)),
    ?assertEqual(LargeAmount, hb_ao:get(<<"total-supply">>, State, #{})).

%% @doc Failed operations never leak or create tokens
failed_operations_preserve_supply_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 100},
        total_supply => 100
    }),
    TotalSupplyBefore = hb_ao:get(<<"total-supply">>, Base, #{}),
    AliceBalanceBefore = get_balance(Base, ?ALICE),
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => 200
        },
        #{from => ?ALICE}
    ),
    {error, _} = dev_token:compute(Base, Req1, #{}),
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => 500
        },
        #{from => ?BOB}
    ),
    {error, _} = dev_token:compute(Base, Req2, #{}),
    ?assertEqual(TotalSupplyBefore, hb_ao:get(<<"total-supply">>, Base, #{})),
    ?assertEqual(AliceBalanceBefore, get_balance(Base, ?ALICE)),
    ?assertEqual(0, get_balance(Base, ?BOB)).

%% @doc Balance arithmetic near max safe integer boundaries is handled correctly
overflow_protection_test() ->
    hb:init(),
    MaxSafe = 9_007_199_254_740_991,
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => MaxSafe},
        total_supply => MaxSafe
    }),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?ALICE, 
            <<"quantity">> => MaxSafe
        },
        #{from => ?MINTER}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ExpectedBalance = MaxSafe + MaxSafe,
    ?assertEqual(ExpectedBalance, get_balance(State, ?ALICE)),
    ?assertEqual(ExpectedBalance, hb_ao:get(<<"total-supply">>, State, #{})).

%% @doc Only the designated minter can mint tokens
only_minter_can_mint_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req1 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?ALICE, 
            <<"quantity">> => 1000
        },
        #{from => ?ALICE}
    ),
    ?assertMatch(
        {error, <<"Mint authority mismatch.">>}, 
        dev_token:compute(Base, Req1, #{})
    ),
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => 1000
        },
        #{from => ?BOB}
    ),
    ?assertMatch(
        {error, <<"Mint authority mismatch.">>}, 
        dev_token:compute(Base, Req2, #{})
    ),
    Req3 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?CHARLIE, 
            <<"quantity">> => 1000
        },
        #{from => ?MINTER}
    ),
    ?assertMatch({ok, _}, dev_token:compute(Base, Req3, #{})).

%% @doc Secure_set cannot be used to directly modify balances
cant_directly_modify_balances_via_set_test_disabled() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    AliceBalanceBefore = get_balance(Base, ?ALICE),
    Req = make_request(
        <<"set">>,
        #{
            <<"balances">> => #{
                <<"device">> => <<"trie@1.0">>, 
                ?ALICE => 999999
            }
        },
        #{from => ?OWNER}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    NewBalance = get_balance(State, ?ALICE),
    case NewBalance =:= 999999 of
        true ->
            ?event(security_warning, 
                {balance_manipulation_possible_via_set, 
                    {original, AliceBalanceBefore}, 
                    {new, NewBalance}
                }
            ),
            ?assert(false);
        false ->
            ?assertEqual(AliceBalanceBefore, NewBalance)
    end.

%% @doc Secure_set cannot manipulate total-supply
cant_directly_modify_supply_via_set_test_disabled() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000},
        total_supply => 1000
    }),
    Req = make_request(
        <<"set">>,
        #{
            <<"total-supply">> => 999_999_999
        },
        #{from => ?OWNER}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    NewSupply = hb_ao:get(<<"total-supply">>, State, #{}),
    case NewSupply =:= 999_999_999 of
        true ->
            ?event(security_warning, 
                {supply_manipulation_possible_via_set, 
                    {original, 1000}, 
                    {new, NewSupply}
                }
            ),
            ?assert(false);
        false ->
            ?assertEqual(1000, NewSupply)
    end.

%% @doc Authority changes are enforced immediately
authority_change_enforced_immediately_test() ->
    hb:init(),
    Base = generate_base_state(),
    NewMinter = <<"new-minter-id">>,
    Req1 = make_request(
        <<"set">>,
        #{ <<"mint-authority">> => NewMinter },
        #{from => ?OWNER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(NewMinter, hb_ao:get(<<"mint-authority">>, State1, #{})),
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?ALICE, 
            <<"quantity">> => 100},
        #{from => ?MINTER}
    ),
    ?assertMatch(
        {error, <<"Mint authority mismatch.">>}, 
        dev_token:compute(State1, Req2, #{})
    ),
    Req3 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?ALICE, 
            <<"quantity">> => 100
        },
        #{from => NewMinter}
    ),
    ?assertMatch({ok, _}, dev_token:compute(State1, Req3, #{})).

%% @doc State remains unchanged after operation failure
state_unchanged_on_failure_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 100}
    }),
    BaseSupply = hb_ao:get(<<"total-supply">>, Base, #{}),
    BaseBalance = get_balance(Base, ?ALICE),
    BaseName = hb_ao:get(<<"name">>, Base, #{}),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => 200
        },
        #{from => ?ALICE}
    ),
    {error, _} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(BaseSupply, hb_ao:get(<<"total-supply">>, Base, #{})),
    ?assertEqual(BaseBalance, get_balance(Base, ?ALICE)),
    ?assertEqual(0, get_balance(Base, ?BOB)),
    ?assertEqual(BaseName, hb_ao:get(<<"name">>, Base, #{})).

%% @doc Trie balances persist correctly across operations
trie_balances_persist_correctly_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req1 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?ALICE, 
            <<"quantity">> => 500
        },
        #{from => ?MINTER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(500, get_balance(State1, ?ALICE)),
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => 300
        },
        #{from => ?MINTER}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    ?assertEqual(500, get_balance(State2, ?ALICE)),
    ?assertEqual(300, get_balance(State2, ?BOB)),
    Req3 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE, 
            <<"recipient">> => ?CHARLIE, 
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    {ok, State3} = dev_token:compute(State2, Req3, #{}),
    ?assertEqual(400, get_balance(State3, ?ALICE)),
    ?assertEqual(300, get_balance(State3, ?BOB)),
    ?assertEqual(100, get_balance(State3, ?CHARLIE)).

%% @doc Notices match actual state changes
notices_match_actual_state_changes_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 300
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    Outbox = hb_ao:get(<<"results/outbox">>, State, #{}),
    ?assertEqual(2, length(Outbox)),
    % Notice order: [DebitNotice, CreditNotice]
    [DebitNotice, CreditNotice] = Outbox,
    % Validate Debit-Notice structure
    ?assertEqual(
        <<"Debit-Notice">>,
        hb_ao:get(<<"action">>, DebitNotice, #{})
    ),
    ?assertEqual(?BOB, hb_ao:get(<<"recipient">>, DebitNotice, #{})),
    ?assertEqual(300, hb_ao:get(<<"quantity">>, DebitNotice, #{})),
    ?assertEqual(?ALICE, hb_ao:get(<<"target">>, DebitNotice, #{})), 
    % Validate Credit-Notice structure
    ?assertEqual(
        <<"Credit-Notice">>,
        hb_ao:get(<<"action">>, CreditNotice, #{})
    ),
    ?assertEqual(?ALICE, hb_ao:get(<<"sender">>, CreditNotice, #{})),
    ?assertEqual(300, hb_ao:get(<<"quantity">>, CreditNotice, #{})),
    ?assertEqual(?BOB, hb_ao:get(<<"target">>, CreditNotice, #{})),   
    % Validate balances updated correctly
    ?assertEqual(700, get_balance(State, ?ALICE)),
    ?assertEqual(300, get_balance(State, ?BOB)).

%% @doc X- prefixed tags are forwarded to transfer notices
transfer_forwards_x_tags_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 100,
            <<"X-Correlation-ID">> => <<"abc123">>,
            <<"x-request-id">> => <<"req-456">>,
            <<"X-Custom-Tag">> => <<"custom-value">>,
            <<"regular-tag">> => <<"should-not-forward">>,
            <<"Y-Tag">> => <<"also-not-forwarded">>
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    Outbox = hb_ao:get(<<"results/outbox">>, State, #{}),
    ?assertEqual(2, length(Outbox)),
    [DebitNotice, CreditNotice] = Outbox,
    ?assertEqual(
        <<"abc123">>, 
        hb_ao:get(<<"X-Correlation-ID">>, DebitNotice, #{})
    ),
    ?assertEqual(
        <<"req-456">>, 
        hb_ao:get(<<"x-request-id">>, DebitNotice, #{})
    ),
    ?assertEqual(
        <<"custom-value">>, 
        hb_ao:get(<<"X-Custom-Tag">>, DebitNotice, #{})
    ),
    ?assertEqual(
        <<"abc123">>, 
        hb_ao:get(<<"X-Correlation-ID">>, CreditNotice, #{})
    ),
    ?assertEqual(
        <<"req-456">>, 
        hb_ao:get(<<"x-request-id">>, CreditNotice, #{})
    ),
    ?assertEqual(
        <<"custom-value">>, 
        hb_ao:get(<<"X-Custom-Tag">>, CreditNotice, #{})
    ),
    ?assertEqual(
        not_found, 
        hb_ao:get(<<"regular-tag">>, DebitNotice, #{})
    ),
    ?assertEqual(
        not_found, 
        hb_ao:get(<<"Y-Tag">>, DebitNotice, #{})
    ),
    ?assertEqual(
        not_found, 
        hb_ao:get(<<"regular-tag">>, CreditNotice, #{})
    ),
    ?assertEqual(
        not_found, 
        hb_ao:get(<<"Y-Tag">>, CreditNotice, #{})
    ).

%% @doc Edge cases for X- tag forwarding
transfer_x_tags_edge_cases_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 50,
            <<"X-">> => <<"just-x-dash">>,  
            <<"X">> => <<"no-dash">>,        
            <<"x-empty">> => <<>>,           
            <<"X-123">> => <<"numeric">>,   
            <<"X-With-Dashes">> => <<"val">> 
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    Outbox = hb_ao:get(<<"results/outbox">>, State, #{}),
    [DebitNotice, CreditNotice] = Outbox,
    ?assertEqual(<<"just-x-dash">>, hb_ao:get(<<"X-">>, DebitNotice, #{})),
    ?assertEqual(<<"just-x-dash">>, hb_ao:get(<<"X-">>, CreditNotice, #{})),
    ?assertEqual(not_found, hb_ao:get(<<"X">>, DebitNotice, #{})),
    ?assertEqual(not_found, hb_ao:get(<<"X">>, CreditNotice, #{})),
    ?assertEqual(<<>>, hb_ao:get(<<"x-empty">>, DebitNotice, #{})),
    ?assertEqual(<<>>, hb_ao:get(<<"x-empty">>, CreditNotice, #{})),
    ?assertEqual(<<"numeric">>, hb_ao:get(<<"X-123">>, DebitNotice, #{})),
    ?assertEqual(<<"numeric">>, hb_ao:get(<<"X-123">>, CreditNotice, #{})),
    ?assertEqual(<<"val">>, hb_ao:get(<<"X-With-Dashes">>, DebitNotice, #{})),
    ?assertEqual(<<"val">>, hb_ao:get(<<"X-With-Dashes">>, CreditNotice, #{})).

%% @doc Batch mint generates one notice per recipient
batch_mint_notice_count_matches_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"batch">>, 
            <<"quantities">> => #{
                ?ALICE => 100, 
                ?BOB => 200, 
                ?CHARLIE => 300
            }
        },
        #{from => ?MINTER}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    Outbox = hb_ao:get(<<"results/outbox">>, State, #{}),
    ?assertEqual(3, length(Outbox)),
    lists:foreach(
        fun(Notice) ->
            ?assertEqual(
                <<"Mint-Notice">>, 
                hb_ao:get(<<"action">>, Notice, #{})
            )
        end,
        Outbox
    ).

% @doc Missing required fields cause clear errors
missing_fields_validated_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    ?assertMatch({error, _}, dev_token:compute(Base, Req1, #{})),
    Req2 = make_request(
        <<"mint">>,
        #{<<"mode">> => <<"single">>, <<"quantity">> => 100},
        #{from => ?MINTER}
    ),
    ?assertMatch({error, _}, dev_token:compute(Base, Req2, #{})).

%% @doc Negative quantities are rejected
negative_quantity_rejected_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => -100
        },
        #{from => ?ALICE}
    ),
    ?assertMatch({error, _}, dev_token:compute(Base, Req1, #{})),
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?ALICE, 
            <<"quantity">> => -500
        },
        #{from => ?MINTER}
    ),
    ?assertMatch({error, _}, dev_token:compute(Base, Req2, #{})).

%% @doc Operations on zero balance accounts handled correctly
zero_balance_account_operations_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 0}
    }),
    ?assertEqual(0, get_balance(Base, ?ALICE)),
    Req1 = make_request(
        <<"transfer">>,
        #{
            <<"from">> => ?ALICE, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => 1
        },
        #{from => ?ALICE}
    ),
    ?assertMatch(
        {error, <<"Insufficient balance.">>}, 
        dev_token:compute(Base, Req1, #{})
    ),
    Req2 = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>, 
            <<"recipient">> => ?ALICE, 
            <<"quantity">> => 100
        },
        #{from => ?MINTER}
    ),
    {ok, State} = dev_token:compute(Base, Req2, #{}),
    ?assertEqual(100, get_balance(State, ?ALICE)).

%% @doc Empty batch mint is handled gracefully
empty_batch_mint_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"batch">>, 
            <<"quantities">> => #{}
        },
        #{from => ?MINTER}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(0, hb_ao:get(<<"total-supply">>, State, #{})),
    Outbox = hb_ao:get(<<"results/outbox">>, State, #{}),
    ?assertEqual(0, length(Outbox)).

%% @doc Invalid mint mode is rejected
invalid_mint_mode_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"invalid">>, 
            <<"recipient">> => ?ALICE, 
            <<"quantity">> => 100
        },
        #{from => ?MINTER}
    ),
    ?assertMatch(
        {error, <<"Invalid mint mode.">>},
        dev_token:compute(Base, Req, #{})
    ).

%% @doc Both owner and minter can successfully use secure_set
both_owner_and_minter_can_set_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req1 = make_request(
        <<"set">>,
        #{ <<"name">> => <<"Owner Updated">> },
        #{from => ?OWNER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(<<"Owner Updated">>, hb_ao:get(<<"name">>, State1, #{})),
    Req2 = make_request(
        <<"set">>,
        #{ <<"ticker">> => <<"MINT">> },
        #{from => ?MINTER}
    ),
    {ok, State2} = dev_token:compute(State1, Req2, #{}),
    ?assertEqual(<<"MINT">>, hb_ao:get(<<"ticker">>, State2, #{})),
    ?assertEqual(<<"Owner Updated">>, hb_ao:get(<<"name">>, State2, #{})).

%% @doc Owner change revokes old owner's set permissions
owner_change_revokes_old_owner_test() ->
    hb:init(),
    Base = generate_base_state(),
    NewOwner = <<"new-owner-id">>,
    Req1 = make_request(
        <<"set">>,
        #{ <<"owner">> => NewOwner },
        #{from => ?OWNER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(NewOwner, hb_ao:get(<<"owner">>, State1, #{})),
    Req2 = make_request(
        <<"set">>,
        #{ <<"name">> => <<"Old Owner Try">> },
        #{from => ?OWNER}
    ),
    ?assertMatch(
        {error, <<"Set authority mismatch.">>}, 
        dev_token:compute(State1, Req2, #{})
    ),
    Req3 = make_request(
        <<"set">>,
        #{ <<"name">> => <<"New Owner Success">> },
        #{from => NewOwner}
    ),
    {ok, State2} = dev_token:compute(State1, Req3, #{}),
    ?assertEqual(<<"New Owner Success">>, hb_ao:get(<<"name">>, State2, #{})).

%% @doc Transfer to single character address should succeed
transfer_to_single_char_address_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    SingleChar = <<"_">>,
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => SingleChar,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(900, get_balance(State, ?ALICE)),
    ?assertEqual(100, get_balance(State, SingleChar)).

%% @doc Transfer to short address (7 chars) should succeed
transfer_to_short_address_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    ShortAddr = <<"BADFOOD">>,
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ShortAddr,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(900, get_balance(State, ?ALICE)),
    ?assertEqual(100, get_balance(State, ShortAddr)).

%% @doc Transfer to maximum length address (128 chars) should succeed
transfer_to_max_length_address_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    % 128-char address - use list_to_binary with duplicate
    MaxAddr = list_to_binary(lists:duplicate(128, $A)),
    ?assertEqual(128, byte_size(MaxAddr)),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => MaxAddr,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(900, get_balance(State, ?ALICE)),
    ?assertEqual(100, get_balance(State, MaxAddr)).

%% @doc Transfer to standard Arweave address
transfer_to_arweave_address_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    ArweaveAddr = <<"1seRanklLU_1VTGkEk7P0xAwMJfA7owA1JHW5KyZKlY">>,
    ?assertEqual(43, byte_size(ArweaveAddr)),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ArweaveAddr,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(900, get_balance(State, ?ALICE)),
    ?assertEqual(100, get_balance(State, ArweaveAddr)).

%% @doc Transfer to empty address should fail
transfer_to_empty_address_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => <<>>,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    % Empty address should be rejected
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, <<"Recipient address cannot be empty.">>}, Result),
    % Balance unchanged
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer to address with special characters should succeed
transfer_to_special_chars_address_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    SpecialAddr = <<"alice@example.com">>,
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => SpecialAddr,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(900, get_balance(State, ?ALICE)),
    ?assertEqual(100, get_balance(State, SpecialAddr)).

%% @doc Transfer to path-like address should fail
transfer_to_path_traversal_address_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    PathAddr = <<"../../root">>,
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => PathAddr,
            <<"quantity">> => 100
        },
        #{from => ?ALICE}
    ),
    % Path-like addresses should be rejected for security
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch(
        {error, <<"Recipient address cannot contain path separators.">>}, 
        Result
    ),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Mint to single character address should succeed
mint_to_single_char_address_test() ->
    hb:init(),
    Base = generate_base_state(),
    SingleChar = <<"x">>,
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => SingleChar,
            <<"quantity">> => 1000
        },
        #{from => ?MINTER}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1000, get_balance(State, SingleChar)),
    ?assertEqual(1000, hb_ao:get(<<"total-supply">>, State, #{})).

%% @doc Mint to max length address should succeed
mint_to_max_length_address_test() ->
    hb:init(),
    Base = generate_base_state(),
    MaxAddr = binary:copy(<<"A">>, 128),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => MaxAddr,
            <<"quantity">> => 1000
        },
        #{from => ?MINTER}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    ?assertEqual(1000, get_balance(State, MaxAddr)).
    
%% @doc Transfer with string quantity should fail
transfer_string_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => <<"100">>  % String, not integer
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer with negative string quantity should fail
transfer_negative_string_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => <<"-100">>
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer with mixed type string quantity should fail
transfer_mixed_string_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => <<"-1000FOO">>
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer with quoted zero string should fail
transfer_quoted_zero_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => <<"\"0\"">>
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer with float quantity should fail
transfer_float_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 100.5  % Float
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer with float string quantity should fail
transfer_float_string_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => <<"100.5">>
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer with scientific notation string should fail
transfer_scientific_notation_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => <<"1e10">>
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer with atom quantity should fail
transfer_atom_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => hundred 
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Transfer with list quantity should fail
transfer_list_quantity_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => [100] 
        },
        #{from => ?ALICE}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, _}, Result),
    ?assertEqual(1000, get_balance(Base, ?ALICE)).

%% @doc Mint with string quantity should fail
mint_string_quantity_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => <<"1000">>
        },
        #{from => ?MINTER}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch(
        {error, <<"Quantity must be a non-negative integer.">>}, 
        Result
    ),
    ?assertEqual(0, hb_ao:get(<<"total-supply">>, Base, #{})).

%% @doc Mint with float quantity should fail
mint_float_quantity_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 1000.5
        },
        #{from => ?MINTER}
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch(
        {error, <<"Quantity must be a non-negative integer.">>}, 
        Result
    ),
    ?assertEqual(0, hb_ao:get(<<"total-supply">>, Base, #{})).

%% @doc Duplicate recipient in map - last value should win
transfer_duplicate_recipient_in_body_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Body = #{
        <<"recipient">> => ?CHARLIE, 
        <<"quantity">> => 100
    },
    BodyWithDupe = Body#{<<"recipient">> => ?BOB},  
    Req = make_request(<<"transfer">>, BodyWithDupe, #{from => ?ALICE}),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    % Should use last value (Bob)
    ?assertEqual(900, get_balance(State, ?ALICE)),
    ?assertEqual(100, get_balance(State, ?BOB)),
    ?assertEqual(0, get_balance(State, ?CHARLIE)).

%% @doc Duplicate quantity in map - last value should win
transfer_duplicate_quantity_in_body_test() ->
    hb:init(),
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1000}
    }),
    Body = #{
        <<"recipient">> => ?BOB,
        <<"quantity">> => 1 
    },
    BodyWithDupe = Body#{<<"quantity">> => 200},  
    Req = make_request(<<"transfer">>, BodyWithDupe, #{from => ?ALICE}),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    % Should use last value (200)
    ?assertEqual(800, get_balance(State, ?ALICE)),
    ?assertEqual(200, get_balance(State, ?BOB)).

%% @doc Duplicate recipient in mint should use last value
mint_duplicate_recipient_test() ->
    hb:init(),
    Base = generate_base_state(),
    Body = #{
        <<"mode">> => <<"single">>,
        <<"recipient">> => ?CHARLIE,  
        <<"quantity">> => 1000
    },
    BodyWithDupe = Body#{<<"recipient">> => ?ALICE}, 
    Req = make_request(<<"mint">>, BodyWithDupe, #{from => ?MINTER}),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    % Should mint to Alice (last value)
    ?assertEqual(1000, get_balance(State, ?ALICE)),
    ?assertEqual(0, get_balance(State, ?CHARLIE)).

%% @doc Duplicate quantity in mint should use last value
mint_duplicate_quantity_test() ->
    hb:init(),
    Base = generate_base_state(),
    Body = #{
        <<"mode">> => <<"single">>,
        <<"recipient">> => ?ALICE,
        <<"quantity">> => 100 
    },
    BodyWithDupe = Body#{<<"quantity">> => 5000}, 
    Req = make_request(<<"mint">>, BodyWithDupe, #{from => ?MINTER}),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    % Should mint 5000 (last value)
    ?assertEqual(5000, get_balance(State, ?ALICE)),
    ?assertEqual(5000, hb_ao:get(<<"total-supply">>, State, #{})).

%% @doc Batch mint with mixed valid and invalid quantities
mint_batch_mixed_valid_invalid_quantities_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"batch">>,
            <<"quantities">> => #{
                ?ALICE => 1000,           
                ?BOB => <<"500">>,        
                ?CHARLIE => 2000,         
                <<"device">> => <<"foo">>,
                <<"path">> => <<"bar">>    
            }
        },
        #{from => ?MINTER}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    % Should mint only to valid integer quantities
    ?assertEqual(1000, get_balance(State, ?ALICE)),
    ?assertEqual(0, get_balance(State, ?BOB)),
    ?assertEqual(2000, get_balance(State, ?CHARLIE)),
    % Total supply should be only valid mints
    ?assertEqual(3000, hb_ao:get(<<"total-supply">>, State, #{})).

simple_process_test() ->
    hb:init(),
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()]
        },
    AliceWallet = ar_wallet:new(),
    AliceAddr = hb_util:human_id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = hb_util:human_id(BobWallet),
    Base =
        generate_process_state(
            #{
                initial_balances =>
                    #{ AliceAddr => 1_000_000_000 }
            },
            Opts
        ),
    ?event({base_state, Base}),
    SchedRes =
        schedule_request(
            Base,
            <<"transfer">>,
            #{
                <<"from">> => AliceAddr,
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
    ?assertEqual(1_000_000_000, hb_ao:get(<<"total-supply">>, State, #{})).

%%% Integration Tests (Token + Pot)

%% @doc Basic test to see what happens when transfer is called with mint-device=pot
transfer_with_pot_mint_device_basic_test() ->
    hb:init(),
    ResourceOxygen = <<"oxygen">>,
    Opts = #{ store => [hb_test_utils:test_store()] },
    % Create integrated state: Alice has 1000 tokens, resource oxygen
    Base =
        generate_integrated_state(
            #{
                initial_balances => #{?ALICE => 1000},
                total_supply => 1000,
                mint_cap => 10000,
                mint_prop => {1, 2},
                pot_resources => #{
                    ResourceOxygen => pot_resource(100, [{?ALICE, 10}])
                }
            },
            Opts
        ),
    ?event({base_state_created, Base}),
    % Alice transfers 300 tokens to Bob
    Req =
        make_request(
            <<"transfer">>,
            #{
                <<"recipient">> => ?BOB,
                <<"quantity">> => 300
            },
            Opts#{from => ?ALICE}
        ),
    ?event({request, Req}),
    {ok,Result} = dev_token:compute(Base, Req, Opts),
    ?assertEqual(700, get_balance(Result, ?ALICE)),
    ?assertEqual(300, get_balance(Result, ?BOB)),
    ?assertEqual(1000, hb_ao:get(<<"total-supply">>, Result, Opts)).

%% @doc Test that transfer works when balance is insufficient but 
%% balance + unclaimed_yield is sufficient
%% This validates that normalize_mint properly claims yields before transfer
transfer_with_unclaimed_yield_test() ->
    hb:init(),
    ResourceOxygen = <<"oxygen">>,
    Opts = #{ store => [hb_test_utils:test_store()] },
    % Alice has 500 tokens in balance
    % Alice has deposits in pot that will yield tokens
    % Alice wants to transfer 700 tokens
    % Should succeed because: balance + yield > 700
    Base =
        generate_integrated_state(
            #{
                initial_balances => #{?ALICE => 500}, 
                total_supply => 500,
                mint_cap => 10000,
                mint_prop => {1, 2}, 
                t => 0,
                last_drip => 0,
                pot_resources => #{
                    ResourceOxygen => pot_resource(100, [{?ALICE, 10}])
                }
            },
            Opts
        ),
    ?event({initial_state, Base}),
    ?event({alice_initial_balance, get_balance(Base, ?ALICE)}),
    % Advance time to generate yield
    % With mint_cap=10000, mint_prop={1,2}, going from t=0 to t=1:
    % ToMint = 10000 * (2^1 - 1^1) / 2^1 = 10000 * 1 / 2 = 5000
    % GlobalAcc = 0 + (5000 / 1000) = 5 (per weighted unit)
    % ResourceAcc = 0 + (5 * 100) = 500
    % Alice's yield = (500 - 0) * 10 = 5000 tokens!
    BaseWithTime = Base#{<<"t">> => 1},
    ?event({state_with_advanced_time, BaseWithTime}),
    % Try to transfer 700 tokens
    % Should fail without normalize_mint (500 < 700)
    % Should succeed with normalize_mint (500 + 5000 = 5500 > 700)
    Req = make_request(
        <<"transfer">>,
        #{
            <<"recipient">> => ?BOB,
            <<"quantity">> => 700
        },
        Opts#{from => ?ALICE}
    ),
    {ok, Result} = dev_token:compute(BaseWithTime, Req, Opts),
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
    hb:init(),
    ResourceOxygen = <<"oxygen">>,
    % Alice has deposits in pot but hasn't claimed yet
    Base = generate_integrated_state(#{
        initial_balances => #{?ALICE => 1000},
        total_supply => 1000,
        mint_cap => 10000,
        mint_prop => {1, 2},
        t => 0,
        last_drip => 0,
        pot_resources => #{
            ResourceOxygen => pot_resource(100, [{?ALICE, 10}])
        }
    }),
    ?event({initial_alice_balance, get_balance(Base, ?ALICE)}),
    % Advance time to generate yield
    % ToMint = 10000 * (2^1 - 1) / 2 = 5000
    % GlobalAcc = 5000 / 1000 = 5
    % ResourceAcc = 5 * 100 = 500
    % Alice's yield = 500 * 10 = 5000
    BaseWithTime = Base#{<<"t">> => 1},
    % Call claim_yield directly
    ResultAfterClaim = dev_pot:claim_yield(
        ?ALICE,
        ResourceOxygen,
        BaseWithTime,
        #{}
    ),
    ?event({after_claim, ResultAfterClaim}),
    AliceBalanceAfterClaim = get_balance(ResultAfterClaim, ?ALICE),
    ?assertEqual(6000, AliceBalanceAfterClaim),
    ?assertEqual(6000, hb_ao:get(<<"total-supply">>, ResultAfterClaim, #{})),
    ResultSecondClaim = dev_pot:claim_yield(
        ?ALICE,
        ResourceOxygen,
        ResultAfterClaim,
        #{}
    ),
    ?assertEqual(6000, get_balance(ResultSecondClaim, ?ALICE)).

%% @doc Test claim_yield across multiple resources
claim_yield_multiple_resources_test() ->
    hb:init(),
    ResourceOxygen = <<"oxygen">>,
    ResourceHydrogen = <<"hydrogen">>,
    % Alice has deposits in two different resources
    Base = generate_integrated_state(#{
        initial_balances => #{?ALICE => 500},
        total_supply => 500,
        mint_cap => 10000,
        mint_prop => {1, 2},
        t => 0,
        last_drip => 0,
        pot_resources => #{
            ResourceOxygen => pot_resource(100, [{?ALICE, 10}]),
            ResourceHydrogen => pot_resource(50, [{?ALICE, 5}])
        }
    }),
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
    BaseWithTime = Base#{<<"t">> => 1},
    % Claim all yields at once using claim_yield/3
    ResultAfterClaimAll = dev_pot:claim_yield(
        BaseWithTime,
        #{<<"subject">> => ?ALICE},
        #{}
    ),
    AliceBalance = get_balance(ResultAfterClaimAll, ?ALICE),
    % Alice should have: 500 + 5000 = 5500
    ?assertEqual(5500, AliceBalance),
    % Total supply should be updated
    ?assertEqual(5500, hb_ao:get(<<"total-supply">>, ResultAfterClaimAll, #{})).

%% @doc Test claim_yield when address has no deposits (edge case)
claim_yield_no_deposits_test() ->
    hb:init(),
    ResourceOxygen = <<"oxygen">>,
    % Charlie has no deposits, only balance
    Base = generate_integrated_state(#{
        initial_balances => #{?CHARLIE => 100},
        total_supply => 100,
        mint_cap => 10000,
        mint_prop => {1, 2},
        t => 0,
        last_drip => 0,
        pot_resources => #{
            ResourceOxygen => pot_resource(100, [{?ALICE, 10}])  % Only Alice has deposits
        }
    }),
    BaseWithTime = Base#{<<"t">> => 1},
    % Charlie tries to claim yield 
    ResultAfterClaim = dev_pot:claim_yield(
        ?CHARLIE,
        ResourceOxygen,
        BaseWithTime,
        #{}
    ),
    % Charlie's balance should be unchanged (still 100)
    ?assertEqual(100, get_balance(ResultAfterClaim, ?CHARLIE)),
    % Total supply should be unchanged (no new minting for Charlie)
    ?assertEqual(100, hb_ao:get(<<"total-supply">>, ResultAfterClaim, #{})),
    ResultAfterClaimAll = dev_pot:claim_yield(
        BaseWithTime,
        #{<<"subject">> => ?CHARLIE},
        #{}
    ),
    ?assertEqual(100, get_balance(ResultAfterClaimAll, ?CHARLIE)),
    ?assertEqual(100, hb_ao:get(<<"total-supply">>, ResultAfterClaimAll, #{})).

%%% Benchmark Tests

benchmark_transfers_test() ->
    hb:init(),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 1_000,
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base = generate_base_state(#{
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
    }),
    Reqs =
        [
            make_request(
                <<"transfer">>,
                #{
                    <<"from">> => ?ALICE,
                    <<"recipient">> => ?BOB,
                    <<"quantity">> => 1,
                    <<"transfer-number">> => I
                },
                #{ from => ?ALICE }
            )
            ||
                I <- lists:seq(1, Transfers)
        ],
    AOCoreStartTime = erlang:monotonic_time(millisecond),
    AOCoreInvokedState =
        lists:foldl(
            fun(Req, State) ->
                {ok, NewState} = hb_ao:resolve(State, Req, #{}),
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
                {ok, NewState} = dev_token:compute(State, Req, #{}),
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
    hb:init(),
    NumRecipients = 10_000,  
    Base = generate_base_state(),
    % Create batch with NumRecipients recipients
    Recipients = [
        list_to_binary("recipient-" ++ integer_to_list(I))
        || I <- lists:seq(1, NumRecipients)
    ],
    Quantities = maps:from_list([{R, 1000} || R <- Recipients]),
    Req = make_request(
        <<"mint">>,
        #{
            <<"quantities">> => Quantities,
            <<"mode">> => <<"batch">>
        },
        #{from => ?MINTER}
    ),
    % Benchmark batch mint
    StartTime = erlang:monotonic_time(millisecond),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
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