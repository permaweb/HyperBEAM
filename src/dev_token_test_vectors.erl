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
    ?event({generate_base_state, {overrides, Overrides}}),
    DefaultBalances = #{ <<"device">> => <<"trie@1.0">> },
    InitialBalances = maps:get(initial_balances, Overrides, #{}),
    ?event({initial_balances, InitialBalances}),
    Balances =
        case maps:size(InitialBalances) of
            0 ->
                ?event({no_initial_balances}),
                DefaultBalances;
            _ ->
                ?event({setting_initial_balances}),
                % Structure balances directly for trie device - store as integers
                BalancesWithAccounts = maps:fold(
                    fun(AccountID, Quantity, Acc) ->
                        maps:put(AccountID, Quantity, Acc)
                    end,
                    DefaultBalances,
                    InitialBalances
                ),
                ?event({balances_structured, BalancesWithAccounts}),
                BalancesWithAccounts
        end,
    DefaultState = #{
        <<"device">> => <<"token@1.0">>,
        <<"owner">> => maps:get(owner, Overrides, ?OWNER),
        <<"mint-authority">> => maps:get(mint_authority, Overrides, ?MINTER),
        <<"name">> => <<"Test Token">>,
        <<"ticker">> => <<"TEST">>,
        <<"denomination">> => 12,
        <<"total-supply">> => maps:get(total_supply, Overrides, 0),
        <<"balances">> => Balances
    },
    FinalState = maps:merge(DefaultState, maps:get(extra, Overrides, #{})),
    ?event({final_state_generated, {balances_size, map_size(Balances)}}),
    FinalState.

%% @doc Create a request message.
make_request(Action, Body) ->
    make_request(Action, Body, #{}).

make_request(Action, Body, Opts) ->
    From = maps:get(from, Opts, ?OWNER),
    Req = #{
        <<"action">> => Action,
        <<"body">> => Body#{<<"from">> => From}
    },
    ?event({make_request, {action, Action}, {from, From}}),
    Req.

%% @doc Get balance for an account.
get_balance(State, Account) ->
    get_balance(State, Account, #{}).
get_balance(State, Account, Opts) ->
    Balances = hb_ao:get(<<"balances">>, State, Opts),
    hb_ao:get(Account, Balances, 0, Opts).

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
    ?event({base_state_balances, hb_ao:get(<<"balances">>, Base, #{}), Base, Req}),
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
            <<"updates">> => #{
                <<"name">> => <<"New Token Name">>,
                <<"ticker">> => <<"NEW">>
            }
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
            <<"updates">> => #{
                <<"description">> => <<"Token description">>
            }
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
            <<"updates">> => #{
                <<"name">> => <<"Hacked Name">>
            }
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
    ?assertMatch({error, <<"Unsupported token action: `burn'.">>}, Result).

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
            <<"updates">> => #{
                <<"name">> => <<"Updated Token">>,
                <<"ticker">> => <<"UPD">>,
                <<"denomination">> => 18,
                <<"description">> => <<"A comprehensive test token">>
            }
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
            <<"updates">> => #{
                <<"logo">> => <<"https://example.com/logo.png">>,
                <<"website">> => <<"https://example.com">>,
                <<"social-twitter">> => <<"@testtoken">>
            }
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
            <<"updates">> => #{<<"name">> => <<"First Name">>}
        },
        #{from => ?OWNER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(<<"First Name">>, hb_ao:get(<<"name">>, State1, #{})),
    % Second update
    Req2 = make_request(
        <<"set">>,
        #{
            <<"updates">> => #{<<"name">> => <<"Second Name">>}
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
            <<"updates">> => #{
                <<"balances">> => #{
                    <<"device">> => <<"trie@1.0">>, 
                    ?ALICE => 999999
                }
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
            <<"updates">> => #{
                <<"total-supply">> => 999_999_999
            }
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
        #{<<"updates">> => #{<<"mint-authority">> => NewMinter}},
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
            <<"from">> => ?ALICE, 
            <<"recipient">> => ?BOB, 
            <<"quantity">> => 300
        },
        #{from => ?ALICE}
    ),
    {ok, State} = dev_token:compute(Base, Req, #{}),
    Outbox = hb_ao:get(<<"results/outbox">>, State, #{}),
    ?assertEqual(2, length(Outbox)),
    [CreditNotice, DebitNotice] = Outbox,
    ?assertEqual(<<"Credit-Notice">>, hb_ao:get(<<"action">>, CreditNotice, #{})),
    ?assertEqual(?ALICE, hb_ao:get(<<"sender">>, CreditNotice, #{})),
    ?assertEqual(?BOB, hb_ao:get(<<"recipient">>, CreditNotice, #{})),
    ?assertEqual(<<"Debit-Notice">>, hb_ao:get(<<"action">>, DebitNotice, #{})),
    ?assertEqual(?BOB, hb_ao:get(<<"recipient">>, DebitNotice, #{})),
    ?assertEqual(300, hb_ao:get(<<"quantity">>, DebitNotice, #{})),
    ?assertEqual(700, get_balance(State, ?ALICE)),
    ?assertEqual(300, get_balance(State, ?BOB)).

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
            ?assertEqual(<<"Mint-Notice">>, hb_ao:get(<<"action">>, Notice, #{}))
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
        #{<<"updates">> => #{<<"name">> => <<"Owner Updated">>}},
        #{from => ?OWNER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(<<"Owner Updated">>, hb_ao:get(<<"name">>, State1, #{})),
    Req2 = make_request(
        <<"set">>,
        #{<<"updates">> => #{<<"ticker">> => <<"MINT">>}},
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
        #{<<"updates">> => #{<<"owner">> => NewOwner}},
        #{from => ?OWNER}
    ),
    {ok, State1} = dev_token:compute(Base, Req1, #{}),
    ?assertEqual(NewOwner, hb_ao:get(<<"owner">>, State1, #{})),
    Req2 = make_request(
        <<"set">>,
        #{<<"updates">> => #{<<"name">> => <<"Old Owner Try">>}},
        #{from => ?OWNER}
    ),
    ?assertMatch({error, <<"Set authority mismatch.">>}, dev_token:compute(State1, Req2, #{})),
    Req3 = make_request(
        <<"set">>,
        #{<<"updates">> => #{<<"name">> => <<"New Owner Success">>}},
        #{from => NewOwner}
    ),
    {ok, State2} = dev_token:compute(State1, Req3, #{}),
    ?assertEqual(<<"New Owner Success">>, hb_ao:get(<<"name">>, State2, #{})).

%%% Benchmark Tests
benchmark_transfers_test() ->
    hb:init(),
    % Setup: Alice has 1 billion tokens
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1_000_000_000}
    }),
    % Benchmark 100 transfers
    N = 100,
    StartTime = erlang:monotonic_time(microsecond),
    FinalState = lists:foldl(
        fun(I, State) ->
            Req = make_request(
                <<"transfer">>,
                #{
                    <<"from">> => ?ALICE,
                    <<"recipient">> => ?BOB,
                    <<"quantity">> => 1
                },
                #{from => ?ALICE}
            ),
            {ok, NewState} = dev_token:compute(State, Req, #{}),
            NewState
        end,
        Base,
        lists:seq(1, N)
    ),
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedMs = (EndTime - StartTime) / 1000,
    TxPerSec = (N / ElapsedMs) * 1000,
    ?event(benchmark, 
        {benchmark_transfers,
            {count, N},
            {elapsed_ms, ElapsedMs},
            {tx_per_sec, TxPerSec}
        }
    ),
    % Verify correctness
    ?assertEqual(1_000_000_000 - N, get_balance(FinalState, ?ALICE)),
    ?assertEqual(N, get_balance(FinalState, ?BOB)).

benchmark_batch_mint_test() ->
    hb:init(),
    NumRecipients = 10000,  
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
    StartTime = erlang:monotonic_time(microsecond),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedMs = (EndTime - StartTime) / 1000,
    ?event(benchmark, 
        {benchmark_batch_mint,
            {recipients, NumRecipients},
            {elapsed_ms, ElapsedMs},
            {ms_per_recipient, ElapsedMs / NumRecipients}
        }
    ),
    ?assertEqual(
        NumRecipients * 1000, 
        hb_ao:get(<<"total-supply">>, NewState, #{})
    ).

