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
            <<"from">> => ?ALICE,
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
            <<"from">> => ?ALICE,
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

mint_single_unauthorized_test() ->
    hb:init(),
    Base = generate_base_state(),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"single">>,
            <<"recipient">> => ?ALICE,
            <<"quantity">> => 1000
        },
        #{from => ?BOB}  % Bob is not the minter
    ),
    Result = dev_token:compute(Base, Req, #{}),
    ?assertMatch({error, <<"Mint authority mismatch.">>}, Result).

mint_batch_test() ->
    hb:init(),
    Base = generate_base_state(),
    Quantities = #{
        ?ALICE => 500,
        ?BOB => 300,
        ?CHARLIE => 200
    },
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"batch">>,
            <<"body">> => Quantities
        },
        #{from => ?MINTER}
    ),
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
            <<"from">> => ?ALICE,
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

%%% Benchmark Tests

benchmark_transfers_test_disabled() ->
    hb:init(),
    % Setup: Alice has 1 billion tokens
    Base = generate_base_state(#{
        initial_balances => #{?ALICE => 1_000_000_000}
    }),
    % Benchmark 1000 transfers
    N = 1000,
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
    ?event({benchmark_transfers,
        {count, N},
        {elapsed_ms, ElapsedMs},
        {tx_per_sec, TxPerSec}
    }),
    % Verify correctness
    ?assertEqual(1_000_000_000 - N, get_balance(FinalState, ?ALICE)),
    ?assertEqual(N, get_balance(FinalState, ?BOB)).

benchmark_batch_mint_test_disabled() ->
    hb:init(),
    Base = generate_base_state(),
    % Create batch with 100 recipients
    Recipients = [
        list_to_binary("recipient-" ++ integer_to_list(I))
        || I <- lists:seq(1, 100)
    ],
    Quantities = maps:from_list([{R, 1000} || R <- Recipients]),
    Req = make_request(
        <<"mint">>,
        #{
            <<"mode">> => <<"batch">>,
            <<"body">> => Quantities
        },
        #{from => ?MINTER}
    ),
    % Benchmark batch mint
    StartTime = erlang:monotonic_time(microsecond),
    {ok, NewState} = dev_token:compute(Base, Req, #{}),
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedMs = (EndTime - StartTime) / 1000,
    ?event({benchmark_batch_mint,
        {recipients, 100},
        {elapsed_ms, ElapsedMs},
        {ms_per_recipient, ElapsedMs / 100}
    }),
    % Verify correctness
    ?assertEqual(100_000, hb_ao:get(<<"total-supply">>, NewState, #{})),
    lists:foreach(
        fun(R) -> ?assertEqual(1000, get_balance(NewState, R)) end,
        Recipients
    ).
