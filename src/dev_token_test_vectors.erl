%%% @doc Test vectors and benchmarks for the `~token@1.0` device.
-module(dev_token_test_vectors).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

-define(ALICE, id(<<"alice">>)).
-define(BOB, id(<<"bob">>)).

%%% Test Utilities

%% @doc Generate generic isolated node messages for testing.
opts() -> opts(#{}).
opts(Base) ->
    hb:init(),
    Base#{
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
    ?event(debug_test, {params, Params}, Opts),
    Extra = maps:get(extra, Params, #{}),
    DefaultState = #{
        <<"device">> => <<"token@1.0">>,
        <<"mint-authority">> => maps:get(mint_authority, Extra, id(<<"minter">>)),
        <<"name">> => <<"Test Token">>,
        <<"ticker">> => <<"TEST">>,
        <<"denomination">> => 12,
        <<"total-supply">> => Total,
        <<"balances">> => Balances
    },
    FinalState = maps:merge(DefaultState, maps:get(extra, Params, #{})),
    ?event({final_state_generated, {balances_size, map_size(Balances)}}),
    {FinalState, Opts}.

generate_process(Params, Opts) ->
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
    ?event(debug_test, {extra_with_proc_base, ExtraWithProcBase}, Opts),
    {Base, _Opts} =
        generate_base_state(
            Params#{ extra => ExtraWithProcBase },
            Opts
        ),
    ?event(debug_test_state, {base_process, Base}, Opts),
    CommittedBase = hb_message:commit(Base, Opts),
    ?event(debug_test_state, {committed_base, CommittedBase}, Opts),
    {ok, _} = 
        hb_ao:resolve(
            CommittedBase,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => CommittedBase
            },
            Opts
        ),
    SchedRes = 
        hb_ao:resolve(
            CommittedBase, 
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"schedule">>
            },
            Opts
        ),
    ?event(debug_test_state, {sched_res, SchedRes}),
    dev_process_lib:ensure_process_key(CommittedBase, Opts).

%% @doc Return a signed token process with a `pot@1.0` mint device.
generate_pot_process(Params, Opts) ->
    Extra = generate_pot_fields(Params, Opts),
    generate_process(Params#{ extra => Extra }, Opts).

%% @doc Create a request message.
generate_assignment(Action, Body, From) ->
    Req = #{
        <<"path">> => <<"compute">>,
        <<"body">> => Body#{ <<"from">> => From, <<"action">> => Action }
    },
    ?event({make_request, {action, Action}, {from, From}}),
    Req.

schedule_request(State, Action, Body, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    schedule_request(State, Action, Body, Wallet, Opts).
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
get_balance(State, Account, Opts) when is_binary(Account) ->
    get_balance(State, #{ <<"balance">> => Account }, Opts);
get_balance(State, Req, Opts) ->
    Res =
        hb_ao:resolve_many(
            [
                State,
                <<"now">>,
                #{
                    <<"path">> => <<"as">>,
                    <<"as">> => <<"execution">>
                },
                Req#{ <<"path">> => <<"balance">> }
            ],
            Opts
        ),
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
    MintPropNumerator = hb_maps:get(mint_prop_numerator, Params, 1, Opts),
    MintPropDenominator = hb_maps:get(mint_prop_denominator, Params, 2, Opts),
    Resources = hb_maps:get(resources, Params, #{}, Opts),
    T = hb_maps:get(t, Params, 0, Opts),
    LastDrip = hb_maps:get(last_drip, Params, T, Opts),
    TotalWeightedUnits =
        hb_maps:fold(
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
        <<"mint-cap">> => MintCap,
        <<"mint-prop-numerator">> => MintPropNumerator,
        <<"mint-prop-denominator">> => MintPropDenominator,
        <<"t-source">> => <<"slot">>,
        <<"total-weighted-units">> => TotalWeightedUnits,
        <<"resources">> => Resources,
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

give_instruction(<<"deposit">>, State, Req, Opts) -> 
    ?event(debug_test, {give_instruction_deposit, {state, State}, {req, Req}}, Opts),
    Id = hb_maps:get(<<"recipient">>, Req, Opts),
    Resource = hb_maps:get(<<"resource">>, Req, Opts),
    Quantity = hb_maps:get(<<"quantity">>, Req, Opts),
    R = dev_pot:deposit(Id, Resource, Quantity, State, Opts),
    ?event(debug_test_state, {give_instruction_deposit_result, {result, R}}, Opts),
    {ok, R};
give_instruction(<<"transfer">>, State, Req, Opts) -> 
    ?event(debug_test, {give_instruction_transfer, {state, State}, {req, Req}}, Opts),
    From = hb_maps:get(<<"from">>, Req, Opts),
    To = hb_maps:get(<<"to">>, Req, Opts),
    Quantity = hb_maps:get(<<"quantity">>, Req, Opts),
    SchedRes =
        schedule_request(
            State,
            <<"transfer">>,
            #{
                <<"from">> => From,
                <<"recipient">> => To,
                <<"quantity">> => Quantity
            },
            Opts
        ),
    ?event(debug_test, {sched_res, SchedRes}, Opts),
    {ok, NewState} = hb_ao:resolve(State, #{ <<"path">> => <<"now">> }, Opts),
    % R = dev_token:transfer(From, To, Quantity, State, Opts),
    ?event(debug_test, {give_instruction_transfer_result, {result, NewState}}, Opts),
    {ok, NewState};
give_instruction(<<"mint">>, State, Req, Opts) -> 
    ?event(debug_test, {give_instruction_mint, {state, State}, {req, Req}}, Opts),
    From = hb_maps:get(<<"from">>, Req, Opts),
    SchedRes =
        schedule_request(
            State,
            <<"mint">>,
            #{
                <<"from">> => From
            },
            Opts
        ),
    ?event(debug_test, {scheduled, {opts, Opts}, {mint_sched_res, SchedRes}}, Opts),
    SchedRes2 = 
        hb_ao:resolve(
            State, 
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"schedule">>
            },
            Opts
        ),
    ?event(debug_test, {scheduled2, {sched_res2, SchedRes2}}, Opts),
    {ok, NewState} = hb_ao:resolve(State, #{ <<"path">> => <<"compute">> }, Opts),
    ?event(debug_test, {give_instruction_mint_result, {result, NewState}}, Opts),
    {ok, NewState};
give_instruction(<<"tick">>, State, Req, Opts) -> 
    ?event(debug_test, {give_instruction_test_drip, {state, State}, {req, Req}}, Opts),
    NewState = dev_pot:test_drip(State, Req, Opts),
    ?event(debug_test, {give_instruction_test_drip_result, {result, NewState}}, Opts),
    {ok, NewState};
give_instruction(_, _, _, _) ->
    ?event(debug_test, {give_instruction_no_op}).

%%% Transfer Tests

transfer_basic_test() ->
    Opts = opts(),
    Minter = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Base =
        generate_process(
            #{
                total_supply => 0,
                initial_balances => #{},
                extra => #{
                    <<"t">> => 0,
                    <<"last-drip">> => 0,
                    <<"mint-prop-numerator">> => 1,
                    <<"mint-prop-denominator">> => 2,
                    <<"mint-authority">> => id(Minter),
                    <<"mint-cap">> => 1_000_000_000,
                    <<"resources">> => #{
                        <<"oxygen">> => pot_resource(200, [])
                    }
                }
            },
            Opts
        ),
    ?event(debug_test_state, {base_state, {base, Base}, {proc_id, dev_process_lib:process_id(Base, #{}, Opts)}}, Opts),
    {ok, S0} =
        give_instruction(
            <<"deposit">>,
            Base,
            #{
                <<"recipient">> => id(Alice),
                <<"resource">> => <<"oxygen">>,
                <<"quantity">> => 1000
            },
            Opts
        ),
    ?event(debug_test_state, {s0_state, {base, S0}, {proc_id, dev_process_lib:process_id(S0, #{}, Opts)}}, Opts),
    {ok, S0SchedRes} = hb_ao:resolve(S0, #{ <<"path">> => <<"schedule">> }, Opts),
    ?event(debug_test_state, {s0_sched_res, S0SchedRes}),
    {ok, S1} =
        give_instruction(
            <<"tick">>,
            S0,
            #{ <<"t">> => 1 },
            Opts
        ),
    ?event(debug_test_state, {s1_state, {base, S1}, {proc_id, dev_process_lib:process_id(S1, #{}, Opts)}}, Opts),
    {ok, S2} =
        give_instruction(
            <<"mint">>,
            S1,
            #{
                <<"from">> => id(Minter)
            },
            Opts
        ),
    ?event(debug_test_state, {s2_state, {base, S2}, {proc_id, dev_process_lib:process_id(S2, #{}, Opts)}}, Opts),
    {ok, S3} =
        give_instruction(
            <<"transfer">>,
            S2,
            #{
                <<"from">> => id(Alice),
                <<"to">> => id(Bob),
                <<"quantity">> => 300
            },
            Opts
        ).
    % SchedRes =
    %     schedule_request(
    %         Base,
    %         <<"transfer">>,
    %         #{
    %             <<"from">> => id(Alice),
    %             <<"recipient">> => id(Bob),
    %             <<"quantity">> => 300
    %         },
    %         Alice,
    %         Opts
    %     ),
    % ?event(debug_test, {sched_res, SchedRes}, Opts),
    % ?assertEqual(700, get_balance(Base, id(Alice), Opts)),
    % ?assertEqual(300, get_balance(Base, id(Bob), Opts)),
    % % Verify total supply unchanged
    % ?assertEqual(1000, hb_ao:get(<<"now/total-supply">>, Base, Opts)).

mint_authority_mismatch_rejected_test() ->
    Opts = opts(),
    Minter = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    NotMinter = ar_wallet:new(),
    Recipient = id(ar_wallet:new()),
    {Base, _} =
        generate_base_state(
            #{
                total_supply => 0,
                initial_balances => #{},
                extra => #{
                    <<"mint-authority">> => id(Minter)
                }
            },
            Opts
        ),
    ?assertEqual(
        {error, <<"Mint authority mismatch.">>},
        dev_mint_authority:mint(
            Base,
            #{
                <<"body">> => #{
                    <<"from">> => id(NotMinter),
                    <<"recipient">> => Recipient,
                    <<"quantity">> => 1
                }
            },
            Opts
        )
    ),
    Balances = hb_ao:get(<<"balances">>, Base, Opts),
    ?assertEqual(0, hb_ao:get(Recipient, Balances, 0, Opts)),
    ?assertEqual(0, hb_ao:get(<<"total-supply">>, Base, Opts)).

simple_process_test() ->
    Opts = opts(),
    Alice = ar_wallet:new(),
    Bob = ar_wallet:new(),
    Base =
        generate_process(
            #{
                initial_balances => #{ id(Alice) => 1_000_000_000 },
                mint_prop_numerator => 1,
                mint_prop_denominator => 2,
                mint_cap => 2_000_000_000,
                resources => #{
                    <<"oxygen">> => pot_resource(100, [])
                }
            },
            Opts
        ),
    ?event(debug_test, {base_state, Base}, Opts),
    SchedRes =
        schedule_request(
            Base,
            <<"transfer">>,
            #{
                <<"from">> => id(Alice),
                <<"recipient">> => id(Bob),
                <<"quantity">> => 1
            },
            Alice,
            Opts
        ),
    ?assertEqual(999_999_999, get_balance(Base, id(Alice), Opts)),
    ?assertEqual(1, get_balance(Base, id(Bob), Opts)),
    ?assertEqual(1_000_000_000, hb_ao:get(<<"now/total-supply">>, Base, Opts)).

reserved_recipient_transfer_rejected_test() ->
    Opts = opts(),
    Alice = ar_wallet:new(),
    AliceID = id(Alice),
    Base =
        generate_process(
            #{
                initial_balances => #{ AliceID => 1_000_000_000 }
            },
            Opts
        ),
    _SchedRes =
        schedule_request(
            Base,
            <<"transfer">>,
            #{
                <<"from">> => AliceID,
                <<"recipient">> => <<"device">>,
                <<"quantity">> => 1
            },
            Alice,
            Opts
        ),
    {ok, BalancesLink} =
        hb_ao:resolve_many(
            [
                Base,
                <<"now">>,
                #{
                    <<"path">> => <<"as">>,
                    <<"as">> => <<"execution">>
                },
                <<"balances">>
            ],
            Opts
        ),
    ?assertEqual(1_000_000_000, get_balance(Base, AliceID, Opts)),
    ?assertEqual(<<"trie@1.0">>, hb_maps:get(<<"device">>, BalancesLink, undefined, Opts)),
    ?assertEqual(1_000_000_000, hb_ao:get(<<"now/total-supply">>, Base, Opts)).

simple_pot_process_test() ->
    Opts = opts(),
    AliceWallet = ar_wallet:new(),
    AliceAddr = hb_util:human_id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = hb_util:human_id(BobWallet),
    ResourceOxygen = <<"oxygen">>,
    Base =
        generate_pot_process(
            #{
                initial_balances => #{ AliceAddr => 1_000_000_000 },
                total_supply => 1_000_000_000,
                mint_cap => 2_000_000_000,
                mint_prop_numerator => 1,
                mint_prop_denominator => 2,
                resources => #{
                    ResourceOxygen => pot_resource(100, [])
                }
            },
            Opts
        ),
    ?event({base_state, Base}),
    MintSchedRes =
        schedule_request(
            Base,
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
    ?event({transfer_sched_res, TransferSchedRes}),
    {ok, State} =
        hb_ao:resolve(
            Base,
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

%%% Benchmark Tests
benchmark_process_transfers_test_() ->
    {timeout, 180, fun benchmark_process_transfers/0}.
benchmark_process_transfers() ->
    Opts = opts(),
    % Benchmark N transfers
    Transfers = 100,
    Accounts = 3_000,
    AliceWallet = ar_wallet:new(),
    AliceAddr = hb_util:human_id(AliceWallet),
    BobWallet = ar_wallet:new(),
    BobAddr = hb_util:human_id(BobWallet),
    % Setup: Alice has 1 billion tokens, the rest have 1 billion tokens each
    Base =
        generate_process(
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
    ?assertEqual(1_000_000_000 - Transfers, get_balance(State, AliceAddr, Opts)),
    ?assertEqual(Transfers, get_balance(State, BobAddr, Opts)),
    ?assertEqual(
        1_000_000_000 * Accounts,
        hb_ao:get(<<"total-supply">>, State, #{})
    ).
