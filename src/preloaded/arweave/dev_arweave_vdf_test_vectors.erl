%%% @doc Deterministic test vectors for post-2.9 Arweave VDF admission rules.
-module(dev_arweave_vdf_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Caller input may lower the worker count but may not raise the
%% operator's ceiling.
worker_limit_test() ->
    Opts = #{ <<"arweave-max-vdf-workers">> => 4 },
    ?assertEqual(
        4,
        lib_arweave_vdf:threads(
            #{ <<"arweave-vdf-threads">> => 1000 },
            #{},
            Opts
        )
    ),
    ?assertEqual(
        2,
        lib_arweave_vdf:threads(
            #{},
            #{ <<"arweave-vdf-threads">> => 2 },
            Opts
        )
    ),
    ?assertEqual(
        1,
        lib_arweave_vdf:threads(
            #{ <<"arweave-vdf-threads">> => 0 },
            #{},
            Opts
        )
    ).

%% @doc A wide VDF range carries the standard 10,800-step suffix without being
%% rejected merely because the parent is farther away.
wide_step_range_test() ->
    Steps = lists:duplicate(10800, <<"step">>),
    ?assertEqual(
        ok,
        lib_arweave_vdf:check_step_range(
            #{
                <<"global-step-number">> => 10802,
                <<"steps">> => Steps
            },
            #{ <<"global-step-number">> => 1 },
            #{}
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-count">> }},
        lib_arweave_vdf:check_step_range(
            #{
                <<"global-step-number">> => 10802,
                <<"steps">> => tl(Steps)
            },
            #{ <<"global-step-number">> => 1 },
            #{}
        )
    ),
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-step-count">> }},
        lib_arweave_vdf:check_step_range(
            #{
                <<"global-step-number">> => 10802,
                <<"steps">> => [<<"step">> | Steps]
            },
            #{ <<"global-step-number">> => 1 },
            #{}
        )
    ).

%% @doc A public verification with the maximum 10,800-step header suffix
%% recomputes the omitted prefix, including the entropy reset it crosses.
wide_step_chain_test() ->
    PrevStep = 1198,
    FinalStep = 12000,
    ResetStep = 1200,
    Initial = <<0:256>>,
    NextSeed = <<2:384>>,
    Outputs =
        compute_outputs(
            PrevStep + 1,
            FinalStep,
            ResetStep,
            NextSeed,
            Initial,
            []
        ),
    [_Omitted1199, _Omitted1200 | Supplied] = Outputs,
    {FinalOutput, FinalCheckpoints} = lists:last(Outputs),
    PrevHash = hb_util:encode(<<4:384>>),
    PrevWeaveSize = 1000,
    PrevInfo =
        #{
            <<"output">> => hb_util:encode(Initial),
            <<"prev-output">> => hb_util:encode(<<3:256>>),
            <<"seed">> => hb_util:encode(<<1:384>>),
            <<"next-seed">> => hb_util:encode(NextSeed),
            <<"partition-upper-bound">> => 100,
            <<"next-partition-upper-bound">> => 200,
            <<"global-step-number">> => PrevStep,
            <<"last-step-checkpoints">> => [],
            <<"steps">> => [],
            <<"vdf-difficulty">> => 1,
            <<"next-vdf-difficulty">> => 2
        },
    SeedData =
        lib_arweave_vdf:seed_data(
            FinalStep,
            PrevInfo,
            PrevHash,
            PrevWeaveSize,
            #{}
        ),
    Steps = [ hb_util:encode(Output) || {Output, _} <- lists:reverse(Supplied) ],
    Info =
        SeedData#{
            <<"output">> => hb_util:encode(FinalOutput),
            <<"prev-output">> => hb_util:encode(Initial),
            <<"global-step-number">> => FinalStep,
            <<"last-step-checkpoints">> =>
                [
                    hb_util:encode(Checkpoint)
                ||
                    Checkpoint <-
                        [FinalOutput |
                            ar_vdf:checkpoint_buffer_to_checkpoints(
                                FinalCheckpoints
                            )]
                ],
            <<"steps">> => Steps,
            <<"next-vdf-difficulty">> => 2
        },
    Base =
        #{
            <<"device">> => <<"arweave-vdf@2.9">>,
            <<"nonce-limiter-info">> => Info,
            <<"prev-nonce-limiter-info">> => PrevInfo,
            <<"prev-indep-hash">> => PrevHash,
            <<"prev-weave-size">> => PrevWeaveSize
        },
    Opts =
        #{
            <<"arweave-max-vdf-workers">> => 4,
            <<"store">> => [hb_test_utils:test_store()]
        },
    ?assertMatch(
        {ok, #{ <<"valid">> := true }},
        hb_ao:resolve(Base, <<"verify-chain">>, Opts)
    ),
    Corrupt =
        Info#{
            <<"steps">> =>
                lists:sublist(Steps, length(Steps) - 1)
                    ++ [hb_util:encode(<<0:256>>)]
        },
    ?assertMatch(
        {error, #{ <<"message">> := <<"invalid-vdf-chain">> }},
        hb_ao:resolve(
            Base#{ <<"nonce-limiter-info">> => Corrupt },
            <<"verify-chain">>,
            Opts
        )
    ).

compute_outputs(Step, End, _Reset, _Seed, Output, Acc) when Step > End ->
    lists:reverse(Acc);
compute_outputs(Step, End, Reset, Seed, Output, Acc) ->
    PrevOutput =
        case Step of
            Reset -> ar_nonce_limiter:mix_seed(Output, Seed);
            _ -> Output
        end,
    Difficulty =
        case Step >= Reset of
            true -> 2;
            false -> 1
        end,
    {ok, Next, Checkpoints} = ar_vdf:compute(Step, PrevOutput, Difficulty),
    compute_outputs(
        Step + 1,
        End,
        Reset,
        Seed,
        Next,
        [{Next, Checkpoints} | Acc]
    ).

%% @doc Reset detection covers both a crossing range and an ordinary range.
reset_point_test() ->
    ?assertEqual(
        111555600,
        field(
            resolve(
                #{
                    <<"prev-step-number">> => 111555523,
                    <<"step-number">> => 111555793
                },
                <<"reset-point">>
            ),
            <<"reset-point">>
        )
    ),
    ?assertEqual(
        <<"none">>,
        field(
            resolve(
                #{
                    <<"prev-step-number">> => 111559070,
                    <<"step-number">> => 111559072
                },
                <<"reset-point">>
            ),
            <<"reset-point">>
        )
    ).

%% @doc A scheduled difficulty is carried through outside a retarget.
next_difficulty_test() ->
    ?assertEqual(
        1111546,
        field(
            resolve(
                #{
                    <<"height">> => 1974871,
                    <<"vdf-difficulty">> => 1111546,
                    <<"next-vdf-difficulty">> => 1111546
                },
                <<"next-difficulty">>
            ),
            <<"next-vdf-difficulty">>
        )
    ).

resolve(Base, Path) ->
    hb_ao:resolve(
        Base#{ <<"device">> => <<"arweave-vdf@2.9">> },
        Path,
        #{ <<"store">> => [hb_test_utils:test_store()] }
    ).

field({ok, Result}, Key) -> hb_maps:get(Key, Result, not_found, #{}).
