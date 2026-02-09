%%% @doc Nonce-limiter helpers extracted from Arweave's protocol logic.
%%%
%%% This module intentionally exposes deterministic, stateless primitives so
%%% they can be consumed by AO-Core devices without running Arweave node
%%% background processes.
-module(ar_nonce_limiter).

-export([
    encode_session_key/1,
    session_key/1,
    session_key/3,
    is_ahead_on_the_timeline/2,
    get_reset_frequency/0,
    get_entropy_reset_point/2,
    get_seed_data/2,
    compute/3,
    verify/9,
    verify_no_reset/6,
    maybe_add_entropy/4,
    mix_seed/2
]).

-include("include/hb.hrl").
-include("include/ar_vdf.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Mirrors upstream `?NONCE_LIMITER_RESET_FREQUENCY`.
-define(NONCE_LIMITER_RESET_FREQUENCY, (10 * 120)).

encode_session_key({NextSeed, StartIntervalNumber, NextVDFDifficulty}) ->
    {hb_util:safe_encode(NextSeed), StartIntervalNumber, NextVDFDifficulty};
encode_session_key(SessionKey) ->
    SessionKey.

session_key(Info) when is_map(Info) ->
    NextSeed =
        get_any(
            Info,
            [<<"next-seed">>, <<"next_seed">>, next_seed],
            <<>>
        ),
    StepNumber =
        hb_util:int(
            get_any(
                Info,
                [<<"global-step-number">>, <<"global_step_number">>, global_step_number],
                0
            )
        ),
    NextVDFDifficulty =
        hb_util:int(
            get_any(
                Info,
                [<<"next-vdf-difficulty">>, <<"next_vdf_difficulty">>, next_vdf_difficulty],
                0
            )
        ),
    session_key(NextSeed, StepNumber, NextVDFDifficulty).

session_key(NextSeed, StepNumber, NextVDFDifficulty) ->
    {NextSeed, StepNumber div get_reset_frequency(), NextVDFDifficulty}.

is_ahead_on_the_timeline(NonceLimiterInfo1, NonceLimiterInfo2) ->
    N1 =
        hb_util:int(
            get_any(
                NonceLimiterInfo1,
                [<<"global-step-number">>, <<"global_step_number">>, global_step_number],
                0
            )
        ),
    N2 =
        hb_util:int(
            get_any(
                NonceLimiterInfo2,
                [<<"global-step-number">>, <<"global_step_number">>, global_step_number],
                0
            )
        ),
    N1 > N2.

compute(StepNumber, PrevOutput, VDFDifficulty) ->
    ar_vdf:compute2(StepNumber, PrevOutput, VDFDifficulty).

verify(
    StartStepNumber,
    PrevOutput,
    NumCheckpointsBetweenHashes,
    Hashes,
    ResetStepNumber,
    _ResetSeed,
    ThreadCount,
    VDFDifficulty,
    _NextVDFDifficulty
) when ResetStepNumber =< StartStepNumber ->
    verify_no_reset(
        StartStepNumber,
        PrevOutput,
        NumCheckpointsBetweenHashes,
        Hashes,
        ThreadCount,
        VDFDifficulty
    );
verify(
    StartStepNumber,
    PrevOutput,
    NumCheckpointsBetweenHashes,
    Hashes,
    ResetStepNumber,
    ResetSeed,
    ThreadCount,
    VDFDifficulty,
    NextVDFDifficulty
) ->
    PrefixCount = max(ResetStepNumber - StartStepNumber - 1, 0),
    PrefixHashes = lists:sublist(Hashes, PrefixCount),
    SuffixHashes = nthtail_safe(PrefixCount, Hashes),
    {Result1, PrevOutput2, ValidatedSteps1} =
        case PrefixHashes of
            [] ->
                {true, mix_seed2(PrevOutput, ResetSeed), []};
            _ ->
                case verify_no_reset(
                    StartStepNumber,
                    PrevOutput,
                    NumCheckpointsBetweenHashes,
                    PrefixHashes,
                    ThreadCount,
                    VDFDifficulty
                ) of
                    {true, ValidatedSteps} ->
                        {true, mix_seed2(hd(ValidatedSteps), ResetSeed), ValidatedSteps};
                    false ->
                        {false, undefined, undefined}
                end
        end,
    case Result1 of
        false ->
            false;
        true ->
            case verify_no_reset(
                ResetStepNumber - 1,
                PrevOutput2,
                NumCheckpointsBetweenHashes,
                SuffixHashes,
                ThreadCount,
                NextVDFDifficulty
            ) of
                {true, ValidatedSteps2} ->
                    {true, ValidatedSteps2 ++ ValidatedSteps1};
                false ->
                    false
            end
    end.

verify_no_reset(
    StartStepNumber,
    PrevOutput,
    NumCheckpointsBetweenHashes,
    Hashes,
    ThreadCount,
    VDFDifficulty
) ->
    Garbage = crypto:strong_rand_bytes(32),
    case ar_vdf:verify2(
        StartStepNumber,
        PrevOutput,
        NumCheckpointsBetweenHashes,
        Hashes,
        0,
        Garbage,
        ThreadCount,
        VDFDifficulty
    ) of
        {true, ValidatedSteps} ->
            {true, ValidatedSteps};
        false ->
            case ar_vdf:verify2(
                StartStepNumber,
                PrevOutput,
                NumCheckpointsBetweenHashes,
                lists:reverse(Hashes),
                0,
                Garbage,
                ThreadCount,
                VDFDifficulty
            ) of
                {true, ValidatedSteps} ->
                    {true, ValidatedSteps};
                false ->
                    false
            end
    end.

get_reset_frequency() ->
    ?NONCE_LIMITER_RESET_FREQUENCY.

get_entropy_reset_point(PrevStepNumber, StepNumber) ->
    ResetLine =
        (PrevStepNumber div get_reset_frequency() + 1) * get_reset_frequency(),
    case ResetLine > StepNumber of
        true -> none;
        false -> ResetLine
    end.

maybe_add_entropy(PrevOutput, PrevStepNumber, StepNumber, Seed) ->
    case get_entropy_reset_point(PrevStepNumber, StepNumber) of
        StepNumber -> mix_seed(PrevOutput, Seed);
        _ -> PrevOutput
    end.

mix_seed(PrevOutput, Seed) ->
    SeedH = crypto:hash(sha256, Seed),
    mix_seed2(PrevOutput, SeedH).

%% @doc Return {Seed, NextSeed, PartitionUpperBound, NextPartitionUpperBound, VDFDifficulty}
%% for the next block mined at `StepNumber`, given `PrevBlock`.
get_seed_data(StepNumber, PrevBlock) ->
    PrevInfo = extract_nonce_limiter_info(PrevBlock),
    PrevStepNumber =
        hb_util:int(
            get_any(
                PrevInfo,
                [<<"global-step-number">>, <<"global_step_number">>, global_step_number],
                0
            )
        ),
    Seed = get_any(PrevInfo, [<<"seed">>, seed], <<>>),
    NextSeed = get_any(PrevInfo, [<<"next-seed">>, <<"next_seed">>, next_seed], <<>>),
    PartitionUpperBound =
        hb_util:int(
            get_any(
                PrevInfo,
                [<<"partition-upper-bound">>, <<"partition_upper_bound">>, partition_upper_bound],
                0
            )
        ),
    NextPartitionUpperBound =
        hb_util:int(
            get_any(
                PrevInfo,
                [
                    <<"next-partition-upper-bound">>,
                    <<"next_partition_upper_bound">>,
                    next_partition_upper_bound
                ],
                0
            )
        ),
    VDFDifficulty =
        hb_util:int(
            get_any(
                PrevInfo,
                [<<"vdf-difficulty">>, <<"vdf_difficulty">>, vdf_difficulty],
                ?VDF_DIFFICULTY
            )
        ),
    PrevNextVDFDifficulty =
        hb_util:int(
            get_any(
                PrevInfo,
                [<<"next-vdf-difficulty">>, <<"next_vdf_difficulty">>, next_vdf_difficulty],
                VDFDifficulty
            )
        ),
    true = StepNumber > PrevStepNumber,
    case get_entropy_reset_point(PrevStepNumber, StepNumber) of
        none ->
            {
                Seed,
                NextSeed,
                PartitionUpperBound,
                NextPartitionUpperBound,
                VDFDifficulty
            };
        _ ->
            {
                NextSeed,
                get_any(PrevBlock, [<<"indep-hash">>, <<"indep_hash">>, indep_hash], <<>>),
                NextPartitionUpperBound,
                hb_util:int(get_any(PrevBlock, [<<"weave-size">>, <<"weave_size">>, weave_size], 0)),
                PrevNextVDFDifficulty
            }
    end.

mix_seed2(PrevOutput, SeedH) ->
    crypto:hash(sha256, <<PrevOutput/binary, SeedH/binary>>).

extract_nonce_limiter_info(PrevBlock) ->
    case get_any(
        PrevBlock,
        [<<"nonce-limiter-info">>, <<"nonce_limiter_info">>, nonce_limiter_info],
        undefined
    ) of
        undefined ->
            PrevBlock;
        Info ->
            Info
    end.

get_any(_Map, [], Default) ->
    Default;
get_any(Map, [Key | Rest], Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> get_any(Map, Rest, Default)
    end.

nthtail_safe(N, List) when N =< 0 ->
    List;
nthtail_safe(N, List) ->
    case length(List) >= N of
        true -> lists:nthtail(N, List);
        false -> []
    end.

%% ------------------------------------------------------------------
%% Tests (adapted from upstream `ar_nonce_limiter_tests`)
%% ------------------------------------------------------------------

entropy_reset_point_test() ->
    ?assertEqual(none, get_entropy_reset_point(1, 100)),
    ?assertEqual(1200, get_entropy_reset_point(1199, 1200)),
    ?assertEqual(2400, get_entropy_reset_point(1201, 2400)).

maybe_add_entropy_test() ->
    PrevOutput = crypto:strong_rand_bytes(32),
    Seed = crypto:strong_rand_bytes(48),
    NoReset = maybe_add_entropy(PrevOutput, 1, 100, Seed),
    ?assertEqual(PrevOutput, NoReset),
    Reset = maybe_add_entropy(PrevOutput, 1199, 1200, Seed),
    ?assertNotEqual(PrevOutput, Reset),
    ?assertEqual(mix_seed(PrevOutput, Seed), Reset).

verify_no_reset_test() ->
    PrevOutput = hb_util:decode(<<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>),
    {ok, Output2, Checkpoints2} = compute(2, PrevOutput, 2),
    {ok, _Output3, Checkpoints3} = compute(3, Output2, 2),
    Hashes = lists:reverse(Checkpoints2) ++ lists:reverse(Checkpoints3),
    ?assertMatch(
        {true, [_ | _]},
        verify_no_reset(1, PrevOutput, 1, Hashes, 1, 2)
    ).

get_seed_data_no_reset_test() ->
    PrevBlock =
        #{
            <<"indep-hash">> => <<"prev-hash">>,
            <<"weave-size">> => 1000,
            <<"nonce-limiter-info">> =>
                #{
                    <<"global-step-number">> => 10,
                    <<"seed">> => <<"seed-a">>,
                    <<"next-seed">> => <<"seed-b">>,
                    <<"partition-upper-bound">> => 1000,
                    <<"next-partition-upper-bound">> => 2000,
                    <<"vdf-difficulty">> => 3,
                    <<"next-vdf-difficulty">> => 4
                }
        },
    ?assertEqual(
        {<<"seed-a">>, <<"seed-b">>, 1000, 2000, 3},
        get_seed_data(11, PrevBlock)
    ).

get_seed_data_with_reset_test() ->
    PrevBlock =
        #{
            <<"indep-hash">> => <<"new-seed">>,
            <<"weave-size">> => 5000,
            <<"nonce-limiter-info">> =>
                #{
                    <<"global-step-number">> => 1199,
                    <<"seed">> => <<"seed-a">>,
                    <<"next-seed">> => <<"seed-b">>,
                    <<"partition-upper-bound">> => 1000,
                    <<"next-partition-upper-bound">> => 2000,
                    <<"vdf-difficulty">> => 3,
                    <<"next-vdf-difficulty">> => 4
                }
        },
    ?assertEqual(
        {<<"seed-b">>, <<"new-seed">>, 2000, 5000, 4},
        get_seed_data(1200, PrevBlock)
    ).
