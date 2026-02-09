%%% @doc VDF helpers used by Arweave-compatible devices.
%%%
%%% This module mirrors the public API used by upstream Arweave's `ar_vdf`
%%% while using the Erlang fallback implementation so it can execute natively
%%% inside HyperBEAM.
-module(ar_vdf).

-export([
    compute/3,
    compute_legacy/3,
    compute2/3,
    verify/8,
    verify2/8,
    debug_sha_verify_no_reset/6,
    debug_sha_verify/8,
    debug_sha2/3,
    step_number_to_salt_number/1,
    checkpoint_buffer_to_checkpoints/1
]).

-include("include/hb.hrl").
-include("include/ar_vdf.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_VDF_DIFFICULTY, 2).
-define(ENCODED_PREV_OUTPUT, <<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>).
-define(RESET_SEED, <<"f_z7RLug8etm3SrmRf-xPwXEL0ZQ_xHng2A5emRDQBw">>).

step_number_to_salt_number(0) ->
    0;
step_number_to_salt_number(StepNumber) when is_integer(StepNumber) ->
    (StepNumber - 1) * ?VDF_CHECKPOINT_COUNT_IN_STEP + 1.

%% @doc Return {ok, Output, CheckpointBuffer}. The buffer matches upstream
%% orientation so `checkpoint_buffer_to_checkpoints/1` yields latest-first
%% checkpoints excluding Output.
compute(StartStepNumber, PrevOutput, IterationCount) ->
    {ok, Output, CheckpointsLatestFirst} =
        debug_sha2(StartStepNumber, PrevOutput, IterationCount),
    TailLatestFirst = tl(CheckpointsLatestFirst),
    Buffer = iolist_to_binary(lists:reverse(TailLatestFirst)),
    {ok, Output, Buffer}.

compute_legacy(StartStepNumber, PrevOutput, IterationCount) ->
    compute2(StartStepNumber, PrevOutput, IterationCount).

compute2(StartStepNumber, PrevOutput, IterationCount) ->
    {ok, Output, CheckpointBuffer} = compute(StartStepNumber, PrevOutput, IterationCount),
    Checkpoints = [Output | checkpoint_buffer_to_checkpoints(CheckpointBuffer)],
    {ok, Output, Checkpoints}.

verify(
    StartSalt,
    PrevOutput,
    NumCheckpointsBetweenHashes,
    Hashes,
    ResetSalt,
    ResetSeed,
    _ThreadCount,
    IterationCount
) ->
    case verify_internal(
        StartSalt,
        PrevOutput,
        NumCheckpointsBetweenHashes,
        Hashes,
        ResetSalt,
        ResetSeed,
        IterationCount
    ) of
        {ok, _StepOutputsLatestFirst} ->
            {true, iolist_to_binary(Hashes)};
        error ->
            false
    end.

verify2(
    StartStepNumber,
    PrevOutput,
    NumCheckpointsBetweenHashes,
    Hashes,
    ResetStepNumber,
    ResetSeed,
    _ThreadCount,
    IterationCount
) ->
    StartSalt = step_number_to_salt_number(StartStepNumber),
    ResetSalt = step_number_to_salt_number(ResetStepNumber - 1),
    case verify_internal(
        StartSalt,
        PrevOutput,
        NumCheckpointsBetweenHashes,
        Hashes,
        ResetSalt,
        ResetSeed,
        IterationCount
    ) of
        {ok, StepOutputsLatestFirst} ->
            {true, StepOutputsLatestFirst};
        error ->
            false
    end.

checkpoint_buffer_to_checkpoints(Buffer) ->
    checkpoint_buffer_to_checkpoints(Buffer, []).

checkpoint_buffer_to_checkpoints(<<>>, Checkpoints) ->
    Checkpoints;
checkpoint_buffer_to_checkpoints(<<Checkpoint:32/binary, Rest/binary>>, Checkpoints) ->
    checkpoint_buffer_to_checkpoints(Rest, [Checkpoint | Checkpoints]).

%% ------------------------------------------------------------------
%% Debug-compatible API
%% ------------------------------------------------------------------

debug_sha2(StepNumber, Output, IterationCount) ->
    Salt = step_number_to_salt_number(StepNumber - 1),
    {Output2, Checkpoints} =
        lists:foldl(
            fun(I, {Acc, L}) ->
                SaltBinary = <<(Salt + I):256>>,
                H = hash(IterationCount, SaltBinary, Acc),
                {H, [H | L]}
            end,
            {Output, []},
            lists:seq(0, ?VDF_CHECKPOINT_COUNT_IN_STEP - 1)
        ),
    {ok, Output2, Checkpoints}.

debug_sha_verify_no_reset(
    StepNumber,
    Output,
    NumCheckpointsBetweenHashes,
    Hashes,
    _ThreadCount,
    IterationCount
) ->
    Salt = step_number_to_salt_number(StepNumber),
    case verify_internal(
        Salt,
        Output,
        NumCheckpointsBetweenHashes,
        Hashes,
        -1,
        <<0:256>>,
        IterationCount
    ) of
        {ok, StepOutputsLatestFirst} ->
            {true, StepOutputsLatestFirst};
        error ->
            false
    end.

debug_sha_verify(
    StepNumber,
    Output,
    NumCheckpointsBetweenHashes,
    Hashes,
    ResetStepNumber,
    ResetSeed,
    _ThreadCount,
    IterationCount
) ->
    StartSalt = step_number_to_salt_number(StepNumber),
    ResetSalt = step_number_to_salt_number(ResetStepNumber - 1),
    case verify_internal(
        StartSalt,
        Output,
        NumCheckpointsBetweenHashes,
        Hashes,
        ResetSalt,
        ResetSeed,
        IterationCount
    ) of
        {ok, StepOutputsLatestFirst} ->
            {true, StepOutputsLatestFirst};
        error ->
            false
    end.

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

hash(0, _Salt, Input) ->
    Input;
hash(N, Salt, Input) when N > 0 ->
    hash(N - 1, Salt, crypto:hash(sha256, <<Salt/binary, Input/binary>>)).

verify_internal(
    StartSalt,
    PrevOutput,
    NumCheckpointsBetweenHashes,
    Hashes,
    ResetSalt,
    ResetSeed,
    IterationCount
) ->
    case NumCheckpointsBetweenHashes > 0 andalso is_list(Hashes) of
        false ->
            error;
        true ->
            verify_hashes(
                StartSalt,
                PrevOutput,
                NumCheckpointsBetweenHashes,
                Hashes,
                ResetSalt,
                ResetSeed,
                IterationCount,
                []
            )
    end.

verify_hashes(
    _Salt,
    _PrevOutput,
    _NumCheckpointsBetweenHashes,
    [],
    _ResetSalt,
    _ResetSeed,
    _IterationCount,
    StepOutputsLatestFirst
) ->
    {ok, StepOutputsLatestFirst};
verify_hashes(
    Salt,
    PrevOutput,
    NumCheckpointsBetweenHashes,
    [ExpectedHash | Rest],
    ResetSalt,
    ResetSeed,
    IterationCount,
    StepOutputsLatestFirst
) ->
    case advance_hashes(
        Salt,
        PrevOutput,
        NumCheckpointsBetweenHashes,
        ResetSalt,
        ResetSeed,
        IterationCount,
        StepOutputsLatestFirst
    ) of
        {ok, NextSalt, NextOutput, NextStepOutputsLatestFirst}
                when NextOutput =:= ExpectedHash ->
            verify_hashes(
                NextSalt,
                NextOutput,
                NumCheckpointsBetweenHashes,
                Rest,
                ResetSalt,
                ResetSeed,
                IterationCount,
                NextStepOutputsLatestFirst
            );
        _ ->
            error
    end.

advance_hashes(
    Salt,
    PrevOutput,
    0,
    _ResetSalt,
    _ResetSeed,
    _IterationCount,
    StepOutputsLatestFirst
) ->
    {ok, Salt, PrevOutput, StepOutputsLatestFirst};
advance_hashes(
    Salt,
    PrevOutput,
    Remaining,
    ResetSalt,
    ResetSeed,
    IterationCount,
    StepOutputsLatestFirst
) when Remaining > 0 ->
    MaybeMixed =
        case Salt =:= ResetSalt of
            true -> crypto:hash(sha256, <<PrevOutput/binary, ResetSeed/binary>>);
            false -> PrevOutput
        end,
    SaltBinary = <<Salt:256>>,
    Output = hash(IterationCount, SaltBinary, MaybeMixed),
    NextStepOutputsLatestFirst =
        case Salt rem ?VDF_CHECKPOINT_COUNT_IN_STEP of
            0 -> [Output | StepOutputsLatestFirst];
            _ -> StepOutputsLatestFirst
        end,
    advance_hashes(
        Salt + 1,
        Output,
        Remaining - 1,
        ResetSalt,
        ResetSeed,
        IterationCount,
        NextStepOutputsLatestFirst
    ).

break_byte(Buf, Pos) ->
    Head = binary:part(Buf, 0, Pos),
    Tail = binary:part(Buf, Pos + 1, size(Buf) - Pos - 1),
    ChangedByte = binary:at(Buf, Pos) bxor 1,
    <<Head/binary, ChangedByte, Tail/binary>>.

%% ------------------------------------------------------------------
%% Tests (adapted from upstream `ar_vdf_tests`)
%% ------------------------------------------------------------------

step_number_to_salt_number_test() ->
    ?assertEqual(0, step_number_to_salt_number(0)),
    ?assertEqual(1, step_number_to_salt_number(1)),
    ?assertEqual(26, step_number_to_salt_number(2)).

vdf_basic_compute_verify_test() ->
    StartStepNumber1 = 2,
    StartStepNumber2 = 3,
    StartSalt1 = step_number_to_salt_number(StartStepNumber1 - 1),
    StartSalt2 = step_number_to_salt_number(StartStepNumber2 - 1),
    PrevOutput = hb_util:decode(?ENCODED_PREV_OUTPUT),
    ResetSeed = hb_util:decode(?RESET_SEED),
    ResetSalt = -1,

    {ok, Output1, Checkpoints1} = compute2(StartStepNumber1, PrevOutput, ?TEST_VDF_DIFFICULTY),
    ?assertEqual(
        {true, iolist_to_binary(lists:reverse(Checkpoints1))},
        verify(
            StartSalt1,
            PrevOutput,
            1,
            lists:reverse(Checkpoints1),
            ResetSalt,
            ResetSeed,
            1,
            ?TEST_VDF_DIFFICULTY
        )
    ),

    {ok, _Output2, Checkpoints2} = compute2(StartStepNumber2, Output1, ?TEST_VDF_DIFFICULTY),
    ?assertEqual(
        {true, iolist_to_binary(lists:reverse(Checkpoints2))},
        verify(
            StartSalt2,
            Output1,
            1,
            lists:reverse(Checkpoints2),
            ResetSalt,
            ResetSeed,
            1,
            ?TEST_VDF_DIFFICULTY
        )
    ),

    Hashes = lists:reverse(Checkpoints1) ++ lists:reverse(Checkpoints2),
    ?assertEqual(
        {true, iolist_to_binary(Hashes)},
        verify(
            StartSalt1,
            PrevOutput,
            1,
            Hashes,
            ResetSalt,
            ResetSeed,
            1,
            ?TEST_VDF_DIFFICULTY
        )
    ),

    BufferHash = iolist_to_binary(Hashes),
    Corrupt = checkpoint_buffer_to_checkpoints(break_byte(BufferHash, 5)),
    ?assertEqual(
        false,
        verify(
            StartSalt1,
            PrevOutput,
            1,
            Corrupt,
            ResetSalt,
            ResetSeed,
            1,
            ?TEST_VDF_DIFFICULTY
        )
    ).

vdf_reset_verify_test() ->
    StartStepNumber1 = 2,
    StartStepNumber2 = 3,
    StartSalt1 = step_number_to_salt_number(StartStepNumber1 - 1),
    PrevOutput = hb_util:decode(?ENCODED_PREV_OUTPUT),
    ResetSeed = hb_util:decode(?RESET_SEED),
    ResetSalt = StartSalt1,

    MixOutput = crypto:hash(sha256, <<PrevOutput/binary, ResetSeed/binary>>),
    {ok, Output1, Checkpoints1} =
        compute2(StartStepNumber1, MixOutput, ?TEST_VDF_DIFFICULTY),
    {ok, _Output2, Checkpoints2} =
        compute2(StartStepNumber2, Output1, ?TEST_VDF_DIFFICULTY),

    Hashes = lists:reverse(Checkpoints1) ++ lists:reverse(Checkpoints2),
    ?assertEqual(
        {true, iolist_to_binary(Hashes)},
        verify(
            StartSalt1,
            PrevOutput,
            1,
            Hashes,
            ResetSalt,
            ResetSeed,
            1,
            ?TEST_VDF_DIFFICULTY
        )
    ),
    ok.
