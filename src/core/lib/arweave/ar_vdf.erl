%%% @doc Copied and adapted from the arweave codebase.
%%% Should track: https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_vdf.erl
-module(ar_vdf).

-export([compute/3, verify2/8, step_number_to_salt_number/1,
		checkpoint_buffer_to_checkpoints/1]).

-include("include/ar_vdf.hrl").
-include("include/ar.hrl").
%% VENDOR: dropped -include_lib("arweave_config/include/arweave_config.hrl"). The only
%% configuration this module reads is the VDF backend selector; see ?VDF_BACKEND below.

%% VENDOR: the mainnet default of the `vdf' config option
%% (arweave_config.hrl:421). We do not carry the option; a validating node has no
%% reason to pick a different SHA-2 implementation, and all three produce the same
%% output by construction.
-define(VDF_BACKEND, openssl).

step_number_to_salt_number(0) ->
	0;
step_number_to_salt_number(StepNumber) ->
	(StepNumber - 1) * ?VDF_CHECKPOINT_COUNT_IN_STEP + 1.

%% default IterationCount = ?VDF_DIFFICULTY
compute(StartStepNumber, PrevOutput, IterationCount) ->
	Salt = step_number_to_salt_number(StartStepNumber - 1),
	SaltBinary = << Salt:256 >>,
	case ?VDF_BACKEND of
		openssl ->
			ar_vdf_nif:vdf_sha2_nif(SaltBinary, PrevOutput, ?VDF_CHECKPOINT_COUNT_IN_STEP - 1, 0,
					IterationCount);
		fused ->
			ar_vdf_nif:vdf_sha2_fused_nif(SaltBinary, PrevOutput, ?VDF_CHECKPOINT_COUNT_IN_STEP - 1, 0,
					IterationCount);
		hiopt_m4 ->
			ar_vdf_nif:vdf_sha2_hiopt_nif(SaltBinary, PrevOutput, ?VDF_CHECKPOINT_COUNT_IN_STEP - 1, 0,
					IterationCount);
		_ ->
			ar_vdf_nif:vdf_sha2_nif(SaltBinary, PrevOutput, ?VDF_CHECKPOINT_COUNT_IN_STEP - 1, 0,
					IterationCount)
	end.

%% no reset in CheckpointGroups, then ResetStepNumber < StartSalt
%%   any number out of bounds of
%%   [StartSalt, StartSalt+group_list_to_sum_step(CheckpointGroups)]
verify(StartSalt, PrevOutput, NumCheckpointsBetweenHashes, Hashes,
		ResetSalt, ResetSeed, ThreadCount, IterationCount) ->
	StartSaltBinary = << StartSalt:256 >>,
	ResetSaltBinary = << ResetSalt:256 >>,
	NumHashes = length(Hashes),
	HashBuffer = iolist_to_binary(Hashes),
	RestStepsSize = ?VDF_BYTE_SIZE * (NumHashes - 1),
	case HashBuffer of
		<< RestSteps:RestStepsSize/binary, LastStep:?VDF_BYTE_SIZE/binary >> ->
			case ar_vdf_nif:vdf_parallel_sha_verify_with_reset_nif(StartSaltBinary,
					PrevOutput, NumHashes - 1, NumCheckpointsBetweenHashes - 1,
					IterationCount, RestSteps, LastStep, ResetSaltBinary, ResetSeed,
					ThreadCount) of
				{ok, Steps} ->
					{true, Steps};
				_ ->
					false
			end;
		_ ->
			false
	end.

verify2(StartStepNumber, PrevOutput, NumCheckpointsBetweenHashes, Hashes,
		ResetStepNumber, ResetSeed, ThreadCount, IterationCount) ->
	StartSalt = step_number_to_salt_number(StartStepNumber),
	ResetSalt = step_number_to_salt_number(ResetStepNumber - 1),
	case verify(StartSalt, PrevOutput, NumCheckpointsBetweenHashes, Hashes,
			ResetSalt, ResetSeed, ThreadCount, IterationCount) of
		false ->
			false;
		{true, CheckpointBuffer} ->
			{true, take_every_nth(?VDF_CHECKPOINT_COUNT_IN_STEP,
					checkpoint_buffer_to_checkpoints(CheckpointBuffer))}
	end.

checkpoint_buffer_to_checkpoints(Buffer) ->
	checkpoint_buffer_to_checkpoints(Buffer, []).

checkpoint_buffer_to_checkpoints(<<>>, Checkpoints) ->
	Checkpoints;
checkpoint_buffer_to_checkpoints(<< Checkpoint:32/binary, Rest/binary >>, Checkpoints) ->
	checkpoint_buffer_to_checkpoints(Rest, [Checkpoint | Checkpoints]).

take_every_nth(N, List) ->
	take_every_nth(N, List, 0).

take_every_nth(_N, [], _Index) ->
	[];
take_every_nth(N, [Element | Rest], Index) when Index rem N == 0 ->
	[Element | take_every_nth(N, Rest, Index + 1)];
take_every_nth(N, [_Element | Rest], Index) ->
	take_every_nth(N, Rest, Index + 1).
