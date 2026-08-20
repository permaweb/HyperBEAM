%%% @doc Copied and adapted from the arweave codebase.
%%% Should track:
%%% https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_nonce_limiter.erl
%%%
%%% VENDOR: upstream is a gen_server holding the VDF session store (six ets tables and a
%%% gb_set of sessions), a compute worker, four prometheus metrics and the VDF
%%% client/server protocol - apply_external_update/2, apply_chain/2 and
%%% ar_nonce_limiter_client. None of that is vendored. The client/server protocol in
%%% particular is a *trust* path: it accepts VDF steps from a configured peer without
%%% recomputing them. A trustless validator must not have it.
%%%
%%% What remains is the pure core: seed rotation, the entropy reset line, the session
%%% key, and the two verification entry points. Every function here is a pure function of
%%% its arguments - there is no process, no ets, no persistent_term and no configuration.
-module(ar_nonce_limiter).

-export([is_ahead_on_the_timeline/2, session_key/1, session_key/3,
		get_seed_data/2,
		get_reset_frequency/0, get_entropy_reset_point/2,
		validate_last_step_checkpoints/4,
		maybe_add_entropy/4, mix_seed/2, mix_seed2/2,
		verify/9, verify_no_reset/6]).

-include("include/ar.hrl").
-include("include/ar_vdf.hrl").
-include("include/ar_consensus.hrl").
%% VENDOR: dropped -include_lib("arweave_config/include/arweave_config.hrl"). The two
%% options this module read are the nonce limiter thread counts, which are now explicit
%% arguments of validate_last_step_checkpoints/4 and verify/9.

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Return true if the first solution is above the second one according
%% to the protocol ordering.
%%
%% VENDOR: dropped the -ifdef(LOCALNET) clause, which relaxes the comparison to >=.
is_ahead_on_the_timeline(NonceLimiterInfo1, NonceLimiterInfo2) ->
	#nonce_limiter_info{ global_step_number = N1 } = NonceLimiterInfo1,
	#nonce_limiter_info{ global_step_number = N2 } = NonceLimiterInfo2,
	N1 > N2.

session_key(#nonce_limiter_info{
		next_seed = NextSeed, global_step_number = StepNumber,
		next_vdf_difficulty = NextVDFDifficulty }) ->
	session_key(NextSeed, StepNumber, NextVDFDifficulty).

session_key(NextSeed, StepNumber, NextVDFDifficulty) ->
	{NextSeed, StepNumber div ar_nonce_limiter:get_reset_frequency(), NextVDFDifficulty}.

%% VENDOR: dropped the -ifdef(LOCALNET) clause, which permits two blocks on the same step.
assert_step_number_is_ahead(StepNumber, PrevStepNumber) ->
	true = StepNumber > PrevStepNumber.

%% @doc Return {Seed, NextSeed, PartitionUpperBound, NextPartitionUpperBound, VDFDifficulty}
%% for the block mined at StepNumber considering its previous block PrevB.
%% The previous block's independent hash, weave size, and VDF difficulty
%% become the new NextSeed, NextPartitionUpperBound, and NextVDFDifficulty
%% accordingly when we cross the next reset line.
%% Note: next_vdf_difficulty is not part of the seed data as it is computed using the
%% block_time_history - which is a heavier operation handled separate from the (quick) seed data
%% retrieval
get_seed_data(StepNumber, PrevB) ->
	NonceLimiterInfo = PrevB#block.nonce_limiter_info,
	#nonce_limiter_info{
		global_step_number = PrevStepNumber,
		seed = Seed, next_seed = NextSeed,
		partition_upper_bound = PartitionUpperBound,
		next_partition_upper_bound = NextPartitionUpperBound,
		%% VDF difficulty in use at the previous block
		vdf_difficulty = VDFDifficulty,
		%% Next VDF difficulty scheduled at the previous block
		next_vdf_difficulty = PrevNextVDFDifficulty
	} = NonceLimiterInfo,
	assert_step_number_is_ahead(StepNumber, PrevStepNumber),
	case get_entropy_reset_point(PrevStepNumber, StepNumber) of
		none ->
			%% Entropy reset line was not crossed between previous and current block
			{ Seed, NextSeed, PartitionUpperBound, NextPartitionUpperBound, VDFDifficulty };
		_ ->
			%% Entropy reset line was crossed between previous and current block
			{
				NextSeed, PrevB#block.indep_hash,
				NextPartitionUpperBound, PrevB#block.weave_size,
				%% The next VDF difficulty that was scheduled at the previous block
				%% (PrevNextVDFDifficulty) was applied when we crossed the entropy reset line and
				%% is now the current VDF difficulty.
				PrevNextVDFDifficulty
			}
	end.

%% @doc Quickly validate the checkpoints of the latest step.
%%
%% VENDOR: upstream's arity-3 validate_last_step_checkpoints/3 first consults the VDF
%% session cache through the gen_server (returning {true, cache_match} or
%% {false, cache_mismatch, CachedSteps}) and reads the thread count from the config. We
%% have neither, so the cache lookup is gone - the checkpoints are always recomputed -
%% and ThreadCount is the new fourth argument. The result is now plainly true | false.
%%
%% Upstream also calls get_or_init_nonce_limiter_info/1 to synthesise a nonce limiter
%% info for pre-2.6 blocks; every block we validate is post-2.9, so PrevB's own info is
%% read directly.
%%
%% NOTE. This proves that one second of VDF work happened somewhere, anchored on the
%% block's own unverified `steps' list. It is NOT a substitute for chaining the steps
%% with verify/9 or verify_no_reset/6.
validate_last_step_checkpoints(B = #block{ nonce_limiter_info = #nonce_limiter_info{
		global_step_number = StepNumber } },
		PrevB = #block{ nonce_limiter_info = #nonce_limiter_info{
				global_step_number = StepNumber } }, _PrevOutput, _ThreadCount) ->
	validate_last_step_checkpoints_same_step_number(B, PrevB);
validate_last_step_checkpoints(#block{
		nonce_limiter_info = #nonce_limiter_info{ output = Output,
				global_step_number = StepNumber, seed = Seed,
				vdf_difficulty = VDFDifficulty,
				last_step_checkpoints = [Output | _] = LastStepCheckpoints } }, PrevB,
				PrevOutput, ThreadCount)
		when length(LastStepCheckpoints) == ?VDF_CHECKPOINT_COUNT_IN_STEP ->
	#nonce_limiter_info{ global_step_number = PrevBStepNumber } =
			PrevB#block.nonce_limiter_info,
	PrevOutput2 = ar_nonce_limiter:maybe_add_entropy(
		PrevOutput, PrevBStepNumber, StepNumber, Seed),
	PrevStepNumber = StepNumber - 1,
	%% The 25 checkpoints are one ?VDF_CHECKPOINT_COUNT_IN_STEP apart on the wire but
	%% one *checkpoint* apart on the hash chain, hence NumCheckpointsBetweenHashes == 1.
	%% They arrive newest-first; the NIF wants ascending.
	case verify_no_reset(PrevStepNumber, PrevOutput2, 1,
			lists:reverse(LastStepCheckpoints), ThreadCount, VDFDifficulty) of
		{true, _Steps} ->
			true;
		false ->
			false
	end;
validate_last_step_checkpoints(_B, _PrevB, _PrevOutput, _ThreadCount) ->
	false.

%% VENDOR: dropped the -ifdef(LOCALNET) clause. Two blocks may never share a step number
%% on mainnet.
validate_last_step_checkpoints_same_step_number(_B, _PrevB) ->
	false.

get_reset_frequency() ->
	?NONCE_LIMITER_RESET_FREQUENCY.

%% @doc Determine whether StepNumber has passed the entropy reset line. If it has return the
%% reset line, otherwise return none.
get_entropy_reset_point(PrevStepNumber, StepNumber) ->
	ResetLine = (PrevStepNumber div ar_nonce_limiter:get_reset_frequency() + 1)
			* ar_nonce_limiter:get_reset_frequency(),
	case ResetLine > StepNumber of
		true ->
			none;
		false ->
			ResetLine
	end.

%% @doc Conditionally add entropy to PrevOutput if the configured number of steps have
%% passed. See ar_nonce_limiter:get_reset_frequency() for more details.
maybe_add_entropy(PrevOutput, PrevStepNumber, StepNumber, Seed) ->
	case get_entropy_reset_point(PrevStepNumber, StepNumber) of
		StepNumber ->
			mix_seed(PrevOutput, Seed);
		_ ->
			PrevOutput
	end.

%% @doc Add entropy to an earlier VDF output to mitigate the impact of a miner with a
%% fast VDF compute. See ar_nonce_limiter:get_reset_frequency() for more details.
mix_seed(PrevOutput, Seed) ->
	SeedH = crypto:hash(sha256, Seed),
	mix_seed2(PrevOutput, SeedH).

mix_seed2(PrevOutput, SeedH) ->
	crypto:hash(sha256, << PrevOutput/binary, SeedH/binary >>).

%% @doc Verify a range of VDF steps that crosses the entropy reset line at
%% ResetStepNumber. The steps before the line are verified at VDFDifficulty, the entropy
%% is mixed in here in Erlang, and the steps from the line on are verified at
%% NextVDFDifficulty.
%%
%% The reset branch inside the NIF is never taken: verify_no_reset/6 passes
%% ResetStepNumber = 0, so ar_vdf:verify2/8 computes
%% step_number_to_salt_number(-1) = -49, which the NIF receives as << -49:256 >> - that
%% is 2^256 - 49, a salt no step can ever reach. Passing ResetStepNumber = 1 instead
%% would yield salt 0, which step 1 *does* use, and would mix entropy where the network
%% does not. Do not "fix" it.
verify(StartStepNumber, PrevOutput, NumCheckpointsBetweenHashes, Hashes, ResetStepNumber,
		ResetSeed, ThreadCount, VDFDifficulty, NextVDFDifficulty) ->
	{Result1, PrevOutput2, ValidatedSteps1} =
		case lists:sublist(Hashes, ResetStepNumber - StartStepNumber - 1) of
			[] ->
				{true, mix_seed2(PrevOutput, ResetSeed), []};
			Hashes1 ->
				case verify_no_reset(StartStepNumber, PrevOutput,
						NumCheckpointsBetweenHashes, Hashes1, ThreadCount, VDFDifficulty) of
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
			Hashes2 = lists:nthtail(ResetStepNumber - StartStepNumber - 1, Hashes),
			case verify_no_reset(ResetStepNumber - 1, PrevOutput2, NumCheckpointsBetweenHashes,
					Hashes2, ThreadCount, NextVDFDifficulty) of
				{true, ValidatedSteps2} ->
					{true, ValidatedSteps2 ++ ValidatedSteps1};
				false ->
					false
			end
	end.

%% VENDOR: added. An empty hash list makes the NIF's binary match fail and verify/8
%% return a quiet `false', which is indistinguishable from a failed proof. A caller that
%% supplies no steps has a bug - and a validator that accepted it would be verifying the
%% VDF chain vacuously - so say so.
verify_no_reset(_StartStepNumber, _PrevOutput, _NumCheckpointsBetweenHashes, [], _ThreadCount,
		_VDFDifficulty) ->
	error(invalid_step_count);
%% VENDOR: dropped the debug_double_check/4 wrapper, which re-runs the whole
%% verification in pure Erlang when the `double_check_nonce_limiter' option is enabled.
%% ar_vdf:debug_sha_verify_no_reset/6 is still vendored, so the cross-check can be run
%% by hand.
verify_no_reset(StartStepNumber, PrevOutput, NumCheckpointsBetweenHashes, Hashes, ThreadCount,
		VDFDifficulty) ->
	Garbage = crypto:strong_rand_bytes(32),
	ar_vdf:verify2(StartStepNumber, PrevOutput, NumCheckpointsBetweenHashes, Hashes,
			0, Garbage, ThreadCount, VDFDifficulty).
