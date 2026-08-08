%%% @doc Different utility functions for node and node worker.
%%%
%%% Copied and adapted from the arweave codebase.
%%% Should track:
%%% https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_node_utils.erl
%%%
%%% VENDOR: validate/6 is NOT a superset of block validation. Its own doc comment below
%%% says so: it performs no PoW, no PoA, no VDF and no RandomX. It is the
%%% field-consistency stage that runs after the pre-validator and the nonce limiter.
-module(ar_node_utils).

%% VENDOR: dropped solution_passes_diff_check/2 - it destructures #mining_solution{} and
%% calls ar_mining_server, neither of which is vendored.
-export([apply_tx/3, apply_txs/3, update_accounts/3, validate/6, do_validate/6,
	validate_block/2,
	h1_passes_diff_check/3, h2_passes_diff_check/3,
	block_passes_diff_check/1, block_passes_diff_check/2, passes_diff_check/4,
	scaled_diff/2, update_account/6, is_account_banned/2]).

-include("include/ar.hrl").
-include("include/ar_pricing.hrl").
-include("include/ar_consensus.hrl").
%% VENDOR: dropped -include("ar_mining.hrl"), needed only by solution_passes_diff_check/2.
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================


%% @doc Update the given accounts by applying a transaction.
apply_tx(Accounts, Denomination, TX) ->
	Addr = ar_tx:get_owner_address(TX),
	case maps:get(Addr, Accounts, not_found) of
		not_found ->
			Accounts;
		_ ->
			apply_tx2(Accounts, Denomination, Addr, TX)
	end.

%% @doc Update the given accounts by applying the given transactions.
apply_txs(Accounts, Denomination, TXs) ->
	lists:foldl(fun(TX, Acc) -> apply_tx(Acc, Denomination, TX) end, Accounts, TXs).

%% @doc Distribute transaction fees across accounts and the endowment pool,
%% reserve a reward for the current miner, release the reserved reward to the corresponding
%% miner. If a double-signing proof is provided, ban the account and assign a
%% reward to the prover.
update_accounts(B, PrevB, Accounts) ->
	EndowmentPool = PrevB#block.reward_pool,
	Rate = ar_pricing:usd_to_ar_rate(PrevB),
	PricePerGiBMinute = PrevB#block.price_per_gib_minute,
	KryderPlusRateMultiplierLatch = PrevB#block.kryder_plus_rate_multiplier_latch,
	KryderPlusRateMultiplier = PrevB#block.kryder_plus_rate_multiplier,
	Denomination = PrevB#block.denomination,
	DebtSupply = PrevB#block.debt_supply,
	TXs = B#block.txs,
	BlockInterval = ar_block_time_history:compute_block_interval(PrevB),
	Args =
		get_miner_reward_and_endowment_pool({EndowmentPool, DebtSupply, TXs,
				B#block.reward_addr, B#block.weave_size, B#block.height, B#block.timestamp,
				Rate, PricePerGiBMinute, KryderPlusRateMultiplierLatch,
				KryderPlusRateMultiplier, Denomination, BlockInterval}),
	Accounts2 = apply_txs(Accounts, Denomination, TXs),
	true = B#block.height >= ar_fork:height_2_6(),
	update_accounts2(B, PrevB, Accounts2, Args).

%%--------------------------------------------------------------------
%% @doc Perform the last stage of block validation. The majority of
%% the checks are made in `ar_block_pre_validator.erl',
%% `ar_nonce_limiter.erl', and `ar_node_utils:update_accounts/3'.
%% @end
%%--------------------------------------------------------------------
-spec validate(NewB, B, Wallets, BlocksAnchors, RecentTXMap, PartitionUpperBound) -> Return when
	NewB :: #block{},
	B :: #block{},
	Wallets :: term(),
	BlocksAnchors :: term(),
	RecentTXMap :: term(),
	PartitionUpperBound :: term(),
	Return :: valid | {invalid, Reason},
	Reason :: term().

%% VENDOR: the log terms carried ar_util:encode(indep_hash); the raw binary is
%% logged instead, since these are log terms rather than anything consensus
%% reads. The try/catch is load-bearing - it turns any
%% exception raised by a check into {invalid, validation_exception} rather than killing
%% the caller.
validate(NewB, B, Wallets, BlockAnchors, RecentTXMap, PartitionUpperBound) ->
	?LOG_INFO([{event, validating_block}, {hash, NewB#block.indep_hash}]),
	case timer:tc(
		fun() ->
			try
				do_validate(NewB, B, Wallets, BlockAnchors, RecentTXMap, PartitionUpperBound)
			catch
				C:R:S ->
					?LOG_ERROR([
						{event, block_validation_exception},
						{class, C},
						{reason, R},
						{stacktrace, S},
						{hash, NewB#block.indep_hash},
						{height, NewB#block.height}
					]),
					{invalid, validation_exception}
			end

		end
	) of
		{TimeTaken, valid} ->
			?LOG_INFO([{event, block_validation_successful},
					{hash, NewB#block.indep_hash},
					{time_taken_us, TimeTaken}]),
			valid;
		{TimeTaken, {invalid, Reason}} ->
			?LOG_INFO([{event, block_validation_failed}, {reason, Reason},
					{hash, NewB#block.indep_hash},
					{time_taken_us, TimeTaken}]),
			{invalid, Reason};
		{TimeTaken, {error, Reason}} ->
			?LOG_INFO([{event, block_validation_failed}, {reason, Reason},
					{hash, NewB#block.indep_hash},
					{time_taken_us, TimeTaken}]),
			{invalid, Reason};
		{TimeTaken, Else} ->
			?LOG_ERROR([{event, block_validation_failed}, {reason, Else},
					{hash, NewB#block.indep_hash},
					{time_taken_us, TimeTaken}]),
			{invalid, Else}

	end.

h1_passes_diff_check(H1, DiffPair, PackingDifficulty) ->
	passes_diff_check(H1, true, DiffPair, PackingDifficulty).

h2_passes_diff_check(H2, DiffPair, PackingDifficulty) ->
	passes_diff_check(H2, false, DiffPair, PackingDifficulty).

block_passes_diff_check(Block) ->
	SolutionHash = Block#block.hash,
	block_passes_diff_check(SolutionHash, Block).

block_passes_diff_check(SolutionHash, Block) ->
	IsPoA1 = (Block#block.recall_byte2 == undefined),
	PackingDifficulty = Block#block.packing_difficulty,
	DiffPair = ar_difficulty:diff_pair(Block),
	passes_diff_check(SolutionHash, IsPoA1, DiffPair, PackingDifficulty).

%% VENDOR: dropped the -ifdef(LOCALNET) clause, which skips the difficulty check entirely.
passes_diff_check(SolutionHash, IsPoA1, not_set, _PackingDifficulty) ->
	?LOG_ERROR([{event, diff_check_not_set}, {solution_hash, SolutionHash}, {is_poa1, IsPoA1}]),
	false;
passes_diff_check(SolutionHash, IsPoA1, {PoA1Diff, Diff}, PackingDifficulty) ->
	Diff2 =
		case IsPoA1 of
			true ->
				PoA1Diff;
			false ->
				Diff
		end,
	binary:decode_unsigned(SolutionHash) > scaled_diff(Diff2, PackingDifficulty).

scaled_diff(RawDiff, PackingDifficulty) ->
	case PackingDifficulty of
		0 ->
			RawDiff;
		_ ->
			SubDiff = ar_difficulty:sub_diff(RawDiff, ?COMPOSITE_PACKING_SUB_CHUNK_COUNT),
			%% We are introducing composite packing along with reducing the recall range
			%% from 200 MiB to 50 MiB while keeping the total worth of the nonces constant
			%% so we want the mining difficulty to be 1 / (PackingDifficulty * 4)
			%% of the difficulty applied to the 200 MiB-range nonces.
			ar_difficulty:scale_diff(SubDiff, {1, PackingDifficulty * 4},
					%% The minimal difficulty height. It does not change at the
					%% packing difficulty fork.
					ar_fork:height_2_8())
	end.

update_account(Addr, Balance, LastTX, 1, true, Accounts) ->
	maps:put(Addr, {Balance, LastTX}, Accounts);
update_account(Addr, Balance, LastTX, Denomination, MiningPermission, Accounts) ->
	maps:put(Addr, {Balance, LastTX, Denomination, MiningPermission}, Accounts).

is_account_banned(Addr, Accounts) ->
	case maps:get(Addr, Accounts, not_found) of
		not_found ->
			false;
		{_, _} ->
			false;
		{_, _, _, MiningPermission} ->
			not MiningPermission
	end.

%%%===================================================================
%%% Private functions.
%%%===================================================================

apply_tx2(Accounts, Denomination, Addr, TX) ->
	update_recipient_balance(
			update_sender_balance(Accounts, Denomination, Addr, TX), Denomination, TX).

update_sender_balance(Accounts, Denomination, Addr,
		#tx{
			id = ID,
			quantity = Qty,
			reward = Reward,
			denomination = TXDenomination
		}) ->
	case maps:get(Addr, Accounts, not_found) of
		{Balance, _LastTX} ->
			Balance2 = ar_pricing:redenominate(Balance, 1, Denomination),
			Spent = ar_pricing:redenominate(Qty + Reward, TXDenomination, Denomination),
			update_account(Addr, Balance2 - Spent, ID, Denomination, true, Accounts);
		{Balance, _LastTX, AccountDenomination, MiningPermission} ->
			Balance2 = ar_pricing:redenominate(Balance, AccountDenomination, Denomination),
			Spent = ar_pricing:redenominate(Qty + Reward, TXDenomination, Denomination),
			update_account(Addr, Balance2 - Spent, ID, Denomination, MiningPermission,
					Accounts);
		_ ->
			Accounts
	end.

update_recipient_balance(Accounts, _Denomination, #tx{ quantity = 0 }) ->
	Accounts;
update_recipient_balance(Accounts, Denomination,
		#tx{
			target = To,
			quantity = Qty,
			denomination = TXDenomination
		}) ->
	case maps:get(To, Accounts, not_found) of
		not_found ->
			Qty2 = ar_pricing:redenominate(Qty, TXDenomination, Denomination),
			update_account(To, Qty2, <<>>, Denomination, true, Accounts);
		{Balance, LastTX} ->
			Qty2 = ar_pricing:redenominate(Qty, TXDenomination, Denomination),
			Balance2 = ar_pricing:redenominate(Balance, 1, Denomination),
			update_account(To, Balance2 + Qty2, LastTX, Denomination, true, Accounts);
		{Balance, LastTX, AccountDenomination, MiningPermission} ->
			Qty2 = ar_pricing:redenominate(Qty, TXDenomination, Denomination),
			Balance2 = ar_pricing:redenominate(Balance, AccountDenomination, Denomination),
			update_account(To, Balance2 + Qty2, LastTX, Denomination, MiningPermission,
					Accounts)
	end.

get_miner_reward_and_endowment_pool(Args) ->
	{EndowmentPool, DebtSupply, TXs, RewardAddr, WeaveSize, Height, Timestamp, Rate,
			PricePerGiBMinute, KryderPlusRateMultiplierLatch, KryderPlusRateMultiplier,
			Denomination, BlockInterval} = Args,
	true = Height >= ar_fork:height_2_4(),
	case ar_pricing_transition:is_v2_pricing_height(Height) of
		true ->
			{MinerReward, EndowmentPool2, DebtSupply2, KryderPlusRateMultiplierLatch2,
					KryderPlusRateMultiplier2, _, _} = ar_pricing:get_miner_reward_endowment_pool_debt_supply({EndowmentPool, DebtSupply,
					TXs, WeaveSize, Height, PricePerGiBMinute, KryderPlusRateMultiplierLatch,
					KryderPlusRateMultiplier, Denomination, BlockInterval}),
			{MinerReward, EndowmentPool2, DebtSupply2, KryderPlusRateMultiplierLatch2, KryderPlusRateMultiplier2};
		false ->
			{MinerReward, EndowmentPool2} = ar_pricing:get_miner_reward_and_endowment_pool({
					EndowmentPool, TXs, RewardAddr, WeaveSize, Height, Timestamp, Rate}),
			{MinerReward, EndowmentPool2, 0, 0, 1}
	end.

validate_account_anchors(Accounts, TXs) ->
	not lists:any(fun(TX) -> is_wallet_invalid(TX, Accounts) end, TXs).

update_accounts2(B, PrevB, Accounts, Args) ->
	case is_account_banned(B#block.reward_addr, Accounts) of
		true ->
			{error, mining_address_banned};
		false ->
			update_accounts3(B, PrevB, Accounts, Args)
	end.

update_accounts3(B, PrevB, Accounts, Args) ->
	case may_be_apply_double_signing_proof(B, PrevB, Accounts) of
		{ok, Accounts2} ->
			Accounts3 = ar_rewards:apply_rewards(PrevB, Accounts2),
			update_accounts4(B, PrevB, Accounts3, Args);
		Error ->
			Error
	end.

may_be_apply_double_signing_proof(#block{ double_signing_proof = undefined }, _PrevB, Accounts) ->
	{ok, Accounts};
may_be_apply_double_signing_proof(#block{
		double_signing_proof = {_Pub, Sig, _, _, _, Sig, _, _, _} }, _PrevB, _Accounts) ->
	{error, invalid_double_signing_proof_same_signature};
may_be_apply_double_signing_proof(B, PrevB, Accounts) ->
	{_Pub, _Signature1, CDiff1, PrevCDiff1, _Preimage1, _Signature2, CDiff2, PrevCDiff2,
			_Preimage2} = B#block.double_signing_proof,
	case ar_block:get_double_signing_condition(CDiff1, PrevCDiff1, CDiff2, PrevCDiff2) of
		false ->
			{error, invalid_double_signing_proof_cdiff};
		true ->
			may_be_apply_double_signing_proof2(B, PrevB, Accounts)
	end.

may_be_apply_double_signing_proof2(B, PrevB, Accounts) ->
	{Pub, _Signature1, _CDiff1, _PrevCDiff1, _Preimage1, _Signature2, _CDiff2, _PrevCDiff2,
			_Preimage2} = B#block.double_signing_proof,
	Key = ar_block:get_reward_key(Pub, B#block.height),
	case B#block.reward_key == Key of
		true ->
			{error, invalid_double_signing_proof_same_address};
		false ->
			Addr = ar_wallet:to_address(Key),
			case is_account_banned(Addr, Accounts) of
				true ->
					{error, invalid_double_signing_proof_already_banned};
				false ->
					LockedRewards = ar_rewards:get_locked_rewards(PrevB),
					case ar_rewards:has_locked_reward(Addr, LockedRewards) of
						false ->
							{error, invalid_double_signing_proof_not_in_reward_history};
						true ->
							may_be_apply_double_signing_proof3(B, PrevB, Accounts)
					end
			end
	end.

may_be_apply_double_signing_proof3(B, PrevB, Accounts) ->
	#block{ height = Height } = B,
	{Pub, Signature1, CDiff1, PrevCDiff1, Preimage1, Signature2, CDiff2, PrevCDiff2,
			Preimage2} = B#block.double_signing_proof,
	SignaturePreimage1 = ar_block:get_block_signature_preimage(CDiff1, PrevCDiff1,
			Preimage1, Height),
	Key = ar_block:get_reward_key(Pub, B#block.height),
	Addr = ar_wallet:to_address(Key),
	case ar_wallet:verify(Key, SignaturePreimage1, Signature1) of
		false ->
			{error, invalid_double_signing_proof_invalid_signature};
		true ->
			SignaturePreimage2 = ar_block:get_block_signature_preimage(CDiff2, PrevCDiff2,
					Preimage2, Height),
			case ar_wallet:verify(Key, SignaturePreimage2, Signature2) of
				false ->
					{error, invalid_double_signing_proof_invalid_signature};
				true ->
					?LOG_INFO([{event, banning_account},
							{address, Addr},
							{previous_block, B#block.previous_block},
							{height, Height}]),
					{ok, ban_account(Addr, Accounts, PrevB#block.denomination)}
			end
	end.

ban_account(Addr, Accounts, Denomination) ->
	case maps:get(Addr, Accounts, not_found) of
		not_found ->
			maps:put(Addr, {1, <<>>, Denomination, false}, Accounts);
		{Balance, LastTX} ->
			Balance2 = ar_pricing:redenominate(Balance, 1, Denomination),
			maps:put(Addr, {Balance2 + 1, LastTX, Denomination, false}, Accounts);
		{Balance, LastTX, AccountDenomination, _MiningPermission} ->
			Balance2 = ar_pricing:redenominate(Balance, AccountDenomination, Denomination),
			maps:put(Addr, {Balance2 + 1, LastTX, Denomination, false}, Accounts)
	end.

update_accounts4(B, PrevB, Accounts, Args) ->
	{MinerReward, EndowmentPool, DebtSupply, KryderPlusRateMultiplierLatch,
			KryderPlusRateMultiplier} = Args,
	case B#block.double_signing_proof of
		undefined ->
			update_accounts5(B, Accounts, Args);
		Proof ->
			Denomination = PrevB#block.denomination,
			BannedAddr = ar_wallet:hash_pub_key(element(1, Proof)),
			Sum = ar_rewards:get_total_reward_for_address(BannedAddr, PrevB) - 1,
			{Dividend, Divisor} = ?DOUBLE_SIGNING_PROVER_REWARD_SHARE,
			LockedRewards = ar_rewards:get_locked_rewards(PrevB),
			Sample = lists:sublist(LockedRewards, ?DOUBLE_SIGNING_REWARD_SAMPLE_SIZE),
			{Min, MinDenomination} = get_minimal_reward(Sample),
			Min2 = ar_pricing:redenominate(Min, MinDenomination, Denomination),
			ProverReward = min(Min2 * Dividend div Divisor, Sum),
			{MinerReward, EndowmentPool, DebtSupply, KryderPlusRateMultiplierLatch,
					KryderPlusRateMultiplier} = Args,
			EndowmentPool2 = EndowmentPool + Sum - ProverReward,
			Accounts2 = ar_rewards:apply_reward(Accounts, B#block.reward_addr, ProverReward,
					Denomination),
			Args2 = {MinerReward, EndowmentPool2, DebtSupply, KryderPlusRateMultiplierLatch,
					KryderPlusRateMultiplier},
			update_accounts5(B, Accounts2, Args2)
	end.

get_minimal_reward(RewardHistory) ->
	get_minimal_reward(
			%% Make sure to traverse in the order of not decreasing denomination.
			lists:reverse(RewardHistory), infinity, 1).

get_minimal_reward([], Min, Denomination) ->
	{Min, Denomination};
get_minimal_reward([{_Addr, _HashRate, Reward, RewardDenomination} | RewardHistory], Min,
		Denomination) ->
	Min2 =
		case Min of
			infinity ->
				infinity;
			_ ->
				ar_pricing:redenominate(Min, Denomination, RewardDenomination)
		end,
	case Reward < Min2 of
		true ->
			get_minimal_reward(RewardHistory, Reward, RewardDenomination);
		false ->
			get_minimal_reward(RewardHistory, Min, Denomination)
	end.

%% VENDOR: dropped the ar_testnet:top_up_test_wallet/2 call. On mainnet it is the
%% identity - the crediting clause is behind -ifdef(TESTNET).
update_accounts5(B, Accounts, Args) ->
	{MinerReward, EndowmentPool, DebtSupply, KryderPlusRateMultiplierLatch,
			KryderPlusRateMultiplier} = Args,
	case validate_account_anchors(Accounts, B#block.txs) of
		true ->
			{ok, {EndowmentPool, MinerReward, DebtSupply, KryderPlusRateMultiplierLatch,
					KryderPlusRateMultiplier, Accounts}};
		false ->
			{error, invalid_account_anchors}
	end.

do_validate(NewB, OldB, Wallets, BlockAnchors, RecentTXMap, PartitionUpperBound) ->
	validate_block(weave_size, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap,
			PartitionUpperBound}).

validate_block(weave_size, {#block{ txs = TXs } = NewB, OldB, Wallets, BlockAnchors,
		RecentTXMap, PartitionUpperBound}) ->
	case ar_block:verify_weave_size(NewB, OldB, TXs) of
		false ->
			{invalid, invalid_weave_size};
		true ->
			validate_block(previous_block, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap,
					PartitionUpperBound})
	end;

validate_block(previous_block, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap,
		PartitionUpperBound}) ->
	case OldB#block.indep_hash == NewB#block.previous_block of
		false ->
			{invalid, invalid_previous_block};
		true ->
			validate_block(previous_solution_hash,
				{NewB, OldB, Wallets, BlockAnchors, RecentTXMap, PartitionUpperBound})
	end;

validate_block(previous_solution_hash, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap,
		PartitionUpperBound}) ->
	true = NewB#block.height >= ar_fork:height_2_6(),
	case NewB#block.previous_solution_hash == OldB#block.hash of
		false ->
			{invalid, invalid_previous_solution_hash};
		true ->
			validate_block(packing_2_5_threshold, {NewB, OldB, Wallets, BlockAnchors,
					RecentTXMap, PartitionUpperBound})
	end;

validate_block(packing_2_5_threshold, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap,
		PartitionUpperBound}) ->
	ExpectedPackingThreshold = ar_block:get_packing_threshold(OldB, PartitionUpperBound),
	Valid =
		case ExpectedPackingThreshold of
			undefined ->
				true;
			_ ->
				NewB#block.packing_2_5_threshold == ExpectedPackingThreshold
		end,
	case Valid of
		true ->
			validate_block(strict_data_split_threshold,
					{NewB, OldB, Wallets, BlockAnchors, RecentTXMap});
		false ->
			{error, invalid_packing_2_5_threshold}
	end;

%% VENDOR: this clause routes straight to usd_to_ar_rate. Upstream's
%% validate_block(difficulty, ...) clause sits between the two in the source but is
%% never reached - difficulty and the retarget are validated in the pre-validator, by
%% ar_retarget:validate_difficulty/2. It is not vendored.
validate_block(strict_data_split_threshold,
		{NewB, OldB, Wallets, BlockAnchors, RecentTXMap}) ->
	Height = NewB#block.height,
	Fork_2_5 = ar_fork:height_2_5(),
	Valid =
		case Height == Fork_2_5 of
			true ->
				NewB#block.strict_data_split_threshold == OldB#block.weave_size;
			false ->
				case Height > Fork_2_5 of
					true ->
						NewB#block.strict_data_split_threshold ==
								OldB#block.strict_data_split_threshold;
					false ->
						true
				end
		end,
	case Valid of
		true ->
			true = NewB#block.height >= ar_fork:height_2_6(),
			validate_block(usd_to_ar_rate, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap});
		false ->
			{error, invalid_strict_data_split_threshold}
	end;

validate_block(usd_to_ar_rate, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap}) ->
	{USDToARRate, ScheduledUSDToARRate} = ar_pricing:recalculate_usd_to_ar_rate(OldB),
	case NewB#block.usd_to_ar_rate == USDToARRate
			andalso NewB#block.scheduled_usd_to_ar_rate == ScheduledUSDToARRate of
		false ->
			{invalid, invalid_usd_to_ar_rate};
		true ->
			validate_block(denomination, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap})
	end;

validate_block(denomination, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap}) ->
	#block{ height = Height, denomination = Denomination,
			redenomination_height = RedenominationHeight } = NewB,
	true = Height >= ar_fork:height_2_6(),
	case ar_pricing:may_be_redenominate(OldB) of
		{Denomination, RedenominationHeight} ->
			validate_block(reward_history_hash, {NewB, OldB, Wallets, BlockAnchors,
					RecentTXMap});
		_ ->
			{invalid, invalid_denomination}
	end;

validate_block(reward_history_hash, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap}) ->
	#block{ reward = Reward, reward_history_hash = RewardHistoryHash,
			denomination = Denomination, height = Height } = NewB,
	#block{ reward_history = RewardHistory,
			reward_history_hash = PreviousRewardHistoryHash } = OldB,
	HashRate = ar_difficulty:get_hash_rate_fixed_ratio(NewB),
	RewardAddr = NewB#block.reward_addr,
	%% Pre-2.8: slice the reward history to compute the hash
	%% Post-2.8: use the previous reward history hash and the head of the history to compute
	%% the new hash.
	LockedRewards = ar_rewards:trim_locked_rewards(Height,
		[{RewardAddr, HashRate, Reward, Denomination} | RewardHistory]),
	case ar_rewards:reward_history_hash(Height, PreviousRewardHistoryHash, LockedRewards) of
		RewardHistoryHash ->
			validate_block(block_time_history_hash, {NewB, OldB, Wallets, BlockAnchors,
					RecentTXMap});
		_ ->
			{invalid, invalid_reward_history_hash}
	end;

validate_block(block_time_history_hash, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap}) ->
	case NewB#block.height >= ar_fork:height_2_7() of
		false ->
			validate_block(next_vdf_difficulty, {NewB, OldB, Wallets, BlockAnchors,
					RecentTXMap});
		true ->
			#block{ block_time_history_hash = HistoryHash } = NewB,
			History = ar_block_time_history:update_history(NewB, OldB),
			case ar_block_time_history:hash(History) of
				HistoryHash ->
					validate_block(next_vdf_difficulty, {NewB, OldB, Wallets, BlockAnchors,
							RecentTXMap});
				_ ->
					{invalid, invalid_block_time_history_hash}
			end
	end;

validate_block(next_vdf_difficulty, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap}) ->
	case NewB#block.height >= ar_fork:height_2_7() of
		false ->
			validate_block(price_per_gib_minute, {NewB, OldB, Wallets, BlockAnchors,
					RecentTXMap});
		true ->
			ExpectedNextVDFDifficulty = ar_block:compute_next_vdf_difficulty(OldB),
			#nonce_limiter_info{ next_vdf_difficulty = NextVDFDifficulty } =
				NewB#block.nonce_limiter_info,
			case ExpectedNextVDFDifficulty == NextVDFDifficulty of
				false ->
					{invalid, invalid_next_vdf_difficulty};
				true ->
					validate_block(price_per_gib_minute, {NewB, OldB, Wallets, BlockAnchors,
							RecentTXMap})
			end
	end;

validate_block(price_per_gib_minute, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap}) ->
	#block{ denomination = Denomination } = NewB,
	#block{ denomination = PrevDenomination } = OldB,
	{Price, ScheduledPrice} = ar_pricing:recalculate_price_per_gib_minute(OldB),
	Price2 = ar_pricing:redenominate(Price, PrevDenomination, Denomination),
	ScheduledPrice2 = ar_pricing:redenominate(ScheduledPrice, PrevDenomination, Denomination),
	case NewB#block.price_per_gib_minute == Price2
			andalso NewB#block.scheduled_price_per_gib_minute == ScheduledPrice2 of
		false ->
			{invalid, invalid_price_per_gib_minute};
		true ->
			validate_block(txs, {NewB, OldB, Wallets, BlockAnchors, RecentTXMap})
	end;

%% VENDOR: this clause routes straight to tx_root. Upstream's
%% validate_block(block_field_sizes, ...) clause sits between the two in the source but
%% is never reached - the 2.6 field size limits are enforced structurally by
%% ar_serialize:binary_to_block/1. It is not vendored.
validate_block(txs, {NewB = #block{ timestamp = Timestamp, height = Height, txs = TXs },
		OldB, Wallets, BlockAnchors, RecentTXMap}) ->
	Rate = ar_pricing:usd_to_ar_rate(OldB),
	PricePerGiBMinute = OldB#block.price_per_gib_minute,
	KryderPlusRateMultiplier = OldB#block.kryder_plus_rate_multiplier,
	Denomination = OldB#block.denomination,
	RedenominationHeight = OldB#block.redenomination_height,
	Args = {TXs, Rate, PricePerGiBMinute, KryderPlusRateMultiplier, Denomination, Height - 1,
			RedenominationHeight, Timestamp, Wallets, BlockAnchors, RecentTXMap},
	case ar_tx_replay_pool:verify_block_txs(Args) of
		invalid ->
			{invalid, invalid_txs};
		valid ->
			true = Height >= ar_fork:height_2_6(),
			%% The field size limits in 2.6 are naturally asserted in
			%% ar_serialize:binary_to_block/1.
			validate_block(tx_root, {NewB, OldB})
	end;

validate_block(tx_root, {NewB, OldB}) ->
	case ar_block:verify_tx_root(NewB) of
		false ->
			{invalid, invalid_tx_root};
		true ->
			validate_block(block_index_root, {NewB, OldB})
	end;

validate_block(block_index_root, {NewB, OldB}) ->
	case ar_block:verify_block_hash_list_merkle(NewB, OldB) of
		false ->
			{invalid, invalid_block_index_root};
		true ->
			validate_block(last_retarget, {NewB, OldB})
	end;

validate_block(last_retarget, {NewB, OldB}) ->
	case ar_block:verify_last_retarget(NewB, OldB) of
		false ->
			{invalid, invalid_last_retarget};
		true ->
			validate_block(cumulative_diff, {NewB, OldB})
	end;

validate_block(cumulative_diff, {NewB, OldB}) ->
	case ar_block:verify_cumulative_diff(NewB, OldB) of
		false ->
			{invalid, invalid_cumulative_difficulty};
		true ->
			validate_block(merkle_rebase_support_threshold, {NewB, OldB})
	end;

validate_block(merkle_rebase_support_threshold, {NewB, OldB}) ->
	#block{ height = Height } = NewB,
	case Height > ar_fork:height_2_7() of
		true ->
			case NewB#block.merkle_rebase_support_threshold
					== OldB#block.merkle_rebase_support_threshold of
				false ->
					{error, invalid_merkle_rebase_support_threshold};
				true ->
					valid
			end;
		false ->
			case Height == ar_fork:height_2_7() of
				true ->
					case NewB#block.merkle_rebase_support_threshold
							== OldB#block.weave_size of
						true ->
							valid;
						false ->
							{error, invalid_merkle_rebase_support_threshold}
					end;
				false ->
					valid
			end
	end.

%% VENDOR: dropped the -ifdef(AR_TEST) clause, which lets unsigned transactions through.
is_wallet_invalid(TX, Wallets) ->
	OwnerAddress = ar_tx:get_owner_address(TX),
	case maps:get(OwnerAddress, Wallets, not_found) of
		{Balance, LastTX} when Balance >= 0 ->
			case Balance of
				0 ->
					byte_size(LastTX) == 0;
				_ ->
					false
			end;
		{Balance, LastTX, _Denomination, _MiningPermission} when Balance >= 0 ->
			case Balance of
				0 ->
					byte_size(LastTX) == 0;
				_ ->
					false
			end;
		_ ->
			true
	end.
