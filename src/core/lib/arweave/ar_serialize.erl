%%% @doc The module contains the serialization and deserialization utilities for the
%%% various protocol entitities - transactions, blocks, proofs, etc
%%%
%%% Copied and adapted from the arweave codebase.
%%% Should track: https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_serialize.erl
%%%
%%% VENDOR: this is the block/proof subset of upstream's 2437-line module. The
%%% transaction, mining-pool, VDF-server, coordinated-mining, wallet-list and
%%% ARQL legs are not vendored. See src/core/lib/arweave/VENDOR.md.
-module(ar_serialize).

-export([block_to_binary/1, binary_to_block/1, binary_to_poa/1,
		block_index_to_binary/1, binary_to_block_index/1,
		encode_double_signing_proof/2, encode_int/2, encode_bin/2,
		encode_bin_list/3, reward_history_to_binary/1, binary_to_reward_history/1,
		block_time_history_to_binary/1, binary_to_block_time_history/1]).

-include("include/ar.hrl").
-include("include/ar_consensus.hrl").
-include("include/ar_vdf.hrl").


%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Serialize the block.
block_to_binary(#block{ indep_hash = H, previous_block = PrevH, timestamp = TS,
		nonce = Nonce, height = Height, diff = Diff, cumulative_diff = CDiff,
		last_retarget = LastRetarget, hash = Hash, block_size = BlockSize,
		weave_size = WeaveSize, reward_addr = Addr, tx_root = TXRoot,
		wallet_list = WalletList, hash_list_merkle = HashListMerkle,
		reward_pool = RewardPool, packing_2_5_threshold = Threshold,
		strict_data_split_threshold = StrictChunkThreshold,
		usd_to_ar_rate = Rate, scheduled_usd_to_ar_rate = ScheduledRate,
		poa = #poa{ option = Option, chunk = Chunk, data_path = DataPath,
				tx_path = TXPath }, tags = Tags, txs = TXs } = B) ->
	Addr2 = case Addr of unclaimed -> <<>>; _ -> Addr end,
	{RateDividend, RateDivisor} = case Rate of undefined -> {undefined, undefined};
			_ -> Rate end,
	{ScheduledRateDividend, ScheduledRateDivisor} =
			case ScheduledRate of
				undefined ->
					{undefined, undefined};
				_ ->
					ScheduledRate
			end,
	Nonce2 = case B#block.height >= ar_fork:height_2_6() of
			true -> binary:encode_unsigned(Nonce, big); false -> Nonce end,
	<< H:48/binary, (encode_bin(PrevH, 8))/binary, (encode_int(TS, 8))/binary,
			(encode_bin(Nonce2, 16))/binary, (encode_int(Height, 8))/binary,
			(encode_int(Diff, 16))/binary, (encode_int(CDiff, 16))/binary,
			(encode_int(LastRetarget, 8))/binary, (encode_bin(Hash, 8))/binary,
			(encode_int(BlockSize, 16))/binary, (encode_int(WeaveSize, 16))/binary,
			(encode_bin(Addr2, 8))/binary, (encode_bin(TXRoot, 8))/binary,
			(encode_bin(WalletList, 8))/binary, (encode_bin(HashListMerkle, 8))/binary,
			(encode_int(RewardPool, 8))/binary, (encode_int(Threshold, 8))/binary,
			(encode_int(StrictChunkThreshold, 8))/binary,
			(encode_int(RateDividend, 8))/binary,
			(encode_int(RateDivisor, 8))/binary,
			(encode_int(ScheduledRateDividend, 8))/binary,
			(encode_int(ScheduledRateDivisor, 8))/binary, (encode_int(Option, 8))/binary,
			(encode_bin(Chunk, 24))/binary, (encode_bin(TXPath, 24))/binary,
			(encode_bin(DataPath, 24))/binary, (encode_bin_list(Tags, 16, 16))/binary,
			(encode_transactions(TXs))/binary, (encode_post_2_6_fields(B))/binary >>.

%% @doc Deserialize the block.
binary_to_block(<< H:48/binary, PrevHSize:8, PrevH:PrevHSize/binary,
		TSSize:8, TS:(TSSize * 8),
		NonceSize:16, Nonce:NonceSize/binary,
		HeightSize:8, Height:(HeightSize * 8),
		DiffSize:16, Diff:(DiffSize * 8),
		CDiffSize:16, CDiff:(CDiffSize * 8),
		LastRetargetSize:8, LastRetarget:(LastRetargetSize * 8),
		HashSize:8, Hash:HashSize/binary,
		BlockSizeSize:16, BlockSize:(BlockSizeSize * 8),
		WeaveSizeSize:16, WeaveSize:(WeaveSizeSize * 8),
		AddrSize:8, Addr:AddrSize/binary,
		TXRootSize:8, TXRoot:TXRootSize/binary, % 0 or 32
		WalletListSize:8, WalletList:WalletListSize/binary,
		HashListMerkleSize:8, HashListMerkle:HashListMerkleSize/binary,
		RewardPoolSize:8, RewardPool:(RewardPoolSize * 8),
		PackingThresholdSize:8, Threshold:(PackingThresholdSize * 8),
		StrictChunkThresholdSize:8, StrictChunkThreshold:(StrictChunkThresholdSize * 8),
		RateDividendSize:8, RateDividend:(RateDividendSize * 8),
		RateDivisorSize:8, RateDivisor:(RateDivisorSize * 8),
		SchedRateDividendSize:8, SchedRateDividend:(SchedRateDividendSize * 8),
		SchedRateDivisorSize:8, SchedRateDivisor:(SchedRateDivisorSize * 8),
		PoAOptionSize:8, PoAOption:(PoAOptionSize * 8),
		ChunkSize:24, Chunk:ChunkSize/binary,
		TXPathSize:24, TXPath:TXPathSize/binary,
		DataPathSize:24, DataPath:DataPathSize/binary,
		Rest/binary >>) when NonceSize =< 512 ->
	Threshold2 = case PackingThresholdSize of 0 -> undefined; _ -> Threshold end,
	StrictChunkThreshold2 = case StrictChunkThresholdSize of 0 -> undefined;
			_ -> StrictChunkThreshold end,
	Rate = case RateDivisorSize of 0 -> undefined;
			_ -> {RateDividend, RateDivisor} end,
	ScheduledRate = case SchedRateDivisorSize of 0 -> undefined;
			_ -> {SchedRateDividend, SchedRateDivisor} end,
	case Height >= ar_fork:height_2_5() andalso
			(Rate == undefined orelse ScheduledRate == undefined) of
		true ->
			{error, invalid_block_input};
		false ->
			Addr2 = case {AddrSize, Height >= ar_fork:height_2_6()} of
					{0, false} -> unclaimed; _ -> Addr end,
			B = #block{ indep_hash = H, previous_block = PrevH, timestamp = TS,
					nonce = Nonce, height = Height, diff = Diff,
					cumulative_diff = CDiff,
					last_retarget = LastRetarget, hash = Hash,
					block_size = BlockSize,
					weave_size = WeaveSize, reward_addr = Addr2, tx_root = TXRoot,
					wallet_list = WalletList, hash_list_merkle = HashListMerkle,
					reward_pool = RewardPool, packing_2_5_threshold = Threshold2,
					strict_data_split_threshold = StrictChunkThreshold2,
					usd_to_ar_rate = Rate, scheduled_usd_to_ar_rate = ScheduledRate,
					poa = #poa{ option = PoAOption, chunk = Chunk, data_path = DataPath,
							tx_path = TXPath }},
			parse_block_tags_transactions(Rest, B)
	end;
binary_to_block(_Bin) ->
	{error, invalid_block_input}.

reward_history_to_binary(RewardHistory) ->
	reward_history_to_binary(RewardHistory, []).

binary_to_reward_history(Bin) ->
	binary_to_reward_history(Bin, []).

block_time_history_to_binary(BlockTimeHistory) ->
	block_time_history_to_binary(BlockTimeHistory, []).

binary_to_block_time_history(Bin) ->
	binary_to_block_time_history(Bin, []).


encode_double_signing_proof(undefined, _Height) ->
	<< 0:8 >>;
encode_double_signing_proof(Proof, Height) ->
	{Key, Sig1, CDiff1, PrevCDiff1, Preimage1,
			Sig2, CDiff2, PrevCDiff2, Preimage2} = Proof,
	case Height >= ar_fork:height_2_9() of
		false ->
			<< 1:8, Key:512/binary, Sig1:512/binary,
				(ar_serialize:encode_int(CDiff1, 16))/binary,
				(ar_serialize:encode_int(PrevCDiff1, 16))/binary, Preimage1:64/binary,
				Sig2:512/binary, (ar_serialize:encode_int(CDiff2, 16))/binary,
				(ar_serialize:encode_int(PrevCDiff2, 16))/binary, Preimage2:64/binary >>;
		true ->
			<< 1:8, (ar_serialize:encode_bin(Key, 16))/binary,
				(ar_serialize:encode_bin(Sig1, 16))/binary,
				(ar_serialize:encode_int(CDiff1, 16))/binary,
				(ar_serialize:encode_int(PrevCDiff1, 16))/binary, Preimage1:64/binary,
				(ar_serialize:encode_bin(Sig2, 16))/binary,
				(ar_serialize:encode_int(CDiff2, 16))/binary,
				(ar_serialize:encode_int(PrevCDiff2, 16))/binary, Preimage2:64/binary >>
	end.

binary_to_poa(<< ChunkSize:24, Chunk:ChunkSize/binary,
		TXPathSize:24, TXPath:TXPathSize/binary,
		DataPathSize:24, DataPath:DataPathSize/binary,
		PackingSize:8, PackingBinary:PackingSize/binary >>) ->
	Packing = binary_to_packing(PackingBinary, error),
	case Packing of
		error ->
			{error, invalid_packing};
		_ ->
			{ok, #{ chunk => Chunk, data_path => DataPath, tx_path => TXPath,
					packing => Packing }}
	end;
binary_to_poa(_Rest) ->
	{error, invalid_input}.

block_index_to_binary(BI) ->
	block_index_to_binary(BI, []).

binary_to_block_index(Bin) ->
	binary_to_block_index(Bin, []).

%%%===================================================================
%%% Private functions.
%%%===================================================================

reward_history_to_binary([], IOList) ->
	iolist_to_binary(IOList);
reward_history_to_binary([{Addr, HashRate, Reward, Denomination} | RewardHistory], IOList) ->
	reward_history_to_binary(RewardHistory, [Addr, ar_serialize:encode_int(HashRate, 8),
			ar_serialize:encode_int(Reward, 8), << Denomination:24 >> | IOList]).

binary_to_reward_history(<< Addr:32/binary, HashRateSize:8, HashRate:(HashRateSize * 8),
		RewardSize:8, Reward:(RewardSize * 8), Denomination:24, Rest/binary >>,
		RewardHistory) ->
	binary_to_reward_history(Rest, [{Addr, HashRate, Reward, Denomination} | RewardHistory]);
binary_to_reward_history(<<>>, RewardHistory) ->
	{ok, RewardHistory};
binary_to_reward_history(_Rest, _RewardHistory) ->
	{error, invalid_reward_history}.

block_time_history_to_binary([], IOList) ->
	iolist_to_binary(IOList);
block_time_history_to_binary([{BlockInterval, VDFInterval, ChunkCount} | BlockTimeHistory],
		IOList) ->
	block_time_history_to_binary(BlockTimeHistory, [
			ar_serialize:encode_int(BlockInterval, 8),
			ar_serialize:encode_int(VDFInterval, 8),
			ar_serialize:encode_int(ChunkCount, 8)
	| IOList]).

binary_to_block_time_history(<< BlockIntervalSize:8,
			BlockInterval:(BlockIntervalSize * 8),
			VDFIntervalSize:8, VDFInterval:(VDFIntervalSize * 8),
			ChunkCountSize:8, ChunkCount:(ChunkCountSize * 8), Rest/binary >>,
		BlockTimeHistory) ->
	binary_to_block_time_history(Rest,
			[{BlockInterval, VDFInterval, ChunkCount} | BlockTimeHistory]);
binary_to_block_time_history(<<>>, BlockTimeHistory) ->
	{ok, BlockTimeHistory};
binary_to_block_time_history(_Rest, _BlockTimeHistory) ->
	{error, invalid_block_time_history}.

encode_post_2_6_fields(#block{ height = Height, hash_preimage = HashPreimage,
			recall_byte = RecallByte, reward = Reward,
			previous_solution_hash = PreviousSolutionHash,
			partition_number = PartitionNumber,
			signature = Sig, nonce_limiter_info = NonceLimiterInfo,
			poa2 = #poa{ chunk = Chunk, data_path = DataPath, tx_path = TXPath },
			recall_byte2 = RecallByte2, price_per_gib_minute = PricePerGiBMinute,
			scheduled_price_per_gib_minute = ScheduledPricePerGiBMinute,
			reward_history_hash = RewardHistoryHash, debt_supply = DebtSupply,
			kryder_plus_rate_multiplier = KryderPlusRateMultiplier,
			kryder_plus_rate_multiplier_latch = KryderPlusRateMultiplierLatch,
			denomination = Denomination, redenomination_height = RedenominationHeight,
			double_signing_proof = DoubleSigningProof,
			previous_cumulative_diff = PrevCDiff } = B) ->
	RewardKey = case B#block.reward_key of undefined -> <<>>; {_Type, Key} -> Key end,
	case Height >= ar_fork:height_2_6() of
		false ->
			<<>>;
		true ->
			<< (encode_bin(HashPreimage, 8))/binary, (encode_int(RecallByte, 16))/binary,
				(encode_int(Reward, 8))/binary, (encode_bin(Sig, 16))/binary,
				(encode_int(RecallByte2, 16))/binary,
				(encode_bin(PreviousSolutionHash, 8))/binary, PartitionNumber:256,
				(encode_nonce_limiter_info(NonceLimiterInfo))/binary,
				(encode_bin(Chunk, 24))/binary, (encode_bin(RewardKey, 16))/binary,
				(encode_bin(TXPath, 24))/binary, (encode_bin(DataPath, 24))/binary,
				(encode_int(PricePerGiBMinute, 8))/binary,
				(encode_int(ScheduledPricePerGiBMinute, 8))/binary,
				RewardHistoryHash:32/binary, (encode_int(DebtSupply, 8))/binary,
				KryderPlusRateMultiplier:24, KryderPlusRateMultiplierLatch:8,
				Denomination:24, (encode_int(RedenominationHeight, 8))/binary,
				(encode_int(PrevCDiff, 16))/binary,
				(encode_double_signing_proof(DoubleSigningProof, Height))/binary,
				(encode_post_2_7_fields(B))/binary >>
	end.

encode_post_2_7_fields(#block{ height = Height,
		merkle_rebase_support_threshold = Threshold, chunk_hash = ChunkHash,
		chunk2_hash = Chunk2Hash,
		block_time_history_hash = BlockTimeHistoryHash,
		nonce_limiter_info = #nonce_limiter_info{ vdf_difficulty = VDFDifficulty,
				next_vdf_difficulty = NextVDFDifficulty } } = B) ->
	case Height >= ar_fork:height_2_7() of
		true ->
			<< (encode_int(Threshold, 16))/binary, ChunkHash:32/binary,
					(encode_bin(Chunk2Hash, 8))/binary,
					BlockTimeHistoryHash:32/binary,
					(encode_int(VDFDifficulty, 8))/binary,
					(encode_int(NextVDFDifficulty, 8))/binary,
					(encode_post_2_8_fields(B))/binary >>;
		false ->
			<<>>
	end.

encode_post_2_8_fields(#block{ height = Height,
		packing_difficulty = PackingDifficulty,
		unpacked_chunk_hash = UnpackedChunkHash, unpacked_chunk2_hash = UnpackedChunk2Hash,
		poa = #poa{ unpacked_chunk = UnpackedChunk },
		poa2 = #poa{ unpacked_chunk = UnpackedChunk2 }} = B) ->
	case Height >= ar_fork:height_2_8() of
		false ->
			<<>>;
		true ->
			<< PackingDifficulty:8,
				(ar_serialize:encode_bin(UnpackedChunkHash, 8))/binary,
				(ar_serialize:encode_bin(UnpackedChunk2Hash, 8))/binary,
				(ar_serialize:encode_bin(UnpackedChunk, 24))/binary,
				(ar_serialize:encode_bin(UnpackedChunk2, 24))/binary,
				(encode_post_2_9_fields(B))/binary >>
	end.

encode_post_2_9_fields(#block{ height = Height, replica_format = ReplicaFormat }) ->
	case Height >= ar_fork:height_2_9() of
		false ->
			<<>>;
		true ->
			<< ReplicaFormat:8 >>
	end.

encode_nonce_limiter_info(#nonce_limiter_info{ output = Output, global_step_number = N,
		seed = Seed, next_seed = NextSeed, partition_upper_bound = PartitionUpperBound,
		next_partition_upper_bound = NextPartitionUpperBound, prev_output = PrevOutput,
		last_step_checkpoints = Checkpoints, steps = Steps }) ->
	CheckpointsLen = length(Checkpoints),
	StepsLen = length(Steps),
	<< Output:32/binary, N:64, Seed:48/binary, NextSeed:48/binary,
			(encode_bin(PrevOutput, 8))/binary,
			PartitionUpperBound:256, NextPartitionUpperBound:256,
			CheckpointsLen:16, (iolist_to_binary(Checkpoints))/binary,
			StepsLen:16, (iolist_to_binary(Steps))/binary >>.

encode_int(undefined, SizeBits) ->
	<< 0:SizeBits >>;
encode_int(N, SizeBits) ->
	Bin = binary:encode_unsigned(N, big),
	<< (byte_size(Bin)):SizeBits, Bin/binary >>.

encode_bin(undefined, SizeBits) ->
	<< 0:SizeBits >>;
encode_bin(Bin, SizeBits) ->
	<< (byte_size(Bin)):SizeBits, Bin/binary >>.

encode_bin_list(Bins, LenBits, ElemSizeBits) ->
	encode_bin_list(Bins, [], 0, LenBits, ElemSizeBits).

encode_bin_list([], Encoded, N, LenBits, _ElemSizeBits) ->
	<< N:LenBits, (iolist_to_binary(Encoded))/binary >>;
encode_bin_list([Bin | Bins], Encoded, N, LenBits, ElemSizeBits) ->
	Elem = encode_bin(Bin, ElemSizeBits),
	encode_bin_list(Bins, [Elem | Encoded], N + 1, LenBits, ElemSizeBits).

%% VENDOR: upstream also serializes whole `#tx{}' records here, through
%% encode_tx/1 (src/ar_serialize.erl:787-825). HyperBEAM's `#tx{}' record has
%% diverged from upstream's - it spells `last_tx' as `anchor' and defaults
%% `format' to `ans104' - so encode_tx/1 and the matching parse_tx/1 body clause
%% are not vendored. Blocks served by `/block2' carry bare 32-byte transaction
%% identifiers, which is the clause below; a block carrying inline transaction
%% bodies fails to serialize rather than serializing wrongly.
encode_transactions(TXs) ->
	encode_transactions(TXs, [], 0).

encode_transactions([], Encoded, N) ->
	<< N:16, (iolist_to_binary(Encoded))/binary >>;
encode_transactions([<< TXID:32/binary >> | TXs], Encoded, N) ->
	encode_transactions(TXs, [<< 32:24, TXID:32/binary >> | Encoded], N + 1).

parse_block_tags_transactions(Bin, B) ->
	case parse_block_tags(Bin) of
		{error, Reason} ->
			{error, Reason};
		{ok, Tags, Rest} ->
			parse_block_transactions(Rest, B#block{ tags = Tags })
	end.

parse_block_transactions(Bin, B) ->
	case {parse_block_transactions(Bin), B#block.height < ar_fork:height_2_6()} of
		{{error, Reason}, _} ->
			{error, Reason};
		{{ok, TXs, <<>>}, true} ->
			{ok, B#block{ txs = TXs }};
		{{ok, TXs, Rest}, false} ->
			parse_block_post_2_6_fields(B#block{ txs = TXs }, Rest);
		_ ->
			{error, invalid_input1}
	end.

parse_block_post_2_6_fields(B, << HashPreimageSize:8, HashPreimage:HashPreimageSize/binary,
		RecallByteSize:16, RecallByte:(RecallByteSize * 8), RewardSize:8,
		Reward:(RewardSize * 8), SigSize:16, Sig:SigSize/binary,
		RecallByte2Size:16, RecallByte2:(RecallByte2Size * 8), PreviousSolutionHashSize:8,
		PreviousSolutionHash:PreviousSolutionHashSize/binary,
		PartitionNumber:256, NonceLimiterOutput:32/binary,
		GlobalStepNumber:64, Seed:48/binary, NextSeed:48/binary,
		PrevOutputSize:8, PrevOutput:PrevOutputSize/binary,
		PartitionUpperBound:256, NextPartitionUpperBound:256,
		LastCheckpointsLen:16, LastCheckpoints:(LastCheckpointsLen * 32)/binary,
		StepsLen:16, Steps:(StepsLen * 32)/binary,
		ChunkSize:24, Chunk:ChunkSize/binary, RewardKeySize:16,
		RewardKey:RewardKeySize/binary, TXPathSize:24, TXPath:TXPathSize/binary,
		DataPathSize:24, DataPath:DataPathSize/binary,
		PricePerGiBMinuteSize:8, PricePerGiBMinute:(PricePerGiBMinuteSize * 8),
		ScheduledPricePerGiBMinuteSize:8,
		ScheduledPricePerGiBMinute:(ScheduledPricePerGiBMinuteSize * 8),
		RewardHistoryHash:32/binary, DebtSupplySize:8, DebtSupply:(DebtSupplySize * 8),
		KryderPlusRateMultiplier:24, KryderPlusRateMultiplierLatch:8,
		Denomination:24, RedenominationHeightSize:8,
		RedenominationHeight:(RedenominationHeightSize * 8),
		PrevCDiffSize:16, PrevCDiff:(PrevCDiffSize * 8),
		Rest/binary >>) ->
	%% The only block where recall_byte may be undefined is the genesis block
	%% of a new weave.
	RecallByte_2 = case RecallByteSize of 0 -> undefined; _ -> RecallByte end,
	Height = B#block.height,
	Nonce = binary:decode_unsigned(B#block.nonce, big),
	NonceLimiterInfo = #nonce_limiter_info{ output = NonceLimiterOutput,
			prev_output = PrevOutput, global_step_number = GlobalStepNumber,
			seed = Seed, next_seed = NextSeed,
			partition_upper_bound = PartitionUpperBound,
			next_partition_upper_bound = NextPartitionUpperBound,
			last_step_checkpoints = parse_checkpoints(LastCheckpoints, Height),
			steps = parse_checkpoints(Steps, Height) },
	RecallByte2_2 = case RecallByte2Size of 0 -> undefined; _ -> RecallByte2 end,
	SigType =
		case {RewardKeySize, Height >= ar_fork:height_2_9()} of
			{?ECDSA_PUB_KEY_SIZE, true} ->
				?ECDSA_KEY_TYPE;
			_ ->
				?RSA_KEY_TYPE
		end,
	B2 = B#block{ hash_preimage = HashPreimage, recall_byte = RecallByte_2,
			reward = Reward, nonce = Nonce, recall_byte2 = RecallByte2_2,
			previous_solution_hash = PreviousSolutionHash,
			signature = Sig, partition_number = PartitionNumber,
			reward_key = {SigType, RewardKey},
			nonce_limiter_info = NonceLimiterInfo,
			poa2 = #poa{ chunk = Chunk, data_path = DataPath, tx_path = TXPath },
			price_per_gib_minute = PricePerGiBMinute,
			scheduled_price_per_gib_minute = ScheduledPricePerGiBMinute,
			reward_history_hash = RewardHistoryHash, debt_supply = DebtSupply,
			kryder_plus_rate_multiplier = KryderPlusRateMultiplier,
			kryder_plus_rate_multiplier_latch = KryderPlusRateMultiplierLatch,
			denomination = Denomination, redenomination_height = RedenominationHeight,
			previous_cumulative_diff = PrevCDiff },
	parse_double_signing_proof(Rest, B2);
parse_block_post_2_6_fields(_B, _Rest) ->
	{error, invalid_input4}.

parse_checkpoints(<<>>, 0) ->
	[];
parse_checkpoints(_, 0) ->
	{error, invalid_checkpoints};
parse_checkpoints(<< Checkpoint:32/binary >>, _Height) ->
	%% The block must have at least one checkpoint (the last nonce limiter output).
	[Checkpoint];
parse_checkpoints(<< Checkpoint:32/binary, Rest/binary >>, Height) ->
	[Checkpoint | parse_checkpoints(Rest, Height)].

parse_block_tags(<< TagsLen:16, Rest/binary >>) when TagsLen =< 2048 ->
	parse_block_tags(TagsLen, Rest, [], 0);
parse_block_tags(_Bin) ->
	{error, invalid_tags_input}.

parse_block_tags(0, Rest, Tags, _TotalSize) ->
	{ok, Tags, Rest};
parse_block_tags(N, << TagSize:16, Tag:TagSize/binary, Rest/binary >>, Tags, TotalSize)
		when TotalSize + TagSize =< 2048 ->
	parse_block_tags(N - 1, << Rest/binary >>, [Tag | Tags], TotalSize + TagSize);
parse_block_tags(_N, _Bin, _Tags, _TotalSize) ->
	{error, invalid_tag_input}.

parse_block_transactions(<< Count:16, Rest/binary >>) when Count =< 1000 ->
	parse_block_transactions(Count, Rest, []);
parse_block_transactions(_Bin) ->
	{error, invalid_transactions_input}.

parse_block_transactions(0, Rest, TXs) ->
	{ok, TXs, Rest};
parse_block_transactions(N, << Size:24, Bin:Size/binary, Rest/binary >>, TXs)
		when N > 0 ->
	case parse_tx(Bin) of
		{error, Reason} ->
			{error, Reason};
		{ok, TX} ->
			parse_block_transactions(N - 1, Rest, [TX | TXs])
	end;
parse_block_transactions(_N, _Rest, _TXs) ->
	{error, invalid_transactions2_input}.

%% VENDOR: only upstream's identifier-only clause is vendored. See the note on
%% encode_transactions/1 above.
parse_tx(<< TXID:32/binary >>) ->
	{ok, TXID};
parse_tx(_Bin) ->
	{error, invalid_tx_input}.

parse_double_signing_proof(<< 0:8, Rest/binary >>, B) ->
	parse_post_2_7_fields(Rest, B);
parse_double_signing_proof(Bin, #block{ height = Height } = B) ->
	case {Bin, Height >= ar_fork:height_2_9()} of
		{<< 1:8, Key:512/binary, Sig1:512/binary,
				CDiff1Size:16, CDiff1:(CDiff1Size * 8),
				PrevCDiff1Size:16, PrevCDiff1:(PrevCDiff1Size * 8),
				Preimage1:64/binary, Sig2:512/binary,
				CDiff2Size:16, CDiff2:(CDiff2Size * 8),
				PrevCDiff2Size:16, PrevCDiff2:(PrevCDiff2Size * 8),
				Preimage2:64/binary, Rest/binary >>, false} ->
			Proof = {Key, Sig1, CDiff1, PrevCDiff1, Preimage1,
					Sig2, CDiff2, PrevCDiff2, Preimage2},
			B2 = B#block{ double_signing_proof = Proof },
			parse_post_2_7_fields(Rest, B2);
		{_Bin, false} ->
			{error, invalid_double_signing_proof_input};
		{<< 1:8, KeySize:16, Key:KeySize/binary, Sig1Size:16, Sig1:Sig1Size/binary,
				CDiff1Size:16, CDiff1:(CDiff1Size * 8),
				PrevCDiff1Size:16, PrevCDiff1:(PrevCDiff1Size * 8),
				Preimage1:64/binary, Sig2Size:16, Sig2:Sig2Size/binary,
				CDiff2Size:16, CDiff2:(CDiff2Size * 8),
				PrevCDiff2Size:16, PrevCDiff2:(PrevCDiff2Size * 8),
				Preimage2:64/binary, Rest/binary >>, true}
					when (KeySize == ?RSA_BLOCK_SIG_SIZE andalso
							Sig1Size == ?RSA_BLOCK_SIG_SIZE andalso
							Sig2Size == ?RSA_BLOCK_SIG_SIZE) orelse
						(KeySize == ?ECDSA_PUB_KEY_SIZE andalso
							Sig1Size == ?ECDSA_SIG_SIZE andalso
							Sig2Size == ?ECDSA_SIG_SIZE) ->
			Proof = {Key, Sig1, CDiff1, PrevCDiff1, Preimage1,
					Sig2, CDiff2, PrevCDiff2, Preimage2},
			B2 = B#block{ double_signing_proof = Proof },
			parse_post_2_7_fields(Rest, B2);
		{_Bin, true} ->
			{error, invalid_double_signing_proof_input2}
end.

parse_post_2_7_fields(Rest, #block{ height = Height } = B) ->
	case {Rest, Height >= ar_fork:height_2_7()} of
		{<<>>, false} ->
			{ok, B};
		{<< ThresholdSize:16, Threshold:(ThresholdSize*8), ChunkHash:32/binary,
				Chunk2HashSize:8, Chunk2Hash:Chunk2HashSize/binary,
				BlockTimeHistoryHash:32/binary,
				VDFDifficultySize:8, VDFDifficulty:(VDFDifficultySize * 8),
				NextVDFDifficultySize:8, NextVDFDifficulty:(NextVDFDifficultySize * 8),
				Rest2/binary >>, true} ->
			Chunk2Hash2 = case Chunk2HashSize of 0 -> undefined; _ -> Chunk2Hash end,
			B2 = B#block{ merkle_rebase_support_threshold = Threshold,
					chunk_hash = ChunkHash, chunk2_hash = Chunk2Hash2,
					block_time_history_hash = BlockTimeHistoryHash,
					nonce_limiter_info = (B#block.nonce_limiter_info)#nonce_limiter_info{
							vdf_difficulty = VDFDifficulty,
							next_vdf_difficulty = NextVDFDifficulty } },
			parse_post_2_8_fields(Rest2, B2);
		_ ->
			{error, invalid_merkle_rebase_support_threshold}
	end.

parse_post_2_8_fields(Rest, #block{ height = Height, poa = PoA, poa2 = PoA2 } = B) ->
	case {Rest, Height >= ar_fork:height_2_8()} of
		{<<>>, false} ->
			{ok, B};
		{<< PackingDifficulty:8, UnpackedChunkHashSize:8,
				UnpackedChunkHash:UnpackedChunkHashSize/binary,
				UnpackedChunk2HashSize:8,
				UnpackedChunk2Hash:UnpackedChunk2HashSize/binary,
				UnpackedChunkSize:24,
				UnpackedChunk:UnpackedChunkSize/binary,
				UnpackedChunk2Size:24,
				UnpackedChunk2:UnpackedChunk2Size/binary, Rest2/binary >>, true} ->
			UnpackedChunkHash_2 =
				case UnpackedChunkHash of
					<<>> -> undefined;
					_ -> UnpackedChunkHash
				end,
			UnpackedChunk2Hash_2 =
				case UnpackedChunk2Hash of
					<<>> -> undefined;
					_ -> UnpackedChunk2Hash
				end,
			parse_post_2_9_fields(Rest2, B#block{ packing_difficulty = PackingDifficulty,
					unpacked_chunk_hash = UnpackedChunkHash_2,
					unpacked_chunk2_hash = UnpackedChunk2Hash_2,
					poa = PoA#poa{ unpacked_chunk = UnpackedChunk },
					poa2 = PoA2#poa{ unpacked_chunk = UnpackedChunk2 } });
		_ ->
			{error, invalid_packing_difficulty}
	end.

parse_post_2_9_fields(Rest, #block{ height = Height } = B) ->
	case {Rest, Height >= ar_fork:height_2_9()} of
		{<<>>, false} ->
			{ok, B};
		{<< ReplicaFormat:8 >>, true} ->
			{ok, B#block{ replica_format = ReplicaFormat }};
		_ ->
			{error, invalid_replica_format}
	end.

block_index_to_binary([], Encoded) ->
	iolist_to_binary(Encoded);
block_index_to_binary([{BH, WeaveSize, TXRoot} | BI], Encoded) ->
	block_index_to_binary(BI,
			[<< BH:48/binary, (encode_int(WeaveSize, 16))/binary,
				(encode_bin(TXRoot, 8))/binary >> | Encoded]).

binary_to_block_index(<<>>, BI) ->
	{ok, BI};
binary_to_block_index(<< BH:48/binary, WeaveSizeSize:16, WeaveSize:(WeaveSizeSize * 8),
		TXRootSize:8, TXRoot:TXRootSize/binary, Rest/binary >>, BI) ->
	binary_to_block_index(Rest, [{BH, WeaveSize, TXRoot} | BI]);
binary_to_block_index(_Rest, _BI) ->
	{error, invalid_input}.

binary_to_packing(<<"unpacked">>, _Error) ->
	unpacked;
binary_to_packing(<<"spora_2_5">>, _Error) ->
	spora_2_5;
binary_to_packing(<< "spora_2_6_", Addr/binary >>, Error) when byte_size(Addr) =< 64 ->
	case hb_util:safe_decode(Addr) of
		{ok, DecodedAddr} ->
			{spora_2_6, DecodedAddr};
		_ ->
			Error
	end;
binary_to_packing(<< "composite_", PackingDifficulty:8, Addr/binary >>, Error)
		when byte_size(Addr) =< 64,
		PackingDifficulty =< ?MAX_PACKING_DIFFICULTY ->
	case hb_util:safe_decode(Addr) of
		{ok, DecodedAddr} ->
			{composite, DecodedAddr, PackingDifficulty};
		_ ->
			Error
	end;
binary_to_packing(<< "replica_2_9_", Addr/binary >>, Error) when byte_size(Addr) =< 64 ->
	case hb_util:safe_decode(Addr) of
		{ok, DecodedAddr} ->
			{replica_2_9, DecodedAddr};
		_ ->
			Error
	end;
binary_to_packing(<<"unpacked_padded">>, _Error) ->
	unpacked_padded;
%% VENDOR: upstream has no catch-all here -- an unrecognised packing name raises
%% `function_clause'. Returning the caller's error term instead is the only
%% place in this port where a validation predicate's shape differs from
%% upstream. Both outcomes reject the block, so consensus is unaffected, but the
%% failure surfaces as the caller's error rather than a crash.
binary_to_packing(_Bin, Error) ->
	Error.

