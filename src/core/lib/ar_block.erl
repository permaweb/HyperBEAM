%%% @doc Copied and adapted from the arweave codebase. 
%%% Should track: https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_block.erl
-module(ar_block).

-export([strict_data_split_threshold/0, get_chunk_padded_offset/1, generate_size_tagged_list_from_txs/2]).
-export([binary_to_json_struct/1]).

-include("include/ar.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

strict_data_split_threshold() -> ?STRICT_DATA_SPLIT_THRESHOLD.

%% @doc Return Offset if it is smaller than or equal to ar_block:strict_data_split_threshold().
%% Otherwise, return the offset of the last byte of the chunk + the size of the padding.
-spec get_chunk_padded_offset(Offset :: non_neg_integer()) -> non_neg_integer().
get_chunk_padded_offset(Offset) ->
	case Offset > ar_block:strict_data_split_threshold() of
		true ->
			ar_poa:get_padded_offset(Offset, ar_block:strict_data_split_threshold());
		false ->
			Offset
	end.

generate_size_tagged_list_from_txs(TXs, Height) ->
	lists:reverse(
		element(2,
			lists:foldl(
				fun(TX, {Pos, List}) ->
					DataSize = TX#tx.data_size,
					End = Pos + DataSize,
					case Height >= ar_fork:height_2_5() of
						true ->
							Padding = ar_tx:get_weave_size_increase(DataSize, Height)
									- DataSize,
							%% Encode the padding information in the Merkle tree.
							case Padding > 0 of
								true ->
									PaddingRoot = <<>>,
									{End + Padding, [{{padding, PaddingRoot}, End + Padding},
											{{TX, get_tx_data_root(TX)}, End} | List]};
								false ->
									{End, [{{TX, get_tx_data_root(TX)}, End} | List]}
							end;
						false ->
							{End, [{{TX, get_tx_data_root(TX)}, End} | List]}
					end
				end,
				{0, []},
				lists:sort(TXs)
			)
		)
	).

get_tx_data_root(#tx{ format = 2, data_root = DataRoot }) ->
    DataRoot;
get_tx_data_root(TX) ->
    (ar_tx:generate_chunk_tree(TX))#tx.data_root.

%% @doc Parse a block in the binary form of `ar_serialize:block_to_binary/1',
%% as Arweave's `/block2' endpoint gives it, into the JSON struct that
%% `ar_serialize:block_to_json_struct/1' gives for the block, and its
%% transactions in block order: a `#tx' header for each that the response
%% includes, or the ID. Blocks below the 2.9 fork height have other fields and
%% are refused.
-spec binary_to_json_struct(binary()) ->
	{ok, #{ binary() => term() }, [binary() | #tx{}]} | {error, atom()}.
binary_to_json_struct(<< H:48/binary, PrevHSize:8, PrevH:PrevHSize/binary,
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
		TXRootSize:8, TXRoot:TXRootSize/binary,
		WalletListSize:8, WalletList:WalletListSize/binary,
		HashListMerkleSize:8, HashListMerkle:HashListMerkleSize/binary,
		RewardPoolSize:8, RewardPool:(RewardPoolSize * 8),
		ThresholdSize:8, Threshold:(ThresholdSize * 8),
		StrictThresholdSize:8, StrictThreshold:(StrictThresholdSize * 8),
		RateDividendSize:8, RateDividend:(RateDividendSize * 8),
		RateDivisorSize:8, RateDivisor:(RateDivisorSize * 8),
		SchedRateDividendSize:8, SchedRateDividend:(SchedRateDividendSize * 8),
		SchedRateDivisorSize:8, SchedRateDivisor:(SchedRateDivisorSize * 8),
		PoAOptionSize:8, PoAOption:(PoAOptionSize * 8),
		ChunkSize:24, Chunk:ChunkSize/binary,
		TXPathSize:24, TXPath:TXPathSize/binary,
		DataPathSize:24, DataPath:DataPathSize/binary,
		Rest/binary >>) when NonceSize =< 512 ->
	maybe
		true ?= Height >= ar_fork:height_2_9()
			orelse {error, 'unsupported-block-height'},
		{ok, Tags, Rest2} ?= binary_to_block_tags(Rest),
		{ok, TXs, Rest3} ?= binary_to_txs(Rest2),
		{ok, Fields, UnpackedChunk} ?= binary_to_post_2_6_fields(Rest3),
		{ok,
			Fields#{
				% Arweave reads the nonce as an integer, so its JSON form has no
				% leading zero bytes.
				<<"nonce">> =>
					hb_util:encode(
						binary:encode_unsigned(binary:decode_unsigned(Nonce))),
				<<"previous_block">> => hb_util:encode(PrevH),
				<<"timestamp">> => TS,
				<<"last_retarget">> => LastRetarget,
				<<"diff">> => integer_to_binary(Diff),
				<<"height">> => Height,
				<<"hash">> => hb_util:encode(Hash),
				<<"indep_hash">> => hb_util:encode(H),
				<<"txs">> => [hb_util:encode(tx_id(TX)) || TX <- TXs],
				<<"tx_root">> => hb_util:encode(TXRoot),
				<<"wallet_list">> => hb_util:encode(WalletList),
				<<"reward_addr">> => hb_util:encode(Addr),
				<<"tags">> => [hb_util:encode(Tag) || Tag <- Tags],
				<<"reward_pool">> => integer_to_binary(RewardPool),
				<<"weave_size">> => integer_to_binary(WeaveSize),
				<<"block_size">> => integer_to_binary(BlockSize),
				<<"cumulative_diff">> => integer_to_binary(CDiff),
				<<"hash_list_merkle">> => hb_util:encode(HashListMerkle),
				<<"poa">> => poa(PoAOption, TXPath, DataPath, Chunk, UnpackedChunk),
				<<"usd_to_ar_rate">> =>
					[integer_to_binary(RateDividend), integer_to_binary(RateDivisor)],
				<<"scheduled_usd_to_ar_rate">> =>
					[
						integer_to_binary(SchedRateDividend),
						integer_to_binary(SchedRateDivisor)
					],
				<<"packing_2_5_threshold">> => integer_to_binary(Threshold),
				<<"strict_data_split_threshold">> => integer_to_binary(StrictThreshold)
			},
			TXs}
	end;
binary_to_json_struct(_Bin) ->
	{error, 'invalid-block'}.

tx_id(#tx{ id = ID }) -> ID;
tx_id(ID) -> ID.

%% @doc Parse the tags of a binary block. Arweave writes them in reverse order,
%% so collecting them in reverse gives them in their own order.
binary_to_block_tags(<< Count:16, Rest/binary >>) when Count =< 2048 ->
	binary_to_block_tags(Count, Rest, [], 0);
binary_to_block_tags(_Bin) ->
	{error, 'invalid-block-tags'}.
binary_to_block_tags(0, Rest, Tags, _TotalSize) ->
	{ok, Tags, Rest};
binary_to_block_tags(N, << Size:16, Tag:Size/binary, Rest/binary >>, Tags, TotalSize)
		when TotalSize + Size =< 2048 ->
	binary_to_block_tags(N - 1, Rest, [Tag | Tags], TotalSize + Size);
binary_to_block_tags(_N, _Bin, _Tags, _TotalSize) ->
	{error, 'invalid-block-tags'}.

%% @doc Parse the transactions of a binary block, collected in reverse as Arweave
%% writes them in reverse.
binary_to_txs(<< Count:16, Rest/binary >>) when Count =< 1000 ->
	binary_to_txs(Count, Rest, []);
binary_to_txs(_Bin) ->
	{error, 'invalid-block-txs'}.
binary_to_txs(0, Rest, TXs) ->
	{ok, TXs, Rest};
binary_to_txs(N, << Size:24, Bin:Size/binary, Rest/binary >>, TXs) ->
	maybe
		{ok, TX} ?= ar_tx:binary_to_tx(Bin),
		binary_to_txs(N - 1, Rest, [TX | TXs])
	end;
binary_to_txs(_N, _Bin, _TXs) ->
	{error, 'invalid-block-txs'}.

%% @doc Parse the fields that follow the transactions of a 2.9 block, and give
%% the unpacked chunk of its first proof.
binary_to_post_2_6_fields(<< HashPreimageSize:8,
		HashPreimage:HashPreimageSize/binary,
		RecallByteSize:16, RecallByte:(RecallByteSize * 8),
		RewardSize:8, Reward:(RewardSize * 8),
		SigSize:16, Sig:SigSize/binary,
		RecallByte2Size:16, RecallByte2:(RecallByte2Size * 8),
		PrevSolutionHashSize:8, PrevSolutionHash:PrevSolutionHashSize/binary,
		PartitionNumber:256, Output:32/binary,
		GlobalStepNumber:64, Seed:48/binary, NextSeed:48/binary,
		PrevOutputSize:8, PrevOutput:PrevOutputSize/binary,
		PartitionUpperBound:256, NextPartitionUpperBound:256,
		LastCheckpointsLen:16, LastCheckpoints:(LastCheckpointsLen * 32)/binary,
		StepsLen:16, Steps:(StepsLen * 32)/binary,
		Chunk2Size:24, Chunk2:Chunk2Size/binary,
		RewardKeySize:16, RewardKey:RewardKeySize/binary,
		TXPath2Size:24, TXPath2:TXPath2Size/binary,
		DataPath2Size:24, DataPath2:DataPath2Size/binary,
		PriceSize:8, Price:(PriceSize * 8),
		SchedPriceSize:8, SchedPrice:(SchedPriceSize * 8),
		RewardHistoryHash:32/binary,
		DebtSupplySize:8, DebtSupply:(DebtSupplySize * 8),
		KryderPlusRateMultiplier:24, KryderPlusRateMultiplierLatch:8,
		Denomination:24,
		RedenominationHeightSize:8,
		RedenominationHeight:(RedenominationHeightSize * 8),
		PrevCDiffSize:16, PrevCDiff:(PrevCDiffSize * 8),
		Rest/binary >>) ->
	maybe
		{ok, DoubleSigningProof, Rest2} ?= binary_to_double_signing_proof(Rest),
		{ok, Fields, UnpackedChunk, UnpackedChunk2} ?=
			binary_to_post_2_7_fields(Rest2, RecallByte2Size > 0),
		NonceLimiterInfo = maps:get(<<"nonce_limiter_info">>, Fields),
		Fields2 =
			Fields#{
				<<"hash_preimage">> => hb_util:encode(HashPreimage),
				<<"recall_byte">> => integer_to_binary(RecallByte),
				<<"reward">> => integer_to_binary(Reward),
				<<"previous_solution_hash">> => hb_util:encode(PrevSolutionHash),
				<<"partition_number">> => PartitionNumber,
				<<"nonce_limiter_info">> =>
					NonceLimiterInfo#{
						<<"output">> => hb_util:encode(Output),
						<<"global_step_number">> => GlobalStepNumber,
						<<"seed">> => hb_util:encode(Seed),
						<<"next_seed">> => hb_util:encode(NextSeed),
						<<"zone_upper_bound">> => PartitionUpperBound,
						<<"next_zone_upper_bound">> => NextPartitionUpperBound,
						<<"prev_output">> => hb_util:encode(PrevOutput),
						<<"last_step_checkpoints">> => checkpoints(LastCheckpoints),
						<<"checkpoints">> => checkpoints(Steps)
					},
				<<"poa2">> => poa(1, TXPath2, DataPath2, Chunk2, UnpackedChunk2),
				<<"signature">> => hb_util:encode(Sig),
				<<"reward_key">> => hb_util:encode(RewardKey),
				<<"price_per_gib_minute">> => integer_to_binary(Price),
				<<"scheduled_price_per_gib_minute">> => integer_to_binary(SchedPrice),
				<<"reward_history_hash">> => hb_util:encode(RewardHistoryHash),
				<<"debt_supply">> => integer_to_binary(DebtSupply),
				<<"kryder_plus_rate_multiplier">> =>
					integer_to_binary(KryderPlusRateMultiplier),
				<<"kryder_plus_rate_multiplier_latch">> =>
					integer_to_binary(KryderPlusRateMultiplierLatch),
				<<"denomination">> => integer_to_binary(Denomination),
				<<"redenomination_height">> => RedenominationHeight,
				<<"double_signing_proof">> => DoubleSigningProof,
				<<"previous_cumulative_diff">> => integer_to_binary(PrevCDiff)
			},
		% The second recall byte is set when the block has a second proof.
		case RecallByte2Size of
			0 -> {ok, Fields2, UnpackedChunk};
			_ ->
				{ok,
					Fields2#{ <<"recall_byte2">> => integer_to_binary(RecallByte2) },
					UnpackedChunk}
		end
	end;
binary_to_post_2_6_fields(_Bin) ->
	{error, 'invalid-block-fields'}.

%% @doc Parse the double-signing proof of a 2.9 block: a zero byte when it has
%% none.
binary_to_double_signing_proof(<< 0:8, Rest/binary >>) ->
	{ok, #{}, Rest};
binary_to_double_signing_proof(<< 1:8, KeySize:16, Key:KeySize/binary,
		Sig1Size:16, Sig1:Sig1Size/binary,
		CDiff1Size:16, CDiff1:(CDiff1Size * 8),
		PrevCDiff1Size:16, PrevCDiff1:(PrevCDiff1Size * 8),
		Preimage1:64/binary,
		Sig2Size:16, Sig2:Sig2Size/binary,
		CDiff2Size:16, CDiff2:(CDiff2Size * 8),
		PrevCDiff2Size:16, PrevCDiff2:(PrevCDiff2Size * 8),
		Preimage2:64/binary, Rest/binary >>) ->
	{ok,
		#{
			<<"pub_key">> => hb_util:encode(Key),
			<<"sig1">> => hb_util:encode(Sig1),
			<<"cdiff1">> => integer_to_binary(CDiff1),
			<<"prev_cdiff1">> => integer_to_binary(PrevCDiff1),
			<<"preimage1">> => hb_util:encode(Preimage1),
			<<"sig2">> => hb_util:encode(Sig2),
			<<"cdiff2">> => integer_to_binary(CDiff2),
			<<"prev_cdiff2">> => integer_to_binary(PrevCDiff2),
			<<"preimage2">> => hb_util:encode(Preimage2)
		},
		Rest};
binary_to_double_signing_proof(_Bin) ->
	{error, 'invalid-double-signing-proof'}.

%% @doc Parse the fields of the 2.7, 2.8 and 2.9 forks that end a block, and give
%% the unpacked chunks of its two proofs.
binary_to_post_2_7_fields(<< RebaseThresholdSize:16,
		RebaseThreshold:(RebaseThresholdSize * 8),
		ChunkHash:32/binary,
		Chunk2HashSize:8, Chunk2Hash:Chunk2HashSize/binary,
		BlockTimeHistoryHash:32/binary,
		VDFDifficultySize:8, VDFDifficulty:(VDFDifficultySize * 8),
		NextVDFDifficultySize:8, NextVDFDifficulty:(NextVDFDifficultySize * 8),
		PackingDifficulty:8,
		UnpackedChunkHashSize:8, UnpackedChunkHash:UnpackedChunkHashSize/binary,
		UnpackedChunk2HashSize:8, UnpackedChunk2Hash:UnpackedChunk2HashSize/binary,
		UnpackedChunkSize:24, UnpackedChunk:UnpackedChunkSize/binary,
		UnpackedChunk2Size:24, UnpackedChunk2:UnpackedChunk2Size/binary,
		ReplicaFormat:8 >>, HasRecallByte2) ->
	Fields =
		#{
			<<"merkle_rebase_support_threshold">> =>
				integer_to_binary(RebaseThreshold),
			<<"chunk_hash">> => hb_util:encode(ChunkHash),
			<<"block_time_history_hash">> => hb_util:encode(BlockTimeHistoryHash),
			<<"nonce_limiter_info">> =>
				#{
					<<"vdf_difficulty">> => integer_to_binary(VDFDifficulty),
					<<"next_vdf_difficulty">> => integer_to_binary(NextVDFDifficulty)
				},
			<<"packing_difficulty">> => PackingDifficulty,
			<<"replica_format">> => ReplicaFormat
		},
	Fields2 =
		case Chunk2HashSize of
			0 -> Fields;
			_ -> Fields#{ <<"chunk2_hash">> => hb_util:encode(Chunk2Hash) }
		end,
	% A block with a packing difficulty of 1 or more gives the hashes of its
	% unpacked chunks.
	Fields3 =
		case {PackingDifficulty >= 1, HasRecallByte2} of
			{false, _} -> Fields2;
			{true, false} ->
				Fields2#{
					<<"unpacked_chunk_hash">> => hb_util:encode(UnpackedChunkHash)
				};
			{true, true} ->
				Fields2#{
					<<"unpacked_chunk_hash">> => hb_util:encode(UnpackedChunkHash),
					<<"unpacked_chunk2_hash">> => hb_util:encode(UnpackedChunk2Hash)
				}
		end,
	{ok, Fields3, UnpackedChunk, UnpackedChunk2};
binary_to_post_2_7_fields(_Bin, _HasRecallByte2) ->
	{error, 'invalid-block-fields'}.

%% @doc The JSON struct of a proof of access. Its unpacked chunk is present when
%% the block is composite-packed.
poa(Option, TXPath, DataPath, Chunk, UnpackedChunk) ->
	PoA =
		#{
			<<"option">> => integer_to_binary(Option),
			<<"tx_path">> => hb_util:encode(TXPath),
			<<"data_path">> => hb_util:encode(DataPath),
			<<"chunk">> => hb_util:encode(Chunk)
		},
	case UnpackedChunk of
		<<>> -> PoA;
		_ -> PoA#{ <<"unpacked_chunk">> => hb_util:encode(UnpackedChunk) }
	end.

%% @doc Split VDF checkpoints into their encoded 32-byte outputs.
checkpoints(Bin) ->
	[hb_util:encode(Checkpoint) || << Checkpoint:32/binary >> <= Bin].