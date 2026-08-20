%%% VENDOR: upstream is a gen_server per storage module that walks a cursor
%%% across a partition, asks `ar_packing_server' for entropy, and hands each
%%% footprint to `ar_entropy_storage'. The cursor, the packing state and the
%%% device lock are HyperBEAM's (`lib_arweave_entropy'), which asks
%%% `~arweave-spora@2.9/entropy' for each of the thirty-two entropies and calls
%%% the functions here to place them. What is kept is the mapping from an
%%% offset to the offsets one footprint covers, and the slicing that turns
%%% thirty-two entropies into one chunk's worth.
-module(ar_entropy_gen).

-export([entropy_offsets/2, reset_entropy_offset/1, shift_entropy_offset/2,
		generate_entropy_keys/2, take_and_combine_entropy_slices/1,
		map_entropies/8]).

-include("include/ar.hrl").
-include("include/ar_consensus.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Return a list of all BucketEndOffsets covered by the entropy needed to encipher
%% the chunk at the given offset. The list returned may include offsets that occur before
%% the provided offset. This is expected if Offset does not refer to a sector 0 chunk.
-spec entropy_offsets(non_neg_integer(), non_neg_integer()) -> [non_neg_integer()].
entropy_offsets(Offset, ModuleEnd) ->
	BucketEndOffset = ar_chunk_storage:get_chunk_bucket_end(Offset),
	BucketEndOffset2 = reset_entropy_offset(BucketEndOffset),
	Partition = ar_replica_2_9:get_entropy_partition(BucketEndOffset),
	{_, EntropyPartitionEnd} = ar_replica_2_9:get_entropy_partition_range(Partition),
	End = min(EntropyPartitionEnd, ModuleEnd),
	entropy_offsets2(BucketEndOffset2, End).

entropy_offsets2(BucketEndOffset, PaddedPartitionEnd)
	when BucketEndOffset > PaddedPartitionEnd ->
	[];
entropy_offsets2(BucketEndOffset, PaddedPartitionEnd) ->
	NextOffset = shift_entropy_offset(BucketEndOffset, 1),
	[BucketEndOffset | entropy_offsets2(NextOffset, PaddedPartitionEnd)].

%% @doc If we are not at the beginning of the entropy, shift the offset to
%% the left. store_entropy_footprint will traverse the entire 2.9 partition shifting
%% the offset by sector size.
reset_entropy_offset(BucketEndOffset) ->
	%% Sanity checks
	BucketEndOffset = ar_chunk_storage:get_chunk_bucket_end(BucketEndOffset),
	%% End sanity checks
	SliceIndex = ar_replica_2_9:get_slice_index(BucketEndOffset),
	shift_entropy_offset(BucketEndOffset, -SliceIndex).

shift_entropy_offset(Offset, SectorCount) ->
	SectorSize = ar_block:get_replica_2_9_entropy_sector_size(),
	ar_chunk_storage:get_chunk_bucket_end(Offset + SectorSize * SectorCount).

map_entropies(_Entropies,
			[],
			_RangeStart,
			_Keys,
			_RewardAddr,
			_Fun,
			_Args,
			Acc) ->
	%% The amount of entropy generated per partition is slightly more than the amount needed.
	%% So at the end of a partition we will have finished processing chunks, but still have
	%% some entropy left. In this case we stop the recursion early and wait for the writes
	%% to complete.
	Acc;
map_entropies(Entropies,
			[BucketEndOffset | EntropyOffsets],
			RangeStart,
			Keys,
			RewardAddr,
			Fun,
			Args,
			Acc) ->

	case take_and_combine_entropy_slices(Entropies) of
		{ChunkEntropy, Rest} ->
			%% Sanity checks
			sanity_check_replica_2_9_entropy_keys(BucketEndOffset, RewardAddr, Keys),
			%% End sanity checks

			Acc2 = case BucketEndOffset > RangeStart of
				true ->
					erlang:apply(Fun,
						[ChunkEntropy, BucketEndOffset, RewardAddr] ++ Args ++ [Acc]);
				false ->
					%% Don't write entropy before the start of the range.
					Acc
			end,

			%% Jump to the next sector covered by this entropy.
			map_entropies(
				Rest,
				EntropyOffsets,
				RangeStart,
				Keys,
				RewardAddr,
				Fun,
				Args,
				Acc2)
	end.

generate_entropy_keys(RewardAddr, Offset) ->
	generate_entropy_keys(RewardAddr, Offset, 0).

generate_entropy_keys(_RewardAddr, _Offset, SubChunkStart)
	when SubChunkStart == ?DATA_CHUNK_SIZE ->
	[];
generate_entropy_keys(RewardAddr, Offset, SubChunkStart) ->
	SubChunkSize = ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
	[ar_replica_2_9:get_entropy_key(RewardAddr, Offset, SubChunkStart)
	 | generate_entropy_keys(RewardAddr, Offset, SubChunkStart + SubChunkSize)].

%% @doc Take the first slice of each entropy and combine into a single binary. This binary
%% can be used to encipher a single chunk.
-spec take_and_combine_entropy_slices(Entropies :: [binary()]) ->
										 {ChunkEntropy :: binary(),
										  RemainingSlicesOfEachEntropy :: [binary()]}.
take_and_combine_entropy_slices(Entropies) ->
	true = ?COMPOSITE_PACKING_SUB_CHUNK_COUNT == length(Entropies),
	take_and_combine_entropy_slices(Entropies, [], []).

take_and_combine_entropy_slices([], Acc, RestAcc) ->
	{iolist_to_binary(Acc), lists:reverse(RestAcc)};
take_and_combine_entropy_slices([<<>> | Entropies], _Acc, _RestAcc) ->
	true = lists:all(fun(Entropy) -> Entropy == <<>> end, Entropies),
	{<<>>, []};
take_and_combine_entropy_slices([<<EntropySlice:?COMPOSITE_PACKING_SUB_CHUNK_SIZE/binary,
								   Rest/binary>>
								 | Entropies],
								Acc,
								RestAcc) ->
	take_and_combine_entropy_slices(Entropies, [Acc, EntropySlice], [Rest | RestAcc]).

sanity_check_replica_2_9_entropy_keys(PaddedEndOffset, RewardAddr, Keys) ->
	sanity_check_replica_2_9_entropy_keys(PaddedEndOffset, RewardAddr, 0, Keys).

sanity_check_replica_2_9_entropy_keys(
		_PaddedEndOffset, _RewardAddr, _SubChunkStartOffset, []) ->
	ok;
sanity_check_replica_2_9_entropy_keys(
		PaddedEndOffset, RewardAddr, SubChunkStartOffset, [Key | Keys]) ->
		Key = ar_replica_2_9:get_entropy_key(RewardAddr, PaddedEndOffset, SubChunkStartOffset),
	SubChunkSize = ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
	sanity_check_replica_2_9_entropy_keys(PaddedEndOffset,
										RewardAddr,
										SubChunkStartOffset + SubChunkSize,
										Keys).
