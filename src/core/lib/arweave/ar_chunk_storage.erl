%% The blob storage optimized for fast reads.
%%
%% VENDOR: upstream is a gen_server owning one storage module's file handles,
%% an ETS file index, prometheus counters and the `ar_sync_record' and
%% `ar_entropy_storage' calls that go with them. What is kept here is the part
%% that decides where a chunk lives and how it is laid out in a file -- the
%% arithmetic and the record format that on-disk compatibility is a property
%% of. The file handles, the write ordering and the sync record are owned by
%% `lib_arweave_chunks', which calls these functions.
%%
%% VENDOR: the two upstream functions that read the node's configuration take
%% the value explicitly instead: `get_chunk_file_start/2' is given the chunk
%% group size, and the path builders are given the data directory.
-module(ar_chunk_storage).

-export([is_storage_supported/3,
		get_storage_module_path/2, get_chunk_storage_path/2, get_filepath/3,
		get_chunk_file_start/2, get_chunk_file_start_by_start_offset/2,
		get_position_and_relative_chunk_offset/2,
		get_position_and_relative_chunk_offset_by_start_offset/2,
		get_chunk_bucket_start/1, get_chunk_bucket_end/1,
		get_chunk_byte_from_bucket_end/1, get_chunk_seek_offset/1,
		get_special_zero_offset/0, is_offset_valid/3,
		extract_end_offset_chunk_pairs/3, sync_record_id/1]).

-include("include/ar.hrl").
-include("include/ar_chunk_storage.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Return true if we can accept the chunk for storage.
%% 256 KiB chunks are stored on disk in chunk_storage optimized for read speed.
%% Unpacked chunks smaller than 256 KiB cannot be stored here currently,
%% because the module does not keep track of the chunk sizes - all chunks
%% are assumed to be 256 KiB.
%%
%% Put another way:
%% 1. Small chunks from before the strict data split threshold are never packed and
%%    never mined, so we store them as unpacked chunks in the rocksdb only.
%% 2. Small chunks after the strict data split threshold are:
%%    - stored in the rocksdb when they are unpacked
%%    - stored in chunk_storage as normal when they are packed
-spec is_storage_supported(
		Offset :: non_neg_integer(),
		ChunkSize :: non_neg_integer(),
		Packing :: term()
) -> true | false.
is_storage_supported(Offset, ChunkSize, Packing) ->
	case Offset > ar_block:strict_data_split_threshold() of
		true ->
			%% All chunks above ar_block:strict_data_split_threshold() are placed in 256 KiB
			%% buckets so technically can be stored in ar_chunk_storage. However, to avoid
			%% managing padding in ar_chunk_storage for unpacked chunks smaller than 256 KiB
			%% (we do not need fast random access to unpacked chunks after
			%% ar_block:strict_data_split_threshold() anyways), we put them to RocksDB.
			Packing /= unpacked orelse ChunkSize == (?DATA_CHUNK_SIZE);
		false ->
			ChunkSize == (?DATA_CHUNK_SIZE)
	end.

get_storage_module_path(DataDir, ?DEFAULT_MODULE) ->
	DataDir;
get_storage_module_path(DataDir, StoreID) ->
	filename:join([DataDir, "storage_modules", StoreID]).

get_chunk_storage_path(DataDir, StoreID) ->
	filename:join([get_storage_module_path(DataDir, StoreID), ?CHUNK_DIR]).

%% @doc Return the path of a file inside a storage module's chunk storage
%% directory.
get_filepath(DataDir, Name, StoreID) ->
	ChunkDir = get_chunk_storage_path(DataDir, StoreID),
	filename:join([ChunkDir, Name]).

get_chunk_file_start(EndOffset, ChunkGroupSize) ->
	StartOffset = EndOffset - ?DATA_CHUNK_SIZE,
	get_chunk_file_start_by_start_offset(StartOffset, ChunkGroupSize).

get_chunk_file_start_by_start_offset(StartOffset, ChunkGroupSize) ->
	hb_util:floor_int(StartOffset, ChunkGroupSize).

get_special_zero_offset() ->
	?DATA_CHUNK_SIZE.

get_position_and_relative_chunk_offset(ChunkFileStart, Offset) ->
	BucketPickOffset = Offset - ?DATA_CHUNK_SIZE,
	get_position_and_relative_chunk_offset_by_start_offset(ChunkFileStart, BucketPickOffset).

get_position_and_relative_chunk_offset_by_start_offset(ChunkFileStart, BucketPickOffset) ->
	BucketStart = hb_util:floor_int(BucketPickOffset, ?DATA_CHUNK_SIZE),
	ChunkOffset = case BucketPickOffset - BucketStart of
		0 ->
			%% Represent 0 as the largest possible offset plus one,
			%% to distinguish zero offset from not yet written data.
			get_special_zero_offset();
		Offset ->
			Offset
	end,
	RelativeOffset = BucketStart - ChunkFileStart,
	Position = RelativeOffset + ?OFFSET_SIZE * (RelativeOffset div ?DATA_CHUNK_SIZE),
	{Position, ChunkOffset}.

-spec get_chunk_bucket_start(Offset :: non_neg_integer()) -> non_neg_integer().
get_chunk_bucket_start(Offset) ->
	PaddedEndOffset = ar_block:get_chunk_padded_offset(Offset),
	hb_util:floor_int(max(0, PaddedEndOffset - ?DATA_CHUNK_SIZE), ?DATA_CHUNK_SIZE).

-spec get_chunk_bucket_end(Offset :: non_neg_integer()) -> non_neg_integer().
get_chunk_bucket_end(Offset) ->
	get_chunk_bucket_start(Offset) + ?DATA_CHUNK_SIZE.

%% @doc Return the byte (>= ChunkStartOffset, < ChunkEndOffset)
%% that necessarily belongs to the chunk stored  in the bucket with the given bucket end
%% offset. For buckets above the strict data split threshold, the byte is the first byte
%% of the chunk that is mapped to the bucket. For buckets below the strict data split
%% threshold, the byte is just guaranteed to belong to the chunk but is not necessarily the
%% chunk's first byte.
-spec get_chunk_byte_from_bucket_end(non_neg_integer()) -> non_neg_integer().
get_chunk_byte_from_bucket_end(BucketEndOffset) ->
	%% sanity checks
	BucketEndOffset = get_chunk_bucket_end(BucketEndOffset),
	%% end sanity checks

	get_chunk_seek_offset(BucketEndOffset) - 1.

%% @doc Returns a byte that is guaranteed to be in the unpadded portion of the chunk
%% identified by Offset. Offset can be any byte within the chunk - in either the unpadded
%% part or the pad. This typically equates to the first byte of the chunk plus one.
%%
%% If Offset is before the ar_block:strict_data_split_threshold() we just return it because we don't
%% have any information about where chunks start or end.
-spec get_chunk_seek_offset(non_neg_integer()) -> non_neg_integer().
get_chunk_seek_offset(Offset) ->
	case Offset > ar_block:strict_data_split_threshold() of
		true ->
			ar_poa:get_padded_offset(Offset, ar_block:strict_data_split_threshold())
					- (?DATA_CHUNK_SIZE)
					+ 1;
		false ->
			Offset
	end.

is_offset_valid(_Byte, _BucketStart, 0) ->
	%% 0 is interpreted as "data has not been written yet".
	false;
is_offset_valid(Byte, BucketStart, ChunkOffset) ->
	Delta = Byte - (BucketStart + ChunkOffset rem ?DATA_CHUNK_SIZE),
	Delta >= 0 andalso Delta < ?DATA_CHUNK_SIZE.

extract_end_offset_chunk_pairs(
		<< 0:?OFFSET_BIT_SIZE, _ZeroChunk:?DATA_CHUNK_SIZE/binary, Rest/binary >>,
		BucketStart,
		Shift
 ) ->
	extract_end_offset_chunk_pairs(Rest, BucketStart, Shift + 1);
extract_end_offset_chunk_pairs(
		<< ChunkOffset:?OFFSET_BIT_SIZE, Chunk:?DATA_CHUNK_SIZE/binary, Rest/binary >>,
		BucketStart,
		Shift
 ) ->
	ChunkOffsetLimit = ?DATA_CHUNK_SIZE,
	EndOffset =
		BucketStart
		+ (ChunkOffset rem ChunkOffsetLimit)
		+ (?DATA_CHUNK_SIZE * Shift),
	[{EndOffset, Chunk}
			| extract_end_offset_chunk_pairs(Rest, BucketStart, Shift + 1)];
extract_end_offset_chunk_pairs(<<>>, _BucketStart, _Shift) ->
	[];
extract_end_offset_chunk_pairs(<< ChunkOffset:?OFFSET_BIT_SIZE, Chunk/binary >>,
		BucketStart, Shift) ->
	?LOG_ERROR([{event, unexpected_chunk_data}, {chunk_offset, ChunkOffset},
			{bucket_start, BucketStart}, {shift, Shift}, {chunk_size, byte_size(Chunk)}]),
	[].

sync_record_id(unpacked_padded) ->
	%% Entropy indexing changed between 2.9.0 and 2.9.1. So we'll use a new
	%% sync_record id (ar_chunk_storage_replica_2_9_1_unpacked) going forward.
	%% The old id (ar_chunk_storage_replica_2_9_unpacked) should not be used.
	ar_chunk_storage_replica_2_9_1_unpacked;
sync_record_id(_Packing) ->
	ar_chunk_storage.
