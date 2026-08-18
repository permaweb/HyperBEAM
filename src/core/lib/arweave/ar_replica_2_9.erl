-module(ar_replica_2_9).

%% VENDOR: upstream also exports get_entropy_partition_range/1 and
%% get_next_fetch_offset/3. Both are chunk-storage syncing helpers that reach
%% into ar_chunk_storage, a gen_server-backed storage module that is not
%% vendored (chunk storage is an explicit non-goal). Neither is reachable from
%% proof validation, so both are dropped rather than left to fail at runtime.
-export([get_entropy_partition/1, get_entropy_key/3,
    get_slice_index/1, get_partition_offset/1, get_entropy_index/2]).

-include("include/ar.hrl").
-include("include/ar_consensus.hrl").
-include_lib("eunit/include/eunit.hrl").

-moduledoc """
    This module handles mapping the 2.9 replica entropy to chunks and sub-chunks.

    Here's a break down of how entropy is mapped to sub-chunks.

    1. Iterate through each chunk's (e.g. chunk0) sub-chunks (e.g. s0, s1) assigning each one
       to a different entropy. This ensures that all contiguous sub-chunks are assigned to
       different entropies, maximizing the amount of work that an on-demand miner needs to do
       to pack and mine a contiguous recall range.

                   chunk0                          chunk1
                   +-----------------------------+ +-----------------------------+
                   |  s0 |  s1 |  s2 | ... | s31 | |  s0 |  s1 |  s2 | ... | s31 |
                   +-----------------------------+ +-----------------------------+
                      v     v     v           v       v      v     v          v
    entropy index:   e0    e1    e2          e31     e32    e33   e33        e63

    2. Each 8 MiB entropy contains 1024 8 KiB slices. To finish packing the sub-chunks we
       will encipher them with the appropriate slice. A sub-chunk's slice index is
       determined by its *chunk* - each sub-chunk in a chunk is assigned to a different
       *entropy* but has the same *slice index*. A slice index and sector index are the same
       but are just used in difference contexts (e.g. slices divide up entropy, sectors
       divide up the partition). A chunk in sector 0 of the partition is enciphered with
       slice index 0 from its entropies.

         sector0   sector1  sector2           sector1023        sector0  sector1
         chunk0    c12413   c26825            cXXXXXX           chunk1   c12414
         +-------++-------++-------+         +-------+          +-------++-------+
         | | | | || | | | || | | | |   ...   | | | | |          | | | | || | | | |
         +-------++-------++-------+         +-------+          +-------++-------+
             |        |        |                 |                  |        |
         +-----------------------------------------------+      +--------------------------+
     e0: | slice0 | slice1 | slice2 | ...... | slice1023 | e32: | slice0 | slice1 | ......
         +-----------------------------------------------+      +--------------------------+
             |        |        |                 |                  |        |
         +-----------------------------------------------+      +--------------------------+
     e1: | slice0 | slice1 | slice2 | ...... | slice1023 | e33: | slice0 | slice1 | ......
         +-----------------------------------------------+      +--------------------------+
             |        |        |                 |                  |        |
         +-----------------------------------------------+      +--------------------------+
     e2: | slice0 | slice1 | slice2 | ...... | slice1023 | e34: | slice0 | slice1 | ......
         +-----------------------------------------------+      +--------------------------+
     ...     |        |        |                 |                  |        |
         +-----------------------------------------------+      +--------------------------+
    e31: | slice0 | slice1 | slice2 | ...... | slice1023 | e63: | slice0 | slice1 | ......
         +-----------------------------------------------+      +--------------------------+
             |        |        |                 |                  |        |
             v        v        v                 v                  v        v

    Glossary:

    entropy: An 8 MiB (?REPLICA_2_9_ENTROPY_SIZE) block of entropy that contains the entropy
             for 1024 sub-chunks (?REPLICA_2_9_ENTROPY_SIZE div
             ?COMPOSITE_PACKING_SUB_CHUNK_SIZE.

    slice: The 8192 byte (?COMPOSITE_PACKING_SUB_CHUNK_SIZE) range of an 'entropy' that will
           be enciphered with a sub-chunk when packing to the replica_2_9 format.

    entropy partition: contains all the entropies needed to encipher all the chunks in a
                       recall partition. A recall partition is 3.6 TB (ar_block:partition_size()),
                       but an entropy partition is slightly larger since enciphering a chunk
                       (256 KiB) requires slices from 32 different entropies (256 MiB).
                       Some of the entropies in a partition can be reused by neighboring
                       recall partitions.

    entropy index: The index of an entropy within an entropy partition. All of a chunk's
                   sub-chunks have a different entropy index.

    slice index: the index of a slice within an entropy. All of a chunk's sub-chunks have
                 the same slice index.

    sector: Each slice of an entropy is distributed to a different sector such that consecutive
            slices map to chunks that are as far as possible from each other within a
            partition. With an entropy size of 8_388_608 bytes and a slice size of 8_192 bytes,
            there are 1024 slices per entropy, which yields 1024 sectors per partition.
""".

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Return the 2.9 partition number the chunk with the given absolute end offset is
%% mapped to. This partition number is a part of the 2.9 replication key. It is NOT
%% the same as the ar_block:partition_size() (3.6 TB) recall partition.
-spec get_entropy_partition(
		AbsoluteChunkEndOffset :: non_neg_integer()
) -> non_neg_integer().
get_entropy_partition(AbsoluteChunkEndOffset) ->
    BucketStart = get_entropy_bucket_start(AbsoluteChunkEndOffset),
    %% VENDOR: upstream calls ar_node:get_partition_number(BucketStart). ar_node
    %% is the node gen_server and is not vendored; its integer clause is just
    %% `Offset div ar_block:partition_size()' and BucketStart is always an
    %% integer here, so the body is inlined.
    BucketStart div ar_block:partition_size().

%% VENDOR: upstream's get_next_fetch_offset/3 and get_entropy_partition_range/1
%% sat here. They walk a chunk-storage cursor and reverse the partition mapping
%% via ar_poa:get_padded_offset/2 and
%% ar_chunk_storage:get_chunk_byte_from_bucket_end/1. ar_chunk_storage is not
%% vendored, so both functions are dropped - see the note above the -export.

%% @doc Return the key used to generate the entropy for the 2.9 replication format.
%% RewardAddr: The address of the miner that mined the chunk.
%% AbsoluteEndOffset: The absolute end offset of the chunk.
%% SubChunkStartOffset: The start offset of the sub-chunk within the chunk. 0 is the first
%% sub-chunk of the chunk, (?DATA_CHUNK_SIZE - ?COMPOSITE_PACKING_SUB_CHUNK_SIZE) is the
%% last sub-chunk of the chunk.
-spec get_entropy_key(
		RewardAddr :: binary(),
		AbsoluteEndOffset :: non_neg_integer(),
		SubChunkStartOffset :: non_neg_integer()
) -> binary().
get_entropy_key(RewardAddr, AbsoluteEndOffset, SubChunkStartOffset) ->
	Partition = get_entropy_partition(AbsoluteEndOffset),
	%% We use the key to generate a large entropy shared by many chunks.
	EntropyIndex = get_entropy_index(AbsoluteEndOffset, SubChunkStartOffset),
	crypto:hash(sha256, << Partition:256, EntropyIndex:256, RewardAddr/binary >>).

%% @doc Return the 0-based index indicating which area within a 2.9 entropy the
%% given sub-chunk is mapped to (aka slice index). Sub-chunks of the same chunk are mapped to
%% different entropies but all use the same slice index.
-spec get_slice_index(
		AbsoluteChunkEndOffset :: non_neg_integer()
) -> non_neg_integer().
get_slice_index(AbsoluteChunkEndOffset) ->
    PartitionRelativeOffset = get_partition_offset(AbsoluteChunkEndOffset),
	SectorSize = ar_block:get_replica_2_9_entropy_sector_size(),
	(PartitionRelativeOffset div SectorSize) rem ar_block:get_sub_chunks_per_replica_2_9_entropy().

%%%===================================================================
%%% Private functions.
%%%===================================================================

%% @doc Return the start offset of the bucket containing the given chunk offset.
%% A chunk bucket is a 0-based, 256-KiB wide, 256-KiB aligned range. A chunk belongs to
%% the bucket that contains the first byte of the chunk.
-spec get_entropy_bucket_start(non_neg_integer()) -> non_neg_integer().
get_entropy_bucket_start(AbsoluteChunkEndOffset) ->
	PaddedEndOffset = ar_block:get_chunk_padded_offset(AbsoluteChunkEndOffset),
	PickOffset = max(0, PaddedEndOffset - ?DATA_CHUNK_SIZE),
	BucketStart = hb_util:floor_int(PickOffset, ?DATA_CHUNK_SIZE),

    %% VENDOR: upstream asserts here that BucketStart equals
    %% ar_chunk_storage:get_chunk_bucket_start(PaddedEndOffset). That function is
    %% a byte-for-byte restatement of the three lines above it, and
    %% ar_chunk_storage is not vendored, so the redundant sanity check is
    %% dropped.

	BucketStart.

%% @doc Return the offset of the chunk within its partition.
-spec get_partition_offset(AbsoluteChunkEndOffset :: non_neg_integer()) -> non_neg_integer().
get_partition_offset(AbsoluteChunkEndOffset) ->
    BucketStart = get_entropy_bucket_start(AbsoluteChunkEndOffset),
    Partition = get_entropy_partition(AbsoluteChunkEndOffset),
    PartitionStart = Partition * ar_block:partition_size(),
    BucketStart - PartitionStart.

%% @doc Returns the index of the entropy containing the slice for specified chunk's sub-chunk.
%% An entropy index is 0-based index used to identify a specific entropy within an entropy
%% partition. It is not unique - the same index will refer to different entropies in different
%% partitions and for different mining addresses. For a unique entropy identifier see
%% get_entropy_key/3.
%%
%% The entropy index is for the 2.9 replication format.
-spec get_entropy_index(
    AbsoluteChunkEndOffset :: non_neg_integer(),
    SubChunkStartOffset :: non_neg_integer()
) -> non_neg_integer().
get_entropy_index(AbsoluteChunkEndOffset, SubChunkStartOffset) ->
    %% Assert that SubChunkStartOffset is less than ?DATA_CHUNK_SIZE
    true = SubChunkStartOffset < ?DATA_CHUNK_SIZE,
    PartitionRelativeOffset = get_partition_offset(AbsoluteChunkEndOffset),
    SectorSize = ar_block:get_replica_2_9_entropy_sector_size(),
    %% Index of this chunk into the sector (i.e. how many chunks into the sector it falls)
    ChunkBucket = (PartitionRelativeOffset rem SectorSize) div ?DATA_CHUNK_SIZE,
    %% Index of this sub-chunk into the chunk (i.e. how many sub-chunks into the chunk it
    %% falls)
    SubChunkBucket = SubChunkStartOffset div ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
    ChunkBucket * ?COMPOSITE_PACKING_SUB_CHUNK_COUNT + SubChunkBucket.

%%%===================================================================
%%% Tests.
%%%===================================================================

%% VENDOR: every upstream test in this module runs under
%% ar_test_node:test_with_mocked_functions/3, which meck-mocks ar_block's
%% partition and sector sizes so that the walk tests fit in a few megabytes.
%% Neither ar_test_node nor its meck harness is vendored into HyperBEAM, so the
%% whole test section is dropped. The entropy mapping is instead exercised
%% against real mainnet replica_2_9 proofs.
