%%% @doc Copied and adapted from the arweave codebase.
%%% Should track:
%%% https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_packing_server.erl
%%%
%%% VENDOR: upstream is a gen_server owning a pool of packing workers, an ets table
%%% holding the RandomX packing state and the back-pressure counters, an entropy cache
%%% and thirteen prometheus metrics. None of that is vendored. What remains is the pure
%%% unpacking core plus an explicit-state constructor: `init_packing_state/1,2' builds
%%% the RandomX states and returns them, and every function that needs them takes the
%%% state as an argument. There is no process, no ets and no persistent_term here.
-module(ar_packing_server).

-export([packing_atom/1, init_packing_state/1, init_packing_state/2,
		get_randomx_state_for_h0/2, get_randomx_state_by_packing/2,
		unpack/6, unpack_sub_chunk/6, chunk_key/3,
		generate_replica_2_9_entropy/4, do_generate_entropy/2,
		unpad_chunk/3, unpad_chunk/4]).

-include("include/ar.hrl").
-include("include/ar_consensus.hrl").
%% VENDOR: dropped -include_lib("arweave_config/include/arweave_config.hrl").
-include_lib("kernel/include/logger.hrl").

%% VENDOR: the three RandomX variants a packing state may carry, in the order they
%% occupy the state tuple. rx512 unpacks spora_2_6 and computes H0 at packing
%% difficulty 0; rx4096 computes H0 at every post-2.9 packing difficulty and unpacks
%% composite; rxsquared generates the replica_2_9 entropy.
-define(RANDOMX_VARIANTS, [rx512, rx4096, rxsquared]).

%%%===================================================================
%%% Public interface.
%%%===================================================================

packing_atom(Packing) when is_atom(Packing) ->
	Packing;
packing_atom({spora_2_6, _Addr}) ->
	spora_2_6;
packing_atom({composite, _Addr, _Diff}) ->
	composite;
packing_atom({replica_2_9, _Addr}) ->
	replica_2_9.

%% @doc Build the RandomX packing state. Mode is `fast' or `light'.
%%
%% VENDOR: upstream's init_packing_state/0 always builds all three states in fast mode
%% and inserts the result into ets, where get_packing_state/0 later reads it. We return
%% the state instead, and let the caller decide which variants to pay for: any variant
%% not listed is left `not_initialised', which the ar_mine_randomx dispatchers reject
%% with {error, invalid_randomx_mode} rather than handing a bad resource to a NIF.
%% ?RANDOMX_PACKING_KEY is a fixed protocol constant, so a state built once stays valid
%% for the life of the network.
init_packing_state(Mode) ->
	init_packing_state(Mode, ?RANDOMX_VARIANTS).

init_packing_state(Mode, Variants) ->
	{
		init_randomx_state(rx512, Mode, Variants),
		init_randomx_state(rx4096, Mode, Variants),
		init_randomx_state(rxsquared, Mode, Variants)
	}.

get_randomx_state_for_h0(PackingDifficulty, PackingState) ->
	{RandomXState512, RandomXState4096, _} = PackingState,
	case PackingDifficulty of
		0 ->
			RandomXState512;
		_ ->
			RandomXState4096
	end.

get_randomx_state_by_packing({composite, _, _}, {_, RandomXState, _}) ->
	RandomXState;
get_randomx_state_by_packing({replica_2_9, _}, {_, _, RandomXState}) ->
	RandomXState;
get_randomx_state_by_packing({spora_2_6, _}, {RandomXState, _, _}) ->
	RandomXState;
get_randomx_state_by_packing(spora_2_5, {RandomXState, _, _}) ->
	RandomXState.

%% @doc Unpack the chunk packed for mining.
%%
%% Return {ok, UnpackedChunk} or {error, invalid_packed_size} or {error, invalid_chunk_size}
%% or {error, invalid_padding}.
%%
%% VENDOR: this is upstream's internal unpack/7 with the prometheus wrapper and the
%% `External' label removed, the RandomX state taken as an argument instead of read from
%% ets, and the {ok, Chunk, WasAlreadyUnpacked} triples collapsed to upstream's public
%% unpack/5 contract - the `already_unpacked' tag only ever fed the repack path, which
%% is not vendored.
unpack({replica_2_9, RewardAddr} = Packing, AbsoluteEndOffset,
		_TXRoot, Chunk, ChunkSize, PackingState) ->
	case validate_chunk_size(Packing, Chunk, ChunkSize) of
		{error, Reason} ->
			?LOG_ERROR([{event, unpack_chunk_size_error}, {error, Reason},
					{chunk_offset, AbsoluteEndOffset},
					{packing, packing_atom(Packing)},
					{expected_chunk_size, ChunkSize},
					{actual_chunk_size, byte_size(Chunk)}]),
			{error, Reason};
		{ok, PackedSize} ->
			SubChunks = get_sub_chunks(Chunk),
			RandomXState = get_randomx_state_by_packing(Packing, PackingState),
			case unpack_replica_2_9_sub_chunks(RewardAddr, AbsoluteEndOffset,
					RandomXState, SubChunks) of
				{ok, Unpacked} ->
					case unpad_chunk(Packing, Unpacked, ChunkSize, PackedSize) of
						error ->
							?LOG_WARNING([{event, unpad_chunk_error},
									{packed_size, PackedSize},
									{chunk_size, ChunkSize},
									{absolute_end_offset, AbsoluteEndOffset}]),
							{error, invalid_padding};
						UnpackedChunk ->
							{ok, UnpackedChunk}
					end;
				Error ->
					?LOG_ERROR([{event, unpack_replica_2_9_sub_chunks_error}, {error, Error}]),
					Error
			end
	end;
unpack(unpacked, _ChunkOffset, _TXRoot, Chunk, _ChunkSize, _PackingState) ->
	%% Allows to reuse the same interface for unpacking and repacking.
	{ok, Chunk};
unpack(unpacked_padded, _ChunkOffset, _TXRoot, Chunk, ChunkSize, _PackingState) ->
	{ok, binary:part(Chunk, 0, ChunkSize)};
unpack(Packing, ChunkOffset, TXRoot, Chunk, ChunkSize, PackingState) ->
	case validate_chunk_size(Packing, Chunk, ChunkSize) of
		{error, Reason} ->
			?LOG_ERROR([{event, unpack_chunk_size_error}, {error, Reason},
					{chunk_offset, ChunkOffset},
					{packing, packing_atom(Packing)},
					{expected_chunk_size, ChunkSize},
					{actual_chunk_size, byte_size(Chunk)}]),
			{error, Reason};
		{ok, _PackedSize} ->
			{_PackingAtom, Key} = chunk_key(Packing, ChunkOffset, TXRoot),
			RandomXState = get_randomx_state_by_packing(Packing, PackingState),
			ar_mine_randomx:randomx_decrypt_chunk(Packing, RandomXState, Key, Chunk, ChunkSize)
	end.

%% @doc Unpack the packed sub-chunk of a composite packing or shared entropy replica.
%%
%% Return {ok, UnpackedSubChunk} or {error, invalid_packed_size}.
%%
%% VENDOR: upstream's unpack_sub_chunk/5 with the RandomX state taken as an argument
%% and the prometheus histograms removed.
unpack_sub_chunk({composite, _, _} = Packing,
		AbsoluteEndOffset, TXRoot, Chunk, SubChunkStartOffset, PackingState) ->
	case byte_size(Chunk) == ?COMPOSITE_PACKING_SUB_CHUNK_SIZE of
		false ->
			{error, invalid_packed_size};
		true ->
			{_PackingAtom, Key} = chunk_key(Packing, AbsoluteEndOffset, TXRoot),
			RandomXState = get_randomx_state_by_packing(Packing, PackingState),
			ar_mine_randomx:randomx_decrypt_sub_chunk(Packing, RandomXState, Key, Chunk,
					SubChunkStartOffset)
	end;
unpack_sub_chunk({replica_2_9, RewardAddr} = Packing,
		AbsoluteEndOffset, _TXRoot, Chunk, SubChunkStartOffset, PackingState) ->
	case byte_size(Chunk) == ?COMPOSITE_PACKING_SUB_CHUNK_SIZE of
		false ->
			{error, invalid_packed_size};
		true ->
			RandomXState = get_randomx_state_by_packing(Packing, PackingState),
			Entropy = generate_replica_2_9_entropy(
				RewardAddr, AbsoluteEndOffset, SubChunkStartOffset, RandomXState),
			EntropySubChunkIndex = ar_replica_2_9:get_slice_index(AbsoluteEndOffset),
			ar_mine_randomx:randomx_decrypt_replica_2_9_sub_chunk({RandomXState,
					Entropy, Chunk, EntropySubChunkIndex})
	end.

%% @doc Generate the 2.9 entropy.
%%
%% VENDOR: upstream's arity-4 clause pair took a CacheEntropy boolean and, when true,
%% took a per-key lock, consulted ar_entropy_cache and recorded cache hit/miss counters.
%% Every recall byte we validate has a distinct key, so the cache can never hit; the
%% argument is now the rxsquared RandomX state, which upstream re-derived from the ets
%% packing state inside do_generate_entropy/2.
-spec generate_replica_2_9_entropy(
		RewardAddr :: binary(),
		BucketEndOffset :: non_neg_integer(),
		SubChunkStartOffset :: non_neg_integer(),
		RandomXState :: term()
) -> binary().
generate_replica_2_9_entropy(RewardAddr, BucketEndOffset, SubChunkStartOffset, RandomXState) ->
	Key = ar_replica_2_9:get_entropy_key(RewardAddr, BucketEndOffset, SubChunkStartOffset),
	do_generate_entropy(RandomXState, Key).

do_generate_entropy(RandomXState, Key) ->
	Entropy = ar_mine_randomx:randomx_generate_replica_2_9_entropy(RandomXState, Key),
	%% Primarily needed for testing where the entropy generated exceeds the entropy
	%% needed for tests.
	binary_part(Entropy, 0, ?REPLICA_2_9_ENTROPY_SIZE).

unpad_chunk(spora_2_5, Unpacked, ChunkSize, _PackedSize) ->
	binary:part(Unpacked, 0, ChunkSize);
unpad_chunk({spora_2_6, _Addr}, Unpacked, ChunkSize, PackedSize) ->
	unpad_chunk(Unpacked, ChunkSize, PackedSize);
unpad_chunk({composite, _Addr, _PackingDifficulty}, Unpacked, ChunkSize, PackedSize) ->
	unpad_chunk(Unpacked, ChunkSize, PackedSize);
unpad_chunk({replica_2_9, _Addr}, Unpacked, ChunkSize, PackedSize) ->
	unpad_chunk(Unpacked, ChunkSize, PackedSize);
unpad_chunk(unpacked, Unpacked, ChunkSize, _PackedSize) ->
	binary:part(Unpacked, 0, ChunkSize).

unpad_chunk(Unpacked, ChunkSize, PackedSize) ->
	Padding = binary:part(Unpacked, ChunkSize, PackedSize - ChunkSize),
	case Padding of
		<<>> ->
			Unpacked;
		_ ->
			case is_zero(Padding) of
				false ->
					error;
				true ->
					binary:part(Unpacked, 0, ChunkSize)
			end
	end.

is_zero(<< 0:8, Rest/binary >>) ->
	is_zero(Rest);
is_zero(<<>>) ->
	true;
is_zero(_Rest) ->
	false.

%%%===================================================================
%%% Private functions.
%%%===================================================================

init_randomx_state(RxMode, Mode, Variants) ->
	case lists:member(RxMode, Variants) of
		false ->
			not_initialised;
		true ->
			case Mode of
				fast ->
					ar_mine_randomx:init_fast(RxMode, ?RANDOMX_PACKING_KEY,
							erlang:system_info(dirty_cpu_schedulers_online));
				light ->
					ar_mine_randomx:init_light(RxMode, ?RANDOMX_PACKING_KEY)
			end
	end.

chunk_key(spora_2_5, ChunkOffset, TXRoot) ->
	%% The presence of the absolute end offset in the key makes sure
	%% packing of every chunk is unique, even when the same chunk is
	%% present in the same transaction or across multiple transactions
	%% or blocks. The presence of the transaction root in the key
	%% ensures one cannot find data that has certain patterns after
	%% packing.
	{spora_2_5, crypto:hash(sha256, << ChunkOffset:256, TXRoot/binary >>)};
chunk_key({spora_2_6, RewardAddr}, ChunkOffset, TXRoot) ->
	%% The presence of the absolute end offset in the key makes sure
	%% packing of every chunk is unique, even when the same chunk is
	%% present in the same transaction or across multiple transactions
	%% or blocks. The presence of the transaction root in the key
	%% ensures one cannot find data that has certain patterns after
	%% packing. The presence of the reward address, combined with
	%% the 2.6 mining mechanics, puts a relatively low cap on the performance
	%% of a single dataset replica, essentially incentivizing miners to create
	%% more weave replicas per invested dollar.
	{
		spora_2_6,
		crypto:hash(sha256, << ChunkOffset:256, TXRoot:32/binary, RewardAddr/binary >>)
	};
chunk_key({composite, RewardAddr, PackingDiff}, ChunkOffset, TXRoot) ->
	%% This is only a part of the packing key. Each sub-chunk is packed using a different
	%% key composed from the key returned by this function and the relative sub-chunk offset.
	{
		composite,
		crypto:hash(sha256, << ChunkOffset:256, TXRoot:32/binary, PackingDiff:8,
				RewardAddr/binary >>)
	}.

get_sub_chunks(<< SubChunk:(?COMPOSITE_PACKING_SUB_CHUNK_SIZE)/binary, Rest/binary >>) ->
	[SubChunk | get_sub_chunks(Rest)];
get_sub_chunks(<<>>) ->
	[].

unpack_replica_2_9_sub_chunks(RewardAddr, AbsoluteEndOffset, RandomXState, SubChunks) ->
	unpack_replica_2_9_sub_chunks(
		RewardAddr, AbsoluteEndOffset, RandomXState, 0, SubChunks, []).

unpack_replica_2_9_sub_chunks(_RewardAddr, _AbsoluteEndOffset, _RandomXState,
		_SubChunkStartOffset, [], UnpackedSubChunks) ->
	{ok, iolist_to_binary(lists:reverse(UnpackedSubChunks))};
unpack_replica_2_9_sub_chunks(RewardAddr, AbsoluteEndOffset, RandomXState,
		SubChunkStartOffset, [SubChunk | SubChunks], UnpackedSubChunks) ->
	EntropySubChunkIndex = ar_replica_2_9:get_slice_index(AbsoluteEndOffset),
	Entropy = generate_replica_2_9_entropy(RewardAddr, AbsoluteEndOffset, SubChunkStartOffset,
			RandomXState),
	case ar_mine_randomx:randomx_decrypt_replica_2_9_sub_chunk({RandomXState,
			Entropy, SubChunk, EntropySubChunkIndex}) of
		{ok, UnpackedSubChunk} ->
			SubChunkSize = ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
			unpack_replica_2_9_sub_chunks(RewardAddr, AbsoluteEndOffset, RandomXState,
					SubChunkStartOffset + SubChunkSize, SubChunks,
					[UnpackedSubChunk | UnpackedSubChunks]);
		Error ->
			Error
	end.

validate_chunk_size(spora_2_5, Chunk, ChunkSize) ->
	PackedSize = byte_size(Chunk),
	case PackedSize ==
			(((ChunkSize - 1) div (?DATA_CHUNK_SIZE)) + 1) * (?DATA_CHUNK_SIZE) of
		false ->
			{error, invalid_packed_size};
		true ->
			{ok, PackedSize}
	end;
validate_chunk_size({spora_2_6, _Addr}, Chunk, ChunkSize) ->
	validate_chunk_size(Chunk, ChunkSize);
validate_chunk_size({composite, _Addr, _PackingDifficulty}, Chunk, ChunkSize) ->
	validate_chunk_size(Chunk, ChunkSize);
validate_chunk_size({replica_2_9, _Addr}, Chunk, ChunkSize) ->
	validate_chunk_size(Chunk, ChunkSize).

validate_chunk_size(Chunk, ChunkSize) ->
	PackedSize = byte_size(Chunk),
	case {PackedSize == ?DATA_CHUNK_SIZE, ChunkSize =< PackedSize andalso ChunkSize > 0} of
		{false, _} ->
			{error, invalid_packed_size};
		{true, false} ->
			%% In practice, we would never get here because the merkle proof
			%% validation does not allow ChunkSize to exceed ?DATA_CHUNK_SIZE.
			{error, invalid_chunk_size};
		_ ->
			{ok, PackedSize}
	end.
