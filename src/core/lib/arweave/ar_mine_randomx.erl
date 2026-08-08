%%% @doc Copied and adapted from the arweave codebase.
%%% Should track:
%%% https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_mine_randomx.erl
-module(ar_mine_randomx).

%% VENDOR: dropped the packing exports - randomx_encrypt_chunk/4,
%% randomx_reencrypt_chunk/7 and randomx_encrypt_replica_2_9_sub_chunk/1. A validator
%% only ever unpacks.
-export([init_fast/3, init_light/2, info/1, hash/2, hash/5,
		randomx_decrypt_chunk/5,
		randomx_decrypt_sub_chunk/5,
		randomx_generate_replica_2_9_entropy/2,
		randomx_decrypt_replica_2_9_sub_chunk/1,
		exor_sub_chunk/2]).

-export([jit/0, large_pages/0, hardware_aes/0, init_fast2/5, init_light2/4]).

-include("include/ar.hrl").
-include("include/ar_consensus.hrl").
%% VENDOR: dropped -include_lib("arweave_config/include/arweave_config.hrl"). The three
%% options this module read are now the constants below.
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% VENDOR: dropped the -ifdef(STUB_RANDOMX) clauses of init_fast/3 and init_light/2.
%% The STUB build replaces RandomX with SHA-256; it is not consensus.
init_fast(RxMode, Key, Threads) ->
	init_fast2(RxMode, Key, jit(), large_pages(), Threads).
%% VENDOR: upstream passes `init_light2(RxMode, jit(), large_pages(), Key)',
%% but the callee is `init_light2(RxMode, Key, JIT, LargePages)' -- so `Key'
%% arrives as `LargePages' and the NIF raises `badarg'. Upstream never trips
%% over it because nothing there calls `init_light/2': the node always builds
%% fast states, and the light tests call `init_light2/4' directly. Light mode
%% is our default, so the arguments are corrected here.
init_light(RxMode, Key) ->
	init_light2(RxMode, Key, jit(), large_pages()).

info(State) ->
	info2(State).

hash(State, Data) ->
	hash(State, Data, jit(), large_pages(), hardware_aes()).

hash(State, Data, JIT, LargePages, HardwareAES) ->
	hash2(State, Data, JIT, LargePages, HardwareAES).

randomx_decrypt_chunk(Packing, RandomxState, Key, Chunk, ChunkSize) ->
	PackedSize = byte_size(Chunk),
	%% For the spora_2_6 and composite packing schemes we want to confirm
	%% the padding in the unpacked chunk is all zeros.
	%% To do that we pass in the maximum chunk size (?DATA_CHUNK_SIZE) to prevent the NIF
	%% from removing the padding. We can then validate the padding and remove it in
	%% ar_packing_server:unpad_chunk/4.
	Size = case Packing of
		{spora_2_6, _Addr} ->
			?DATA_CHUNK_SIZE;
		{composite, _Addr, _PackingDifficulty} ->
			?DATA_CHUNK_SIZE;
		_ ->
			ChunkSize
	end,
	case randomx_decrypt_chunk2(RandomxState, Key, Chunk, Size, Packing) of
		{error, invalid_randomx_mode} ->
			{error, invalid_randomx_mode};
		{error, Error} ->
			%% All other errors are from the NIF, so we treat as an exception
			{exception, Error};
		{ok, Unpacked} ->
			%% Validating the padding (for spora_2_6 and composite) and then remove it.
			case ar_packing_server:unpad_chunk(Packing, Unpacked, ChunkSize, PackedSize) of
				error ->
					?LOG_WARNING([{event, unpad_chunk_error},
							{packed_size, PackedSize},
							{chunk_size, ChunkSize}]),
					{error, invalid_padding};
				UnpackedChunk ->
					{ok, UnpackedChunk}
			end
	end.

randomx_decrypt_sub_chunk(Packing, RandomxState, Key, Chunk, SubChunkStartOffset) ->
	case randomx_decrypt_sub_chunk2(Packing, RandomxState, Key, Chunk, SubChunkStartOffset) of
		{error, invalid_randomx_mode} ->
			{error, invalid_randomx_mode};
		{error, Error} ->
			%% All other errors are from the NIF, so we treat as an exception
			{exception, Error};
		Reply ->
			Reply
	end.

%% VENDOR: dropped the AR_TEST clause of randomx_generate_replica_2_9_entropy/2, which
%% substitutes a SHA-256 chain for the fused RandomX entropy.
randomx_generate_replica_2_9_entropy({rxsquared, RandomxState}, Key) ->
	{ok, EntropyFused} = ar_rxsquared_nif:rsp_fused_entropy_nif(
		RandomxState,
		?COMPOSITE_PACKING_SUB_CHUNK_COUNT,
		?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
		?REPLICA_2_9_RANDOMX_LANE_COUNT,
		?REPLICA_2_9_RANDOMX_DEPTH,
		jit(),
		large_pages(),
		hardware_aes(),
		?REPLICA_2_9_RANDOMX_PROGRAM_COUNT,
		Key
	),
	EntropyFused.

randomx_decrypt_replica_2_9_sub_chunk(
		{_PackingState, Entropy, SubChunk, EntropySubChunkIndex}) ->
	SubChunkSize = ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
	EntropyPart = binary:part(Entropy, EntropySubChunkIndex * SubChunkSize, SubChunkSize),
	{ok, exor_sub_chunk(SubChunk, EntropyPart)}.

%% @doc Encipher/decipher the given sub-chunk using the given 2.9 entropy.
-spec exor_sub_chunk(
		SubChunk :: binary(),
		EntropyPart :: binary()
) -> binary().
exor_sub_chunk(SubChunk, EntropyPart) ->
	crypto:exor(SubChunk, EntropyPart).

%%%===================================================================
%%% Private functions.
%%%===================================================================

%% -------------------------------------------------------------------------------------------
%% Helper functions
%% -------------------------------------------------------------------------------------------
%% VENDOR: jit/0, large_pages/0 and hardware_aes/0 read Config#config.{enable,disable}
%% upstream. Their mainnet defaults are inlined here - `randomx_jit' is not in the
%% default disable list, `randomx_large_pages' is not in the default enable list, and
%% `randomx_hardware_aes' is not in the default disable list.
jit() ->
	1.

large_pages() ->
	0.

hardware_aes() ->
	1.

init_fast2(rx512, Key, JIT, LargePages, Threads) ->
	{ok, FastState} = ar_rx512_nif:rx512_init_nif(Key, ?RANDOMX_HASHING_MODE_FAST, JIT, LargePages, Threads),
	{rx512, FastState};
init_fast2(rx4096, Key, JIT, LargePages, Threads) ->
	{ok, FastState} = ar_rx4096_nif:rx4096_init_nif(Key, ?RANDOMX_HASHING_MODE_FAST, JIT, LargePages, Threads),
	{rx4096, FastState};
init_fast2(rxsquared, Key, JIT, LargePages, Threads) ->
	{ok, FastState} = ar_rxsquared_nif:rxsquared_init_nif(Key, ?RANDOMX_HASHING_MODE_FAST, JIT, LargePages, Threads),
	{rxsquared, FastState};
init_fast2(RxMode, _Key, _JIT, _LargePages, _Threads) ->
	?LOG_ERROR([{event, invalid_randomx_mode}, {mode, RxMode}]),
	{error, invalid_randomx_mode}.
init_light2(rx512, Key, JIT, LargePages) ->
	{ok, LightState} = ar_rx512_nif:rx512_init_nif(Key, ?RANDOMX_HASHING_MODE_LIGHT, JIT, LargePages, 0),
	{rx512, LightState};
init_light2(rx4096, Key, JIT, LargePages) ->
	{ok, LightState} = ar_rx4096_nif:rx4096_init_nif(Key, ?RANDOMX_HASHING_MODE_LIGHT, JIT, LargePages, 0),
	{rx4096, LightState};
init_light2(rxsquared, Key, JIT, LargePages) ->
	{ok, LightState} = ar_rxsquared_nif:rxsquared_init_nif(Key, ?RANDOMX_HASHING_MODE_LIGHT, JIT, LargePages, 0),
	{rxsquared, LightState};
init_light2(RxMode, _Key, _JIT, _LargePages) ->
	?LOG_ERROR([{event, invalid_randomx_mode}, {mode, RxMode}]),
	{exceperrortion, invalid_randomx_mode}.

info2({rx512, State}) ->
	ar_rx512_nif:rx512_info_nif(State);
info2({rx4096, State}) ->
	ar_rx4096_nif:rx4096_info_nif(State);
info2({rxsquared, State}) ->
	ar_rxsquared_nif:rxsquared_info_nif(State);
info2(_) ->
	{error, invalid_randomx_mode}.

%% -------------------------------------------------------------------------------------------
%% hash2 and randomx_decrypt_[chunk|sub_chunk]2
%% VENDOR: the STUB clauses, which are selected when State is {stub_state, Key}, are
%% dropped throughout - see the note on init_fast/3.
%% -------------------------------------------------------------------------------------------
hash2({rx512, State}, Data, JIT, LargePages, HardwareAES) ->
	{ok, Hash} = ar_rx512_nif:rx512_hash_nif(State, Data, JIT, LargePages, HardwareAES),
	Hash;
hash2({rx4096, State}, Data, JIT, LargePages, HardwareAES) ->
	{ok, Hash} = ar_rx4096_nif:rx4096_hash_nif(State, Data, JIT, LargePages, HardwareAES),
	Hash;
hash2({rxsquared, State}, Data, JIT, LargePages, HardwareAES) ->
	{ok, Hash} = ar_rxsquared_nif:rxsquared_hash_nif(State, Data, JIT, LargePages, HardwareAES),
	Hash;
hash2(_BadState, _Data, _JIT, _LargePages, _HardwareAES) ->
	{error, invalid_randomx_mode}.

randomx_decrypt_chunk2({rx512, RandomxState}, Key, Chunk, ChunkSize, spora_2_5) ->
	ar_rx512_nif:rx512_decrypt_chunk_nif(RandomxState, Key, Chunk, ChunkSize, ?RANDOMX_PACKING_ROUNDS,
			jit(), large_pages(), hardware_aes());
randomx_decrypt_chunk2({rx512, RandomxState}, Key, Chunk, ChunkSize, {spora_2_6, _Addr}) ->
	ar_rx512_nif:rx512_decrypt_chunk_nif(RandomxState, Key, Chunk, ChunkSize, ?RANDOMX_PACKING_ROUNDS_2_6,
			jit(), large_pages(), hardware_aes());
randomx_decrypt_chunk2({rx4096, RandomxState}, Key, Chunk, ChunkSize,
		{composite, _Addr, PackingDifficulty}) ->
	ar_rx4096_nif:rx4096_decrypt_composite_chunk_nif(RandomxState, Key, Chunk, ChunkSize,
			jit(), large_pages(), hardware_aes(), ?COMPOSITE_PACKING_ROUND_COUNT,
			PackingDifficulty, ?COMPOSITE_PACKING_SUB_CHUNK_COUNT);
randomx_decrypt_chunk2(_BadState, _Key, _Chunk, _ChunkSize, _Packing) ->
	{error, invalid_randomx_mode}.

randomx_decrypt_sub_chunk2(Packing, {rx4096, RandomxState}, Key, Chunk, SubChunkStartOffset) ->
	{_, _, IterationCount} = Packing,
	RoundCount = ?COMPOSITE_PACKING_ROUND_COUNT,
	OutSize = ?COMPOSITE_PACKING_SUB_CHUNK_SIZE,
	ar_rx4096_nif:rx4096_decrypt_composite_sub_chunk_nif(RandomxState, Key, Chunk, OutSize,
		jit(), large_pages(), hardware_aes(), RoundCount, IterationCount, SubChunkStartOffset);
randomx_decrypt_sub_chunk2(_Packing, _BadState, _Key, _Chunk, _SubChunkStartOffset) ->
	{error, invalid_randomx_mode}.
