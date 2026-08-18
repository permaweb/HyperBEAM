-module(ar_rx4096_nif).

-on_load(init_nif/0).

-export([rx4096_hash_nif/5, rx4096_info_nif/1, rx4096_init_nif/5,
		rx4096_encrypt_composite_chunk_nif/9,
		rx4096_decrypt_composite_chunk_nif/10,
		rx4096_decrypt_composite_sub_chunk_nif/10,
		rx4096_reencrypt_composite_chunk_nif/13
]).

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% VENDOR: the `?LOG_ERROR' call that preceded each
%% `erlang:nif_error(nif_not_loaded)' upstream is dropped with the
%% `ar.hrl' include it needed. Those lines ran only when the `.so'
%% failed to load, immediately before raising.

rx4096_info_nif(_State) ->
	erlang:nif_error(nif_not_loaded).

rx4096_init_nif(_Key, _HashingMode, _JIT, _LargePages, _Threads) ->
	erlang:nif_error(nif_not_loaded).

rx4096_hash_nif(_State, _Data, _JIT, _LargePages, _HardwareAES) ->
	erlang:nif_error(nif_not_loaded).

rx4096_encrypt_composite_chunk_nif(_State, _Key, _Chunk, _JIT, _LargePages, _HardwareAES,
		_RoundCount, _IterationCount, _SubChunkCount) ->
	erlang:nif_error(nif_not_loaded).

rx4096_decrypt_composite_chunk_nif(_State, _Data, _Chunk, _OutSize,
		_JIT, _LargePages, _HardwareAES, _RoundCount, _IterationCount, _SubChunkCount) ->
	erlang:nif_error(nif_not_loaded).

rx4096_decrypt_composite_sub_chunk_nif(_State, _Data, _Chunk, _OutSize,
		_JIT, _LargePages, _HardwareAES, _RoundCount, _IterationCount, _Offset) ->
	erlang:nif_error(nif_not_loaded).

rx4096_reencrypt_composite_chunk_nif(_State,
		_DecryptKey, _EncryptKey, _Chunk,
		_JIT, _LargePages, _HardwareAES,
		_DecryptRoundCount, _EncryptRoundCount,
		_DecryptIterationCount, _EncryptIterationCount,
		_DecryptSubChunkCount, _EncryptSubChunkCount) ->
	erlang:nif_error(nif_not_loaded).

%% VENDOR: `code:priv_dir(arweave)' upstream. The `.so' ships in
%% HyperBEAM's own `priv', so the application name differs; nothing else
%% about the load path does.
init_nif() ->
	PrivDir = code:priv_dir(hb),
	ok = erlang:load_nif(filename:join([PrivDir, "rx4096_arweave"]), 0).
