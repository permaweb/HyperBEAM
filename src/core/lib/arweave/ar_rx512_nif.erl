-module(ar_rx512_nif).

-on_load(init_nif/0).

-export([rx512_hash_nif/5, rx512_info_nif/1, rx512_init_nif/5,
		rx512_encrypt_chunk_nif/7, rx512_decrypt_chunk_nif/8,
		rx512_reencrypt_chunk_nif/10
]).

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% VENDOR: the `?LOG_ERROR' call that preceded each
%% `erlang:nif_error(nif_not_loaded)' upstream is dropped with the
%% `ar.hrl' include it needed. Those lines ran only when the `.so'
%% failed to load, immediately before raising.

rx512_info_nif(_State) ->
	erlang:nif_error(nif_not_loaded).

rx512_init_nif(_Key, _HashingMode, _JIT, _LargePages, _Threads) ->
	erlang:nif_error(nif_not_loaded).

rx512_hash_nif(_State, _Data, _JIT, _LargePages, _HardwareAES) ->
	erlang:nif_error(nif_not_loaded).

rx512_encrypt_chunk_nif(_State, _Data, _Chunk, _RoundCount, _JIT, _LargePages, _HardwareAES) ->
	erlang:nif_error(nif_not_loaded).

rx512_decrypt_chunk_nif(_State, _Data, _Chunk, _OutSize, _RoundCount, _JIT, _LargePages,
		_HardwareAES) ->
	erlang:nif_error(nif_not_loaded).

rx512_reencrypt_chunk_nif(_State, _DecryptKey, _EncryptKey, _Chunk, _ChunkSize,
		_DecryptRoundCount, _EncryptRoundCount, _JIT, _LargePages, _HardwareAES) ->
	erlang:nif_error(nif_not_loaded).

%% VENDOR: `code:priv_dir(arweave)' upstream. The `.so' ships in
%% HyperBEAM's own `priv', so the application name differs; nothing else
%% about the load path does.
init_nif() ->
	PrivDir = code:priv_dir(hb),
	ok = erlang:load_nif(filename:join([PrivDir, "rx512_arweave"]), 0).
