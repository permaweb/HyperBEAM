-module(ar_rxsquared_nif).

-on_load(init_nif/0).

-export([rxsquared_hash_nif/5, rxsquared_info_nif/1, rxsquared_init_nif/5,
		rsp_fused_entropy_nif/10,
		rsp_feistel_encrypt_nif/2,
		rsp_feistel_decrypt_nif/2]).

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% VENDOR: the `?LOG_ERROR' call that preceded each
%% `erlang:nif_error(nif_not_loaded)' upstream is dropped with the
%% `ar.hrl' include it needed. Those lines ran only when the `.so'
%% failed to load, immediately before raising.

rxsquared_info_nif(_State) ->
	erlang:nif_error(nif_not_loaded).

rxsquared_init_nif(_Key, _HashingMode, _JIT, _LargePages, _Threads) ->
	erlang:nif_error(nif_not_loaded).

rxsquared_hash_nif(_State, _Data, _JIT, _LargePages, _HardwareAES) ->
	erlang:nif_error(nif_not_loaded).

%% VENDOR: `code:priv_dir(arweave)' upstream. The `.so' ships in
%% HyperBEAM's own `priv', so the application name differs; nothing else
%% about the load path does.
init_nif() ->
	PrivDir = code:priv_dir(hb),
	ok = erlang:load_nif(filename:join([PrivDir, "rxsquared_arweave"]), 0).

%%%===================================================================
%%% Randomx square packing
%%%===================================================================

rsp_fused_entropy_nif(
	_RandomxState,
	_ReplicaEntropySubChunkCount,
	_CompositePackingSubChunkSize,
	_LaneCount,
	_RxDepth,
	_JitEnabled,
	_LargePagesEnabled,
	_HardwareAESEnabled,
	_RandomxProgramCount,
	_Key
) ->
	erlang:nif_error(nif_not_loaded).

rsp_feistel_encrypt_nif(_InMsg, _Key) ->
	erlang:nif_error(nif_not_loaded).

rsp_feistel_decrypt_nif(_InMsg, _Key) ->
	erlang:nif_error(nif_not_loaded).
