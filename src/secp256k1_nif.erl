-module(secp256k1_nif).
-export([sign/2, sign/3, ecrecover/2, ecrecover/3, sign_recoverable/2, recover_pk_and_verify/2]).

-on_load(init/0).

%% Based on Arweave's src/secp256k1_nif.erl

init() ->
	PrivDir = code:priv_dir(hb),
	ok = erlang:load_nif(filename:join([PrivDir, "secp256k1_arweave"]), 0).

sign_recoverable(_Digest, _PrivateBytes) ->
	erlang:nif_error(nif_not_loaded).

recover_pk_and_verify(_Digest, _Signature) ->
	erlang:nif_error(nif_not_loaded).

%% @doc DigestType can be `sha256` or `ethereum`.
sign(Msg, PrivBytes) ->
    sign(Msg, PrivBytes, sha256).
sign(Msg, PrivBytes, DigestType) ->
	Digest = digest_message(DigestType, Msg),
	{ok, Signature} = sign_recoverable(Digest, PrivBytes),
	Signature.

%% @doc DigestType can be `sha256` or `ethereum`.
ecrecover(Msg, Signature) ->
    ecrecover(Msg, Signature, sha256).
ecrecover(Msg, Signature, DigestType) ->
	Digest = digest_message(DigestType, Msg),
    NormalizedSig = normalize_signature(Signature, DigestType),
	case recover_pk_and_verify(Digest, NormalizedSig) of
		{ok, true, PubKey} -> {true, PubKey};
		{ok, false, _PubKey} -> {false, <<>>};
		{error, _Reason} -> {false, <<>>}
	end.

digest_message(sha256, Msg) -> crypto:hash(sha256, Msg);
digest_message(ethereum, Msg) -> ethereum_hash(Msg).

%% @doc Normalize Ethereum v values: 27/28 -> 0/1
normalize_signature(<<Compact:64/binary, V:8>>, ethereum) when V >= 27 -> 
    <<Compact/binary, (V - 27):8>>;
normalize_signature(Signature, _) -> 
    Signature.

%% @doc Ethereum EIP-191 personal_sign hash:
%% keccak256("\x19Ethereum Signed Message:\n" + len(msg) + msg)
ethereum_hash(Msg) ->
	Prefix = <<"\x19Ethereum Signed Message:\n">>,
	Len = integer_to_binary(byte_size(Msg)),
	hb_keccak:keccak_256(<<Prefix/binary, Len/binary, Msg/binary>>).
