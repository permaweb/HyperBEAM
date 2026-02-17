-module(secp256k1_nif).
-export([sign/2, ecrecover/2, sign_recoverable/2, recover_pk_and_verify/2]).

-on_load(init/0).

%% Based on Arweave's src/secp256k1_nif.erl

init() ->
	PrivDir = code:priv_dir(hb),
	ok = erlang:load_nif(filename:join([PrivDir, "secp256k1_arweave"]), 0).

sign_recoverable(_Digest, _PrivateBytes) ->
	erlang:nif_error(nif_not_loaded).

recover_pk_and_verify(_Digest, _Signature) ->
	erlang:nif_error(nif_not_loaded).

sign(Msg, PrivBytes) ->
	Digest = crypto:hash(sha256, Msg),
	{ok, Signature} = sign_recoverable(Digest, PrivBytes),
	Signature.

ecrecover(Msg, Signature) ->
	Digest = crypto:hash(sha256, Msg),
	case recover_pk_and_verify(Digest, Signature) of
		{ok, true, PubKey} -> {true, PubKey};
		{ok, false, _PubKey} -> {false, <<>>};
		{error, _Reason} -> {false, <<>>}
	end.
