-module(secp_check).
-export([main/1]).

%% Exercises HyperBEAM's existing `secp256k1_nif' (priv/secp256k1_arweave.so)
%% the way Arweave's `ar_wallet:verify/3' does for 2.9-era ECDSA block
%% signatures: `secp256k1_nif:ecrecover(SignaturePreimage, Signature)'.
main(_) ->
	Msg = <<"arweave 2.9 block signature preimage">>,
	{Pub, Priv} = crypto:generate_key(ecdh, secp256k1),
	io:format("pubkey size = ~p bytes, privkey size = ~p bytes~n",
		[byte_size(Pub), byte_size(Priv)]),
	%% The NIF returns a 33-byte COMPRESSED key; ?ECDSA_PUB_KEY_SIZE is 33 and
	%% ar_wallet:verify/3 compares it against the block's reward_key directly.
	Compressed = compress(Pub),
	Sig = secp256k1_nif:sign(Msg, Priv),
	io:format("recoverable signature size = ~p bytes (?ECDSA_SIG_SIZE = 65)~n",
		[byte_size(Sig)]),
	{Ok, Recovered} = secp256k1_nif:ecrecover(Msg, Sig),
	io:format("recovered pubkey size = ~p bytes (?ECDSA_PUB_KEY_SIZE = 33)~n",
		[byte_size(Recovered)]),
	io:format("ecrecover ok = ~p, recovered == compressed pubkey: ~p~n",
		[Ok, Recovered =:= Compressed]),
	%% Independent cross-check against OTP's own ECDSA verifier.
	<<R:32/binary, S:32/binary, _V:8>> = Sig,
	Der = der(R, S),
	io:format("crypto:verify(ecdsa, sha256, ...) = ~p~n",
		[crypto:verify(ecdsa, sha256, Msg, Der, [Pub, secp256k1])]),
	%% Negative controls.
	%% ECDSA recovery always yields *some* key; ar_wallet:verify/3 rejects by
	%% comparing it against the block's reward_key, so that is what we assert.
	{Ok2, Rec2} = secp256k1_nif:ecrecover(<<Msg/binary, "!">>, Sig),
	io:format("tampered message: ecrecover ok = ~p, pubkey matches reward key = ~p"
		" (ar_wallet:verify/3 would return ~p)~n",
		[Ok2, Rec2 =:= Compressed, Ok2 andalso Rec2 =:= Compressed]),
	io:format("tampered message: crypto:verify = ~p~n",
		[crypto:verify(ecdsa, sha256, <<Msg/binary, "!">>, Der, [Pub, secp256k1])]),
	init:stop().

compress(<<4, X:32/binary, Y:32/binary>>) ->
	<<_:31/binary, Last>> = Y,
	<<(2 + (Last band 1)), X/binary>>.

der(R, S) ->
	RI = int(R),
	SI = int(S),
	Body = <<RI/binary, SI/binary>>,
	<<16#30, (byte_size(Body)):8, Body/binary>>.

int(<<0, Rest/binary>>) when byte_size(Rest) > 0 ->
	int(Rest);
int(Bin = <<First, _/binary>>) when First >= 16#80 ->
	<<16#02, (byte_size(Bin) + 1):8, 0, Bin/binary>>;
int(Bin) ->
	<<16#02, (byte_size(Bin)):8, Bin/binary>>.
