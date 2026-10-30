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

%% @doc DigestType can be `sha256`, `ethereum` or `{typed_ethereum, Address}`.
sign(Msg, PrivBytes) ->
    sign(Msg, PrivBytes, sha256).
sign(Msg, PrivBytes, DigestType) ->
	Digest = digest_message(DigestType, Msg),
	{ok, Signature} = sign_recoverable(Digest, PrivBytes),
	ethereum_signature(Signature, DigestType).

%% @doc DigestType can be `sha256`, `ethereum` or `{typed_ethereum, Address}`.
ecrecover(Msg, Signature) ->
    ecrecover(Msg, Signature, sha256).
ecrecover(Msg, Signature, DigestType) ->
	Digest = digest_message(DigestType, Msg),
    case normalize_signature(Signature, DigestType) of
        invalid -> {false, <<>>};
        NormalizedSig ->
            case recover_pk_and_verify(Digest, NormalizedSig) of
                {ok, true, PubKey} -> {true, PubKey};
                {ok, false, _PubKey} -> {false, <<>>};
                {error, _Reason} -> {false, <<>>}
            end
    end.

digest_message(sha256, Msg) -> crypto:hash(sha256, Msg);
digest_message(ethereum, Msg) -> ethereum_hash(Msg);
digest_message({typed_ethereum, Address}, Msg) -> typed_ethereum_hash(Address, Msg).

%% @doc Give an Ethereum signature the recovery byte `27' or `28' that
%% ANS-104 signers give.
ethereum_signature(<<Compact:64/binary, V:8>>, ethereum) when V < 2 ->
    <<Compact/binary, (V + 27):8>>;
ethereum_signature(Signature, _) ->
    Signature.

%% @doc Normalize the recovery byte of an Ethereum signature from `27' or
%% `28' to `0' or `1'. A signature with any other recovery byte is `invalid'.
normalize_signature(<<_Compact:64/binary, V:8>>, DigestType)
        when (DigestType =:= ethereum orelse
            (is_tuple(DigestType) andalso
                element(1, DigestType) =:= typed_ethereum)),
            V < 27 ->
    invalid;
normalize_signature(<<Compact:64/binary, V:8>>, ethereum) when V >= 27 -> 
    <<Compact/binary, (V - 27):8>>;
normalize_signature(<<Compact:64/binary, V:8>>, {typed_ethereum, _}) when V >= 27 ->
    <<Compact/binary, (V - 27):8>>;
normalize_signature(Signature, _) -> 
    Signature.

%% @doc Ethereum EIP-191 personal_sign hash:
%% keccak256("\x19Ethereum Signed Message:\n" + len(msg) + msg)
ethereum_hash(Msg) ->
	Prefix = <<"\x19Ethereum Signed Message:\n">>,
	Len = integer_to_binary(byte_size(Msg)),
	hb_keccak:keccak_256(<<Prefix/binary, Len/binary, Msg/binary>>).

%% @doc EIP-712 hash of a data item's signature data for the `typed_ethereum`
%% signature type: the domain is `Bundlr` version `1` and the struct is
%% `Bundlr(bytes Transaction hash,address address)`, with the signer's
%% address as the `address` field.
typed_ethereum_hash(<<"0x", Hex:40/binary>>, Msg) ->
	DomainType = <<"EIP712Domain(string name,string version)">>,
	StructType = <<"Bundlr(bytes Transaction hash,address address)">>,
	DomainSeparator =
		hb_keccak:keccak_256(<<
			(hb_keccak:keccak_256(DomainType))/binary,
			(hb_keccak:keccak_256(<<"Bundlr">>))/binary,
			(hb_keccak:keccak_256(<<"1">>))/binary
		>>),
	StructHash =
		hb_keccak:keccak_256(<<
			(hb_keccak:keccak_256(StructType))/binary,
			(hb_keccak:keccak_256(Msg))/binary,
			0:96,
			(binary:decode_hex(Hex))/binary
		>>),
	hb_keccak:keccak_256(<<16#19, 16#01, DomainSeparator/binary, StructHash/binary>>).
