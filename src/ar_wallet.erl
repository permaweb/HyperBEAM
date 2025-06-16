-module(ar_wallet).
-export([sign/2, sign/3, hmac/1, hmac/2, verify/3, verify/4]).
-export([to_pubkey/1, to_pubkey/2, to_address/1, to_address/2, new/0, new/1]).
-export([new_keyfile/2, load_keyfile/1, load_keyfile/2, load_key/1, load_key/2]).
-include("include/ar.hrl").

-include_lib("public_key/include/public_key.hrl").

%%% @doc Utilities for manipulating wallets.

-define(WALLET_DIR, ".").

%%% Public interface.


new() ->
    new({rsa, 65537}).


new(KeyType = {KeyAlg, PublicExpnt}) when KeyType =:= {rsa, 65537} ->
    {[_, Pub], [_, Pub, Priv | _]} = {[_, Pub], [_, Pub, Priv | _]} =
        crypto:generate_key(KeyAlg, {4096, PublicExpnt}),
    {{KeyType, Priv, Pub}, {KeyType, Pub}}.


%% @doc Sign some data with a private key.
sign(Key, Data) ->
    sign(Key, Data, sha256).


%% @doc sign some data, hashed using the provided DigestType.
%% TODO: support signing for other key types
sign({{rsa, PublicExpnt}, Priv, Pub}, Data, DigestType) when PublicExpnt =:= 65537 ->
    rsa_pss:sign(
      Data,
      DigestType,
      #'RSAPrivateKey'{
        publicExponent = PublicExpnt,
        modulus = binary:decode_unsigned(Pub),
        privateExponent = binary:decode_unsigned(Priv)
       });
sign({{KeyType, Priv, Pub}, {KeyType, Pub}}, Data, DigestType) ->
    sign({KeyType, Priv, Pub}, Data, DigestType).


hmac(Data) ->
    hmac(Data, sha256).


hmac(Data, DigestType) -> crypto:mac(hmac, DigestType, <<"ar">>, Data).


%% @doc Verify that a signature is correct.
verify(Key, Data, Sig) ->
    verify(Key, Data, Sig, sha256).


verify({{rsa, PublicExpnt}, Pub}, Data, Sig, DigestType) when PublicExpnt =:= 65537 ->
    rsa_pss:verify(
      Data,
      DigestType,
      Sig,
      #'RSAPublicKey'{
        publicExponent = PublicExpnt,
        modulus = binary:decode_unsigned(Pub)
       }).


%% @doc Find a public key from a wallet.
to_pubkey(Pubkey) ->
    to_pubkey(Pubkey, ?DEFAULT_KEY_TYPE).


to_pubkey(PubKey, {rsa, 65537}) when bit_size(PubKey) == 256 ->
    % Small keys are not secure, nobody is using them, the clause
    % is for backwards-compatibility.
    PubKey;
to_pubkey({{_, _, PubKey}, {_, PubKey}}, {rsa, 65537}) ->
    PubKey;
to_pubkey(PubKey, {rsa, 65537}) ->
    PubKey.


%% @doc Generate an address from a public key.
to_address(Pubkey) ->
    to_address(Pubkey, ?DEFAULT_KEY_TYPE).


to_address(PubKey, {rsa, 65537}) when bit_size(PubKey) == 256 ->
    PubKey;
to_address({{_, _, PubKey}, {_, PubKey}}, _) ->
    to_address(PubKey);
to_address(PubKey, {rsa, 65537}) ->
    to_rsa_address(PubKey);
to_address(PubKey, {ecdsa, 256}) ->
    to_ecdsa_address(PubKey).


%% @doc Generate a new wallet public and private key, with a corresponding keyfile.
%% The provided key is used as part of the file name.
new_keyfile(KeyType, WalletName) when is_list(WalletName) ->
    new_keyfile(KeyType, list_to_binary(WalletName));
new_keyfile(KeyType, WalletName) ->
    {Pub, Priv, Key} =
        case KeyType of
            {?RSA_SIGN_ALG, PublicExpnt} ->
                {[Expnt, Pb], [Expnt, Pb, Prv, P1, P2, E1, E2, C]} =
                    crypto:generate_key(rsa, {?RSA_PRIV_KEY_SZ, PublicExpnt}),
                Ky =
                    hb_json:encode(
                      #{
                        kty => <<"RSA">>,
                        ext => true,
                        e => hb_util:encode(Expnt),
                        n => hb_util:encode(Pb),
                        d => hb_util:encode(Prv),
                        p => hb_util:encode(P1),
                        q => hb_util:encode(P2),
                        dp => hb_util:encode(E1),
                        dq => hb_util:encode(E2),
                        qi => hb_util:encode(C)
                       }),
                {Pb, Prv, Ky};
            {?ECDSA_SIGN_ALG, secp256k1} ->
                {OrigPub, Prv} = crypto:generate_key(ecdh, secp256k1),
                <<4:8, PubPoint/binary>> = OrigPub,
                PubPointMid = byte_size(PubPoint) div 2,
                <<X:PubPointMid/binary, Y:PubPointMid/binary>> = PubPoint,
                Ky =
                    hb_json:encode(
                      #{
                        kty => <<"EC">>,
                        crv => <<"secp256k1">>,
                        x => hb_util:encode(X),
                        y => hb_util:encode(Y),
                        d => hb_util:encode(Prv)
                       }),
                {compress_ecdsa_pubkey(OrigPub), Prv, Ky};
            {?EDDSA_SIGN_ALG, ed25519} ->
                {{_, Prv, Pb}, _} = new(KeyType),
                Ky =
                    hb_json:encode(
                      #{
                        kty => <<"OKP">>,
                        alg => <<"EdDSA">>,
                        crv => <<"Ed25519">>,
                        x => hb_util:encode(Pb),
                        d => hb_util:encode(Prv)
                       }),
                {Pb, Prv, Ky}
        end,
    Filename = wallet_filepath(WalletName, Pub, KeyType),
    filelib:ensure_dir(Filename),
    file:write_file(Filename, Key),
    {{KeyType, Priv, Pub}, {KeyType, Pub}}.


wallet_filepath(Wallet) ->
    filename:join([?WALLET_DIR, binary_to_list(Wallet)]).


wallet_filepath2(Wallet) ->
    filename:join([?WALLET_DIR, binary_to_list(Wallet)]).


%% @doc Read the keyfile for the key with the given address from disk.
%% Return not_found if arweave_keyfile_[addr].json or [addr].json is not found
%% in [data_dir]/?WALLET_DIR.
load_key(Addr) ->
    load_key(Addr, #{}).


%% @doc Read the keyfile for the key with the given address from disk.
%% Return not_found if arweave_keyfile_[addr].json or [addr].json is not found
%% in [data_dir]/?WALLET_DIR.
load_key(Addr, Opts) ->
    Path = hb_util:encode(Addr),
    case filelib:is_file(Path) of
        false ->
            Path2 = wallet_filepath2(hb_util:encode(Addr)),
            case filelib:is_file(Path2) of
                false ->
                    not_found;
                true ->
                    load_keyfile(Path2, Opts)
            end;
        true ->
            load_keyfile(Path, Opts)
    end.


%% @doc Extract the public and private key from a keyfile.
load_keyfile(File) ->
    load_keyfile(File, #{}).


%% @doc Extract the public and private key from a keyfile.
load_keyfile(File, Opts) ->
    {ok, Body} = file:read_file(File),
    Key = hb_json:decode(Body),
    {Pub, Priv, KeyType} =
        case hb_maps:get(<<"kty">>, Key, undefined, Opts) of
            <<"EC">> ->
                XEncoded = hb_maps:get(<<"x">>, Key, undefined, Opts),
                YEncoded = hb_maps:get(<<"y">>, Key, undefined, Opts),
                PrivEncoded = hb_maps:get(<<"d">>, Key, undefined, Opts),
                OrigPub = iolist_to_binary([<<4:8>>,
                                            hb_util:decode(XEncoded),
                                            hb_util:decode(YEncoded)]),
                Pb = compress_ecdsa_pubkey(OrigPub),
                Prv = hb_util:decode(PrivEncoded),
                KyType = {?ECDSA_SIGN_ALG, secp256k1},
                {Pb, Prv, KyType};
            <<"OKP">> ->
                PubEncoded = hb_maps:get(<<"x">>, Key, undefined, Opts),
                PrivEncoded = hb_maps:get(<<"d">>, Key, undefined, Opts),
                Pb = hb_util:decode(PubEncoded),
                Prv = hb_util:decode(PrivEncoded),
                KyType = {?EDDSA_SIGN_ALG, ed25519},
                {Pb, Prv, KyType};
            _ ->
                PubEncoded = hb_maps:get(<<"n">>, Key, undefined, Opts),
                PrivEncoded = hb_maps:get(<<"d">>, Key, undefined, Opts),
                Pb = hb_util:decode(PubEncoded),
                Prv = hb_util:decode(PrivEncoded),
                KyType = {?RSA_SIGN_ALG, 65537},
                {Pb, Prv, KyType}
        end,
    {{KeyType, Priv, Pub}, {KeyType, Pub}}.


%%%===================================================================
%%% Private functions.
%%%===================================================================


to_rsa_address(PubKey) ->
    hash_address(PubKey).


hash_address(PubKey) ->
    crypto:hash(sha256, PubKey).


to_ecdsa_address(PubKey) ->
    hb_keccak:key_to_ethereum_address(PubKey).


%%%===================================================================
%%% Private functions.
%%%===================================================================


wallet_filepath(WalletName, PubKey, KeyType) ->
    wallet_filepath(wallet_name(WalletName, PubKey, KeyType)).


wallet_name(wallet_address, PubKey, KeyType) ->
    hb_util:encode(to_address(PubKey, KeyType));
wallet_name(WalletName, _, _) ->
    WalletName.


compress_ecdsa_pubkey(<<4:8, PubPoint/binary>>) ->
    PubPointMid = byte_size(PubPoint) div 2,
    <<X:PubPointMid/binary, Y:PubPointMid/integer-unit:8>> = PubPoint,
    PubKeyHeader =
        case Y rem 2 of
            0 -> <<2:8>>;
            1 -> <<3:8>>
        end,
    iolist_to_binary([PubKeyHeader, X]).
