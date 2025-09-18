# ar_wallet

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/ar_wallet.erl)

Utilities for manipulating wallets.

---

## Exported Functions

- `from_json/1`
- `from_json/2`
- `hmac/1`
- `hmac/2`
- `load_key/1`
- `load_key/2`
- `load_keyfile/1`
- `load_keyfile/2`
- `new_keyfile/2`
- `new/0`
- `new/1`
- `sign/2`
- `sign/3`
- `to_address/1`
- `to_address/2`
- `to_json/1`
- `to_pubkey/1`
- `to_pubkey/2`
- `verify/3`
- `verify/4`

---

### new

Utilities for manipulating wallets.

```erlang
new() ->
    new({rsa, 65537}).
```

### new

```erlang
new(KeyType = {KeyAlg, PublicExpnt}) when KeyType =:= {rsa, 65537} ->
    {[_, Pub], [_, Pub, Priv|_]} = {[_, Pub], [_, Pub, Priv|_]}
        = crypto:generate_key(KeyAlg, {4096, PublicExpnt}),
    {{KeyType, Priv, Pub}, {KeyType, Pub}}.
```

### sign

Sign some data with a private key.

```erlang
sign(Key, Data) ->
    sign(Key, Data, sha256).
```

### sign

sign some data, hashed using the provided DigestType.

```erlang
sign({{rsa, PublicExpnt}, Priv, Pub}, Data, DigestType) when PublicExpnt =:= 65537 ->
    rsa_pss:sign(
        Data,
        DigestType,
        #'RSAPrivateKey'{
            publicExponent = PublicExpnt,
            modulus = binary:decode_unsigned(Pub),
            privateExponent = binary:decode_unsigned(Priv)
        }
    );
```

### sign

sign some data, hashed using the provided DigestType.

```erlang
sign({{KeyType, Priv, Pub}, {KeyType, Pub}}, Data, DigestType) ->
    sign({KeyType, Priv, Pub}, Data, DigestType).
```

### hmac

```erlang
hmac(Data) ->
    hmac(Data, sha256).
```

### hmac

Verify that a signature is correct.

```erlang
hmac(Data, DigestType) -> crypto:mac(hmac, DigestType, <<"ar">>, Data).
```

### verify

Verify that a signature is correct.

```erlang
verify(Key, Data, Sig) ->
    verify(Key, Data, Sig, sha256).
```

### verify

```erlang
verify({{rsa, PublicExpnt}, Pub}, Data, Sig, DigestType) when PublicExpnt =:= 65537 ->
    rsa_pss:verify(
        Data,
        DigestType,
        Sig,
        #'RSAPublicKey'{
            publicExponent = PublicExpnt,
            modulus = binary:decode_unsigned(Pub)
        }
    ).
```

### to_pubkey

Find a public key from a wallet.

```erlang
to_pubkey(Pubkey) ->
    to_pubkey(Pubkey, ?DEFAULT_KEY_TYPE).
```

### to_pubkey

```erlang
to_pubkey(PubKey, {rsa, 65537}) when bit_size(PubKey) == 256 ->
    % Small keys are not secure, nobody is using them, the clause
    % is for backwards-compatibility.
```

### to_pubkey

```erlang
to_pubkey({{_, _, PubKey}, {_, PubKey}}, {rsa, 65537}) ->
    PubKey;
```

### to_pubkey

```erlang
to_pubkey(PubKey, {rsa, 65537}) ->
    PubKey.
```

### to_address

Generate an address from a public key.

```erlang
to_address(Pubkey) ->
    to_address(Pubkey, ?DEFAULT_KEY_TYPE).
```

### to_address

```erlang
to_address(PubKey, {rsa, 65537}) when bit_size(PubKey) == 256 ->
    PubKey;
```

### to_address

```erlang
to_address({{_, _, PubKey}, {_, PubKey}}, _) ->
    to_address(PubKey);
```

### to_address

```erlang
to_address(PubKey, {rsa, 65537}) ->
    to_rsa_address(PubKey);
```

### to_address

```erlang
to_address(PubKey, {ecdsa, 256}) ->
	to_ecdsa_address(PubKey).
```

### new_keyfile

Generate a new wallet public and private key, with a corresponding keyfile.

```erlang
new_keyfile(KeyType, WalletName) when is_list(WalletName) ->
    new_keyfile(KeyType, list_to_binary(WalletName));
```

### new_keyfile

Generate a new wallet public and private key, with a corresponding keyfile.

```erlang
new_keyfile(KeyType, WalletName) ->
    {Pub, Priv, Key} =
        case KeyType of
            {?RSA_SIGN_ALG, PublicExpnt} ->
                {[Expnt, Pb], [Expnt, Pb, Prv, P1, P2, E1, E2, C]} =
                    crypto:generate_key(rsa, {?RSA_PRIV_KEY_SZ, PublicExpnt}),
                PrivKey = {KeyType, Prv, Pb},
                Ky = to_json(PrivKey),
                {Pb, Prv, Ky};
            {?ECDSA_SIGN_ALG, secp256k1} ->
                {OrigPub, Prv} = crypto:generate_key(ecdh, secp256k1),
                CompressedPub = compress_ecdsa_pubkey(OrigPub),
                PrivKey = {KeyType, Prv, CompressedPub},
                Ky = to_json(PrivKey),
                {CompressedPub, Prv, Ky};
            {?EDDSA_SIGN_ALG, ed25519} ->
                {{_, Prv, Pb}, _} = new(KeyType),
                PrivKey = {KeyType, Prv, Pb},
                Ky = to_json(PrivKey),
                {Pb, Prv, Ky}
        end,
    Filename = wallet_filepath(WalletName, Pub, KeyType),
    filelib:ensure_dir(Filename),
    file:write_file(Filename, Key),
    {{KeyType, Priv, Pub}, {KeyType, Pub}}.
```

### wallet_filepath

```erlang
wallet_filepath(Wallet) ->
    filename:join([?WALLET_DIR, binary_to_list(Wallet)]).
```

### wallet_filepath2

```erlang
wallet_filepath2(Wallet) ->
    filename:join([?WALLET_DIR, binary_to_list(Wallet)]).
```

### load_key

Read the keyfile for the key with the given address from disk.

```erlang
load_key(Addr) ->
    load_key(Addr, #{}).
```

### load_key

Read the keyfile for the key with the given address from disk.

```erlang
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
```

### load_keyfile

Extract the public and private key from a keyfile.

```erlang
load_keyfile(File) ->
    load_keyfile(File, #{}).
```

### load_keyfile

Extract the public and private key from a keyfile.

```erlang
load_keyfile(File, Opts) ->
    {ok, Body} = file:read_file(File),
    from_json(Body, Opts).
```

### to_json

Convert a wallet private key to JSON (JWK) format

```erlang
to_json({PrivKey, _PubKey}) ->
    to_json(PrivKey);
```

### to_json

Convert a wallet private key to JSON (JWK) format

```erlang
to_json({{?RSA_SIGN_ALG, PublicExpnt}, Priv, Pub}) when PublicExpnt =:= 65537 ->
    hb_json:encode(#{
        kty => <<"RSA">>,
        ext => true,
        e => hb_util:encode(<<PublicExpnt:32>>),
        n => hb_util:encode(Pub),
        d => hb_util:encode(Priv)
    });
```

### to_json

Convert a wallet private key to JSON (JWK) format

```erlang
to_json({{?ECDSA_SIGN_ALG, secp256k1}, Priv, CompressedPub}) ->
    % For ECDSA, we need to expand the compressed pubkey to get X,Y coordinates
    % This is a simplified version - ideally we'd implement pubkey expansion
    hb_json:encode(#{
        kty => <<"EC">>,
        crv => <<"secp256k1">>,
        d => hb_util:encode(Priv)
        % TODO: Add x and y coordinates from expanded pubkey
    });
```

### to_json

Convert a wallet private key to JSON (JWK) format

```erlang
to_json({{?EDDSA_SIGN_ALG, ed25519}, Priv, Pub}) ->
    hb_json:encode(#{
        kty => <<"OKP">>,
        alg => <<"EdDSA">>,
        crv => <<"Ed25519">>,
        x => hb_util:encode(Pub),
        d => hb_util:encode(Priv)
    }).
```

### from_json

Parse a wallet from JSON (JWK) format

```erlang
from_json(JsonBinary) ->
    from_json(JsonBinary, #{}).
```

### from_json

Parse a wallet from JSON (JWK) format with options

```erlang
from_json(JsonBinary, Opts) ->
    Key = hb_json:decode(JsonBinary),
    {Pub, Priv, KeyType} =
        case hb_maps:get(<<"kty">>, Key, undefined, Opts) of
            <<"EC">> ->
                XEncoded = hb_maps:get(<<"x">>, Key, undefined, Opts),
                YEncoded = hb_maps:get(<<"y">>, Key, undefined, Opts),
                PrivEncoded = hb_maps:get(<<"d">>, Key, undefined, Opts),
                OrigPub = iolist_to_binary([<<4:8>>, hb_util:decode(XEncoded),
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
```

### to_rsa_address

```erlang
to_rsa_address(PubKey) ->
    hash_address(PubKey).
```

### hash_address

```erlang
hash_address(PubKey) ->
    crypto:hash(sha256, PubKey).
```

### to_ecdsa_address

```erlang
to_ecdsa_address(PubKey) ->
	hb_keccak:key_to_ethereum_address(PubKey).
```

### wallet_filepath

```erlang
wallet_filepath(WalletName, PubKey, KeyType) ->
    wallet_filepath(wallet_name(WalletName, PubKey, KeyType)).
```

### wallet_name

```erlang
wallet_name(wallet_address, PubKey, KeyType) ->
    hb_util:encode(to_address(PubKey, KeyType));
```

### wallet_name

```erlang
wallet_name(WalletName, _, _) ->
    WalletName.
```

### compress_ecdsa_pubkey

```erlang
compress_ecdsa_pubkey(<<4:8, PubPoint/binary>>) ->
    PubPointMid = byte_size(PubPoint) div 2,
    <<X:PubPointMid/binary, Y:PubPointMid/integer-unit:8>> = PubPoint,
    PubKeyHeader =
        case Y rem 2 of
            0 -> <<2:8>>;
            1 -> <<3:8>>
        end,
```

---

*Generated from [ar_wallet.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/ar_wallet.erl)*
