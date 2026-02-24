-module(ar_wallet).
-export([sign/2, sign/3, hmac/1, hmac/2, verify/3, verify/4]).
-export([to_pubkey/1, to_pubkey/2, to_address/1, to_address/2, new/0, new_ecdsa/0, new/1]).
-export([new_keyfile/2, load_keyfile/1, load_keyfile/2, load_key/1, load_key/2]).
-export([to_json/1, from_json/1, from_json/2]).
-export([recover_key/3]).
-export([compress_ecdsa_pubkey/1]).
-include("include/ar.hrl").
-include_lib("public_key/include/public_key.hrl").

%%% @doc Utilities for manipulating wallets.

-define(WALLET_DIR, ".").
-define(WALLET_POOL_TARGET, 6).

%%% Public interface.

new() ->
    new({rsa, 65537}).
new(KeyType) when KeyType =:= {rsa, 65537} orelse KeyType =:= {eddsa, ed25519} ->
    case request_pooled_wallet(KeyType) of
        {ok, Wallet} -> Wallet;
        timeout -> generate_wallet(KeyType)
    end;
new(KeyType = {?ECDSA_SIGN_ALG, secp256k1}) ->
    case request_pooled_wallet(KeyType) of
        {ok, Wallet} -> Wallet;
        timeout -> generate_wallet(KeyType)
    end.

new_ecdsa() ->
    new({?ECDSA_SIGN_ALG, secp256k1}).

generate_wallet(KeyType = {KeyAlg, PublicExpnt}) when KeyType =:= {rsa, 65537} ->
    {[_, Pub], [_, Pub, Priv|_]} = {[_, Pub], [_, Pub, Priv|_]}
        = crypto:generate_key(KeyAlg, {4096, PublicExpnt}),
    {{KeyType, Priv, Pub}, {KeyType, Pub}};
generate_wallet(KeyType = {KeyAlg, KeyCrv}) when KeyAlg =:= ?ECDSA_SIGN_ALG andalso KeyCrv =:= secp256k1 ->
    {OrigPub, Priv} = crypto:generate_key(ecdh, KeyCrv),
    Pub = compress_ecdsa_pubkey(OrigPub),
    {{KeyType, Priv, Pub}, {KeyType, Pub}};
generate_wallet(KeyType = {KeyAlg, Curve}) when KeyType =:= {?EDDSA_SIGN_ALG, ed25519} ->
    {Pub, Priv} = crypto:generate_key(KeyAlg, Curve),
    {{KeyType, Priv, Pub}, {KeyType, Pub}}.

request_pooled_wallet(KeyType) ->
    Pool = ensure_wallet_pool(KeyType),
    Ref = make_ref(),
    Pool ! {wallet, self(), Ref},
    receive
        {wallet, Ref, Wallet} -> {ok, Wallet}
    after 30000 ->
        timeout
    end.

ensure_wallet_pool(KeyType) ->
    PoolName = wallet_pool_name(KeyType),
    case whereis(PoolName) of
        undefined ->
            Pid = spawn(fun() -> wallet_pool_loop(KeyType, queue:new(), queue:new(), 0) end),
            case catch register(PoolName, Pid) of
                true -> Pid;
                _ -> whereis(PoolName)
            end;
        Pid ->
            Pid
    end.

wallet_pool_loop(KeyType, Wallets, Waiters, InFlight) ->
    {Wallets1, InFlight1} = maybe_spawn_wallet_workers(KeyType, Wallets, Waiters, InFlight),
    receive
        {wallet, From, Ref} ->
            case queue:out(Wallets1) of
                {{value, Wallet}, Rest} ->
                    From ! {wallet, Ref, Wallet},
                    wallet_pool_loop(KeyType, Rest, Waiters, InFlight1);
                {empty, _} ->
                    wallet_pool_loop(KeyType, Wallets1, queue:in({From, Ref}, Waiters), InFlight1)
            end;
        {wallet_generated, Wallet} ->
            case queue:out(Waiters) of
                {{value, {From, Ref}}, RestWaiters} ->
                    From ! {wallet, Ref, Wallet},
                    wallet_pool_loop(KeyType, Wallets1, RestWaiters, InFlight1 - 1);
                {empty, _} ->
                    wallet_pool_loop(KeyType, queue:in(Wallet, Wallets1), Waiters, InFlight1 - 1)
            end
    end.

maybe_spawn_wallet_workers(KeyType, Wallets, Waiters, InFlight) ->
    Desired = ?WALLET_POOL_TARGET + queue:len(Waiters),
    Available = queue:len(Wallets) + InFlight,
    Needed = max(0, Desired - Available),
    Parent = self(),
    lists:foreach(
        fun(_) ->
            spawn(fun() -> Parent ! {wallet_generated, generate_wallet(KeyType)} end)
        end,
        lists:seq(1, Needed)
    ),
    {Wallets, InFlight + Needed}.

wallet_pool_name({rsa, 65537}) ->
    ar_wallet_pool_rsa_65537;
wallet_pool_name({?EDDSA_SIGN_ALG, ed25519}) ->
    ar_wallet_pool_ed25519;
wallet_pool_name({?ECDSA_SIGN_ALG, secp256k1}) ->
    ar_wallet_pool_ecdsa_secp256k1.

%% @doc Sign some data with a private key.
sign(Key, Data) ->
    sign(Key, Data, sha256).

%% @doc sign some data, hashed using the provided DigestType.
%% RSA and ECDSA signatures use wallet-level wrappers.
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
sign({{KeyAlg, KeyCrv}, Priv, _Pub}, Data, _DigestType)
        when KeyAlg =:= ?ECDSA_SIGN_ALG andalso KeyCrv =:= secp256k1 ->
    secp256k1_nif:sign(Data, Priv);
sign({KeyType = {KeyAlg, Curve}, Priv, _Pub}, Data, _DigestType) when KeyType =:= {?EDDSA_SIGN_ALG, ed25519} ->
    crypto:sign(KeyAlg, none, Data, [Priv, Curve]);
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
        }
    );
%% NOTE: We will not write pubkey for ECDSA signature. So don't use verify function 
%% for ECDSA directly, use ecrecover pattern. This function will return always false 
%% if called with no Pub.
verify({{KeyAlg, KeyCrv}, Pub}, Data, Sig, _DigestType)
        when KeyAlg =:= ?ECDSA_SIGN_ALG andalso KeyCrv =:= secp256k1 ->
    {Pass, PubExtracted} = secp256k1_nif:ecrecover(Data, Sig),
    Pass andalso PubExtracted =:= Pub;
verify({{KeyAlg, Curve}, Pub}, Data, Sig, _DigestType) when
      byte_size(Pub) == 32 andalso byte_size(Sig) == 64 andalso Curve =:= ed25519 andalso KeyAlg =:= ?EDDSA_SIGN_ALG ->
    crypto:verify(eddsa, none, Data, Sig, [Pub, Curve]);
verify({solana, Pub}, Data, Sig, _DigestType) when
      byte_size(Pub) == 32 andalso byte_size(Sig) == 64 ->
    HexData = hb_util:to_hex(Data),
    crypto:verify(eddsa, none, HexData, Sig, [Pub, ed25519]).

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
to_address(PubKey, {?ECDSA_SIGN_ALG, 256}) ->
	to_ecdsa_address(PubKey);
to_address(PubKey, {?ECDSA_SIGN_ALG, secp256k1}) ->
	%% For Arweave L1 ECDSA transactions, address is SHA256 hash of public key
	%% (same as RSA). The keccak-based Ethereum address is used elsewhere.
	hash_address(PubKey);
to_address(PubKey, {?EDDSA_SIGN_ALG, ed25519}) ->
    to_eddsa_address(PubKey);
to_address(PubKey, solana) ->
    to_solana_address(PubKey).

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
    from_json(Body, Opts).

%% @doc Convert a wallet private key to JSON (JWK) format
to_json({PrivKey, _PubKey}) ->
    to_json(PrivKey);
to_json({{?RSA_SIGN_ALG, PublicExpnt}, Priv, Pub}) when PublicExpnt =:= 65537 ->
    hb_json:encode(#{
        kty => <<"RSA">>,
        ext => true,
        e => hb_util:encode(<<PublicExpnt:32>>),
        n => hb_util:encode(Pub),
        d => hb_util:encode(Priv)
    });
to_json({{?ECDSA_SIGN_ALG, secp256k1}, Priv, CompressedPub}) ->
    % For ECDSA, we need to expand the compressed pubkey to get X,Y coordinates
    % This is a simplified version - ideally we'd implement pubkey expansion
    hb_json:encode(#{
        kty => <<"EC">>,
        crv => <<"secp256k1">>,
        d => hb_util:encode(Priv)
        % TODO: Add x and y coordinates from expanded pubkey
    });
to_json({{?EDDSA_SIGN_ALG, ed25519}, Priv, Pub}) ->
    hb_json:encode(#{
        kty => <<"OKP">>,
        alg => <<"EdDSA">>,
        crv => <<"Ed25519">>,
        x => hb_util:encode(Pub),
        d => hb_util:encode(Priv)
    }).

%% @doc Parse a wallet from JSON (JWK) format
from_json(JsonBinary) ->
    from_json(JsonBinary, #{}).

%% @doc Parse a wallet from JSON (JWK) format with options
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

%% @doc Recover the public key from a signature (for ECDSA).
%% For ECDSA transactions, the public key is not included in the transaction,
%% it must be recovered from the signature.
recover_key(_Data, <<>>, ?ECDSA_KEY_TYPE) ->
    <<>>;
recover_key(Data, Signature, ?ECDSA_KEY_TYPE) ->
    {_Pass, PubKey} = secp256k1_nif:ecrecover(Data, Signature),
    %% Note: if Pass = false, then PubKey will be <<>>
    PubKey.

%%%===================================================================
%%% Private functions.
%%%===================================================================

to_rsa_address(PubKey) ->
    hash_address(PubKey).

hash_address(PubKey) ->
    crypto:hash(sha256, PubKey).

to_ecdsa_address(PubKey) ->
	hb_keccak:key_to_ethereum_address(PubKey).

to_eddsa_address(PubKey) ->
    hash_address(PubKey).

to_solana_address(PubKey) ->
    hb_util:base58_encode(PubKey).
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