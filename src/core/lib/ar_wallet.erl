-module(ar_wallet).
-export([sign/2, sign/3, hmac/1, hmac/2, verify/3, verify/4]).
-export([to_pubkey/1, to_pubkey/2, to_address/1, to_address/2, new/0, new_ecdsa/0, new/1]).
-export([new/3, committer_device/1]).
-export([request_pooled_wallet/1]).
-export([new_keyfile/2, load_keyfile/1, load_keyfile/2, load_key/1, load_key/2]).
-export([to_json/1, from_json/1, from_json/2]).
-export([recover_key/3]).
-export([rsa_private_key/1]).
-export([compress_ecdsa_pubkey/1]).
-include("include/ar.hrl").
-include_lib("public_key/include/public_key.hrl").

%%% @doc Utilities for manipulating wallets.
%%%
%%% A wallet is the committer message of a key: `did-ar@1.0' for an RSA,
%%% Ed25519 or ECDSA key, and `did-pkh@1.0' for an Ethereum or Solana key. Its
%%% `owner' is the public key, its `type' is the signature type of the key,
%%% and `d' in its `priv' message is the private key, which no ID or encoding
%%% of the wallet includes. If the `priv' message of an RSA wallet also
%%% contains the CRT values of the key (`p', `q', `dp', `dq' and `qi'), the
%%% wallet signs with them. `verify' needs only the `owner' and the `type'. Key
%%% files contain the key in the JWK format.

-define(WALLET_DIR, ".").
-define(WALLET_POOL_TARGET, 6).
-define(SECP256K1_PRIME,
    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F).
%% The keys of the CRT values of an RSA key in a JWK and in the `priv' message
%% of a wallet, ordered as `crypto:generate_key/2' gives the values.
-define(RSA_CRT_KEYS, [<<"p">>, <<"q">>, <<"dp">>, <<"dq">>, <<"qi">>]).

%%% Public interface.

new() ->
    new({rsa, 65537}).
new(KeyType) when KeyType =:= {rsa, 65537} orelse KeyType =:= {eddsa, ed25519} orelse KeyType =:= ethereum orelse KeyType =:= solana ->
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

%% @doc Make the wallet of a key from its type, private key and public key.
new(KeyType, Priv, Pub) ->
    new(KeyType, Priv, Pub, #{}).

%% @doc Make the wallet of a key, with the CRT values of an RSA key in its
%% `priv' message.
new(KeyType, Priv, Pub, CRT) ->
    #{
        <<"device">> => committer_device(KeyType),
        <<"type">> => ar_tx:serialize_sig_type(KeyType),
        <<"owner">> => Pub,
        <<"priv">> => CRT#{ <<"d">> => Priv }
    }.

%% @doc Return the device of the committer of a key: `did-ar@1.0' for a key
%% whose address is its SHA-256 (RSA, Ed25519 and ECDSA), else `did-pkh@1.0'.
committer_device(KeyType)
        when KeyType =:= ?RSA_KEY_TYPE;
            KeyType =:= ?EDDSA_KEY_TYPE;
            KeyType =:= ?ECDSA_KEY_TYPE ->
    <<"did-ar@1.0">>;
committer_device(_KeyType) ->
    <<"did-pkh@1.0">>.

generate_wallet(KeyType = {KeyAlg, PublicExpnt}) when KeyType =:= {rsa, 65537} ->
    {[_, Pub], [_, Pub, Priv | CRT]} =
        crypto:generate_key(KeyAlg, {4096, PublicExpnt}),
    new(KeyType, Priv, Pub, maps:from_list(lists:zip(?RSA_CRT_KEYS, CRT)));
generate_wallet(KeyType = {KeyAlg, KeyCrv}) when KeyAlg =:= ?ECDSA_SIGN_ALG andalso KeyCrv =:= secp256k1 ->
    {OrigPub, Priv} = crypto:generate_key(ecdh, KeyCrv),
    Pub = compress_ecdsa_pubkey(OrigPub),
    new(KeyType, Priv, Pub);
generate_wallet(ethereum)  ->
    {Pub, Priv} = crypto:generate_key(ecdh, secp256k1),
    new(ethereum, Priv, Pub);
generate_wallet(solana) ->
    generate_wallet({eddsa, ed25519});
generate_wallet(KeyType = {KeyAlg, Curve}) when KeyType =:= {?EDDSA_SIGN_ALG, ed25519} ->
    {Pub, Priv} = crypto:generate_key(KeyAlg, Curve),
    new(KeyType, Priv, Pub).

%% @doc Request a wallet from the pool, waiting up to 30 seconds.
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

maybe_spawn_wallet_workers(KeyType, Wallets, _Waiters, InFlight) ->
    % Waiters share the fixed pool target; they do not each add a worker.
    Desired = ?WALLET_POOL_TARGET,
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
    ar_wallet_pool_ecdsa_secp256k1;
wallet_pool_name(solana) ->
    ar_wallet_pool_solana;
wallet_pool_name(ethereum) ->
    ar_wallet_pool_ethereum.

%% @doc Sign some data with a wallet.
sign(Wallet, Data) ->
    sign(Wallet, Data, sha256).

%% @doc sign some data, hashed using the provided DigestType.
%% RSA and ECDSA signatures use wallet-level wrappers.
sign(Wallet = #{ <<"type">> := ?RSA_SIGN_TYPE }, Data, DigestType) ->
    rsa_pss:sign(Data, DigestType, rsa_private_key(Wallet));
sign(Wallet = #{ <<"type">> := ?ECDSA_SIGN_TYPE }, Data, _DigestType) ->
    #{ <<"priv">> := #{ <<"d">> := Priv } } = Wallet,
    secp256k1_nif:sign(Data, Priv);
sign(Wallet = #{ <<"type">> := ?EDDSA_SIGN_TYPE }, Data, _DigestType) ->
    #{ <<"priv">> := #{ <<"d">> := Priv } } = Wallet,
    crypto:sign(eddsa, none, Data, [Priv, ed25519]);
sign(Wallet = #{ <<"type">> := ?ETHEREUM_SIGN_TYPE }, Data, _DigestType) ->
    #{ <<"priv">> := #{ <<"d">> := Priv } } = Wallet,
    secp256k1_nif:sign(Data, Priv, ethereum).

%% @doc Return the `RSAPrivateKey' record of an RSA wallet, with the CRT values
%% of the key if its `priv' message contains them.
rsa_private_key(Wallet = #{ <<"type">> := Type }) ->
    #{ <<"owner">> := Pub, <<"priv">> := Priv } = Wallet,
    {rsa, PublicExpnt} = ar_tx:deserialize_sig_type(Type),
    Ints = maps:map(fun(_, Value) -> binary:decode_unsigned(Value) end, Priv),
    #'RSAPrivateKey'{
        publicExponent = PublicExpnt,
        modulus = binary:decode_unsigned(Pub),
        privateExponent = maps:get(<<"d">>, Ints),
        prime1 = maps:get(<<"p">>, Ints, undefined),
        prime2 = maps:get(<<"q">>, Ints, undefined),
        exponent1 = maps:get(<<"dp">>, Ints, undefined),
        exponent2 = maps:get(<<"dq">>, Ints, undefined),
        coefficient = maps:get(<<"qi">>, Ints, undefined)
    }.

hmac(Data) ->
    hmac(Data, sha256).

hmac(Data, DigestType) -> crypto:mac(hmac, DigestType, <<"ar">>, Data).

%% @doc Verify that a signature is correct, using the `owner' and `type' of a
%% wallet.
verify(Wallet, Data, Sig) ->
    verify(Wallet, Data, Sig, sha256).

verify(#{ <<"type">> := Type = ?RSA_SIGN_TYPE, <<"owner">> := Pub },
        Data, Sig, DigestType) ->
    {rsa, PublicExpnt} = ar_tx:deserialize_sig_type(Type),
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
verify(#{ <<"type">> := ?ECDSA_SIGN_TYPE, <<"owner">> := Pub },
        Data, Sig, _DigestType) ->
    {Pass, PubExtracted} = secp256k1_nif:ecrecover(Data, Sig),
    Pass andalso PubExtracted =:= Pub;
verify(#{ <<"type">> := ?EDDSA_SIGN_TYPE, <<"owner">> := Pub },
        Data, Sig, _DigestType)
        when byte_size(Pub) == 32 andalso byte_size(Sig) == 64 ->
    crypto:verify(eddsa, none, Data, Sig, [Pub, ed25519]);
verify(#{ <<"type">> := ?ETHEREUM_SIGN_TYPE, <<"owner">> := Pub },
        Data, Sig, _DigestType) ->
    {Pass, PubExtracted} = secp256k1_nif:ecrecover(Data, Sig, ethereum),
    Pass andalso PubExtracted =:= compress_ecdsa_pubkey(Pub);
verify(#{ <<"type">> := ?TYPED_ETHEREUM_SIGN_TYPE, <<"owner">> := Address },
        Data, Sig, _DigestType) ->
    {Pass, PubExtracted} =
        secp256k1_nif:ecrecover(Data, Sig, {typed_ethereum, Address}),
    Pass andalso
        to_address(Address, typed_ethereum) =:=
            to_ethereum_address(decompress_ecdsa_pubkey(PubExtracted));
verify(#{ <<"type">> := ?SOLANA_SIGN_TYPE, <<"owner">> := Pub },
        Data, Sig, _DigestType)
        when byte_size(Pub) == 32 andalso byte_size(Sig) == 64 ->
    HexData = hb_util:to_hex(Data),
    crypto:verify(eddsa, none, HexData, Sig, [Pub, ed25519]).

%% @doc Find a public key from a wallet.
to_pubkey(#{ <<"owner">> := Owner }) ->
    Owner;
to_pubkey(Pubkey) ->
    to_pubkey(Pubkey, ?DEFAULT_KEY_TYPE).
to_pubkey(PubKey, {rsa, 65537}) when bit_size(PubKey) == 256 ->
    % Small keys are not secure, nobody is using them, the clause
    % is for backwards-compatibility.
    PubKey;
to_pubkey(PubKey, {rsa, 65537}) ->
    PubKey.

%% @doc Generate an address from a wallet, or from a public key. A wallet's
%% address is the address of its `owner' for its key type, which the URI of
%% its committer holds.
to_address(#{ <<"owner">> := Owner, <<"type">> := Type }) ->
    to_address(Owner, ar_tx:deserialize_sig_type(Type));
to_address(Pubkey) ->
    to_address(Pubkey, ?DEFAULT_KEY_TYPE).
to_address(PubKey, {rsa, 65537}) when bit_size(PubKey) == 256 ->
    PubKey;
to_address(PubKey, {rsa, 65537}) ->
    to_rsa_address(PubKey);
to_address(PubKey, {?ECDSA_SIGN_ALG, secp256k1}) ->
	%% For Arweave L1 ECDSA transactions, address is SHA256 hash of public key
	%% (same as RSA). The keccak-based Ethereum address is used elsewhere.
	hash_address(PubKey);
to_address(PubKey, {?EDDSA_SIGN_ALG, ed25519}) ->
    to_eddsa_address(PubKey);
to_address(PubKey, solana) ->
    to_solana_address(PubKey);
to_address(PubKey, ethereum) ->
    to_ethereum_address(PubKey);
to_address(Address, typed_ethereum) ->
    hb_keccak:address_to_checksum_address(Address).

%% @doc Generate a new wallet public and private key, with a corresponding keyfile.
%% The provided key is used as part of the file name.
new_keyfile(KeyType, WalletName) when is_list(WalletName) ->
    new_keyfile(KeyType, list_to_binary(WalletName));
new_keyfile(KeyType, WalletName) ->
    Wallet = #{ <<"owner">> := Pub } = new(KeyType),
    Filename = wallet_filepath(WalletName, Pub, KeyType),
    filelib:ensure_dir(Filename),
    file:write_file(Filename, to_json(Wallet)),
    Wallet.

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

%% @doc Extract the public and private key from a keyfile. An RSA key without
%% its CRT values gets them, recovered from `n', `e' and `d'.
load_keyfile(File, Opts) ->
    {ok, Body} = file:read_file(File),
    with_crt(from_json(Body, Opts)).

%% @doc Convert a wallet to JSON (JWK) format
to_json(Wallet = #{ <<"type">> := Type = ?RSA_SIGN_TYPE }) ->
    #{ <<"owner">> := Pub, <<"priv">> := #{ <<"d">> := Priv } } = Wallet,
    {?RSA_SIGN_ALG, PublicExpnt} = ar_tx:deserialize_sig_type(Type),
    % Add the CRT values of the key, if the wallet has them.
    CRT =
        maps:map(
            fun(_, Value) -> hb_util:encode(Value) end,
            maps:with(?RSA_CRT_KEYS, maps:get(<<"priv">>, Wallet))
        ),
    hb_json:encode(CRT#{
        kty => <<"RSA">>,
        ext => true,
        e => hb_util:encode(binary:encode_unsigned(PublicExpnt)),
        n => hb_util:encode(Pub),
        d => hb_util:encode(Priv)
    });
to_json(Wallet = #{ <<"type">> := ?ECDSA_SIGN_TYPE }) ->
    #{ <<"owner">> := CompressedPub, <<"priv">> := #{ <<"d">> := Priv } } = Wallet,
    <<4:8, X:32/binary, Y:32/binary>> = decompress_ecdsa_pubkey(CompressedPub),
    hb_json:encode(#{
        kty => <<"EC">>,
        crv => <<"secp256k1">>,
        x => hb_util:encode(X),
        y => hb_util:encode(Y),
        d => hb_util:encode(Priv)
    });
to_json(Wallet = #{ <<"type">> := ?EDDSA_SIGN_TYPE }) ->
    #{ <<"owner">> := Pub, <<"priv">> := #{ <<"d">> := Priv } } = Wallet,
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
    % Keep the CRT values of an RSA key, if the JWK has them.
    CRT =
        maps:map(
            fun(_, Value) -> hb_util:decode(Value) end,
            hb_maps:with(?RSA_CRT_KEYS, Key, Opts)
        ),
    new(KeyType, Priv, Pub, CRT).

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

to_ethereum_address(PubKey) ->
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

%% @doc Restore the uncompressed form of a compressed secp256k1 public key.
decompress_ecdsa_pubkey(<<Prefix:8, X:256>>)
        when Prefix =:= 2 orelse Prefix =:= 3 ->
    P = ?SECP256K1_PRIME,
    Root =
        binary:decode_unsigned(
            crypto:mod_pow((X * X * X + 7) rem P, (P + 1) div 4, P)
        ),
    Y =
        case Root rem 2 =:= Prefix - 2 of
            true -> Root;
            false -> P - Root
        end,
    <<4:8, X:256, Y:256>>.

%% @doc Add the CRT values of an RSA key to its wallet, if it has none: recover
%% them from `n', `e' and `d'. Return any other wallet as it is.
with_crt(Wallet = #{ <<"type">> := ?RSA_SIGN_TYPE, <<"priv">> := Priv })
        when not is_map_key(<<"p">>, Priv) ->
    #'RSAPrivateKey'{ modulus = N, publicExponent = E, privateExponent = D } =
        rsa_private_key(Wallet),
    Wallet#{ <<"priv">> => maps:merge(Priv, recover_crt(N, E, D)) };
with_crt(Wallet) ->
    Wallet.

%% @doc Recover the CRT values of an RSA key from `n', `e' and `d', with `p' the
%% greater prime. If no prime of `n' is found, or the primes do not multiply to
%% `n', return no CRT values.
recover_crt(N, E, D) ->
    case find_prime(N, odd_part(E * D - 1, 0), 100) of
        {ok, Prime} ->
            Other = N div Prime,
            crt_values(max(Prime, Other), min(Prime, Other), N, D);
        not_found ->
            #{}
    end.

%% @doc Return the CRT values of the primes `P' and `Q' of `N', as a JWK gives
%% them, if `P' and `Q' multiply to `N'. Otherwise return none.
crt_values(P, Q, N, D) when P * Q =:= N, Q > 1 ->
    #{
        <<"p">> => binary:encode_unsigned(P),
        <<"q">> => binary:encode_unsigned(Q),
        <<"dp">> => binary:encode_unsigned(D rem (P - 1)),
        <<"dq">> => binary:encode_unsigned(D rem (Q - 1)),
        <<"qi">> => crypto:mod_pow(Q, P - 2, P)
    };
crt_values(_P, _Q, _N, _D) ->
    #{}.

%% @doc Find a prime of `N' (NIST SP 800-56B, appendix C), with
%% `e * d - 1 = R * 2^T' and `R' odd: square `G^R' modulo `N', for a random
%% `G', until it gives 1. If the value before 1 is not `N - 1', the gcd of `N'
%% and that value minus 1 is a prime. Try up to `Tries' values of `G'.
find_prime(_N, _RT, 0) ->
    not_found;
find_prime(N, {R, T}, Tries) ->
    G = rand:uniform(N - 3) + 1,
    case root_of_one(binary:decode_unsigned(crypto:mod_pow(G, R, N)), N, T) of
        {ok, Root} when Root =/= N - 1 -> {ok, gcd(Root - 1, N)};
        _ -> find_prime(N, {R, T}, Tries - 1)
    end.

%% @doc Square `X' modulo `N', up to `T' times, until it gives 1, and return the
%% value before 1. If `X' is 1, there is none.
root_of_one(1, _N, _T) -> not_found;
root_of_one(_X, _N, 0) -> not_found;
root_of_one(X, N, T) ->
    case X * X rem N of
        1 -> {ok, X};
        Square -> root_of_one(Square, N, T - 1)
    end.

%% @doc Split `K' into `{R, T}', with `K = R * 2^T' and `R' odd.
odd_part(K, T) when K band 1 =:= 0 -> odd_part(K bsr 1, T + 1);
odd_part(R, T) -> {R, T}.

%% @doc Return the greatest common divisor of two integers.
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).
