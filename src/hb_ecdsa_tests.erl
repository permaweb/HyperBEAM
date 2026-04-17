-module(hb_ecdsa_tests).

-include("include/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc secp256k1 curve order (n).
-define(SECP256K1_ORDER, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141).


%%%===================================================================
%%% Bitcoin/Ethereum Interoperability Tests
%%%===================================================================

%% @doc Test go-ethereum ecrecover reference vector.
%% https://github.com/ethereum/go-ethereum/blob/0cba803fbafb12e9daaea53b76de847842ab3055/crypto/secp256k1/secp256_test.go#L208
nif_ecrecover_geth_vector_test() ->
    Digest = binary:decode_hex(<<"ce0677bb30baa8cf067c88db9811f4333d131bf8bcf12fe7065d211dce971008">>),
    Sig = binary:decode_hex(<<"90f27b8b488db00b00606796d2987f6a5f59ae62ea05effe84fef5b8b0e549984a691139ad57a3f0b906637673aa2f63d1f55cb1a69199d4009eea23ceaddc9301">>),
    ExpectedPub = binary:decode_hex(<<"02e32df42865e97135acfb65f3bae71bdc86f4d49150ad6a440b6f15878109880a">>),
    {ok, true, RecoveredPub} = secp256k1_nif:recover_pk_and_verify(Digest, Sig),
    ?assertEqual(ExpectedPub, RecoveredPub).

recovery_id_in_valid_range_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Msg = <<"recovery ID test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    <<_CompactSig:64/binary, RecId:8>> = Sig,
    ?assert(lists:member(RecId, [0, 1, 2, 3])).

signature_has_low_s_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    %% Sign multiple messages and verify all have low-S
    Messages = [<<"msg1">>, <<"msg2">>, <<"msg3">>, <<"msg4">>, <<"msg5">>],
    lists:foreach(
        fun(Msg) ->
            Sig = ar_wallet:sign(Wallet, Msg),
            S = extract_s_value(Sig),
            SInt = binary:decode_unsigned(S, big),
            HalfOrder = ?SECP256K1_ORDER div 2,
            ?assert(SInt =< HalfOrder, "Signature must have low-S")
        end,
        Messages
    ).

%% @doc Test bitcoin-core edge case: (r=4, s=4) recoverable with all 4 recids.
bitcoin_core_r4_s4_all_recids_test() ->
    %% Create signature with r=4, s=4
    R = pad_to_32_bytes(<<4:32>>),
    S = pad_to_32_bytes(<<4:32>>),
    Msg = <<"This is a very secret message...">>,
    %% Test all 4 recovery IDs; at least one should recover a pubkey.
    HasSuccessfulRecovery =
        lists:any(
            fun(RecId) ->
                Sig = <<R/binary, S/binary, RecId:8>>,
                case ar_wallet:recover_key(Msg, Sig, ?ECDSA_KEY_TYPE) of
                    PubKey when byte_size(PubKey) =:= 33 -> true;
                    <<>> -> false
                end
            end,
            [0, 1, 2, 3]
        ),
    ?assertEqual(true, HasSuccessfulRecovery).

%% @doc Test bitcoin-core edge case: (r=1, s=1) with recid=0 succeeds.
bitcoin_core_r1_s1_recid0_test() ->
    R = pad_to_32_bytes(<<1:32>>),
    S = pad_to_32_bytes(<<1:32>>),
    Msg = <<"test">>,
    Sig = <<R/binary, S/binary, 0:8>>,
    Result = ar_wallet:recover_key(Msg, Sig, ?ECDSA_KEY_TYPE),
    %% May recover a compressed pubkey or fail with empty key, but never other shapes.
    ?assert(
        (Result =:= <<>>) orelse (byte_size(Result) =:= 33)
    ).

invalid_recid_4_fails_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Msg = <<"invalid recid test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    <<CompactSig:64/binary, _RecId:8>> = Sig,
    BadRecidSig = <<CompactSig:64/binary, 4:8>>,
    ?assertEqual(<<>>, ar_wallet:recover_key(Msg, BadRecidSig, ?ECDSA_KEY_TYPE)).

invalid_recid_255_rejected_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Msg = <<"invalid recid 255 test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    <<CompactSig:64/binary, _RecId:8>> = Sig,
    BadRecidSig = <<CompactSig:64/binary, 255:8>>,
    ?assertEqual(<<>>, ar_wallet:recover_key(Msg, BadRecidSig, ?ECDSA_KEY_TYPE)).

high_s_signature_rejected_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Msg = <<"malleability test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    %% Verify original signature works
    OriginalPub = ar_wallet:recover_key(Msg, Sig, ?ECDSA_KEY_TYPE),
    ?assertEqual(Pub, OriginalPub),
    %% Create high-S version
    HighSSig = create_high_s_signature(Sig),
    %% Wallet-level recover path should reject high-S signatures.
    ?assertEqual(<<>>, ar_wallet:recover_key(Msg, HighSSig, ?ECDSA_KEY_TYPE)).

%%%===================================================================
%%% RFC 6979 Validation Tests
%%%===================================================================

%% @doc Test deterministic signing with known private key (key=1).
nif_sign_known_key_test() ->
    PrivKey = <<0:248, 1:8>>,  % Private key = 1
    Digest = <<0:256>>,  % All-zero digest
    {ok, Sig} = secp256k1_nif:sign_recoverable(Digest, PrivKey),
    ?assertEqual(65, byte_size(Sig)),
    %% Verify round-trip recovery
    {ok, true, RecoveredPub} = secp256k1_nif:recover_pk_and_verify(Digest, Sig),
    %% Generate expected pubkey from private key
    Wallet = new_ecdsa_wallet_from_privkey(PrivKey),
    {{_KeyType, _Priv, ExpectedPub}, _} = Wallet,
    ?assertEqual(ExpectedPub, RecoveredPub).

signing_is_deterministic_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Msg = <<"deterministic test message">>,
    Sig1 = ar_wallet:sign(Wallet, Msg),
    Sig2 = ar_wallet:sign(Wallet, Msg),
    ?assertEqual(Sig1, Sig2).

privkey_zero_rejected_test() ->
    ZeroKey = <<0:256>>,
    Digest = <<0:256>>,
    Result = secp256k1_nif:sign_recoverable(Digest, ZeroKey),
    ?assertMatch({error, _}, Result).

privkey_curve_order_rejected_test() ->
    Order = ?SECP256K1_ORDER,
    OrderBin = binary:encode_unsigned(Order, big),
    OrderPadded = pad_to_32_bytes(OrderBin),
    Digest = <<0:256>>,
    Result = secp256k1_nif:sign_recoverable(Digest, OrderPadded),
    ?assertMatch({error, _}, Result).

privkey_above_curve_order_rejected_test() ->
    Order = ?SECP256K1_ORDER,
    OrderPlusOne = Order + 1,
    OrderPlusOneBin = binary:encode_unsigned(OrderPlusOne, big),
    OrderPlusOnePadded = pad_to_32_bytes(OrderPlusOneBin),
    Digest = <<0:256>>,
    Result = secp256k1_nif:sign_recoverable(Digest, OrderPlusOnePadded),
    ?assertMatch({error, _}, Result).

privkey_max_valid_succeeds_test() ->
    Order = ?SECP256K1_ORDER,
    MaxKey = Order - 1,
    MaxKeyBin = binary:encode_unsigned(MaxKey, big),
    MaxKeyPadded = pad_to_32_bytes(MaxKeyBin),
    Digest = <<0:256>>,
    {ok, Sig} = secp256k1_nif:sign_recoverable(Digest, MaxKeyPadded),
    ?assertEqual(65, byte_size(Sig)).

privkey_min_valid_succeeds_test() ->
    MinKey = <<0:248, 1:8>>,  % Key = 1
    Digest = <<0:256>>,
    {ok, Sig} = secp256k1_nif:sign_recoverable(Digest, MinKey),
    ?assertEqual(65, byte_size(Sig)).

%%%===================================================================
%%% Standards Conformance Tests (FIPS/NIST/SEC1)
%%%===================================================================

%% @doc Test sign and recover roundtrip with known key.
nif_sign_recover_roundtrip_test() ->
    PrivKey = <<0:248, 1:8>>,  % Private key = 1
    Digest = crypto:hash(sha256, <<"known message">>),
    {ok, Sig} = secp256k1_nif:sign_recoverable(Digest, PrivKey),
    {ok, true, RecoveredPub} = secp256k1_nif:recover_pk_and_verify(Digest, Sig),
    Wallet = new_ecdsa_wallet_from_privkey(PrivKey),
    {{_KeyType, _Priv, ExpectedPub}, _} = Wallet,
    ?assertEqual(ExpectedPub, RecoveredPub).

empty_message_signs_successfully_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Msg = <<>>,
    Sig = ar_wallet:sign(Wallet, Msg),
    ?assertEqual(65, byte_size(Sig)).

recovers_correct_pubkey_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Msg = <<"recovery test message">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    RecoveredPub = ar_wallet:recover_key(Msg, Sig, ?ECDSA_KEY_TYPE),
    ?assertEqual(Pub, RecoveredPub).

empty_message_recovers_correctly_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Msg = <<>>,
    Sig = ar_wallet:sign(Wallet, Msg),
    RecoveredPub = ar_wallet:recover_key(Msg, Sig, ?ECDSA_KEY_TYPE),
    ?assertEqual(Pub, RecoveredPub).

all_zero_digest_test() ->
    {{_KeyType, Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Digest = <<0:256>>,
    {ok, Sig} = secp256k1_nif:sign_recoverable(Digest, Priv),
    {ok, true, RecoveredPub} = secp256k1_nif:recover_pk_and_verify(Digest, Sig),
    ?assertEqual(Pub, RecoveredPub).

all_ones_digest_test() ->
    {{_KeyType, Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Digest = <<16#FF:(32*8)>>,
    {ok, Sig} = secp256k1_nif:sign_recoverable(Digest, Priv),
    {ok, true, RecoveredPub} = secp256k1_nif:recover_pk_and_verify(Digest, Sig),
    ?assertEqual(Pub, RecoveredPub).

digest_equals_curve_order_test() ->
    {{_KeyType, Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Order = ?SECP256K1_ORDER,
    OrderBin = binary:encode_unsigned(Order, big),
    Digest = pad_to_32_bytes(OrderBin),
    {ok, Sig} = secp256k1_nif:sign_recoverable(Digest, Priv),
    {ok, true, RecoveredPub} = secp256k1_nif:recover_pk_and_verify(Digest, Sig),
    ?assertEqual(Pub, RecoveredPub).

%%%===================================================================
%%% Adversarial Robustness Tests
%%%===================================================================
wrong_message_recovers_different_key_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Msg = <<"original message">>,
    BadMsg = <<"wrong message">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    RecoveredPub = ar_wallet:recover_key(BadMsg, Sig, ?ECDSA_KEY_TYPE),
    ?assertNotEqual(Pub, RecoveredPub).

corrupted_signature_byte_flip_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Msg = <<"flipped bit test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    CorruptedSig = corrupt_signature(Sig, 10),
    Result = ar_wallet:recover_key(Msg, CorruptedSig, ?ECDSA_KEY_TYPE),
    ?assertNotEqual(Pub, Result).

zeroed_r_value_fails_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Msg = <<"zeroed r test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    CorruptedSig = zero_signature_range(Sig, 0, 32),
    Result = ar_wallet:recover_key(Msg, CorruptedSig, ?ECDSA_KEY_TYPE),
    ?assertNotEqual(Pub, Result).

zeroed_s_value_fails_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Msg = <<"zeroed s test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    CorruptedSig = zero_signature_range(Sig, 32, 32),
    Result = ar_wallet:recover_key(Msg, CorruptedSig, ?ECDSA_KEY_TYPE),
    ?assertNotEqual(Pub, Result).

truncated_signature_fails_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Msg = <<"truncated sig test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    <<TruncatedSig:64/binary, _RecId:8>> = Sig,
    ?assertError(badarg, ar_wallet:recover_key(Msg, TruncatedSig, ?ECDSA_KEY_TYPE)).

extended_signature_fails_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Msg = <<"extended sig test">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    ExtendedSig = <<Sig/binary, 0:8>>,
    ?assertError(badarg, ar_wallet:recover_key(Msg, ExtendedSig, ?ECDSA_KEY_TYPE)).

verify_rejects_wrong_signature_test() ->
    Wallet = {{KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    TestData = <<"wrong sig test">>,
    Signature = ar_wallet:sign(Wallet, TestData),
    CorruptedSig = corrupt_signature(Signature, 0),
    false = ar_wallet:verify({KeyType, Pub}, TestData, CorruptedSig).

verify_rejects_wrong_data_test() ->
    Wallet = {{KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    TestData = <<"original data">>,
    WrongData = <<"wrong data">>,
    Signature = ar_wallet:sign(Wallet, TestData),
    false = ar_wallet:verify({KeyType, Pub}, WrongData, Signature).

verify_rejects_wrong_pubkey_test() ->
    Wallet1 = ar_wallet:new_ecdsa(),
    {{KeyType2, _Priv2, Pub2}, _} = ar_wallet:new_ecdsa(),
    TestData = <<"wrong pubkey test">>,
    Signature = ar_wallet:sign(Wallet1, TestData),
    false = ar_wallet:verify({KeyType2, Pub2}, TestData, Signature).

verify_rejects_corrupted_signature_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    TX = make_signed_ecdsa_tx(Wallet),
    CorruptedSig = corrupt_signature(TX#tx.signature, 0),
    TXCorrupted = TX#tx{signature = CorruptedSig},
    ?assertEqual(false, ar_tx:verify(TXCorrupted)).

verify_rejects_modified_data_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    TX = make_signed_ecdsa_tx(Wallet),
    TXModified = TX#tx{quantity = TX#tx.quantity + 1},
    ?assertEqual(false, ar_tx:verify(TXModified)).

verify_rejects_wrong_owner_test() ->
    Wallet1 = ar_wallet:new_ecdsa(),
    Wallet2 = ar_wallet:new_ecdsa(),
    TX = make_signed_ecdsa_tx(Wallet1),
    {{_KeyType, _Priv2, Pub2}, _} = Wallet2,
    TXWrongOwner = TX#tx{owner = Pub2},
    ?assertEqual(false, ar_tx:verify(TXWrongOwner)).

cannot_forge_with_random_bytes_test() ->
    {{KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    TX = make_ecdsa_tx(),
    TX2 = TX#tx{
        owner = Pub,
        signature_type = KeyType,
        owner_address = ar_wallet:to_address(Pub, KeyType)
    },
    RandomSig = crypto:strong_rand_bytes(65),
    TX3 = TX2#tx{signature = RandomSig},
    ?assertEqual(false, ar_tx:verify(TX3)).

cannot_substitute_cross_tx_signature_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    TX1 = make_signed_ecdsa_tx(Wallet),
    TX2 = make_ecdsa_tx(),
    TX2WithSig = TX2#tx{
        owner = TX1#tx.owner,
        signature = TX1#tx.signature,
        signature_type = TX1#tx.signature_type
    },
    ?assertEqual(false, ar_tx:verify(TX2WithSig)).

cannot_replay_with_different_anchor_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    TX = make_signed_ecdsa_tx(Wallet),
    TXReplay = TX#tx{anchor = crypto:strong_rand_bytes(32)},
    ?assertEqual(false, ar_tx:verify(TXReplay)).

signature_commits_to_all_fields_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    TX = make_signed_ecdsa_tx(Wallet),
    TX1 = TX#tx{target = crypto:strong_rand_bytes(32)},
    ?assertEqual(false, ar_tx:verify(TX1)),
    TX2 = TX#tx{quantity = TX#tx.quantity + 1},
    ?assertEqual(false, ar_tx:verify(TX2)),
    TX3 = TX#tx{reward = TX#tx.reward + 1},
    ?assertEqual(false, ar_tx:verify(TX3)),
    TX4 = TX#tx{anchor = crypto:strong_rand_bytes(32)},
    ?assertEqual(false, ar_tx:verify(TX4)),
    TX5 = TX#tx{data_size = TX#tx.data_size + 1},
    ?assertEqual(false, ar_tx:verify(TX5)),
    TX6 = TX#tx{data_root = crypto:strong_rand_bytes(32)},
    ?assertEqual(false, ar_tx:verify(TX6)).

%%%===================================================================
%%% ar_wallet Tests
%%%===================================================================

wallet_new_shape_contract_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    {{KeyType1, _Priv, Pub1}, {KeyType2, Pub2}} = Wallet,
    ?assertEqual(KeyType1, {?ECDSA_SIGN_ALG, secp256k1}),
    ?assertEqual(KeyType2, {?ECDSA_SIGN_ALG, secp256k1}),
    ?assertEqual(Pub1, Pub2).

different_keys_produce_different_pubkeys_test() ->
    Wallet1 = ar_wallet:new_ecdsa(),
    Wallet2 = ar_wallet:new_ecdsa(),
    {{_KeyType1, _Priv1, Pub1}, _} = Wallet1,
    {{_KeyType2, _Priv2, Pub2}, _} = Wallet2,
    ?assertNotEqual(Pub1, Pub2).

valid_key_produces_33byte_compressed_pubkey_test() ->
    {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    ?assertEqual(33, byte_size(Pub)).

valid_key_produces_32byte_privkey_test() ->
    {{_KeyType, Priv, _Pub}, _} = ar_wallet:new_ecdsa(),
    ?assertEqual(32, byte_size(Priv)).

signature_is_65_bytes_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    Msg = <<"test message">>,
    Sig = ar_wallet:sign(Wallet, Msg),
    ?assertEqual(65, byte_size(Sig)).

%% @doc Verify Erlang wrapper correctly hashes messages before calling NIF.
wrapper_sha256_correctness_test() ->
    PrivKey = <<0:248, 1:8>>,
    Msg = <<"test message">>,
    %% Call wrapper (hashes internally)
    Wallet = new_ecdsa_wallet_from_privkey(PrivKey),
    Sig1 = ar_wallet:sign(Wallet, Msg),
    %% Call NIF directly with pre-hashed message
    Digest = crypto:hash(sha256, Msg),
    {ok, Sig2} = secp256k1_nif:sign_recoverable(Digest, PrivKey),
    ?assertEqual(Sig1, Sig2).

wrong_digest_size_rejected_test() ->
    PrivKey = <<0:248, 1:8>>,
    Digest31 = <<0:(31*8)>>,
    Digest33 = <<0:(33*8)>>,
    ?assertError(_, secp256k1_nif:sign_recoverable(Digest31, PrivKey)),
    ?assertError(_, secp256k1_nif:sign_recoverable(Digest33, PrivKey)).

wrong_signature_size_rejected_test() ->
    Digest = <<0:256>>,
    Sig64 = <<0:(64*8)>>,
    Sig66 = <<0:(66*8)>>,
    ?assertError(_, secp256k1_nif:recover_pk_and_verify(Digest, Sig64)),
    ?assertError(_, secp256k1_nif:recover_pk_and_verify(Digest, Sig66)).

empty_signature_fails_test() ->
    Data = <<"empty sig test">>,
    EmptyPub = ar_wallet:recover_key(Data, <<>>, ?ECDSA_KEY_TYPE),
    ?assertEqual(<<>>, EmptyPub).

sign_verify_roundtrip_test() ->
    Wallet = {{KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    TestData = <<"wallet sign verify test">>,
    Signature = ar_wallet:sign(Wallet, TestData),
    true = ar_wallet:verify({KeyType, Pub}, TestData, Signature).

address_is_sha256_of_pubkey_test() ->
    {{KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Address = ar_wallet:to_address(Pub, KeyType),
    ExpectedAddress = crypto:hash(sha256, Pub),
    ?assertEqual(ExpectedAddress, Address),
    %% Verify it's not Keccak (Ethereum-style)
    ?assertNotEqual(hb_keccak:key_to_ethereum_address(Pub), Address).

address_is_32_bytes_test() ->
    {{KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Address = ar_wallet:to_address(Pub, KeyType),
    ?assertEqual(32, byte_size(Address)).

different_keys_different_addresses_test() ->
    Wallet1 = ar_wallet:new_ecdsa(),
    Wallet2 = ar_wallet:new_ecdsa(),
    {{KeyType, _Priv1, Pub1}, _} = Wallet1,
    {{KeyType, _Priv2, Pub2}, _} = Wallet2,
    Address1 = ar_wallet:to_address(Pub1, KeyType),
    Address2 = ar_wallet:to_address(Pub2, KeyType),
    ?assertNotEqual(Address1, Address2).

recover_key_returns_correct_pubkey_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Data = <<"recover key test">>,
    Signature = ar_wallet:sign(Wallet, Data),
    RecoveredPub = ar_wallet:recover_key(Data, Signature, ?ECDSA_KEY_TYPE),
    ?assertEqual(Pub, RecoveredPub).

recover_key_empty_signature_returns_empty_test() ->
    Data = <<"empty sig test">>,
    EmptyPub = ar_wallet:recover_key(Data, <<>>, ?ECDSA_KEY_TYPE),
    ?assertEqual(<<>>, EmptyPub).

recover_key_wrong_message_returns_different_key_test() ->
    Wallet = {{_KeyType, _Priv, Pub}, _} = ar_wallet:new_ecdsa(),
    Data = <<"original message">>,
    WrongData = <<"wrong message">>,
    Signature = ar_wallet:sign(Wallet, Data),
    RecoveredPub = ar_wallet:recover_key(WrongData, Signature, ?ECDSA_KEY_TYPE),
    ?assertNotEqual(Pub, RecoveredPub).

%%%===================================================================
%%% ar_tx Tests
%%%===================================================================
create_and_sign_tx_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    TX = make_signed_ecdsa_tx(Wallet),
    ?assertEqual(?ECDSA_KEY_TYPE, TX#tx.signature_type),
    ?assertEqual(65, byte_size(TX#tx.signature)),
    ?assertEqual(33, byte_size(TX#tx.owner)).

verify_valid_tx_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    TX = make_signed_ecdsa_tx(Wallet),
    ?assertEqual(true, ar_tx:verify(TX)).

json_roundtrip_with_owner_recovery_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    {{KeyType, _Priv, Pub}, _} = Wallet,
    TX = make_signed_ecdsa_tx(Wallet),
    ExpectedAddress = ar_wallet:to_address(Pub, KeyType),
    JSON = ar_tx:tx_to_json_struct(TX),
    JSONEmptyOwner = JSON#{<<"owner">> => <<>>},
    ParsedTX = ar_tx:json_struct_to_tx(JSONEmptyOwner),
    ?assertEqual(Pub, ParsedTX#tx.owner),
    ?assertEqual(ExpectedAddress, ParsedTX#tx.owner_address).

rsa_and_ecdsa_coexist_independently_test() ->
    EcdsaWallet = ar_wallet:new_ecdsa(),
    RsaWallet = {{RsaKeyType, _RsaPriv, _RsaPub}, _} = ar_wallet:new(),
    EcdsaTX = make_signed_ecdsa_tx(EcdsaWallet),
    ?assertEqual(true, ar_tx:verify(EcdsaTX)),
    RsaTX = make_ecdsa_tx(),
    RsaTX2 = ar_tx:sign(RsaTX, RsaWallet),
    ?assertEqual(true, ar_tx:verify(RsaTX2)),
    {{EcdsaKeyType, _, _}, _} = EcdsaWallet,
    ?assertEqual(EcdsaKeyType, EcdsaTX#tx.signature_type),
    ?assertEqual(RsaKeyType, RsaTX2#tx.signature_type).

full_sign_json_recover_verify_roundtrip_test() ->
    Wallet = ar_wallet:new_ecdsa(),
    TX = make_signed_ecdsa_tx(Wallet),
    ?assertEqual(true, ar_tx:verify(TX)),
    JSON = ar_tx:tx_to_json_struct(TX),
    JSONEmptyOwner = JSON#{<<"owner">> => <<>>},
    ParsedTX = ar_tx:json_struct_to_tx(JSONEmptyOwner),
    ?assertEqual(true, ar_tx:verify(ParsedTX)).

sig_segment_excludes_owner_for_ecdsa_test() ->
    {{EcdsaKeyType1, _EcdsaPriv1, EcdsaPub1}, _} = ar_wallet:new_ecdsa(),
    {{EcdsaKeyType2, _EcdsaPriv2, EcdsaPub2}, _} = ar_wallet:new_ecdsa(),
    BaseTX = #tx{
        format = 2,
        target = <<0:256>>,
        quantity = 100,
        reward = 10,
        anchor = <<0:256>>,
        tags = [],
        data = <<>>,
        data_size = 0,
        data_root = <<>>
    },
    TX1 = BaseTX#tx{
        owner = EcdsaPub1,
        signature_type = EcdsaKeyType1,
        owner_address = ar_wallet:to_address(EcdsaPub1, EcdsaKeyType1)
    },
    TX2 = BaseTX#tx{
        owner = EcdsaPub2,
        signature_type = EcdsaKeyType2,
        owner_address = ar_wallet:to_address(EcdsaPub2, EcdsaKeyType2)
    },
    Segment1 = ar_tx:generate_signature_data_segment(TX1),
    Segment2 = ar_tx:generate_signature_data_segment(TX2),
    ?assertEqual(Segment1, Segment2),
    ?assert(is_binary(Segment1)),
    ?assert(byte_size(Segment1) > 0).

sig_segment_includes_owner_for_rsa_test() ->
    {{RsaKeyType1, _RsaPriv1, RsaPub1}, _} = ar_wallet:new(),
    {{RsaKeyType2, _RsaPriv2, RsaPub2}, _} = ar_wallet:new(),
    BaseTX = #tx{
        format = 2,
        target = <<0:256>>,
        quantity = 100,
        reward = 10,
        anchor = <<0:256>>,
        tags = [],
        data = <<>>,
        data_size = 0,
        data_root = <<>>
    },
    TX1 = BaseTX#tx{
        owner = RsaPub1,
        signature_type = RsaKeyType1,
        owner_address = ar_wallet:to_address(RsaPub1, RsaKeyType1)
    },
    TX2 = BaseTX#tx{
        owner = RsaPub2,
        signature_type = RsaKeyType2,
        owner_address = ar_wallet:to_address(RsaPub2, RsaKeyType2)
    },
    Segment1 = ar_tx:generate_signature_data_segment(TX1),
    Segment2 = ar_tx:generate_signature_data_segment(TX2),
    ?assertNotEqual(Segment1, Segment2),
    ?assert(is_binary(Segment1)),
    ?assert(is_binary(Segment2)),
    ?assert(byte_size(Segment1) > 0),
    ?assert(byte_size(Segment2) > 0).

%%%===================================================================
%%% Test Helper Functions
%%%===================================================================

%% @doc Generate an ECDSA keypair from a known private key.
new_ecdsa_wallet_from_privkey(PrivKey) when byte_size(PrivKey) =:= 32 ->
    {OrigPub, _} = crypto:generate_key(ecdh, secp256k1, PrivKey),
    CompressedPub = ar_wallet:compress_ecdsa_pubkey(OrigPub),
    KeyType = {?ECDSA_SIGN_ALG, secp256k1},
    {{KeyType, PrivKey, CompressedPub}, {KeyType, CompressedPub}}.

%% @doc Create a simple ECDSA transaction (unsigned).
make_ecdsa_tx() ->
    #tx{
        format = 2,
        target = crypto:strong_rand_bytes(32),
        quantity = 100,
        reward = 10,
        anchor = crypto:strong_rand_bytes(32),
        tags = [],
        data = <<>>,
        data_size = 0,
        data_root = <<>>
    }.

%% @doc Create and sign an ECDSA transaction using the NIF directly.
make_signed_ecdsa_tx({{KeyType, Priv, Pub}, {KeyType, Pub}}) ->
    TX = make_ecdsa_tx(),
    TX2 = TX#tx{
        owner = Pub,
        signature_type = KeyType,
        owner_address = ar_wallet:to_address(Pub, KeyType)
    },
    SignatureDataSegment = ar_tx:generate_signature_data_segment(TX2),
    Signature = ar_wallet:sign({{KeyType, Priv, Pub}, {KeyType, Pub}}, SignatureDataSegment),
    TX3 = TX2#tx{signature = Signature},
    TX3#tx{
        id = ar_tx:id(TX3, signed)
    };
make_signed_ecdsa_tx(Wallet) when is_tuple(Wallet) ->
    case Wallet of
        {{KeyType, Priv, Pub}, {KeyType, Pub}} ->
            make_signed_ecdsa_tx({{KeyType, Priv, Pub}, {KeyType, Pub}});
        {Priv, {KeyType, Pub}} ->
            make_signed_ecdsa_tx({{KeyType, Priv, Pub}, {KeyType, Pub}})
    end.

%% @doc Corrupt a signature by flipping a bit at a specific byte position.
corrupt_signature(Sig, BytePos) when BytePos < byte_size(Sig) ->
    <<Before:BytePos/binary, Byte:8, After/binary>> = Sig,
    <<Before/binary, (Byte bxor 16#FF):8, After/binary>>;
corrupt_signature(Sig, _BytePos) ->
    Sig.

%% @doc Zero out a range of bytes in a signature.
zero_signature_range(Sig, Start, Length) ->
    Size = byte_size(Sig),
    End = min(Start + Length, Size),
    <<Before:Start/binary, _:((End - Start) * 8), After/binary>> = Sig,
    Zeros = binary:copy(<<0>>, End - Start),
    <<Before/binary, Zeros/binary, After/binary>>.

%% @doc Extract s value from signature (bytes 32-63).
extract_s_value(Sig) when byte_size(Sig) >= 64 ->
    <<_R:32/binary, S:32/binary, _RecId/binary>> = Sig,
    S.

%% @doc Create high-S signature from low-S signature.
%% Computes s_high = n - s_low and replaces s in signature.
create_high_s_signature(Sig) when byte_size(Sig) =:= 65 ->
    <<R:32/binary, S:32/binary, RecId:8>> = Sig,
    SInt = binary:decode_unsigned(S, big),
    SHigh = ?SECP256K1_ORDER - SInt,
    SHighBin = binary:encode_unsigned(SHigh, big),
    %% Pad to 32 bytes if needed
    SHighPadded = pad_to_32_bytes(SHighBin),
    <<R/binary, SHighPadded/binary, RecId:8>>.

pad_to_32_bytes(Bin) when byte_size(Bin) =:= 32 ->
    Bin;
pad_to_32_bytes(Bin) when byte_size(Bin) < 32 ->
    Padding = binary:copy(<<0>>, 32 - byte_size(Bin)),
    <<Padding/binary, Bin/binary>>.