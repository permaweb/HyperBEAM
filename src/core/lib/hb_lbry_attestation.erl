-module(hb_lbry_attestation).
-export([
    verify/3,
    signature_digest/2,
    verify_signature/3,
    channel_hash/1,
    channel_public_key/1,
    normalize_public_key/1,
    public_key_to_uncompressed/1
]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SECP256K1_P,
    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F).
-define(SECP256K1_N,
    16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141).
%% DER/SPKI prefixes for secp256k1 public keys, as stored on-chain by legacy
%% channel claims. Matched byte-exactly: any other algorithm identifier or
%% curve OID is rejected.
-define(SPKI_UNCOMPRESSED_PREFIX,
    <<16#30, 16#56, 16#30, 16#10, 16#06, 16#07, 16#2a, 16#86, 16#48, 16#ce,
        16#3d, 16#02, 16#01, 16#06, 16#05, 16#2b, 16#81, 16#04, 16#00, 16#0a,
        16#03, 16#42, 16#00>>).
-define(SPKI_COMPRESSED_PREFIX,
    <<16#30, 16#36, 16#30, 16#10, 16#06, 16#07, 16#2a, 16#86, 16#48, 16#ce,
        16#3d, 16#02, 16#01, 16#06, 16#05, 16#2b, 16#81, 16#04, 16#00, 16#0a,
        16#03, 16#22, 16#00>>).

verify(
    #{ <<"inputs">> := [FirstInput | _] },
    #{ <<"claim-envelope">> := Envelope },
    Channel
) ->
    case maps:get(<<"signed">>, Envelope, false) of
        true ->
            verify_signed(FirstInput, Envelope, Channel);
        false ->
            {error, unsigned_claim}
    end.

signature_digest(FirstInput, Envelope) ->
    Piece1 = maps:get(<<"signature-digest-piece">>, FirstInput),
    ChannelHash = maps:get(<<"signing-channel-hash">>, Envelope),
    Message = maps:get(<<"message">>, Envelope),
    crypto:hash(sha256, <<Piece1/binary, ChannelHash/binary, Message/binary>>).

verify_signature(<<R:256, S:256>>, <<Digest:32/binary>>, PublicKey) ->
    verify_signature_ints(R, S, Digest, PublicKey);
verify_signature(_, _, _) ->
    {error, invalid_signature}.

channel_hash(Channel) ->
    case channel_claim_id(Channel) of
        {ok, ClaimID} ->
            try binary:decode_hex(hb_util:to_lower(ClaimID)) of
                Hash when byte_size(Hash) == 20 -> {ok, reverse(Hash)};
                _ -> {error, invalid_channel_claim_id}
            catch
                _:_ -> {error, invalid_channel_claim_id}
            end;
        Error ->
            Error
    end.

channel_public_key(Channel) ->
    case first_present([
        [<<"value">>, <<"public_key">>],
        [<<"value">>, <<"channel">>, <<"public_key">>],
        [<<"public_key">>]
    ], Channel) of
        {ok, Hex} when is_binary(Hex) ->
            try binary:decode_hex(hb_util:to_lower(Hex)) of
                <<Prefix, _/binary>> = PublicKey when
                        byte_size(PublicKey) == 33,
                        (Prefix == 2 orelse Prefix == 3) ->
                    {ok, PublicKey};
                PublicKey ->
                    {error, {unsupported_public_key, byte_size(PublicKey)}}
            catch
                _:_ -> {error, invalid_public_key}
            end;
        _ ->
            {error, missing_public_key}
    end.

%% @doc Normalize an on-chain channel public key to the 33-byte compressed
%% secp256k1 form. Accepts bare compressed and uncompressed points as well as
%% the DER/SPKI encodings used by legacy channel claims. Off-curve points and
%% unrecognized encodings fail closed.
normalize_public_key(<<Prefix, X:256>> = PublicKey) when Prefix == 2; Prefix == 3 ->
    case decode_public_key(PublicKey) of
        {ok, {X, _Y}} -> {ok, PublicKey};
        _ -> {error, invalid_channel_public_key}
    end;
normalize_public_key(<<4, X:256, Y:256>>) ->
    case valid_point(X, Y) of
        true -> {ok, <<(2 + (Y band 1)), X:256>>};
        false -> {error, invalid_channel_public_key}
    end;
normalize_public_key(<<Prefix:23/binary, Point:65/binary>>)
        when Prefix == ?SPKI_UNCOMPRESSED_PREFIX ->
    normalize_public_key(Point);
normalize_public_key(<<Prefix:23/binary, Point:33/binary>>)
        when Prefix == ?SPKI_COMPRESSED_PREFIX ->
    normalize_public_key(Point);
normalize_public_key(_) ->
    {error, unsupported_channel_public_key}.

verify_signed(FirstInput, Envelope, Channel) ->
    maybe
        {ok, ChannelHash} ?= channel_hash(Channel),
        EmbeddedHash = maps:get(<<"signing-channel-hash">>, Envelope),
        {ok, PublicKey} ?= channel_public_key(Channel),
        Digest = signature_digest(FirstInput, Envelope),
        Signature = maps:get(<<"claim-signature">>, Envelope),
        {ok, SignatureValid} ?= verify_signature(Signature, Digest, PublicKey),
        BindingValid = ChannelHash == EmbeddedHash,
        Attestation = #{
            <<"device">> => <<"lbry-channel-attestation@1.0">>,
            <<"tier">> => 2,
            <<"valid">> => SignatureValid andalso BindingValid,
            <<"signature-valid">> => SignatureValid,
            <<"channel-hash-valid">> => BindingValid,
            <<"digest">> => hb_util:to_hex(Digest),
            <<"signing-channel-id">> => hb_util:to_hex(reverse(EmbeddedHash)),
            <<"channel-id">> => hb_util:to_hex(reverse(ChannelHash)),
            <<"public-key">> => hb_util:to_hex(PublicKey)
        },
        ?event(lbry_attestation,
            {attestation_result,
                {valid, maps:get(<<"valid">>, Attestation)},
                {signature_valid, SignatureValid},
                {channel_hash_valid, BindingValid},
                {channel_id, maps:get(<<"channel-id">>, Attestation)}}
        ),
        {ok, Attestation}
    end.

verify_signature_ints(R, S, Digest, PublicKey) when
        R > 0, R < ?SECP256K1_N, S > 0, S < ?SECP256K1_N ->
    maybe
        {ok, UncompressedPublicKey} ?= public_key_to_uncompressed(PublicKey),
        Signature = compact_to_der(R, S),
        {ok, crypto_verify(Digest, Signature, UncompressedPublicKey)}
    end;
verify_signature_ints(_, _, _, _) ->
    {ok, false}.

crypto_verify(Digest, Signature, PublicKey) ->
    try crypto:verify(
        ecdsa,
        sha256,
        {digest, Digest},
        Signature,
        [PublicKey, secp256k1]
    ) of
        Result -> Result
    catch
        _:_ -> false
    end.

compact_to_der(R, S) ->
    RBin = der_int(R),
    SBin = der_int(S),
    <<16#30,
        (4 + byte_size(RBin) + byte_size(SBin)),
        16#02,
        (byte_size(RBin)),
        RBin/binary,
        16#02,
        (byte_size(SBin)),
        SBin/binary>>.

der_int(Int) ->
    Raw = binary:encode_unsigned(Int),
    case Raw of
        <<First, _/binary>> when First band 16#80 =/= 0 ->
            <<0, Raw/binary>>;
        _ ->
            Raw
    end.

public_key_to_uncompressed(<<4, X:256, Y:256>> = PublicKey) ->
    case valid_point(X, Y) of
        true -> {ok, PublicKey};
        false -> {error, invalid_public_key}
    end;
public_key_to_uncompressed(PublicKey) ->
    case decode_public_key(PublicKey) of
        {ok, {X, Y}} -> {ok, <<4, X:256, Y:256>>};
        Error -> Error
    end.

%% Coordinates must be canonical field elements: encodings with `X >= p' or
%% `Y >= p' are invalid even when they satisfy the curve equation modulo `p'.
decode_public_key(<<Prefix, X:256>>)
        when (Prefix == 2 orelse Prefix == 3), X < ?SECP256K1_P ->
    Y2 = mod(pow_mod(X, 3, ?SECP256K1_P) + 7, ?SECP256K1_P),
    Y0 = pow_mod(Y2, (?SECP256K1_P + 1) div 4, ?SECP256K1_P),
    Y =
        case Y0 rem 2 of
            Parity when Parity == Prefix band 1 -> Y0;
            _ -> ?SECP256K1_P - Y0
        end,
    case mod(Y * Y, ?SECP256K1_P) of
        Y2 -> {ok, {X, Y}};
        _ -> {error, invalid_public_key}
    end;
decode_public_key(_) ->
    {error, invalid_public_key}.

valid_point(X, Y) when X < ?SECP256K1_P, Y < ?SECP256K1_P ->
    Y2 = mod(pow_mod(X, 3, ?SECP256K1_P) + 7, ?SECP256K1_P),
    mod(Y * Y, ?SECP256K1_P) == Y2;
valid_point(_, _) ->
    false.

pow_mod(_Base, 0, Modulus) ->
    1 rem Modulus;
pow_mod(Base, Exponent, Modulus) ->
    pow_mod(mod(Base, Modulus), Exponent, Modulus, 1).

pow_mod(_Base, 0, _Modulus, Acc) ->
    Acc;
pow_mod(Base, Exponent, Modulus, Acc) ->
    NextAcc =
        case Exponent band 1 of
            1 -> mod(Acc * Base, Modulus);
            0 -> Acc
        end,
    pow_mod(mod(Base * Base, Modulus), Exponent bsr 1, Modulus, NextAcc).

mod(Value, Modulus) ->
    Result = Value rem Modulus,
    case Result < 0 of
        true -> Result + Modulus;
        false -> Result
    end.

channel_claim_id(Channel) ->
    case first_present([[<<"claim_id">>], [<<"channel_id">>]], Channel) of
        {ok, ClaimID} when is_binary(ClaimID), byte_size(ClaimID) == 40 ->
            {ok, hb_util:to_lower(ClaimID)};
        _ ->
            {error, missing_channel_claim_id}
    end.

first_present([], _Map) ->
    not_found;
first_present([Path | Rest], Map) ->
    case hb_util:deep_get(Path, Map, #{}) of
        not_found -> first_present(Rest, Map);
        Value -> {ok, Value}
    end.

reverse(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

normalize_public_key_test() ->
    PrivateKey = <<1:256>>,
    {Uncompressed, _} = crypto:generate_key(ecdh, secp256k1, PrivateKey),
    Compressed = ar_wallet:compress_ecdsa_pubkey(Uncompressed),
    ?assertEqual({ok, Compressed}, normalize_public_key(Compressed)),
    ?assertEqual({ok, Compressed}, normalize_public_key(Uncompressed)),
    ?assertEqual(
        {ok, Compressed},
        normalize_public_key(<<?SPKI_UNCOMPRESSED_PREFIX/binary, Uncompressed/binary>>)
    ),
    ?assertEqual(
        {ok, Compressed},
        normalize_public_key(<<?SPKI_COMPRESSED_PREFIX/binary, Compressed/binary>>)
    ).

normalize_public_key_rejects_other_curves_test() ->
    PrivateKey = <<1:256>>,
    {Uncompressed, _} = crypto:generate_key(ecdh, secp256k1, PrivateKey),
    P256Prefix =
        binary:decode_hex(
            <<"3059301306072a8648ce3d020106082a8648ce3d030107034200">>
        ),
    ?assertEqual(
        {error, unsupported_channel_public_key},
        normalize_public_key(<<P256Prefix/binary, Uncompressed/binary>>)
    ).

normalize_public_key_rejects_non_canonical_coordinates_test() ->
    ?assertEqual(
        {error, invalid_channel_public_key},
        normalize_public_key(<<2, (?SECP256K1_P + 1):256>>)
    ),
    ?assertEqual(
        {error, invalid_channel_public_key},
        normalize_public_key(<<4, 1:256, ?SECP256K1_P:256>>)
    ).

normalize_public_key_rejects_off_curve_points_test() ->
    % x = 0 is not on secp256k1: 7 is not a quadratic residue mod p.
    ?assertEqual(
        {error, invalid_channel_public_key},
        normalize_public_key(<<2, 0:256>>)
    ),
    ?assertEqual(
        {error, invalid_channel_public_key},
        normalize_public_key(<<4, 0:256, 1:256>>)
    ),
    ?assertEqual(
        {error, unsupported_channel_public_key},
        normalize_public_key(<<1, 2, 3>>)
    ).

verify_signature_accepts_compact_secp256k1_test() ->
    PrivateKey = <<1:256>>,
    {PublicKey0, _} = crypto:generate_key(ecdh, secp256k1, PrivateKey),
    PublicKey = ar_wallet:compress_ecdsa_pubkey(PublicKey0),
    Message = <<"lbry attestation">>,
    Digest = crypto:hash(sha256, Message),
    CompactSignature = der_to_compact(
        crypto:sign(ecdsa, sha256, Message, [PrivateKey, secp256k1])
    ),
    ?assertEqual({ok, true}, verify_signature(CompactSignature, Digest, PublicKey)),
    <<R:32/binary, S:256>> = CompactSignature,
    HighS = ?SECP256K1_N - S,
    ?assertEqual({ok, true}, verify_signature(<<R/binary, HighS:256>>, Digest, PublicKey)).

verify_signature_rejects_tampered_digest_test() ->
    PrivateKey = <<1:256>>,
    {PublicKey0, _} = crypto:generate_key(ecdh, secp256k1, PrivateKey),
    PublicKey = ar_wallet:compress_ecdsa_pubkey(PublicKey0),
    Message = <<"lbry attestation">>,
    CompactSignature = der_to_compact(
        crypto:sign(ecdsa, sha256, Message, [PrivateKey, secp256k1])
    ),
    BadDigest = crypto:hash(sha256, <<"tampered">>),
    ?assertEqual({ok, false}, verify_signature(CompactSignature, BadDigest, PublicKey)).

verify_checks_channel_hash_binding_test() ->
    PrivateKey = <<1:256>>,
    {PublicKey0, _} = crypto:generate_key(ecdh, secp256k1, PrivateKey),
    PublicKey = ar_wallet:compress_ecdsa_pubkey(PublicKey0),
    ChannelHash = <<1:160>>,
    FirstInput = #{ <<"signature-digest-piece">> => <<2:288>> },
    Message = <<"claim protobuf">>,
    Payload =
        <<(maps:get(<<"signature-digest-piece">>, FirstInput))/binary,
            ChannelHash/binary,
            Message/binary>>,
    Signature = der_to_compact(
        crypto:sign(ecdsa, sha256, Payload, [PrivateKey, secp256k1])
    ),
    Envelope = #{
        <<"signed">> => true,
        <<"signing-channel-hash">> => ChannelHash,
        <<"claim-signature">> => Signature,
        <<"message">> => Message
    },
    Channel = #{
        <<"claim_id">> => hb_util:to_hex(reverse(ChannelHash)),
        <<"value">> => #{ <<"public_key">> => hb_util:to_hex(PublicKey) }
    },
    {ok, Attestation} = verify(#{ <<"inputs">> => [FirstInput] }, #{ <<"claim-envelope">> => Envelope }, Channel),
    ?assertEqual(true, maps:get(<<"valid">>, Attestation)).

real_lbry_signature_verifies_task0_test() ->
    {ok, Tx} = hb_lbry_tx:parse_hex(hb_lbry_tx:task0_tx_hex()),
    [FirstInput] = maps:get(<<"inputs">>, Tx),
    [ClaimOutput | _] = maps:get(<<"outputs">>, Tx),
    Envelope = maps:get(<<"claim-envelope">>, ClaimOutput),
    Channel = #{
        <<"claim_id">> => <<"585d54c7b82fd92043ed583c5aea18a9547028aa">>,
        <<"value">> => #{
            <<"public_key">> =>
                <<"03fa4e5fe9f02f2f1a8c34ec150b91f762d8b07b7be942f26aa80c40902d5dbd11">>
        }
    },
    Digest = signature_digest(FirstInput, Envelope),
    Signature = maps:get(<<"claim-signature">>, Envelope),
    {ok, PublicKey} = channel_public_key(Channel),
    ?assertEqual(
        {ok, true},
        verify_signature(Signature, Digest, PublicKey)
    ),
    {ok, Attestation} = verify(Tx, ClaimOutput, Channel),
    ?assertEqual(true, maps:get(<<"valid">>, Attestation)),
    ?assertEqual(true, maps:get(<<"signature-valid">>, Attestation)),
    ?assertEqual(true, maps:get(<<"channel-hash-valid">>, Attestation)).

der_to_compact(<<16#30, _TotalLen, 16#02, RLen, R0:RLen/binary, 16#02, SLen, S0:SLen/binary>>) ->
    <<(fixed_int(R0))/binary, (fixed_int(S0))/binary>>.

fixed_int(Int) ->
    Trimmed = trim_zeroes(Int),
    Padding = 32 - byte_size(Trimmed),
    <<0:(Padding * 8), Trimmed/binary>>.

trim_zeroes(<<0, Rest/binary>> = Int) when byte_size(Int) > 32 ->
    trim_zeroes(Rest);
trim_zeroes(Int) ->
    Int.
