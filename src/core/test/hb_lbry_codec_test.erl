-module(hb_lbry_codec_test).
-include_lib("eunit/include/eunit.hrl").

stream_descriptor_codec_roundtrip_test() ->
    {Raw, SDHash} = sample_descriptor(),
    Descriptor =
        hb_message:convert(
            Raw,
            <<"structured@1.0">>,
            #{
                <<"device">> => <<"lbry-stream-descriptor@1.0">>,
                <<"sd-hash">> => SDHash
            },
            opts()
        ),
    ?assertEqual(<<"lbry-stream-descriptor@1.0">>, maps:get(<<"device">>, Descriptor)),
    ?assertEqual(SDHash, maps:get(<<"sd-hash">>, Descriptor)),
    ?assertEqual(Raw, roundtrip_raw(Descriptor, <<"lbry-stream-descriptor@1.0">>)).

stream_descriptor_codec_attaches_native_commitment_test() ->
    {Raw, SDHash} = sample_descriptor(),
    Descriptor =
        hb_message:convert(
            Raw,
            <<"structured@1.0">>,
            #{
                <<"device">> => <<"lbry-stream-descriptor@1.0">>,
                <<"sd-hash">> => SDHash
            },
            opts()
        ),
    Commitments = maps:get(<<"commitments">>, Descriptor),
    [Commitment] = maps:values(hb_cache:ensure_all_loaded(Commitments, opts())),
    ?assertEqual(
        <<"lbry-stream-descriptor@1.0">>,
        maps:get(<<"commitment-device">>, Commitment)
    ),
    ?assertEqual(SDHash, maps:get(<<"native-id">>, Commitment)),
    ?assertEqual(
        true,
        hb_message:verify(Descriptor, #{ <<"commitment-ids">> => <<"all">> }, opts())
    ).

stream_descriptor_verify_rejects_tampered_raw_test() ->
    {Raw, SDHash} = sample_descriptor(),
    Descriptor =
        hb_message:convert(
            Raw,
            <<"structured@1.0">>,
            #{
                <<"device">> => <<"lbry-stream-descriptor@1.0">>,
                <<"sd-hash">> => SDHash
            },
            opts()
        ),
    {OtherRaw, _} = other_descriptor(),
    Tampered = Descriptor#{ <<"raw">> => OtherRaw },
    ?assertEqual(
        false,
        hb_message:verify(Tampered, #{ <<"commitment-ids">> => <<"all">> }, opts())
    ).

stream_descriptor_verify_rejects_sd_hash_field_mismatch_test() ->
    {Raw, SDHash} = sample_descriptor(),
    Descriptor =
        hb_message:convert(
            Raw,
            <<"structured@1.0">>,
            #{
                <<"device">> => <<"lbry-stream-descriptor@1.0">>,
                <<"sd-hash">> => SDHash
            },
            opts()
        ),
    Tampered = Descriptor#{
        <<"sd-hash">> => hb_util:to_hex(crypto:hash(sha384, <<"other">>))
    },
    ?assertEqual(
        false,
        hb_message:verify(Tampered, #{ <<"commitment-ids">> => <<"all">> }, opts())
    ).

blob_codec_roundtrip_and_verify_test() ->
    Bytes = <<"encrypted blob payload">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Blob =
        hb_message:convert(
            Bytes,
            <<"structured@1.0">>,
            #{
                <<"device">> => <<"lbry-blob@1.0">>,
                <<"blob-hash">> => Hash
            },
            opts()
        ),
    ?assertEqual(<<"lbry-blob@1.0">>, maps:get(<<"device">>, Blob)),
    ?assertEqual(Hash, maps:get(<<"blob-hash">>, Blob)),
    ?assertEqual(Bytes, maps:get(<<"data">>, Blob)),
    ?assertEqual(Bytes, roundtrip_raw(Blob, <<"lbry-blob@1.0">>)),
    ?assertEqual(
        true,
        hb_message:verify(Blob, #{ <<"commitment-ids">> => <<"all">> }, opts())
    ).

blob_codec_rejects_hash_mismatch_test() ->
    Bytes = <<"encrypted blob payload">>,
    WrongHash = hb_lbry_stream_descriptor:blob_hash(<<"other payload">>),
    ?assertError(
        {case_clause, {error, {hash_mismatch, WrongHash, _}}},
        hb_message:convert(
            Bytes,
            <<"structured@1.0">>,
            #{
                <<"device">> => <<"lbry-blob@1.0">>,
                <<"blob-hash">> => WrongHash
            },
            opts()
        )
    ).

transaction_codec_roundtrip_test() ->
    RawTx = minimal_tx(),
    Tx =
        hb_message:convert(
            RawTx,
            <<"structured@1.0">>,
            <<"lbry-transaction@1.0">>,
            opts()
        ),
    ?assertEqual(<<"lbry-transaction@1.0">>, maps:get(<<"device">>, Tx)),
    ?assertEqual(RawTx, maps:get(<<"raw">>, Tx)),
    ?assertEqual(RawTx, roundtrip_raw(Tx, <<"lbry-transaction@1.0">>)),
    Hex =
        hb_message:convert(
            Tx,
            #{ <<"device">> => <<"lbry-transaction@1.0">>, <<"format">> => <<"hex">> },
            opts()
        ),
    ?assertEqual(hb_util:to_hex(RawTx), Hex),
    TxFromHex =
        hb_message:convert(
            Hex,
            <<"structured@1.0">>,
            #{ <<"device">> => <<"lbry-transaction@1.0">>, <<"encoding">> => <<"hex">> },
            opts()
        ),
    ?assertEqual(maps:get(<<"txid">>, Tx), maps:get(<<"txid">>, TxFromHex)).

claim_codec_roundtrip_test() ->
    ChannelHash = <<1:160>>,
    Signature = <<2:512>>,
    Message = <<"claim protobuf">>,
    Raw = <<1, ChannelHash/binary, Signature/binary, Message/binary>>,
    Claim =
        hb_message:convert(
            Raw,
            <<"structured@1.0">>,
            <<"lbry-claim@1.0">>,
            opts()
        ),
    ?assertEqual(<<"lbry-claim@1.0">>, maps:get(<<"device">>, Claim)),
    ?assertEqual(true, maps:get(<<"signed">>, Claim)),
    ?assertEqual(Raw, roundtrip_raw(Claim, <<"lbry-claim@1.0">>)),
    ?assertEqual(
        hb_util:to_hex(Raw),
        hb_message:convert(
            Claim,
            #{ <<"device">> => <<"lbry-claim@1.0">>, <<"format">> => <<"hex">> },
            opts()
        )
    ).

channel_codec_roundtrip_test() ->
    Channel = sample_channel(),
    Encoded =
        hb_message:convert(
            Channel,
            <<"structured@1.0">>,
            <<"lbry-channel@1.0">>,
            opts()
        ),
    ?assertEqual(<<"lbry-channel@1.0">>, maps:get(<<"device">>, Encoded)),
    ?assertEqual(maps:get(<<"claim_id">>, Channel), maps:get(<<"channel-id">>, Encoded)),
    ?assertEqual(Channel, roundtrip_raw(Encoded, <<"lbry-channel@1.0">>)).

stream_codec_roundtrip_test() ->
    SDHash = hb_util:to_hex(crypto:hash(sha384, <<"descriptor">>)),
    Stream = #{
        <<"claim_id">> => <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
        <<"name">> => <<"sample">>,
        <<"txid">> => <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
        <<"nout">> => 0,
        <<"value">> => #{ <<"source">> => #{ <<"sd_hash">> => SDHash } },
        <<"signing_channel">> => sample_channel()
    },
    Encoded =
        hb_message:convert(
            Stream,
            <<"structured@1.0">>,
            <<"lbry-stream@1.0">>,
            opts()
    ),
    ?assertEqual(<<"lbry-stream@1.0">>, maps:get(<<"device">>, Encoded)),
    ?assertNot(maps:is_key(<<"sd-hash">>, Encoded)),
    ?assertEqual(0, maps:get(<<"nout">>, Encoded)),
    ?assertEqual(Stream, roundtrip_raw(Encoded, <<"lbry-stream@1.0">>)).

stream_codec_prefers_signed_claim_sd_hash_test() ->
    {ok, Tx} = hb_lbry_tx:parse_hex(hb_lbry_tx:task0_tx_hex()),
    [ClaimOutput | _] = maps:get(<<"outputs">>, Tx),
    Envelope = maps:get(<<"claim-envelope">>, ClaimOutput),
    SignedSDHash = <<"3da16b833f169c21caeb62ca66111227413f30f63c9d2f52f2a787643e086c334ee6949e05875cfe94a816aba02e492e">>,
    Stream = #{
        <<"claim_id">> => <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
        <<"value">> => #{
            <<"source">> => #{
                <<"sd_hash">> => hb_util:to_hex(crypto:hash(sha384, <<"sdk">>))
            }
        },
        <<"claim-envelope">> => Envelope
    },
    Encoded =
        hb_message:convert(
            Stream,
            <<"structured@1.0">>,
            <<"lbry-stream@1.0">>,
            opts()
    ),
    ?assertEqual(SignedSDHash, maps:get(<<"sd-hash">>, Encoded)).

stream_codec_rejects_malformed_claim_envelope_test() ->
    Stream = #{
        <<"value">> => #{
            <<"source">> => #{
                <<"sd_hash">> => hb_util:to_hex(crypto:hash(sha384, <<"sdk">>))
            }
        },
        <<"claim-envelope">> => #{ <<"message">> => <<1, 2, 3>> }
    },
    ?assertError(
        {case_clause, {error, truncated_fixed64}},
        hb_message:convert(
            Stream,
            <<"structured@1.0">>,
            <<"lbry-stream@1.0">>,
            opts()
        )
    ).

channel_attestation_codec_test() ->
    Attestation = #{
        <<"valid">> => true,
        <<"signature-valid">> => true,
        <<"channel-hash-valid">> => true,
        <<"digest">> => hb_util:to_hex(crypto:hash(sha256, <<"digest">>))
    },
    Encoded =
        hb_message:convert(
            Attestation,
            <<"structured@1.0">>,
            <<"lbry-channel-attestation@1.0">>,
            opts()
        ),
    ?assertEqual(
        <<"lbry-channel-attestation@1.0">>,
        maps:get(<<"device">>, Encoded)
    ),
    ?assertEqual(true, maps:get(<<"valid">>, Encoded)).

roundtrip_raw(Msg, Device) ->
    hb_message:convert(
        Msg,
        #{ <<"device">> => Device, <<"format">> => <<"raw">> },
        opts()
    ).

sample_channel() ->
    PrivateKey = <<1:256>>,
    {PublicKey0, _} = crypto:generate_key(ecdh, secp256k1, PrivateKey),
    PublicKey = ar_wallet:compress_ecdsa_pubkey(PublicKey0),
    ChannelHash = <<3:160>>,
    #{
        <<"claim_id">> => hb_util:to_hex(reverse(ChannelHash)),
        <<"value">> => #{ <<"public_key">> => hb_util:to_hex(PublicKey) }
    }.

sample_descriptor() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31>>,
    Plaintext = <<"hello verified legacy stream">>,
    Ciphertext = crypto:crypto_one_time(
        aes_128_cbc,
        Key,
        IV,
        pkcs7_pad(Plaintext),
        true
    ),
    BlobHash = hb_lbry_stream_descriptor:blob_hash(Ciphertext),
    Descriptor =
        #{
            <<"stream_type">> => <<"lbryfile">>,
            <<"stream_name">> => hb_util:to_hex(<<"sample.mp4">>),
            <<"key">> => hb_util:to_hex(Key),
            <<"suggested_file_name">> => hb_util:to_hex(<<"sample.mp4">>),
            <<"stream_hash">> => hb_lbry_stream_descriptor:blob_hash(<<"stream hash test">>),
            <<"blobs">> => [
                #{
                    <<"length">> => byte_size(Ciphertext),
                    <<"blob_num">> => 0,
                    <<"iv">> => hb_util:to_hex(IV),
                    <<"blob_hash">> => BlobHash
                },
                #{
                    <<"length">> => 0,
                    <<"blob_num">> => 1,
                    <<"iv">> => hb_util:to_hex(<<0:128>>)
                }
            ]
        },
    Raw = hb_json:encode(Descriptor),
    {Raw, hb_lbry_stream_descriptor:descriptor_hash(Raw)}.

other_descriptor() ->
    {Raw, SDHash} = sample_descriptor(),
    JSON = hb_json:decode(Raw),
    OtherRaw = hb_json:encode(JSON#{ <<"stream_type">> => <<"other">> }),
    {OtherRaw, SDHash}.

minimal_tx() ->
    <<1:32/little-signed, 0, 0, 0:32/little>>.

pkcs7_pad(Plaintext) ->
    PadLen = 16 - (byte_size(Plaintext) rem 16),
    <<Plaintext/binary, (binary:copy(<<PadLen>>, PadLen))/binary>>.

reverse(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

opts() ->
    #{}.
