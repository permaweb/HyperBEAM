-module(hb_lbry_native_object_test).
-include_lib("eunit/include/eunit.hrl").

claim_codec_roundtrip_test() ->
    Raw = unsigned_claim_bytes(<<"claim protobuf">>),
    Claim = hb_message:convert(Raw, <<"structured@1.0">>, <<"lbry-claim@1.0">>, #{}),
    ?assertEqual(<<"lbry-claim@1.0">>, maps:get(<<"device">>, Claim)),
    ?assertEqual(
        Raw,
        hb_message:convert(
            Claim,
            #{ <<"device">> => <<"lbry-claim@1.0">>, <<"format">> => <<"raw">> },
            #{}
        )
    ).

channel_codec_roundtrip_test() ->
    Channel = sample_channel_map(),
    Encoded =
        hb_message:convert(
            Channel,
            <<"structured@1.0">>,
            <<"lbry-channel@1.0">>,
            #{}
        ),
    ?assertEqual(<<"lbry-channel@1.0">>, maps:get(<<"device">>, Encoded)),
    ?assertEqual(
        Channel,
        hb_message:convert(
            Encoded,
            #{ <<"device">> => <<"lbry-channel@1.0">>, <<"format">> => <<"raw">> },
            #{}
        )
    ).

stream_codec_roundtrip_test() ->
    Stream = sample_stream_map(),
    Encoded =
        hb_message:convert(
            Stream,
            <<"structured@1.0">>,
            <<"lbry-stream@1.0">>,
            #{}
        ),
    ?assertEqual(<<"lbry-stream@1.0">>, maps:get(<<"device">>, Encoded)),
    ?assertEqual(
        Stream,
        hb_message:convert(
            Encoded,
            #{ <<"device">> => <<"lbry-stream@1.0">>, <<"format">> => <<"raw">> },
            #{}
        )
    ).

claim_message_verifies_test() ->
    {RawTx, TxID, ClaimID} =
        proof_tx_fixture(<<"example">>, unsigned_claim_bytes(<<"claim protobuf">>)),
    {ok, Msg} = hb_lbry_commitment:claim_output_message(RawTx, 0),
    ?assertEqual(<<"lbry-claim@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(ClaimID, maps:get(<<"claim-id">>, Msg)),
    ?assertEqual(TxID, maps:get(<<"txid">>, Msg)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

channel_message_verifies_test() ->
    {PublicKey, _Channel} = channel_key_fixture(),
    {RawTx, _TxID, ClaimID} =
        proof_tx_fixture(<<"@example">>, channel_claim_bytes(PublicKey)),
    {ok, Msg} = hb_lbry_commitment:channel_output_message(RawTx, 0),
    ?assertEqual(<<"lbry-channel@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(ClaimID, maps:get(<<"claim-id">>, Msg)),
    ?assertEqual(ClaimID, maps:get(<<"channel-id">>, Msg)),
    ?assertEqual(hb_util:to_hex(PublicKey), maps:get(<<"public-key">>, Msg)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

stream_message_verifies_test() ->
    SDHashBytes = crypto:hash(sha384, <<"descriptor">>),
    {RawTx, _TxID, _ClaimID} =
        proof_tx_fixture(<<"sample">>, stream_claim_bytes(SDHashBytes)),
    {ok, Msg} = hb_lbry_commitment:stream_claim_message(RawTx, 0),
    ?assertEqual(<<"lbry-stream@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(hb_util:to_hex(SDHashBytes), maps:get(<<"sd-hash">>, Msg)),
    ?assert(lists:member(<<"lbry-claim@1.0">>, hb_message:commitment_devices(Msg, #{}))),
    ?assert(lists:member(<<"lbry-stream@1.0">>, hb_message:commitment_devices(Msg, #{}))),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

claim_output_store_reads_stream_kind_test() ->
    SDHashBytes = crypto:hash(sha384, <<"descriptor">>),
    {RawTx, TxID, _ClaimID} =
        proof_tx_fixture(<<"sample">>, stream_claim_bytes(SDHashBytes)),
    Store = (store_for_raw_tx(RawTx, TxID))#{ <<"kind">> => <<"stream">> },
    {ok, Msg} = hb_store:read(Store, <<TxID/binary, ":0">>, #{}),
    ?assertEqual(<<"lbry-stream@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(hb_util:to_hex(SDHashBytes), maps:get(<<"sd-hash">>, Msg)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

claim_output_store_reads_channel_path_test() ->
    {PublicKey, _Channel} = channel_key_fixture(),
    {RawTx, TxID, ClaimID} =
        proof_tx_fixture(<<"@example">>, channel_claim_bytes(PublicKey)),
    Store = store_for_raw_tx(RawTx, TxID),
    {ok, Msg} = hb_store:read(Store, <<"lbry/channel/", TxID/binary, "/0">>, #{}),
    ?assertEqual(<<"lbry-channel@1.0">>, maps:get(<<"device">>, Msg)),
    ?assertEqual(ClaimID, maps:get(<<"channel-id">>, Msg)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

store_for_raw_tx(RawTx, TxID) ->
    #{
        <<"store-module">> => hb_store_lbry_claim_output,
        <<"fixtures">> => #{
            <<"odysee/transaction/", TxID/binary>> => #{
                <<"device">> => <<"lbry-transaction@1.0">>,
                <<"content-type">> => <<"application/vnd.lbry.transaction">>,
                <<"body">> => RawTx,
                <<"txid">> => TxID,
                <<"tx-size">> => byte_size(RawTx),
                <<"tx-store-path">> => <<"odysee/transaction/", TxID/binary>>
            }
        }
    }.

sample_channel_map() ->
    {PublicKey, ClaimID} = channel_key_fixture(),
    #{
        <<"claim_id">> => ClaimID,
        <<"value">> => #{ <<"public_key">> => hb_util:to_hex(PublicKey) }
    }.

sample_stream_map() ->
    {PublicKey, ChannelID} = channel_key_fixture(),
    SDHash = hb_util:to_hex(crypto:hash(sha384, <<"descriptor">>)),
    #{
        <<"claim_id">> => <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
        <<"name">> => <<"sample">>,
        <<"txid">> => <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
        <<"nout">> => 0,
        <<"value">> => #{ <<"source">> => #{ <<"sd_hash">> => SDHash } },
        <<"signing_channel">> => #{
            <<"claim_id">> => ChannelID,
            <<"value">> => #{ <<"public_key">> => hb_util:to_hex(PublicKey) }
        }
    }.

channel_key_fixture() ->
    PrivateKey = <<1:256>>,
    {Uncompressed, _} = crypto:generate_key(ecdh, secp256k1, PrivateKey),
    PublicKey = ar_wallet:compress_ecdsa_pubkey(Uncompressed),
    ClaimID = hb_util:to_hex(reverse_binary(<<3:160>>)),
    {PublicKey, ClaimID}.

unsigned_claim_bytes(Message) ->
    <<0, Message/binary>>.

channel_claim_bytes(PublicKey) ->
    unsigned_claim_bytes(field(2, field(1, PublicKey))).

stream_claim_bytes(SDHashBytes) ->
    unsigned_claim_bytes(field(1, field(1, field(6, SDHashBytes)))).

proof_tx_fixture(Name, Value) ->
    Script = proof_claim_script(Name, Value),
    RawTx = proof_tx_with_script(Script),
    TxHash = crypto:hash(sha256, crypto:hash(sha256, RawTx)),
    TxID = hb_util:to_hex(reverse_binary(TxHash)),
    ClaimID =
        hb_util:to_hex(
            reverse_binary(
                crypto:hash(
                    ripemd160,
                    crypto:hash(sha256, <<TxHash/binary, 0:32/big>>)
                )
            )
        ),
    {RawTx, TxID, ClaimID}.

proof_tx_with_script(Script) ->
    ScriptSize = byte_size(Script),
    <<
        1:32/little,
        1,
        0:256,
        16#ffffffff:32/little,
        0,
        16#ffffffff:32/little,
        1,
        1000:64/little,
        ScriptSize,
        Script/binary,
        0:32/little
    >>.

proof_claim_script(Name, Value) ->
    AddressScript = <<16#76, 16#a9, 20, 0:160, 16#88, 16#ac>>,
    <<
        16#b5,
        (proof_push(Name))/binary,
        (proof_push(Value))/binary,
        16#6d,
        16#75,
        AddressScript/binary
    >>.

proof_push(Data) when is_binary(Data), byte_size(Data) < 76 ->
    <<(byte_size(Data)), Data/binary>>.

field(Number, Value) ->
    Key = (Number bsl 3) bor 2,
    <<(varint(Key))/binary, (varint(byte_size(Value)))/binary, Value/binary>>.

varint(Value) when Value < 16#80 ->
    <<Value>>;
varint(Value) ->
    <<((Value band 16#7f) bor 16#80), (varint(Value bsr 7))/binary>>.

reverse_binary(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).
