-module(hb_odysee_device_test).
-include_lib("eunit/include/eunit.hrl").

descriptor_device_returns_codec_message_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes, _Plaintext} =
        sample_descriptor(),
    {ok, Server, Handle} = blob_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes),
    try
        {ok, Descriptor} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"descriptor">>,
                #{},
                #{ <<"sd-hash">> => DescriptorHash },
                opts(Server)
            ),
        ?assertEqual(<<"lbry-stream-descriptor@1.0">>, maps:get(<<"device">>, Descriptor)),
        ?assertEqual(DescriptorHash, maps:get(<<"sd-hash">>, Descriptor))
    after
        hb_mock_server:stop(Handle)
    end.

media_device_serves_range_from_header_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes, Plaintext} =
        sample_descriptor(),
    {ok, Server, Handle} = blob_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"media">>,
                #{},
                #{
                    <<"sd-hash">> => DescriptorHash,
                    <<"range">> => <<"bytes=1-4">>
                },
                opts(Server)
            ),
        ?assertEqual(206, maps:get(<<"status">>, Response)),
        ?assertEqual(binary:part(Plaintext, 1, 4), maps:get(<<"body">>, Response)),
        ?assertEqual(
            iolist_to_binary([
                <<"bytes 1-4/">>,
                integer_to_binary(byte_size(Plaintext))
            ]),
            maps:get(<<"content-range">>, Response)
        ),
        ?assertEqual(byte_size(Plaintext), maps:get(<<"byte-size">>, Response)),
        ?assertEqual(
            <<"descriptor-last-blob">>,
            maps:get(<<"byte-size-source">>, Response)
        )
    after
        hb_mock_server:stop(Handle)
    end.

media_device_resolves_claim_target_with_total_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes, Plaintext} =
        sample_descriptor(),
    ClaimID = <<"0123456789abcdef0123456789abcdef01234567">>,
    Claim = #{
        <<"claim_id">> => ClaimID,
        <<"value">> => #{
            <<"source">> => #{
                <<"sd_hash">> => DescriptorHash,
                <<"size">> => integer_to_binary(byte_size(Plaintext) + 100),
                <<"media_type">> => <<"video/mp4">>,
                <<"name">> => <<"sample.mp4">>
            }
        }
    },
    {ok, Server, Handle} =
        blob_and_proxy_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes, Claim),
    try
        Opts = (opts(Server))#{ <<"lbry-proxy-node">> => Server },
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"media">>,
                #{},
                #{
                    <<"claim-id">> => ClaimID,
                    <<"range">> => <<"bytes=1-4">>
                },
                Opts
            ),
        ?assertEqual(206, maps:get(<<"status">>, Response)),
        ?assertEqual(<<"video/mp4">>, maps:get(<<"content-type">>, Response)),
        ?assertEqual(4, maps:get(<<"content-length">>, Response)),
        ?assertEqual(
            iolist_to_binary([
                <<"bytes 1-4/">>,
                integer_to_binary(byte_size(Plaintext))
            ]),
            maps:get(<<"content-range">>, Response)
        ),
        ?assertEqual(ClaimID, maps:get(<<"claim-id">>, Response)),
        ?assertEqual(<<"sample.mp4">>, maps:get(<<"filename">>, Response)),
        ?assertEqual(byte_size(Plaintext), maps:get(<<"byte-size">>, Response)),
        ?assertEqual(
            <<"descriptor-last-blob">>,
            maps:get(<<"byte-size-source">>, Response)
        ),
        ?assertEqual(binary:part(Plaintext, 1, 4), maps:get(<<"body">>, Response))
    after
        hb_mock_server:stop(Handle)
    end.

range_device_requires_explicit_range_test() ->
    {ok, Response} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"range">>,
            #{},
            #{ <<"sd-hash">> => hb_util:to_hex(crypto:hash(sha384, <<"descriptor">>)) },
            #{}
        ),
    ?assertEqual(416, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"missing_range">>, maps:get(<<"error">>, Response)).

media_device_rejects_direct_out_of_range_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes, _Plaintext} =
        sample_descriptor(),
    {ok, Server, Handle} = blob_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"media">>,
                #{},
                #{
                    <<"sd-hash">> => DescriptorHash,
                    <<"range">> => <<"bytes=1000-1005">>
                },
                opts(Server)
            ),
        ?assertEqual(416, maps:get(<<"status">>, Response)),
        ?assertEqual(<<"invalid_range">>, maps:get(<<"error">>, Response))
    after
        hb_mock_server:stop(Handle)
    end.

descriptor_device_maps_protected_blob_to_403_test() ->
    SDHash = hb_util:to_hex(crypto:hash(sha384, <<"protected">>)),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/blob", blob, {403, <<"protected">>}}
    ]),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"descriptor">>,
                #{},
                #{ <<"sd-hash">> => SDHash },
                opts(Server)
            ),
        ?assertEqual(403, maps:get(<<"status">>, Response)),
        ?assertEqual(<<"protected">>, maps:get(<<"error">>, Response))
    after
        hb_mock_server:stop(Handle)
    end.

verify_blobs_device_normalizes_descriptor_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes, _Plaintext} =
        sample_descriptor(),
    {ok, Server, Handle} = blob_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"verify-blobs">>,
                #{},
                #{ <<"sd-hash">> => DescriptorHash, <<"limit">> => <<"1">> },
                opts(Server)
            ),
        ?assertEqual(<<"odysee@1.0">>, maps:get(<<"device">>, Response)),
        ?assertEqual(1, maps:get(<<"verified-blob-count">>, Response)),
        Descriptor = maps:get(<<"descriptor">>, Response),
        ?assertEqual(<<"lbry-stream-descriptor@1.0">>, maps:get(<<"device">>, Descriptor))
    after
        hb_mock_server:stop(Handle)
    end.

stream_graph_stream_uses_tx_claim_envelope_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes, _Plaintext} =
        sample_descriptor(),
    ClaimID = <<"0123456789abcdef0123456789abcdef01234567">>,
    Claim = #{
        <<"claim_id">> => ClaimID,
        <<"txid">> => <<"synthetic-txid">>,
        <<"nout">> => 0,
        <<"value">> => #{ <<"source">> => #{ <<"sd_hash">> => DescriptorHash } }
    },
    ClaimResponse =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"items">> => [Claim] },
            <<"id">> => 1
        }),
    TxResponse =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"hex">> => claim_tx_hex(DescriptorHash) },
            <<"id">> => 1
        }),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/api/v1/proxy", proxy, fun(Req) ->
            case maps:get(<<"qs">>, Req) of
                <<"m=claim_search">> -> {200, ClaimResponse};
                <<"m=transaction_show">> -> {200, TxResponse}
            end
        end},
        {"/blob", blob, fun(Req) ->
            case maps:get(<<"qs">>, Req) of
                <<"hash=", DescriptorHash/binary>> -> {200, RawDescriptor};
                <<"hash=", BlobHash/binary>> -> {200, BlobBytes}
            end
        end}
    ]),
    try
        Opts = (opts(Server))#{ <<"lbry-proxy-node">> => Server },
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"stream-graph">>,
                #{},
                #{ <<"claim-id">> => ClaimID },
                Opts
            ),
        Stream = maps:get(<<"stream">>, Response),
        ?assertEqual(DescriptorHash, maps:get(<<"sd-hash">>, Stream))
    after
        hb_mock_server:stop(Handle)
    end.

blob_device_serves_raw_blob_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes, _Plaintext} =
        sample_descriptor(),
    {ok, Server, Handle} = blob_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"blob">>,
                #{},
                #{ <<"hash">> => BlobHash },
                opts(Server)
            ),
        ?assertEqual(200, maps:get(<<"status">>, Response)),
        ?assertEqual(
            <<"application/octet-stream">>,
            maps:get(<<"content-type">>, Response)
        ),
        ?assertEqual(byte_size(BlobBytes), maps:get(<<"content-length">>, Response)),
        ?assertEqual(BlobHash, maps:get(<<"blob-hash">>, Response)),
        ?assertEqual(BlobBytes, maps:get(<<"body">>, Response))
    after
        hb_mock_server:stop(Handle)
    end.

blob_device_maps_protected_to_403_test() ->
    Hash = hb_util:to_hex(crypto:hash(sha384, <<"protected blob">>)),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/blob", blob, {403, <<"protected">>}}
    ]),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"blob">>,
                #{},
                #{ <<"hash">> => Hash },
                opts(Server)
            ),
        ?assertEqual(403, maps:get(<<"status">>, Response)),
        ?assertEqual(<<"protected">>, maps:get(<<"error">>, Response))
    after
        hb_mock_server:stop(Handle)
    end.

blob_device_rejects_invalid_hash_test() ->
    {ok, Response} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"blob">>,
            #{},
            #{ <<"hash">> => <<"not-a-blob-hash">> },
            #{}
        ),
    ?assertEqual(404, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"not_found">>, maps:get(<<"error">>, Response)).

transaction_device_includes_raw_hex_test() ->
    {_RawDescriptor, DescriptorHash, _BlobHash, _BlobBytes, _Plaintext} =
        sample_descriptor(),
    Hex = claim_tx_hex(DescriptorHash),
    TxID = hb_lbry_tx:txid(binary:decode_hex(Hex)),
    TxResponse =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"hex">> => Hex },
            <<"id">> => 1
        }),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/api/v1/proxy", proxy, {200, TxResponse}}
    ]),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"transaction">>,
                #{},
                #{ <<"txid">> => TxID },
                #{ <<"lbry-proxy-node">> => Server, <<"http-client">> => httpc }
            ),
        ?assertEqual(Hex, maps:get(<<"raw-hex">>, Response)),
        ?assertEqual(Hex, maps:get(<<"raw">>, Response)),
        ?assertEqual(TxID, maps:get(<<"txid">>, Response)),
        ?assertEqual(
            <<"lbry-transaction@1.0">>,
            maps:get(<<"device">>, Response)
        )
    after
        hb_mock_server:stop(Handle)
    end.

transaction_device_rejects_txid_mismatch_test() ->
    {_RawDescriptor, DescriptorHash, _BlobHash, _BlobBytes, _Plaintext} =
        sample_descriptor(),
    Hex = claim_tx_hex(DescriptorHash),
    WrongTxID =
        <<"0000000000000000000000000000000000000000000000000000000000000000">>,
    TxResponse =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"hex">> => Hex },
            <<"id">> => 1
        }),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/api/v1/proxy", proxy, {200, TxResponse}}
    ]),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"transaction">>,
                #{},
                #{ <<"txid">> => WrongTxID },
                #{ <<"lbry-proxy-node">> => Server, <<"http-client">> => httpc }
            ),
        ?assertEqual(502, maps:get(<<"status">>, Response)),
        ?assertEqual(<<"txid_mismatch">>, maps:get(<<"error">>, Response))
    after
        hb_mock_server:stop(Handle)
    end.

source_route_txid_preserves_native_commitment_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = hb_lbry_commitment:transaction_message(Raw),
    TxID = maps:get(<<"txid">>, Msg),
    {ok, Source} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"id">> => uppercase(TxID) },
            source_test_opts(TxID, Msg)
        ),
    ?assertEqual(TxID, maps:get(<<"txid">>, Source)),
    ?assert(source_has_commitment(Source, <<"lbry-transaction@1.0">>)).

source_http_route_exposes_native_signature_input_test() ->
    application:ensure_all_started(inets),
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = hb_lbry_commitment:transaction_message(Raw),
    TxID = maps:get(<<"txid">>, Msg),
    Store = source_test_store(TxID, Msg),
    Node = hb_http_server:start_node(#{ <<"store">> => [Store] }),
    URL = binary_to_list(<<Node/binary, "~odysee@1.0/source?id=", TxID/binary>>),
    {ok, {{_, 200, _}, Headers, _Body}} =
        httpc:request(get, {URL, []}, [], [{body_format, binary}]),
    SignatureInput = http_header(<<"signature-input">>, Headers),
    ?assertNotEqual(not_found, SignatureInput),
    ?assertNotEqual(
        nomatch,
        binary:match(SignatureInput, <<"alg=\"lbry-transaction@1.0/sha-256d\"">>)
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(SignatureInput, <<"native-id=\"", TxID/binary, "\"">>)
    ).

source_route_blob_preserves_native_commitment_test() ->
    Body = <<"encrypted bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Body),
    Msg = hb_lbry_commitment:blob_message(Hash, Body),
    {ok, Source} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"native-id">> => uppercase(Hash) },
            source_test_opts(Hash, Msg)
        ),
    ?assertEqual(Hash, maps:get(<<"blob-hash">>, Source)),
    ?assert(source_has_commitment(Source, <<"lbry-blob@1.0">>)).

source_route_outpoint_preserves_native_commitment_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, TxMsg} = hb_lbry_commitment:transaction_message(Raw),
    TxID = maps:get(<<"txid">>, TxMsg),
    Key = <<TxID/binary, ":0">>,
    {ok, Msg} = hb_lbry_commitment:claim_output_message(Raw, 0),
    {ok, Source} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"id">> => uppercase(Key) },
            source_test_opts(Key, Msg)
        ),
    ?assertEqual(TxID, maps:get(<<"txid">>, Source)),
    ?assertEqual(0, maps:get(<<"nout">>, Source)),
    ?assert(source_has_commitment(Source, <<"lbry-claim@1.0">>)).

source_route_accepts_duplicate_aliases_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = hb_lbry_commitment:transaction_message(Raw),
    TxID = maps:get(<<"txid">>, Msg),
    {ok, Source} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"id">> => uppercase(TxID), <<"native-id">> => TxID },
            source_test_opts(TxID, Msg)
        ),
    ?assertEqual(TxID, maps:get(<<"txid">>, Source)),
    ?assert(source_has_commitment(Source, <<"lbry-transaction@1.0">>)).

source_route_rejects_claim_id_test() ->
    ClaimID = <<"585d54c7bb8fd92043ed583c5aea18a9547028aa">>,
    {ok, Response} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"id">> => ClaimID },
            #{}
        ),
    ?assertEqual(400, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"unsupported_native_source_id">>, maps:get(<<"error">>, Response)).

source_route_rejects_malformed_id_test() ->
    {ok, Response} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"id">> => <<"not-a-source-id">> },
            #{}
        ),
    ?assertEqual(400, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"unsupported_native_source_id">>, maps:get(<<"error">>, Response)).

source_route_rejects_signed_nout_alias_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    TxID = hb_lbry_tx:txid(Raw),
    {ok, Response} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"id">> => <<TxID/binary, ":+1">> },
            #{}
        ),
    ?assertEqual(400, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"unsupported_native_source_id">>, maps:get(<<"error">>, Response)).

source_route_rejects_read_parameter_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    TxID = hb_lbry_tx:txid(Raw),
    {ok, Response} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"read">> => TxID },
            #{}
        ),
    ?assertEqual(400, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"missing_native_source_id">>, maps:get(<<"error">>, Response)).

source_route_rejects_conflicting_aliases_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    TxID = hb_lbry_tx:txid(Raw),
    Hash = hb_lbry_stream_descriptor:blob_hash(<<"encrypted bytes">>),
    {ok, Response} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"id">> => TxID, <<"native-id">> => Hash },
            #{}
        ),
    ?assertEqual(400, maps:get(<<"status">>, Response)),
    ?assertEqual(<<"conflicting_native_source_id">>, maps:get(<<"error">>, Response)).

source_route_ignores_accept_for_internal_read_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = hb_lbry_commitment:transaction_message(Raw),
    TxID = maps:get(<<"txid">>, Msg),
    {ok, Source} =
        hb_ao:raw(
            <<"odysee@1.0">>,
            <<"source">>,
            #{},
            #{ <<"id">> => TxID, <<"accept">> => <<"application/aos-2">> },
            source_test_opts(TxID, Msg)
        ),
    ?assertEqual(Raw, maps:get(<<"raw">>, Source)),
    ?assertEqual(false, maps:is_key(<<"body">>, Source)).

media_device_defaults_missing_range_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes, Plaintext} =
        sample_descriptor(),
    {ok, Server, Handle} = blob_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes),
    try
        {ok, Response} =
            hb_ao:raw(
                <<"odysee@1.0">>,
                <<"media">>,
                #{},
                #{ <<"sd-hash">> => DescriptorHash },
                opts(Server)
            ),
        ?assertEqual(206, maps:get(<<"status">>, Response)),
        ?assertEqual(Plaintext, maps:get(<<"body">>, Response)),
        ?assertEqual(
            iolist_to_binary([
                <<"bytes 0-">>,
                integer_to_binary(byte_size(Plaintext) - 1),
                <<"/">>,
                integer_to_binary(byte_size(Plaintext))
            ]),
            maps:get(<<"content-range">>, Response)
        )
    after
        hb_mock_server:stop(Handle)
    end.

blob_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes) ->
    hb_mock_server:start([
        {"/blob", blob, fun(Req) ->
            case maps:get(<<"qs">>, Req) of
                <<"hash=", DescriptorHash/binary>> -> {200, RawDescriptor};
                <<"hash=", BlobHash/binary>> -> {200, BlobBytes}
            end
        end}
    ]).

blob_and_proxy_server(RawDescriptor, DescriptorHash, BlobHash, BlobBytes, Claim) ->
    Response =
        hb_json:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"result">> => #{ <<"items">> => [Claim] },
            <<"id">> => 1
        }),
    hb_mock_server:start([
        {"/api/v1/proxy", proxy, {200, Response}},
        {"/blob", blob, fun(Req) ->
            case maps:get(<<"qs">>, Req) of
                <<"hash=", DescriptorHash/binary>> -> {200, RawDescriptor};
                <<"hash=", BlobHash/binary>> -> {200, BlobBytes}
            end
        end}
    ]).

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
    RawDescriptor = hb_json:encode(Descriptor),
    DescriptorHash = hb_lbry_stream_descriptor:descriptor_hash(RawDescriptor),
    {RawDescriptor, DescriptorHash, BlobHash, Ciphertext, Plaintext}.

claim_tx_hex(DescriptorHash) ->
    hb_util:to_hex(claim_tx(binary:decode_hex(DescriptorHash))).

claim_tx(SDHash) ->
    Claim = <<0, (claim_message(SDHash))/binary>>,
    Script = <<16#b5, (push(<<"sample">>))/binary, (push(Claim))/binary, 16#6d, 16#75>>,
    <<1:32/little-signed,
        1,
        0:256,
        0:32/little,
        0,
        16#ffffffff:32/little,
        1,
        0:64/little,
        (tx_varbytes(Script))/binary,
        0:32/little>>.

claim_message(SDHash) ->
    Source = field(6, SDHash),
    Stream = field(1, Source),
    field(1, Stream).

field(Number, Value) ->
    Key = (Number bsl 3) bor 2,
    <<(proto_varint(Key))/binary, (proto_varint(byte_size(Value)))/binary, Value/binary>>.

proto_varint(Value) when Value < 16#80 ->
    <<Value>>;
proto_varint(Value) ->
    <<((Value band 16#7f) bor 16#80), (proto_varint(Value bsr 7))/binary>>.

push(Value) when byte_size(Value) < 16#4c ->
    <<(byte_size(Value)), Value/binary>>.

tx_varbytes(Value) when byte_size(Value) < 16#fd ->
    <<(byte_size(Value)), Value/binary>>.

pkcs7_pad(Plaintext) ->
    PadLen = 16 - (byte_size(Plaintext) rem 16),
    <<Plaintext/binary, (binary:copy(<<PadLen>>, PadLen))/binary>>.

opts(Server) ->
    #{
        <<"http-client">> => httpc,
        <<"lbry-blob-store">> => #{ <<"node">> => Server }
    }.

source_test_opts(Key, Msg) ->
    #{ <<"store">> => [source_test_store(Key, Msg)] }.

source_test_store(Key, Msg) ->
    Store = hb_test_utils:test_store(hb_store_volatile, <<"odysee-source">>),
    ok = hb_store:write(Store, #{ Key => Msg }, #{}),
    Store.

source_has_commitment(Msg, Device) ->
    lists:any(
        fun(Commitment) ->
            maps:get(<<"commitment-device">>, Commitment, undefined) == Device
        end,
        maps:values(maps:get(<<"commitments">>, Msg, #{}))
    ).

uppercase(Bin) ->
    hb_util:bin(string:uppercase(binary_to_list(Bin))).

http_header(Name, Headers) ->
    LowerName = hb_util:bin(string:lowercase(hb_util:bin(Name))),
    case [
        hb_util:bin(Value)
     ||
        {Key, Value} <- Headers,
        hb_util:bin(string:lowercase(hb_util:bin(Key))) == LowerName
    ] of
        [Value | _] -> Value;
        [] -> not_found
    end.
