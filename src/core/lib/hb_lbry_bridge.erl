-module(hb_lbry_bridge).
-export([
    blob/2,
    descriptor/2,
    verify_blobs/3,
    stream_graph/2,
    verified_stream/2,
    stream_size/2,
    reassemble_stream/2,
    stream_range/4
]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

blob(Hash, Opts) ->
    read_blob(Hash, Opts).

descriptor(SDHash, Opts) ->
    Result =
        case read_blob(SDHash, Opts) of
            {ok, RawDescriptor} ->
                hb_lbry_stream_descriptor:parse(RawDescriptor, SDHash);
            Error ->
                Error
        end,
    ?event(lbry_bridge,
        {descriptor_result, {sd_hash, SDHash}, {result, result_class(Result)}},
        Opts
    ),
    Result.

verify_blobs(SDHash, Limit, Opts) when is_integer(Limit), Limit >= 0 ->
    Result =
        maybe
            {ok, Descriptor} ?= descriptor(SDHash, Opts),
            DataBlobs = data_blobs(Descriptor),
            Selected = lists:sublist(DataBlobs, Limit),
            {ok, Verified} ?= verify_blob_list(Selected, Opts, []),
            {ok, #{
                <<"sd-hash">> => hb_util:to_lower(SDHash),
                <<"data-blob-count">> => length(DataBlobs),
                <<"verified-blob-count">> => length(Verified),
                <<"verified-blobs">> => Verified,
                <<"descriptor">> => Descriptor
            }}
        end,
    ?event(lbry_bridge,
        {verify_blobs_result,
            {sd_hash, SDHash},
            {limit, Limit},
            {result, result_class(Result)}},
        Opts
    ),
    Result.

stream_graph(ClaimIDOrName, Opts) ->
    ?event(lbry_bridge, {stream_graph_start, {target, ClaimIDOrName}}, Opts),
    Result =
        maybe
            {ok, Claim} ?= hb_lbry_proxy:claim(ClaimIDOrName, Opts),
            {ok, SDHash} ?= claim_sd_hash(Claim),
            {ok, TxID} ?= claim_txid(Claim),
            {ok, Descriptor} ?= descriptor(SDHash, Opts),
            {ok, RawTxResult} ?= hb_lbry_proxy:transaction_show(TxID, Opts),
            {ok, RawTxHex} ?= raw_tx_hex(RawTxResult),
            {ok, ParsedTx} ?= hb_lbry_tx:parse_hex(RawTxHex),
            {ok, #{
                <<"claim">> => Claim,
                <<"sd-hash">> => SDHash,
                <<"txid">> => TxID,
                <<"descriptor">> => Descriptor,
                <<"raw-tx">> => RawTxResult,
                <<"parsed-tx">> => ParsedTx
            }}
        end,
    ?event(lbry_bridge,
        {stream_graph_result, {target, ClaimIDOrName}, {result, result_class(Result)}},
        Opts
    ),
    Result.

verified_stream(ClaimIDOrName, Opts) ->
    ?event(lbry_bridge, {verified_stream_start, {target, ClaimIDOrName}}, Opts),
    Result =
        maybe
            {ok, StreamGraph} ?= stream_graph(ClaimIDOrName, Opts),
            Claim = maps:get(<<"claim">>, StreamGraph),
            ParsedTx = maps:get(<<"parsed-tx">>, StreamGraph),
            {ok, ClaimID} ?= claim_id(Claim),
            {ok, Nout} ?= claim_nout(Claim),
            {ok, ClaimOutput} ?= tx_output(ParsedTx, Nout),
            ok ?= matching_claim_id(ClaimID, ClaimOutput),
            {ok, SigningChannel} ?= claim_signing_channel(Claim),
            {ok, Attestation} ?= hb_lbry_attestation:verify(
                ParsedTx,
                ClaimOutput,
                SigningChannel
            ),
            ok ?= valid_attestation(Attestation),
            {ok, SignedSDHash} ?= signed_claim_sd_hash(
                ClaimOutput,
                maps:get(<<"sd-hash">>, StreamGraph)
            ),
            {ok, StreamGraph#{
                <<"attestation">> => Attestation,
                <<"signed-sd-hash">> => SignedSDHash
            }}
        end,
    ?event(lbry_bridge,
        {verified_stream_result, {target, ClaimIDOrName}, {result, result_class(Result)}},
        Opts
    ),
    Result.

stream_size(SDHash, Opts) ->
    Result =
        maybe
            {ok, Descriptor} ?= descriptor(SDHash, Opts),
            Fetch =
                fun(Hash) ->
                    read_blob(Hash, Opts)
                end,
            {ok, Size} ?= hb_lbry_stream_descriptor:stream_size(Descriptor, Fetch),
            {ok, #{
                <<"sd-hash">> => hb_util:to_lower(SDHash),
                <<"byte-size">> => Size
            }}
        end,
    ?event(lbry_bridge,
        {stream_size_result, {sd_hash, SDHash}, {result, result_class(Result)}},
        Opts
    ),
    Result.

reassemble_stream(SDHash, Opts) ->
    Result =
        maybe
            {ok, Descriptor} ?= descriptor(SDHash, Opts),
            Fetch =
                fun(Hash) ->
                    read_blob(Hash, Opts)
                end,
            {ok, Bytes} ?= hb_lbry_stream_descriptor:reassemble(Descriptor, Fetch),
            {ok, #{
                <<"sd-hash">> => hb_util:to_lower(SDHash),
                <<"byte-size">> => byte_size(Bytes),
                <<"bytes">> => Bytes,
                <<"descriptor">> => Descriptor
            }}
        end,
    ?event(lbry_bridge,
        {reassemble_result, {sd_hash, SDHash}, {result, result_class(Result)}},
        Opts
    ),
    Result.

stream_range(SDHash, Start, End, Opts) when
        is_integer(Start), is_integer(End), Start >= 0, End >= Start ->
    Result =
        maybe
            {ok, Descriptor} ?= descriptor(SDHash, Opts),
            Stride = maps:get(<<"plain-blob-stride">>, Descriptor),
            FirstBlobNum = Start div Stride,
            LastBlobNum = End div Stride,
            Blobs = lists:filter(
                fun(Blob) ->
                    BlobNum = maps:get(<<"blob-num">>, Blob),
                    BlobNum >= FirstBlobNum andalso BlobNum =< LastBlobNum
                end,
                data_blobs(Descriptor)
            ),
            {ok, Chunks} ?= range_chunks(
                Blobs,
                maps:get(<<"key">>, Descriptor),
                Start,
                End,
                Stride,
                Opts,
                []
            ),
            Bytes = iolist_to_binary(Chunks),
            ok ?= non_empty_range(Bytes),
            {ok, #{
                <<"sd-hash">> => hb_util:to_lower(SDHash),
                <<"start">> => Start,
                <<"end">> => Start + byte_size(Bytes) - 1,
                <<"requested-end">> => End,
                <<"bytes">> => Bytes
            }}
        end,
    ?event(lbry_bridge,
        {range_result,
            {sd_hash, SDHash},
            {start, Start},
            {requested_end, End},
            {result, result_class(Result)}},
        Opts
    ),
    Result.

read_blob(Hash, Opts) ->
    Store = blob_store(Opts),
    hb_store_lbry_blob:read(Store, #{ <<"read">> => Hash }, Opts).

blob_store(Opts) ->
    Base = #{ <<"store-module">> => hb_store_lbry_blob },
    hb_maps:merge(
        Base,
        hb_maps:get(<<"lbry-blob-store">>, Opts, #{}, Opts),
        Opts
    ).

data_blobs(Descriptor) ->
    lists:filter(
        fun(Blob) ->
            maps:get(<<"terminator">>, Blob, false) =/= true
        end,
        maps:get(<<"blobs">>, Descriptor)
    ).

verify_blob_list([], _Opts, Acc) ->
    {ok, lists:reverse(Acc)};
verify_blob_list([Blob | Rest], Opts, Acc) ->
    Hash = maps:get(<<"blob-hash">>, Blob),
    case read_blob(Hash, Opts) of
        {ok, Bytes} ->
            Verified = #{
                <<"blob-num">> => maps:get(<<"blob-num">>, Blob),
                <<"blob-hash">> => Hash,
                <<"length">> => byte_size(Bytes)
            },
            verify_blob_list(Rest, Opts, [Verified | Acc]);
        Error ->
            Error
    end.

range_chunks([], _KeyHex, _Start, _End, _Stride, _Opts, Acc) ->
    {ok, lists:reverse(Acc)};
range_chunks([Blob | Rest], KeyHex, Start, End, Stride, Opts, Acc) ->
    maybe
        {ok, Plaintext} ?= read_decrypted_blob(Blob, KeyHex, Opts),
        BlobStart = maps:get(<<"blob-num">>, Blob) * Stride,
        BlobEnd = BlobStart + byte_size(Plaintext) - 1,
        Chunk = slice_overlap(Plaintext, BlobStart, BlobEnd, Start, End),
        range_chunks(Rest, KeyHex, Start, End, Stride, Opts, [Chunk | Acc])
    end.

read_decrypted_blob(Blob, KeyHex, Opts) ->
    Hash = maps:get(<<"blob-hash">>, Blob),
    ExpectedLength = maps:get(<<"length">>, Blob),
    case read_blob(Hash, Opts) of
        {ok, Ciphertext} ->
            case byte_size(Ciphertext) of
                ExpectedLength ->
                    hb_lbry_stream_descriptor:decrypt_blob(KeyHex, Blob, Ciphertext);
                ActualLength ->
                    {error, {length_mismatch, Hash, ExpectedLength, ActualLength}}
            end;
        Error ->
            Error
    end.

slice_overlap(_Plaintext, BlobStart, BlobEnd, Start, End) when
        BlobEnd < Start orelse BlobStart > End ->
    <<>>;
slice_overlap(Plaintext, BlobStart, _BlobEnd, Start, End) ->
    SliceStart = max(Start, BlobStart),
    SliceEnd = min(End, BlobStart + byte_size(Plaintext) - 1),
    Offset = SliceStart - BlobStart,
    Length = SliceEnd - SliceStart + 1,
    binary:part(Plaintext, Offset, Length).

non_empty_range(<<>>) ->
    {error, invalid_range};
non_empty_range(_) ->
    ok.

signed_claim_sd_hash(#{ <<"claim-envelope">> := Envelope }, SDHash) ->
    case hb_lbry_claim_proto:stream_sd_hash(maps:get(<<"message">>, Envelope)) of
        {ok, SignedSDHash} ->
            RequestedSDHash = hb_util:to_lower(SDHash),
            case SignedSDHash of
                RequestedSDHash -> {ok, SignedSDHash};
                _ -> {error, {signed_sd_hash_mismatch, SignedSDHash, RequestedSDHash}}
            end;
        Error ->
            Error
    end.

valid_attestation(#{ <<"valid">> := true }) ->
    ok;
valid_attestation(Attestation) ->
    {error, {
        invalid_attestation,
        maps:get(<<"signature-valid">>, Attestation, undefined),
        maps:get(<<"channel-hash-valid">>, Attestation, undefined)
    }}.

claim_sd_hash(Claim) ->
    case hb_util:deep_get([<<"value">>, <<"source">>, <<"sd_hash">>], Claim, #{}) of
        not_found -> {error, missing_sd_hash};
        SDHash -> {ok, SDHash}
    end.

claim_txid(Claim) ->
    case maps:get(<<"txid">>, Claim, undefined) of
        undefined -> {error, missing_txid};
        TxID -> {ok, TxID}
    end.

claim_id(Claim) ->
    case maps:get(<<"claim_id">>, Claim, undefined) of
        ClaimID when is_binary(ClaimID), byte_size(ClaimID) > 0 ->
            {ok, hb_util:to_lower(ClaimID)};
        _ ->
            {error, missing_claim_id}
    end.

claim_nout(Claim) ->
    case maps:get(<<"nout">>, Claim, undefined) of
        Nout when is_integer(Nout), Nout >= 0 -> {ok, Nout};
        Nout when is_binary(Nout) ->
            try binary_to_integer(Nout) of
                Int when Int >= 0 -> {ok, Int};
                _ -> {error, missing_nout}
            catch
                _:_ -> {error, missing_nout}
            end;
        _ ->
            {error, missing_nout}
    end.

claim_signing_channel(Claim) ->
    case maps:get(<<"signing_channel">>, Claim, undefined) of
        Channel when is_map(Channel) -> {ok, Channel};
        _ -> {error, missing_signing_channel}
    end.

tx_output(Tx, Nout) ->
    case lists:filter(
        fun(Output) ->
            maps:get(<<"nout">>, Output, undefined) == Nout
        end,
        maps:get(<<"outputs">>, Tx, [])
    ) of
        [Output | _] -> {ok, Output};
        [] -> {error, missing_claim_output}
    end.

matching_claim_id(ClaimID, ClaimOutput) ->
    case maps:get(<<"claim-id">>, ClaimOutput, undefined) of
        OutputClaimID when is_binary(OutputClaimID) ->
            NormalizedOutput = hb_util:to_lower(OutputClaimID),
            case NormalizedOutput of
                ClaimID -> ok;
                _ -> {error, {claim_id_mismatch, NormalizedOutput, ClaimID}}
            end;
        _ ->
            {error, missing_claim_output_claim_id}
    end.

raw_tx_hex(TxResult) ->
    case maps:get(<<"hex">>, TxResult, undefined) of
        undefined -> {error, missing_raw_tx_hex};
        Hex -> {ok, Hex}
    end.

result_class({ok, _}) -> ok;
result_class({error, _}) -> error;
result_class({failure, _}) -> failure.

verify_blobs_fetches_limited_data_blobs_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes} = sample_descriptor(),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/blob", blob, fun(Req) ->
            case maps:get(<<"qs">>, Req) of
                <<"hash=", DescriptorHash/binary>> -> {200, RawDescriptor};
                <<"hash=", BlobHash/binary>> -> {200, BlobBytes}
            end
        end}
    ]),
    try
        Opts = #{
            <<"http-client">> => httpc,
            <<"lbry-blob-store">> => #{ <<"node">> => Server }
        },
        {ok, Result} = verify_blobs(DescriptorHash, 1, Opts),
        ?assertEqual(1, maps:get(<<"verified-blob-count">>, Result)),
        ?assertEqual(1, maps:get(<<"data-blob-count">>, Result))
    after
        hb_mock_server:stop(Handle)
    end.

stream_range_fetches_requested_slice_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes} = sample_descriptor(),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/blob", blob, fun(Req) ->
            case maps:get(<<"qs">>, Req) of
                <<"hash=", DescriptorHash/binary>> -> {200, RawDescriptor};
                <<"hash=", BlobHash/binary>> -> {200, BlobBytes}
            end
        end}
    ]),
    try
        Opts = #{
            <<"http-client">> => httpc,
            <<"lbry-blob-store">> => #{ <<"node">> => Server }
        },
        {ok, Result} = stream_range(DescriptorHash, 0, 5, Opts),
        ?assertEqual(<<"bridge">>, maps:get(<<"bytes">>, Result)),
        ?assertEqual(5, maps:get(<<"end">>, Result))
    after
        hb_mock_server:stop(Handle)
    end.

stream_range_rejects_empty_slice_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes} = sample_descriptor(),
    {ok, Server, Handle} = hb_mock_server:start([
        {"/blob", blob, fun(Req) ->
            case maps:get(<<"qs">>, Req) of
                <<"hash=", DescriptorHash/binary>> -> {200, RawDescriptor};
                <<"hash=", BlobHash/binary>> -> {200, BlobBytes}
            end
        end}
    ]),
    try
        Opts = #{
            <<"http-client">> => httpc,
            <<"lbry-blob-store">> => #{ <<"node">> => Server }
        },
        ?assertEqual({error, invalid_range}, stream_range(DescriptorHash, 1000, 1005, Opts))
    after
        hb_mock_server:stop(Handle)
    end.

verified_stream_rejects_sdk_sd_hash_mismatch_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes} = sample_descriptor(),
    ClaimID = <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
    Claim = #{
        <<"claim_id">> => ClaimID,
        <<"txid">> => <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
        <<"nout">> => 0,
        <<"value">> => #{ <<"source">> => #{ <<"sd_hash">> => DescriptorHash } },
        <<"signing_channel">> => #{
            <<"claim_id">> => <<"585d54c7b82fd92043ed583c5aea18a9547028aa">>,
            <<"value">> => #{
                <<"public_key">> =>
                    <<"03fa4e5fe9f02f2f1a8c34ec150b91f762d8b07b7be942f26aa80c40902d5dbd11">>
            }
        }
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
            <<"result">> => #{ <<"hex">> => hb_lbry_tx:task0_tx_hex() },
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
        Opts = #{
            <<"http-client">> => httpc,
            <<"lbry-proxy-node">> => Server,
            <<"lbry-blob-store">> => #{ <<"node">> => Server }
        },
        {error, {signed_sd_hash_mismatch, _Signed, DescriptorHash}} =
            verified_stream(ClaimID, Opts)
    after
        hb_mock_server:stop(Handle)
    end.

verified_stream_rejects_invalid_attestation_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes} = sample_descriptor(),
    ClaimID = <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
    Claim = #{
        <<"claim_id">> => ClaimID,
        <<"txid">> => <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
        <<"nout">> => 0,
        <<"value">> => #{ <<"source">> => #{ <<"sd_hash">> => DescriptorHash } },
        <<"signing_channel">> => #{
            <<"claim_id">> => <<"0000000000000000000000000000000000000000">>,
            <<"value">> => #{
                <<"public_key">> =>
                    <<"03fa4e5fe9f02f2f1a8c34ec150b91f762d8b07b7be942f26aa80c40902d5dbd11">>
            }
        }
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
            <<"result">> => #{ <<"hex">> => hb_lbry_tx:task0_tx_hex() },
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
        Opts = #{
            <<"http-client">> => httpc,
            <<"lbry-proxy-node">> => Server,
            <<"lbry-blob-store">> => #{ <<"node">> => Server }
        },
        ?assertEqual(
            {error, {invalid_attestation, true, false}},
            verified_stream(ClaimID, Opts)
        )
    after
        hb_mock_server:stop(Handle)
    end.

verified_stream_rejects_claim_id_mismatch_test() ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes} = sample_descriptor(),
    BadClaimID = <<"0000000000000000000000000000000000000000">>,
    Claim = #{
        <<"claim_id">> => BadClaimID,
        <<"txid">> => <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
        <<"nout">> => 0,
        <<"value">> => #{ <<"source">> => #{ <<"sd_hash">> => DescriptorHash } },
        <<"signing_channel">> => #{
            <<"claim_id">> => <<"585d54c7b82fd92043ed583c5aea18a9547028aa">>,
            <<"value">> => #{
                <<"public_key">> =>
                    <<"03fa4e5fe9f02f2f1a8c34ec150b91f762d8b07b7be942f26aa80c40902d5dbd11">>
            }
        }
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
            <<"result">> => #{ <<"hex">> => hb_lbry_tx:task0_tx_hex() },
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
        Opts = #{
            <<"http-client">> => httpc,
            <<"lbry-proxy-node">> => Server,
            <<"lbry-blob-store">> => #{ <<"node">> => Server }
        },
        ?assertEqual(
            {error, {
                claim_id_mismatch,
                <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
                BadClaimID
            }},
            verified_stream(BadClaimID, Opts)
        )
    after
        hb_mock_server:stop(Handle)
    end.

sample_descriptor() ->
    Key = <<0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15>>,
    IV = <<16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31>>,
    Plaintext = <<"bridge smoke">>,
    BlobBytes =
        crypto:crypto_one_time(
            aes_128_cbc,
            Key,
            IV,
            pkcs7_pad(Plaintext),
            true
        ),
    BlobHash = hb_lbry_stream_descriptor:blob_hash(BlobBytes),
    RawDescriptor =
        hb_json:encode(#{
            <<"stream_type">> => <<"lbryfile">>,
            <<"stream_name">> => hb_util:to_hex(<<"sample.mp4">>),
            <<"key">> => hb_util:to_hex(Key),
            <<"suggested_file_name">> => hb_util:to_hex(<<"sample.mp4">>),
            <<"stream_hash">> => hb_lbry_stream_descriptor:blob_hash(<<"stream">>),
            <<"blobs">> => [
                #{
                    <<"length">> => byte_size(BlobBytes),
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
        }),
    DescriptorHash = hb_lbry_stream_descriptor:blob_hash(RawDescriptor),
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes}.

pkcs7_pad(Plaintext) ->
    PadLen = 16 - (byte_size(Plaintext) rem 16),
    <<Plaintext/binary, (binary:copy(<<PadLen>>, PadLen))/binary>>.
