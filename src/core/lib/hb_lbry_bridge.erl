-module(hb_lbry_bridge).
-export([
    blob/2,
    blob_message/2,
    transaction_message/2,
    claim_output_message/3,
    channel_output_message/3,
    stream_message/3,
    descriptor_message/2,
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

%% @doc Read a blob through the store and return the full blob message,
%% including its native `lbry-blob@1.0' commitment.
blob_message(Hash, Opts) ->
    Store = blob_store(Opts),
    hb_store_lbry_blob:read(Store, #{ <<"read">> => Hash }, Opts).

%% @doc Read a raw transaction through the store and return the full
%% transaction message, including its native `lbry-transaction@1.0'
%% commitment.
transaction_message(TxID, Opts) ->
    Store = transaction_store(Opts),
    hb_store_lbry_transaction:read(Store, #{ <<"read">> => TxID }, Opts).

%% @doc Read immutable claim-output evidence by outpoint through the store,
%% returning a message with a native `lbry-claim@1.0' commitment.
claim_output_message(TxID, Nout, Opts) ->
    read_output(TxID, Nout, claim_output_store(Opts), Opts).

%% @doc Read immutable channel-output evidence by outpoint through the
%% store, returning a message with a native `lbry-channel@1.0' commitment
%% and the normalized channel public key.
channel_output_message(TxID, Nout, Opts) ->
    Store = (claim_output_store(Opts))#{ <<"kind">> => <<"channel">> },
    read_output(TxID, Nout, Store, Opts).

%% @doc Read immutable stream claim-output evidence by outpoint through the
%% store, returning a message with native `lbry-claim@1.0' and
%% `lbry-stream@1.0' commitments.
stream_message(TxID, Nout, Opts) ->
    Store = (claim_output_store(Opts))#{ <<"kind">> => <<"stream">> },
    read_output(TxID, Nout, Store, Opts).

%% @doc Read a stream descriptor through the blob store and return the full
%% descriptor message, including its native `lbry-stream-descriptor@1.0'
%% commitment.
descriptor_message(SDHash, Opts) ->
    case read_blob(SDHash, Opts) of
        {ok, Raw} -> hb_lbry_commitment:descriptor_message(Raw, SDHash);
        Error -> Error
    end.

read_output(TxID, Nout, Store, Opts) ->
    Key = <<TxID/binary, ":", (integer_to_binary(Nout))/binary>>,
    hb_store_lbry_claim_output:read(Store, #{ <<"read">> => Key }, Opts).

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

%% @doc Compose a verified stream from store-sourced evidence messages. The
%% locator (SDK resolve) only points at the immutable outpoint; the stream
%% claim, channel claim, and descriptor evidence are fetched through stores
%% and every native commitment is verified through `hb_message:verify'. Any
%% missing evidence or failing commitment fails closed. Unsigned claims fail
%% closed with `unsigned_claim': no channel attests to the content.
verified_stream(ClaimIDOrName, Opts) ->
    ?event(lbry_bridge, {verified_stream_start, {target, ClaimIDOrName}}, Opts),
    Result =
        maybe
            {ok, Claim} ?= hb_lbry_proxy:claim(ClaimIDOrName, Opts),
            {ok, ClaimID} ?= claim_id(Claim),
            {ok, SDKSDHash} ?= claim_sd_hash(Claim),
            {ok, TxID} ?= claim_txid(Claim),
            {ok, Nout} ?= claim_nout(Claim),
            {ok, StreamMsg0} ?= stream_message(TxID, Nout, Opts),
            ok ?= matching_claim_id(ClaimID, StreamMsg0),
            {ok, SignedSDHash} ?=
                matching_sd_hash(maps:get(<<"sd-hash">>, StreamMsg0), SDKSDHash),
            {ok, ParsedTx} ?=
                hb_lbry_tx:parse(maps:get(<<"raw-transaction">>, StreamMsg0)),
            {ok, StreamMsg, ChannelMsg, Attestation} ?=
                channel_attestation(StreamMsg0, ParsedTx, Opts),
            ok ?= valid_attestation(Attestation),
            {ok, DescriptorMsg} ?= descriptor_message(SignedSDHash, Opts),
            ok ?= native_verified(StreamMsg, Opts),
            ok ?= native_verified(ChannelMsg, Opts),
            ok ?= native_verified(DescriptorMsg, Opts),
            {ok, #{
                <<"claim">> => Claim,
                <<"sd-hash">> => SignedSDHash,
                <<"signed-sd-hash">> => SignedSDHash,
                <<"txid">> => TxID,
                <<"descriptor">> => DescriptorMsg,
                <<"parsed-tx">> => ParsedTx,
                <<"attestation">> => Attestation,
                <<"stream-evidence">> => StreamMsg,
                <<"channel-evidence">> => ChannelMsg
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
    case blob_message(Hash, Opts) of
        {ok, #{ <<"data">> := Bytes }} -> {ok, Bytes};
        Error -> Error
    end.

blob_store(Opts) ->
    Base = #{ <<"store-module">> => hb_store_lbry_blob },
    hb_maps:merge(
        Base,
        hb_maps:get(<<"lbry-blob-store">>, Opts, #{}, Opts),
        Opts
    ).

transaction_store(Opts) ->
    Base = #{ <<"store-module">> => hb_store_lbry_transaction },
    hb_maps:merge(
        Base,
        hb_maps:get(<<"lbry-tx-store">>, Opts, #{}, Opts),
        Opts
    ).

claim_output_store(Opts) ->
    Base = #{ <<"store-module">> => hb_store_lbry_claim_output },
    hb_maps:merge(
        Base,
        hb_maps:get(<<"lbry-tx-store">>, Opts, #{}, Opts),
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

%% @doc Fetch and bind the channel evidence for a signed stream claim. The
%% channel is located by the claim envelope's embedded signing-channel id --
%% never by the SDK's signing-channel hints -- and the channel public key
%% comes from the verified raw channel claim. Attaches the channel
%% attestation commitment to the stream evidence and builds the
%% frontend-facing attestation view.
channel_attestation(StreamMsg, ParsedTx, Opts) ->
    Envelope = maps:get(<<"claim-envelope">>, StreamMsg),
    case maps:get(<<"signed">>, Envelope, false) of
        false ->
            {error, unsigned_claim};
        true ->
            maybe
                EnvelopeChannelID = maps:get(<<"signing-channel-id">>, Envelope),
                {ok, ChannelClaim} ?=
                    hb_lbry_proxy:claim_search(EnvelopeChannelID, Opts),
                {ok, ChannelTxID} ?= claim_txid(ChannelClaim),
                {ok, ChannelNout} ?= claim_nout(ChannelClaim),
                {ok, ChannelMsg} ?=
                    channel_output_message(ChannelTxID, ChannelNout, Opts),
                {ok, CommittedStreamMsg} ?=
                    hb_lbry_commitment:with_attestation_commitment(
                        StreamMsg,
                        ChannelMsg
                    ),
                {ok, Attestation} ?=
                    hb_lbry_attestation:verify(
                        ParsedTx,
                        StreamMsg,
                        evidence_channel(ChannelMsg)
                    ),
                {ok, CommittedStreamMsg, ChannelMsg, Attestation}
            end
    end.

%% The attestation view builder reads SDK-shaped channel maps; feed it the
%% raw-evidence values so the view reflects what was actually verified.
evidence_channel(ChannelMsg) ->
    #{
        <<"claim_id">> => maps:get(<<"claim-id">>, ChannelMsg),
        <<"value">> =>
            #{ <<"public_key">> => maps:get(<<"public-key">>, ChannelMsg) }
    }.

matching_sd_hash(SignedSDHash, SDKSDHash) ->
    Requested = hb_util:to_lower(SDKSDHash),
    case SignedSDHash of
        Requested -> {ok, SignedSDHash};
        _ -> {error, {signed_sd_hash_mismatch, SignedSDHash, Requested}}
    end.

native_verified(Msg, Opts) ->
    case hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, Opts) of
        true -> ok;
        _ -> {error, native_commitment_failure}
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

verified_stream_uses_raw_channel_evidence_test() ->
    Fixture = signed_stream_fixture(<<1:256>>, <<1:256>>),
    % The SDK lies about the channel public key; the raw channel claim must
    % be the source of the verification key, so verification still succeeds
    % and the attestation reports the real key.
    LyingKey =
        hb_util:to_hex(
            ar_wallet:compress_ecdsa_pubkey(
                element(1, crypto:generate_key(ecdh, secp256k1, <<3:256>>))
            )
        ),
    {ok, Server, Handle} =
        fixture_server(Fixture, #{ sdk_channel_public_key => LyingKey }),
    try
        {ok, Result} =
            verified_stream(maps:get(stream_claim_id, Fixture), fixture_opts(Server)),
        Attestation = maps:get(<<"attestation">>, Result),
        ?assertEqual(true, maps:get(<<"valid">>, Attestation)),
        ?assertEqual(
            maps:get(channel_public_key, Fixture),
            maps:get(<<"public-key">>, Attestation)
        ),
        {_, DescriptorHash, _, _} = maps:get(descriptor, Fixture),
        ?assertEqual(DescriptorHash, maps:get(<<"signed-sd-hash">>, Result)),
        StreamEvidence = maps:get(<<"stream-evidence">>, Result),
        ?assertEqual(3, map_size(maps:get(<<"commitments">>, StreamEvidence))),
        ?assert(maps:is_key(<<"channel-evidence">>, StreamEvidence)),
        ?assertEqual(
            true,
            hb_message:verify(
                StreamEvidence,
                #{ <<"commitment-ids">> => <<"all">> },
                #{}
            )
        ),
        ?assertEqual(
            false,
            hb_message:verify(
                maps:remove(<<"channel-evidence">>, StreamEvidence),
                #{ <<"commitment-ids">> => <<"all">> },
                #{}
            )
        ),
        ChannelEvidence = maps:get(<<"channel-evidence">>, Result),
        ?assertEqual(
            maps:get(channel_claim_id, Fixture),
            maps:get(<<"claim-id">>, ChannelEvidence)
        ),
        ?assertEqual(
            maps:get(channel_public_key, Fixture),
            maps:get(<<"public-key">>, ChannelEvidence)
        )
    after
        hb_mock_server:stop(Handle)
    end.

verified_stream_reports_ancestor_derived_for_updated_channel_test() ->
    Fixture = ancestor_channel_stream_fixture(),
    {ok, Server, Handle} =
        fixture_server(Fixture, #{ extra_txs => maps:get(extra_txs, Fixture) }),
    try
        Opts0 = fixture_opts(Server),
        TxStore = maps:get(<<"lbry-tx-store">>, Opts0),
        Opts = Opts0#{
            <<"lbry-tx-store">> => TxStore#{ <<"walk-ancestry">> => true }
        },
        {ok, Result} = verified_stream(maps:get(stream_claim_id, Fixture), Opts),
        ?assertEqual(
            true,
            maps:get(<<"valid">>, maps:get(<<"attestation">>, Result))
        ),
        ChannelEvidence = maps:get(<<"channel-evidence">>, Result),
        ?assertEqual(<<"update">>, maps:get(<<"claim-op">>, ChannelEvidence)),
        ?assertEqual(
            <<"ancestor-derived">>,
            maps:get(<<"claim-proof-strength">>, ChannelEvidence)
        ),
        ?assertMatch([_], maps:get(<<"claim-ancestry">>, ChannelEvidence)),
        StreamEvidence = maps:get(<<"stream-evidence">>, Result),
        ?assertEqual(
            <<"hash-derived">>,
            maps:get(<<"claim-proof-strength">>, StreamEvidence)
        ),
        ?assertEqual(
            true,
            hb_message:verify(
                StreamEvidence,
                #{ <<"commitment-ids">> => <<"all">> },
                #{}
            )
        )
    after
        hb_mock_server:stop(Handle)
    end.

%% A fixture whose signing channel is an on-chain update with walkable
%% create ancestry: the channel evidence upgrades to ancestor-derived while
%% the stream claim itself stays a hash-derived create.
ancestor_channel_stream_fixture() ->
    Descriptor = {_, DescriptorHash, _, _} = sample_descriptor(),
    {ChannelPrivKey, Compressed, _} = hb_lbry_ancestry:test_key(),
    ChannelClaim = <<0, (proto_field(2, proto_field(1, Compressed)))/binary>>,
    ChannelCreate = hb_lbry_ancestry:test_create_tx(<<"@chan">>, ChannelClaim),
    {ok, ParsedCreate} = hb_lbry_tx:parse(ChannelCreate),
    [CreateOutput | _] = maps:get(<<"outputs">>, ParsedCreate),
    ChannelClaimID = maps:get(<<"claim-id">>, CreateOutput),
    ChannelUpdate =
        hb_lbry_ancestry:test_update_tx(
            <<"@chan">>,
            ChannelClaimID,
            [{ChannelCreate, 0}],
            #{ <<"claim">> => ChannelClaim }
        ),
    {ok, ParsedUpdate} = hb_lbry_tx:parse(ChannelUpdate),
    [UpdateOutput | _] = maps:get(<<"outputs">>, ParsedUpdate),
    ChannelHash = maps:get(<<"claim-hash">>, UpdateOutput),
    StreamProto =
        proto_field(1,
            proto_field(1,
                proto_field(6, binary:decode_hex(DescriptorHash)))),
    PrevHash = <<1:256>>,
    Piece1 = <<PrevHash/binary, 0:32/little>>,
    Digest =
        crypto:hash(
            sha256,
            <<Piece1/binary, ChannelHash/binary, StreamProto/binary>>
        ),
    Signature = compact_signature(ChannelPrivKey, Digest),
    Envelope = <<1, ChannelHash/binary, Signature/binary, StreamProto/binary>>,
    Fixture =
        fixture_from_txs(
            Descriptor,
            ChannelUpdate,
            ParsedUpdate,
            create_claim_tx(PrevHash, <<"video">>, Envelope),
            hb_util:to_hex(Compressed)
        ),
    Fixture#{
        extra_txs => #{
            hb_lbry_tx:txid(ChannelCreate) => hb_util:to_hex(ChannelCreate)
        }
    }.

verified_stream_rejects_channel_binding_mismatch_test() ->
    % The stream is signed by channel A, but the locator serves channel B's
    % evidence under A's claim id. The derived channel claim id must not
    % match the envelope's embedded signing-channel hash.
    FixtureA = signed_stream_fixture(<<1:256>>, <<1:256>>),
    FixtureB = signed_stream_fixture(<<2:256>>, <<2:256>>),
    {ok, Server, Handle} =
        fixture_server(FixtureA, #{
            channel_txid => maps:get(channel_txid, FixtureB),
            channel_tx_hex => maps:get(channel_tx_hex, FixtureB)
        }),
    try
        ?assertEqual(
            {error,
                {channel_binding_mismatch,
                    maps:get(channel_claim_id, FixtureB),
                    maps:get(channel_claim_id, FixtureA)}},
            verified_stream(maps:get(stream_claim_id, FixtureA), fixture_opts(Server))
        )
    after
        hb_mock_server:stop(Handle)
    end.

verified_stream_rejects_forged_signature_test() ->
    % The envelope is signed with a key that is not the channel's.
    Fixture = signed_stream_fixture(<<1:256>>, <<2:256>>),
    {ok, Server, Handle} = fixture_server(Fixture, #{}),
    try
        ?assertEqual(
            {error, invalid_claim_signature},
            verified_stream(maps:get(stream_claim_id, Fixture), fixture_opts(Server))
        )
    after
        hb_mock_server:stop(Handle)
    end.

verified_stream_rejects_unsigned_claim_test() ->
    Fixture = unsigned_stream_fixture(),
    {ok, Server, Handle} = fixture_server(Fixture, #{}),
    try
        ?assertEqual(
            {error, unsigned_claim},
            verified_stream(maps:get(stream_claim_id, Fixture), fixture_opts(Server))
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

%% Build a complete signed-stream fixture: a channel claim transaction and
%% a stream claim transaction whose envelope is signed over the real v2
%% digest with `SignerPrivKey'. When the signer differs from the channel
%% key, the signature is genuinely invalid for the channel.
signed_stream_fixture(ChannelPrivKey, SignerPrivKey) ->
    Descriptor = {_, DescriptorHash, _, _} = sample_descriptor(),
    {ChannelPub, _} = crypto:generate_key(ecdh, secp256k1, ChannelPrivKey),
    Compressed = ar_wallet:compress_ecdsa_pubkey(ChannelPub),
    ChannelClaim = <<0, (proto_field(2, proto_field(1, Compressed)))/binary>>,
    ChannelTx = create_claim_tx(<<0:256>>, <<"@chan">>, ChannelClaim),
    {ok, ParsedChannelTx} = hb_lbry_tx:parse(ChannelTx),
    [ChannelOutput | _] = maps:get(<<"outputs">>, ParsedChannelTx),
    ChannelHash = maps:get(<<"claim-hash">>, ChannelOutput),
    StreamProto =
        proto_field(1,
            proto_field(1,
                proto_field(6, binary:decode_hex(DescriptorHash)))),
    PrevHash = <<1:256>>,
    Piece1 = <<PrevHash/binary, 0:32/little>>,
    Digest =
        crypto:hash(
            sha256,
            <<Piece1/binary, ChannelHash/binary, StreamProto/binary>>
        ),
    Signature = compact_signature(SignerPrivKey, Digest),
    Envelope = <<1, ChannelHash/binary, Signature/binary, StreamProto/binary>>,
    fixture_from_txs(
        Descriptor,
        ChannelTx,
        ParsedChannelTx,
        create_claim_tx(PrevHash, <<"video">>, Envelope),
        hb_util:to_hex(Compressed)
    ).

unsigned_stream_fixture() ->
    Descriptor = {_, DescriptorHash, _, _} = sample_descriptor(),
    {ChannelPub, _} = crypto:generate_key(ecdh, secp256k1, <<1:256>>),
    Compressed = ar_wallet:compress_ecdsa_pubkey(ChannelPub),
    ChannelClaim = <<0, (proto_field(2, proto_field(1, Compressed)))/binary>>,
    ChannelTx = create_claim_tx(<<0:256>>, <<"@chan">>, ChannelClaim),
    {ok, ParsedChannelTx} = hb_lbry_tx:parse(ChannelTx),
    StreamProto =
        proto_field(1,
            proto_field(1,
                proto_field(6, binary:decode_hex(DescriptorHash)))),
    Envelope = <<0, StreamProto/binary>>,
    fixture_from_txs(
        Descriptor,
        ChannelTx,
        ParsedChannelTx,
        create_claim_tx(<<1:256>>, <<"video">>, Envelope),
        hb_util:to_hex(Compressed)
    ).

fixture_from_txs(Descriptor, ChannelTx, ParsedChannelTx, StreamTx, PublicKeyHex) ->
    [ChannelOutput | _] = maps:get(<<"outputs">>, ParsedChannelTx),
    {ok, ParsedStreamTx} = hb_lbry_tx:parse(StreamTx),
    [StreamOutput | _] = maps:get(<<"outputs">>, ParsedStreamTx),
    #{
        descriptor => Descriptor,
        channel_claim_id => maps:get(<<"claim-id">>, ChannelOutput),
        channel_txid => maps:get(<<"txid">>, ParsedChannelTx),
        channel_tx_hex => hb_util:to_hex(ChannelTx),
        stream_claim_id => maps:get(<<"claim-id">>, StreamOutput),
        stream_txid => maps:get(<<"txid">>, ParsedStreamTx),
        stream_tx_hex => hb_util:to_hex(StreamTx),
        channel_public_key => PublicKeyHex
    }.

fixture_server(Fixture, Overrides) ->
    {RawDescriptor, DescriptorHash, BlobHash, BlobBytes} =
        maps:get(descriptor, Fixture),
    StreamClaimID = maps:get(stream_claim_id, Fixture),
    ChannelClaimID = maps:get(channel_claim_id, Fixture),
    StreamTxID = maps:get(stream_txid, Fixture),
    ChannelTxID = maps:get(channel_txid, maps:merge(Fixture, Overrides)),
    ChannelTxHex = maps:get(channel_tx_hex, maps:merge(Fixture, Overrides)),
    SDKPublicKey =
        maps:get(
            sdk_channel_public_key,
            Overrides,
            maps:get(channel_public_key, Fixture)
        ),
    StreamClaim = #{
        <<"claim_id">> => StreamClaimID,
        <<"txid">> => StreamTxID,
        <<"nout">> => 0,
        <<"value">> => #{ <<"source">> => #{ <<"sd_hash">> => DescriptorHash } },
        <<"signing_channel">> => #{
            <<"claim_id">> => ChannelClaimID,
            <<"value">> => #{ <<"public_key">> => SDKPublicKey }
        }
    },
    ChannelClaim = #{
        <<"claim_id">> => ChannelClaimID,
        <<"txid">> => ChannelTxID,
        <<"nout">> => 0
    },
    Claims = #{ StreamClaimID => StreamClaim, ChannelClaimID => ChannelClaim },
    Txs = maps:merge(
        #{
            StreamTxID => maps:get(stream_tx_hex, Fixture),
            ChannelTxID => ChannelTxHex
        },
        maps:get(extra_txs, Overrides, #{})
    ),
    hb_mock_server:start([
        {"/api/v1/proxy", proxy, fun(Req) ->
            Body = hb_json:decode(maps:get(<<"body">>, Req)),
            Params = maps:get(<<"params">>, Body),
            case maps:get(<<"qs">>, Req) of
                <<"m=claim_search">> ->
                    [ID] = maps:get(<<"claim_ids">>, Params),
                    {200, proxy_result(#{ <<"items">> => [maps:get(ID, Claims)] })};
                <<"m=transaction_show">> ->
                    TxID = maps:get(<<"txid">>, Params),
                    {200, proxy_result(#{ <<"hex">> => maps:get(TxID, Txs) })}
            end
        end},
        {"/blob", blob, fun(Req) ->
            case maps:get(<<"qs">>, Req) of
                <<"hash=", DescriptorHash/binary>> -> {200, RawDescriptor};
                <<"hash=", BlobHash/binary>> -> {200, BlobBytes}
            end
        end}
    ]).

proxy_result(Result) ->
    hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => Result,
        <<"id">> => 1
    }).

fixture_opts(Server) ->
    #{
        <<"http-client">> => httpc,
        <<"lbry-proxy-node">> => Server,
        <<"lbry-blob-store">> => #{ <<"node">> => Server },
        <<"lbry-tx-store">> => #{ <<"lbry-proxy-node">> => Server }
    }.

create_claim_tx(PrevHash, Name, Claim) ->
    Script = <<
        16#b5,
        (script_push(Name))/binary,
        (script_push(Claim))/binary,
        16#6d, 16#75
    >>,
    <<1:32/little-signed,
        1,
        PrevHash/binary,
        0:32/little,
        0,
        16#ffffffff:32/little,
        1,
        0:64/little,
        (byte_size(Script)),
        Script/binary,
        0:32/little>>.

proto_field(Number, Value) ->
    FieldKey = (Number bsl 3) bor 2,
    <<(proto_varint(FieldKey))/binary,
        (proto_varint(byte_size(Value)))/binary,
        Value/binary>>.

proto_varint(Value) when Value < 16#80 ->
    <<Value>>;
proto_varint(Value) ->
    <<((Value band 16#7f) bor 16#80), (proto_varint(Value bsr 7))/binary>>.

script_push(Value) when byte_size(Value) < 16#4c ->
    <<(byte_size(Value)), Value/binary>>;
script_push(Value) when byte_size(Value) =< 16#ff ->
    <<16#4c, (byte_size(Value)), Value/binary>>.

compact_signature(PrivKey, Digest) ->
    der_to_compact(
        crypto:sign(ecdsa, sha256, {digest, Digest}, [PrivKey, secp256k1])
    ).

der_to_compact(
    <<16#30, _TotalLen, 16#02, RLen, R0:RLen/binary, 16#02, SLen, S0:SLen/binary>>
) ->
    <<(fixed_int(R0))/binary, (fixed_int(S0))/binary>>.

fixed_int(Int) ->
    Trimmed = trim_zeroes(Int),
    Padding = 32 - byte_size(Trimmed),
    <<0:(Padding * 8), Trimmed/binary>>.

trim_zeroes(<<0, Rest/binary>> = Int) when byte_size(Int) > 32 ->
    trim_zeroes(Rest);
trim_zeroes(Int) ->
    Int.

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
