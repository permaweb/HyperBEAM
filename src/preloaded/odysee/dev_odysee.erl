%%% @doc Odysee source commitment device.
%%%
%%% This device is the first native-commitment layer for the Odysee bridge. It
%%% does not replace the specialized Odysee compatibility devices; it commits to
%%% the normalized public messages they produce and verifies the source-specific
%%% invariants that are available for each message type.
-module(dev_odysee).
-implements(<<"odysee@1.0">>).
-export([
    info/1,
    index/3,
    resolve/3,
    claim/3,
    source/3,
    transaction/3,
    descriptor/3,
    blob/3,
    verify_blobs/3,
    stream_graph/3,
    verified_stream/3,
    range/3,
    media/3,
    bytes/3,
    commit/3,
    verify/3,
    to_hint/3
]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee@1.0">>).
-define(LBRY_BLOB_COMMITMENT_DEVICE, <<"lbry-blob@1.0">>).
-define(LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE, <<"lbry-stream-descriptor@1.0">>).
-define(LBRY_CLAIM_COMMITMENT_DEVICE, <<"lbry-claim@1.0">>).
-define(LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE, <<"lbry-claim-output@1.0">>).
-define(LBRY_TRANSACTION_COMMITMENT_DEVICE, <<"lbry-transaction@1.0">>).
-define(DEFAULT_RANGE_SIZE, 1048576).

%% @doc Return the public device API.
info(_Opts) ->
    #{
        exports => [
            <<"index">>,
            <<"resolve">>,
            <<"claim">>,
            <<"source">>,
            <<"transaction">>,
            <<"descriptor">>,
            <<"blob">>,
            <<"verify-blobs">>,
            <<"stream-graph">>,
            <<"verified-stream">>,
            <<"range">>,
            <<"media">>,
            <<"bytes">>,
            <<"commit">>,
            <<"verify">>,
            <<"to-hint">>
        ]
    }.

index(_Base, _Req, _Opts) ->
    {ok, #{
        <<"device">> => ?DEVICE,
        <<"paths">> => #{
            <<"resolve">> => [<<"claim-id">>, <<"name">>, <<"url">>],
            <<"claim">> => [<<"claim-id">>, <<"name">>, <<"url">>],
            <<"source">> => [<<"id">>, <<"native-id">>, <<"kind">>],
            <<"transaction">> => [<<"txid">>],
            <<"descriptor">> => [<<"sd-hash">>],
            <<"blob">> => [<<"hash">>],
            <<"verify-blobs">> => [<<"sd-hash">>, <<"limit">>],
            <<"stream-graph">> => [<<"claim-id">>, <<"name">>, <<"url">>],
            <<"verified-stream">> => [<<"claim-id">>, <<"name">>, <<"url">>],
            <<"range">> => [<<"sd-hash">>, <<"start">>, <<"end">>],
            <<"media">> => [
                <<"sd-hash">>,
                <<"claim-id">>,
                <<"name">>,
                <<"url">>,
                <<"range">>
            ],
            <<"bytes">> => [<<"sd-hash">>],
            <<"commit">> => [<<"type">>],
            <<"verify">> => [<<"commitment-ids">>]
        },
        <<"source-kinds">> =>
            [
                <<"blob">>,
                <<"stream-descriptor">>,
                <<"transaction">>,
                <<"claim-output">>,
                <<"claim">>
            ]
    }}.

%% @doc Preserve nested source messages in bundle form when verifying.
to_hint(_Base, Req, _Opts) ->
    {ok, Req#{ <<"bundle">> => true }}.

resolve(Base, Req, Opts) ->
    with_target(Base, Req, Opts, fun(Target) ->
        map_result(
            hb_lbry_proxy:claim(Target, Opts),
            fun(Claim) -> stream_claim(Claim, Opts) end
        )
    end).

claim(Base, Req, Opts) ->
    resolve(Base, Req, Opts).

%% @doc Read a committed public Odysee/LBRY source object by native identifier.
source(Base, Req, Opts) ->
    case native_source_path(Base, Req, Opts) of
        {ok, Kind, Keys} ->
            ?event(odysee_device,
                {source_read, {kind, Kind}, {keys, Keys}},
                Opts
            ),
            case read_source_key(Keys, Opts) of
                {ok, Msg} ->
                    {ok, Msg};
                {error, Reason} ->
                    ?event(odysee_device,
                        {source_read_failed, {keys, Keys}, {reason, Reason}},
                        Opts
                    ),
                    error_response(Reason);
                {failure, Reason} ->
                    ?event(odysee_device,
                        {source_read_failed, {keys, Keys}, {reason, Reason}},
                        Opts
                    ),
                    error_response({failure, Reason})
            end;
        {error, Reason} ->
            ?event(odysee_device, {source_key_rejected, {reason, Reason}}, Opts),
            error_response(Reason)
    end.

read_source_key(Key, Opts) when is_binary(Key) ->
    read_source_key([Key], Opts);
read_source_key([Key | Rest], Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_store:read(Store, Key, maps:without([<<"store">>, store], Opts)) of
        {error, not_found} when Rest =/= [] ->
            read_source_key(Rest, Opts);
        not_found when Rest =/= [] ->
            read_source_key(Rest, Opts);
        Result ->
            Result
    end.

transaction(Base, Req, Opts) ->
    with_txid(Base, Req, Opts, fun(TxID) ->
        map_result(
            hb_lbry_bridge:transaction_message(TxID, Opts),
            fun(Tx) ->
                RawHex = hb_util:to_hex(hb_maps:get(<<"raw">>, Tx, <<>>, Opts)),
                View = hb_message:uncommitted(Tx, Opts),
                hex_binaries(View#{ <<"raw">> => RawHex, <<"raw-hex">> => RawHex })
            end
        )
    end).

descriptor(Base, Req, Opts) ->
    with_sd_hash(Base, Req, Opts, fun(SDHash) ->
        map_result(
            hb_lbry_bridge:descriptor_message(SDHash, Opts),
            fun(Descriptor) ->
                codec(<<"lbry-stream-descriptor@1.0">>, Descriptor, #{}, Opts)
            end
        )
    end).

blob(Base, Req, Opts) ->
    with_blob_hash(Base, Req, Opts, fun(Hash) ->
        map_result(
            hb_lbry_bridge:blob(Hash, Opts),
            fun(Bytes) ->
                #{
                    <<"status">> => 200,
                    <<"content-type">> => <<"application/octet-stream">>,
                    <<"content-length">> => byte_size(Bytes),
                    <<"blob-hash">> => hb_util:to_lower(Hash),
                    <<"body">> => Bytes
                }
            end
        )
    end).

verify_blobs(Base, Req, Opts) ->
    with_sd_hash(Base, Req, Opts, fun(SDHash) ->
        Limit = integer_param(Base, Req, <<"limit">>, 1, Opts),
        map_result(
            hb_lbry_bridge:verify_blobs(SDHash, Limit, Opts),
            fun(Result) -> normalize_verify_blobs(Result, Opts) end
        )
    end).

stream_graph(Base, Req, Opts) ->
    with_target(Base, Req, Opts, fun(Target) ->
        map_result(
            hb_lbry_bridge:stream_graph(Target, Opts),
            fun(StreamGraph) ->
                hex_binaries(normalize_stream_graph(StreamGraph, Target, Opts))
            end
        )
    end).

verified_stream(Base, Req, Opts) ->
    with_target(Base, Req, Opts, fun(Target) ->
        map_result(
            hb_lbry_bridge:verified_stream(Target, Opts),
            fun(VerifiedStream) ->
                hex_binaries(normalize_verified_stream(VerifiedStream, Target, Opts))
            end
        )
    end).

range(Base, Req, Opts) ->
    with_sd_hash(Base, Req, Opts, fun(SDHash) ->
        case explicit_range(Base, Req, Opts) of
            {ok, Start, End} -> range_response(SDHash, Start, End, Opts);
            Error -> error_map(Error)
        end
    end).

media(Base, Req, Opts) ->
    with_media_source(Base, Req, Opts, fun(Source) ->
        case request_range(Base, Req, Opts) of
            {ok, Start, End} ->
                case bounded_range(Source, Start, End) of
                    {ok, BoundedStart, BoundedEnd} ->
                        range_response(Source, BoundedStart, BoundedEnd, Opts);
                    Error ->
                        error_map(Error)
                end;
            Error ->
                error_map(Error)
        end
    end).

bytes(Base, Req, Opts) ->
    with_sd_hash(Base, Req, Opts, fun(SDHash) ->
        map_result(
            hb_lbry_bridge:reassemble_stream(SDHash, Opts),
            fun(Result) ->
                #{
                    <<"status">> => 200,
                    <<"content-type">> => <<"application/octet-stream">>,
                    <<"accept-ranges">> => <<"bytes">>,
                    <<"sd-hash">> => hb_util:to_lower(SDHash),
                    <<"byte-size">> => maps:get(<<"byte-size">>, Result),
                    <<"descriptor">> =>
                        codec(
                            <<"lbry-stream-descriptor@1.0">>,
                            maps:get(<<"descriptor">>, Result),
                            #{},
                            Opts
                        ),
                    <<"body">> => maps:get(<<"bytes">>, Result)
                }
            end
        )
    end).

%% @doc Add an Odysee source commitment to a normalized Odysee message.
commit(Base, Req, Opts) ->
    safe(fun() ->
        Type = commitment_type(Base, Req, Opts),
        Msg = remove_matching_commitments(Base, Type, Opts),
        CommittedKeys = committed_keys(Type, Msg, Opts),
        Digest = source_digest(Msg, CommittedKeys, Opts),
        Commitment0 = #{
            <<"commitment-device">> => ?DEVICE,
            <<"type">> => Type,
            <<"signature">> => Digest,
            <<"committed">> => hb_util:list_to_numbered_message(CommittedKeys),
            <<"source-digest">> => Digest,
            <<"verification-tier">> =>
                integer_to_binary(verification_tier(Type, Msg, Opts)),
            <<"verification-limitations">> => verification_limitations(Type, Msg, Opts)
        },
        Commitment = add_evidence(Type, Msg, Commitment0, Opts),
        ID = commitment_id(Type, Commitment),
        Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
        {ok, Msg#{ <<"commitments">> => Commitments#{ ID => Commitment } }}
    end).

%% @doc Verify an Odysee source commitment.
verify(Base, Req, Opts) ->
    safe(fun() ->
        Type = hb_maps:get(<<"type">>, Req, source, Opts),
        CommittedKeys =
            hb_util:message_to_ordered_list(
                hb_maps:get(<<"committed">>, Req, #{}, Opts),
                Opts
            ),
        ExpectedDigest = hb_maps:get(<<"source-digest">>, Req, not_found, Opts),
        ActualDigest = source_digest(Base, CommittedKeys, Opts),
        Signature = hb_maps:get(<<"signature">>, Req, not_found, Opts),
        DigestValid =
            ExpectedDigest =/= not_found
                andalso ExpectedDigest =:= ActualDigest,
        SignatureValid = Signature =:= ExpectedDigest,
        {ok, DigestValid andalso SignatureValid andalso verify_type(Type, Base, Req, Opts)}
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

native_source_path(Base, Req, Opts) ->
    case source_param_values(Base, Req, Opts) of
        [] ->
            {error, missing_native_source_id};
        Values ->
            case source_param_key(Values) of
                {ok, ID} -> classify_source_param(ID, source_kind(Base, Req, Opts));
                Error -> Error
            end
    end.

classify_source_param(ID0, Kind) ->
    ID = normalize_source_id(ID0),
    case ID of
        <<"odysee/", _/binary>> ->
            {ok, <<"path">>, [ID]};
        <<"lbry/blob/", Hash/binary>> ->
            source_path_result(blob_source_path(Hash));
        <<"lbry/blob-id/", Hash/binary>> ->
            source_path_result(blob_source_path(Hash));
        <<"lbry/descriptor/", Hash/binary>> ->
            source_path_result(descriptor_source_path(Hash));
        <<"lbry/descriptor-id/", Hash/binary>> ->
            source_path_result(descriptor_source_path(Hash));
        <<"lbry/stream-descriptor/", Hash/binary>> ->
            source_path_result(descriptor_source_path(Hash));
        <<"lbry/transaction/", TxID/binary>> ->
            source_path_result(transaction_source_path(TxID));
        <<"lbry/tx/", TxID/binary>> ->
            source_path_result(transaction_source_path(TxID));
        <<"lbry/claim-output/", Rest/binary>> ->
            source_path_result(claim_output_source_path(Rest));
        <<"lbry/claim-proof/", Rest/binary>> ->
            source_path_result(claim_output_source_path(Rest));
        _ when Kind =:= <<"claim">>; Kind =:= <<"stream">>;
                Kind =:= <<"channel">>; Kind =:= <<"comment">>;
                Kind =:= <<"comment-reaction">>; Kind =:= <<"file-view-count">>;
                Kind =:= <<"file-reaction">>; Kind =:= <<"subscription-count">> ->
            source_path_result(classify_bare_native_source_id(ID, Kind));
        _ ->
            native_source_key_path(ID)
    end.

native_source_key_path(ID) ->
    case classify_native_source_key(hb_util:to_lower(ID)) of
        {ok, <<"blob">>, Hash} ->
            {ok, <<"blob">>, [<<"odysee/blob/", Hash/binary>>, Hash]};
        {ok, <<"transaction">>, TxID} ->
            {ok, <<"transaction">>, [<<"odysee/transaction/", TxID/binary>>, TxID]};
        {ok, <<"outpoint">>, Key} ->
            [TxID, Nout] = binary:split(Key, <<":">>),
            {ok,
                <<"claim-output">>,
                [<<"odysee/claim-proof/", TxID/binary, "/", Nout/binary>>, Key]};
        _ ->
            {error, unsupported_native_source_id}
    end.

source_path_result({ok, Kind, Key}) ->
    {ok, Kind, [Key]};
source_path_result(_Error) ->
    {error, unsupported_native_source_id}.

source_kind(Base, Req, Opts) ->
    case first_message_value([<<"kind">>, <<"source-kind">>, <<"type">>], [Req, Base], Opts) of
        Kind when is_binary(Kind) -> hb_ao:normalize_key(Kind);
        _ -> not_found
    end.

first_message_value(_Keys, [], _Opts) ->
    not_found;
first_message_value(Keys, [Msg | Rest], Opts) when is_map(Msg) ->
    case first_value(Keys, Msg, Opts) of
        not_found -> first_message_value(Keys, Rest, Opts);
        Value -> Value
    end;
first_message_value(Keys, [_ | Rest], Opts) ->
    first_message_value(Keys, Rest, Opts).

classify_bare_native_source_id(ID, <<"blob">>) ->
    blob_source_path(ID);
classify_bare_native_source_id(ID, <<"stream-descriptor">>) ->
    descriptor_source_path(ID);
classify_bare_native_source_id(ID, <<"descriptor">>) ->
    descriptor_source_path(ID);
classify_bare_native_source_id(ID, <<"claim">>) ->
    surface_source_path(<<"claim">>, <<"odysee/claim-id/">>, ID);
classify_bare_native_source_id(ID, <<"stream">>) ->
    surface_source_path(<<"stream">>, <<"odysee/stream-id/">>, ID);
classify_bare_native_source_id(ID, <<"channel">>) ->
    surface_source_path(<<"channel">>, <<"odysee/channel/">>, ID);
classify_bare_native_source_id(ID, <<"comment">>) ->
    surface_source_path(<<"comment">>, <<"odysee/comment/">>, ID);
classify_bare_native_source_id(ID, <<"comment-reaction">>) ->
    surface_source_path(
        <<"comment-reaction">>,
        <<"odysee/comment-reaction/">>,
        ID
    );
classify_bare_native_source_id(ID, <<"file-view-count">>) ->
    surface_source_path(
        <<"file-view-count">>,
        <<"odysee/file-view-count/">>,
        ID
    );
classify_bare_native_source_id(ID, <<"file-reaction">>) ->
    surface_source_path(
        <<"file-reaction">>,
        <<"odysee/file-reaction/">>,
        ID
    );
classify_bare_native_source_id(ID, <<"subscription-count">>) ->
    surface_source_path(
        <<"subscription-count">>,
        <<"odysee/subscription-count/">>,
        ID
    );
classify_bare_native_source_id(ID, <<"transaction">>) ->
    transaction_source_path(ID);
classify_bare_native_source_id(ID, <<"tx">>) ->
    transaction_source_path(ID);
classify_bare_native_source_id(ID, Kind)
        when Kind =:= <<"claim-output">>; Kind =:= <<"claim-proof">>; Kind =:= <<"outpoint">> ->
    claim_output_source_path(ID);
classify_bare_native_source_id(ID, _Kind) ->
    case {byte_size(ID), binary:match(ID, <<":">>)} of
        {96, nomatch} -> blob_source_path(ID);
        {64, nomatch} -> transaction_source_path(ID);
        {40, nomatch} -> claim_source_path(ID);
        {_Size, _Colon} -> claim_output_source_path(ID)
    end.

blob_source_path(Hash0) ->
    Hash = normalize_hex(Hash0),
    case valid_hex(Hash, 96) of
        true -> {ok, <<"blob">>, <<"odysee/blob/", Hash/binary>>};
        false -> {error, invalid_blob_hash}
    end.

descriptor_source_path(Hash0) ->
    Hash = normalize_hex(Hash0),
    case valid_hex(Hash, 96) of
        true -> {ok, <<"stream-descriptor">>, <<"odysee/descriptor/", Hash/binary>>};
        false -> {error, invalid_descriptor_hash}
    end.

transaction_source_path(TxID0) ->
    TxID = normalize_hex(TxID0),
    case valid_hex(TxID, 64) of
        true -> {ok, <<"transaction">>, <<"odysee/transaction/", TxID/binary>>};
        false -> {error, invalid_txid}
    end.

claim_source_path(ClaimID0) ->
    ClaimID = normalize_hex(ClaimID0),
    case valid_hex(ClaimID, 40) of
        true -> {ok, <<"claim">>, <<"odysee/claim-id/", ClaimID/binary>>};
        false -> {error, invalid_claim_id}
    end.

surface_source_path(Type, Prefix, ID) when is_binary(ID), byte_size(ID) > 0 ->
    {ok, Type, <<Prefix/binary, ID/binary>>};
surface_source_path(_Type, _Prefix, _ID) ->
    {error, missing_native_source_id}.

claim_output_source_path(Rest0) ->
    Rest = normalize_source_id(Rest0),
    Parts =
        case binary:split(Rest, <<"/">>) of
            [PathTxID, PathNOut] -> [PathTxID, PathNOut];
            _ -> binary:split(Rest, <<":">>)
        end,
    case Parts of
        [TxID0, NOut0] ->
            TxID = normalize_hex(TxID0),
            case {valid_hex(TxID, 64), non_negative_integer(NOut0)} of
                {true, {ok, NOut}} ->
                    {ok,
                        <<"claim-output">>,
                        <<"odysee/claim-proof/", TxID/binary, "/", (integer_to_binary(NOut))/binary>>};
                {false, _} ->
                    {error, invalid_txid};
                {_, Error} ->
                    Error
            end;
        _ ->
            {error, invalid_claim_output_id}
    end.

valid_hex(Hex, Size) when is_binary(Hex), byte_size(Hex) =:= Size ->
    try byte_size(binary:decode_hex(Hex)) =:= Size div 2
    catch _:_ -> false
    end;
valid_hex(_Hex, _Size) ->
    false.

non_negative_integer(Bin) when is_binary(Bin) ->
    try
        Int = binary_to_integer(Bin),
        case Int >= 0 of
            true -> {ok, Int};
            false -> {error, invalid_nout}
        end
    catch _:_ ->
        {error, invalid_nout}
    end;
non_negative_integer(Int) when is_integer(Int), Int >= 0 ->
    {ok, Int};
non_negative_integer(_Value) ->
    {error, invalid_nout}.

normalize_source_id(<<"/", Rest/binary>>) ->
    normalize_source_id(Rest);
normalize_source_id(ID) when is_binary(ID) ->
    ID.

commitment_type(Base, Req, Opts) ->
    case hb_maps:get(<<"type">>, Req, not_found, Opts) of
        not_found -> infer_type(Base, Opts);
        Type -> Type
    end.

infer_type(Base, Opts) ->
    case hb_maps:get(<<"device">>, Base, not_found, Opts) of
        <<"odysee-claim@1.0">> -> <<"claim">>;
        <<"odysee-stream-descriptor@1.0">> -> <<"stream-descriptor">>;
        <<"lbry-stream-descriptor@1.0">> -> <<"stream-descriptor">>;
        <<"odysee-channel@1.0">> -> <<"channel">>;
        <<"odysee-comment@1.0">> -> <<"comment">>;
        <<"odysee-reaction@1.0">> -> <<"comment-reaction">>;
        <<"odysee-file@1.0">> -> <<"file-view-count">>;
        <<"odysee-file-reaction@1.0">> -> <<"file-reaction">>;
        <<"odysee-subscription@1.0">> -> <<"subscription-count">>;
        <<"odysee-blob@1.0">> -> <<"blob">>;
        <<"lbry-blob@1.0">> -> <<"blob">>;
        <<"odysee-claim-proof@1.0">> -> <<"claim-proof">>;
        <<"lbry-claim-output@1.0">> -> <<"claim-proof">>;
        <<"odysee-stream@1.0">> ->
            case hb_maps:get(<<"view">>, Base, not_found, Opts) of
                <<"verified-stream">> -> <<"stream-attestation">>;
                _ -> <<"stream">>
            end;
        _ -> <<"source">>
    end.

remove_matching_commitments(Msg, Type, Opts) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    Filtered =
        hb_maps:filter(
            fun(_ID, Commitment) ->
                not (
                    hb_maps:get(<<"commitment-device">>, Commitment, not_found, Opts) =:= ?DEVICE
                        andalso hb_maps:get(<<"type">>, Commitment, not_found, Opts) =:= Type
                )
            end,
            Commitments,
            Opts
        ),
    case map_size(Filtered) of
        0 -> hb_maps:without([<<"commitments">>], Msg, Opts);
        _ -> Msg#{ <<"commitments">> => Filtered }
    end.

committed_keys(Type, Msg, Opts) ->
    Candidates =
        case Type of
            <<"claim">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"canonical-url">>,
                    <<"value-type">>,
                    <<"claim-store-path">>,
                    <<"claim-proof-store-path">>,
                    <<"txid">>,
                    <<"nout">>,
                    <<"height">>,
                    <<"claim-op">>
                ];
            <<"stream-descriptor">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"sd-hash">>,
                    <<"stream-hash">>,
                    <<"stream-name">>,
                    <<"key">>,
                    <<"suggested-file-name">>,
                    <<"blob-store-paths">>,
                    <<"descriptor-store-path">>
                ];
            <<"channel">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"channel-id">>,
                    <<"channel-name">>,
                    <<"identity-type">>,
                    <<"value-type">>,
                    <<"canonical-url">>,
                    <<"permanent-url">>,
                    <<"short-url">>,
                    <<"public-key">>,
                    <<"public-key-id">>,
                    <<"signature-valid">>,
                    <<"committer-format">>,
                    <<"ao-committer">>,
                    <<"claim-store-path">>,
                    <<"channel-store-path">>,
                    <<"claim-proof-store-path">>,
                    <<"txid">>,
                    <<"nout">>,
                    <<"height">>,
                    <<"claim-op">>
                ];
            <<"claim-proof">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"txid">>,
                    <<"nout">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"claim-op">>,
                    <<"claim-value-size">>,
                    <<"claim-value-hash">>,
                    <<"claim-proof-store-path">>,
                    <<"claim-script-valid">>,
                    <<"txid-valid">>,
                    <<"nout-valid">>,
                    <<"claim-id-valid">>,
                    <<"claim-name-valid">>,
                    <<"claim-value-hash-valid">>,
                    <<"valid">>,
                    <<"proof-tier">>
                ];
            <<"comment">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"comment-id">>,
                    <<"claim-id">>,
                    <<"channel-id">>,
                    <<"channel-name">>,
                    <<"comment">>,
                    <<"ancestors">>,
                    <<"comment-store-path">>,
                    <<"claim-store-path">>,
                    <<"channel-store-path">>,
                    <<"signature">>,
                    <<"signing-ts">>,
                    <<"public-key">>
                ];
            <<"comment-reaction">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"comment-id">>,
                    <<"comment-ids">>,
                    <<"my_reactions">>,
                    <<"others_reactions">>,
                    <<"comment-reaction-store-path">>
                ];
            <<"stream-attestation">> ->
                [
                    <<"device">>,
                    <<"view">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"target">>,
                    <<"sd-hash">>,
                    <<"signed-sd-hash">>,
                    <<"signature-verification">>,
                    <<"channel-verification">>,
                    <<"descriptor-verification">>
                ];
            <<"stream">> ->
                [
                    <<"device">>,
                    <<"claim-id">>,
                    <<"claim-name">>,
                    <<"sd-hash">>,
                    <<"media-type">>,
                    <<"source-hash">>,
                    <<"source-size">>,
                    <<"stream-store-path">>,
                    <<"claim-store-path">>,
                    <<"descriptor-store-path">>,
                    <<"channel-store-path">>,
                    <<"claim-proof-store-path">>,
                    <<"txid">>,
                    <<"nout">>,
                    <<"claim-height">>,
                    <<"claim-op">>,
                    <<"streaming-url">>,
                    <<"download-url">>
                ];
            <<"blob">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"blob-hash">>,
                    <<"blob-size">>,
                    <<"blob-store-path">>
                ];
            <<"file-view-count">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-ids">>,
                    <<"view-counts">>,
                    <<"file-view-count-store-path">>
                ];
            <<"file-reaction">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-ids">>,
                    <<"my_reactions">>,
                    <<"others_reactions">>,
                    <<"file-reaction-store-path">>
                ];
            <<"subscription-count">> ->
                [
                    <<"device">>,
                    <<"content-type">>,
                    <<"body">>,
                    <<"claim-id">>,
                    <<"claim-ids">>,
                    <<"sub-counts">>,
                    <<"subscription-count-store-path">>
                ];
            _ ->
                hb_maps:keys(Msg, Opts)
        end,
    lists:sort(
        [
            Key
        ||
            Key0 <- Candidates,
            Key <- [hb_ao:normalize_key(Key0)],
            not lists:member(Key, [<<"commitments">>, <<"priv">>]),
            hb_maps:is_key(Key, Msg, Opts)
        ]
    ).

source_digest(Msg, Keys, Opts) ->
    hb_util:human_id(
        crypto:hash(
            sha256,
            term_to_binary([
                {Key, canonical(hb_maps:get(Key, Msg, not_found, Opts), Opts)}
            ||
                Key <- Keys
            ])
        )
    ).

canonical(Map, Opts) when is_map(Map) ->
    case encoded_list(Map, Opts) of
        true ->
            canonical(ordered_message_values(Map, Opts), Opts);
        false ->
            [
                {Key, canonical(hb_maps:get(Key, Map, not_found, Opts), Opts)}
            ||
                Key <- lists:sort(hb_maps:keys(Map, Opts) -- [<<"ao-types">>])
            ]
    end;
canonical(List, Opts) when is_list(List) ->
    [canonical(Value, Opts) || Value <- List];
canonical(Value, _Opts) when is_integer(Value) ->
    integer_to_binary(Value);
canonical(true, _Opts) ->
    <<"true">>;
canonical(false, _Opts) ->
    <<"false">>;
canonical(Value, _Opts) ->
    Value.

encoded_list(Map, Opts) ->
    try
        AOTypes = dev_structured:decode_ao_types(Map, Opts),
        dev_structured:is_list_from_ao_types(AOTypes, Opts)
            andalso hb_util:is_ordered_list(Map, Opts)
    catch
        _:_ -> false
    end.

commitment_id(Type, Commitment) ->
    hb_util:human_id(crypto:hash(sha256, term_to_binary({?DEVICE, Type, Commitment}))).

verification_tier(<<"stream-descriptor">>, _Msg, _Opts) -> 2;
verification_tier(<<"blob">>, _Msg, _Opts) -> 2;
verification_tier(<<"comment">>, _Msg, _Opts) -> 2;
verification_tier(<<"claim-proof">>, _Msg, _Opts) -> 2;
verification_tier(<<"stream-attestation">>, _Msg, _Opts) -> 1;
verification_tier(_Type, _Msg, _Opts) -> 1.

verification_limitations(<<"claim">>, _Msg, _Opts) ->
    [<<"raw LBRY transaction proof is not included in this commitment">>];
verification_limitations(<<"channel">>, _Msg, _Opts) ->
    [<<"raw LBRY transaction proof is not included in this commitment">>];
verification_limitations(<<"stream">>, _Msg, _Opts) ->
    [<<"stream claim signature validity must be verified by a stream attestation commitment">>];
verification_limitations(<<"stream-attestation">>, _Msg, _Opts) ->
    [<<"stream claim signature validity is currently SDK/resolve-attested">>];
verification_limitations(<<"claim-proof">>, _Msg, _Opts) ->
    [<<"block inclusion proof is not included in this transaction-output proof">>];
verification_limitations(_Type, _Msg, _Opts) ->
    [].

add_evidence(<<"claim">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"claim-name">>, hb_maps:get(<<"claim-name">>, Msg, not_found, Opts)},
            {<<"value-type">>, hb_maps:get(<<"value-type">>, Msg, not_found, Opts)},
            {<<"claim-store-path">>, hb_maps:get(<<"claim-store-path">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)},
            {<<"txid">>, hb_maps:get(<<"txid">>, Msg, not_found, Opts)},
            {<<"nout">>, hb_maps:get(<<"nout">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"stream-descriptor">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"sd-hash">>, hb_maps:get(<<"sd-hash">>, Msg, not_found, Opts)},
            {<<"stream-hash">>, hb_maps:get(<<"stream-hash">>, Msg, not_found, Opts)},
            {<<"descriptor-store-path">>, hb_maps:get(<<"descriptor-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"channel">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"channel-id">>, hb_maps:get(<<"channel-id">>, Msg, not_found, Opts)},
            {<<"channel-name">>, hb_maps:get(<<"channel-name">>, Msg, not_found, Opts)},
            {<<"claim-store-path">>, hb_maps:get(<<"claim-store-path">>, Msg, not_found, Opts)},
            {<<"channel-store-path">>, hb_maps:get(<<"channel-store-path">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)},
            {<<"txid">>, hb_maps:get(<<"txid">>, Msg, not_found, Opts)},
            {<<"nout">>, hb_maps:get(<<"nout">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"claim-proof">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"txid">>, hb_maps:get(<<"txid">>, Msg, not_found, Opts)},
            {<<"nout">>, hb_maps:get(<<"nout">>, Msg, not_found, Opts)},
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"stream">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"sd-hash">>, hb_maps:get(<<"sd-hash">>, Msg, not_found, Opts)},
            {<<"stream-store-path">>, hb_maps:get(<<"stream-store-path">>, Msg, not_found, Opts)},
            {<<"claim-store-path">>, hb_maps:get(<<"claim-store-path">>, Msg, not_found, Opts)},
            {<<"descriptor-store-path">>, hb_maps:get(<<"descriptor-store-path">>, Msg, not_found, Opts)},
            {<<"channel-store-path">>, hb_maps:get(<<"channel-store-path">>, Msg, not_found, Opts)},
            {<<"claim-proof-store-path">>, hb_maps:get(<<"claim-proof-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"comment">>, Msg, Commitment, Opts) ->
    Comment = comment_source(Msg, Opts),
    put_optionals(
        [
            {<<"comment-id">>, first_value([<<"comment-id">>], Comment, Opts)},
            {<<"channel-id">>, first_value([<<"channel-id">>], Comment, Opts)},
            {<<"comment-store-path">>, first_value([<<"comment-store-path">>], Comment, Opts)},
            {<<"claim-store-path">>, first_value([<<"claim-store-path">>], Comment, Opts)},
            {<<"channel-store-path">>, first_value([<<"channel-store-path">>], Comment, Opts)}
        ],
        Commitment
    );
add_evidence(<<"comment-reaction">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"comment-id">>, hb_maps:get(<<"comment-id">>, Msg, not_found, Opts)},
            {<<"comment-reaction-store-path">>,
                hb_maps:get(<<"comment-reaction-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"blob">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"blob-hash">>, hb_maps:get(<<"blob-hash">>, Msg, not_found, Opts)},
            {<<"blob-store-path">>, hb_maps:get(<<"blob-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"file-view-count">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"file-view-count-store-path">>,
                hb_maps:get(<<"file-view-count-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"file-reaction">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"file-reaction-store-path">>,
                hb_maps:get(<<"file-reaction-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(<<"subscription-count">>, Msg, Commitment, Opts) ->
    put_optionals(
        [
            {<<"claim-id">>, hb_maps:get(<<"claim-id">>, Msg, not_found, Opts)},
            {<<"subscription-count-store-path">>,
                hb_maps:get(<<"subscription-count-store-path">>, Msg, not_found, Opts)}
        ],
        Commitment
    );
add_evidence(_Type, _Msg, Commitment, _Opts) ->
    Commitment.

put_optionals([], Msg) ->
    Msg;
put_optionals([{_Key, not_found} | Rest], Msg) ->
    put_optionals(Rest, Msg);
put_optionals([{Key, Value} | Rest], Msg) ->
    put_optionals(Rest, Msg#{ Key => Value }).

verify_type(<<"claim">>, Base, _Req, Opts) ->
    ClaimID = hb_maps:get(<<"claim-id">>, Base, not_found, Opts),
    BodyValid = case decode_body(Base, Opts) of
        {ok, Source} ->
            ClaimID =/= not_found
                andalso contains_claim_id(Source, ClaimID, Opts);
        _ ->
            ClaimID =/= not_found
    end,
    BodyValid;
verify_type(<<"stream-descriptor">>, Base, _Req, Opts) ->
    case hb_ao:raw(<<"odysee-stream-descriptor@1.0">>, <<"decode">>, #{}, Base, Opts) of
        {ok, Desc} ->
            hb_maps:get(<<"sd-hash">>, Desc, not_found, Opts)
                =:= hb_maps:get(<<"sd-hash">>, Base, not_found, Opts);
        _ ->
            false
    end;
verify_type(<<"channel">>, Base, _Req, Opts) ->
    ChannelID = hb_maps:get(<<"channel-id">>, Base, not_found, Opts),
    case decode_body(Base, Opts) of
        {ok, Claim} ->
            ChannelID =/= not_found
                andalso ChannelID =:= first_value([<<"claim_id">>, <<"claim-id">>], Claim, Opts)
                andalso first_value([<<"value_type">>, <<"value-type">>], Claim, Opts) =:= <<"channel">>
                andalso hb_maps:get(<<"public-key">>, Base, not_found, Opts) =/= not_found;
        _ ->
            false
    end;
verify_type(<<"claim-proof">>, Base, _Req, Opts) ->
    case hb_ao:raw(<<"odysee-claim-proof@1.0">>, <<"verify">>, Base, #{}, Opts) of
        {ok, #{ <<"valid">> := true }} -> true;
        _ -> false
    end;
verify_type(<<"stream-attestation">>, Base, _Req, Opts) ->
    hb_maps:get(<<"valid">>, Base, false, Opts) =:= true;
verify_type(<<"stream">>, Base, _Req, Opts) ->
    hb_maps:get(<<"claim-id">>, Base, not_found, Opts) =/= not_found
        andalso hb_maps:get(<<"sd-hash">>, Base, not_found, Opts) =/= not_found;
verify_type(<<"comment">>, Base, _Req, Opts) ->
    Comment = comment_source(Base, Opts),
    case hb_maps:get(<<"public-key">>, Comment, not_found, Opts) of
        not_found ->
            false;
        PublicKey ->
            case hb_ao:raw(
                <<"odysee-comment@1.0">>,
                <<"verify-signature">>,
                Comment,
                #{ <<"public-key">> => PublicKey },
                Opts
            ) of
                {ok, #{ <<"is-valid">> := true }} -> true;
                _ -> false
            end
    end;
verify_type(<<"comment-reaction">>, Base, _Req, Opts) ->
    summary_store_path_valid(
        Base,
        <<"comment-id">>,
        <<"comment-ids">>,
        <<"comment-reaction-store-path">>,
        <<"odysee/comment-reaction/">>,
        Opts
    );
verify_type(<<"blob">>, Base, _Req, Opts) ->
    case {hb_maps:get(<<"blob-hash">>, Base, not_found, Opts), hb_maps:get(<<"body">>, Base, not_found, Opts)} of
        {Hash, Body} when is_binary(Hash), is_binary(Body) ->
            normalize_hex(Hash) =:= sha384_hex(Body);
        _ ->
            false
    end;
verify_type(<<"file-view-count">>, Base, _Req, Opts) ->
    summary_store_path_valid(
        Base,
        <<"claim-id">>,
        <<"claim-ids">>,
        <<"file-view-count-store-path">>,
        <<"odysee/file-view-count/">>,
        Opts
    );
verify_type(<<"file-reaction">>, Base, _Req, Opts) ->
    summary_store_path_valid(
        Base,
        <<"claim-id">>,
        <<"claim-ids">>,
        <<"file-reaction-store-path">>,
        <<"odysee/file-reaction/">>,
        Opts
    );
verify_type(<<"subscription-count">>, Base, _Req, Opts) ->
    summary_store_path_valid(
        Base,
        <<"claim-id">>,
        <<"claim-ids">>,
        <<"subscription-count-store-path">>,
        <<"odysee/subscription-count/">>,
        Opts
    );
verify_type(_Type, _Base, _Req, _Opts) ->
    true.

comment_source(Base, Opts) ->
    case hb_maps:get(<<"comment">>, Base, not_found, Opts) of
        Comment when is_map(Comment) -> Comment;
        _ -> Base
    end.

first_value([], _Map, _Opts) ->
    not_found;
first_value([Key | Rest], Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> first_value(Rest, Map, Opts);
        Value -> Value
    end.

decode_body(Msg, Opts) ->
    case hb_maps:get(<<"body">>, Msg, not_found, Opts) of
        Body when is_binary(Body) ->
            try {ok, hb_json:decode(Body)}
            catch _:_ -> {error, invalid_json}
            end;
        _ ->
            {error, body_not_found}
    end.

normalized_surface_matches(Device, Base, Keys, Opts) ->
    case hb_ao:raw(Device, <<"normalize">>, Base, #{}, Opts) of
        {ok, Normalized} ->
            lists:all(
                fun(Key) ->
                    hb_maps:get(Key, Base, not_found, Opts)
                        =:= hb_maps:get(Key, Normalized, not_found, Opts)
                end,
                Keys
            )
                orelse surface_keys_present(Base, Keys, Opts);
        _ ->
            surface_keys_present(Base, Keys, Opts)
    end.

surface_keys_present(Base, Keys, Opts) ->
    lists:all(
        fun(Key) ->
            hb_maps:get(Key, Base, not_found, Opts) =/= not_found
        end,
        Keys
    ).

summary_store_path_valid(Base, IDKey, IDsKey, PathKey, Prefix, Opts) ->
    case {
        hb_maps:get(IDKey, Base, not_found, Opts),
        hb_maps:get(IDsKey, Base, not_found, Opts),
        hb_maps:get(PathKey, Base, not_found, Opts)
    } of
        {ID, IDs, Path} when is_binary(ID), is_binary(Path) ->
            lists:member(ID, ordered_values(IDs, Opts))
                andalso Path =:= <<Prefix/binary, ID/binary>>;
        _ ->
            false
    end.

ordered_values(List, _Opts) when is_list(List) ->
    List;
ordered_values(Map, Opts) when is_map(Map) ->
    try ordered_message_values(Map, Opts)
    catch _:_ -> []
    end;
ordered_values(_Value, _Opts) ->
    [].

ordered_message_values(Map, Opts) ->
    hb_util:message_to_ordered_list(
        hb_maps:without([<<"ao-types">>], Map, Opts),
        Opts
    ).

sha384_hex(Bin) ->
    hb_util:to_hex(crypto:hash(sha384, Bin)).

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

contains_claim_id(Source, ClaimID, Opts) when is_map(Source) ->
    first_value([<<"claim_id">>, <<"claim-id">>], Source, Opts) =:= ClaimID
        orelse lists:any(
            fun(Value) -> contains_claim_id(Value, ClaimID, Opts) end,
            maps:values(Source)
        );
contains_claim_id(Source, ClaimID, Opts) when is_list(Source) ->
    lists:any(fun(Value) -> contains_claim_id(Value, ClaimID, Opts) end, Source);
contains_claim_id(_Source, _ClaimID, _Opts) ->
    false.

range_response(SDHash, Start, End, Opts) when is_binary(SDHash) ->
    range_response(#{ <<"sd-hash">> => SDHash }, Start, End, Opts);
range_response(Source, Start, End, Opts) ->
    SDHash = maps:get(<<"sd-hash">>, Source),
    map_result(
        hb_lbry_bridge:stream_range(SDHash, Start, End, Opts),
        fun(Result) ->
            Body = maps:get(<<"bytes">>, Result),
            ActualEnd = maps:get(<<"end">>, Result),
            Total = maps:get(<<"byte-size">>, Source, undefined),
            ?event(odysee_device,
                {media_slice,
                    {sd_hash, hb_util:to_lower(SDHash)},
                    {start, Start},
                    {actual_end, ActualEnd},
                    {requested_end, maps:get(<<"requested-end">>, Result)},
                    {size, byte_size(Body)}},
                Opts
            ),
            maps:merge(
                #{
                    <<"status">> => 206,
                    <<"content-type">> =>
                        maps:get(
                            <<"content-type">>,
                            Source,
                            <<"application/octet-stream">>
                        ),
                    <<"content-length">> => byte_size(Body),
                    <<"accept-ranges">> => <<"bytes">>,
                    <<"content-range">> => content_range(Start, ActualEnd, Total),
                    <<"sd-hash">> => hb_util:to_lower(SDHash),
                    <<"start">> => Start,
                    <<"end">> => ActualEnd,
                    <<"requested-end">> => maps:get(<<"requested-end">>, Result),
                    <<"body">> => Body
                },
                maps:from_list(response_metadata(Source, Total))
            )
        end
    ).

response_metadata(Source, Total) ->
    [
        {K, V}
     ||
        {K, V} <- [
            {<<"byte-size">>, Total},
            {
                <<"byte-size-source">>,
                maps:get(<<"byte-size-source">>, Source, undefined)
            },
            {<<"claim-id">>, maps:get(<<"claim-id">>, Source, undefined)},
            {<<"filename">>, maps:get(<<"filename">>, Source, undefined)}
        ],
        V =/= undefined
    ].

normalize_stream_graph(StreamGraph, Target, Opts) ->
    Claim = maps:get(<<"claim">>, StreamGraph),
    ParsedTx = maps:get(<<"parsed-tx">>, StreamGraph),
    ClaimOutput = claim_output(ParsedTx, claim_nout(Claim)),
    #{
        <<"device">> => ?DEVICE,
        <<"view">> => <<"stream-graph">>,
        <<"target">> => Target,
        <<"stream">> => stream_claim(stream_claim_source(Claim, ClaimOutput), Opts),
        <<"descriptor">> =>
            codec(
                <<"lbry-stream-descriptor@1.0">>,
                maps:get(<<"descriptor">>, StreamGraph),
                #{},
                Opts
            ),
        <<"transaction">> =>
            codec(
                <<"lbry-transaction@1.0">>,
                maps:get(<<"raw">>, ParsedTx),
                #{},
                Opts
            ),
        <<"sd-hash">> => maps:get(<<"sd-hash">>, StreamGraph),
        <<"txid">> => maps:get(<<"txid">>, StreamGraph)
    }.

normalize_verified_stream(VerifiedStream, Target, Opts) ->
    StreamGraph = normalize_stream_graph(VerifiedStream, Target, Opts),
    Claim = maps:get(<<"claim">>, VerifiedStream),
    ParsedTx = maps:get(<<"parsed-tx">>, VerifiedStream),
    ClaimOutput = claim_output(ParsedTx, claim_nout(Claim)),
    StreamEvidence = maps:get(<<"stream-evidence">>, VerifiedStream),
    ChannelEvidence = maps:get(<<"channel-evidence">>, VerifiedStream),
    ClaimOp = maps:get(<<"claim-op">>, StreamEvidence),
    ChannelClaimOp = maps:get(<<"claim-op">>, ChannelEvidence),
    StreamStrength = maps:get(<<"claim-proof-strength">>, StreamEvidence),
    ChannelStrength = maps:get(<<"claim-proof-strength">>, ChannelEvidence),
    StreamGraph#{
        <<"view">> => <<"verified-stream">>,
        <<"signed-sd-hash">> => maps:get(<<"signed-sd-hash">>, VerifiedStream),
        <<"claim-op">> => ClaimOp,
        <<"channel-claim-op">> => ChannelClaimOp,
        <<"claim-proof-strength">> => StreamStrength,
        <<"channel-claim-proof-strength">> => ChannelStrength,
        <<"proof-strength">> =>
            combined_proof_strength(StreamStrength, ChannelStrength),
        <<"claim-envelope">> =>
            case ClaimOutput of
                not_found ->
                    not_found;
                Output ->
                    codec(
                        <<"lbry-claim@1.0">>,
                        maps:get(<<"claim">>, Output),
                        #{},
                        Opts
                    )
            end,
        <<"channel">> =>
            codec(
                <<"lbry-channel@1.0">>,
                maps:get(<<"channel-evidence">>, VerifiedStream),
                #{},
                Opts
            ),
        <<"attestation">> =>
            codec(
                <<"lbry-channel-attestation@1.0">>,
                maps:get(<<"attestation">>, VerifiedStream),
                #{},
                Opts
            )
    }.

combined_proof_strength(StreamStrength, ChannelStrength) ->
    Ranked = [<<"asserted">>, <<"ancestor-derived">>, <<"hash-derived">>],
    hd(
        [
            Strength
         ||
            Strength <- Ranked,
            Strength == StreamStrength orelse Strength == ChannelStrength
        ]
    ).

normalize_verify_blobs(Result, Opts) ->
    Result#{
        <<"device">> => ?DEVICE,
        <<"descriptor">> =>
            codec(
                <<"lbry-stream-descriptor@1.0">>,
                maps:get(<<"descriptor">>, Result),
                #{},
                Opts
            )
    }.

hex_binaries(Map) when is_map(Map) ->
    maps:map(fun(_Key, Value) -> hex_binaries(Value) end, Map);
hex_binaries(List) when is_list(List) ->
    [hex_binaries(Value) || Value <- List];
hex_binaries(Bin) when is_binary(Bin) ->
    case unicode:characters_to_binary(Bin, utf8) of
        Bin -> Bin;
        _ -> hb_util:to_hex(Bin)
    end;
hex_binaries(Other) ->
    Other.

stream_claim(Claim, Opts) ->
    codec(<<"lbry-stream@1.0">>, Claim, #{}, Opts).

stream_claim_source(Claim, #{ <<"claim-envelope">> := Envelope }) ->
    Claim#{ <<"claim-envelope">> => Envelope };
stream_claim_source(Claim, _ClaimOutput) ->
    Claim.

codec(Device, Msg, Req, Opts) ->
    hb_message:uncommitted(
        hb_message:convert(
            Msg,
            <<"structured@1.0">>,
            Req#{ <<"device">> => Device },
            Opts
        ),
        Opts
    ).

with_target(Base, Req, Opts, Fun) ->
    case param(Base, Req, target_keys(), Opts) of
        {ok, Target} -> {ok, Fun(Target)};
        Error -> error_response(Error)
    end.

with_txid(Base, Req, Opts, Fun) ->
    case param(Base, Req, [<<"txid">>, <<"tx-id">>, <<"tx_id">>], Opts) of
        {ok, TxID} -> {ok, Fun(TxID)};
        Error -> error_response(Error)
    end.

with_sd_hash(Base, Req, Opts, Fun) ->
    case param(Base, Req, [<<"sd-hash">>, <<"sd_hash">>, <<"sdhash">>], Opts) of
        {ok, SDHash} -> {ok, Fun(SDHash)};
        Error -> error_response(Error)
    end.

with_blob_hash(Base, Req, Opts, Fun) ->
    case param(Base, Req, [<<"hash">>, <<"blob-hash">>, <<"blob_hash">>], Opts) of
        {ok, Hash} -> {ok, Fun(Hash)};
        Error -> error_response(Error)
    end.

source_param_values(Base, Req, Opts) ->
    [
        Value
     ||
        Value <- [
            hb_maps:get(<<"id">>, Req, not_found, Opts),
            hb_maps:get(<<"id">>, Base, not_found, Opts),
            hb_maps:get(<<"native-id">>, Req, not_found, Opts),
            hb_maps:get(<<"native-id">>, Base, not_found, Opts)
        ],
        Value =/= not_found
    ].

source_param_key(Values) ->
    Normalized =
        lists:usort([
            hb_util:to_lower(Value)
         ||
            Value <- Values,
            is_binary(Value),
            byte_size(Value) > 0
        ]),
    case {length(Normalized), length(Values)} of
        {0, _} -> {error, unsupported_native_source_id};
        {1, N} when N == length(Values) -> {ok, hd(Normalized)};
        {_, N} when N =/= length(Values) -> {error, unsupported_native_source_id};
        _ -> {error, conflicting_native_source_id}
    end.

classify_native_source_key(Key) ->
    case binary:split(Key, <<":">>) of
        [TxID, NoutBin] ->
            case {valid_hex_bytes(TxID, 32), parse_source_nout(NoutBin)} of
                {true, {ok, Nout}} ->
                    {ok,
                        <<"outpoint">>,
                        <<TxID/binary, ":", (integer_to_binary(Nout))/binary>>};
                _ ->
                    {error, unsupported_native_source_id}
            end;
        [Single] ->
            case {valid_hex_bytes(Single, 48), valid_hex_bytes(Single, 32)} of
                {true, _} -> {ok, <<"blob">>, Single};
                {_, true} -> {ok, <<"transaction">>, Single};
                _ -> {error, unsupported_native_source_id}
            end
    end.

valid_hex_bytes(Bin, Bytes) when is_binary(Bin), byte_size(Bin) == Bytes * 2 ->
    try binary:decode_hex(Bin) of
        Decoded -> byte_size(Decoded) == Bytes
    catch
        _:_ -> false
    end;
valid_hex_bytes(_Bin, _Bytes) ->
    false.

with_media_source(Base, Req, Opts, Fun) ->
    case param(Base, Req, [<<"sd-hash">>, <<"sd_hash">>, <<"sdhash">>], Opts) of
        {ok, SDHash} ->
            {Size, SizeSource} = exact_stream_size(SDHash, undefined, Opts),
            {ok,
                Fun(optional_source_fields(#{
                    <<"sd-hash">> => SDHash,
                    <<"byte-size">> => Size,
                    <<"byte-size-source">> => SizeSource
                }))};
        _ ->
            with_target(Base, Req, Opts, fun(Target) ->
                map_result(
                    hb_lbry_proxy:claim(Target, Opts),
                    fun(Claim) ->
                        case media_source_from_claim(Claim, Opts) of
                            {ok, Source} -> Fun(Source#{ <<"target">> => Target });
                            Error -> error_map(Error)
                        end
                    end
                )
            end)
    end.

target_keys() ->
    [
        <<"claim-id">>,
        <<"claim_id">>,
        <<"claim">>,
        <<"url">>,
        <<"name">>,
        <<"target">>
    ].

param(Base, Req, Keys, Opts) ->
    case hb_maps:get_first(param_paths(Base, Req, Keys), not_found, Opts) of
        Value when is_binary(Value), byte_size(Value) > 0 -> {ok, Value};
        _ -> {error, {missing_required, hd(Keys)}}
    end.

param_paths(Base, Req, Keys) ->
    lists:flatmap(
        fun(Key) ->
            [{Req, Key}, {Base, Key}]
        end,
        Keys
    ).

integer_param(Base, Req, Key, Default, Opts) ->
    case hb_maps:get_first([{Req, Key}, {Base, Key}], Default, Opts) of
        Int when is_integer(Int) ->
            Int;
        Bin when is_binary(Bin) ->
            try binary_to_integer(Bin) of
                Parsed -> Parsed
            catch
                _:_ -> Default
            end;
        _ ->
            Default
    end.

explicit_range(Base, Req, Opts) ->
    case {
        integer_param(Base, Req, <<"start">>, undefined, Opts),
        integer_param(Base, Req, <<"end">>, undefined, Opts)
    } of
        {Start, End} when is_integer(Start), is_integer(End), End >= Start ->
            {ok, Start, End};
        _ ->
            {error, missing_range}
    end.

request_range(Base, Req, Opts) ->
    case explicit_range(Base, Req, Opts) of
        {ok, _, _} = Explicit ->
            Explicit;
        _ ->
            case param(Base, Req, [<<"range">>], Opts) of
                {ok, Range} -> parse_range(Range, Opts);
                _ -> {ok, 0, default_range_size(Opts) - 1}
            end
    end.

parse_range(<<"bytes=", Spec/binary>>, Opts) ->
    case binary:split(Spec, <<"-">>) of
        [StartBin, EndBin] when byte_size(StartBin) > 0 ->
            maybe
                {ok, Start} ?= parse_nonnegative_integer(StartBin),
                {ok, End} ?= range_end(Start, EndBin, Opts),
                true ?= End >= Start orelse {error, invalid_range},
                {ok, Start, End}
            end;
        _ ->
            {error, invalid_range}
    end;
parse_range(_, _Opts) ->
    {error, invalid_range}.

range_end(Start, <<>>, Opts) ->
    {ok, Start + default_range_size(Opts) - 1};
range_end(_Start, EndBin, _Opts) ->
    parse_nonnegative_integer(EndBin).

default_range_size(Opts) ->
    hb_maps:get(<<"odysee-default-range-size">>, Opts, ?DEFAULT_RANGE_SIZE, Opts).

parse_nonnegative_integer(Bin) ->
    try binary_to_integer(Bin) of
        Int when Int >= 0 -> {ok, Int};
        _ -> {error, invalid_integer}
    catch
        _:_ -> {error, invalid_integer}
    end.

parse_source_nout(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    case all_digits(Bin) of
        true -> parse_nonnegative_integer(Bin);
        false -> {error, invalid_integer}
    end;
parse_source_nout(_Bin) ->
    {error, invalid_integer}.

all_digits(<<>>) ->
    true;
all_digits(<<Char, Rest/binary>>) when Char >= $0, Char =< $9 ->
    all_digits(Rest);
all_digits(_Bin) ->
    false.

bounded_range(Source, Start, End) ->
    case maps:get(<<"byte-size">>, Source, undefined) of
        undefined ->
            {ok, Start, End};
        Size when Start < Size ->
            {ok, Start, min(End, Size - 1)};
        _ ->
            {error, invalid_range}
    end.

content_range(Start, End, undefined) ->
    content_range(Start, End, <<"*">>);
content_range(Start, End, Total) when is_integer(Total) ->
    content_range(Start, End, integer_to_binary(Total));
content_range(Start, End, Total) ->
    iolist_to_binary([
        <<"bytes ">>,
        integer_to_binary(Start),
        <<"-">>,
        integer_to_binary(End),
        <<"/">>,
        Total
    ]).

media_source_from_claim(Claim, Opts) ->
    case hb_util:deep_get([<<"value">>, <<"source">>], Claim, #{}) of
        Source when is_map(Source) ->
            case maps:get(<<"sd_hash">>, Source, undefined) of
                undefined ->
                    {error, missing_sd_hash};
                SDHash ->
                    ClaimSize = integer_value(maps:get(<<"size">>, Source, undefined)),
                    {Size, SizeSource} = exact_stream_size(SDHash, ClaimSize, Opts),
                    {ok,
                        optional_source_fields(#{
                            <<"sd-hash">> => SDHash,
                            <<"claim-id">> => maps:get(<<"claim_id">>, Claim, undefined),
                            <<"byte-size">> => Size,
                            <<"byte-size-source">> => SizeSource,
                            <<"content-type">> =>
                                maps:get(<<"media_type">>, Source, undefined),
                            <<"filename">> => maps:get(<<"name">>, Source, undefined)
                        })}
            end;
        _ ->
            {error, missing_source}
    end.

optional_source_fields(Source) ->
    maps:filter(
        fun(_Key, Value) ->
            Value =/= undefined
        end,
        Source
    ).

integer_value(Value) when is_integer(Value) ->
    Value;
integer_value(Value) when is_binary(Value) ->
    try binary_to_integer(Value) of
        Int -> Int
    catch
        _:_ -> undefined
    end;
integer_value(_Value) ->
    undefined.

exact_stream_size(SDHash, Fallback, Opts) ->
    case hb_lbry_bridge:stream_size(SDHash, Opts) of
        {ok, #{ <<"byte-size">> := Size }} -> {Size, <<"descriptor-last-blob">>};
        _ when Fallback =/= undefined -> {Fallback, <<"claim-source-size">>};
        _ -> {undefined, undefined}
    end.

claim_nout(Claim) ->
    case maps:get(<<"nout">>, Claim, undefined) of
        Nout when is_integer(Nout) ->
            Nout;
        Nout when is_binary(Nout) ->
            try binary_to_integer(Nout) of
                Int -> Int
            catch
                _:_ -> undefined
            end;
        _ ->
            undefined
    end.

claim_output(_Tx, undefined) ->
    not_found;
claim_output(Tx, Nout) ->
    case [
        Output
     ||
        Output <- maps:get(<<"outputs">>, Tx, []),
        maps:get(<<"nout">>, Output, undefined) == Nout,
        maps:is_key(<<"claim">>, Output)
    ] of
        [Output | _] -> Output;
        [] -> not_found
    end.

map_result({ok, Value}, Fun) ->
    Fun(Value);
map_result({error, Reason}, _Fun) ->
    error_map(Reason);
map_result({failure, Reason}, _Fun) ->
    error_map({failure, Reason}).

error_response({error, Reason}) ->
    {ok, error_map(Reason)};
error_response(Reason) ->
    {ok, error_map(Reason)}.

error_map(Reason) ->
    #{
        <<"status">> => status_for(Reason),
        <<"content-type">> => <<"text/plain">>,
        <<"error">> => error_term(Reason),
        <<"body">> => hb_util:bin(io_lib:format("~p", [Reason]))
    }.

status_for({missing_required, _}) -> 400;
status_for({error, Reason}) -> status_for(Reason);
status_for(missing_native_source_id) -> 400;
status_for(unsupported_native_source_id) -> 400;
status_for(conflicting_native_source_id) -> 400;
status_for(missing_range) -> 416;
status_for(invalid_range) -> 416;
status_for(invalid_integer) -> 400;
status_for(not_found) -> 404;
status_for({http_status, 403, _}) -> 403;
status_for(protected) -> 403;
status_for(protected_content) -> 403;
status_for({hash_mismatch, _, _}) -> 502;
status_for({txid_mismatch, _, _}) -> 502;
status_for({failure, _}) -> 502;
status_for({invalid_attestation, _, _}) -> 502;
status_for({channel_binding_mismatch, _, _}) -> 502;
status_for(invalid_claim_signature) -> 502;
status_for(native_commitment_failure) -> 502;
status_for(_) -> 500.

error_term(Reason) when is_atom(Reason) ->
    hb_util:bin(Reason);
error_term({error, Reason}) ->
    error_term(Reason);
error_term({Reason, _}) when is_atom(Reason) ->
    hb_util:bin(Reason);
error_term({Reason, _, _}) when is_atom(Reason) ->
    hb_util:bin(Reason);
error_term(_Reason) ->
    <<"error">>.

-ifdef(TEST).

claim_commitment_verifies_test() ->
    Claim = #{
        <<"claim_id">> => <<"abc123">>,
        <<"name">> => <<"example">>,
        <<"value">> => #{ <<"title">> => <<"Example">> }
    },
    Msg = #{
        <<"device">> => <<"odysee-claim@1.0">>,
        <<"claim-id">> => <<"abc123">>,
        <<"claim-name">> => <<"example">>,
        <<"claim">> => Claim,
        <<"value">> => hb_maps:get(<<"value">>, Claim, #{})
    },
    {ok, Committed} = commit(Msg, #{ <<"type">> => <<"claim">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    Tampered = Committed#{ <<"claim-id">> => <<"bad">> },
    ?assertEqual(false, hb_message:verify(Tampered, source_verify_req(Tampered), #{})).

descriptor_commitment_verifies_test() ->
    {Descriptor, SDHash} = descriptor_fixture(),
    {ok, Desc} =
        hb_ao:raw(
            <<"odysee-stream-descriptor@1.0">>,
            <<"decode">>,
            #{},
            #{ <<"body">> => Descriptor, <<"sd-hash">> => SDHash },
            #{}
    ),
    {ok, Committed} = commit(Desc, #{ <<"type">> => <<"stream-descriptor">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"sd-hash">> => <<"bad">> },
            source_verify_req(Committed),
            #{}
        )
    ).

channel_commitment_verifies_test() ->
    Channel = channel_fixture(),
    {ok, Committed} = commit(Channel, #{ <<"type">> => <<"channel">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"channel-id">> => <<"bad">> },
            source_verify_req(Committed),
            #{}
        )
    ).

comment_commitment_verifies_test() ->
    Comment = comment_fixture(),
    {ok, Committed} = commit(Comment, #{ <<"type">> => <<"comment">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    Nested = hb_maps:get(<<"comment">>, Committed, #{}),
    Tampered = Committed#{ <<"comment">> => Nested#{ <<"comment">> => <<"tampered">> } },
    ?assertEqual(false, hb_message:verify(Tampered, source_verify_req(Committed), #{})).

comment_reaction_commitment_verifies_test() ->
    Reaction = comment_reaction_fixture(),
    {ok, Committed} = commit(Reaction, #{ <<"type">> => <<"comment-reaction">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"comment-id">> => <<"wrong">> },
            source_verify_req(Committed),
            #{}
        )
    ).

blob_commitment_verifies_test() ->
    {Blob, Body, _Hash} = blob_fixture(),
    {ok, Committed} = commit(Blob, #{ <<"type">> => <<"blob">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"body">> => <<Body/binary, "!">> },
            source_verify_req(Committed),
            #{}
        )
    ).

file_view_count_commitment_verifies_test() ->
    Counts = file_view_count_fixture(),
    {ok, Committed} = commit(Counts, #{ <<"type">> => <<"file-view-count">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"view-counts">> => [0] },
            source_verify_req(Committed),
            #{}
        )
    ).

file_reaction_commitment_verifies_test() ->
    Reaction = file_reaction_fixture(),
    {ok, Committed} = commit(Reaction, #{ <<"type">> => <<"file-reaction">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"claim-id">> => <<"wrong">> },
            source_verify_req(Committed),
            #{}
        )
    ).

subscription_count_commitment_verifies_test() ->
    Counts = subscription_count_fixture(),
    {ok, Committed} = commit(Counts, #{ <<"type">> => <<"subscription-count">> }, #{}),
    ?assert(hb_message:verify(Committed, source_verify_req(Committed), #{})),
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"claim-id">> => <<"wrong">> },
            source_verify_req(Committed),
            #{}
        )
    ).

store_fixture_read_attaches_source_commitment_test() ->
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/claim/test">> => claim_fixture()
        }
    },
    {ok, Msg} = hb_store:read(Store, <<"odysee/claim/test">>, #{}),
    ?assert(hb_maps:is_key(<<"commitments">>, Msg, #{})),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

store_fixture_read_keeps_verifiable_commitment_test() ->
    {ok, Committed} = commit(claim_fixture(), #{ <<"type">> => <<"claim">> }, #{}),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/claim/test">> => Committed
        }
    },
    {ok, Msg} = hb_store:read(Store, <<"/odysee/claim/test">>, #{}),
    ?assertEqual(false, hb_maps:is_key(<<"claim">>, Msg, #{})),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

store_fixture_read_commits_channel_comment_blob_and_descriptor_test() ->
    {Blob, _Body, BlobHash} = blob_fixture(),
    {Descriptor, SDHash} = descriptor_fixture(),
    {ok, Desc} =
        hb_ao:raw(
            <<"odysee-stream-descriptor@1.0">>,
            <<"decode">>,
            #{},
            #{ <<"body">> => Descriptor, <<"sd-hash">> => SDHash },
            #{}
        ),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/channel/channel-1">> => channel_fixture(),
            <<"odysee/comment/vector-1">> => comment_fixture(),
            <<"odysee/blob/", BlobHash/binary>> => Blob,
            <<"odysee/descriptor/", SDHash/binary>> => Desc
        }
    },
    {ok, Channel} = hb_store:read(Store, <<"odysee/channel/channel-1">>, #{}),
    ?assert(hb_message:verify(Channel, source_verify_req(Channel), #{})),
    {ok, Comment} = hb_store:read(Store, <<"odysee/comment/vector-1">>, #{}),
    ?assert(hb_message:verify(Comment, source_verify_req(Comment), #{})),
    {ok, BlobMsg} = hb_store:read(Store, <<"odysee/blob/", BlobHash/binary>>, #{}),
    ?assert(has_commitment_device(BlobMsg, ?LBRY_BLOB_COMMITMENT_DEVICE)),
    ?assertEqual(?LBRY_BLOB_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, BlobMsg, #{})),
    ?assert(hb_message:verify(BlobMsg, source_verify_req(BlobMsg), #{})),
    {ok, DescMsg} = hb_store:read(Store, <<"odysee/descriptor/", SDHash/binary>>, #{}),
    ?assert(has_commitment_device(DescMsg, ?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE)),
    ?assertEqual(
        ?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE,
        hb_maps:get(<<"device">>, DescMsg, #{})
    ),
    ?assertEqual(SDHash, hb_maps:get(<<"sd-hash">>, DescMsg, #{})),
    ?assert(hb_message:verify(DescMsg, source_verify_req(DescMsg), #{})).

store_fixture_read_commits_surface_summaries_test() ->
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/comment-reaction/vector-1">> => comment_reaction_fixture(),
            <<"odysee/file-view-count/claim-1">> => file_view_count_fixture(),
            <<"odysee/file-reaction/claim-1">> => file_reaction_fixture(),
            <<"odysee/subscription-count/channel-1">> => subscription_count_fixture()
        }
    },
    {ok, CommentReaction} = hb_store:read(Store, <<"odysee/comment-reaction/vector-1">>, #{}),
    ?assertEqual(<<"vector-1">>, hb_maps:get(<<"comment-id">>, CommentReaction, #{})),
    ?assert(hb_message:verify(CommentReaction, source_verify_req(CommentReaction), #{})),
    {ok, ViewCounts} = hb_store:read(Store, <<"odysee/file-view-count/claim-1">>, #{}),
    ?assertEqual(<<"claim-1">>, hb_maps:get(<<"claim-id">>, ViewCounts, #{})),
    ?assert(hb_message:verify(ViewCounts, source_verify_req(ViewCounts), #{})),
    {ok, FileReaction} = hb_store:read(Store, <<"odysee/file-reaction/claim-1">>, #{}),
    ?assertEqual(<<"claim-1">>, hb_maps:get(<<"claim-id">>, FileReaction, #{})),
    ?assert(hb_message:verify(FileReaction, source_verify_req(FileReaction), #{})),
    {ok, SubscriptionCounts} =
        hb_store:read(Store, <<"odysee/subscription-count/channel-1">>, #{}),
    ?assertEqual(<<"channel-1">>, hb_maps:get(<<"claim-id">>, SubscriptionCounts, #{})),
    ?assert(hb_message:verify(SubscriptionCounts, source_verify_req(SubscriptionCounts), #{})).

store_fixture_read_commits_transaction_test() ->
    {Raw, TxID} = transaction_fixture(),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/transaction/", TxID/binary>> => #{
                <<"device">> => <<"lbry-transaction@1.0">>,
                <<"content-type">> => <<"application/vnd.lbry.transaction">>,
                <<"body">> => Raw,
                <<"txid">> => TxID,
                <<"tx-size">> => byte_size(Raw)
            }
        }
    },
    {ok, Msg} = hb_store:read(Store, <<"odysee/transaction/", TxID/binary>>, #{}),
    ?assert(has_commitment_device(Msg, ?LBRY_TRANSACTION_COMMITMENT_DEVICE)),
    ?assertEqual(?LBRY_TRANSACTION_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, Msg, #{})),
    ?assertEqual(TxID, hb_maps:get(<<"txid">>, Msg, #{})),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

source_reads_native_blob_and_transaction_ids_test() ->
    {Blob, _Body, BlobHash} = blob_fixture(),
    {Raw, TxID} = transaction_fixture(),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/blob/", BlobHash/binary>> => Blob,
            <<"odysee/transaction/", TxID/binary>> => #{
                <<"device">> => <<"lbry-transaction@1.0">>,
                <<"content-type">> => <<"application/vnd.lbry.transaction">>,
                <<"body">> => Raw,
                <<"txid">> => TxID,
                <<"tx-size">> => byte_size(Raw)
            }
        }
    },
    {ok, BlobMsg} = source(#{}, #{ <<"id">> => BlobHash }, #{ <<"store">> => Store }),
    ?assertEqual(?LBRY_BLOB_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, BlobMsg, #{})),
    ?assert(hb_message:verify(BlobMsg, source_verify_req(BlobMsg), #{})),
    {ok, TxMsg} = source(#{}, #{ <<"id">> => TxID }, #{ <<"store">> => Store }),
    ?assertEqual(?LBRY_TRANSACTION_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, TxMsg, #{})),
    ?assertEqual(TxID, hb_maps:get(<<"txid">>, TxMsg, #{})),
    ?assert(hb_message:verify(TxMsg, source_verify_req(TxMsg), #{})).

source_reads_prefixed_lbry_source_ids_test() ->
    {Descriptor, SDHash} = descriptor_fixture(),
    {ok, Desc} =
        hb_ao:raw(
            <<"odysee-stream-descriptor@1.0">>,
            <<"decode">>,
            #{},
            #{ <<"body">> => Descriptor, <<"sd-hash">> => SDHash },
            #{}
        ),
    {TxHex, TxID, _ClaimID} = proof_tx_fixture(<<"example">>, <<"raw claim">>),
    Raw = binary:decode_hex(TxHex),
    {ok, ClaimOutput} = hb_lbry_commitment:claim_output_message(Raw, 0),
    Store = #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/descriptor/", SDHash/binary>> => Desc,
            <<"odysee/claim-proof/", TxID/binary, "/0">> => ClaimOutput
        }
    },
    {ok, DescMsg} =
        source(
            #{},
            #{ <<"id">> => <<"lbry/stream-descriptor/", SDHash/binary>> },
            #{ <<"store">> => Store }
        ),
    ?assertEqual(?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, DescMsg, #{})),
    ?assertEqual(SDHash, hb_maps:get(<<"sd-hash">>, DescMsg, #{})),
    ?assert(hb_message:verify(DescMsg, source_verify_req(DescMsg), #{})),
    {ok, ClaimMsg} =
        source(
            #{},
            #{ <<"id">> => <<"lbry/claim-output/", TxID/binary, "/0">> },
            #{ <<"store">> => Store }
        ),
    ?assertEqual(?LBRY_CLAIM_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, ClaimMsg, #{})),
    ?assertEqual(TxID, hb_maps:get(<<"txid">>, ClaimMsg, #{})),
    ?assertEqual(0, hb_maps:get(<<"nout">>, ClaimMsg, #{})),
    ?assert(hb_message:verify(ClaimMsg, source_verify_req(ClaimMsg), #{})).

source_reads_store_path_surface_objects_test() ->
    Store = surface_fixture_store(),
    Opts = #{ <<"store">> => Store },
    {ok, Claim} =
        source(#{}, #{ <<"id">> => <<"odysee/claim-id/abc123">> }, Opts),
    ?assertEqual(<<"abc123">>, hb_maps:get(<<"claim-id">>, Claim, #{})),
    ?assert(has_commitment_device(Claim, ?DEVICE)),
    ?assert(hb_message:verify(Claim, source_verify_req(Claim), #{})),
    {ok, Channel} =
        source(#{}, #{ <<"id">> => <<"odysee/channel/channel-1">> }, Opts),
    ?assertEqual(<<"channel-1">>, hb_maps:get(<<"channel-id">>, Channel, #{})),
    ?assert(has_commitment_device(Channel, ?DEVICE)),
    ?assert(hb_message:verify(Channel, source_verify_req(Channel), #{})),
    {ok, Comment} =
        source(#{}, #{ <<"id">> => <<"odysee/comment/vector-1">> }, Opts),
    ?assertEqual(<<"vector-1">>, hb_maps:get(<<"comment-id">>, Comment, #{})),
    ?assert(has_commitment_device(Comment, ?DEVICE)),
    ?assert(hb_message:verify(Comment, source_verify_req(Comment), #{})).

source_reads_kinded_surface_objects_test() ->
    Store = surface_fixture_store(),
    Opts = #{ <<"store">> => Store },
    Cases = [
        {<<"comment-reaction">>, <<"vector-1">>, <<"comment-id">>, <<"vector-1">>},
        {<<"file-view-count">>, <<"claim-1">>, <<"claim-id">>, <<"claim-1">>},
        {<<"file-reaction">>, <<"claim-1">>, <<"claim-id">>, <<"claim-1">>},
        {<<"subscription-count">>, <<"channel-1">>, <<"claim-id">>, <<"channel-1">>}
    ],
    lists:foreach(
        fun({Kind, ID, Field, Expected}) ->
            {ok, Msg} = source(#{}, #{ <<"id">> => ID, <<"kind">> => Kind }, Opts),
            ?assertEqual(Expected, hb_maps:get(Field, Msg, #{})),
            ?assert(has_commitment_device(Msg, ?DEVICE)),
            ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{}))
        end,
        Cases
    ).

store_live_blob_read_fetches_and_commits_hash_test() ->
    Body = <<"encrypted blob">>,
    BlobHash = sha384_hex(Body),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/blob", blob, {200, Body}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"blob-url-template">> => <<MockServer/binary, "/blob?hash={hash}">>
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/blob-id/", BlobHash/binary>>, #{}),
        ?assertEqual(BlobHash, hb_maps:get(<<"blob-hash">>, Msg, #{})),
        ?assert(has_commitment_device(Msg, ?LBRY_BLOB_COMMITMENT_DEVICE)),
        ?assertEqual(?LBRY_BLOB_COMMITMENT_DEVICE, hb_maps:get(<<"device">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(blob, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_claim_proof_read_fetches_and_commits_test() ->
    {TxHex, TxID, ClaimID} = proof_tx_fixture(<<"example">>, <<"raw claim">>),
    Raw = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{
            <<"txid">> => TxID,
            <<"hex">> => TxHex,
            <<"outputs">> => []
        },
        <<"id">> => 1
    }),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", transaction_show, {200, Raw}},
            {"/api/v1/proxy", transaction_show, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"lbry-proxy-url">> => MockServer
        },
        Path = <<"odysee/claim-proof/", TxID/binary, "/0">>,
        {ok, Msg} = hb_store:read(Store, Path, #{}),
        ?assertEqual(ClaimID, hb_maps:get(<<"claim-id">>, Msg, #{})),
        ?assertEqual(true, hb_maps:get(<<"valid">>, Msg, #{})),
        ?assert(has_commitment_device(Msg, ?LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE)),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        LbryStore = Store#{ <<"store-module">> => hb_store_lbry_claim_output },
        {ok, LbryMsg} = hb_store:read(LbryStore, <<TxID/binary, ":0">>, #{}),
        ?assertEqual(ClaimID, hb_maps:get(<<"claim-id">>, LbryMsg, #{})),
        ?assert(has_commitment_device(LbryMsg, ?LBRY_CLAIM_COMMITMENT_DEVICE)),
        ?assert(hb_message:verify(LbryMsg, source_verify_req(LbryMsg), #{})),
        [_Request1, _Request2] = hb_mock_server:get_requests(transaction_show, 2, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_claim_id_read_searches_and_commits_test() ->
    Claim = claim_source_fixture(),
    Raw = search_response([Claim]),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", claim_search, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"lbry-proxy-url">> => MockServer
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/claim-id/abc123">>, #{}),
        ?assertEqual(<<"abc123">>, hb_maps:get(<<"claim-id">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(claim_search, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_stream_id_read_searches_and_commits_test() ->
    Claim = stream_source_fixture(),
    Raw = search_response([Claim]),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", stream_search, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"lbry-proxy-url">> => MockServer
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/stream-id/stream-1">>, #{}),
        ?assertEqual(<<"stream-1">>, hb_maps:get(<<"claim-id">>, Msg, #{})),
        ?assertEqual(<<"odysee/claim-id/stream-1">>, hb_maps:get(<<"claim-store-path">>, Msg, #{})),
        ?assertEqual(<<"odysee/descriptor/", (hb_maps:get(<<"sd-hash">>, Msg, #{}))/binary>>, hb_maps:get(<<"descriptor-store-path">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(stream_search, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_channel_read_searches_and_commits_test() ->
    Claim = channel_source_fixture(),
    Raw = search_response([Claim]),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", channel_search, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"lbry-proxy-url">> => MockServer
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/channel-id/channel-1">>, #{}),
        ?assertEqual(<<"channel-1">>, hb_maps:get(<<"channel-id">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(channel_search, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

store_live_comment_read_fetches_and_commits_test() ->
    Comment = comment_source_fixture(),
    Raw = hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => Comment,
        <<"id">> => 1
    }),
    {ok, MockServer, ServerHandle} =
        hb_mock_server:start([
            {"/", comment_by_id, {200, Raw}}
        ]),
    try
        Store = #{
            <<"store-module">> => hb_store_odysee,
            <<"odysee-comment-url">> => MockServer
        },
        {ok, Msg} = hb_store:read(Store, <<"odysee/comment-id/vector-1">>, #{}),
        ?assertEqual(<<"vector-1">>, hb_maps:get(<<"comment-id">>, Msg, #{})),
        ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
        [_Request] = hb_mock_server:get_requests(comment_by_id, 1, ServerHandle)
    after
        hb_mock_server:stop(ServerHandle)
    end.

remote_store_verifies_native_lbry_blob_and_caches_commitment_id_test() ->
    Body = <<"encrypted blob">>,
    BlobHash = sha384_hex(Body),
    Key = <<"lbry/blob/", BlobHash/binary>>,
    SourceStore = #{
        <<"store-module">> => hb_store_lbry_blob,
        <<"fixtures">> => #{
            <<"odysee/blob/", BlobHash/binary>> => #{
                <<"device">> => <<"lbry-blob@1.0">>,
                <<"content-type">> => <<"application/octet-stream">>,
                <<"body">> => Body,
                <<"blob-hash">> => BlobHash,
                <<"blob-size">> => byte_size(Body)
            }
        }
    },
    SourceNode = hb_http_server:start_node(#{ <<"store">> => SourceStore }),
    ClientStore = hb_test_utils:test_store(),
    RemoteStore = [
        #{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => SourceNode,
            <<"require-codec">> => <<"json@1.0">>,
            <<"verify-remote-read">> => true,
            <<"local-store">> => [ClientStore]
        }
    ],
    {ok, Msg} = hb_cache:read(Key, #{ <<"store">> => RemoteStore }),
    ?assert(has_commitment_device(Msg, ?LBRY_BLOB_COMMITMENT_DEVICE)),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})),
    [CommitmentID] = commitment_ids_by_device(Msg, ?LBRY_BLOB_COMMITMENT_DEVICE),
    {ok, CachedByKey} = hb_cache:read(Key, #{ <<"store">> => [ClientStore] }),
    ?assert(hb_message:verify(CachedByKey, source_verify_req(CachedByKey), #{})),
    {ok, CachedByCommitmentID} =
        hb_cache:read(CommitmentID, #{ <<"store">> => [ClientStore] }),
    ?assert(hb_message:verify(
        CachedByCommitmentID,
        source_verify_req(CachedByCommitmentID),
        #{}
    )).

remote_store_rejects_substituted_native_blob_test() ->
    Body = <<"the real blob">>,
    BlobHash = sha384_hex(Body),
    RequestedHash = sha384_hex(<<"a different blob">>),
    Key = <<"lbry/blob/", RequestedHash/binary>>,
    SourceStore = #{
        <<"store-module">> => hb_store_lbry_blob,
        <<"fixtures">> => #{
            <<"odysee/blob/", RequestedHash/binary>> => #{
                <<"device">> => <<"lbry-blob@1.0">>,
                <<"content-type">> => <<"application/octet-stream">>,
                <<"body">> => Body,
                <<"blob-hash">> => BlobHash,
                <<"blob-size">> => byte_size(Body)
            }
        }
    },
    SourceNode = hb_http_server:start_node(#{ <<"store">> => SourceStore }),
    ClientStore = hb_test_utils:test_store(),
    RemoteStore = [
        #{
            <<"store-module">> => hb_store_remote_node,
            <<"node">> => SourceNode,
            <<"require-codec">> => <<"json@1.0">>,
            <<"verify-remote-read">> => true,
            <<"local-store">> => [ClientStore]
        }
    ],
    ?assertMatch({error, _}, hb_cache:read(Key, #{ <<"store">> => RemoteStore })),
    ?assertEqual({error, not_found}, hb_cache:read(Key, #{ <<"store">> => [ClientStore] })).

source_verify_req(Msg) ->
    #{
        <<"commitment-ids">> => odysee_commitment_ids(Msg)
    }.

odysee_commitment_ids(Msg) ->
    Commitments = hb_maps:get(<<"commitments">>, Msg, #{}, #{}),
    [
        ID
    ||
        {ID, Commitment} <- maps:to_list(Commitments),
        lists:member(
            hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{}),
            [
                ?DEVICE,
                ?LBRY_BLOB_COMMITMENT_DEVICE,
                ?LBRY_STREAM_DESCRIPTOR_COMMITMENT_DEVICE,
                ?LBRY_CLAIM_COMMITMENT_DEVICE,
                ?LBRY_CLAIM_OUTPUT_COMMITMENT_DEVICE,
                ?LBRY_TRANSACTION_COMMITMENT_DEVICE
            ]
        )
    ].

has_commitment_device(Msg, Device) ->
    lists:member(Device, hb_message:commitment_devices(Msg, #{})).

commitment_ids_by_device(Msg, Device) ->
    [
        ID
    ||
        {ID, Commitment} <- maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, #{})),
        hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{}) =:= Device
    ].

surface_fixture_store() ->
    #{
        <<"store-module">> => hb_store_odysee,
        <<"fixtures">> => #{
            <<"odysee/claim-id/abc123">> => claim_fixture(),
            <<"odysee/channel/channel-1">> => channel_fixture(),
            <<"odysee/comment/vector-1">> => comment_fixture(),
            <<"odysee/comment-reaction/vector-1">> => comment_reaction_fixture(),
            <<"odysee/file-view-count/claim-1">> => file_view_count_fixture(),
            <<"odysee/file-reaction/claim-1">> => file_reaction_fixture(),
            <<"odysee/subscription-count/channel-1">> => subscription_count_fixture()
        }
    }.

claim_fixture() ->
    Claim = claim_source_fixture(),
    #{
        <<"device">> => <<"odysee-claim@1.0">>,
        <<"content-type">> => <<"application/json">>,
        <<"body">> => hb_json:encode(Claim),
        <<"claim">> => Claim,
        <<"claim-id">> => <<"abc123">>,
        <<"claim-name">> => <<"example">>,
        <<"value">> => hb_maps:get(<<"value">>, Claim, #{})
    }.

claim_source_fixture() ->
    #{
        <<"claim_id">> => <<"abc123">>,
        <<"name">> => <<"example">>,
        <<"value">> => #{ <<"title">> => <<"Example">> }
    }.

stream_source_fixture() ->
    #{
        <<"claim_id">> => <<"stream-1">>,
        <<"canonical_url">> => <<"lbry://example#1">>,
        <<"name">> => <<"example">>,
        <<"value_type">> => <<"stream">>,
        <<"value">> => #{
            <<"source">> => #{
                <<"media_type">> => <<"video/mp4">>,
                <<"name">> => <<"example.mp4">>,
                <<"sd_hash">> =>
                    <<"6ee8f762a2eedbd2b5eeade82ca4d0a6287f55db4195563cc52fc004701b7d55edcfad277a5141084bdf5fca3adb403a">>,
                <<"size">> => 42
            },
            <<"stream_type">> => <<"video">>
        }
    }.

channel_fixture() ->
    Claim = channel_source_fixture(),
    hb_util:ok(
        hb_ao:raw(
            <<"odysee-channel@1.0">>,
            <<"channel">>,
            #{},
            #{ <<"claim">> => Claim },
            #{}
        )
    ).

channel_source_fixture() ->
    #{
        <<"claim_id">> => <<"channel-1">>,
        <<"canonical_url">> => <<"lbry://@example#1">>,
        <<"name">> => <<"@example">>,
        <<"value_type">> => <<"channel">>,
        <<"value">> => #{
            <<"title">> => <<"Example">>,
            <<"public_key">> => <<"3082010a0282010100">>,
            <<"public_key_id">> => <<"bLGr4w">>
        }
    }.

comment_fixture() ->
    Comment = comment_source_fixture(),
    hb_util:ok(
        hb_ao:raw(
            <<"odysee-comment@1.0">>,
            <<"normalize">>,
            #{},
            #{ <<"comment">> => Comment },
            #{}
        )
    ).

comment_reaction_fixture() ->
    Msg = hb_util:ok(
        hb_ao:raw(
            <<"odysee-reaction@1.0">>,
            <<"normalize">>,
            #{},
            #{ <<"result">> => comment_reaction_result() },
            #{}
        )
    ),
    Msg#{
        <<"comment-id">> => <<"vector-1">>,
        <<"comment-reaction-store-path">> => <<"odysee/comment-reaction/vector-1">>
    }.

comment_source_fixture() ->
    (commentron_vector())#{
        <<"comment_id">> => <<"vector-1">>,
        <<"comment">> => <<"nicee">>
    }.

comment_reaction_result() ->
    #{
        <<"my_reactions">> => #{
            <<"vector-1">> => [<<"like">>]
        },
        <<"others_reactions">> => #{
            <<"vector-1">> => #{ <<"like">> => 53 }
        }
    }.

file_view_count_fixture() ->
    Msg = hb_util:ok(
        hb_ao:raw(
            <<"odysee-file@1.0">>,
            <<"normalize">>,
            #{ <<"claim-ids">> => <<"claim-1">> },
            #{ <<"counts">> => [1504] },
            #{}
        )
    ),
    Msg#{
        <<"claim-id">> => <<"claim-1">>,
        <<"file-view-count-store-path">> => <<"odysee/file-view-count/claim-1">>
    }.

file_reaction_fixture() ->
    Msg = hb_util:ok(
        hb_ao:raw(
            <<"odysee-file-reaction@1.0">>,
            <<"normalize">>,
            #{},
            #{ <<"result">> => file_reaction_result() },
            #{}
        )
    ),
    Msg#{
        <<"claim-id">> => <<"claim-1">>,
        <<"file-reaction-store-path">> => <<"odysee/file-reaction/claim-1">>
    }.

file_reaction_result() ->
    #{
        <<"my_reactions">> => #{
            <<"claim-1">> => [<<"like">>]
        },
        <<"others_reactions">> => #{
            <<"claim-1">> => #{ <<"like">> => 59 }
        }
    }.

subscription_count_fixture() ->
    Msg = hb_util:ok(
        hb_ao:raw(
            <<"odysee-subscription@1.0">>,
            <<"normalize">>,
            #{ <<"claim-ids">> => <<"channel-1">> },
            #{ <<"counts">> => [169000] },
            #{}
        )
    ),
    Msg#{
        <<"claim-id">> => <<"channel-1">>,
        <<"subscription-count-store-path">> => <<"odysee/subscription-count/channel-1">>
    }.

search_response(Claims) ->
    hb_json:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => #{
            <<"items">> => Claims,
            <<"page">> => 1,
            <<"page_size">> => length(Claims),
            <<"total_items">> => length(Claims),
            <<"total_pages">> => 1
        },
        <<"id">> => 1
    }).

blob_fixture() ->
    Body = <<"encrypted blob">>,
    BlobHash = sha384_hex(Body),
    {
        #{
            <<"device">> => <<"odysee-blob@1.0">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => Body,
            <<"blob-hash">> => BlobHash,
            <<"blob-size">> => byte_size(Body)
        },
        Body,
        BlobHash
    }.

transaction_fixture() ->
    Raw =
        <<
            1:32/little,
            1,
            0:256,
            16#ffffffff:32/little,
            0,
            16#ffffffff:32/little,
            1,
            0:64/little,
            0,
            0:32/little
        >>,
    {Raw, hb_lbry_tx:txid(Raw)}.

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
    {hb_util:to_hex(RawTx), TxID, ClaimID}.

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

proof_push(Bin) when byte_size(Bin) < 16#4c ->
    <<(byte_size(Bin)), Bin/binary>>.

reverse_binary(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

commentron_vector() ->
    #{
        <<"channel-id">> => <<"7fadfe1d0dce928350137a13497b6fc36627cf45">>,
        <<"channel_id">> => <<"7fadfe1d0dce928350137a13497b6fc36627cf45">>,
        <<"public-key">> =>
            <<"3056301006072a8648ce3d020106052b8104000a03420004e0743cfa62857d1d7bda9ca6ba0ec3325902866e6442f51a9da2b143bc0ba40cda532e483e1a8a48c84b4b9dc16a117b2f9763d518db50d8fed2b818937ef8b1">>,
        <<"signature">> =>
            <<"fe35046bd949fc89037d64ac3558fea859022a166558b459b6883acafa15ca9ec567ca23e7b4ae19e4dbc3f92aac30a132315db7abcb03c15c61662fb9f49458">>,
        <<"signing-ts">> => <<"1582846386">>,
        <<"signing_ts">> => <<"1582846386">>,
        <<"data">> => <<"nicee">>
    }.

descriptor_fixture() ->
    StreamNameHex = hb_util:to_hex(<<"verified.mp4">>),
    KeyHex = <<"000102030405060708090a0b0c0d0e0f">>,
    SuggestedHex = StreamNameHex,
    BlobHash = hb_util:to_hex(crypto:hash(sha384, <<"encrypted blob">>)),
    Blob = #{
        <<"blob_num">> => 0,
        <<"blob_hash">> => BlobHash,
        <<"iv">> => <<"00112233445566778899aabbccddeeff">>,
        <<"length">> => 16
    },
    Terminator = #{
        <<"blob_num">> => 1,
        <<"iv">> => <<"ffeeddccbbaa99887766554433221100">>,
        <<"length">> => 0
    },
    StreamHash = descriptor_stream_hash(StreamNameHex, KeyHex, SuggestedHex, [Blob, Terminator]),
    JSON =
        hb_json:encode(#{
            <<"stream_type">> => <<"lbryfile">>,
            <<"stream_name">> => StreamNameHex,
            <<"key">> => KeyHex,
            <<"suggested_file_name">> => SuggestedHex,
            <<"stream_hash">> => StreamHash,
            <<"blobs">> => [Blob, Terminator]
        }),
    {JSON, hb_util:to_hex(crypto:hash(sha384, JSON))}.

descriptor_stream_hash(StreamNameHex, KeyHex, SuggestedHex, Blobs) ->
    BlobSums =
        iolist_to_binary([
            descriptor_blob_hashsum(
                hb_ao:normalize_keys(Blob, #{})
            )
        ||
            Blob <- Blobs
        ]),
    BlobDigest = crypto:hash(sha384, BlobSums),
    hb_util:to_hex(
        crypto:hash(sha384, <<StreamNameHex/binary, KeyHex/binary, SuggestedHex/binary, BlobDigest/binary>>)
    ).

descriptor_blob_hashsum(Blob) ->
    Length = hb_maps:get(<<"length">>, Blob, #{}),
    BlobNum = first_value([<<"blob-num">>, <<"blob_num">>], Blob, #{}),
    IV = hb_maps:get(<<"iv">>, Blob, #{}),
    HashPrefix =
        case first_value([<<"blob-hash">>, <<"blob_hash">>], Blob, #{}) of
            not_found -> <<>>;
            Hash -> Hash
        end,
    crypto:hash(
        sha384,
        <<HashPrefix/binary, (integer_to_binary(BlobNum))/binary, IV/binary, (integer_to_binary(Length))/binary>>
    ).

-endif.
