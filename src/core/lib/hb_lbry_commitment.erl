%%% @doc Construction and verification helpers for native LBRY commitments.
%%% A native commitment binds a HyperBEAM message to an LBRY source object
%%% (blob, stream descriptor, transaction, claim output) through the object's
%%% own content addressing, rather than through a node signature. Messages
%%% carrying these commitments verify through `hb_message:verify' via the
%%% `commitment-device' dispatch in `dev_message'.
-module(hb_lbry_commitment).
-export([commitment_id/1, commitment/5, with_commitment/6]).
-export([native_id/2, native_id_bytes/1, outpoint_bytes/2]).
-export([blob_message/2, transaction_message/1, descriptor_message/2]).
-export([claim_output_message/2, channel_output_message/2]).
-export([stream_claim_message/2, with_attestation_commitment/2]).
-export([claim_output_verification/3, channel_output_verification/3]).
-export([stream_output_verification/3, attestation_verification/3]).
-export([verify_remote_read/3, expected_remote_commitment/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Derive the commitment map key for a native LBRY identifier.
%% Commitment map keys must be 43-character human IDs: `dev_message:id'
%% accumulates selected commitment keys through `hb_util:native_id', which
%% only accepts 32-byte IDs. Non-32-byte native identifiers are therefore
%% rehashed with SHA-256 before encoding. This matches the HTTP signature
%% layer's `derived_commitment_id', so the key survives wire round-trips
%% without an explicit `id' parameter.
commitment_id(NativeIDBytes) when byte_size(NativeIDBytes) == 32 ->
    hb_util:human_id(NativeIDBytes);
commitment_id(NativeIDBytes) when is_binary(NativeIDBytes) ->
    hb_util:human_id(crypto:hash(sha256, NativeIDBytes)).

%% @doc Build a native commitment message and its map key. The `signature'
%% field carries the base64url-encoded native identifier bytes: the HTTP
%% signature transport requires the field, and deriving it from the native
%% identifier keeps the commitment deterministic from the source object.
%% `Extra' fields are merged into the commitment message; their values must
%% be binaries to survive the HTTP signature transport.
commitment(Device, Type, {NativeIDType, NativeIDBytes}, Committed, Extra) ->
    {
        commitment_id(NativeIDBytes),
        maps:merge(
            Extra,
            #{
                <<"commitment-device">> => Device,
                <<"type">> => Type,
                <<"signature">> => hb_util:encode(NativeIDBytes),
                <<"committed">> => Committed,
                <<"native-id">> => hb_util:to_hex(NativeIDBytes),
                <<"native-id-type">> => NativeIDType
            }
        )
    }.

%% @doc Attach a native commitment to a message, merging with any
%% commitments already present: a single evidence message may carry several
%% native commitments (for example a stream claim output carries the claim
%% binding, the descriptor `sd_hash' binding, and the channel attestation).
with_commitment(Msg, Device, Type, NativeIDSpec, Committed, Extra) ->
    {ID, Commitment} = commitment(Device, Type, NativeIDSpec, Committed, Extra),
    Commitments = maps:get(<<"commitments">>, Msg, #{}),
    Msg#{ <<"commitments">> => Commitments#{ ID => Commitment } }.

%% @doc Extract the native identifier from a commitment message, requiring
%% the `signature' field to encode the same bytes. Returns the normalized
%% hex form and the raw bytes.
native_id(Commitment, Opts) ->
    maybe
        {ok, Hex, Bytes} ?=
            native_id_bytes(hb_maps:get(<<"native-id">>, Commitment, undefined, Opts)),
        true ?=
            signature_matches(
                hb_maps:get(<<"signature">>, Commitment, undefined, Opts),
                Bytes
            ) orelse {error, signature_native_id_mismatch},
        {ok, Hex, Bytes}
    end.

%% @doc Decode a hex native identifier into normalized hex and raw bytes.
native_id_bytes(Hex) when is_binary(Hex) ->
    Normalized = hb_util:to_lower(Hex),
    try binary:decode_hex(Normalized) of
        Bytes -> {ok, Normalized, Bytes}
    catch
        _:_ -> {error, invalid_native_id}
    end;
native_id_bytes(_) ->
    {error, missing_native_id}.

signature_matches(Signature, NativeIDBytes) when is_binary(Signature) ->
    try hb_util:decode(Signature) of
        NativeIDBytes -> true;
        _ -> false
    catch
        _:_ -> false
    end;
signature_matches(_, _) ->
    false.

%% @doc Build the canonical blob message for verified encrypted blob bytes.
%% The caller must have verified that `SHA-384(Bytes)' matches `HexHash'.
blob_message(HexHash, Bytes) ->
    Normalized = hb_util:to_lower(HexHash),
    with_commitment(
        #{
            <<"device">> => <<"lbry-blob@1.0">>,
            <<"data">> => Bytes,
            <<"blob-hash">> => Normalized
        },
        <<"lbry-blob@1.0">>,
        <<"sha-384">>,
        {<<"blob-hash">>, binary:decode_hex(Normalized)},
        [<<"blob-hash">>, <<"data">>, <<"device">>],
        #{}
    ).

%% @doc Build the canonical transaction message for raw LBRY transaction
%% bytes. The native identifier is the display-order txid, which is recomputed
%% from the raw bytes, so the commitment cannot disagree with the content.
transaction_message(Raw) when is_binary(Raw) ->
    case hb_lbry_tx:parse(Raw) of
        {ok, Tx} ->
            TxIDHex = maps:get(<<"txid">>, Tx),
            {ok,
                with_commitment(
                    Tx#{ <<"device">> => <<"lbry-transaction@1.0">> },
                    <<"lbry-transaction@1.0">>,
                    <<"sha-256d">>,
                    {<<"txid">>, binary:decode_hex(TxIDHex)},
                    [<<"device">>, <<"raw">>, <<"txid">>],
                    #{}
                )};
        Error ->
            Error
    end.

%% @doc Build the canonical stream-descriptor message for raw descriptor
%% bytes and their expected `sd_hash'. The parse enforces the hash match and
%% the full descriptor structure rules.
descriptor_message(Raw, SDHash) ->
    case hb_lbry_stream_descriptor:parse(Raw, SDHash) of
        {ok, Descriptor} ->
            Normalized = hb_util:to_lower(SDHash),
            {ok,
                with_commitment(
                    Descriptor,
                    <<"lbry-stream-descriptor@1.0">>,
                    <<"sha-384">>,
                    {<<"sd-hash">>, binary:decode_hex(Normalized)},
                    [<<"device">>, <<"raw">>, <<"sd-hash">>],
                    #{}
                )};
        Error ->
            Error
    end.

%% @doc Build the canonical claim-output message for an output of a raw LBRY
%% transaction. The native identifier is the immutable display-order
%% outpoint. The commitment `type' distinguishes hash-derived claim-id
%% bindings (`create' outputs) from assertion-level bindings (`update'
%% outputs whose create ancestry is not walked).
claim_output_message(Raw, Nout) when is_binary(Raw), is_integer(Nout) ->
    maybe
        {ok, Tx} ?= hb_lbry_tx:parse(Raw),
        {ok, Output} ?= claim_output(Tx, Nout),
        TxIDHex = maps:get(<<"txid">>, Tx),
        ClaimOp = maps:get(<<"claim-op">>, Output),
        ClaimID = maps:get(<<"claim-id">>, Output),
        Msg = #{
            <<"device">> => <<"lbry-claim@1.0">>,
            <<"claim-id">> => ClaimID,
            <<"claim-op">> => ClaimOp,
            <<"claim-name">> => maps:get(<<"claim-name">>, Output),
            <<"claim">> => maps:get(<<"claim">>, Output),
            <<"claim-envelope">> => maps:get(<<"claim-envelope">>, Output),
            <<"txid">> => TxIDHex,
            <<"nout">> => Nout,
            <<"raw-transaction">> => Raw
        },
        {ok,
            with_commitment(
                Msg,
                <<"lbry-claim@1.0">>,
                claim_type(ClaimOp),
                {<<"outpoint">>, outpoint_bytes(TxIDHex, Nout)},
                [
                    <<"claim">>, <<"claim-id">>, <<"claim-name">>,
                    <<"claim-op">>, <<"device">>, <<"nout">>,
                    <<"raw-transaction">>, <<"txid">>
                ],
                #{ <<"claim-id">> => ClaimID, <<"claim-op">> => ClaimOp }
            )}
    end.

%% @doc Build the canonical channel-output message for a channel claim
%% output. Extends the claim-output evidence with the channel public key,
%% extracted from the raw channel claim protobuf and normalized to the
%% compressed form. Outputs without channel key material fail closed.
channel_output_message(Raw, Nout) ->
    maybe
        {ok, ClaimMsg} ?= claim_output_message(Raw, Nout),
        Envelope = maps:get(<<"claim-envelope">>, ClaimMsg),
        {ok, RawPublicKey} ?=
            hb_lbry_claim_proto:channel_public_key(
                maps:get(<<"message">>, Envelope)
            ),
        {ok, PublicKey} ?= hb_lbry_attestation:normalize_public_key(RawPublicKey),
        PublicKeyHex = hb_util:to_hex(PublicKey),
        ClaimID = maps:get(<<"claim-id">>, ClaimMsg),
        ClaimOp = maps:get(<<"claim-op">>, ClaimMsg),
        TxIDHex = maps:get(<<"txid">>, ClaimMsg),
        Msg = (maps:remove(<<"commitments">>, ClaimMsg))#{
            <<"device">> => <<"lbry-channel@1.0">>,
            <<"channel-id">> => ClaimID,
            <<"public-key">> => PublicKeyHex
        },
        {ok,
            with_commitment(
                Msg,
                <<"lbry-channel@1.0">>,
                claim_type(ClaimOp),
                {<<"outpoint">>, outpoint_bytes(TxIDHex, Nout)},
                [
                    <<"channel-id">>, <<"claim">>, <<"claim-id">>,
                    <<"claim-name">>, <<"claim-op">>, <<"device">>,
                    <<"nout">>, <<"public-key">>, <<"raw-transaction">>,
                    <<"txid">>
                ],
                #{
                    <<"claim-id">> => ClaimID,
                    <<"claim-op">> => ClaimOp,
                    <<"public-key">> => PublicKeyHex
                }
            )}
    end.

%% @doc Build the canonical stream claim-output message: claim-output
%% evidence extended with the descriptor `sd_hash' extracted from the stream
%% claim protobuf. Carries two native commitments -- the claim binding and
%% the stream `sd_hash' binding -- each verified by its own codec.
stream_claim_message(Raw, Nout) ->
    maybe
        {ok, ClaimMsg} ?= claim_output_message(Raw, Nout),
        Envelope = maps:get(<<"claim-envelope">>, ClaimMsg),
        {ok, SDHash} ?=
            hb_lbry_claim_proto:stream_sd_hash(maps:get(<<"message">>, Envelope)),
        ClaimID = maps:get(<<"claim-id">>, ClaimMsg),
        ClaimOp = maps:get(<<"claim-op">>, ClaimMsg),
        TxIDHex = maps:get(<<"txid">>, ClaimMsg),
        Msg = ClaimMsg#{
            <<"device">> => <<"lbry-stream@1.0">>,
            <<"sd-hash">> => SDHash
        },
        {ok,
            with_commitment(
                Msg,
                <<"lbry-stream@1.0">>,
                claim_type(ClaimOp),
                {<<"sd-hash">>, binary:decode_hex(SDHash)},
                [
                    <<"claim">>, <<"claim-id">>, <<"claim-name">>,
                    <<"claim-op">>, <<"device">>, <<"nout">>,
                    <<"raw-transaction">>, <<"sd-hash">>, <<"txid">>
                ],
                #{
                    <<"claim-id">> => ClaimID,
                    <<"claim-op">> => ClaimOp,
                    <<"outpoint">> => hb_util:to_hex(outpoint_bytes(TxIDHex, Nout))
                }
            )}
    end.

%% @doc Attach a channel-attestation commitment to a signed stream claim
%% message. The channel evidence message provides the normalized public key
%% and its claim-id binding; the envelope's embedded signing-channel hash
%% must match the channel evidence, and the claim signature must verify
%% against the channel key before the commitment is attached. The commitment
%% records the channel outpoint so an independent verifier can re-fetch and
%% re-verify the channel evidence by immutable identifier.
with_attestation_commitment(StreamMsg, ChannelMsg) ->
    maybe
        Envelope = maps:get(<<"claim-envelope">>, StreamMsg),
        true ?=
            maps:get(<<"signed">>, Envelope, false)
                orelse {error, unsigned_claim},
        SigningChannelID = maps:get(<<"signing-channel-id">>, Envelope),
        ChannelID = maps:get(<<"claim-id">>, ChannelMsg),
        true ?=
            ChannelID == SigningChannelID
                orelse {error, {channel_binding_mismatch, ChannelID, SigningChannelID}},
        PublicKeyHex = maps:get(<<"public-key">>, ChannelMsg),
        Raw = maps:get(<<"raw-transaction">>, StreamMsg),
        {ok, Tx} ?= hb_lbry_tx:parse(Raw),
        [FirstInput | _] = maps:get(<<"inputs">>, Tx),
        Digest = hb_lbry_attestation:signature_digest(FirstInput, Envelope),
        Signature = maps:get(<<"claim-signature">>, Envelope),
        {ok, true} ?=
            valid_or_error(
                hb_lbry_attestation:verify_signature(
                    Signature,
                    Digest,
                    binary:decode_hex(PublicKeyHex)
                ),
                invalid_claim_signature
            ),
        TxIDHex = maps:get(<<"txid">>, StreamMsg),
        Nout = maps:get(<<"nout">>, StreamMsg),
        ID = commitment_id(Signature),
        Commitment = #{
            <<"commitment-device">> => <<"lbry-channel-attestation@1.0">>,
            <<"type">> => <<"secp256k1-sha256">>,
            <<"signature">> => hb_util:encode(Signature),
            <<"committed">> => [
                <<"claim">>, <<"claim-id">>, <<"claim-op">>, <<"device">>,
                <<"nout">>, <<"raw-transaction">>, <<"txid">>
            ],
            <<"native-id">> => hb_util:to_hex(outpoint_bytes(TxIDHex, Nout)),
            <<"native-id-type">> => <<"outpoint">>,
            <<"claim-id">> => maps:get(<<"claim-id">>, StreamMsg),
            <<"claim-op">> => maps:get(<<"claim-op">>, StreamMsg),
            <<"channel-id">> => ChannelID,
            <<"channel-public-key">> => PublicKeyHex,
            <<"channel-txid">> => maps:get(<<"txid">>, ChannelMsg),
            <<"channel-nout">> =>
                integer_to_binary(maps:get(<<"nout">>, ChannelMsg))
        },
        Commitments = maps:get(<<"commitments">>, StreamMsg, #{}),
        {ok, StreamMsg#{ <<"commitments">> => Commitments#{ ID => Commitment } }}
    else
        {error, _} = Error -> Error;
        _ -> {error, invalid_attestation_input}
    end.

%% @doc Verify a claim-output commitment against its message. Re-parses the
%% committed raw transaction, requires the recomputed txid to match the
%% commitment's outpoint, selects the committed output, and requires every
%% committed claim field to match the freshly parsed output. For `create'
%% outputs the claim id is hash-derived; for `update' outputs it is only
%% asserted in-script, which the commitment `type' makes explicit. Returns
%% the freshly parsed claim envelope on success for further checks.
claim_output_verification(Base, Req, Opts) ->
    maybe
        {ok, _Hex, OutpointBytes} ?= native_id(Req, Opts),
        verify_claim_output(Base, Req, OutpointBytes, Opts)
    else
        {error, _} = Error -> Error;
        _ -> {error, claim_output_mismatch}
    end.

%% @doc Verify a stream commitment: the claim-output binding must hold for
%% the outpoint recorded in the commitment, and the descriptor `sd_hash'
%% must re-derive from the freshly parsed stream claim protobuf to the
%% commitment's native identifier.
stream_output_verification(Base, Req, Opts) ->
    maybe
        <<"lbry-stream@1.0">> ?= device_field(Base, Opts),
        {ok, SDHex, SDBytes} ?= native_id(Req, Opts),
        48 ?= byte_size(SDBytes),
        {ok, _OutHex, OutpointBytes} ?=
            native_id_bytes(hb_maps:get(<<"outpoint">>, Req, undefined, Opts)),
        {ok, Envelope} ?= verify_claim_output(Base, Req, OutpointBytes, Opts),
        {ok, DerivedSDHash} ?=
            hb_lbry_claim_proto:stream_sd_hash(maps:get(<<"message">>, Envelope)),
        SDHex ?= DerivedSDHash,
        SDHex ?= lower_field(Base, <<"sd-hash">>, Opts),
        {ok, Envelope}
    else
        {error, _} = Error -> Error;
        _ -> {error, stream_output_mismatch}
    end.

%% @doc Verify a channel-attestation commitment: the committed raw
%% transaction must contain a signed claim envelope at the commitment's
%% outpoint, whose embedded signature matches the commitment's, verifies
%% against the recorded channel public key over the v2 signature digest, and
%% whose embedded signing-channel hash matches the recorded channel claim
%% id. The key-to-channel binding itself is the channel evidence message's
%% commitment to prove; the recorded channel outpoint locates that evidence.
attestation_verification(Base, Req, Opts) ->
    maybe
        <<"secp256k1-sha256">> ?= hb_maps:get(<<"type">>, Req, undefined, Opts),
        <<"lbry-stream@1.0">> ?= device_field(Base, Opts),
        {ok, _OutHex, OutpointBytes} ?=
            native_id_bytes(hb_maps:get(<<"native-id">>, Req, undefined, Opts)),
        {ok, Tx, Output} ?= output_evidence(Base, OutpointBytes, Opts),
        Envelope = maps:get(<<"claim-envelope">>, Output),
        true ?=
            maps:get(<<"signed">>, Envelope, false)
                orelse {error, unsigned_claim},
        {ok, Signature} ?=
            decode_signature(hb_maps:get(<<"signature">>, Req, undefined, Opts)),
        Signature ?= maps:get(<<"claim-signature">>, Envelope),
        PublicKeyHex = lower_field(Req, <<"channel-public-key">>, Opts),
        {ok, _, PublicKey} ?= native_id_bytes(PublicKeyHex),
        {ok, PublicKey} ?= hb_lbry_attestation:normalize_public_key(PublicKey),
        [FirstInput | _] = maps:get(<<"inputs">>, Tx),
        Digest = hb_lbry_attestation:signature_digest(FirstInput, Envelope),
        {ok, true} ?=
            valid_or_error(
                hb_lbry_attestation:verify_signature(Signature, Digest, PublicKey),
                invalid_claim_signature
            ),
        ChannelID = lower_field(Req, <<"channel-id">>, Opts),
        ChannelID ?=
            hb_util:to_hex(
                reverse(maps:get(<<"signing-channel-hash">>, Envelope))
            ),
        {ok, ChannelID}
    else
        {error, _} = Error -> Error;
        _ -> {error, attestation_mismatch}
    end.

%% The claim commitment legitimately lives on claim, channel, and stream
%% evidence messages, so the committed `device' is pinned to that family;
%% the channel and stream verifications additionally pin their exact device.
verify_claim_output(Base, Req, OutpointBytes, Opts) ->
    maybe
        true ?=
            lists:member(
                device_field(Base, Opts),
                [
                    <<"lbry-claim@1.0">>,
                    <<"lbry-channel@1.0">>,
                    <<"lbry-stream@1.0">>
                ]
            ) orelse {error, claim_device_mismatch},
        {ok, _Tx, Output} ?= output_evidence(Base, OutpointBytes, Opts),
        ClaimOp = maps:get(<<"claim-op">>, Output),
        ClaimOp ?= hb_maps:get(<<"claim-op">>, Base, undefined, Opts),
        ClaimOp ?= hb_maps:get(<<"claim-op">>, Req, undefined, Opts),
        true ?=
            claim_type(ClaimOp) == hb_maps:get(<<"type">>, Req, undefined, Opts)
                orelse {error, claim_type_mismatch},
        ClaimID = maps:get(<<"claim-id">>, Output),
        ClaimID ?= lower_field(Base, <<"claim-id">>, Opts),
        ClaimID ?= lower_field(Req, <<"claim-id">>, Opts),
        ClaimBytes = maps:get(<<"claim">>, Output),
        ClaimBytes ?= hb_maps:get(<<"claim">>, Base, undefined, Opts),
        ClaimName = maps:get(<<"claim-name">>, Output),
        ClaimName ?= hb_maps:get(<<"claim-name">>, Base, undefined, Opts),
        {ok, maps:get(<<"claim-envelope">>, Output)}
    else
        {error, _} = Error -> Error;
        _ -> {error, claim_output_mismatch}
    end.

output_evidence(Base, OutpointBytes, Opts) ->
    maybe
        {ok, TxIDHex, Nout} ?= split_outpoint(OutpointBytes),
        Raw = hb_maps:get(<<"raw-transaction">>, Base, undefined, Opts),
        true ?= is_binary(Raw) orelse {error, missing_raw_transaction},
        {ok, Tx} ?= hb_lbry_tx:parse(Raw),
        TxIDHex ?= maps:get(<<"txid">>, Tx),
        TxIDHex ?= lower_field(Base, <<"txid">>, Opts),
        Nout ?= integer_field(Base, <<"nout">>, Opts),
        {ok, Output} ?= claim_output(Tx, Nout),
        {ok, Tx, Output}
    else
        {error, _} = Error -> Error;
        _ -> {error, output_evidence_mismatch}
    end.

decode_signature(Signature) when is_binary(Signature) ->
    try hb_util:decode(Signature) of
        Bytes when byte_size(Bytes) == 64 -> {ok, Bytes};
        _ -> {error, invalid_attestation_signature}
    catch
        _:_ -> {error, invalid_attestation_signature}
    end;
decode_signature(_) ->
    {error, invalid_attestation_signature}.

valid_or_error({ok, true}, _Error) -> {ok, true};
valid_or_error(_, Error) -> {error, Error}.

reverse(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).

%% @doc Verify a channel-output commitment: the claim-output binding must
%% hold, and the channel public key must re-derive from the raw channel
%% claim protobuf to the committed normalized value.
channel_output_verification(Base, Req, Opts) ->
    maybe
        <<"lbry-channel@1.0">> ?= device_field(Base, Opts),
        {ok, Envelope} ?= claim_output_verification(Base, Req, Opts),
        {ok, RawPublicKey} ?=
            hb_lbry_claim_proto:channel_public_key(
                maps:get(<<"message">>, Envelope)
            ),
        {ok, PublicKey} ?= hb_lbry_attestation:normalize_public_key(RawPublicKey),
        PublicKeyHex = hb_util:to_hex(PublicKey),
        PublicKeyHex ?= lower_field(Base, <<"public-key">>, Opts),
        PublicKeyHex ?= lower_field(Req, <<"public-key">>, Opts),
        ChannelID = lower_field(Base, <<"claim-id">>, Opts),
        ChannelID ?= lower_field(Base, <<"channel-id">>, Opts),
        {ok, PublicKeyHex}
    else
        {error, _} = Error -> Error;
        _ -> {error, channel_output_mismatch}
    end.

%% @doc Verify a message returned by an untrusted remote store for an LBRY
%% native identifier, before it is cached or returned. The message must
%% carry a commitment of the expected device family whose native identifier
%% matches the requested key, and every LBRY-family commitment on the
%% message must verify. On success the message is returned with only its
%% LBRY-family commitments: the remote node's own transport signatures are
%% neither required nor trusted, and they commit response-context keys that
%% do not survive caching. Keys that are not LBRY native identifiers (such
%% as regular HyperBEAM message IDs) pass through unchanged: the no-trust
%% proof applies to immutable source objects only.
%%
%% Outpoint keys do not encode which evidence kind was requested, so by
%% default any claim-family commitment satisfies them. A store that expects
%% a specific kind can narrow the acceptable devices with the
%% `verify-remote-devices' option.
verify_remote_read(Key, Msg, Opts) ->
    case expected_remote_commitment(Key) of
        untyped ->
            {ok, Msg};
        {ok, Devices, NativeIDHex} ->
            require_native_commitments(
                narrow_devices(Devices, Opts),
                NativeIDHex,
                Key,
                Msg,
                Opts
            )
    end.

narrow_devices(Devices, Opts) ->
    case hb_maps:get(<<"verify-remote-devices">>, Opts, undefined, Opts) of
        Allowed when is_list(Allowed) ->
            [Device || Device <- Devices, lists:member(Device, Allowed)];
        _ ->
            Devices
    end.

%% @doc Classify a store key as an LBRY native identifier. Returns the
%% acceptable commitment devices and the expected commitment `native-id' for
%% the key, or `untyped' when the key is not LBRY-shaped.
expected_remote_commitment(Key) when is_binary(Key) ->
    case binary:split(Key, <<":">>) of
        [TxID, NoutBin] ->
            case {hex_bytes(TxID, 32), nout_value(NoutBin)} of
                {{ok, TxIDHex, _}, {ok, Nout}} ->
                    {ok,
                        [
                            <<"lbry-claim@1.0">>,
                            <<"lbry-channel@1.0">>,
                            <<"lbry-stream@1.0">>
                        ],
                        hb_util:to_hex(outpoint_bytes(TxIDHex, Nout))};
                _ ->
                    untyped
            end;
        [Single] ->
            case {hex_bytes(Single, 48), hex_bytes(Single, 32)} of
                {{ok, Hex, _}, _} -> {ok, [<<"lbry-blob@1.0">>], Hex};
                {_, {ok, Hex, _}} -> {ok, [<<"lbry-transaction@1.0">>], Hex};
                _ -> untyped
            end
    end;
expected_remote_commitment(_) ->
    untyped.

require_native_commitments(Devices, NativeIDHex, Key, Msg, Opts) when is_map(Msg) ->
    Commitments =
        hb_cache:ensure_all_loaded(
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
            Opts
        ),
    LbryCommitments =
        maps:filter(
            fun(_ID, Commitment) -> lbry_commitment(Commitment) end,
            Commitments
        ),
    Bound =
        [
            Commitment
         ||
            Commitment <- maps:values(LbryCommitments),
            lists:member(
                maps:get(<<"commitment-device">>, Commitment, undefined),
                Devices
            ),
            native_id_matches(Commitment, NativeIDHex)
        ],
    maybe
        true ?= Bound =/= [] orelse {error, {missing_native_commitment, Key}},
        true ?=
            hb_message:verify(
                Msg,
                #{ <<"commitment-ids">> => maps:keys(LbryCommitments) },
                Opts
            ) orelse {error, commitment_verification_failed},
        {ok, Msg#{ <<"commitments">> => LbryCommitments }}
    else
        {error, _} = Error -> Error;
        _ -> {error, remote_verification_failed}
    end;
require_native_commitments(_Devices, _NativeIDHex, Key, _Msg, _Opts) ->
    {error, {missing_native_commitment, Key}}.

lbry_commitment(#{ <<"commitment-device">> := <<"lbry-", _/binary>> }) -> true;
lbry_commitment(_) -> false.

native_id_matches(Commitment, NativeIDHex) ->
    case maps:get(<<"native-id">>, Commitment, undefined) of
        Hex when is_binary(Hex) -> hb_util:to_lower(Hex) == NativeIDHex;
        _ -> false
    end.

hex_bytes(Hex, Bytes) when is_binary(Hex), byte_size(Hex) == Bytes * 2 ->
    try binary:decode_hex(hb_util:to_lower(Hex)) of
        Decoded -> {ok, hb_util:to_lower(Hex), Decoded}
    catch
        _:_ -> {error, invalid_hex}
    end;
hex_bytes(_, _) ->
    {error, invalid_hex}.

nout_value(NoutBin) ->
    try binary_to_integer(NoutBin) of
        Nout when Nout >= 0 -> {ok, Nout};
        _ -> {error, invalid_nout}
    catch
        _:_ -> {error, invalid_nout}
    end.

%% @doc Encode a display-order outpoint as native identifier bytes.
outpoint_bytes(TxIDHex, Nout) ->
    <<(binary:decode_hex(hb_util:to_lower(TxIDHex)))/binary, Nout:32/big>>.

split_outpoint(<<TxIDBytes:32/binary, Nout:32/big>>) ->
    {ok, hb_util:to_hex(TxIDBytes), Nout};
split_outpoint(_) ->
    {error, invalid_outpoint}.

claim_type(<<"create">>) -> <<"hash160-outpoint">>;
claim_type(<<"update">>) -> <<"asserted-claim-id">>.

claim_output(Tx, Nout) ->
    Outputs =
        [
            Output
         ||
            Output <- maps:get(<<"outputs">>, Tx, []),
            maps:get(<<"nout">>, Output, undefined) == Nout,
            maps:is_key(<<"claim">>, Output)
        ],
    case Outputs of
        [Output | _] -> {ok, Output};
        [] -> {error, missing_claim_output}
    end.

device_field(Base, Opts) ->
    case hb_maps:get(<<"device">>, Base, undefined, Opts) of
        Device when is_binary(Device) -> Device;
        _ -> undefined
    end.

lower_field(Msg, Key, Opts) ->
    case hb_maps:get(Key, Msg, undefined, Opts) of
        Value when is_binary(Value) -> hb_util:to_lower(Value);
        _ -> undefined
    end.

integer_field(Msg, Key, Opts) ->
    case hb_maps:get(Key, Msg, undefined, Opts) of
        Value when is_integer(Value) ->
            Value;
        Value when is_binary(Value) ->
            try binary_to_integer(Value) of
                Int -> Int
            catch
                _:_ -> undefined
            end;
        _ ->
            undefined
    end.

%%% Tests

commitment_id_is_human_id_safe_test() ->
    BlobID = commitment_id(crypto:hash(sha384, <<"blob">>)),
    TxID = commitment_id(crypto:hash(sha256, <<"tx">>)),
    ClaimID = commitment_id(binary:part(crypto:hash(sha256, <<"claim">>), 0, 20)),
    lists:foreach(
        fun(ID) ->
            ?assertEqual(43, byte_size(ID)),
            ?assertEqual(32, byte_size(hb_util:native_id(ID)))
        end,
        [BlobID, TxID, ClaimID]
    ),
    ?assertEqual(
        hb_util:human_id(crypto:hash(sha256, <<"tx">>)),
        TxID
    ).

commitment_signature_round_trips_to_id_test() ->
    NativeBytes = crypto:hash(sha384, <<"native">>),
    {ID, Commitment} =
        commitment(
            <<"lbry-blob@1.0">>,
            <<"sha-384">>,
            {<<"blob-hash">>, NativeBytes},
            [<<"data">>],
            #{}
        ),
    Signature = hb_util:decode(maps:get(<<"signature">>, Commitment)),
    ?assertEqual(NativeBytes, Signature),
    ?assertEqual(ID, hb_util:human_id(crypto:hash(sha256, Signature))).

native_id_requires_signature_match_test() ->
    NativeBytes = crypto:hash(sha384, <<"native">>),
    {_, Commitment} =
        commitment(
            <<"lbry-blob@1.0">>,
            <<"sha-384">>,
            {<<"blob-hash">>, NativeBytes},
            [<<"data">>],
            #{}
        ),
    ?assertMatch({ok, _, NativeBytes}, native_id(Commitment, #{})),
    Tampered = Commitment#{
        <<"signature">> => hb_util:encode(crypto:hash(sha384, <<"other">>))
    },
    ?assertEqual(
        {error, signature_native_id_mismatch},
        native_id(Tampered, #{})
    ),
    ?assertEqual(
        {error, missing_native_id},
        native_id(maps:remove(<<"native-id">>, Commitment), #{})
    ).

blob_message_verifies_test() ->
    Bytes = <<"encrypted blob bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Msg = blob_message(Hash, Bytes),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

blob_message_rejects_tampered_data_test() ->
    Bytes = <<"encrypted blob bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Msg = blob_message(Hash, Bytes),
    Tampered = Msg#{ <<"data">> => <<"tampered blob bytes!">> },
    ?assertEqual(
        false,
        hb_message:verify(Tampered, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

transaction_message_verifies_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = transaction_message(Raw),
    ?assertEqual(
        <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>,
        maps:get(<<"txid">>, Msg)
    ),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

transaction_message_rejects_tampered_raw_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = transaction_message(Raw),
    <<First, Rest/binary>> = Raw,
    Tampered = Msg#{ <<"raw">> => <<(First bxor 1), Rest/binary>> },
    ?assertEqual(
        false,
        hb_message:verify(Tampered, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

transaction_message_uses_display_order_txid_key_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = transaction_message(Raw),
    [CommitmentID] = maps:keys(maps:get(<<"commitments">>, Msg)),
    DisplayTxIDBytes =
        binary:decode_hex(
            <<"51d3cd6a27420addb648347410233931b862ab52660c1dba58806b5b0f38a460">>
        ),
    ?assertEqual(hb_util:human_id(DisplayTxIDBytes), CommitmentID).

blob_message_id_calculation_is_safe_test() ->
    Bytes = <<"encrypted blob bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Msg = blob_message(Hash, Bytes),
    ?assertEqual(43, byte_size(hb_message:id(Msg))),
    ?assertEqual(43, byte_size(hb_message:id(Msg, all))).

claim_output_message_from_create_verifies_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = claim_output_message(Raw, 0),
    ?assertEqual(
        <<"9cc7f0e3de8db3b2ffd6dc0b4f1a0f0ca48a6b49">>,
        maps:get(<<"claim-id">>, Msg)
    ),
    ?assertEqual(<<"create">>, maps:get(<<"claim-op">>, Msg)),
    [Commitment] = maps:values(maps:get(<<"commitments">>, Msg)),
    ?assertEqual(<<"hash160-outpoint">>, maps:get(<<"type">>, Commitment)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

claim_output_message_rejects_tampering_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = claim_output_message(Raw, 0),
    <<First, Rest/binary>> = Raw,
    Tampers = [
        Msg#{ <<"raw-transaction">> => <<(First bxor 1), Rest/binary>> },
        Msg#{ <<"claim-id">> => <<"0000000000000000000000000000000000000000">> },
        Msg#{ <<"claim">> => <<"forged envelope">> },
        Msg#{ <<"claim-op">> => <<"update">> },
        Msg#{ <<"claim-name">> => <<"forged-name">> },
        Msg#{ <<"nout">> => 1 }
    ],
    lists:foreach(
        fun(Tampered) ->
            ?assertEqual(
                false,
                hb_message:verify(
                    Tampered,
                    #{ <<"commitment-ids">> => <<"all">> },
                    #{}
                )
            )
        end,
        Tampers
    ).

claim_output_message_labels_updates_as_asserted_test() ->
    Raw = update_claim_tx(),
    {ok, Msg} = claim_output_message(Raw, 0),
    ?assertEqual(<<"update">>, maps:get(<<"claim-op">>, Msg)),
    [Commitment] = maps:values(maps:get(<<"commitments">>, Msg)),
    ?assertEqual(<<"asserted-claim-id">>, maps:get(<<"type">>, Commitment)),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

claim_output_verify_rejects_type_forgery_test() ->
    Raw = update_claim_tx(),
    {ok, Msg} = claim_output_message(Raw, 0),
    [{ID, Commitment}] = maps:to_list(maps:get(<<"commitments">>, Msg)),
    % An update commitment relabeled as a hash-derived proof must not verify.
    Forged = Msg#{
        <<"commitments">> =>
            #{ ID => Commitment#{ <<"type">> => <<"hash160-outpoint">> } }
    },
    ?assertEqual(
        false,
        hb_message:verify(Forged, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

channel_output_message_normalizes_public_key_test() ->
    {Compressed, Uncompressed} = sample_channel_keys(),
    SPKIUncompressed =
        <<(binary:decode_hex(
            <<"3056301006072a8648ce3d020106052b8104000a034200">>
        ))/binary, Uncompressed/binary>>,
    lists:foreach(
        fun(StoredKey) ->
            Raw = channel_claim_tx(StoredKey),
            {ok, Msg} = channel_output_message(Raw, 0),
            ?assertEqual(<<"lbry-channel@1.0">>, maps:get(<<"device">>, Msg)),
            ?assertEqual(
                hb_util:to_hex(Compressed),
                maps:get(<<"public-key">>, Msg)
            ),
            ?assertEqual(
                maps:get(<<"claim-id">>, Msg),
                maps:get(<<"channel-id">>, Msg)
            ),
            ?assertEqual(
                true,
                hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
            )
        end,
        [Compressed, Uncompressed, SPKIUncompressed]
    ).

channel_output_message_rejects_tampered_public_key_test() ->
    {Compressed, _} = sample_channel_keys(),
    Raw = channel_claim_tx(Compressed),
    {ok, Msg} = channel_output_message(Raw, 0),
    OtherKey = ar_wallet:compress_ecdsa_pubkey(
        element(1, crypto:generate_key(ecdh, secp256k1, <<2:256>>))
    ),
    Tampered = Msg#{ <<"public-key">> => hb_util:to_hex(OtherKey) },
    ?assertEqual(
        false,
        hb_message:verify(Tampered, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

channel_output_message_rejects_stream_claims_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    ?assertEqual({error, {missing_field, 2}}, channel_output_message(Raw, 0)).

device_tampering_fails_verification_test() ->
    Bytes = <<"encrypted blob bytes">>,
    Hash = hb_lbry_stream_descriptor:blob_hash(Bytes),
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, TxMsg} = transaction_message(Raw),
    {ok, ClaimMsg} = claim_output_message(Raw, 0),
    {ok, StreamMsg} = stream_claim_message(Raw, 0),
    {Compressed, _} = sample_channel_keys(),
    {ok, ChannelMsg} = channel_output_message(channel_claim_tx(Compressed), 0),
    DescriptorRaw = hb_json:encode(sample_descriptor_json()),
    {ok, DescriptorMsg} =
        descriptor_message(
            DescriptorRaw,
            hb_lbry_stream_descriptor:descriptor_hash(DescriptorRaw)
        ),
    Tampered = [
        (blob_message(Hash, Bytes))#{ <<"device">> => <<"lbry-claim@1.0">> },
        TxMsg#{ <<"device">> => <<"lbry-blob@1.0">> },
        ClaimMsg#{ <<"device">> => <<"lbry-blob@1.0">> },
        StreamMsg#{ <<"device">> => <<"lbry-claim@1.0">> },
        ChannelMsg#{ <<"device">> => <<"lbry-claim@1.0">> },
        DescriptorMsg#{ <<"device">> => <<"lbry-blob@1.0">> }
    ],
    lists:foreach(
        fun(Msg) ->
            ?assertEqual(
                false,
                hb_message:verify(
                    Msg,
                    #{ <<"commitment-ids">> => <<"all">> },
                    #{}
                )
            )
        end,
        Tampered
    ).

stream_claim_message_carries_both_commitments_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = stream_claim_message(Raw, 0),
    ?assertEqual(
        <<"3da16b833f169c21caeb62ca66111227413f30f63c9d2f52f2a787643e086c334ee6949e05875cfe94a816aba02e492e">>,
        maps:get(<<"sd-hash">>, Msg)
    ),
    Commitments = maps:get(<<"commitments">>, Msg),
    ?assertEqual(2, map_size(Commitments)),
    Devices =
        lists:sort(
            [
                maps:get(<<"commitment-device">>, Commitment)
             ||
                Commitment <- maps:values(Commitments)
            ]
        ),
    ?assertEqual([<<"lbry-claim@1.0">>, <<"lbry-stream@1.0">>], Devices),
    ?assertEqual(
        true,
        hb_message:verify(Msg, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

stream_claim_message_rejects_tampered_sd_hash_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, Msg} = stream_claim_message(Raw, 0),
    Tampered = Msg#{
        <<"sd-hash">> => hb_util:to_hex(crypto:hash(sha384, <<"other">>))
    },
    ?assertEqual(
        false,
        hb_message:verify(Tampered, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ).

stream_claim_message_rejects_non_stream_claims_test() ->
    {Compressed, _} = sample_channel_keys(),
    Raw = channel_claim_tx(Compressed),
    ?assertEqual({error, {missing_field, 1}}, stream_claim_message(Raw, 0)).

attestation_commitment_rejects_tampered_channel_params_test() ->
    Raw = binary:decode_hex(hb_lbry_tx:task0_tx_hex()),
    {ok, StreamMsg} = stream_claim_message(Raw, 0),
    ChannelMsg = #{
        <<"claim-id">> => <<"585d54c7b82fd92043ed583c5aea18a9547028aa">>,
        <<"public-key">> =>
            <<"03fa4e5fe9f02f2f1a8c34ec150b91f762d8b07b7be942f26aa80c40902d5dbd11">>,
        <<"txid">> =>
            <<"0000000000000000000000000000000000000000000000000000000000000001">>,
        <<"nout">> => 0
    },
    {ok, Committed} = with_attestation_commitment(StreamMsg, ChannelMsg),
    ?assertEqual(
        true,
        hb_message:verify(Committed, #{ <<"commitment-ids">> => <<"all">> }, #{})
    ),
    Commitments = maps:get(<<"commitments">>, Committed),
    [{AttID, Attestation}] =
        [
            {ID, Commitment}
         ||
            {ID, Commitment} <- maps:to_list(Commitments),
            maps:get(<<"commitment-device">>, Commitment) ==
                <<"lbry-channel-attestation@1.0">>
        ],
    OtherKey =
        hb_util:to_hex(
            ar_wallet:compress_ecdsa_pubkey(
                element(1, crypto:generate_key(ecdh, secp256k1, <<2:256>>))
            )
        ),
    Forgeries = [
        Attestation#{ <<"channel-public-key">> => OtherKey },
        Attestation#{
            <<"channel-id">> =>
                <<"0000000000000000000000000000000000000000">>
        }
    ],
    lists:foreach(
        fun(Forged) ->
            Tampered =
                Committed#{
                    <<"commitments">> => Commitments#{ AttID => Forged }
                },
            ?assertEqual(
                false,
                hb_message:verify(
                    Tampered,
                    #{ <<"commitment-ids">> => <<"all">> },
                    #{}
                )
            )
        end,
        Forgeries
    ),
    % The attestation device pin in isolation: verify only the attestation
    % commitment on a device-tampered message, so the failure cannot come
    % from the claim or stream commitments.
    ?assertEqual(
        false,
        hb_message:verify(
            Committed#{ <<"device">> => <<"lbry-claim@1.0">> },
            #{ <<"commitment-ids">> => [AttID] },
            #{}
        )
    ).

sample_channel_keys() ->
    {Uncompressed, _} = crypto:generate_key(ecdh, secp256k1, <<1:256>>),
    {ar_wallet:compress_ecdsa_pubkey(Uncompressed), Uncompressed}.

sample_descriptor_json() ->
    Key = <<0:128>>,
    IV = <<1:128>>,
    Cipher = crypto:crypto_one_time(aes_128_cbc, Key, IV, <<2:128>>, true),
    #{
        <<"stream_type">> => <<"lbryfile">>,
        <<"stream_name">> => hb_util:to_hex(<<"sample.mp4">>),
        <<"key">> => hb_util:to_hex(Key),
        <<"suggested_file_name">> => hb_util:to_hex(<<"sample.mp4">>),
        <<"stream_hash">> => hb_lbry_stream_descriptor:blob_hash(<<"stream">>),
        <<"blobs">> => [
            #{
                <<"length">> => byte_size(Cipher),
                <<"blob_num">> => 0,
                <<"iv">> => hb_util:to_hex(IV),
                <<"blob_hash">> => hb_lbry_stream_descriptor:blob_hash(Cipher)
            },
            #{
                <<"length">> => 0,
                <<"blob_num">> => 1,
                <<"iv">> => hb_util:to_hex(<<0:128>>)
            }
        ]
    }.

channel_claim_tx(StoredKey) ->
    Claim = <<0, (proto_field(2, proto_field(1, StoredKey)))/binary>>,
    create_claim_tx(<<"@channel">>, Claim).

update_claim_tx() ->
    SDHash = crypto:hash(sha384, <<"updated stream">>),
    Claim = <<0, (proto_field(1, proto_field(1, proto_field(6, SDHash))))/binary>>,
    ClaimHash = binary:part(crypto:hash(sha256, <<"prior claim">>), 0, 20),
    Script = <<
        16#b7,
        (script_push(<<"sample">>))/binary,
        (script_push(ClaimHash))/binary,
        (script_push(Claim))/binary,
        16#6d, 16#6d
    >>,
    tx_with_script(Script).

create_claim_tx(Name, Claim) ->
    Script = <<
        16#b5,
        (script_push(Name))/binary,
        (script_push(Claim))/binary,
        16#6d, 16#75
    >>,
    tx_with_script(Script).

tx_with_script(Script) ->
    <<1:32/little-signed,
        1,
        0:256,
        0:32/little,
        0,
        16#ffffffff:32/little,
        1,
        0:64/little,
        (byte_size(Script)),
        Script/binary,
        0:32/little>>.

proto_field(Number, Value) ->
    Key = (Number bsl 3) bor 2,
    <<(proto_varint(Key))/binary,
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
