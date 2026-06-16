%%% @doc Construction and verification helpers for native LBRY commitments.
-module(hb_lbry_commitment).
-export([commitment_id/1, native_id/2, native_id_bytes/1, native_id_fields/2]).
-export([claim_output_message/2, claim_output_message/3]).
-export([channel_output_message/2, channel_output_message/3]).
-export([stream_claim_message/2, stream_claim_message/3]).
-export([claim_output_verification/3, channel_output_verification/3]).
-export([stream_output_verification/3]).
-export([outpoint_bytes/2]).

commitment_id(NativeIDBytes) when byte_size(NativeIDBytes) == 32 ->
    hb_util:human_id(NativeIDBytes);
commitment_id(NativeIDBytes) when is_binary(NativeIDBytes) ->
    hb_util:human_id(crypto:hash(sha256, NativeIDBytes)).

native_id_fields(Type, Hex) ->
    case native_id_bytes(Hex) of
        {ok, Normalized, Bytes} ->
            {ok, #{
                <<"signature">> => hb_util:encode(Bytes),
                <<"native-id">> => Normalized,
                <<"native-id-type">> => Type
            }};
        Error ->
            Error
    end.

native_id(Commitment, Opts) ->
    case native_id_bytes(hb_maps:get(<<"native-id">>, Commitment, undefined, Opts)) of
        {ok, Hex, Bytes} ->
            case signature_matches(
                hb_maps:get(<<"signature">>, Commitment, undefined, Opts),
                Bytes
            ) of
                true -> {ok, Hex, Bytes};
                false -> {error, signature_native_id_mismatch}
            end;
        Error ->
            Error
    end.

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
signature_matches(_Signature, _NativeIDBytes) ->
    false.

claim_output_message(Raw, Nout) ->
    claim_output_message(Raw, Nout, undefined).

claim_output_message(_Raw, _Nout, Ancestry) when Ancestry =/= undefined ->
    {error, ancestry_not_supported};
claim_output_message(Raw, Nout, undefined) when is_binary(Raw), is_integer(Nout) ->
    maybe
        {ok, Tx} ?= hb_lbry_tx:parse(Raw),
        {ok, Output} ?= claim_output(Tx, Nout),
        TxIDHex = maps:get(<<"txid">>, Tx),
        ClaimOp = maps:get(<<"claim-op">>, Output),
        ClaimID = maps:get(<<"claim-id">>, Output),
        Type = claim_type(ClaimOp),
        Strength = proof_strength(Type),
        Msg = #{
            <<"device">> => <<"lbry-claim@1.0">>,
            <<"claim-id">> => ClaimID,
            <<"claim-op">> => ClaimOp,
            <<"claim-name">> => maps:get(<<"claim-name">>, Output),
            <<"claim">> => maps:get(<<"claim">>, Output),
            <<"claim-envelope">> => maps:get(<<"claim-envelope">>, Output),
            <<"claim-proof-strength">> => Strength,
            <<"txid">> => TxIDHex,
            <<"nout">> => Nout,
            <<"raw-transaction">> => Raw
        },
        {ok,
            with_commitment(
                Msg,
                <<"lbry-claim@1.0">>,
                Type,
                {<<"outpoint">>, outpoint_bytes(TxIDHex, Nout)},
                claim_committed_list(),
                #{
                    <<"claim-id">> => ClaimID,
                    <<"claim-op">> => ClaimOp,
                    <<"claim-proof-strength">> => Strength
                }
            )}
    end.

channel_output_message(Raw, Nout) ->
    channel_output_message(Raw, Nout, undefined).

channel_output_message(_Raw, _Nout, Ancestry) when Ancestry =/= undefined ->
    {error, ancestry_not_supported};
channel_output_message(Raw, Nout, undefined) ->
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
        Type = claim_commitment_type(ClaimMsg),
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
                Type,
                {<<"outpoint">>, outpoint_bytes(TxIDHex, Nout)},
                lists:sort(claim_committed_list() ++ [<<"channel-id">>, <<"public-key">>]),
                #{
                    <<"claim-id">> => ClaimID,
                    <<"claim-op">> => ClaimOp,
                    <<"claim-proof-strength">> => proof_strength(Type),
                    <<"public-key">> => PublicKeyHex
                }
            )}
    end.

stream_claim_message(Raw, Nout) ->
    stream_claim_message(Raw, Nout, undefined).

stream_claim_message(_Raw, _Nout, Ancestry) when Ancestry =/= undefined ->
    {error, ancestry_not_supported};
stream_claim_message(Raw, Nout, undefined) ->
    maybe
        {ok, ClaimMsg} ?= claim_output_message(Raw, Nout),
        Envelope = maps:get(<<"claim-envelope">>, ClaimMsg),
        {ok, SDHash} ?=
            hb_lbry_claim_proto:stream_sd_hash(maps:get(<<"message">>, Envelope)),
        ClaimID = maps:get(<<"claim-id">>, ClaimMsg),
        ClaimOp = maps:get(<<"claim-op">>, ClaimMsg),
        Type = claim_commitment_type(ClaimMsg),
        TxIDHex = maps:get(<<"txid">>, ClaimMsg),
        Msg = ClaimMsg#{
            <<"device">> => <<"lbry-stream@1.0">>,
            <<"sd-hash">> => SDHash
        },
        {ok,
            share_committed_keys(
                with_commitment(
                    Msg,
                    <<"lbry-stream@1.0">>,
                    Type,
                    {<<"sd-hash">>, binary:decode_hex(SDHash)},
                    lists:sort(claim_committed_list() ++ [<<"sd-hash">>]),
                    #{
                        <<"claim-id">> => ClaimID,
                        <<"claim-op">> => ClaimOp,
                        <<"claim-proof-strength">> => proof_strength(Type),
                        <<"outpoint">> => hb_util:to_hex(outpoint_bytes(TxIDHex, Nout))
                    }
                )
            )}
    end.

claim_output_verification(Base, Req, Opts) ->
    maybe
        {ok, _Hex, OutpointBytes} ?= native_id(Req, Opts),
        verify_claim_output(Base, Req, OutpointBytes, Opts)
    else
        {error, _} = Error -> Error;
        _ -> {error, claim_output_mismatch}
    end.

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
        {ok, Tx, Output} ?= output_evidence(Base, OutpointBytes, Opts),
        ClaimOp = maps:get(<<"claim-op">>, Output),
        ClaimOp ?= hb_maps:get(<<"claim-op">>, Base, undefined, Opts),
        ClaimOp ?= hb_maps:get(<<"claim-op">>, Req, undefined, Opts),
        ok ?=
            verify_claim_proof(
                hb_maps:get(<<"type">>, Req, undefined, Opts),
                ClaimOp,
                Base,
                Req,
                Opts
            ),
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

verify_claim_proof(Type, ClaimOp, Base, Req, Opts) ->
    maybe
        ok ?= claim_type_shape(Type, ClaimOp),
        Strength = proof_strength(Type),
        Strength ?= lower_field(Base, <<"claim-proof-strength">>, Opts),
        Strength ?= lower_field(Req, <<"claim-proof-strength">>, Opts),
        ok
    else
        {error, _} = Error -> Error;
        _ -> {error, claim_proof_mismatch}
    end.

claim_type_shape(<<"hash160-outpoint">>, <<"create">>) -> ok;
claim_type_shape(<<"asserted-claim-id">>, <<"update">>) -> ok;
claim_type_shape(_Type, _ClaimOp) -> {error, claim_type_mismatch}.

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

claim_output(Tx, Nout) ->
    Outputs = [
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

claim_commitment_type(ClaimMsg) ->
    [Commitment] = maps:values(maps:get(<<"commitments">>, ClaimMsg)),
    maps:get(<<"type">>, Commitment).

claim_committed_list() ->
    [
        <<"claim">>,
        <<"claim-id">>,
        <<"claim-name">>,
        <<"claim-op">>,
        <<"claim-proof-strength">>,
        <<"device">>,
        <<"nout">>,
        <<"raw-transaction">>,
        <<"txid">>
    ].

share_committed_keys(Msg) ->
    Commitments = maps:get(<<"commitments">>, Msg, #{}),
    Keys =
        lists:usort(
            lists:flatten([
                maps:get(<<"committed">>, Commitment, [])
             ||
                Commitment <- maps:values(Commitments)
            ])
        ),
    Msg#{
        <<"commitments">> =>
            maps:map(
                fun(_ID, Commitment) ->
                    Commitment#{ <<"committed">> => Keys }
                end,
                Commitments
            )
    }.

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

with_commitment(Msg, Device, Type, NativeIDSpec, Committed, Extra) ->
    {ID, Commitment} = commitment(Device, Type, NativeIDSpec, Committed, Extra),
    Commitments = maps:get(<<"commitments">>, Msg, #{}),
    Msg#{ <<"commitments">> => Commitments#{ ID => Commitment } }.

claim_type(<<"create">>) -> <<"hash160-outpoint">>;
claim_type(<<"update">>) -> <<"asserted-claim-id">>.

proof_strength(<<"hash160-outpoint">>) -> <<"hash-derived">>;
proof_strength(<<"asserted-claim-id">>) -> <<"asserted">>.

outpoint_bytes(TxIDHex, Nout) when is_binary(TxIDHex), is_integer(Nout), Nout >= 0 ->
    <<(binary:decode_hex(hb_util:to_lower(TxIDHex)))/binary, Nout:32/big>>.

split_outpoint(<<TxIDBytes:32/binary, Nout:32/big>>) ->
    {ok, hb_util:to_hex(TxIDBytes), Nout};
split_outpoint(_) ->
    {error, invalid_outpoint}.
