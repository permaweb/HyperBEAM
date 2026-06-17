-module(dev_lbry_stream).
-implements(<<"lbry-stream@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, verify/3, content_type/1]).
-include("include/hb.hrl").

content_type(_) ->
    {ok, <<"application/vnd.lbry.stream+json">>}.

%% @doc Verify a stream commitment: the claim-output binding plus the
%% descriptor `sd_hash' re-derived from the stream claim protobuf. See
%% `hb_lbry_commitment:stream_output_verification/3'.
verify(Base, Req, Opts) ->
    Result = hb_lbry_commitment:stream_output_verification(Base, Req, Opts),
    Valid =
        case Result of
            {ok, _Envelope} -> true;
            _ -> false
        end,
    ?event(lbry_commitment, {stream_verify, {valid, Valid}, {result, Result}}),
    {ok, Valid}.

from(Map, Req, Opts) when is_map(Map) ->
    case normalize(Map) of
        {ok, Normalized} -> lib_lbry_codec:from_structured(Normalized, Req, Opts);
        Error -> Error
    end.

to(Bin, _Req, _Opts) when is_binary(Bin) ->
    {ok, Bin};
to(TABM, Req, Opts) ->
    {ok, Structured} = lib_lbry_codec:to_structured(TABM, Req, Opts),
    lib_lbry_codec:raw_or_structured(ensure_device(Structured), Req, Opts).

to_hint(_Msg, Req, _Opts) ->
    lib_lbry_codec:to_hint(Req).

normalize(Stream = #{ <<"device">> := <<"lbry-stream@1.0">> }) ->
    {ok, ensure_device(Stream)};
normalize(Stream) ->
    case source_sd_hash(Stream) of
        {ok, SDHash} -> {ok, normalize(Stream, SDHash)};
        not_found -> {ok, normalize(Stream, undefined)};
        Error -> Error
    end.

normalize(Stream, SDHash) ->
    Base = #{
        <<"device">> => <<"lbry-stream@1.0">>,
        <<"raw">> => Stream
    },
    fold_optional(
        [
            {<<"claim-id">>, maps:get(<<"claim_id">>, Stream, undefined)},
            {<<"name">>, maps:get(<<"name">>, Stream, undefined)},
            {<<"txid">>, maps:get(<<"txid">>, Stream, undefined)},
            {<<"nout">>, maps:get(<<"nout">>, Stream, undefined)},
            {<<"sd-hash">>, SDHash},
            {<<"signing-channel">>, maps:get(<<"signing_channel">>, Stream, undefined)}
        ],
        Base
    ).

source_sd_hash(Stream) ->
    case claim_envelope(Stream) of
        {ok, #{ <<"message">> := Message }} ->
            hb_lbry_claim_proto:stream_sd_hash(Message);
        {ok, _Envelope} ->
            {error, missing_claim_message};
        not_found ->
            not_found;
        Error ->
            Error
    end.

claim_envelope(#{ <<"claim-envelope">> := Envelope }) when is_map(Envelope) ->
    {ok, Envelope};
claim_envelope(#{ <<"claim-envelope">> := _Envelope }) ->
    {error, invalid_claim_envelope};
claim_envelope(#{ <<"claim">> := #{ <<"claim-envelope">> := Envelope } })
        when is_map(Envelope) ->
    {ok, Envelope};
claim_envelope(#{ <<"claim">> := #{ <<"claim-envelope">> := _Envelope } }) ->
    {error, invalid_claim_envelope};
claim_envelope(_) ->
    not_found.

fold_optional([], Acc) ->
    Acc;
fold_optional([{_Key, undefined} | Rest], Acc) ->
    fold_optional(Rest, Acc);
fold_optional([{_Key, not_found} | Rest], Acc) ->
    fold_optional(Rest, Acc);
fold_optional([{Key, Value} | Rest], Acc) ->
    fold_optional(Rest, Acc#{ Key => Value }).

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-stream@1.0">> }.
