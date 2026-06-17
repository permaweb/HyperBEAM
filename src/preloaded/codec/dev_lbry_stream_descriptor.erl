-module(dev_lbry_stream_descriptor).
-implements(<<"lbry-stream-descriptor@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, verify/3, content_type/1]).
-include("include/hb.hrl").

content_type(_) ->
    {ok, <<"application/vnd.lbry.stream-descriptor+json">>}.

from(Map, Req, Opts) when is_map(Map) ->
    lib_lbry_codec:from_structured(ensure_device(Map), Req, Opts);
from(Raw, Req, Opts) when is_binary(Raw) ->
    Result =
        case hb_maps:get(<<"sd-hash">>, Req, undefined, Opts) of
            undefined -> hb_lbry_stream_descriptor:parse(Raw);
            SDHash -> hb_lbry_commitment:descriptor_message(Raw, SDHash)
        end,
    case Result of
        {ok, Descriptor} ->
            lib_lbry_codec:from_structured(Descriptor, Req, Opts);
        Error ->
            Error
    end.

to(Bin, _Req, _Opts) when is_binary(Bin) ->
    {ok, Bin};
to(TABM, Req, Opts) ->
    {ok, Structured} = lib_lbry_codec:to_structured(TABM, Req, Opts),
    lib_lbry_codec:raw_or_structured(ensure_device(Structured), Req, Opts).

to_hint(_Msg, Req, _Opts) ->
    lib_lbry_codec:to_hint(Req).

%% @doc Verify a stream descriptor commitment: the raw descriptor bytes must
%% hash to the commitment's native `sd_hash' and parse into a structurally
%% valid stream descriptor. The message's `sd-hash' and `device' keys must
%% agree with the commitment. Any missing or mismatching input fails closed.
verify(Base, Req, Opts) ->
    Valid =
        maybe
            <<"sha-384">> ?= hb_maps:get(<<"type">>, Req, undefined, Opts),
            <<"lbry-stream-descriptor@1.0">> ?=
                hb_maps:get(<<"device">>, Base, undefined, Opts),
            ok ?=
                hb_lbry_commitment:committed_subset(
                    Req,
                    [<<"device">>, <<"raw">>, <<"sd-hash">>],
                    Opts
                ),
            {ok, Hex, Bytes} ?= hb_lbry_commitment:native_id(Req, Opts),
            48 ?= byte_size(Bytes),
            Raw = hb_maps:get(<<"raw">>, Base, undefined, Opts),
            true ?= is_binary(Raw),
            {ok, _Descriptor} ?= hb_lbry_stream_descriptor:parse(Raw, Hex),
            Hex == sd_hash_field(Base, Opts)
        else
            _ -> false
        end,
    ?event(lbry_commitment, {descriptor_verify, {valid, Valid}}),
    {ok, Valid}.

sd_hash_field(Base, Opts) ->
    case hb_maps:get(<<"sd-hash">>, Base, undefined, Opts) of
        SDHash when is_binary(SDHash) -> hb_util:to_lower(SDHash);
        _ -> undefined
    end.

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-stream-descriptor@1.0">> }.
