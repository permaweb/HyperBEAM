%%% @doc Codec for individual LBRY blobs. A blob is a content-addressed unit
%%% of encrypted stream data: its native identifier is the SHA-384 hash of
%%% the encrypted bytes. Blob messages carry the encrypted bytes under
%%% `data' and a `lbry-blob@1.0' commitment, so a node can verify a blob
%%% fetched by hash without holding the stream descriptor.
-module(dev_lbry_blob).
-implements(<<"lbry-blob@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, verify/3, content_type/1]).
-include("include/hb.hrl").

content_type(_) ->
    {ok, <<"application/octet-stream">>}.

from(Map, Req, Opts) when is_map(Map) ->
    lib_lbry_codec:from_structured(ensure_device(Map), Req, Opts);
from(Raw, Req, Opts) when is_binary(Raw) ->
    case hb_maps:get(<<"blob-hash">>, Req, undefined, Opts) of
        undefined ->
            {error, missing_blob_hash};
        Hash ->
            case hb_lbry_stream_descriptor:verify_blob_hash(Hash, Raw) of
                ok ->
                    lib_lbry_codec:from_structured(
                        hb_lbry_commitment:blob_message(Hash, Raw),
                        Req,
                        Opts
                    );
                Error ->
                    Error
            end
    end.

to(Bin, _Req, _Opts) when is_binary(Bin) ->
    {ok, Bin};
to(TABM, Req, Opts) ->
    {ok, Structured} = lib_lbry_codec:to_structured(TABM, Req, Opts),
    case hb_maps:get(<<"format">>, Req, <<"structured">>, Opts) of
        <<"raw">> ->
            case hb_maps:get(<<"data">>, Structured, undefined, Opts) of
                Data when is_binary(Data) -> {ok, Data};
                _ -> {error, missing_blob_data}
            end;
        _ ->
            {ok, ensure_device(Structured)}
    end.

to_hint(_Msg, Req, _Opts) ->
    lib_lbry_codec:to_hint(Req).

%% @doc Verify a blob commitment: the SHA-384 hash of the committed `data'
%% bytes must equal the commitment's native identifier, and the message's
%% `blob-hash' and `device' keys must agree with the commitment. Any missing
%% or mismatching input fails closed.
verify(Base, Req, Opts) ->
    Valid =
        maybe
            <<"sha-384">> ?= hb_maps:get(<<"type">>, Req, undefined, Opts),
            <<"lbry-blob@1.0">> ?= hb_maps:get(<<"device">>, Base, undefined, Opts),
            ok ?=
                hb_lbry_commitment:committed_subset(
                    Req,
                    [<<"blob-hash">>, <<"data">>, <<"device">>],
                    Opts
                ),
            {ok, Hex, Bytes} ?= hb_lbry_commitment:native_id(Req, Opts),
            48 ?= byte_size(Bytes),
            Data = hb_maps:get(<<"data">>, Base, undefined, Opts),
            true ?= is_binary(Data),
            ok ?= hb_lbry_stream_descriptor:verify_blob_hash(Hex, Data),
            Hex == hash_field(Base, Opts)
        else
            _ -> false
        end,
    ?event(lbry_commitment, {blob_verify, {valid, Valid}}),
    {ok, Valid}.

hash_field(Base, Opts) ->
    case hb_maps:get(<<"blob-hash">>, Base, undefined, Opts) of
        Hash when is_binary(Hash) -> hb_util:to_lower(Hash);
        _ -> undefined
    end.

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-blob@1.0">> }.
