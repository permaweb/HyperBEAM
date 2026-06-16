%%% @doc Native LBRY claim codec plus compatibility resolve surface.
-module(dev_lbry_claim).
-implements(<<"lbry-claim@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([info/1, resolve/3, from/3, to/3, to_hint/3, verify/3, content_type/1]).

info(_Opts) ->
    #{
        exports => [
            <<"resolve">>,
            <<"from">>,
            <<"to">>,
            <<"to-hint">>,
            <<"verify">>
        ]
    }.

resolve(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-claim@1.0">>, <<"resolve">>, Base, Req, Opts).

content_type(_) ->
    {ok, <<"application/vnd.lbry.claim">>}.

verify(Base, Req, Opts) ->
    Valid =
        case hb_lbry_commitment:claim_output_verification(Base, Req, Opts) of
            {ok, _Envelope} -> true;
            _ -> false
        end,
    {ok, Valid}.

from(Map, Req, Opts) when is_map(Map) ->
    lib_lbry_codec:from_structured(ensure_device(extract_envelope(Map)), Req, Opts);
from(Raw, Req, Opts) when is_binary(Raw) ->
    case claim_bytes(Raw, Req, Opts) of
        {ok, Bytes} ->
            case hb_lbry_tx:parse_claim_envelope(Bytes) of
                {ok, Envelope} ->
                    lib_lbry_codec:from_structured(ensure_device(Envelope), Req, Opts);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

to(Bin, _Req, _Opts) when is_binary(Bin) ->
    {ok, Bin};
to(TABM, Req, Opts) ->
    {ok, Structured} = lib_lbry_codec:to_structured(TABM, Req, Opts),
    lib_lbry_codec:raw_hex_or_structured(ensure_device(Structured), Req, Opts).

to_hint(_Msg, Req, _Opts) ->
    lib_lbry_codec:to_hint(Req).

claim_bytes(Raw, Req, Opts) ->
    case hb_maps:get(<<"encoding">>, Req, undefined, Opts) of
        <<"hex">> -> lib_lbry_codec:hex_to_binary(Raw);
        _ -> {ok, Raw}
    end.

extract_envelope(#{ <<"claim-envelope">> := Envelope }) when is_map(Envelope) ->
    Envelope;
extract_envelope(Map) ->
    Map.

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-claim@1.0">> }.
