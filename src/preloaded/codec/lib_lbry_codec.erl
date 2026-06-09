-module(lib_lbry_codec).
-export([
    from_structured/3,
    to_structured/3,
    to_hint/1,
    raw_or_structured/3,
    raw_hex_or_structured/3,
    hex_to_binary/1
]).

from_structured(Msg, Req, Opts) ->
    ConvOpts = Opts#{ <<"hashpath">> => ignore },
    {ok,
        hb_message:convert(
            Msg,
            tabm,
            Req#{
                <<"device">> => <<"structured@1.0">>,
                <<"bundle">> => true
            },
            ConvOpts
        )
    }.

to_structured(TABM, _Req, Opts) ->
    ConvOpts = Opts#{ <<"hashpath">> => ignore },
    {ok,
        hb_message:convert(
            TABM,
            <<"structured@1.0">>,
            tabm,
            ConvOpts
        )
    }.

to_hint(Req) ->
    {ok, Req#{ <<"bundle">> => true }}.

raw_or_structured(Structured, Req, Opts) ->
    case hb_maps:get(<<"format">>, Req, <<"structured">>, Opts) of
        <<"raw">> -> raw(Structured, Opts);
        _ -> {ok, Structured}
    end.

raw_hex_or_structured(Structured, Req, Opts) ->
    case hb_maps:get(<<"format">>, Req, <<"structured">>, Opts) of
        <<"raw">> -> raw(Structured, Opts);
        <<"hex">> ->
            case raw(Structured, Opts) of
                {ok, Raw} when is_binary(Raw) -> {ok, hb_util:to_hex(Raw)};
                {ok, _} -> {error, invalid_raw_hex};
                Error -> Error
            end;
        _ -> {ok, Structured}
    end.

hex_to_binary(Hex) when is_binary(Hex) ->
    try binary:decode_hex(hb_util:to_lower(Hex)) of
        Bin -> {ok, Bin}
    catch
        _:_ -> {error, invalid_hex}
    end.

raw(Structured, Opts) ->
    case maps:get(<<"raw">>, Structured, undefined) of
        undefined -> {error, missing_raw};
        Raw -> {ok, hb_cache:ensure_all_loaded(Raw, Opts)}
    end.
