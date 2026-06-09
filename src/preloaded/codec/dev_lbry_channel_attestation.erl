-module(dev_lbry_channel_attestation).
-implements(<<"lbry-channel-attestation@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, content_type/1]).

content_type(_) ->
    {ok, <<"application/vnd.lbry.channel-attestation+json">>}.

from(Map, Req, Opts) when is_map(Map) ->
    lib_lbry_codec:from_structured(ensure_device(Map), Req, Opts).

to(Bin, _Req, _Opts) when is_binary(Bin) ->
    {ok, Bin};
to(TABM, Req, Opts) ->
    {ok, Structured} = lib_lbry_codec:to_structured(TABM, Req, Opts),
    lib_lbry_codec:raw_or_structured(ensure_device(Structured), Req, Opts).

to_hint(_Msg, Req, _Opts) ->
    lib_lbry_codec:to_hint(Req).

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-channel-attestation@1.0">> }.
