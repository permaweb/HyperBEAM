-module(dev_lbry_channel_attestation).
-implements(<<"lbry-channel-attestation@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, verify/3, content_type/1]).
-include("include/hb.hrl").

content_type(_) ->
    {ok, <<"application/vnd.lbry.channel-attestation+json">>}.

%% @doc Verify a channel-attestation commitment: the committed claim
%% envelope's secp256k1 signature against the recorded channel public key
%% and the embedded signing-channel binding. See
%% `hb_lbry_commitment:attestation_verification/3'.
verify(Base, Req, Opts) ->
    Result = hb_lbry_commitment:attestation_verification(Base, Req, Opts),
    Valid =
        case Result of
            {ok, _ChannelID} -> true;
            _ -> false
        end,
    ?event(lbry_commitment,
        {attestation_verify, {valid, Valid}, {result, Result}}
    ),
    {ok, Valid}.

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
