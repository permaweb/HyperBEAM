%%% @doc Native LBRY channel codec plus compatibility channel surfaces.
-module(dev_lbry_channel).
-implements(<<"lbry-channel@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([
    info/1,
    channel/3,
    from_claim/3,
    from/3,
    to/3,
    to_hint/3,
    verify/3,
    content_type/1
]).

info(_Opts) ->
    #{
        exports => [
            <<"channel">>,
            <<"from-claim">>,
            <<"from">>,
            <<"to">>,
            <<"to-hint">>,
            <<"verify">>
        ]
    }.

channel(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-channel@1.0">>, <<"channel">>, Base, Req, Opts).

from_claim(Base, Req, Opts) ->
    hb_ao:raw(<<"odysee-channel@1.0">>, <<"from-claim">>, Base, Req, Opts).

content_type(_) ->
    {ok, <<"application/vnd.lbry.channel+json">>}.

verify(Base, Req, Opts) ->
    Valid =
        case hb_lbry_commitment:channel_output_verification(Base, Req, Opts) of
            {ok, _PublicKeyHex} -> true;
            _ -> false
        end,
    {ok, Valid}.

from(Map, Req, Opts) when is_map(Map) ->
    case normalize(Map) of
        {ok, ChannelMsg} ->
            lib_lbry_codec:from_structured(ChannelMsg, Req, Opts);
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

normalize(Channel = #{ <<"device">> := <<"lbry-channel@1.0">> }) ->
    {ok, ensure_device(Channel)};
normalize(Channel) ->
    maybe
        {ok, ChannelHash} ?= hb_lbry_attestation:channel_hash(Channel),
        {ok, PublicKey} ?= hb_lbry_attestation:channel_public_key(Channel),
        {ok, ensure_device(#{
            <<"raw">> => Channel,
            <<"channel-id">> => hb_util:to_hex(reverse(ChannelHash)),
            <<"channel-hash">> => ChannelHash,
            <<"public-key">> => hb_util:to_hex(PublicKey)
        })}
    end.

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-channel@1.0">> }.

reverse(Bin) ->
    list_to_binary(lists:reverse(binary_to_list(Bin))).
