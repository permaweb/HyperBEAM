-module(dev_lbry_channel).
-implements(<<"lbry-channel@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, verify/3, content_type/1]).
-include("include/hb.hrl").

content_type(_) ->
    {ok, <<"application/vnd.lbry.channel+json">>}.

%% @doc Verify a channel-output commitment: the claim-output binding plus
%% the channel public key re-derived from the raw channel claim protobuf.
%% See `hb_lbry_commitment:channel_output_verification/3'.
verify(Base, Req, Opts) ->
    Result = hb_lbry_commitment:channel_output_verification(Base, Req, Opts),
    Valid =
        case Result of
            {ok, _PublicKeyHex} -> true;
            _ -> false
        end,
    ?event(lbry_commitment, {channel_verify, {valid, Valid}, {result, Result}}),
    {ok, Valid}.

from(Map, Req, Opts) when is_map(Map) ->
    case normalize(Map) of
        {ok, Channel} ->
            lib_lbry_codec:from_structured(Channel, Req, Opts);
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
