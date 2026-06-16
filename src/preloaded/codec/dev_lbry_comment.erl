%%% @doc Verify channel-signed Odysee/LBRY comments natively.
%%%
%%% A comment signature commits the comment text to a signing channel at a
%%% timestamp. The signed digest is a single SHA256 over the concatenation of
%%% the timestamp string bytes, the channel claim hash (20 raw bytes in
%%% internal byte order) and the comment body bytes:
%%%
%%%   digest = SHA256(
%%%       signing_ts_bytes
%%%       || channel_claim_hash[20, internal order]
%%%       || comment_body_bytes
%%%   )
%%%
%%% The signature is a 64-byte compact secp256k1 ECDSA signature (`r || s')
%%% verified against the signing channel's secp256k1 public key. High-S
%%% signatures are accepted (normalized on verify), matching lbry-sdk and
%%% commentron.
%%%
%%% Trust note: a valid signature proves the channel authored the comment text
%%% at the given timestamp. It does NOT prove which claim the comment is placed
%%% on -- placement is unsigned commentron metadata and is not covered here.
-module(dev_lbry_comment).
-implements(<<"lbry-comment@1.0">>).
-device_libraries([lib_lbry_codec]).
-export([from/3, to/3, to_hint/3, verify/3, content_type/1]).
-include("include/hb.hrl").

content_type(_) ->
    {ok, <<"application/vnd.lbry.comment+json">>}.

%% @doc Verify a channel-signed comment: rebuild the signing digest from the
%% comment body, signing timestamp and channel claim hash, then check the
%% compact secp256k1 signature against the signing channel public key.
verify(Base, Req, Opts) ->
    Result = comment_verification(Base, Req, Opts),
    Valid =
        case Result of
            {ok, true} -> true;
            _ -> false
        end,
    ?event(lbry_comment, {comment_verify, {valid, Valid}, {result, Result}}),
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

%% @doc Resolve the comment fields from the request or base message, build the
%% signing digest and verify the channel signature. Returns `{ok, true}' or
%% `{ok, false}' for signature outcomes, and `{error, Reason}' for structural
%% failures (missing fields, malformed signature or public key).
comment_verification(Base, Req, Opts) ->
    maybe
        {ok, Comment} ?= field(Base, Req, <<"comment">>, Opts),
        {ok, SigningTs} ?= field(Base, Req, <<"signing-ts">>, Opts),
        {ok, ChannelHash} ?= channel_hash(Base, Req, Opts),
        {ok, PublicKey} ?= channel_public_key(Base, Req, Opts),
        {ok, Signature} ?= signature(Base, Req, Opts),
        hb_lbry_attestation:verify_comment(
            Comment,
            SigningTs,
            ChannelHash,
            {Signature, PublicKey}
        )
    end.

%% @doc Read a binary field, preferring the request and falling back to the
%% base message. Absence is a structural failure.
field(Base, Req, Key, Opts) ->
    case hb_maps:find(Key, Req, Opts) of
        {ok, Value} when is_binary(Value) ->
            {ok, Value};
        _ ->
            case hb_maps:find(Key, Base, Opts) of
                {ok, Value} when is_binary(Value) -> {ok, Value};
                _ -> {error, {missing_field, Key}}
            end
    end.

%% @doc The 20-byte channel claim hash in internal byte order, derived from the
%% 40-hex display claim id by unhexlify-and-reverse (`hb_lbry_attestation').
channel_hash(Base, Req, Opts) ->
    case field(Base, Req, <<"channel-id">>, Opts) of
        {ok, ChannelID} ->
            hb_lbry_attestation:channel_hash(#{ <<"claim_id">> => ChannelID });
        {error, _} = Error ->
            Error
    end.

%% @doc The signing channel secp256k1 public key. Accept a `channel-public-key'
%% hex (33-byte compressed or DER SubjectPublicKeyInfo) directly, falling back
%% to a `public_key' embedded in a channel value map.
channel_public_key(Base, Req, Opts) ->
    case field(Base, Req, <<"channel-public-key">>, Opts) of
        {ok, Hex} ->
            case lib_lbry_codec:hex_to_binary(Hex) of
                {ok, Raw} ->
                    hb_lbry_attestation:normalize_public_key(Raw);
                Error ->
                    Error
            end;
        {error, _} ->
            hb_lbry_attestation:channel_public_key(
                channel_source(Base, Req, Opts)
            )
    end.

channel_source(Base, Req, Opts) ->
    case hb_maps:find(<<"value">>, Req, Opts) of
        {ok, _} -> Req;
        _ -> Base
    end.

%% @doc The 64-byte compact secp256k1 signature, hex-encoded.
signature(Base, Req, Opts) ->
    case field(Base, Req, <<"signature">>, Opts) of
        {ok, Hex} ->
            case lib_lbry_codec:hex_to_binary(Hex) of
                {ok, Sig} when byte_size(Sig) == 64 -> {ok, Sig};
                {ok, _} -> {error, invalid_signature};
                Error -> Error
            end;
        {error, _} = Error ->
            Error
    end.

ensure_device(Msg) ->
    Msg#{ <<"device">> => <<"lbry-comment@1.0">> }.
