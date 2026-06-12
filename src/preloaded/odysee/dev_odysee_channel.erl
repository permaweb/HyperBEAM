%%% @doc Odysee channel identity compatibility device.
%%%
%%% This device normalizes channel claims and signing-channel envelopes into an
%%% AO-Core message while preserving the source claim data used for later
%%% comment and stream signature verification.
-module(dev_odysee_channel).
-implements(<<"odysee-channel@1.0">>).
-export([info/1, channel/3, from_claim/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEVICE, <<"odysee-channel@1.0">>).

%% @doc Return the public device API.
info(_Opts) ->
    #{ exports => [<<"channel">>, <<"from-claim">>] }.

%% @doc Resolve/derive a channel identity message.
channel(Base, Req, Opts) ->
    from_claim(Base, Req, Opts).

%% @doc Normalize a channel claim or a stream claim's `signing_channel'.
from_claim(Base, Req, Opts) ->
    safe(fun() ->
        maybe
            {ok, Claim, SourceClaim} ?= ensure_channel_claim(Base, Req, Opts),
            ok_message(normalize_channel(Claim, SourceClaim, Opts))
        else
            Error -> Error
        end
    end).

safe(Fun) ->
    try Fun() of
        Res -> Res
    catch
        _:{error, Reason} -> {error, Reason};
        _:Reason -> {error, Reason}
    end.

ok_message(Msg) when is_map(Msg) -> {ok, Msg};
ok_message(Error) -> Error.

ensure_channel_claim(Base, Req, Opts) ->
    case channel_candidate(Base, Req, Opts) of
        {ok, _Claim, _SourceClaim} = Channel ->
            Channel;
        not_found ->
            case hb_ao:raw(<<"odysee-claim@1.0">>, <<"resolve">>, Base, Req, Opts) of
                {ok, ClaimMsg} ->
                    case candidate_from_value(ClaimMsg, Opts) of
                        {ok, _Claim, _SourceClaim} = Channel -> Channel;
                        not_found -> {error, channel_not_found}
                    end;
                Error ->
                    Error
            end
    end.

channel_candidate(Base, Req, Opts) ->
    Candidates = [
        {Req, <<"channel">>},
        {Req, <<"signing-channel">>},
        {Req, <<"signing_channel">>},
        {Req, <<"claim">>},
        {Req, <<"source">>},
        {Req, <<"body">>},
        {Base, <<"channel">>},
        {Base, <<"signing-channel">>},
        {Base, <<"signing_channel">>},
        {Base, <<"claim">>},
        {Base, <<"source">>},
        {Base, <<"body">>}
    ],
    case candidate_from_value(Base, Opts) of
        {ok, _Claim, _SourceClaim} = Channel -> Channel;
        not_found -> candidate_from_fields(Candidates, Opts)
    end.

candidate_from_fields([], _Opts) ->
    not_found;
candidate_from_fields([{Msg, Key} | Rest], Opts) when is_map(Msg) ->
    case hb_maps:get(Key, Msg, not_found, Opts) of
        not_found -> candidate_from_fields(Rest, Opts);
        Value ->
            case candidate_from_value(Value, Opts) of
                {ok, _Claim, _SourceClaim} = Channel -> Channel;
                not_found -> candidate_from_fields(Rest, Opts)
            end
    end;
candidate_from_fields([_ | Rest], Opts) ->
    candidate_from_fields(Rest, Opts).

candidate_from_value(Value, Opts) when is_binary(Value) ->
    case try_decode_json(Value) of
        {ok, Decoded} -> candidate_from_value(Decoded, Opts);
        _ -> not_found
    end;
candidate_from_value(Value, Opts) when is_map(Value) ->
    Claim = hb_maps:get(<<"claim">>, Value, Value, Opts),
    case channel_from_claim(Claim, Opts) of
        {ok, Channel} ->
            {ok, Channel, Claim};
        not_found ->
            case signing_channel_from_claim(Claim, Opts) of
                {ok, Channel} -> {ok, Channel, Claim};
                not_found -> not_found
            end
    end;
candidate_from_value(_Value, _Opts) ->
    not_found.

channel_from_claim(Claim, Opts) when is_map(Claim) ->
    case value_type(Claim, Opts) of
        <<"channel">> -> {ok, Claim};
        not_found ->
            case has_channel_public_key(Claim, Opts) of
                true -> {ok, Claim};
                false -> not_found
            end;
        _ -> not_found
    end;
channel_from_claim(_Claim, _Opts) ->
    not_found.

signing_channel_from_claim(Claim, Opts) when is_map(Claim) ->
    case first_value([<<"signing_channel">>, <<"signing-channel">>], Claim, Opts) of
        Channel when is_map(Channel) -> {ok, Channel};
        _ -> not_found
    end;
signing_channel_from_claim(_Claim, _Opts) ->
    not_found.

normalize_channel(Claim, SourceClaim, Opts) ->
    maybe
        {ok, ClaimID} ?= required_first([<<"claim_id">>, <<"claim-id">>], Claim, Opts),
        {ok, ClaimName} ?= required_first([<<"name">>, <<"claim-name">>], Claim, Opts),
        {ok, Value} ?= required(<<"value">>, Claim, Opts),
        PublicKey = first_value([<<"public_key">>, <<"public-key">>], Value, Opts),
        Msg0 = #{
            <<"device">> => ?DEVICE,
            <<"content-type">> => <<"application/json">>,
            <<"body">> => hb_json:encode(Claim),
            <<"claim">> => Claim,
            <<"value">> => Value,
            <<"claim-id">> => ClaimID,
            <<"channel-id">> => ClaimID,
            <<"claim-name">> => ClaimName,
            <<"channel-name">> => ClaimName,
            <<"claim-store-path">> => <<"odysee/claim-id/", ClaimID/binary>>,
            <<"channel-store-path">> => <<"odysee/channel/", ClaimID/binary>>,
            <<"identity-type">> => <<"channel">>
        },
        Msg1 =
            case SourceClaim =:= Claim of
                true -> Msg0;
                false -> Msg0#{ <<"source-claim">> => SourceClaim }
            end,
        Optional = [
            {<<"value-type">>, value_type(Claim, Opts)},
            {<<"canonical-url">>, canonical_url(Claim, Opts)},
            {<<"permanent-url">>, first_value([<<"permanent_url">>, <<"permanent-url">>], Claim, Opts)},
            {<<"short-url">>, first_value([<<"short_url">>, <<"short-url">>], Claim, Opts)},
            {<<"title">>, first_value([<<"title">>], Value, Opts)},
            {<<"description">>, first_value([<<"description">>], Value, Opts)},
            {<<"thumbnail">>, thumbnail_url(Value, Opts)},
            {<<"tags">>, first_value([<<"tags">>], Value, Opts)},
            {<<"languages">>, first_value([<<"languages">>], Value, Opts)},
            {<<"public-key">>, PublicKey},
            {<<"public-key-id">>, first_value([<<"public_key_id">>, <<"public-key-id">>], Value, Opts)},
            {<<"signature-valid">>, first_value([<<"signature_valid">>, <<"signature-valid">>], SourceClaim, Opts)},
            {<<"committer-format">>, public_key_format(PublicKey)},
            {<<"ao-committer">>, PublicKey},
            {<<"claim-proof-store-path">>, claim_proof_store_path(Claim, Opts)},
            {<<"txid">>, first_value([<<"txid">>], Claim, Opts)},
            {<<"nout">>, first_value([<<"nout">>], Claim, Opts)},
            {<<"height">>, first_value([<<"height">>], Claim, Opts)},
            {<<"claim-op">>, first_value([<<"claim_op">>, <<"claim-op">>], Claim, Opts)}
        ],
        lists:foldl(fun put_optional/2, Msg1, Optional)
    end.

required(Key, Map, Opts) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> {error, {missing, Key}};
        Value -> {ok, Value}
    end.

required_first(Keys, Map, Opts) ->
    case first_value(Keys, Map, Opts) of
        not_found -> {error, {missing, hd(Keys)}};
        Value -> {ok, Value}
    end.

value_type(Claim, Opts) ->
    first_value([<<"value_type">>, <<"value-type">>], Claim, Opts).

canonical_url(Claim, Opts) ->
    first_value(
        [
            <<"canonical_url">>,
            <<"canonical-url">>,
            <<"permanent_url">>,
            <<"permanent-url">>,
            <<"short_url">>,
            <<"short-url">>
        ],
        Claim,
        Opts
    ).

has_channel_public_key(Claim, Opts) ->
    case hb_maps:get(<<"value">>, Claim, not_found, Opts) of
        Value when is_map(Value) ->
            first_value([<<"public_key">>, <<"public-key">>], Value, Opts) =/= not_found;
        _ ->
            false
    end.

thumbnail_url(Value, Opts) ->
    case first_value([<<"thumbnail">>], Value, Opts) of
        Thumbnail when is_map(Thumbnail) -> first_value([<<"url">>], Thumbnail, Opts);
        Other -> Other
    end.

public_key_format(not_found) -> not_found;
public_key_format(_PublicKey) -> <<"lbry-channel-public-key">>.

claim_proof_store_path(Claim, Opts) ->
    case {first_value([<<"txid">>], Claim, Opts), first_value([<<"nout">>], Claim, Opts)} of
        {TxID, NOut} when is_binary(TxID), is_integer(NOut) orelse is_binary(NOut) ->
            <<"odysee/claim-proof/", TxID/binary, "/", (path_int(NOut))/binary>>;
        _ ->
            not_found
    end.

path_int(Int) when is_integer(Int) ->
    integer_to_binary(Int);
path_int(Bin) when is_binary(Bin) ->
    Bin.

first_value([], _Map, _Opts) ->
    not_found;
first_value([Key | Rest], Map, Opts) when is_map(Map) ->
    case hb_maps:get(Key, Map, not_found, Opts) of
        not_found -> first_value(Rest, Map, Opts);
        Value -> Value
    end;
first_value(_Keys, _Map, _Opts) ->
    not_found.

put_optional({_Key, not_found}, Msg) -> Msg;
put_optional({Key, Value}, Msg) -> Msg#{ Key => Value }.

try_decode_json(Raw) ->
    try {ok, hb_json:decode(Raw)}
    catch _:_ -> {error, invalid_json}
    end.

-ifdef(TEST).

channel_from_direct_claim_test() ->
    Claim = channel_claim(),
    {ok, Msg} = channel(#{}, #{ <<"claim">> => Claim }, #{}),
    ?assertEqual(<<"f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1">>, hb_maps:get(<<"channel-id">>, Msg, #{})),
    ?assertEqual(<<"@veritasium">>, hb_maps:get(<<"channel-name">>, Msg, #{})),
    ?assertEqual(<<"3082010a0282010100">>, hb_maps:get(<<"public-key">>, Msg, #{})).

channel_from_stream_signing_channel_test() ->
    StreamClaim = stream_claim(),
    {ok, Msg} = channel(#{}, #{ <<"claim">> => StreamClaim }, #{}),
    ?assertEqual(<<"@veritasium">>, hb_maps:get(<<"channel-name">>, Msg, #{})),
    ?assertEqual(StreamClaim, hb_maps:get(<<"source-claim">>, Msg, #{})),
    ?assertEqual(true, hb_maps:get(<<"signature-valid">>, Msg, #{})).

channel_from_claim_message_test() ->
    Claim = channel_claim(),
    ClaimMsg = #{
        <<"claim">> => Claim,
        <<"claim-id">> => hb_maps:get(<<"claim_id">>, Claim, #{}),
        <<"claim-name">> => hb_maps:get(<<"name">>, Claim, #{}),
        <<"value">> => hb_maps:get(<<"value">>, Claim, #{})
    },
    {ok, Msg} = channel(ClaimMsg, #{}, #{}),
    ?assertEqual(<<"channel">>, hb_maps:get(<<"value-type">>, Msg, #{})).

channel_rejects_unsigned_stream_test() ->
    ?assertMatch(
        {error, channel_not_found},
        channel(#{}, #{ <<"claim">> => maps:remove(<<"signing_channel">>, stream_claim()) }, #{})
    ).

channel_claim() ->
    #{
        <<"claim_id">> => <<"f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1f1">>,
        <<"canonical_url">> => <<"lbry://@veritasium#f">>,
        <<"name">> => <<"@veritasium">>,
        <<"value_type">> => <<"channel">>,
        <<"value">> => #{
            <<"title">> => <<"Veritasium">>,
            <<"description">> => <<"An element of truth.">>,
            <<"public_key">> => <<"3082010a0282010100">>,
            <<"public_key_id">> => <<"bLGr4w">>,
            <<"thumbnail">> => #{ <<"url">> => <<"https://thumbnails.lbry.com/veritasium">> },
            <<"tags">> => [<<"science">>, <<"education">>]
        }
    }.

stream_claim() ->
    #{
        <<"claim_id">> => <<"346c1fed0fbc2f0b3ecc8bf3915aa8aaa029c169">>,
        <<"name">> => <<"why-is-it-so-easy-to-disrupt-gps">>,
        <<"value_type">> => <<"stream">>,
        <<"value">> => #{ <<"title">> => <<"Why Is It So Easy To Disrupt GPS?">> },
        <<"signing_channel">> => channel_claim(),
        <<"signature_valid">> => true
    }.

-endif.
