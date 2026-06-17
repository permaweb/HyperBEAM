-module(hb_lbry_claim_proto).
-export([stream_sd_hash/1, channel_public_key/1]).
-include_lib("eunit/include/eunit.hrl").

stream_sd_hash(Message) when is_binary(Message) ->
    maybe
        {ok, Stream} ?= length_field(Message, 1),
        {ok, Source} ?= length_field(Stream, 1),
        {ok, SDHash} ?= length_field(Source, 6),
        ok ?= valid_hash(SDHash),
        {ok, hb_util:to_hex(SDHash)}
    end.

%% @doc Extract the raw channel public key bytes from a channel claim
%% protobuf (`Claim.channel.public_key'). The bytes are returned untouched:
%% legacy channels store DER/SPKI-wrapped keys, which the caller must
%% normalize before use.
channel_public_key(Message) when is_binary(Message) ->
    maybe
        {ok, Channel} ?= length_field(Message, 2),
        {ok, PublicKey} ?= length_field(Channel, 1),
        {ok, PublicKey}
    end.

length_field(Message, FieldNum) ->
    case find_field(Message, FieldNum) of
        {ok, 2, Value} -> {ok, Value};
        {ok, WireType, _Value} -> {error, {invalid_wire_type, FieldNum, WireType}};
        Error -> Error
    end.

find_field(<<>>, FieldNum) ->
    {error, {missing_field, FieldNum}};
find_field(Message, FieldNum) ->
    maybe
        {ok, Key, Rest} ?= read_varint(Message),
        Number = Key bsr 3,
        WireType = Key band 7,
        {ok, Value, Tail} ?= read_value(WireType, Rest),
        case Number of
            FieldNum -> {ok, WireType, Value};
            _ -> find_field(Tail, FieldNum)
        end
    end.

read_value(0, Raw) ->
    read_varint(Raw);
read_value(1, <<Value:8/binary, Rest/binary>>) ->
    {ok, Value, Rest};
read_value(1, _) ->
    {error, truncated_fixed64};
read_value(2, Raw) ->
    maybe
        {ok, Size, Rest} ?= read_varint(Raw),
        take(Size, Rest)
    end;
read_value(5, <<Value:4/binary, Rest/binary>>) ->
    {ok, Value, Rest};
read_value(5, _) ->
    {error, truncated_fixed32};
read_value(WireType, _Raw) ->
    {error, {unsupported_wire_type, WireType}}.

read_varint(Raw) ->
    read_varint(Raw, 0, 0).

read_varint(<<Byte, Rest/binary>>, Shift, Acc) when Shift < 70 ->
    Value = Acc bor ((Byte band 16#7f) bsl Shift),
    case Byte band 16#80 of
        0 -> {ok, Value, Rest};
        _ -> read_varint(Rest, Shift + 7, Value)
    end;
read_varint(_, _Shift, _Acc) ->
    {error, invalid_varint}.

take(Size, Raw) when is_integer(Size), Size >= 0, byte_size(Raw) >= Size ->
    <<Value:Size/binary, Rest/binary>> = Raw,
    {ok, Value, Rest};
take(_, _) ->
    {error, truncated_binary}.

valid_hash(Hash) when byte_size(Hash) == 48 ->
    ok;
valid_hash(Hash) ->
    {error, {invalid_sd_hash_size, byte_size(Hash)}}.

stream_sd_hash_from_task0_claim_test() ->
    {ok, Tx} = hb_lbry_tx:parse_hex(hb_lbry_tx:task0_tx_hex()),
    [ClaimOutput | _] = maps:get(<<"outputs">>, Tx),
    Envelope = maps:get(<<"claim-envelope">>, ClaimOutput),
    ?assertEqual(
        {ok, <<"3da16b833f169c21caeb62ca66111227413f30f63c9d2f52f2a787643e086c334ee6949e05875cfe94a816aba02e492e">>},
        stream_sd_hash(maps:get(<<"message">>, Envelope))
    ).

channel_public_key_from_channel_claim_test() ->
    PublicKey = <<2, 1:256>>,
    Channel = field(1, PublicKey),
    Claim = field(2, Channel),
    ?assertEqual({ok, PublicKey}, channel_public_key(Claim)).

channel_public_key_requires_channel_field_test() ->
    Claim = field(1, field(1, <<"stream">>)),
    ?assertEqual({error, {missing_field, 2}}, channel_public_key(Claim)).

stream_sd_hash_rejects_wrong_hash_size_test() ->
    BadHash = <<1, 2, 3>>,
    Source = field(6, BadHash),
    Stream = field(1, Source),
    Claim = field(1, Stream),
    ?assertEqual({error, {invalid_sd_hash_size, 3}}, stream_sd_hash(Claim)).

field(Number, Value) ->
    Key = (Number bsl 3) bor 2,
    <<(varint(Key))/binary, (varint(byte_size(Value)))/binary, Value/binary>>.

varint(Value) when Value < 16#80 ->
    <<Value>>;
varint(Value) ->
    <<((Value band 16#7f) bor 16#80), (varint(Value bsr 7))/binary>>.
