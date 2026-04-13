%%% @doc Succinct encoding and decoding for Arweave data offset indexing.
%%% Arweave data items are extremely numerous (>25,000,000,000 as of Feb 2026), and
%%% as such small optimizations to the encoding of their offsets have a significant
%%% effect. For example, a single byte sized in the encoding at time of writing
%%% saves ~25 GB of storage.
%%%
%%% Version 1 of the encoding is as follows:
%%%     Encoded ::= MempoolTX | RelativeRef | ConfirmedMessage
%%%     MempoolTX ::= << Version:4, 0:4 >>
%%%     RelativeRef ::= << Version:4, Codec:4, RELATIVE:64, ParentID:256, Range >>
%%%     ConfirmedMessage ::= << Version:4, Codec:4, Range >>
%%%     Range ::= << Offset:64, Length:unsigned-variable-length-integer >>
%%% where:
%%%     - Version: 4-bit unsigned integer. Max: 15. Current: version `1`.
%%%     - Codec: 4-bit unsigned integer. Max: 15. Registry included below.
%%%     - Offset: 64-bit uint. Max: 2^64-1.
%%%     - RELATIVE: An atom, expressing that the offset is relative to the start
%%%       of another transaction, rather than the start of the Arweave global
%%%       address space. Always expressed as 2^64-1.
%%%     - ParentID: The ID of a parent message for a relative offset, 256-bit uint.
%%%     - Length: big-endian unsigned variable-length integer.
%%%     - MempoolTX: Always << 1:4, 0: 4>>, indicating the version and that the
%%%       key refers to an Arweave transaction that is not yet confirmed.
%%%     - RelativeRef: A reference to an offset inside an unconfirmed Arweave
%%%       transaction, yet to receive a global offset.
%%%     - ConfirmedMessage: A message (any codec) that has been confirmed and has
%%%       received a global offset.
%%%
%%% Codec Registry:
%%%     - 0: `tx@1.0`: An Arweave transaction.
%%%     - 1: `ans102@1.0`: The initial JSON data item format.
%%%     - 2: `~ans104@1.0`: Binary data items.
%%%     - 3: `~httpsig@1.0`: RFC-9421 compatible HTTP signed messages.
%%%
%%% Codec indexes should, in general, be sorted by the time of their first write
%%% to Arweave: Arweave TXs as 0, ANS-102 as 1, ANS-104 as 2, etc.
%%%
%%% All `length` values are read by decoding all of the remaining bytes in the
%%% offset encoding as an unsigned big-endian integer. This allows the length
%%% to contract to only the number of bytes actually necessary to represent it.
-module(hb_store_arweave_offset).
-export([encode/3, decode/1, path/1, mismatch_path/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(IN_BIT_RANGE(X, Bits), (is_integer(X) andalso X >= 0 andalso X < (1 bsl Bits))).

-define(OFFSET_SZ, (8*8)).
-define(OFFSET_MAX, ((1 bsl ?OFFSET_SZ) - 1)).
-define(FORMAT_VERSION, 1).
-define(MEMPOOL_TX, <<?FORMAT_VERSION:4, 0:4>>).

path(ID) when ?IS_ID(ID) -> hb_util:native_id(ID);
path(ID) -> throw({cannot_encode_path, ID}).

mismatch_path(ID) when ?IS_ID(ID) ->
    <<"mismatch/", (hb_util:native_id(ID))/binary>>;
mismatch_path(ID) -> throw({cannot_encode_mismatch_path, ID}).

%% @doc Encode an offset entry.
%% MempoolTX: a single byte when the key refers to an unconfirmed TX.
encode(<<"tx@1.0">>, relative, _Length) ->
    ?MEMPOOL_TX;
%% RelativeRef: sentinel offset + parent ID + range.
encode(Codec, #{ <<"relative">> := ParentID, <<"offset">> := RelOffset }, Length)
        when is_binary(Codec) andalso ?IS_ID(ParentID)
        andalso ?IN_BIT_RANGE(RelOffset, ?OFFSET_SZ)
        andalso is_integer(Length) andalso Length >= 0 ->
    <<
        (encode_format(Codec))/binary,
        ?OFFSET_MAX:?OFFSET_SZ,
        (hb_util:native_id(ParentID))/binary,
        RelOffset:?OFFSET_SZ,
        (binary:encode_unsigned(Length))/binary
    >>;
%% ConfirmedMessage: global offset + length.
encode(Codec, StartOffset, Length)
        when is_binary(Codec)
        andalso is_integer(StartOffset)
        andalso ?IN_BIT_RANGE(StartOffset, ?OFFSET_SZ)
        andalso is_integer(Length) andalso Length >= 0 ->
    <<
        (encode_format(Codec))/binary,
        StartOffset:?OFFSET_SZ,
        (binary:encode_unsigned(Length))/binary
    >>;
encode(Codec, Offset, Length) ->
    throw({cannot_encode_offset, {Codec, Offset, Length}}).

%% @doc Decode an offset entry.
decode(?MEMPOOL_TX) ->
    % MempoolTX: exactly one byte, version 1, codec tx@1.0.
    {<<"tx@1.0">>, relative, 0};
decode(<<Fmt:1/binary, ?OFFSET_MAX:?OFFSET_SZ,
         ParentID:32/binary, RelOffset:?OFFSET_SZ, Length/binary>>) ->
    % RelativeRef: `RELATIVE` atom in the offset field signals a parent-relative ref.
    {_, Codec} = decode_format(Fmt),
    {
        Codec,
        #{
            <<"relative">> => hb_util:encode(ParentID),
            <<"offset">> => RelOffset
        },
        binary:decode_unsigned(Length)
    };
decode(<<Fmt:1/binary, Offset:?OFFSET_SZ, Length/binary>>) ->
    % ConfirmedMessage: global offset.
    {_, Codec} = decode_format(Fmt),
    {Codec, Offset, binary:decode_unsigned(Length)};
decode(Binary) ->
    throw({cannot_decode_offset, Binary}).

encode_codec(<<"tx@1.0">>) -> 0;
encode_codec(<<"ans102@1.0">>) -> 1;
encode_codec(<<"ans104@1.0">>) -> 2;
encode_codec(<<"httpsig@1.0">>) -> 3;
encode_codec(Codec) -> throw({cannot_encode_codec, Codec}).

decode_codec(0) -> <<"tx@1.0">>;
decode_codec(1) -> <<"ans102@1.0">>;
decode_codec(2) -> <<"ans104@1.0">>;
decode_codec(3) -> <<"httpsig@1.0">>;
decode_codec(Codec) -> throw({cannot_decode_codec, Codec}).

encode_format(CodecName) ->
    <<?FORMAT_VERSION:4, (encode_codec(CodecName)):4>>.

decode_format(<<_Version:4, CodecName:4>>) ->
    {?FORMAT_VERSION, decode_codec(CodecName)};
decode_format(Binary) ->
    throw({cannot_decode_format, Binary}).

%%% Tests

confirmed_round_trip_test() ->
    Encoded = encode(<<"tx@1.0">>, 12345, 678),
    ?assertEqual({<<"tx@1.0">>, 12345, 678}, decode(Encoded)).

mempool_tx_round_trip_test() ->
    Encoded = encode(<<"tx@1.0">>, relative, 0),
    ?assertEqual(1, byte_size(Encoded)),
    ?assertEqual({<<"tx@1.0">>, relative, 0}, decode(Encoded)).

relative_ref_round_trip_test() ->
    ParentID = hb_util:encode(crypto:strong_rand_bytes(32)),
    Encoded =
        encode(<<"ans104@1.0">>,
            #{ <<"relative">> => ParentID, <<"offset">> => 321 },
            654
        ),
    ?assertEqual(
        {
            <<"ans104@1.0">>,
            #{ <<"relative">> => ParentID, <<"offset">> => 321 },
            654
        },
        decode(Encoded)
    ).

relative_ref_zero_offset_round_trip_test() ->
    ParentID = hb_util:encode(crypto:strong_rand_bytes(32)),
    Encoded =
        encode(
            <<"ans104@1.0">>,
            #{ <<"relative">> => ParentID, <<"offset">> => 0 },
            100
        ),
    ?assertMatch({<<"ans104@1.0">>, #{ <<"offset">> := 0 }, 100}, decode(Encoded)).