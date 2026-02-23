%%% @doc Succinct encoding and decoding for Arweave data offset indexing.
%%% Arweave data items are extremely numerous (>25,000,000,000 as of Feb 2026), and
%%% as such small optimizations to the encoding of their offsets have a significant
%%% effect. For exampple, a single byte sized in the encoding at time of writing
%%% saves ~25 GB of storage.
%%% 
%%% The encoding is as follows:
%%%     << Version:4, Codec:4, StartOffset:64, Length/binary >>
%%% where:
%%%     - Version: 4-bit unsigned integer. Max: 15. Current: version `1`.
%%%     - Codec: 4-bit unsigned integer. Max: 15.
%%%     - StartOffset: 64-bit uint. Max: 2^64-1.
%%%     - Length: unsigned variable-length integer.
%%% 
%%% Codecs:
%%%     - 0: `tx@1.0`: An Arweave transaction.
%%%     - 1: [Reserved for ANS-102: The initial JSON data item format.]
%%%     - 2: `~ans104@1.0`: Binary data items.
%%%     - 3: [Reserved for `~httpsig@1.0`: RFC-9421 compatible HTTP signed messages.]
%%% 
%%% Codec indexes should, in general, be sorted by the time of their first write
%%% to Arweave: Arweave TXs as 0, ANS-102 as 1, ANS-104 as 2, etc.
%%% 
%%% All `length` values are read by decoding all of the remaining bytes in the 
%%% offset encoding as an unsigned big-endian integer. This allows the length
%%% to contract to only the number of bytes actually necessary to represent it.
%%% 
-module(hb_store_arweave_offset).
-export([encode/3, decode/1, path/1]).
-include("include/hb.hrl").

%% @doc Determine if a value is within a given unsigned bit range.
-define(IN_BIT_RANGE(X, Bits), (X >= 0 andalso X < (1 bsl Bits))).

-define(OFFSET_SZ, (8*8)). % 64-bit uint. Max: 2^64-1.
-define(FORMAT_VERSION, 1). % 4-bit uint. Max: 15.

%% @doc Reserved for future use. At the present time, store containing offsets are
%% expected to be utilized only as sub-stores to a `hb_store_arweave' store. As
%% as consequence, the path is simply the ID of the data item, with the prefix
%% of `~arweave@2.9/offset/` implied.
path(ID) when ?IS_ID(ID) -> hb_util:native_id(ID);
path(ID) -> throw({cannot_encode_path, ID}).

%% @doc Encode the offset of the data if it is valid. Throws `cannot_encode_offset'
%% if invalid.
encode(Type, StartOffset, Length)
        when
        (Type == true orelse Type == false orelse is_binary(Type))
        andalso ?IN_BIT_RANGE(StartOffset, ?OFFSET_SZ*8)
        andalso is_integer(Length) andalso Length >= 0
    ->
    <<
        (encode_format(Type))/binary,
        StartOffset:?OFFSET_SZ,
        (binary:encode_unsigned(Length))/binary
    >>;
encode(IsTX, StartOffset, Length) ->
    throw({cannot_encode_offset, {IsTX, StartOffset, Length}}).

decode(<<Format:1/binary, StartOffset:?OFFSET_SZ, Length/binary>>) ->
    {Version, CodecName} = decode_format(Format),
    {Version, CodecName, StartOffset, binary:decode_unsigned(Length)};
decode(Binary) ->
    throw({cannot_decode_offset, Binary}).

%% @doc Encode the type of the data.
encode_type(<<"tx@1.0">>) -> 0;
encode_type(<<"~ans102@1.0">>) -> 1;
encode_type(<<"ans104@1.0">>) -> 2;
encode_type(<<"~httpsig@1.0">>) -> 3;
encode_type(Type) -> throw({cannot_encode_type, Type}).

%% @doc Decode the type of the data to a binary codec name.
decode_type(0) -> <<"tx@1.0">>;
decode_type(1) -> <<"~ans102@1.0">>;
decode_type(2) -> <<"ans104@1.0">>;
decode_type(3) -> <<"~httpsig@1.0">>;
decode_type(Type) -> throw({cannot_decode_type, Type}).

%% @doc Encode the format of the offset. See the module documentation for the
%% present index of supported codecs.
encode_format(CodecName) ->
    << ?FORMAT_VERSION:4, (encode_type(CodecName)):4 >>;
encode_format(CodecName) ->
    throw({cannot_encode_format, CodecName}).

%% @doc Decode the format of the offset.
decode_format(<<FormatVersion:4, CodecName:4>>) ->
    {FormatVersion, decode_type(CodecName)};
decode_format(Binary) ->
    throw({cannot_decode_format, Binary}).