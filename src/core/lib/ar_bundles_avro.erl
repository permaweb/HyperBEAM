%%% @doc The Avro encoding and decoding of the tags of ANS-104 items. Avro
%%% encodes an array as blocks: each block starts with the count of its tags,
%%% or with the negative of that count and then the byte size of the block, and
%%% a count of 0 ends the array. `encode/1' gives one block with the count of
%%% the tags, then 0, or no bytes for no tags. An item may give its tags in
%%% another form, and its signature is over its own bytes.
%%%
%%% The Avro form of tag bytes gives the counts of their blocks, then the 0
%%% that ends the array, each as an Avro long. It is empty for the bytes that
%%% `encode/1' gives. `decode/1' gives the tags and the Avro form of their
%%% bytes, and `encode/2' gives the same bytes back from both.
-module(ar_bundles_avro).
-export([encode/1, encode/2, decode/1]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Encode tags as one block with the count of the tags, then 0. No tags
%% give no bytes.
encode([]) ->
    <<>>;
encode(Tags) ->
    encode(Tags, <<(encode_zigzag(length(Tags)))/binary, 0>>).

%% @doc Encode tags in an Avro form. An empty Avro form gives the bytes that
%% `encode/1' gives.
encode(Tags, <<>>) ->
    encode(Tags);
encode(Tags, Form) ->
    encode_blocks(Tags, Form).

%% @doc Encode tags in blocks with the counts that an Avro form gives, then
%% the 0 that ends the array. If the counts are not those of the tags, throw
%% `invalid_avro_form'.
encode_blocks([], <<0>>) ->
    <<0>>;
encode_blocks(Tags, Form) ->
    {Count, Rest} = decode_zigzag(Form),
    case Count =/= 0 andalso abs(Count) =< length(Tags) of
        true ->
            {BlockTags, RestTags} = lists:split(abs(Count), Tags),
            Block =
                <<
                    <<
                        (encode_avro_bytes(Name))/binary,
                        (encode_avro_bytes(Value))/binary
                    >>
                ||
                    {Name, Value} <- BlockTags
                >>,
            <<
                (encode_block_header(Count, Block))/binary,
                Block/binary,
                (encode_blocks(RestTags, Rest))/binary
            >>;
        false ->
            throw({invalid_avro_form, Form})
    end.

%% @doc Encode the count of a block, and the byte size of the block if the count
%% is negative.
encode_block_header(Count, _Block) when Count > 0 ->
    encode_zigzag(Count);
encode_block_header(Count, Block) ->
    <<(encode_zigzag(Count))/binary, (encode_zigzag(byte_size(Block)))/binary>>.

%% @doc Encode a tag name or value as Avro bytes: its length, then its bytes.
encode_avro_bytes(Bytes) ->
    <<(encode_zigzag(byte_size(Bytes)))/binary, Bytes/binary>>.

%% @doc Encode an integer using ZigZag encoding.
encode_zigzag(Int) when Int >= 0 ->
    encode_vint(Int bsl 1);
encode_zigzag(Int) ->
    encode_vint(-(Int bsl 1) - 1).

%% @doc Encode a ZigZag integer to VInt binary format: 7 bits per byte, the low
%% bits first, with the high bit set on every byte but the last.
encode_vint(ZigZag) when ZigZag < 16#80 ->
    <<ZigZag>>;
encode_vint(ZigZag) ->
    <<(ZigZag band 16#7F bor 16#80), (encode_vint(ZigZag bsr 7))/binary>>.

%% @doc Decode the Avro bytes of tags to the tags and the Avro form of the
%% bytes. If `encode/2' does not give the same bytes back from them, as for a
%% long in more bytes than Avro gives it, throw `invalid_avro_long'.
decode(<<>>) ->
    {[], <<>>};
decode(Bytes) ->
    {Tags, Counts} = decode_avro_tag_section(Bytes),
    Form = avro_form(Counts),
    case encode(Tags, Form) of
        Bytes -> {Tags, Form};
        _ -> throw({invalid_ans104_tags, invalid_avro_long})
    end.

%% @doc Return the Avro form of the counts of the blocks of tag bytes: empty
%% for one block with a positive count, else each count as an Avro long.
avro_form([Count, 0]) when Count > 0 ->
    <<>>;
avro_form(Counts) ->
    << <<(encode_zigzag(Count))/binary>> || Count <- Counts >>.

%% @doc Decode Avro blocks (for tags) from binary, with the count of each block
%% and the 0 that ends the array.
decode_avro_tag_section(TagsBinary) ->
    case decode_avro_tag_array(TagsBinary, [], []) of
        {Tags, Counts, <<>>} -> {Tags, Counts};
        {_Tags, _Counts, _Residue} ->
            throw({invalid_ans104_tags, residue_in_tag_section})
    end.

decode_avro_tag_array(<<>>, _Acc, _Counts) ->
    throw({invalid_ans104_tags, missing_avro_array_terminator});
decode_avro_tag_array(Binary, Acc, Counts) ->
    {Count, Rest} = decode_zigzag(Binary),
    decode_avro_tag_array_block(Count, Rest, Acc, [Count | Counts]).

decode_avro_tag_array_block(0, Rest, Acc, Counts) ->
    {lists:reverse(Acc), lists:reverse(Counts), Rest};
decode_avro_tag_array_block(Count, Rest, Acc, Counts) when Count > 0 ->
    {Tags, Rest2} = decode_avro_tags(Rest, Count),
    decode_avro_tag_array(Rest2, lists:reverse(Tags, Acc), Counts);
decode_avro_tag_array_block(Count, Rest, Acc, Counts) when Count < 0 ->
    {BlockSize, BlockRest} = decode_zigzag(Rest),
    case BlockSize >= 0 andalso byte_size(BlockRest) >= BlockSize of
        true ->
            <<Block:BlockSize/binary, Rest2/binary>> = BlockRest,
            Tags = decode_avro_tag_block(Block, -Count),
            decode_avro_tag_array(Rest2, lists:reverse(Tags, Acc), Counts);
        false ->
            throw({invalid_ans104_tags, invalid_avro_block_size})
    end.

decode_avro_tag_block(Block, Count) ->
    case decode_avro_tags(Block, Count) of
        {Tags, <<>>} -> Tags;
        {_Tags, _Residue} -> throw({invalid_ans104_tags, residue_in_avro_block})
    end.

decode_avro_tags(Binary, Count) when Count =:= 0 ->
    {[], Binary};
decode_avro_tags(<<>>, _Count) ->
    throw({invalid_ans104_tags, truncated_avro_tag_item});
decode_avro_tags(Binary, Count) ->
    {NameSize, Rest} = decode_zigzag(Binary),
    decode_avro_name(NameSize, Rest, Count).

decode_avro_name(NameSize, _Rest, _Count) when NameSize < 0 ->
    throw({invalid_ans104_tags, invalid_avro_name_size});
decode_avro_name(NameSize, Rest, _Count) when byte_size(Rest) < NameSize ->
    throw({invalid_ans104_tags, truncated_avro_name});
decode_avro_name(NameSize, Rest, Count) ->
    <<Name:NameSize/binary, Rest2/binary>> = Rest,
    {ValueSize, Rest3} = decode_zigzag(Rest2),
    decode_avro_value(ValueSize, Name, Rest3, Count).

decode_avro_value(0, Name, Rest, Count) ->
    {DecodedTags, NonAvroRest} = decode_avro_tags(Rest, Count - 1),
    {[{Name, <<>>} | DecodedTags], NonAvroRest};
decode_avro_value(ValueSize, _Name, _Rest, _Count) when ValueSize < 0 ->
    throw({invalid_ans104_tags, invalid_avro_value_size});
decode_avro_value(ValueSize, _Name, Rest, _Count) when byte_size(Rest) < ValueSize ->
    throw({invalid_ans104_tags, truncated_avro_value});
decode_avro_value(ValueSize, Name, Rest, Count) ->
    <<Value:ValueSize/binary, Rest2/binary>> = Rest,
    {DecodedTags, NonAvroRest} = decode_avro_tags(Rest2, Count - 1),
    {[{Name, Value} | DecodedTags], NonAvroRest}.

%% @doc Decode a VInt encoded ZigZag integer from binary. An Avro long has at
%% most ten bytes; a continuation run that shifts past bit 63 throws
%% `{invalid_ans104_tags, invalid_avro_long}'.
decode_zigzag(Binary) ->
    {ZigZag, Rest} = decode_vint(Binary, 0, 0),
    case ZigZag band 1 of
        1 -> {-(ZigZag bsr 1) - 1, Rest};
        0 -> {ZigZag bsr 1, Rest}
    end.

decode_vint(_Binary, _Result, Shift) when Shift > 63 ->
    throw({invalid_ans104_tags, invalid_avro_long});
decode_vint(<<>>, Result, _Shift) ->
    {Result, <<>>};
decode_vint(<<Byte, Rest/binary>>, Result, Shift) ->
    VIntPart = Byte band 16#7F,
    NewResult = Result bor (VIntPart bsl Shift),
    case Byte band 16#80 of
        0 -> {NewResult, Rest};
        _ -> decode_vint(Rest, NewResult, Shift + 7)
    end.

%%% Tests

%% @doc Each form of an array of two tags decodes to the tags and its Avro form,
%% which encode to the same bytes: one block, one block with a negative count
%% and its byte size, two blocks, and two blocks of which one is negative.
avro_form_test() ->
    Tags = [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}],
    A = <<2, "a", 2, "1">>,
    B = <<2, "b", 2, "2">>,
    lists:foreach(
        fun({Bytes, Form}) ->
            ?assertEqual({Tags, Form}, decode(Bytes)),
            ?assertEqual(Bytes, encode(Tags, Form))
        end,
        [
            {<<4, A/binary, B/binary, 0>>, <<>>},
            {<<3, 16, A/binary, B/binary, 0>>, <<3, 0>>},
            {<<2, A/binary, 2, B/binary, 0>>, <<2, 2, 0>>},
            {<<1, 8, A/binary, 2, B/binary, 0>>, <<1, 2, 0>>}
        ]
    ),
    % An empty array is the 0 that ends it, with the Avro form 0. With an
    % empty Avro form, no tags give no bytes.
    ?assertEqual({[], <<0>>}, decode(<<0>>)),
    ?assertEqual(<<0>>, encode([], <<0>>)),
    ?assertEqual(<<>>, encode([], <<>>)),
    % If the counts of a form are not those of the tags, it throws.
    ?assertThrow({invalid_avro_form, _}, encode(Tags, <<3, 2, 0>>)),
    % A count of 2 in two bytes throws, as `encode/2' gives it in one.
    ?assertThrow(
        {invalid_ans104_tags, invalid_avro_long},
        decode(<<16#84, 0, A/binary, B/binary, 0>>)
    ).
