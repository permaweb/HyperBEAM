-module(ar_bundles).
-export([generate_id/2, type/1, map/1, hd/1, member/2, find/2]).
-export([manifest/1, manifest_item/1, parse_manifest/1]).
-export([new_item/4, sign_item/2, verify_item/1]).
-export([encode_tags/1, decode_tags/1]).
-export([serialize/1, serialize/2, deserialize/1, deserialize/2]).
-export([data_item_signature_data/1]).
-export([serialize_bundle_data/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @doc Module for creating, signing, and verifying Arweave data items and bundles.
-define(BUNDLE_TAGS, [
    {<<"bundle-format">>, <<"binary">>},
    {<<"bundle-version">>, <<"2.0.0">>}
]).

-define(LIST_TAGS, [
    {<<"map-format">>, <<"list">>}
]).

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Generate the ID for a given transaction.
generate_id(Item, signed) ->
    crypto:hash(sha256, Item#tx.signature);
generate_id(Item, unsigned) ->
    crypto:hash(
        sha256,
        data_item_signature_data(Item, unsigned)
    ).

%% @doc Return the first item in a bundle-map/list.
hd(#tx { data = #{ <<"1">> := Msg } }) -> Msg;
hd(#tx { data = [First | _] }) -> First;
hd(TX = #tx { data = Binary }) when is_binary(Binary) ->
    ?MODULE:hd((deserialize(serialize(TX), binary))#tx.data);
hd(#{ <<"1">> := Msg }) -> Msg;
hd(_) -> undefined.

%% @doc Convert an item containing a map or list into an Erlang map.
map(#tx { data = Map }) when is_map(Map) -> Map;
map(#tx { data = Data }) when is_list(Data) ->
    maps:from_list(
        lists:zipwith(
            fun({Index, Item}) -> {integer_to_binary(Index), map(Item)} end,
            lists:seq(1, length(Data)),
            Data
        )
    );
map(Item = #tx { data = Data }) when is_binary(Data) ->
    (maybe_unbundle(Item))#tx.data.

%% @doc Check if an item exists in a bundle-map/list.
member(Key, Item) ->
    find(Key, Item) =/= not_found.

%% @doc Find an item in a bundle-map/list and return it.
find(Key, Map) when is_map(Map) ->
    case maps:get(Key, Map, not_found) of
        not_found -> find(Key, maps:values(Map));
        Item -> Item
    end;
find(_Key, []) -> not_found;
find(Key, [Item|Rest]) ->
    case find(Key, Item) of
        not_found -> find(Key, Rest);
        CorrectItem -> CorrectItem
    end;
find(Key, Item = #tx { id = Key }) -> Item;
find(Key, Item = #tx { data = Data }) ->
    case hb_tx:id(Item, unsigned) of
        Key -> Item;
        _ ->
            case is_binary(Data) of
                false -> find(Key, Data);
                true -> not_found
            end
    end;
find(_Key, _) ->
    not_found.

%% @doc Return the manifest item in a bundle-map/list.
manifest_item(#tx { manifest = Manifest }) when is_record(Manifest, tx) ->
    Manifest;
manifest_item(_Item) -> undefined.

%% @doc Create a new data item. Should only be used for testing.
new_item(Target, Anchor, Tags, Data) ->
    hb_tx:reset_ids(
        #tx{
            format = ans104,
            target = Target,
            last_tx = Anchor,
            tags = Tags,
            data = Data,
            data_size = byte_size(Data)
        }
    ).

%% @doc Sign a data item.
sign_item(_, undefined) -> throw(wallet_not_found);
sign_item(RawItem, {PrivKey, {KeyType, Owner}}) ->
    Item = (hb_tx:normalize_data(RawItem))#tx{format = ans104, owner = Owner, signature_type = KeyType},
    % Generate the signature from the data item's data segment in 'signed'-ready mode.
    Sig = ar_wallet:sign(PrivKey, data_item_signature_data(Item, signed)),
    hb_tx:reset_ids(Item#tx{signature = Sig}).

%% @doc Verify the validity of a data item.
verify_item(DataItem) ->
    ValidID = verify_data_item_id(DataItem),
    ValidSignature = verify_data_item_signature(DataItem),
    ValidTags = verify_data_item_tags(DataItem),
    ValidID andalso ValidSignature andalso ValidTags.

type(Item) when is_record(Item, tx) ->
    lists:keyfind(<<"bundle-map">>, 1, Item#tx.tags),
    case lists:keyfind(<<"bundle-map">>, 1, Item#tx.tags) of
        {<<"bundle-map">>, _} ->
            case lists:keyfind(<<"map-format">>, 1, Item#tx.tags) of
                {<<"map-format">>, <<"list">>} -> list;
                _ -> map
            end;
        _ ->
            binary
    end;
type(Data) when erlang:is_map(Data) ->
    map;
type(Data) when erlang:is_list(Data) ->
    list;
type(_) ->
    binary.

%%%===================================================================
%%% Private functions.
%%%===================================================================

%% @doc Generate the data segment to be signed for a data item.
data_item_signature_data(RawItem) ->
    data_item_signature_data(RawItem, signed).
data_item_signature_data(RawItem, unsigned) ->
    data_item_signature_data(RawItem#tx { owner = ?DEFAULT_OWNER }, signed);
data_item_signature_data(RawItem, signed) ->
    true = enforce_valid_tx(RawItem),
    NormItem = hb_tx:normalize_data(RawItem),
    ar_deep_hash:hash([
        utf8_encoded("dataitem"),
        utf8_encoded("1"),
        %% Only SignatureType 1 is supported for now (RSA 4096)
        utf8_encoded("1"),
        <<(NormItem#tx.owner)/binary>>,
        <<(NormItem#tx.target)/binary>>,
        <<(NormItem#tx.last_tx)/binary>>,
        encode_tags(NormItem#tx.tags),
        <<(NormItem#tx.data)/binary>>
    ]).

%% @doc Verify the data item's ID matches the signature.
verify_data_item_id(DataItem) ->
    ExpectedID = generate_id(DataItem, signed),
    DataItem#tx.id == ExpectedID.

%% @doc Verify the data item's signature.
verify_data_item_signature(DataItem) ->
    SignatureData = data_item_signature_data(DataItem),
    %?event({unsigned_id, hb_util:encode(id(DataItem, unsigned)), hb_util:encode(SignatureData)}),
    ar_wallet:verify(
        {DataItem#tx.signature_type, DataItem#tx.owner}, SignatureData, DataItem#tx.signature
    ).

%% @doc Verify the validity of the data item's tags.
verify_data_item_tags(DataItem) ->
    ValidCount = length(DataItem#tx.tags) =< 128,
    ValidTags = lists:all(
        fun({Name, Value}) ->
            byte_size(Name) =< 1024 andalso byte_size(Value) =< 3072
        end,
        DataItem#tx.tags
    ),
    ValidCount andalso ValidTags.

manifest(Map) when is_map(Map) -> Map;
manifest(#tx { manifest = undefined }) -> undefined;
manifest(#tx { manifest = ManifestTX }) ->
    hb_json:decode(ManifestTX#tx.data).

parse_manifest(Item) when is_record(Item, tx) ->
    parse_manifest(Item#tx.data);
parse_manifest(Bin) ->
    hb_json:decode(Bin).

%% @doc Convert a #tx record to its binary representation.
serialize(not_found) -> throw(not_found);
serialize(TX) -> serialize(TX, binary).
serialize(TX, binary) when is_binary(TX) -> TX;
serialize(RawTX, binary) ->
    true = enforce_valid_tx(RawTX),
    TX = hb_tx:normalize(RawTX),
    EncodedTags = encode_tags(TX#tx.tags),
    <<
        (encode_signature_type(TX#tx.signature_type))/binary,
        (TX#tx.signature)/binary,
        (TX#tx.owner)/binary,
        (encode_optional_field(TX#tx.target))/binary,
        (encode_optional_field(TX#tx.last_tx))/binary,
        (encode_tags_size(TX#tx.tags, EncodedTags))/binary,
        EncodedTags/binary,
        (TX#tx.data)/binary
    >>;
serialize(TX, json) ->
    true = enforce_valid_tx(TX),
    hb_json:encode(hb_message:convert(TX, <<"ans104@1.0">>, #{})).

%% @doc Take an item and ensure that it is of valid form. Useful for ensuring
%% that a message is viable for serialization/deserialization before execution.
%% This function should throw simple, easy to follow errors to aid devs in
%% debugging issues.
enforce_valid_tx(List) when is_list(List) ->
    lists:all(fun enforce_valid_tx/1, List);
enforce_valid_tx(Map) when is_map(Map) ->
    lists:all(fun(Item) -> enforce_valid_tx(Item) end, maps:values(Map));
enforce_valid_tx(TX) ->
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX, message),
        {invalid_tx, TX}
    ),
	hb_util:ok_or_throw(TX,
		hb_util:check_value(TX#tx.format, [ans104]),
		{invalid_field, format, TX#tx.format}
	),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.id, [0, 32]),
        {invalid_field, id, TX#tx.id}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.unsigned_id, [0, 32]),
        {invalid_field, unsigned_id, TX#tx.unsigned_id}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.last_tx, [0, 32]),
        {invalid_field, last_tx, TX#tx.last_tx}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.owner, [0, byte_size(?DEFAULT_OWNER)]),
        {invalid_field, owner, TX#tx.owner}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.target, [0, 32]),
        {invalid_field, target, TX#tx.target}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.signature, [0, 65, byte_size(?DEFAULT_SIG)]),
        {invalid_field, signature, TX#tx.signature}
    ),
    lists:foreach(
        fun({Name, Value}) ->
            hb_util:ok_or_throw(TX,
                hb_util:check_type(Name, binary),
                {invalid_field, tag_name, Name}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_size(Name, {range, 0, ?MAX_TAG_NAME_SIZE}),
                {invalid_field, tag_name, Name}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_type(Value, binary),
                {invalid_field, tag_value, Value}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_size(Value, {range, 0, ?MAX_TAG_VALUE_SIZE}),
                {invalid_field, tag_value, Value}
            );
            (InvalidTagForm) ->
                throw({invalid_field, tag, InvalidTagForm})
        end,
        TX#tx.tags
    ),
    hb_util:ok_or_throw(
        TX,
        hb_util:check_type(TX#tx.data, binary)
            orelse hb_util:check_type(TX#tx.data, map)
            orelse hb_util:check_type(TX#tx.data, list),
        {invalid_field, data, TX#tx.data}
    ),
    true.

add_bundle_tags(Tags) -> ?BUNDLE_TAGS ++ (Tags -- ?BUNDLE_TAGS).

add_list_tags(Tags) ->
    (?BUNDLE_TAGS ++ (Tags -- ?BUNDLE_TAGS)) ++ ?LIST_TAGS.

add_manifest_tags(Tags, ManifestID) ->
    lists:filter(
        fun
            ({<<"bundle-map">>, _}) -> false;
            (_) -> true
        end,
        Tags
    ) ++ [{<<"bundle-map">>, hb_util:encode(ManifestID)}].


finalize_bundle_data(Processed) ->
    Length = <<(length(Processed)):256/integer>>,
    Index = <<<<(byte_size(Data)):256/integer, ID/binary>> || {ID, Data} <- Processed>>,
    Items = <<<<Data/binary>> || {_, Data} <- Processed>>,
    <<Length/binary, Index/binary, Items/binary>>.

to_serialized_pair(Item) ->
    % TODO: This is a hack to get the ID of the item. We need to do this because we may not
    % have the ID in 'item' if it is just a map/list. We need to make this more efficient.
    Serialized = ar_bundles:serialize(hb_tx:reset_ids(hb_tx:normalize(Item)), binary),
    Deserialized = ar_bundles:deserialize(Serialized, binary),
    UnsignedID = hb_tx:id(Deserialized, unsigned),
    {UnsignedID, Serialized}.

serialize_bundle_data(Map, Item) when is_map(Map) ->
    % TODO: Make this compatible with the normal manifest spec.
    % For now we just serialize the map to a JSON string of Key=>TXID
    BinItems = maps:map(fun(_, BinItem) -> to_serialized_pair(BinItem) end, Map),
    Index = maps:map(fun(_, {TXID, _}) -> hb_util:encode(TXID) end, BinItems),
    NewManifest = new_manifest(Index),
    Bin = finalize_bundle_data([to_serialized_pair(NewManifest) | maps:values(BinItems)]),
    Item#tx{
        data = Bin,
        manifest = NewManifest,
        tags = add_manifest_tags(
            add_bundle_tags(Item#tx.tags),
            hb_tx:id(NewManifest, unsigned)
        )
    };
% serialize_bundle_data(List, Item) when is_list(List) ->
%     % TODO: As of May 2025, this code can not be reached.
%     Bin = finalize_bundle_data(lists:map(fun to_serialized_pair/1, List)),
%     Item#tx{
%         data = Bin,
%         tags = add_bundle_tags(Item#tx.tags)
%     };
serialize_bundle_data(Data, _Item) ->
    throw({cannot_serialize_tx_data, must_be_map_or_list, Data}).

new_manifest(Index) ->
    TX = hb_tx:normalize(#tx{
        format = ans104,
        tags = [
            {<<"data-protocol">>, <<"bundle-map">>},
            {<<"variant">>, <<"0.0.1">>}
        ],
        data = hb_json:encode(Index)
    }),
    TX.

%% @doc Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 -- but it is is written in
%% little-endian format which is why we encode to `<<1, 0>>'.
encode_signature_type({rsa, 65537}) ->
    <<1, 0>>;
encode_signature_type(_) ->
    unsupported_tx_format.

%% @doc Encode an optional field (target, anchor) with a presence byte.
encode_optional_field(<<>>) ->
    <<0>>;
encode_optional_field(Field) ->
    <<1:8/integer, Field/binary>>.

%% @doc Encode a UTF-8 string to binary.
utf8_encoded(String) ->
    unicode:characters_to_binary(String, utf8).

encode_tags_size([], <<>>) ->
    <<0:64/little-integer, 0:64/little-integer>>;
encode_tags_size(Tags, EncodedTags) ->
    <<(length(Tags)):64/little-integer, (byte_size(EncodedTags)):64/little-integer>>.

%% @doc Encode tags into a binary format using Apache Avro.
encode_tags([]) ->
    <<>>;
encode_tags(Tags) ->
    EncodedBlocks = lists:flatmap(
        fun({Name, Value}) ->
            Res = [encode_avro_string(Name), encode_avro_string(Value)],
            case lists:member(error, Res) of
                true ->
                    throw({cannot_encode_empty_string, Name, Value});
                false ->
                    Res
            end
        end,
        Tags
    ),
    TagCount = length(Tags),
    ZigZagCount = encode_zigzag(TagCount),
    <<ZigZagCount/binary, (list_to_binary(EncodedBlocks))/binary, 0>>.

%% @doc Encode a string for Avro using ZigZag and VInt encoding.
encode_avro_string(<<>>) ->
    % Zero length strings are treated as a special case, due to the Avro encoder.
    << 0 >>;
encode_avro_string(String) ->
    StringBytes = unicode:characters_to_binary(String, utf8),
    Length = byte_size(StringBytes),
    <<(encode_zigzag(Length))/binary, StringBytes/binary>>.

%% @doc Encode an integer using ZigZag encoding.
encode_zigzag(Int) when Int >= 0 ->
    encode_vint(Int bsl 1);
encode_zigzag(Int) ->
    encode_vint(Int bsl 1, -1).

%% @doc Encode a ZigZag integer to VInt binary format.
encode_vint(ZigZag) ->
    encode_vint(ZigZag, []).

encode_vint(0, Acc) ->
    list_to_binary(lists:reverse(Acc));
encode_vint(ZigZag, Acc) ->
    VIntByte = ZigZag band 16#7F,
    ZigZagShifted = ZigZag bsr 7,
    case ZigZagShifted of
        0 -> encode_vint(0, [VIntByte | Acc]);
        _ -> encode_vint(ZigZagShifted, [VIntByte bor 16#80 | Acc])
    end.

%% @doc Convert binary data back to a #tx record.
deserialize(not_found) -> throw(not_found);
deserialize(Binary) -> deserialize(Binary, binary).
deserialize(Item, binary) when is_record(Item, tx) ->
    maybe_unbundle(Item);
deserialize(Binary, binary) ->
    %try
    {SignatureType, Signature, Owner, Rest} = decode_signature(Binary),
    {Target, Rest2} = decode_optional_field(Rest),
    {Anchor, Rest3} = decode_optional_field(Rest2),
    {Tags, Data} = decode_tags(Rest3),
    maybe_unbundle(
        hb_tx:reset_ids(#tx{
            format = ans104,
            signature_type = SignatureType,
            signature = Signature,
            owner = Owner,
            target = Target,
            last_tx = Anchor,
            tags = Tags,
            data = Data,
            data_size = byte_size(Data)
        })
    );
%catch
%    _:_:_Stack ->
%        {error, invalid_item}
%end;
deserialize(Bin, json) ->
    try
        Map = hb_json:decode(Bin),
        hb_message:convert(Map, <<"ans104@1.0">>, #{})
    catch
        _:_:_Stack ->
            {error, invalid_item}
    end.

maybe_unbundle(Item) ->
    Format = lists:keyfind(<<"bundle-format">>, 1, Item#tx.tags),
    Version = lists:keyfind(<<"bundle-version">>, 1, Item#tx.tags),
    case {Format, Version} of
        {{<<"bundle-format">>, <<"binary">>}, {<<"bundle-version">>, <<"2.0.0">>}} ->
            maybe_map_to_list(maybe_unbundle_map(Item));
        _ ->
            Item
    end.

maybe_map_to_list(Item) ->
    case lists:keyfind(<<"map-format">>, 1, Item#tx.tags) of
        {<<"map-format">>, <<"List">>} ->
            unbundle_list(Item);
        _ ->
            Item
    end.

unbundle_list(Item) ->
    Item#tx{
        data =
            lists:map(
                fun(Index) ->
                    maps:get(list_to_binary(integer_to_list(Index)), Item#tx.data)
                end,
                lists:seq(1, maps:size(Item#tx.data))
            )
    }.

maybe_unbundle_map(Bundle) ->
    case lists:keyfind(<<"bundle-map">>, 1, Bundle#tx.tags) of
        {<<"bundle-map">>, MapTXID} ->
            case unbundle(Bundle) of
                detached -> Bundle#tx { data = detached };
                Items ->
                    MapItem = find_single_layer(hb_util:decode(MapTXID), Items),
                    Map = hb_json:decode(MapItem#tx.data),
                    Bundle#tx{
                        manifest = MapItem,
                        data =
                            maps:map(
                                fun(_K, TXID) ->
                                    find_single_layer(hb_util:decode(TXID), Items)
                                end,
                                Map
                            )
                    }
            end;
        _ ->
            unbundle(Bundle)
    end.

%% @doc An internal helper for finding an item in a single-layer of a bundle.
%% Does not recurse! You probably want `find/2' in most cases.
find_single_layer(UnsignedID, TX) when is_record(TX, tx) ->
    find_single_layer(UnsignedID, TX#tx.data);
find_single_layer(UnsignedID, Items) ->
    TX = lists:keyfind(UnsignedID, #tx.unsigned_id, Items),
    case is_record(TX, tx) of
        true -> TX;
        false ->
            throw({cannot_find_item, hb_util:encode(UnsignedID)})
    end.

unbundle(Item = #tx{data = <<Count:256/integer, Content/binary>>}) ->
    {ItemsBin, Items} = decode_bundle_header(Count, Content),
    Item#tx{data = decode_bundle_items(Items, ItemsBin)};
unbundle(#tx{data = <<>>}) -> detached.

decode_bundle_items([], <<>>) ->
    [];
decode_bundle_items([{_ID, Size} | RestItems], ItemsBin) ->
    [
            deserialize(binary:part(ItemsBin, 0, Size))
        |
            decode_bundle_items(
                RestItems,
                binary:part(
                    ItemsBin,
                    Size,
                    byte_size(ItemsBin) - Size
                )
            )
    ].

decode_bundle_header(Count, Bin) -> decode_bundle_header(Count, Bin, []).
decode_bundle_header(0, ItemsBin, Header) ->
    {ItemsBin, lists:reverse(Header)};
decode_bundle_header(Count, <<Size:256/integer, ID:32/binary, Rest/binary>>, Header) ->
    decode_bundle_header(Count - 1, Rest, [{ID, Size} | Header]).

%% @doc Decode the signature from a binary format. Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 - but it is is written in
%% little-endian format which is why we match on `<<1, 0>>'.
decode_signature(<<1, 0, Signature:512/binary, Owner:512/binary, Rest/binary>>) ->
    {{rsa, 65537}, Signature, Owner, Rest};
decode_signature(Other) ->
    ?event({error_decoding_signature, Other}),
    unsupported_tx_format.

%% @doc Decode tags from a binary format using Apache Avro.
decode_tags(<<0:64/little-integer, 0:64/little-integer, Rest/binary>>) ->
    {[], Rest};
decode_tags(<<_TagCount:64/little-integer, _TagSize:64/little-integer, Binary/binary>>) ->
    {Count, BlocksBinary} = decode_zigzag(Binary),
    {Tags, Rest} = decode_avro_tags(BlocksBinary, Count),
    %% Pull out the terminating zero
    {0, Rest2} = decode_zigzag(Rest),
    {Tags, Rest2}.

decode_optional_field(<<0, Rest/binary>>) ->
    {<<>>, Rest};
decode_optional_field(<<1:8/integer, Field:32/binary, Rest/binary>>) ->
    {Field, Rest}.

%% @doc Decode Avro blocks (for tags) from binary.
decode_avro_tags(<<>>, _) ->
    {[], <<>>};
decode_avro_tags(Binary, Count) when Count =:= 0 ->
    {[], Binary};
decode_avro_tags(Binary, Count) ->
    {NameSize, Rest} = decode_zigzag(Binary),
    decode_avro_name(NameSize, Rest, Count).

decode_avro_name(0, Rest, _) ->
    {[], Rest};
decode_avro_name(NameSize, Rest, Count) ->
    <<Name:NameSize/binary, Rest2/binary>> = Rest,
    {ValueSize, Rest3} = decode_zigzag(Rest2),
    decode_avro_value(ValueSize, Name, Rest3, Count).

decode_avro_value(0, Name, Rest, Count) ->
    {DecodedTags, NonAvroRest} = decode_avro_tags(Rest, Count - 1),
    {[{Name, <<>>} | DecodedTags], NonAvroRest};
decode_avro_value(ValueSize, Name, Rest, Count) ->
    <<Value:ValueSize/binary, Rest2/binary>> = Rest,
    {DecodedTags, NonAvroRest} = decode_avro_tags(Rest2, Count - 1),
    {[{Name, Value} | DecodedTags], NonAvroRest}.

%% @doc Decode a VInt encoded ZigZag integer from binary.
decode_zigzag(Binary) ->
    {ZigZag, Rest} = decode_vint(Binary, 0, 0),
    case ZigZag band 1 of
        1 -> {-(ZigZag bsr 1) - 1, Rest};
        0 -> {ZigZag bsr 1, Rest}
    end.

decode_vint(<<>>, Result, _Shift) ->
    {Result, <<>>};
decode_vint(<<Byte, Rest/binary>>, Result, Shift) ->
    VIntPart = Byte band 16#7F,
    NewResult = Result bor (VIntPart bsl Shift),
    case Byte band 16#80 of
        0 -> {NewResult, Rest};
        _ -> decode_vint(Rest, NewResult, Shift + 7)
    end.

%%%===================================================================
%%% Unit tests.
%%% To run:
%%% erlc -o ebin src/*.erl; erl -pa ebin -eval "eunit:test(ar_bundles, [verbose])" -s init stop
%%%===================================================================

ar_bundles_test_() ->
    [
        {timeout, 30, fun test_no_tags/0},
        {timeout, 30, fun test_with_tags/0},
        {timeout, 30, fun test_with_zero_length_tag/0},
        {timeout, 30, fun test_unsigned_data_item_id/0},
        {timeout, 30, fun test_unsigned_data_item_normalization/0},
        {timeout, 30, fun test_empty_bundle/0},
        {timeout, 30, fun test_bundle_with_one_item/0},
        {timeout, 30, fun test_bundle_with_two_items/0},
        {timeout, 30, fun test_recursive_bundle/0},
        {timeout, 30, fun test_bundle_map/0},
        {timeout, 30, fun test_basic_member_id/0},
        {timeout, 30, fun test_deep_member/0},
        {timeout, 30, fun test_extremely_large_bundle/0},
        {timeout, 30, fun test_serialize_deserialize_deep_signed_bundle/0}
    ].

run_test() ->
    test_with_zero_length_tag().

test_no_tags() ->
    {Priv, Pub} = ar_wallet:new(),
    {KeyType, Owner} = Pub,
    Target = crypto:strong_rand_bytes(32),
    Anchor = crypto:strong_rand_bytes(32),
    DataItem = new_item(Target, Anchor, [], <<"data">>),
    SignedDataItem = sign_item(DataItem, {Priv, Pub}),

    ?assertEqual(true, verify_item(SignedDataItem)),
    assert_data_item(KeyType, Owner, Target, Anchor, [], <<"data">>, SignedDataItem),

    SignedDataItem2 = deserialize(serialize(SignedDataItem)),

    ?assertEqual(SignedDataItem, SignedDataItem2),
    ?assertEqual(true, verify_item(SignedDataItem2)),
    assert_data_item(KeyType, Owner, Target, Anchor, [], <<"data">>, SignedDataItem2).

test_with_tags() ->
    {Priv, Pub} = ar_wallet:new(),
    {KeyType, Owner} = Pub,
    Target = crypto:strong_rand_bytes(32),
    Anchor = crypto:strong_rand_bytes(32),
    Tags = [{<<"tag1">>, <<"value1">>}, {<<"tag2">>, <<"value2">>}],
    DataItem = new_item(Target, Anchor, Tags, <<"taggeddata">>),
    SignedDataItem = sign_item(DataItem, {Priv, Pub}),

    ?assertEqual(true, verify_item(SignedDataItem)),
    assert_data_item(KeyType, Owner, Target, Anchor, Tags, <<"taggeddata">>, SignedDataItem),

    SignedDataItem2 = deserialize(serialize(SignedDataItem)),

    ?assertEqual(SignedDataItem, SignedDataItem2),
    ?assertEqual(true, verify_item(SignedDataItem2)),
    assert_data_item(KeyType, Owner, Target, Anchor, Tags, <<"taggeddata">>, SignedDataItem2).

test_with_zero_length_tag() ->
    Item = hb_tx:normalize(#tx{
        format = ans104,
        tags = [
            {<<"normal-tag-1">>, <<"tag1">>},
            {<<"empty-tag">>, <<>>},
            {<<"normal-tag-2">>, <<"tag2">>}
        ],
        data = <<"Typical data field.">>
    }),
    Serialized = serialize(Item),
    Deserialized = deserialize(Serialized),
    ?assertEqual(Item, Deserialized).

test_unsigned_data_item_id() ->
    Item1 = deserialize(
        serialize(hb_tx:reset_ids(#tx{format = ans104, data = <<"data1">>}))
    ),
    Item2 = deserialize(
        serialize(hb_tx:reset_ids(#tx{format = ans104, data = <<"data2">>}))),
    ?assertNotEqual(Item1#tx.unsigned_id, Item2#tx.unsigned_id).

test_unsigned_data_item_normalization() ->
    NewItem = hb_tx:normalize(#tx{ format = ans104, data = <<"Unsigned data">> }),
    ReNormItem = deserialize(serialize(NewItem)),
    ?assertEqual(NewItem, ReNormItem).

assert_data_item(KeyType, Owner, Target, Anchor, Tags, Data, DataItem) ->
    ?assertEqual(KeyType, DataItem#tx.signature_type),
    ?assertEqual(Owner, DataItem#tx.owner),
    ?assertEqual(Target, DataItem#tx.target),
    ?assertEqual(Anchor, DataItem#tx.last_tx),
    ?assertEqual(Tags, DataItem#tx.tags),
    ?assertEqual(Data, DataItem#tx.data),
    ?assertEqual(byte_size(Data), DataItem#tx.data_size).

test_empty_bundle() ->
    Bundle = serialize([]),
    BundleItem = deserialize(Bundle),
    ?assertEqual(#{}, BundleItem#tx.data).

test_bundle_with_one_item() ->
    Item = new_item(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(32),
        [],
        ItemData = crypto:strong_rand_bytes(1000)
    ),
    ?event({item, Item}),
    Bundle = serialize([Item]),
    ?event({bundle, Bundle}),
    BundleItem = deserialize(Bundle),
    ?event({bundle_item, BundleItem}),
    ?assertEqual(ItemData, (maps:get(<<"1">>, BundleItem#tx.data))#tx.data).

test_bundle_with_two_items() ->
    Item1 = new_item(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(32),
        [],
        ItemData1 = crypto:strong_rand_bytes(32)
    ),
    Item2 = new_item(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(32),
        [{<<"tag1">>, <<"value1">>}, {<<"tag2">>, <<"value2">>}],
        ItemData2 = crypto:strong_rand_bytes(32)
    ),
    Bundle = serialize([Item1, Item2]),
    BundleItem = deserialize(Bundle),
    ?assertEqual(ItemData1, (maps:get(<<"1">>, BundleItem#tx.data))#tx.data),
    ?assertEqual(ItemData2, (maps:get(<<"2">>, BundleItem#tx.data))#tx.data).

test_recursive_bundle() ->
    W = ar_wallet:new(),
    Item1 = sign_item(#tx{
        id = crypto:strong_rand_bytes(32),
        last_tx = crypto:strong_rand_bytes(32),
        data = <<1:256/integer>>
    }, W),
    Item2 = sign_item(#tx{
        id = crypto:strong_rand_bytes(32),
        last_tx = crypto:strong_rand_bytes(32),
        data = [Item1]
    }, W),
    Item3 = sign_item(#tx{
        id = crypto:strong_rand_bytes(32),
        last_tx = crypto:strong_rand_bytes(32),
        data = [Item2]
    }, W),
    Bundle = serialize([Item3]),
    BundleItem = deserialize(Bundle),
    #{<<"1">> := UnbundledItem3} = BundleItem#tx.data,
    #{<<"1">> := UnbundledItem2} = UnbundledItem3#tx.data,
    #{<<"1">> := UnbundledItem1} = UnbundledItem2#tx.data,
    ?assert(verify_item(UnbundledItem1)),
    % TODO: Verify bundled lists...
    ?assertEqual(Item1#tx.data, UnbundledItem1#tx.data).

test_bundle_map() ->
    W = ar_wallet:new(),
    Item1 = sign_item(#tx{
        format = ans104,
        data = <<"item1_data">>
    }, W),
    Item2 = sign_item(#tx{
        format = ans104,
        last_tx = crypto:strong_rand_bytes(32),
        data = #{<<"key1">> => Item1}
    }, W),
    Bundle = serialize(Item2),
    BundleItem = deserialize(Bundle),
    ?assertEqual(Item1#tx.data, (maps:get(<<"key1">>, BundleItem#tx.data))#tx.data),
    ?assert(verify_item(BundleItem)).

test_extremely_large_bundle() ->
    W = ar_wallet:new(),
    Data = crypto:strong_rand_bytes(100_000_000),
    Norm = hb_tx:normalize(#tx { data = #{ <<"key">> => #tx { data = Data } } }),
    Signed = sign_item(Norm, W),
    Serialized = serialize(Signed),
    Deserialized = deserialize(Serialized),
    ?assert(verify_item(Deserialized)).

test_basic_member_id() ->
    W = ar_wallet:new(),
    Item = sign_item(
        #tx{
            data = <<"data">>
        },
        W
    ),
    ?assertEqual(true, member(Item#tx.id, Item)),
    ?assertEqual(true, member(hb_tx:id(Item, unsigned), Item)),
    ?assertEqual(false, member(crypto:strong_rand_bytes(32), Item)).

test_deep_member() ->
    W = ar_wallet:new(),
    Item = sign_item(
        #tx{
            data =
                #{<<"key1">> =>
                    sign_item(#tx{
                        data = <<"data">>
                    }, W)
                }
        },
        W
    ),
    Item2 = deserialize(serialize(sign_item(
        #tx{
            data = #{ <<"key2">> => Item }
        },
        W
    ))),
    ?assertEqual(true, member(<<"key1">>, Item2)),
    ?assertEqual(true, member(<<"key2">>, Item2)),
    ?assertEqual(true, member(Item#tx.id, Item2)),
    ?assertEqual(true, member(Item2#tx.id, Item2)),
    ?assertEqual(true, member(hb_tx:id(Item, unsigned), Item2)),
    ?assertEqual(true, member(hb_tx:id(Item2, unsigned), Item2)),
    ?assertEqual(false, member(crypto:strong_rand_bytes(32), Item2)).

test_serialize_deserialize_deep_signed_bundle() ->
    W = ar_wallet:new(),
    % Test that we can serialize, deserialize, and get the same IDs back.
    Item1 = sign_item(#tx{data = <<"item1_data">>}, W),
    Item2 = sign_item(#tx{data = #{<<"key1">> => Item1}}, W),
    Bundle = serialize(Item2),
    Deser2 = deserialize(Bundle),
    hb_tx:format(Deser2),
    #{ <<"key1">> := Deser1 } = Deser2#tx.data,
    hb_tx:format(Deser1),
    ?assertEqual(hb_tx:id(Item2, unsigned), hb_tx:id(Deser2, unsigned)),
    ?assertEqual(hb_tx:id(Item2, signed), hb_tx:id(Deser2, signed)),
    ?assertEqual(hb_tx:id(Item1, unsigned), hb_tx:id(Deser1, unsigned)),
    ?assertEqual(hb_tx:id(Item1, signed), hb_tx:id(Deser1, signed)),
    % Test that we can sign an item twice and the unsigned ID is the same.
    Item3 = sign_item(Item2, W),
    ?assertEqual(hb_tx:id(Item3, unsigned), hb_tx:id(Item2, unsigned)),
    ?assert(verify_item(Item3)).