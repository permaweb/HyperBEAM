-module(ar_bundles).
-export([signer/1]).
-export([id/1, id/2, hd/1, member/2, find/2]).
-export([new_item/4, sign_item/2, verify_item/1]).
-export([encode_tags/1, decode_tags/1]).
-export([serialize/1, deserialize/1, serialize_bundle/2]).
-export([data_item_signature_data/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% @doc Module for creating, signing, and verifying Arweave data items and bundles.

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Return the address of the signer of an item, if it is signed.
signer(#tx { owner = ?DEFAULT_OWNER }) -> undefined;
signer(Item) -> crypto:hash(sha256, Item#tx.owner).

%% @doc Return the ID of an item -- either signed or unsigned as specified.
%% If the item is unsigned and the user requests the signed ID, we return
%% the atom `not_signed'. In all other cases, we return the ID of the item.
id(Item) -> id(Item, unsigned).
id(Item, Type) when not is_record(Item, tx) ->
    id(dev_arweave_common:normalize(Item), Type);
id(Item = #tx { unsigned_id = ?DEFAULT_ID }, unsigned) ->
    CorrectedItem = dev_arweave_common:reset_ids(Item),
    CorrectedItem#tx.unsigned_id;
id(#tx { unsigned_id = UnsignedID }, unsigned) ->
    UnsignedID;
id(#tx { id = ?DEFAULT_ID }, signed) ->
    not_signed;
id(#tx { id = ID }, signed) ->
    ID.

%% @doc Return the first item in a bundle-map/list.
hd(#tx { data = #{ <<"1">> := Msg } }) -> Msg;
hd(#tx { data = [First | _] }) -> First;
hd(TX = #tx { data = Binary }) when is_binary(Binary) ->
    ?MODULE:hd((deserialize(serialize(TX)))#tx.data);
hd(#{ <<"1">> := Msg }) -> Msg;
hd(_) -> undefined.

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
    case id(Item, unsigned) of
        Key -> Item;
        _ ->
            case is_binary(Data) of
                false -> find(Key, Data);
                true -> not_found
            end
    end;
find(_Key, _) ->
    not_found.

%% @doc Create a new data item. Should only be used for testing.
new_item(Target, Anchor, Tags, Data) ->
    dev_arweave_common:reset_ids(
        #tx{
            format = ans104,
            target = Target,
            anchor = Anchor,
            tags = Tags,
            data = Data,
            data_size = byte_size(Data)
        }
    ).

%% @doc Sign a data item.
sign_item(_, undefined) -> throw(wallet_not_found);
sign_item(RawItem, {PrivKey, {KeyType, Owner}}) ->
    Item = (dev_arweave_common:normalize(RawItem))#tx{format = ans104, owner = Owner, signature_type = KeyType},
    % Generate the signature from the data item's data segment in 'signed'-ready mode.
    Sig = ar_wallet:sign(PrivKey, data_item_signature_data(Item)),
    dev_arweave_common:reset_ids(Item#tx{signature = Sig}).

%% @doc Verify the validity of a data item.
verify_item(DataItem) ->
    ValidID = verify_data_item_id(DataItem),
    ValidSignature = verify_data_item_signature(DataItem),
    ValidTags = verify_data_item_tags(DataItem),
    ValidID andalso ValidSignature andalso ValidTags.

%%%===================================================================
%%% Private functions.
%%%===================================================================

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
        hb_util:check_size(TX#tx.id, [0, 32]),
        {invalid_field, id, TX#tx.id}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.unsigned_id, [0, 32]),
        {invalid_field, unsigned_id, TX#tx.unsigned_id}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.anchor, [0, 32]),
        {invalid_field, anchor, TX#tx.anchor}
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
    hb_util:ok_or_throw(TX,
        hb_util:check_type(TX#tx.tags, list),
        {invalid_field, tags, TX#tx.tags}
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
                {invalid_field, tag_value, {Name, Value}}
            ),
            hb_util:ok_or_throw(TX,
                hb_util:check_size(Value, {range, 0, ?MAX_TAG_VALUE_SIZE}),
                {invalid_field, tag_value, {Name, Value}}
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


%% @doc Generate the data segment to be signed for a data item.
data_item_signature_data(RawItem) ->
    true = enforce_valid_tx(RawItem),
    Item = dev_arweave_common:serialize_data(RawItem),
    ar_deep_hash:hash([
        utf8_encoded("dataitem"),
        utf8_encoded("1"),
        %% Only SignatureType 1 is supported for now (RSA 4096)
        utf8_encoded("1"),
        <<(Item#tx.owner)/binary>>,
        <<(Item#tx.target)/binary>>,
        <<(Item#tx.anchor)/binary>>,
        encode_tags(Item#tx.tags),
        <<(Item#tx.data)/binary>>
    ]).

%% @doc Verify the data item's ID matches the signature.
verify_data_item_id(DataItem) ->
    ExpectedID = crypto:hash(sha256, DataItem#tx.signature),
    DataItem#tx.id == ExpectedID.

%% @doc Verify the data item's signature.
verify_data_item_signature(DataItem) ->
    SignatureData = data_item_signature_data(DataItem),
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

%% @doc Convert an ans104 #tx record to its binary representation.
serialize(not_found) -> throw(not_found);
serialize(TX) when is_binary(TX) -> TX;
serialize(RawTX) when is_record(RawTX, tx) ->
    true = enforce_valid_tx(RawTX),
    TX = dev_arweave_common:serialize_data(RawTX),
    EncodedTags = encode_tags(TX#tx.tags),
    <<
        (encode_signature_type(TX#tx.signature_type))/binary,
        (TX#tx.signature)/binary,
        (TX#tx.owner)/binary,
        (encode_optional_field(TX#tx.target))/binary,
        (encode_optional_field(TX#tx.anchor))/binary,
        (encode_tags_size(TX#tx.tags, EncodedTags))/binary,
        EncodedTags/binary,
        (TX#tx.data)/binary
    >>;
serialize(TX) ->
    throw({cannot_serialize_tx, must_be_binary_or_tx, TX}).

serialize_bundle(List, Normalize) when is_list(List) ->
    FinalizedData = finalize_bundle_data(
        lists:map(
            fun(Item) ->
                {UnsignedID, SignedID, Serialized} =
                    to_serialized_pair(Item, Normalize),
                {SignedID, Serialized}
            end,
            List)
    ),
    {undefined, FinalizedData};
serialize_bundle(Map, Normalize) when is_map(Map) ->
    % TODO: Make this compatible with the normal manifest spec.
    % For now we just serialize the map to a JSON string of Key=>TXID
    BinItems = maps:map(
        fun(_, Item) -> 
            {UnsignedID, SignedID, Serialized} =
                to_serialized_pair(Item, Normalize),
            {UnsignedID, Serialized}
        end,
        Map),
    Index = maps:map(fun(_, {TXID, _}) -> hb_util:encode(TXID) end, BinItems),
    NewManifest = new_manifest(Index),
    {NewManifestUnsignedID, NewManifestSignedID, NewManifestSerialized} =
        to_serialized_pair(NewManifest, Normalize),
    %?event({generated_manifest, NewManifest == Manifest, hb_util:encode(id(NewManifest, unsigned)), Index}),
    FinalizedData = finalize_bundle_data(
        [{NewManifestUnsignedID, NewManifestSerialized} | maps:values(BinItems)]),
    {NewManifest, FinalizedData};
serialize_bundle(Data, _Normalize) when is_binary(Data) ->
    {undefined, Data};
serialize_bundle(Data, _Normalize) ->
    throw({cannot_serialize_tx_data, must_be_list_or_map, Data}).

finalize_bundle_data(Processed) ->
    Length = <<(length(Processed)):256/little-integer>>,
    Index = <<<<(byte_size(Data)):256/little-integer, ID/binary>> || {ID, Data} <- Processed>>,
    Items = <<<<Data/binary>> || {_, Data} <- Processed>>,
    <<Length/binary, Index/binary, Items/binary>>.

new_manifest(Index) ->
    ?event({new_manifest, Index}),
    TX = dev_arweave_common:normalize(#tx{
        format = ans104,
        tags = [
            {<<"data-protocol">>, <<"bundle-map">>},
            {<<"variant">>, <<"0.0.1">>}
        ],
        data = hb_json:encode(Index)
    }),
    TX.

to_serialized_pair(Item, Normalize) when is_binary(Item) ->
    % Support bundling of bare binary payloads by wrapping them in a TX that
    % is explicitly marked as a binary data item.
    to_serialized_pair(
        #tx{ tags = [{<<"ao-type">>, <<"binary">>}], data = Item }, Normalize);
to_serialized_pair(Item, true) ->
    to_serialized_pair(dev_arweave_common:normalize(Item), false);
to_serialized_pair(Item, false) ->
    ?event({to_serialized_pair, Item}),
    % TODO: This is a hack to get the ID of the item. We need to do this because we may not
    % have the ID in 'item' if it is just a map/list. We need to make this more efficient.
    Serialized = serialize(Item),
    Deserialized = deserialize(Serialized),
    UnsignedID = id(Deserialized, unsigned),
    SignedID = id(Deserialized, signed),
    ?event({serialized_pair,
        {unsigned_id, UnsignedID}, {signed_id, SignedID}, {size, byte_size(Serialized)}}),
    {UnsignedID, SignedID, Serialized}.

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
    <<1:8/little-integer, Field/binary>>.

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
            Res = [encode_avro_name(Name), encode_avro_value(Value)],
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
encode_avro_name(<<>>) ->
    % Zero length names are treated as a special case, due to the Avro encoder.
    << 0 >>;
encode_avro_name(String) ->
    StringBytes = utf8_encoded(String),
    Length = byte_size(StringBytes),
    <<(encode_zigzag(Length))/binary, StringBytes/binary>>.

encode_avro_value(<<>>) ->
    % Zero length values are treated as a special case, due to the Avro encoder.
    << 0 >>;
encode_avro_value(Value) when is_binary(Value) ->
    % Tag values can be raw binaries
    Length = byte_size(Value),
    <<(encode_zigzag(Length))/binary, Value/binary>>.

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

%% @doc Convert binary data back to #tx record(s).
%% When deserializing a binary, it is assumed the binary is an ans104 *item*,
%% and *not* a bundle. It may be an item that contains a bundle, though.
%% When deserializing a #tx it is the #tx.data that is deserialized (after
%% consulting the #tx.tags to confirm that data format).
deserialize(not_found) -> throw(not_found);
deserialize(Item) when is_record(Item, tx) ->
    maybe_unbundle(Item);
deserialize(Binary) ->
    deserialize_item(Binary).

deserialize_item(Binary) ->
    {SignatureType, Signature, Owner, Rest} = decode_signature(Binary),
    {Target, Rest2} = decode_optional_field(Rest),
    {Anchor, Rest3} = decode_optional_field(Rest2),
    {Tags, Data} = decode_tags(Rest3),
    maybe_unbundle(
        dev_arweave_common:reset_ids(#tx{
            format = ans104,
            signature_type = SignatureType,
            signature = Signature,
            owner = Owner,
            target = Target,
            anchor = Anchor,
            tags = Tags,
            data = Data,
            data_size = byte_size(Data)
        })
    ).

maybe_unbundle(Item) ->
    case dev_arweave_common:type(Item) of
        list -> unbundle_list(Item);
        binary -> Item;
        map -> unbundle_map(Item)
    end.

unbundle_list(Item) ->
    case unbundle(Item#tx.data) of
        detached -> Item#tx{data = detached};
        Items -> Item#tx{data = hb_util:list_to_numbered_message(Items)}
    end.

unbundle_map(Item) ->
    MapTXID = dev_arweave_common:tagfind(<<"bundle-map">>, Item#tx.tags, <<>>),
    case unbundle(Item#tx.data) of
        detached -> Item#tx{data = detached};
        Items ->
            MapItem = find_single_layer(hb_util:decode(MapTXID), Items),
            Map = hb_json:decode(MapItem#tx.data),
            Item#tx{
                manifest = MapItem,
                data =
                    maps:map(
                        fun(_K, TXID) ->
                            find_single_layer(
                                hb_util:decode(TXID), Items)
                        end,
                        Map
                    )
            }
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

unbundle(<<Count:256/little-integer, Content/binary>>) ->
    {ItemsBin, Items} = decode_bundle_header(Count, Content),
    decode_bundle_items(Items, ItemsBin);
unbundle(<<>>) -> detached.

decode_bundle_items([], <<>>) ->
    [];
decode_bundle_items([{_ID, Size} | RestItems], ItemsBin) ->
    [
            deserialize_item(binary:part(ItemsBin, 0, Size))
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
decode_bundle_header(Count, <<Size:256/little-integer, ID:32/binary, Rest/binary>>, Header) ->
    decode_bundle_header(Count - 1, Rest, [{ID, Size} | Header]).

%% @doc Decode the signature from a binary format. Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 - but it is is written in
%% little-endian format which is why we match on `<<1, 0>>'.
decode_signature(<<1, 0, Signature:512/binary, Owner:512/binary, Rest/binary>>) ->
    {{rsa, 65537}, Signature, Owner, Rest};
decode_signature(Other) ->
    ?event({error_decoding_signature,
        {sig_type, {explicit, binary:part(Other, 0, 2)}},
        {binary, Other}}),
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
decode_optional_field(<<1:8/little-integer, Field:32/binary, Rest/binary>>) ->
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
%%%===================================================================

encode_tags_test() ->
    BinValue = <<1, 2, 3, 255, 254>>,
    TestCases = [
        {simple_string_tags, [{<<"tag1">>, <<"value1">>}]},
        {binary_value_tag, [{<<"binary-tag">>, BinValue}]},
        {mixed_tags,
            [
                {<<"string-tag">>, <<"string-value">>},
                {<<"binary-tag">>, BinValue}
            ]
        },
        {empty_value_tag, [{<<"empty-value-tag">>, <<>>}]},
        {unicode_tag, [{<<"unicode-tag">>, <<"你好世界">>}]}
    ],
    lists:foreach(
        fun({Label, InputTags}) ->
            Encoded = encode_tags(InputTags),
            Wrapped =
                <<
                    (length(InputTags)):64/little,
                    (byte_size(Encoded)):64/little,
                    Encoded/binary
                >>,
            {DecodedTags, <<>>} = decode_tags(Wrapped),
            ?assertEqual(InputTags, DecodedTags, Label)
        end,
        TestCases
    ),
    % Test case: Empty tags list
    EmptyTags = [],
    EncodedEmpty = encode_tags(EmptyTags),
    ?assertEqual(<<>>, EncodedEmpty),
    WrappedEmpty = <<0:64/little, 0:64/little>>,
    {[], <<>>} = decode_tags(WrappedEmpty).

no_tags_test() ->
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

with_tags_test() ->
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

with_zero_length_tag_test() ->
    Item = dev_arweave_common:normalize(#tx{
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

unsigned_data_item_id_test() ->
    Item1 = deserialize(
        serialize(
            dev_arweave_common:reset_ids(
                #tx{format = ans104, data = <<"data1">>}))
    ),
    Item2 = deserialize(
        serialize(
            dev_arweave_common:reset_ids(
                #tx{format = ans104, data = <<"data2">>}))),
    ?assertNotEqual(Item1#tx.unsigned_id, Item2#tx.unsigned_id).

unsigned_data_item_normalization_test() ->
    NewItem = dev_arweave_common:normalize(#tx{ format = ans104, data = <<"Unsigned data">> }),
    ReNormItem = deserialize(serialize(NewItem)),
    ?assertEqual(NewItem, ReNormItem).

assert_data_item(KeyType, Owner, Target, Anchor, Tags, Data, DataItem) ->
    ?assertEqual(KeyType, DataItem#tx.signature_type),
    ?assertEqual(Owner, DataItem#tx.owner),
    ?assertEqual(Target, DataItem#tx.target),
    ?assertEqual(Anchor, DataItem#tx.anchor),
    ?assertEqual(Tags, DataItem#tx.tags),
    ?assertEqual(Data, DataItem#tx.data),
    ?assertEqual(byte_size(Data), DataItem#tx.data_size).

empty_bundle_test() ->
    Bundle = serialize(dev_arweave_common:normalize([])),
    ?event(debug_test, {bundle, {explicit, Bundle}}),
    BundleItem = deserialize(Bundle),
    ?assertEqual(#{}, BundleItem#tx.data).

bundle_with_one_item_test() ->
    Item = new_item(
        crypto:strong_rand_bytes(32),
        crypto:strong_rand_bytes(32),
        [],
        ItemData = crypto:strong_rand_bytes(1000)
    ),
    ?event(debug_test, {item, Item}),
    Bundle = serialize(dev_arweave_common:normalize([Item])),
    ?event(debug_test, {bundle, {explicit, Bundle}}),
    Deserialized = deserialize(Bundle),
    ?event(debug_test, {bundle_item, Deserialized}),
    ?assertEqual(ItemData, (maps:get(<<"1">>, Deserialized#tx.data))#tx.data).

bundle_with_two_items_test() ->
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
    Bundle = serialize(dev_arweave_common:normalize([Item1, Item2])),
    BundleItem = deserialize(Bundle),
    ?assertEqual(ItemData1, (maps:get(<<"1">>, BundleItem#tx.data))#tx.data),
    ?assertEqual(ItemData2, (maps:get(<<"2">>, BundleItem#tx.data))#tx.data).

recursive_bundle_test() ->
    W = ar_wallet:new(),
    Item1 = sign_item(#tx{
        id = crypto:strong_rand_bytes(32),
        anchor = crypto:strong_rand_bytes(32),
        data = <<1:256/integer>>
    }, W),
    Item2 = sign_item(#tx{
        id = crypto:strong_rand_bytes(32),
        anchor = crypto:strong_rand_bytes(32),
        data = [Item1]
    }, W),
    Item3 = sign_item(#tx{
        id = crypto:strong_rand_bytes(32),
        anchor = crypto:strong_rand_bytes(32),
        data = [Item2]
    }, W),
    Bundle = serialize(dev_arweave_common:normalize([Item3])),
    BundleItem = deserialize(Bundle),
    #{<<"1">> := UnbundledItem3} = BundleItem#tx.data,
    #{<<"1">> := UnbundledItem2} = UnbundledItem3#tx.data,
    #{<<"1">> := UnbundledItem1} = UnbundledItem2#tx.data,
    ?assert(verify_item(UnbundledItem1)),
    % TODO: Verify bundled lists...
    ?assertEqual(Item1#tx.data, UnbundledItem1#tx.data).

bundle_map_test() ->
    W = ar_wallet:new(),
    Item1 = sign_item(#tx{
        format = ans104,
        data = <<"item1_data">>
    }, W),
    Item2 = sign_item(#tx{
        format = ans104,
        anchor = crypto:strong_rand_bytes(32),
        data = #{<<"key1">> => Item1}
    }, W),
    Bundle = serialize(dev_arweave_common:normalize(Item2)),
    BundleItem = deserialize(Bundle),
    ?assertEqual(Item1#tx.data, (maps:get(<<"key1">>, BundleItem#tx.data))#tx.data),
    ?assert(verify_item(BundleItem)).

extremely_large_bundle_test() ->
    W = ar_wallet:new(),
    Data = crypto:strong_rand_bytes(100_000_000),
    Norm = dev_arweave_common:normalize(#tx { data = #{ <<"key">> => #tx { data = Data } } }),
    Signed = sign_item(Norm, W),
    Serialized = serialize(dev_arweave_common:normalize(Signed)),
    Deserialized = deserialize(Serialized),
    ?assert(verify_item(Deserialized)).

basic_member_id_test() ->
    W = ar_wallet:new(),
    Item = sign_item(
        #tx{
            data = <<"data">>
        },
        W
    ),
    ?assertEqual(true, member(Item#tx.id, Item)),
    ?assertEqual(true, member(id(Item, unsigned), Item)),
    ?assertEqual(false, member(crypto:strong_rand_bytes(32), Item)).

deep_member_test() ->
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
    Item2 = deserialize(serialize(dev_arweave_common:normalize(sign_item(
        #tx{
            data = #{ <<"key2">> => Item }
        },
        W
    )))),
    ?assertEqual(true, member(<<"key1">>, Item2)),
    ?assertEqual(true, member(<<"key2">>, Item2)),
    ?assertEqual(true, member(Item#tx.id, Item2)),
    ?assertEqual(true, member(Item2#tx.id, Item2)),
    ?assertEqual(true, member(id(Item, unsigned), Item2)),
    ?assertEqual(true, member(id(Item2, unsigned), Item2)),
    ?assertEqual(false, member(crypto:strong_rand_bytes(32), Item2)).

serialize_deserialize_deep_signed_bundle_test() ->
    W = ar_wallet:new(),
    % Test that we can serialize, deserialize, and get the same IDs back.
    Item1 = sign_item(#tx{data = <<"item1_data">>}, W),
    Item2 = sign_item(#tx{data = #{<<"key1">> => Item1}}, W),
    Bundle = serialize(dev_arweave_common:normalize(Item2)),
    Deser2 = deserialize(Bundle),
    #{ <<"key1">> := Deser1 } = Deser2#tx.data,
    ?assertEqual(id(Item2, unsigned), id(Deser2, unsigned)),
    ?assertEqual(id(Item2, signed), id(Deser2, signed)),
    ?assertEqual(id(Item1, unsigned), id(Deser1, unsigned)),
    ?assertEqual(id(Item1, signed), id(Deser1, signed)),
    % Test that we can sign an item twice and the unsigned ID is the same.
    Item3 = sign_item(Item2, W),
    ?assertEqual(id(Item3, unsigned), id(Item2, unsigned)),
    ?assert(verify_item(Item3)).

%% @doc Deserialize and reserialize a data item produced by the arbundles JS
%% library. This validates both that we can read an arbundles.js data itme
%% but also that our data item serialization code is compatible with it.
arbundles_item_roundtrip_test() ->
    {ok, Bin} = file:read_file(<<"test/arbundles.js/ans104-item.bundle">>),
    ?event(debug_test, {bin, {explicit, Bin}}),
    Item = deserialize(Bin),
    ?event(debug_test, {item, Item}),
    ?assert(verify_item(Item)),
    ?assertEqual(<<"hello world">>, Item#tx.data),
    ?assertEqual(11, Item#tx.data_size),    
    ?assertEqual(
        hb_util:decode(<<"eJmUI4azsmhRCZRf3MaX0CFDHwWn9oStIirZma3ql68">>),
        Item#tx.target),
    ?assertEqual(?DEFAULT_ANCHOR, Item#tx.anchor),
    ?assertEqual([
        {<<"Content-Type">>, <<"text/plain">>},
        {<<"App-Name">>, <<"arbundles-gen">>}
    ], Item#tx.tags),
    Serialized = serialize(dev_arweave_common:normalize(Item)),
    ?assertEqual(Bin, Serialized).

arbundles_list_bundle_roundtrip_test() ->
    W = ar_wallet:new(),
    {ok, Bin} = file:read_file(<<"test/arbundles.js/ans104-list-bundle.bundle">>),
    TX = sign_item(#tx{ 
        format = ans104,
        data = Bin,
        data_size = byte_size(Bin),
        tags = ?BUNDLE_TAGS
    }, W),
    ?event(debug_test, {tx, {explicit, TX}}),
    ?assert(verify_item(TX)),

    Deserialized = deserialize(TX),
    ?event(debug_test, {deserialized, Deserialized}),
    ?assertEqual(3, maps:size(Deserialized#tx.data)),
    #{<<"1">> := Item1, <<"2">> := Item2, <<"3">> := Item3} = 
        Deserialized#tx.data,
    ?assertEqual(<<"first">>, Item1#tx.data),
    ?assertEqual([{<<"Type">>, <<"list">>}, {<<"Index">>, <<"0">>}], Item1#tx.tags),
    ?assertEqual(
        hb_util:decode(<<"Tu6LHQdEVK7lNF3AOAHrVBjl2CFvQizd5VaWBvdFRSs">>),
        Item1#tx.target),
    ?assertEqual(
        hb_util:decode(<<"N1k7gUBck6EBgmApl58Nxxhe3TTATSHeEyyXhdFVe9A">>),
        Item1#tx.anchor),
    ?assertEqual(<<"second">>, Item2#tx.data),
    ?assertEqual([{<<"Type">>, <<"list">>}, {<<"Index">>, <<"1">>}], Item2#tx.tags),
    ?assertEqual(?DEFAULT_TARGET, Item2#tx.target),
    ?assertEqual(
        hb_util:decode(<<"fgAVH_xJJU1tkzWSmSfBfb_KBX8sa_FQ2b7YWuE08Ko">>),
        Item2#tx.anchor),
    ?assertEqual(<<"third">>, Item3#tx.data),
    ?assertEqual([{<<"Type">>, <<"list">>}, {<<"Index">>, <<"2">>}], Item3#tx.tags),
    ?assertEqual(?DEFAULT_TARGET, Item3#tx.target),
    ?assertEqual(?DEFAULT_ANCHOR, Item3#tx.anchor),
    ?assert(verify_item(Item1)),
    ?assert(verify_item(Item2)),
    ?assert(verify_item(Item3)),

    Reserialized = dev_arweave_common:normalize(Deserialized),
    ?event(debug_test, {reserialized, Reserialized}),
    ?assert(verify_item(Reserialized)),
    ?assertEqual(Bin, Reserialized#tx.data),
    ok.

arbundles_single_list_bundle_roundtrip_test() ->
    W = ar_wallet:new(),
    {ok, Bin} = file:read_file(<<"test/arbundles.js/ans104-single-list-bundle.bundle">>),
    % Deserialize and verify the arbundles.js bundle
    TX = sign_item(#tx{ 
        format = ans104,
        data = Bin,
        data_size = byte_size(Bin),
        tags = ?BUNDLE_TAGS
    }, W),
    ?event(debug_test, {tx, {explicit, TX}}),
    ?assert(verify_item(TX)),
    
    Deserialized = deserialize(TX),
    ?event(debug_test, {deserialized, Deserialized}),
    ?assertEqual(1, maps:size(Deserialized#tx.data)),
    #{<<"1">> := Item} = Deserialized#tx.data,
    ?event(debug_test, {item, Item}),
    ?assertEqual(
        <<"IchWLlJKLaCqKd4KW6BcDKe560XpfgFuPHXjjK8tfgA">>,
        hb_util:encode(Item#tx.id)),
    ?assertEqual(<<"only">>, Item#tx.data),
    ?assertEqual([{<<"Type">>, <<"list">>}, {<<"Index">>, <<"1">>}], Item#tx.tags),
    ?assert(verify_item(Item)),

    Reserialized = dev_arweave_common:normalize(Deserialized),
    ?event(debug_test, {reserialized, Reserialized}),
    ?assert(verify_item(Reserialized)),
    ?assertEqual(Bin, Reserialized#tx.data),
    ok.

%% @doc Read a serialized bundle from disk, assert it is as it should be, and
%% do a full deserialize/serialize roundtrip to confirm idempotency.
%% The file in question was validated against dha-team/arbundles v1.0.3 on
%% 2025-09-07, so this test also serves to validate that ar_bundles.erl can
%% read and write to a bundle that is compatible with dha-team/arbundles.
arbundles_map_bundle_roundtrip_test() ->
    {ok, Bin} = file:read_file(<<"test/arbundles.js/ans104-map-bundle-erlang.bundle">>),
    
    Deserialized = deserialize(Bin),
    ?event(debug_test, {deserialized, Deserialized}),
    ?assert(verify_item(Deserialized)),
    ?assertEqual([
        {<<"bundle-format">>, <<"binary">>},
        {<<"bundle-version">>, <<"2.0.0">>},
        {<<"bundle-map">>, <<"DwgwetwuSXGrnQiHFziiRLPKIucN5ua9KWkHA-nRQJQ">>}
    ], Deserialized#tx.tags),

    #{ <<"key1">> := Item1, <<"key2">> := Item2 } = Deserialized#tx.data,
    ?assert(verify_item(Item1)),
    ?assert(verify_item(Item2)),
    ?assertEqual(<<"item1_data">>, Item1#tx.data),
    ?assertEqual(<<"item2_data">>, Item2#tx.data),

    Manifest = Deserialized#tx.manifest,
    ?event(debug_test, {manifest, Manifest}),
    ?assertNotEqual(undefined, Manifest),
    ?assertEqual(false, dev_arweave_common:is_signed(Manifest)),
    ?assertEqual([
        {<<"data-protocol">>, <<"bundle-map">>},
        {<<"variant">>, <<"0.0.1">>}
    ], Manifest#tx.tags),
    Index = hb_json:decode(Manifest#tx.data),
    ?event(debug_test, {index, Index}),
    ?assertEqual(#{ 
        <<"key1">> => <<"zZXTg5K_9G3EnpMUOhp9QX1tqa8dJa32p2JPkQtiPT0">>,
        <<"key2">> => <<"m4D2fObeaz5qFkhpacO1K351jaksg2j0-wpyCetAOb4">>
    }, Index),
    
    Reserialized = serialize(dev_arweave_common:normalize(Deserialized)),
    ?event(debug_test, {reserialized, Reserialized}),
    ?assertEqual(Bin, Reserialized).

%% @doc This test generates and writes a map bundle to a file so that we can
%% validate that it is handled correctly by dha-team/arbundles. You can
%% validate the bundle by running
%% `node test/arbundles.js/validate-bundle.js test/arbundles.js/ans104-map-bundle-erlang.bundle`
%% 
%% We will also use this file in the arbundles_map_bundle_roundtrip_test as
%% a regression test to confirm that ar_bundles.erl continues to validate
%% and generate a compatible bundle.
%% 
%% To regenerate the .bundle file, rename the test to
%% `generate_and_write_map_bundle_test'
generate_and_write_map_bundle_test_disabled() ->
    W = ar_wallet:new(),
    Item1 = sign_item(#tx{
        format = ans104,
        data = <<"item1_data">>
    }, W),
    Item2 = sign_item(#tx{
        format = ans104,
        data = <<"item2_data">>
    }, W),
    Bundle = sign_item(#tx{
        format = ans104,
        data = #{
            <<"key1">> => Item1,
            <<"key2">> => Item2
        }
    }, W),
    ?event(debug_test, {bundle, {explicit, Bundle}}),
    ?assert(verify_item(Bundle)),
    Serialized = serialize(Bundle),
    ?event(debug_test, {serialized, {explicit, Serialized}}),

    Deserialized = deserialize(Serialized),
    ?event(debug_test, {deserialized, {explicit, Deserialized}}),
    ?assert(verify_item(Deserialized)),
    ok = file:write_file(
        <<"test/arbundles.js/ans104-map-bundle-erlang.bundle">>, Serialized).