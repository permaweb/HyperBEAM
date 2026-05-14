-module(ar_bundles).
-export([signer/1]).
-export([id/1, id/2, hd/1, member/2, find/2]).
-export([new_item/4, sign_item/2, verify_item/1]).
-export([encode_tags/1, decode_tags/1]).
-export([serialize/1, deserialize/1, serialize_bundle/3]).
-export([deserialize_header/1, deserialize_item_wrapper/1]).
-export([data_item_signature_data/1]).
-export([bundle_header_size/1, decode_bundle_header/1]).
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
    Item =
        (dev_arweave_common:normalize(RawItem))#tx{
            format = ans104,
            owner = Owner,
            signature_type = KeyType
        },
    % Generate the signature from the data item's data segment in 'signed'-ready mode.
    Sig = ar_wallet:sign(PrivKey, data_item_signature_data(Item)),
    dev_arweave_common:reset_ids(Item#tx{signature = Sig}).

%% @doc Verify the validity of a data item.
verify_item(DataItem) ->
    ValidID = verify_data_item_id(DataItem),
    ValidSignature = verify_data_item_signature(DataItem),
    ValidTags = verify_data_item_tags(DataItem),
    ?event(debug, {verify_item,
        {id, ValidID},
        {signature, ValidSignature},
        {tags, ValidTags}}),
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
        hb_util:check_size(TX#tx.owner, [0, 32, 33, 42, 65, byte_size(?DEFAULT_OWNER)]),
        {invalid_field, owner, TX#tx.owner}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.target, [0, 32]),
        {invalid_field, target, TX#tx.target}
    ),
    hb_util:ok_or_throw(TX,
        hb_util:check_size(TX#tx.signature, [0, 64, 65, byte_size(?DEFAULT_SIG)]),
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
    {_, Item} = dev_arweave_common:serialize_data(RawItem),
    ar_deep_hash:hash([
        utf8_encoded("dataitem"),
        utf8_encoded("1"),
        utf8_encoded(get_signature_type(Item#tx.signature_type)),
        <<(Item#tx.owner)/binary>>,
        <<(Item#tx.target)/binary>>,
        <<(Item#tx.anchor)/binary>>,
        encode_tags(Item#tx.tags),
        <<(Item#tx.data)/binary>>
    ]).

get_signature_type({rsa, 65537}) -> "1";
get_signature_type({eddsa, ed25519}) -> "2";
get_signature_type(ethereum) -> "3";
get_signature_type(solana) -> "4";
get_signature_type(typed_ethereum) -> "7".

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
    {_, TX} = dev_arweave_common:serialize_data(RawTX),
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

serialize_bundle(list, List, Normalize) when is_list(List) ->
    FinalizedData = finalize_bundle_data(
        lists:map(
            fun(Item) ->
                to_serialized_pair(Item, Normalize, signed)
            end,
            List)
    ),
    {undefined, FinalizedData};
serialize_bundle(BundleType, Map, Normalize) when is_map(Map) ->
    % TODO: Make this compatible with the normal manifest spec.
    % For now we just serialize the map to a JSON string of Key=>TXID
    BinItems = maps:map(
        fun(_, Item) -> 
            to_serialized_pair(Item, Normalize, unsigned)
        end,
        Map),
    {Manifest, BinItems2} = maybe_generate_manifest(BundleType, BinItems, Normalize),
    FinalizedData = finalize_bundle_data(BinItems2),
    {Manifest, FinalizedData};
serialize_bundle(_, Data, _Normalize) when is_binary(Data) ->
    {undefined, Data};
serialize_bundle(_, Data, _Normalize) ->
    throw({cannot_serialize_tx_data, must_be_list_or_map_or_binary, Data}).

maybe_generate_manifest(map, BinItems, Normalize) ->
    Index = maps:map(fun(_, {TXID, _}) -> hb_util:encode(TXID) end, BinItems),
    Manifest = new_manifest(Index),
    {ManifestID, ManifestSerialized} =
        to_serialized_pair(Manifest, Normalize, unsigned),
    {Manifest, [{ManifestID, ManifestSerialized} | maps:values(BinItems)]};
maybe_generate_manifest(_, BinItems, _Normalize) ->
    {undefined, maps:values(BinItems)}.

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

to_serialized_pair(Item, Normalize, Signed) when is_binary(Item) ->
    % Support bundling of bare binary payloads by wrapping them in a TX that
    % is explicitly marked as a binary data item.
    to_serialized_pair(
        #tx{ tags = [{<<"ao-type">>, <<"binary">>}], data = Item },
        Normalize, Signed);
to_serialized_pair(Item, true, Signed) ->
    to_serialized_pair(dev_arweave_common:normalize(Item), false, Signed);
to_serialized_pair(Item, false, Signed) ->
    ?event({to_serialized_pair, Item}),
    % TODO: This is a hack to get the ID of the item. We need to do this because we may not
    % have the ID in 'item' if it is just a map/list. We need to make this more efficient.
    Serialized = serialize(Item),
    Deserialized = deserialize(Serialized),
    case id(Deserialized, Signed) of
        not_signed ->
            % A signed ID was requested, but the item is not signed, so fall
            % back to unsigned.
            {id(Deserialized, unsigned), Serialized};
        ID ->
            {ID, Serialized}
    end.

%% @doc Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 -- but it is is written in
%% little-endian format which is why we encode to `<<1, 0>>'.
encode_signature_type({rsa, 65537}) ->
    <<1, 0>>;
encode_signature_type({eddsa, ed25519}) ->
    <<2, 0>>;
encode_signature_type(ethereum) ->
    <<3, 0>>;
encode_signature_type(solana) ->
    <<4, 0>>;
encode_signature_type(SigType) ->
    ?event(warning, {error_encoding_signature_type, {sig_type, SigType}}),
    {unsupported_tx_format, SigType}.

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

%% @doc Deserialize an item and unbundle it if it is a bundle, returning a #tx
%% with possibly deeply nested items in the #tx.data field.
deserialize_item(Binary) ->
    maybe_unbundle(deserialize_item_wrapper(Binary)).

%% @doc Deserialize only the _wrapper_ of an item, leaving the data unprocessed
%% in the case that it is a bundle. It may be unbundled by calling `maybe_unbundle/1'
%% at any later point.
deserialize_item_wrapper(Binary) ->
    {ok, _HeaderSize, Header} = deserialize_header(Binary),
    dev_arweave_common:reset_ids(Header).

%% @doc Deserialize the header of an item, returning a #tx record with the 
%% remaining unprocessed data in the #tx.data field.
deserialize_header(Binary) ->
    {SignatureType, Signature, Owner, Rest} = decode_signature(Binary),
    {Target, Rest2} = decode_optional_field(Rest),
    {Anchor, Rest3} = decode_optional_field(Rest2),
    {Tags, RemainingData} = decode_tags(Rest3),
    HeaderSize = byte_size(Binary) - byte_size(RemainingData),
    {
        ok,
        HeaderSize,
        #tx{
            format = ans104,
            signature_type = SignatureType,
            signature = Signature,
            owner = Owner,
            target = Target,
            anchor = Anchor,
            tags = Tags,
            data = RemainingData,
            data_size = byte_size(RemainingData)
        }
    }.

maybe_unbundle(Item) ->
    case dev_arweave_common:type(Item) of
        list -> unbundle_list(Item);
        binary -> Item;
        map -> unbundle_map(Item)
    end.

unbundle_list(Item) ->
    case unbundle(Item#tx.data) of
        ?DEFAULT_DATA -> Item#tx{data = ?DEFAULT_DATA};
        Items -> Item#tx{data = hb_util:list_to_numbered_message(Items)}
    end.

unbundle_map(Item) ->
    MapTXID = dev_arweave_common:tagfind(<<"bundle-map">>, Item#tx.tags, <<>>),
    case unbundle(Item#tx.data) of
        ?DEFAULT_DATA -> Item#tx{data = ?DEFAULT_DATA};
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
unbundle(?DEFAULT_DATA) -> ?DEFAULT_DATA.

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

bundle_header_size(<<Count:256/little-integer, _/binary>>) ->
    % Eeach item in the bundle header index consumes 64 bytes
    32 + (Count * 64);
bundle_header_size(_) ->
    invalid_bundle_header.

decode_bundle_header(<<Count:256/little-integer, Content/binary>>) ->
    decode_bundle_header(Count, Content);
decode_bundle_header(<<>>) ->
    {<<>>, []};
decode_bundle_header(_) ->
    invalid_bundle_header.

decode_bundle_header(Count, Bin) -> decode_bundle_header(Count, Bin, []).
decode_bundle_header(0, ItemsBin, Header) ->
    {ItemsBin, lists:reverse(Header)};
decode_bundle_header(
    Count,
    <<Size:256/little-integer, ID:32/binary, Rest/binary>>,
    Header
) ->
    decode_bundle_header(Count - 1, Rest, [{ID, Size} | Header]);
decode_bundle_header(_, _, _) ->
    invalid_bundle_header.

%% @doc Decode the signature from a binary format. Only RSA 4096 is currently supported.
%% Note: the signature type '1' corresponds to RSA 4096 - but it is is written in
%% little-endian format which is why we match on `<<1, 0>>'.
decode_signature(<<1, 0, Signature:512/binary, Owner:512/binary, Rest/binary>>) ->
    {{rsa, 65537}, Signature, Owner, Rest};
decode_signature(<<2, 0, Signature:64/binary, Owner:32/binary, Rest/binary>>) ->
    {{eddsa, ed25519}, Signature, Owner, Rest};
decode_signature(<<3, 0, Signature:65/binary, Owner:65/binary, Rest/binary>>) ->
    {ethereum, Signature, Owner, Rest};
decode_signature(<<4, 0, Signature:64/binary, Owner:32/binary, Rest/binary>>) ->
    {solana, Signature, Owner, Rest};
decode_signature(<<7, 0, Signature:65/binary, Owner:42/binary, Rest/binary>>) ->
    {typed_ethereum, Signature, Owner, Rest};
decode_signature(Other) ->
    SigType = binary:part(Other, 0, 2),
    ?event(warning, {error_decoding_signature,
        {sig_type, {explicit, SigType}}}),
    {unsupported_tx_format, SigType}.

%% @doc Decode tags from a binary format using Apache Avro.
decode_tags(<<0:64/little-integer, 0:64/little-integer, Rest/binary>>) ->
    {[], Rest};
decode_tags(<<_TagCount:64/little-integer, _TagSize:64/little-integer, Binary/binary>>) ->
    case decode_zigzag(Binary) of
        {0, Rest} ->
            %% count=0 is itself the Avro end-of-array terminator; no items, no second zero.
            {[], Rest};
        {Count, BlocksBinary} ->
            {Tags, Rest} = decode_avro_tags(BlocksBinary, Count),
            %% Pull out the terminating zero block that follows the last item block.
            {0, Rest2} = decode_zigzag(Rest),
            {Tags, Rest2}
    end.

decode_optional_field(<<0, Rest/binary>>) ->
    {<<>>, Rest};
decode_optional_field(<<1:8/little-integer, Field:32/binary, Rest/binary>>) ->
    {Field, Rest}.

%% @doc Decode Avro blocks (for tags) from binary.
decode_avro_tags(<<>>, _) ->
    {[], <<>>};
decode_avro_tags(Binary, Count) when Count =:= 0 ->
    {[], Binary};
decode_avro_tags(Binary, Count) when Count < 0 ->
    %% Avro long-form block: negative count encodes item count as abs(Count),
    %% followed by a zigzag-encoded byte size which we skip.
    {_ByteBlockSize, Rest} = decode_zigzag(Binary),
    decode_avro_tags(Rest, -Count);
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

bundle_header_size_test() ->
    ?assertEqual(672, bundle_header_size(<<10:256/little, 1234/little>>)),
    ?assertEqual(32, bundle_header_size(<<0:256/little>>)),
    ?assertEqual(invalid_bundle_header, bundle_header_size(<<>>)),
    ?assertEqual(invalid_bundle_header, bundle_header_size(<<0>>)).

decode_bundle_header_test() ->
    ?assertEqual({<<>>, []}, decode_bundle_header(<<>>)),
    Tail = <<"tail">>,
    ?assertEqual(
        {Tail, []},
        decode_bundle_header(<<0:256/little, Tail/binary>>)
    ),
    ID1 = crypto:strong_rand_bytes(32),
    Items1 = <<"abcde">>,
    ?assertEqual(
        {Items1, [{ID1, 5}]},
        decode_bundle_header(<<1:256/little, 5:256/little, ID1:32/binary, Items1/binary>>)
    ),
    ID2 = crypto:strong_rand_bytes(32),
    ID3 = crypto:strong_rand_bytes(32),
    Items2 = <<"payload">>,
    ?assertEqual(
        {Items2, [{ID2, 4}, {ID3, 2}]},
        decode_bundle_header(
            <<
                2:256/little,
                4:256/little, ID2:32/binary,
                2:256/little, ID3:32/binary,
                Items2/binary
            >>
        )
    ),
    ?assertEqual(
        {<<>>, [{ID1, 6}]},
        decode_bundle_header(<<1:256/little, 6:256/little, ID1:32/binary>>)
    ).

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
    Bundle = serialize(dev_arweave_common:normalize(#tx{data = []})),
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
    Bundle = serialize(dev_arweave_common:normalize(#tx{data = [Item]})),
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
    Bundle = serialize(dev_arweave_common:normalize(#tx{data = [Item1, Item2]})),
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
    Bundle = serialize(dev_arweave_common:normalize(#tx{data = [Item3]})),
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

eddsa_cases_test() ->
    Key = ar_wallet:new(?EDDSA_KEY_TYPE),
    %% Owner and SignatureType defined during signing process.
    Item1 = sign_item(#tx{
        format = ans104,
        target = crypto:strong_rand_bytes(32),
        anchor = crypto:strong_rand_bytes(32),
        tags = [{<<"tag1">>, <<"value1">>}, {<<"tag2">>, <<"value2">>}],
        data = <<"item1_data">>
    }, Key),
    Bundle = serialize(dev_arweave_common:normalize(Item1)),
    BundleItem = deserialize(Bundle),
    %% Sign a valid transaction and verify it
    ?assert(verify_item(BundleItem)),
    %% Missing Anchor should fail
    ?assertNot(verify_item(BundleItem#tx{anchor = <<>>})),
    %% Missing Tags should fail
    ?assertNot(verify_item(BundleItem#tx{tags = []})),
    %% Missing Owner should fail
    ?assertNot(verify_item(BundleItem#tx{owner = crypto:strong_rand_bytes(32)})),
    %% Missing Target should fail
    ?assertNot(verify_item(BundleItem#tx{target = <<>>})),
    %% Missing Data should fail
    ?assertNot(verify_item(BundleItem#tx{data = <<>>})),
    ok.

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

deserialize_ed25519_transaction_test() ->
    % ans104-item-ed25519.bin is dataitem 1rTy7gQuK9lJydlKqCEhtGLp2WWG-GOrVo5JdiCmaxs
    {ok, Serialized} = file:read_file(<<"test/arbundles.js/ans104-item-ed25519.bin">>),
    Deserialized = deserialize(Serialized),
    ?assertEqual([{<<"Content-Type">>,<<"image/png">>}], Deserialized#tx.tags),
    ?assertEqual(<<"ZbExyvGrJKOJTJcHMtKzoOZVCQBkjZ+5">>, Deserialized#tx.anchor),
    ?assertEqual(<<"ejhYD9Cw9VCsVik6yGLoclo3CLRvAITHTZamLY_6ro4">>,
        hb_util:human_id(ar_wallet:to_address(Deserialized#tx.owner, Deserialized#tx.signature_type))),
    ?assert(verify_item(Deserialized)).

deserialize_solana_transaction_test() ->
    % ans104-item-ed25519.bin is dataitem hXKqH_9rkYZ7LwvVps81uKNZd_i36WZjlp4Wnc5BkiE
    {ok, Serialized} = file:read_file(<<"test/arbundles.js/ans104-item-solana.bin">>),
    Deserialized = deserialize(Serialized),
    ?assertEqual([], Deserialized#tx.tags),
    ?assertEqual(<<"e/GCI2gwfkcyXG6Q3n3CVuA0zT4EmSSf">>, Deserialized#tx.anchor),
    ?assertEqual(<<"GGuACHp2FbtB4wwT5TmPCU6W5FGa3wB1vqno4gsKsxHz">>,
        hb_util:human_id(ar_wallet:to_address(Deserialized#tx.owner, Deserialized#tx.signature_type))),
    ?assert(verify_item(Deserialized)).

deserialize_ethereum_transaction_test() ->
    % ans104-item-ethereum.bin is dataitem te5MPrOxPqXrVygIQgzp4ZgImLN8CW-qPaI_olhlWyx
    {ok, Serialized} = file:read_file(<<"test/arbundles.js/ans104-item-ethereum.bin">>),
    Deserialized = deserialize(Serialized),
    ?assertEqual(ethereum, Deserialized#tx.signature_type),
    ExpectedTags = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"App-Name">>, <<"Rodeo">>},
        {<<"Token-Contract">>, <<"0xB6e822C6D5E0dEC983d76F28E56616057f88380f">>},
        {<<"Token-Id">>, <<"328">>},
        {<<"Chain-Id">>, <<"8453">>}
    ],
    ?assertEqual(ExpectedTags, Deserialized#tx.tags),
    ?assertEqual(<<"zZHoADuo74sWmhEF0V-D4sxa4rj3rUR5_r7tSpWSmtY">>, hb_util:encode(Deserialized#tx.anchor)),
    ?assertEqual(<<"0x626334b6ef6D3e8537E9f8d97d65f59832219315">>,
        hb_util:human_id(ar_wallet:to_address(Deserialized#tx.owner, Deserialized#tx.signature_type))),
    ?assert(verify_item(Deserialized)).

%% @doc Test TX header with avro with negative count.
%% TXID: sfuxzQEEIFo5w6swrIPNjqUXCkaRm1BiuP5E3tmuNeU
deserialize_header_test() ->
    Binary = <<3,0,159,201,29,54,204,49,217,185,169,30,79,27,113,154,209,149,155,4,94,109,231,29,126,91,206,2,58,46,48,92,26,78,97,101,237,211,88,230,249,10,184,209,7,255,179,104,207,12,190,33,166,28,50,204,70,145,233,91,105,67,114,10,93,238,28,4,215,155,88,31,136,43,116,83,120,249,145,176,234,17,216,136,40,180,180,163,138,235,206,53,81,171,155,115,126,116,41,35,253,114,207,9,111,109,251,54,165,211,123,116,38,234,140,182,15,169,158,249,185,207,52,106,5,41,28,28,118,69,217,53,0,0,2,0,0,0,0,0,0,0,112,0,0,0,0,0,0,0,3,216,1,18,73,80,70,83,45,72,97,115,104,118,98,97,102,107,114,101,105,98,103,100,115,97,104,54,122,111,108,50,101,111,101,54,98,112,120,54,51,104,99,55,117,51,112,107,53,120,105,106,51,116,120,110,98,120,102,120,97,116,116,97,116,113,51,108,108,52,119,100,97,24,67,111,110,116,101,110,116,45,84,121,112,101,48,97,112,112,108,105,99,97,116,105,111,110,47,111,99,116,101,116,45,115,116,114,101,97,109,0,82,73,70,70,34,0,0,0,87,69,66,80,86,80,56,76,21,0,0,0,47,99,192,24,0,7,16,17,253,15,3,144,16,254,239,151,34,250,159,74,4,0>>,
    {ok, _, TXheader} = deserialize_header(Binary),
    [{T1, V1}, {T2, V2}] = TXheader#tx.tags,
    ?assertEqual(<<"IPFS-Hash">>, T1),
    ?assertEqual(<<"bafkreibgdsah6zol2eoe6bpx63hc7u3pk5xij3txnbxfxattatq3ll4wda">>, V1),
    ?assertEqual(<<"Content-Type">>, T2),
    ?assertEqual(<<"application/octet-stream">>, V2).

%% @doc Test TX with tag_count=0 and tag_byte_size=1 (single 0x00 Avro terminator).
%% TXID: 8cjDy2khfMsc3hrvGp7PrLVYfD_4aYQxEILNSZ0Pv74
deserialize_header_zero_tags_test() ->
    Binary = <<1,0,18,82,93,211,156,242,244,169,161,47,166,208,115,36,213,40,0,93,154,240,8,204,20,212,104,151,139,9,128,241,81,27,11,57,160,166,69,192,186,39,42,227,40,237,130,69,179,65,136,42,80,65,139,79,25,65,153,143,46,15,132,111,87,243,246,67,181,142,222,225,147,110,169,242,74,192,105,255,120,19,209,63,112,186,141,170,140,103,45,0,211,110,163,82,135,225,255,208,107,100,3,152,255,81,154,126,86,97,163,28,62,180,189,114,89,160,66,187,111,125,66,109,170,58,123,61,184,219,142,116,35,103,150,240,248,53,150,212,192,41,85,108,128,184,101,193,27,132,181,143,15,11,191,248,33,41,240,210,238,48,102,172,50,166,214,39,64,109,63,59,230,203,223,73,29,160,231,104,134,147,74,57,203,0,50,89,33,127,157,189,246,131,158,162,39,239,221,254,204,89,80,61,114,150,64,63,118,46,140,6,108,49,94,201,118,236,101,27,77,104,189,52,185,12,133,36,54,147,231,201,110,213,71,233,29,52,157,226,155,90,44,212,250,10,52,210,35,59,68,148,166,228,94,97,65,90,246,57,92,170,53,51,77,192,244,62,201,15,55,55,200,121,224,215,182,124,227,130,27,20,133,20,184,45,80,240,55,144,69,75,241,149,161,35,178,2,76,86,47,116,208,124,252,36,111,95,55,138,254,75,253,190,155,10,60,132,70,38,212,182,215,219,125,128,195,11,200,55,2,254,250,188,172,56,165,118,160,31,170,89,144,194,231,138,50,13,0,157,171,171,5,109,15,84,207,112,17,28,47,241,135,102,184,93,161,9,217,110,155,38,229,78,168,18,26,200,67,18,138,179,37,96,53,34,234,230,52,135,152,2,235,76,231,195,71,34,157,128,29,60,58,166,222,58,7,252,194,120,77,53,126,1,111,189,35,252,150,36,238,153,22,104,198,183,169,228,197,6,23,65,72,48,128,185,242,229,123,234,251,95,182,36,36,232,125,164,253,137,69,117,123,224,247,128,38,82,229,26,129,164,38,33,159,92,182,124,100,81,66,111,93,32,29,8,78,37,74,89,127,97,96,152,166,198,6,107,67,35,193,209,38,247,181,139,163,49,127,19,91,124,74,244,204,168,187,235,191,122,106,174,150,76,191,24,214,127,38,161,212,173,48,67,70,229,238,61,83,12,113,62,250,65,191,183,157,18,202,100,136,209,199,81,241,15,3,88,39,135,178,184,58,148,107,175,186,180,132,157,26,157,24,95,47,8,162,28,3,118,66,213,6,77,219,52,180,67,19,254,191,115,164,187,70,25,124,141,48,150,100,64,55,18,71,246,116,51,233,203,163,91,112,107,226,9,98,146,87,91,229,11,80,103,123,169,251,119,194,87,150,250,225,4,193,18,199,151,15,157,102,249,18,53,17,253,39,17,63,222,5,126,52,81,191,47,73,154,18,228,62,24,106,107,176,251,63,84,167,135,6,5,93,141,50,88,12,102,236,224,223,227,60,158,139,89,252,62,173,1,133,135,47,232,244,158,255,170,207,235,52,221,202,238,182,172,33,131,78,227,138,211,6,201,53,178,85,17,69,2,104,221,137,222,191,110,185,60,17,219,112,98,173,110,146,105,91,108,206,189,213,39,162,33,255,223,252,71,102,179,138,174,207,129,192,31,39,180,139,253,57,128,188,223,192,79,35,111,121,86,63,165,54,201,240,149,118,185,58,202,47,95,63,129,242,182,212,29,138,132,91,214,194,214,124,52,57,131,245,121,136,15,109,75,121,133,72,96,213,72,47,17,214,188,240,42,106,32,226,101,64,196,6,122,144,67,206,148,18,131,139,72,6,182,149,176,188,235,217,105,57,180,82,226,57,58,33,116,123,117,237,198,0,66,76,63,243,210,40,116,10,70,24,220,109,12,13,7,57,146,203,165,38,118,211,75,141,33,97,114,5,231,8,56,48,38,22,135,86,239,113,240,200,149,249,109,14,99,66,27,33,60,224,220,45,90,208,94,214,73,64,7,96,77,31,78,205,192,62,21,152,241,104,29,59,37,136,214,23,33,123,19,114,233,39,182,2,237,147,194,16,125,174,219,118,93,118,93,136,93,192,87,70,194,235,128,189,254,11,166,87,132,173,14,165,156,2,230,246,12,86,212,5,105,51,115,199,80,80,139,135,177,225,177,190,93,29,100,177,207,43,68,87,111,129,187,86,68,126,99,230,197,39,41,212,137,251,47,235,109,229,0,251,17,223,32,156,185,52,180,8,246,187,178,27,136,66,196,125,142,54,128,56,200,108,65,0,1,188,29,211,86,11,51,139,73,247,84,155,179,70,10,236,247,120,180,171,217,67,163,222,9,142,221,1,205,187,99,159,180,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,91,91,53,57,48,52,57,44,32,123,34,100,105,116,101,109,34,58,32,91,34,115,116,80,97,105,68,82,80,98,101,103,102,78,116,90,118,69,55,77,119,118,45,97,75,118,95,116,57,79,90,73,108,74,117,101,52,97,121,67,74,121,80,119,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,49,51,54,49,49,48,56,57,55,125,44,32,48,44,32,51,48,50,53,54,55,54,54,51,49,93,44,32,91,53,57,48,52,57,44,32,123,34,100,105,116,101,109,34,58,32,91,34,51,95,104,65,77,78,71,48,105,104,100,53,77,51,77,102,108,66,100,97,113,79,120,120,113,72,100,74,79,104,56,105,102,66,56,104,49,45,109,53,95,79,52,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,49,56,54,48,55,55,53,49,57,125,44,32,51,48,50,53,54,55,54,54,51,49,44,32,49,53,52,56,55,53,48,49,50,56,93,44,32,91,49,57,54,56,51,44,32,123,34,100,105,116,101,109,34,58,32,91,34,56,103,104,54,117,119,68,115,119,77,111,49,80,81,112,83,87,121,51,49,110,104,52,97,118,68,87,82,71,112,107,103,95,117,80,101,79,88,90,49,73,121,65,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,48,51,55,51,56,57,55,57,125,44,32,52,53,55,52,52,50,54,55,53,57,44,32,53,55,49,54,52,56,52,49,57,93,44,32,91,54,53,54,49,44,32,123,34,100,105,116,101,109,34,58,32,91,34,72,95,72,111,81,113,67,82,45,45,90,74,122,81,97,53,72,51,90,111,45,109,121,100,75,75,45,120,69,78,54,98,66,53,119,76,102,83,68,106,75,55,69,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,48,57,54,52,51,49,48,57,125,44,32,53,49,52,54,48,55,53,49,55,56,44,32,49,55,54,50,52,54,52,50,52,93,44,32,91,54,53,54,49,44,32,123,34,100,105,116,101,109,34,58,32,91,34,83,103,87,112,122,75,108,103,102,106,71,88,103,77,97,86,90,53,82,115,45,104,49,122,118,67,95,105,108,80,70,84,65,107,65,87,110,75,98,122,120,121,56,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,49,53,53,49,56,54,48,48,125,44,32,53,51,50,50,51,50,49,54,48,50,44,32,49,55,50,54,50,51,48,49,54,93,44,32,91,50,49,56,55,44,32,123,34,100,105,116,101,109,34,58,32,91,34,51,101,45,88,83,104,66,74,71,50,84,95,53,90,95,54,81,54,73,88,101,117,65,75,116,45,82,120,71,65,105,67,48,78,90,86,106,75,66,53,57,74,115,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,49,55,52,53,54,56,57,54,125,44,32,53,52,57,52,57,52,52,54,49,56,44,32,53,55,49,49,54,51,55,53,93,44,32,91,50,49,56,55,44,32,123,34,100,105,116,101,109,34,58,32,91,34,120,111,113,71,115,75,88,52,110,109,69,90,97,54,55,100,49,109,110,109,53,103,113,54,45,83,105,113,116,100,77,122,100,55,110,79,54,90,69,69,49,77,69,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,49,57,51,56,57,49,55,50,125,44,32,53,53,53,50,48,54,48,57,57,51,44,32,53,55,48,52,55,57,57,51,93,44,32,91,55,50,57,44,32,123,34,100,105,116,101,109,34,58,32,91,34,116,114,115,68,110,111,55,103,83,76,57,86,109,97,105,83,79,98,78,121,66,68,122,54,80,115,85,122,85,50,81,52,74,111,86,70,75,101,51,113,79,88,119,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,50,48,48,51,57,56,48,49,125,44,32,53,54,48,57,49,48,56,57,56,54,44,32,49,57,49,56,57,54,53,57,93,44,32,91,50,52,51,44,32,123,34,100,105,116,101,109,34,58,32,91,34,121,51,113,115,103,72,45,109,56,70,98,117,110,118,105,65,71,82,76,77,115,105,116,56,75,87,113,90,57,116,70,76,90,88,106,109,67,45,68,79,45,85,103,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,50,48,50,53,51,51,53,50,125,44,32,53,54,50,56,50,57,56,54,52,53,44,32,54,50,51,50,55,57,51,93,44,32,91,56,49,44,32,123,34,100,105,116,101,109,34,58,32,91,34,77,82,98,110,88,116,87,101,117,87,68,73,117,86,69,70,52,117,72,101,70,120,121,117,84,51,56,86,78,122,115,95,120,100,68,51,104,90,86,71,106,54,65,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,50,48,51,50,54,52,51,56,125,44,32,53,54,51,52,53,51,49,52,51,56,44,32,50,49,56,53,55,52,52,93,44,32,91,50,55,44,32,123,34,100,105,116,101,109,34,58,32,91,34,118,82,69,118,106,98,74,53,89,111,51,88,115,117,103,86,119,82,71,55,65,48,56,76,121,72,87,99,104,111,70,81,97,53,48,100,74,121,101,45,56,80,107,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,50,48,51,52,57,52,56,53,125,44,32,53,54,51,54,55,49,55,49,56,50,44,32,55,48,52,55,51,56,93,44,32,91,49,44,32,123,34,100,105,116,101,109,34,58,32,91,34,113,79,117,70,50,85,48,102,52,112,49,122,76,108,90,103,88,73,86,48,122,50,54,76,101,81,54,52,72,75,86,95,87,95,89,78,52,81,113,117,95,81,115,34,93,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,50,48,51,53,48,52,54,52,125,44,32,53,54,51,55,52,50,49,57,50,48,44,32,49,55,52,50,49,93,44,32,91,45,49,44,32,123,34,99,97,112,116,117,114,101,34,58,32,123,34,100,105,116,101,109,34,58,32,91,34,101,109,99,115,55,79,50,67,73,85,52,81,66,70,68,115,104,56,66,51,57,57,112,114,76,70,77,107,113,52,101,114,98,52,84,75,104,74,108,85,76,81,69,34,93,44,32,34,116,105,109,101,34,58,32,91,49,54,55,55,50,50,48,51,53,48,46,49,57,48,49,52,53,55,93,125,44,32,34,109,105,110,95,98,108,111,99,107,34,58,32,91,49,49,50,52,52,52,51,44,32,34,72,101,72,89,89,111,65,107,82,73,75,119,74,90,98,95,112,110,116,50,72,120,122,80,114,80,69,120,106,90,57,85,66,49,109,81,102,85,113,71,57,122,107,70,48,71,65,107,119,97,67,119,55,99,51,77,103,88,112,117,116,100,75,71,34,93,44,32,34,97,112,105,95,116,105,109,101,115,116,97,109,112,34,58,32,49,54,55,55,50,50,48,51,53,49,51,52,52,44,32,34,100,114,111,112,112,101,100,34,58,32,110,117,108,108,125,44,32,53,54,51,55,52,51,57,51,52,49,44,32,49,55,51,56,55,93,93>>,
    {ok, _, TXheader} = deserialize_header(Binary),
    ?assertEqual([], TXheader#tx.tags).
