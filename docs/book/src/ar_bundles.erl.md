# ar_bundles

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/ar_bundles.erl)

Module for creating, signing, and verifying Arweave data items and bundles.

---

## Exported Functions

- `data_item_signature_data/1`
- `decode_tags/1`
- `deserialize/1`
- `deserialize/2`
- `encode_tags/1`
- `find/2`
- `format/1`
- `format/2`
- `format/3`
- `hd/1`
- `id/1`
- `id/2`
- `is_signed/1`
- `manifest_item/1`
- `manifest/1`
- `map/1`
- `member/2`
- `new_item/4`
- `normalize/1`
- `parse_manifest/1`
- `print/1`
- `reset_ids/1`
- `serialize/1`
- `serialize/2`
- `sign_item/2`
- `signer/1`
- `type/1`
- `verify_item/1`

---

### print

Module for creating, signing, and verifying Arweave data items and bundles.

```erlang
print(Item) ->
    io:format(standard_error, "~s", [lists:flatten(format(Item))]).
```

### format

Module for creating, signing, and verifying Arweave data items and bundles.

```erlang
format(Item) -> format(Item, 0).
```

### format

Module for creating, signing, and verifying Arweave data items and bundles.

```erlang
format(Item, Indent) -> format(Item, Indent, #{}).
```

### format

Module for creating, signing, and verifying Arweave data items and bundles.

```erlang
format(Item, Indent, Opts) when is_list(Item); is_map(Item) ->
    format(normalize(Item), Indent, Opts);
```

### format

Module for creating, signing, and verifying Arweave data items and bundles.

```erlang
format(Item, Indent, Opts) when is_record(Item, tx) ->
    MustVerify = hb_opts:get(debug_ids, true, Opts),
    Valid =
        if MustVerify -> verify_item(Item);
        true -> true
        end,
    UnsignedID =
        if MustVerify -> hb_util:encode(id(Item, unsigned));
        true -> <<"[SKIPPED ID]">>
        end,
    SignedID =
        if MustVerify ->
            case id(Item, signed) of
                not_signed -> <<"[NOT SIGNED]">>;
                ID -> hb_util:encode(ID)
            end;
        true -> <<"[SKIPPED ID]">>
        end,
    format_line(
        "TX ( ~s: ~s ) {",
        [
            if
                MustVerify andalso Item#tx.signature =/= ?DEFAULT_SIG ->
                    lists:flatten(
                        io_lib:format(
                            "~s (signed) ~s (unsigned)",
                            [SignedID, UnsignedID]
                        )
                    );
                true -> UnsignedID
            end,
            if
                not MustVerify -> "[SKIPPED VERIFICATION]";
                Valid == true -> "[SIGNED+VALID]";
                true -> "[UNSIGNED/INVALID]"
            end
        ],
        Indent
    ) ++
    case MustVerify andalso (not Valid) andalso Item#tx.signature =/= ?DEFAULT_SIG of
        true ->
            format_line("!!! CAUTION: ITEM IS SIGNED BUT INVALID !!!", Indent + 1);
        false -> []
    end ++
    case is_signed(Item) of
        true ->
            format_line("Signer: ~s", [hb_util:encode(signer(Item))], Indent + 1);
        false -> []
    end ++
    format_line("Target: ~s", [
            case Item#tx.target of
                <<>> -> "[NONE]";
                Target -> hb_util:id(Target)
            end
        ], Indent + 1) ++
    format_line("Last TX: ~s", [
            case Item#tx.anchor of
                ?DEFAULT_LAST_TX -> "[NONE]";
                LastTX -> hb_util:encode(LastTX)
            end
        ], Indent + 1) ++
    format_line("Tags:", Indent + 1) ++
    lists:map(
        fun({Key, Val}) -> format_line("~s -> ~s", [Key, Val], Indent + 2) end,
        Item#tx.tags
    ) ++
    format_line("Data:", Indent + 1) ++ format_data(Item, Indent + 2) ++
    format_line("}", Indent);
```

### format

Module for creating, signing, and verifying Arweave data items and bundles.

```erlang
format(Item, Indent, _Opts) ->
    % Whatever we have, its not a tx...
```

### format_data

```erlang
format_data(Item, Indent) when is_binary(Item#tx.data) ->
    case lists:keyfind(<<"bundle-format">>, 1, Item#tx.tags) of
        {_, _} ->
            format_data(deserialize(serialize(Item)), Indent);
        false ->
            format_line(
                "Binary: ~p... <~p bytes>",
                [format_binary(Item#tx.data), byte_size(Item#tx.data)],
                Indent
            )
    end;
```

### format_data

```erlang
format_data(Item, Indent) when is_map(Item#tx.data) ->
    format_line("Map:", Indent) ++
    lists:map(
        fun({Name, MapItem}) ->
            format_line("~s ->", [Name], Indent + 1) ++
            format(MapItem, Indent + 2)
        end,
        maps:to_list(Item#tx.data)
    );
```

### format_data

```erlang
format_data(Item, Indent) when is_list(Item#tx.data) ->
    format_line("List:", Indent) ++
    lists:map(
        fun(ListItem) ->
            format(ListItem, Indent + 1)
        end,
        Item#tx.data
    ).
```

### format_binary

```erlang
format_binary(Bin) ->
    lists:flatten(
        io_lib:format(
            "~p",
            [
                binary:part(
                    Bin,
                    0,
                    case byte_size(Bin) of
                        X when X < ?BIN_PRINT -> X;
                        _ -> ?BIN_PRINT
                    end
                )
            ]
        )
    ).
```

### format_line

```erlang
format_line(Str, Indent) -> format_line(Str, "", Indent).
```

### format_line

```erlang
format_line(RawStr, Fmt, Ind) ->
    io_lib:format(
        [$\s || _ <- lists:seq(1, Ind * ?INDENT_SPACES)] ++
            lists:flatten(RawStr) ++ "\n",
        Fmt
    ).
```

### signer

Return the address of the signer of an item, if it is signed.

```erlang
signer(#tx { owner = ?DEFAULT_OWNER }) -> undefined;
```

### signer

Return the address of the signer of an item, if it is signed.
Check if an item is signed.

```erlang
signer(Item) -> crypto:hash(sha256, Item#tx.owner).
```

### is_signed

Return the address of the signer of an item, if it is signed.
Check if an item is signed.

```erlang
is_signed(Item) ->
    Item#tx.signature =/= ?DEFAULT_SIG.
```

### id

Return the ID of an item -- either signed or unsigned as specified.

```erlang
id(Item) -> id(Item, unsigned).
```

### id

Return the ID of an item -- either signed or unsigned as specified.

```erlang
id(Item, Type) when not is_record(Item, tx) ->
    id(normalize(Item), Type);
```

### id

Return the ID of an item -- either signed or unsigned as specified.

```erlang
id(Item = #tx { unsigned_id = ?DEFAULT_ID }, unsigned) ->
    CorrectedItem = reset_ids(Item),
    CorrectedItem#tx.unsigned_id;
```

### id

Return the ID of an item -- either signed or unsigned as specified.

```erlang
id(#tx { unsigned_id = UnsignedID }, unsigned) ->
    UnsignedID;
```

### id

Return the ID of an item -- either signed or unsigned as specified.

```erlang
id(#tx { id = ?DEFAULT_ID }, signed) ->
    not_signed;
```

### id

Return the ID of an item -- either signed or unsigned as specified.

```erlang
id(#tx { id = ID }, signed) ->
    ID.
```

### hd

Return the first item in a bundle-map/list.

```erlang
hd(#tx { data = #{ <<"1">> := Msg } }) -> Msg;
```

### hd

Return the first item in a bundle-map/list.

```erlang
hd(#tx { data = [First | _] }) -> First;
```

### hd

Return the first item in a bundle-map/list.

```erlang
hd(TX = #tx { data = Binary }) when is_binary(Binary) ->
    ?MODULE:hd((deserialize(serialize(TX), binary))#tx.data);
```

### hd

Return the first item in a bundle-map/list.

```erlang
hd(#{ <<"1">> := Msg }) -> Msg;
```

### hd

Return the first item in a bundle-map/list.
Convert an item containing a map or list into an Erlang map.

```erlang
hd(_) -> undefined.
```

### map

Return the first item in a bundle-map/list.
Convert an item containing a map or list into an Erlang map.

```erlang
map(#tx { data = Map }) when is_map(Map) -> Map;
```

### map

Return the first item in a bundle-map/list.
Convert an item containing a map or list into an Erlang map.

```erlang
map(#tx { data = Data }) when is_list(Data) ->
    maps:from_list(
        lists:zipwith(
            fun({Index, Item}) -> {integer_to_binary(Index), map(Item)} end,
            lists:seq(1, length(Data)),
            Data
        )
    );
```

### map

Return the first item in a bundle-map/list.
Convert an item containing a map or list into an Erlang map.

```erlang
map(Item = #tx { data = Data }) when is_binary(Data) ->
    (maybe_unbundle(Item))#tx.data.
```

### member

Check if an item exists in a bundle-map/list.

```erlang
member(Key, Item) ->
    find(Key, Item) =/= not_found.
```

### find

Find an item in a bundle-map/list and return it.

```erlang
find(Key, Map) when is_map(Map) ->
    case maps:get(Key, Map, not_found) of
        not_found -> find(Key, maps:values(Map));
        Item -> Item
    end;
```

### find

Find an item in a bundle-map/list and return it.

```erlang
find(_Key, []) -> not_found;
```

### find

Find an item in a bundle-map/list and return it.

```erlang
find(Key, [Item|Rest]) ->
    case find(Key, Item) of
        not_found -> find(Key, Rest);
        CorrectItem -> CorrectItem
    end;
```

### find

Find an item in a bundle-map/list and return it.

```erlang
find(Key, Item = #tx { id = Key }) -> Item;
```

### find

Find an item in a bundle-map/list and return it.

```erlang
find(Key, Item = #tx { data = Data }) ->
    case id(Item, unsigned) of
        Key -> Item;
        _ ->
            case is_binary(Data) of
                false -> find(Key, Data);
                true -> not_found
            end
    end;
```

### find

Find an item in a bundle-map/list and return it.

```erlang
find(_Key, _) ->
    not_found.
```

### manifest_item

Return the manifest item in a bundle-map/list.

```erlang
manifest_item(#tx { manifest = Manifest }) when is_record(Manifest, tx) ->
    Manifest;
```

### manifest_item

Return the manifest item in a bundle-map/list.
Create a new data item. Should only be used for testing.

```erlang
manifest_item(_Item) -> undefined.
```

### new_item

Return the manifest item in a bundle-map/list.
Create a new data item. Should only be used for testing.

```erlang
new_item(Target, Anchor, Tags, Data) ->
    reset_ids(
        #tx{
            format = ans104,
            target = Target,
            anchor = Anchor,
            tags = Tags,
            data = Data,
            data_size = byte_size(Data)
        }
    ).
```

### sign_item

Sign a data item.

```erlang
sign_item(_, undefined) -> throw(wallet_not_found);
```

### sign_item

Sign a data item.

```erlang
sign_item(RawItem, {PrivKey, {KeyType, Owner}}) ->
    Item = (normalize_data(RawItem))#tx{format = ans104, owner = Owner, signature_type = KeyType},
    % Generate the signature from the data item's data segment in 'signed'-ready mode.
```

### verify_item

Verify the validity of a data item.

```erlang
verify_item(DataItem) ->
    ValidID = verify_data_item_id(DataItem),
    ValidSignature = verify_data_item_signature(DataItem),
    ValidTags = verify_data_item_tags(DataItem),
    ValidID andalso ValidSignature andalso ValidTags.
```

### type

```erlang
type(Item) when is_record(Item, tx) ->
    case lists:keyfind(<<"bundle-map">>, 1, Item#tx.tags) of
        {<<"bundle-map">>, _} ->
            case lists:keyfind(<<"map-format">>, 1, Item#tx.tags) of
                {<<"map-format">>, <<"list">>} -> list;
                _ -> map
            end;
        _ ->
            binary
    end;
```

### type

```erlang
type(Data) when erlang:is_map(Data) ->
    map;
```

### type

```erlang
type(Data) when erlang:is_list(Data) ->
    list;
```

### type

```erlang
type(_) ->
    binary.
```

### data_item_signature_data

Generate the data segment to be signed for a data item.

```erlang
data_item_signature_data(RawItem) ->
    data_item_signature_data(RawItem, signed).
```

### data_item_signature_data

```erlang
data_item_signature_data(RawItem, unsigned) ->
    data_item_signature_data(RawItem#tx { owner = ?DEFAULT_OWNER }, signed);
```

### data_item_signature_data

```erlang
data_item_signature_data(RawItem, signed) ->
    true = enforce_valid_tx(RawItem),
    NormItem = normalize_data(RawItem),
    ar_deep_hash:hash([
        utf8_encoded("dataitem"),
        utf8_encoded("1"),
        %% Only SignatureType 1 is supported for now (RSA 4096)
        utf8_encoded("1"),
        <<(NormItem#tx.owner)/binary>>,
        <<(NormItem#tx.target)/binary>>,
        <<(NormItem#tx.anchor)/binary>>,
        encode_tags(NormItem#tx.tags),
        <<(NormItem#tx.data)/binary>>
    ]).
```

### verify_data_item_id

Verify the data item's ID matches the signature.

```erlang
verify_data_item_id(DataItem) ->
    ExpectedID = crypto:hash(sha256, DataItem#tx.signature),
    DataItem#tx.id == ExpectedID.
```

### verify_data_item_signature

Verify the data item's signature.

```erlang
verify_data_item_signature(DataItem) ->
    SignatureData = data_item_signature_data(DataItem),
    %?event({unsigned_id, hb_util:encode(id(DataItem, unsigned)), hb_util:encode(SignatureData)}),
    ar_wallet:verify(
        {DataItem#tx.signature_type, DataItem#tx.owner}, SignatureData, DataItem#tx.signature
    ).
```

### verify_data_item_tags

Verify the validity of the data item's tags.

```erlang
verify_data_item_tags(DataItem) ->
    ValidCount = length(DataItem#tx.tags) =< 128,
    ValidTags = lists:all(
        fun({Name, Value}) ->
            byte_size(Name) =< 1024 andalso byte_size(Value) =< 3072
        end,
        DataItem#tx.tags
    ),
    ValidCount andalso ValidTags.
```

### normalize

Ensure that a data item (potentially containing a map or list) has a

```erlang
normalize(Item) -> reset_ids(normalize_data(Item)).
```

### normalize_data

Ensure that a data item (potentially containing a map or list) has a

```erlang
normalize_data(not_found) -> throw(not_found);
```

### normalize_data

Ensure that a data item (potentially containing a map or list) has a

```erlang
normalize_data(Item = #tx{data = Bin}) when is_binary(Bin) ->
    ?event({normalize_data, binary, Item}),
    normalize_data_size(Item);
```

### normalize_data

Ensure that a data item (potentially containing a map or list) has a

```erlang
normalize_data(Bundle) when is_list(Bundle); is_map(Bundle) ->
    ?event({normalize_data, bundle, Bundle}),
    normalize_data(#tx{ data = Bundle });
```

### normalize_data

Ensure that a data item (potentially containing a map or list) has a

```erlang
normalize_data(Item = #tx { data = Data }) when is_list(Data) ->
    ?event({normalize_data, list, Item}),
    normalize_data(
        Item#tx{
            tags = add_list_tags(Item#tx.tags),
            data =
                maps:from_list(
                    lists:zipwith(
                        fun(Index, MapItem) ->
                            {
                                integer_to_binary(Index),
                                update_ids(normalize_data(MapItem))
                            }
                        end,
                        lists:seq(1, length(Data)),
                        Data
                    )
                )
        }
    );
```

### normalize_data

Ensure that a data item (potentially containing a map or list) has a

```erlang
normalize_data(Item = #tx{data = Data}) ->
    ?event({normalize_data, map, Item}),
    normalize_data_size(
        case serialize_bundle_data(Data, Item#tx.manifest) of
            {Manifest, Bin} ->
                Item#tx{
                    data = Bin,
                    manifest = Manifest,
                    tags =
                        add_manifest_tags(
                            add_bundle_tags(Item#tx.tags),
                            id(Manifest, unsigned)
                        )
                };
            DirectBin ->
                Item#tx{
                    data = DirectBin,
                    tags = add_bundle_tags(Item#tx.tags)
                }
        end
    ).
```

### normalize_data_size

Reset the data size of a data item. Assumes that the data is already normalized.

```erlang
normalize_data_size(Item = #tx{data = Bin}) when is_binary(Bin) ->
    Item#tx{data_size = byte_size(Bin)};
```

### normalize_data_size

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.

```erlang
normalize_data_size(Item) -> Item.
```

### serialize

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.

```erlang
serialize(not_found) -> throw(not_found);
```

### serialize

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.

```erlang
serialize(TX) -> serialize(TX, binary).
```

### serialize

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.

```erlang
serialize(TX, binary) when is_binary(TX) -> TX;
```

### serialize

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.

```erlang
serialize(RawTX, binary) ->
    true = enforce_valid_tx(RawTX),
    TX = normalize(RawTX),
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
```

### serialize

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.
Take an item and ensure that it is of valid form. Useful for ensuring

```erlang
serialize(TX, json) ->
    true = enforce_valid_tx(TX),
    hb_json:encode(hb_message:convert(TX, <<"ans104@1.0">>, #{})).
```

### enforce_valid_tx

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.
Take an item and ensure that it is of valid form. Useful for ensuring

```erlang
enforce_valid_tx(List) when is_list(List) ->
    lists:all(fun enforce_valid_tx/1, List);
```

### enforce_valid_tx

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.
Take an item and ensure that it is of valid form. Useful for ensuring

```erlang
enforce_valid_tx(Map) when is_map(Map) ->
    lists:all(fun(Item) -> enforce_valid_tx(Item) end, maps:values(Map));
```

### enforce_valid_tx

Reset the data size of a data item. Assumes that the data is already normalized.
Convert a #tx record to its binary representation.
Take an item and ensure that it is of valid form. Useful for ensuring

```erlang
enforce_valid_tx(TX) ->
    ok_or_throw(TX,
        check_type(TX, message),
        {invalid_tx, TX}
    ),
    ok_or_throw(TX,
        check_size(TX#tx.id, [0, 32]),
        {invalid_field, id, TX#tx.id}
    ),
    ok_or_throw(TX,
        check_size(TX#tx.unsigned_id, [0, 32]),
        {invalid_field, unsigned_id, TX#tx.unsigned_id}
    ),
    ok_or_throw(TX,
        check_size(TX#tx.anchor, [0, 32]),
        {invalid_field, last_tx, TX#tx.anchor}
    ),
    ok_or_throw(TX,
        check_size(TX#tx.owner, [0, byte_size(?DEFAULT_OWNER)]),
        {invalid_field, owner, TX#tx.owner}
    ),
    ok_or_throw(TX,
        check_size(TX#tx.target, [0, 32]),
        {invalid_field, target, TX#tx.target}
    ),
    ok_or_throw(TX,
        check_size(TX#tx.signature, [0, 65, byte_size(?DEFAULT_SIG)]),
        {invalid_field, signature, TX#tx.signature}
    ),
    ok_or_throw(TX,
        check_type(TX#tx.tags, list),
        {invalid_field, tags, TX#tx.tags}
    ),
    lists:foreach(
        fun({Name, Value}) ->
            ok_or_throw(TX,
                check_type(Name, binary),
                {invalid_field, tag_name, Name}
            ),
            ok_or_throw(TX,
                check_size(Name, {range, 0, ?MAX_TAG_NAME_SIZE}),
                {invalid_field, tag_name, Name}
            ),
            ok_or_throw(TX,
                check_type(Value, binary),
                {invalid_field, tag_value, {Name, Value}}
            ),
            ok_or_throw(TX,
                check_size(Value, {range, 0, ?MAX_TAG_VALUE_SIZE}),
                {invalid_field, tag_value, {Name, Value}}
            );
            (InvalidTagForm) ->
                throw({invalid_field, tag, InvalidTagForm})
        end,
        TX#tx.tags
    ),
    ok_or_throw(
        TX,
        check_type(TX#tx.data, binary)
            orelse check_type(TX#tx.data, map)
            orelse check_type(TX#tx.data, list),
        {invalid_field, data, TX#tx.data}
    ),
    true.
```

### check_size

Force that a binary is either empty or the given number of bytes.

```erlang
check_size(Bin, {range, Start, End}) ->
    check_type(Bin, binary)
        andalso byte_size(Bin) >= Start
        andalso byte_size(Bin) =< End;
```

### check_size

Force that a binary is either empty or the given number of bytes.

```erlang
check_size(Bin, Sizes) ->
    check_type(Bin, binary)
        andalso lists:member(byte_size(Bin), Sizes).
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, binary) when is_binary(Value) -> true;
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, _) when is_binary(Value) -> false;
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, list) when is_list(Value) -> true;
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, _) when is_list(Value) -> false;
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, map) when is_map(Value) -> true;
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, _) when is_map(Value) -> false;
```

### check_type

Ensure that a value is of the given type.

```erlang
check_type(Value, message) ->
    is_record(Value, tx) or is_map(Value) or is_list(Value);
```

### check_type

Ensure that a value is of the given type.
Throw an error if the given value is not ok.

```erlang
check_type(_Value, _) -> false.
```

### ok_or_throw

Ensure that a value is of the given type.
Throw an error if the given value is not ok.

```erlang
ok_or_throw(_, true, _) -> true;
```

### ok_or_throw

Ensure that a value is of the given type.
Throw an error if the given value is not ok.

```erlang
ok_or_throw(_TX, false, Error) ->
    throw(Error).
```

### update_ids

Take an item and ensure that both the unsigned and signed IDs are

```erlang
update_ids(Item = #tx { unsigned_id = ?DEFAULT_ID }) ->
    update_ids(
        Item#tx {
            unsigned_id =
                crypto:hash(
                    sha256,
                    data_item_signature_data(Item, unsigned)
                )
        }
    );
```

### update_ids

Take an item and ensure that both the unsigned and signed IDs are

```erlang
update_ids(Item = #tx { id = ?DEFAULT_ID, signature = ?DEFAULT_SIG }) ->
    Item;
```

### update_ids

Take an item and ensure that both the unsigned and signed IDs are

```erlang
update_ids(Item = #tx { signature = ?DEFAULT_SIG }) ->
    Item#tx { id = ?DEFAULT_ID };
```

### update_ids

Take an item and ensure that both the unsigned and signed IDs are

```erlang
update_ids(Item = #tx { signature = Sig }) when Sig =/= ?DEFAULT_SIG ->
    Item#tx { id = crypto:hash(sha256, Sig) };
```

### update_ids

Take an item and ensure that both the unsigned and signed IDs are
Re-calculate both of the IDs for an item. This is a wrapper

```erlang
update_ids(TX) -> TX.
```

### reset_ids

Take an item and ensure that both the unsigned and signed IDs are
Re-calculate both of the IDs for an item. This is a wrapper

```erlang
reset_ids(Item) ->
    update_ids(Item#tx { unsigned_id = ?DEFAULT_ID, id = ?DEFAULT_ID }).
```

### add_bundle_tags

```erlang
add_bundle_tags(Tags) -> ?BUNDLE_TAGS ++ (Tags -- ?BUNDLE_TAGS).
```

### add_list_tags

```erlang
add_list_tags(Tags) ->
    (?BUNDLE_TAGS ++ (Tags -- ?BUNDLE_TAGS)) ++ ?LIST_TAGS.
```

### add_manifest_tags

```erlang
add_manifest_tags(Tags, ManifestID) ->
    lists:filter(
        fun
            ({<<"bundle-map">>, _}) -> false;
            (_) -> true
        end,
        Tags
    ) ++ [{<<"bundle-map">>, hb_util:encode(ManifestID)}].
```

### finalize_bundle_data

```erlang
finalize_bundle_data(Processed) ->
    Length = <<(length(Processed)):256/integer>>,
    Index = <<<<(byte_size(Data)):256/integer, ID/binary>> || {ID, Data} <- Processed>>,
    Items = <<<<Data/binary>> || {_, Data} <- Processed>>,
    <<Length/binary, Index/binary, Items/binary>>.
```

### to_serialized_pair

```erlang
to_serialized_pair(Item) when is_binary(Item) ->
    % Support bundling of bare binary payloads by wrapping them in a TX that
    % is explicitly marked as a binary data item.
```

### to_serialized_pair

```erlang
to_serialized_pair(Item) ->
    % TODO: This is a hack to get the ID of the item. We need to do this because we may not
    % have the ID in 'item' if it is just a map/list. We need to make this more efficient.
```

### serialize_bundle_data

```erlang
serialize_bundle_data(Map, _Manifest) when is_map(Map) ->
    % TODO: Make this compatible with the normal manifest spec.
```

### serialize_bundle_data

```erlang
serialize_bundle_data(List, _Manifest) when is_list(List) ->
    finalize_bundle_data(lists:map(fun to_serialized_pair/1, List));
```

### serialize_bundle_data

```erlang
serialize_bundle_data(Data, _Manifest) ->
    throw({cannot_serialize_tx_data, must_be_map_or_list, Data}).
```

### new_manifest

```erlang
new_manifest(Index) ->
    TX = normalize(#tx{
        format = ans104,
        tags = [
            {<<"data-protocol">>, <<"bundle-map">>},
            {<<"variant">>, <<"0.0.1">>}
        ],
        data = hb_json:encode(Index)
    }),
    TX.
```

### manifest

```erlang
manifest(Map) when is_map(Map) -> Map;
```

### manifest

```erlang
manifest(#tx { manifest = undefined }) -> undefined;
```

### manifest

```erlang
manifest(#tx { manifest = ManifestTX }) ->
    hb_json:decode(ManifestTX#tx.data).
```

### parse_manifest

```erlang
parse_manifest(Item) when is_record(Item, tx) ->
    parse_manifest(Item#tx.data);
```

### parse_manifest

```erlang
parse_manifest(Bin) ->
    hb_json:decode(Bin).
```

### encode_signature_type

Only RSA 4096 is currently supported.

```erlang
encode_signature_type({rsa, 65537}) ->
    <<1, 0>>;
```

### encode_signature_type

Only RSA 4096 is currently supported.

```erlang
encode_signature_type(_) ->
    unsupported_tx_format.
```

### encode_optional_field

Encode an optional field (target, anchor) with a presence byte.

```erlang
encode_optional_field(<<>>) ->
    <<0>>;
```

### encode_optional_field

Encode an optional field (target, anchor) with a presence byte.

```erlang
encode_optional_field(Field) ->
    <<1:8/integer, Field/binary>>.
```

### utf8_encoded

Encode a UTF-8 string to binary.

```erlang
utf8_encoded(String) ->
    unicode:characters_to_binary(String, utf8).
```

### encode_tags_size

```erlang
encode_tags_size([], <<>>) ->
    <<0:64/little-integer, 0:64/little-integer>>;
```

### encode_tags_size

```erlang
encode_tags_size(Tags, EncodedTags) ->
    <<(length(Tags)):64/little-integer, (byte_size(EncodedTags)):64/little-integer>>.
```

### encode_tags

Encode tags into a binary format using Apache Avro.

```erlang
encode_tags([]) ->
    <<>>;
```

### encode_tags

Encode tags into a binary format using Apache Avro.

```erlang
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
```

### encode_avro_name

Encode a string for Avro using ZigZag and VInt encoding.

```erlang
encode_avro_name(<<>>) ->
    % Zero length names are treated as a special case, due to the Avro encoder.
```

### encode_avro_name

```erlang
encode_avro_name(String) ->
    StringBytes = utf8_encoded(String),
    Length = byte_size(StringBytes),
    <<(encode_zigzag(Length))/binary, StringBytes/binary>>.
```

### encode_avro_value

```erlang
encode_avro_value(<<>>) ->
    % Zero length values are treated as a special case, due to the Avro encoder.
```

### encode_avro_value

```erlang
encode_avro_value(Value) when is_binary(Value) ->
    % Tag values can be raw binaries
    Length = byte_size(Value),
    <<(encode_zigzag(Length))/binary, Value/binary>>.
```

### encode_zigzag

Encode an integer using ZigZag encoding.

```erlang
encode_zigzag(Int) when Int >= 0 ->
    encode_vint(Int bsl 1);
```

### encode_zigzag

Encode an integer using ZigZag encoding.

```erlang
encode_zigzag(Int) ->
    encode_vint(Int bsl 1, -1).
```

### encode_vint

Encode a ZigZag integer to VInt binary format.

```erlang
encode_vint(ZigZag) ->
    encode_vint(ZigZag, []).
```

### encode_vint

```erlang
encode_vint(0, Acc) ->
    list_to_binary(lists:reverse(Acc));
```

### encode_vint

```erlang
encode_vint(ZigZag, Acc) ->
    VIntByte = ZigZag band 16#7F,
    ZigZagShifted = ZigZag bsr 7,
    case ZigZagShifted of
        0 -> encode_vint(0, [VIntByte | Acc]);
        _ -> encode_vint(ZigZagShifted, [VIntByte bor 16#80 | Acc])
    end.
```

### deserialize

Convert binary data back to a #tx record.

```erlang
deserialize(not_found) -> throw(not_found);
```

### deserialize

Convert binary data back to a #tx record.

```erlang
deserialize(Binary) -> deserialize(Binary, binary).
```

### deserialize

Convert binary data back to a #tx record.

```erlang
deserialize(Item, binary) when is_record(Item, tx) ->
    maybe_unbundle(Item);
```

### deserialize

Convert binary data back to a #tx record.

```erlang
deserialize(Binary, binary) ->
    %try
    {SignatureType, Signature, Owner, Rest} = decode_signature(Binary),
    {Target, Rest2} = decode_optional_field(Rest),
    {Anchor, Rest3} = decode_optional_field(Rest2),
    {Tags, Data} = decode_tags(Rest3),
    maybe_unbundle(
        reset_ids(#tx{
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
    );
%catch
%    _:_:_Stack ->
%        {error, invalid_item}
%end;
```

### deserialize

Convert binary data back to a #tx record.

```erlang
deserialize(Bin, json) ->
    try
        Map = hb_json:decode(Bin),
        hb_message:convert(Map, <<"ans104@1.0">>, #{})
    catch
        _:_:_Stack ->
            {error, invalid_item}
    end.
```

### maybe_unbundle

```erlang
maybe_unbundle(Item) ->
    Format = lists:keyfind(<<"bundle-format">>, 1, Item#tx.tags),
    Version = lists:keyfind(<<"bundle-version">>, 1, Item#tx.tags),
    case {Format, Version} of
        {{<<"bundle-format">>, <<"binary">>}, {<<"bundle-version">>, <<"2.0.0">>}} ->
            maybe_map_to_list(maybe_unbundle_map(Item));
        _ ->
            Item
    end.
```

### maybe_map_to_list

```erlang
maybe_map_to_list(Item) ->
    case lists:keyfind(<<"map-format">>, 1, Item#tx.tags) of
        {<<"map-format">>, <<"List">>} ->
            unbundle_list(Item);
        _ ->
            Item
    end.
```

### unbundle_list

```erlang
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
```

### maybe_unbundle_map

```erlang
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
```

### find_single_layer

An internal helper for finding an item in a single-layer of a bundle.

```erlang
find_single_layer(UnsignedID, TX) when is_record(TX, tx) ->
    find_single_layer(UnsignedID, TX#tx.data);
```

### find_single_layer

An internal helper for finding an item in a single-layer of a bundle.

```erlang
find_single_layer(UnsignedID, Items) ->
    TX = lists:keyfind(UnsignedID, #tx.unsigned_id, Items),
    case is_record(TX, tx) of
        true -> TX;
        false ->
            throw({cannot_find_item, hb_util:encode(UnsignedID)})
    end.
```

### unbundle

```erlang
unbundle(Item = #tx{data = <<Count:256/integer, Content/binary>>}) ->
    {ItemsBin, Items} = decode_bundle_header(Count, Content),
    Item#tx{data = decode_bundle_items(Items, ItemsBin)};
```

### unbundle

```erlang
unbundle(#tx{data = <<>>}) -> detached.
```

### decode_bundle_items

```erlang
decode_bundle_items([], <<>>) ->
    [];
```

### decode_bundle_items

```erlang
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
```

### decode_bundle_header

```erlang
decode_bundle_header(Count, Bin) -> decode_bundle_header(Count, Bin, []).
```

### decode_bundle_header

```erlang
decode_bundle_header(0, ItemsBin, Header) ->
    {ItemsBin, lists:reverse(Header)};
```

### decode_bundle_header

```erlang
decode_bundle_header(Count, <<Size:256/integer, ID:32/binary, Rest/binary>>, Header) ->
    decode_bundle_header(Count - 1, Rest, [{ID, Size} | Header]).
```

### decode_signature

Decode the signature from a binary format. Only RSA 4096 is currently supported.

```erlang
decode_signature(<<1, 0, Signature:512/binary, Owner:512/binary, Rest/binary>>) ->
    {{rsa, 65537}, Signature, Owner, Rest};
```

### decode_signature

Decode the signature from a binary format. Only RSA 4096 is currently supported.

```erlang
decode_signature(Other) ->
    ?event({error_decoding_signature, Other}),
    unsupported_tx_format.
```

### decode_tags

Decode tags from a binary format using Apache Avro.

```erlang
decode_tags(<<0:64/little-integer, 0:64/little-integer, Rest/binary>>) ->
    {[], Rest};
```

### decode_tags

Decode tags from a binary format using Apache Avro.

```erlang
decode_tags(<<_TagCount:64/little-integer, _TagSize:64/little-integer, Binary/binary>>) ->
    {Count, BlocksBinary} = decode_zigzag(Binary),
    {Tags, Rest} = decode_avro_tags(BlocksBinary, Count),
    %% Pull out the terminating zero
    {0, Rest2} = decode_zigzag(Rest),
    {Tags, Rest2}.
```

### decode_optional_field

```erlang
decode_optional_field(<<0, Rest/binary>>) ->
    {<<>>, Rest};
```

### decode_optional_field

```erlang
decode_optional_field(<<1:8/integer, Field:32/binary, Rest/binary>>) ->
    {Field, Rest}.
```

### decode_avro_tags

Decode Avro blocks (for tags) from binary.

```erlang
decode_avro_tags(<<>>, _) ->
    {[], <<>>};
```

### decode_avro_tags

Decode Avro blocks (for tags) from binary.

```erlang
decode_avro_tags(Binary, Count) when Count =:= 0 ->
    {[], Binary};
```

### decode_avro_tags

Decode Avro blocks (for tags) from binary.

```erlang
decode_avro_tags(Binary, Count) ->
    {NameSize, Rest} = decode_zigzag(Binary),
    decode_avro_name(NameSize, Rest, Count).
```

### decode_avro_name

```erlang
decode_avro_name(0, Rest, _) ->
    {[], Rest};
```

### decode_avro_name

```erlang
decode_avro_name(NameSize, Rest, Count) ->
    <<Name:NameSize/binary, Rest2/binary>> = Rest,
    {ValueSize, Rest3} = decode_zigzag(Rest2),
    decode_avro_value(ValueSize, Name, Rest3, Count).
```

### decode_avro_value

```erlang
decode_avro_value(0, Name, Rest, Count) ->
    {DecodedTags, NonAvroRest} = decode_avro_tags(Rest, Count - 1),
    {[{Name, <<>>} | DecodedTags], NonAvroRest};
```

### decode_avro_value

```erlang
decode_avro_value(ValueSize, Name, Rest, Count) ->
    <<Value:ValueSize/binary, Rest2/binary>> = Rest,
    {DecodedTags, NonAvroRest} = decode_avro_tags(Rest2, Count - 1),
    {[{Name, Value} | DecodedTags], NonAvroRest}.
```

### decode_zigzag

Decode a VInt encoded ZigZag integer from binary.

```erlang
decode_zigzag(Binary) ->
    {ZigZag, Rest} = decode_vint(Binary, 0, 0),
    case ZigZag band 1 of
        1 -> {-(ZigZag bsr 1) - 1, Rest};
        0 -> {ZigZag bsr 1, Rest}
    end.
```

### decode_vint

```erlang
decode_vint(<<>>, Result, _Shift) ->
    {Result, <<>>};
```

### decode_vint

```erlang
decode_vint(<<Byte, Rest/binary>>, Result, Shift) ->
    VIntPart = Byte band 16#7F,
    NewResult = Result bor (VIntPart bsl Shift),
    case Byte band 16#80 of
        0 -> {NewResult, Rest};
        _ -> decode_vint(Rest, NewResult, Shift + 7)
    end.
```

### ar_bundles_test_

```erlang
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
        {timeout, 30, fun test_serialize_deserialize_deep_signed_bundle/0},
        {timeout, 30, fun test_encode_tags/0}
    ].
```

### test_encode_tags

```erlang
test_encode_tags() ->
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
```

### run_test

```erlang
run_test() ->
    test_with_zero_length_tag().
```

### test_no_tags

```erlang
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
```

### test_with_tags

```erlang
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
```

### test_with_zero_length_tag

```erlang
test_with_zero_length_tag() ->
    Item = normalize(#tx{
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
```

### test_unsigned_data_item_id

```erlang
test_unsigned_data_item_id() ->
    Item1 = deserialize(
        serialize(reset_ids(#tx{format = ans104, data = <<"data1">>}))
    ),
    Item2 = deserialize(
        serialize(reset_ids(#tx{format = ans104, data = <<"data2">>}))),
    ?assertNotEqual(Item1#tx.unsigned_id, Item2#tx.unsigned_id).
```

### test_unsigned_data_item_normalization

```erlang
test_unsigned_data_item_normalization() ->
    NewItem = normalize(#tx{ format = ans104, data = <<"Unsigned data">> }),
    ReNormItem = deserialize(serialize(NewItem)),
    ?assertEqual(NewItem, ReNormItem).
```

### assert_data_item

```erlang
assert_data_item(KeyType, Owner, Target, Anchor, Tags, Data, DataItem) ->
    ?assertEqual(KeyType, DataItem#tx.signature_type),
    ?assertEqual(Owner, DataItem#tx.owner),
    ?assertEqual(Target, DataItem#tx.target),
    ?assertEqual(Anchor, DataItem#tx.anchor),
    ?assertEqual(Tags, DataItem#tx.tags),
    ?assertEqual(Data, DataItem#tx.data),
    ?assertEqual(byte_size(Data), DataItem#tx.data_size).
```

### test_empty_bundle

```erlang
test_empty_bundle() ->
    Bundle = serialize([]),
    BundleItem = deserialize(Bundle),
    ?assertEqual(#{}, BundleItem#tx.data).
```

### test_bundle_with_one_item

```erlang
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
```

### test_bundle_with_two_items

```erlang
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
```

### test_recursive_bundle

```erlang
test_recursive_bundle() ->
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
    Bundle = serialize([Item3]),
    BundleItem = deserialize(Bundle),
    #{<<"1">> := UnbundledItem3} = BundleItem#tx.data,
    #{<<"1">> := UnbundledItem2} = UnbundledItem3#tx.data,
    #{<<"1">> := UnbundledItem1} = UnbundledItem2#tx.data,
    ?assert(verify_item(UnbundledItem1)),
    % TODO: Verify bundled lists...
```

### test_bundle_map

```erlang
test_bundle_map() ->
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
    Bundle = serialize(Item2),
    BundleItem = deserialize(Bundle),
    ?assertEqual(Item1#tx.data, (maps:get(<<"key1">>, BundleItem#tx.data))#tx.data),
    ?assert(verify_item(BundleItem)).
```

### test_extremely_large_bundle

```erlang
test_extremely_large_bundle() ->
    W = ar_wallet:new(),
    Data = crypto:strong_rand_bytes(100_000_000),
    Norm = normalize(#tx { data = #{ <<"key">> => #tx { data = Data } } }),
    Signed = sign_item(Norm, W),
    Serialized = serialize(Signed),
    Deserialized = deserialize(Serialized),
    ?assert(verify_item(Deserialized)).
```

### test_basic_member_id

```erlang
test_basic_member_id() ->
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
```

### test_deep_member

```erlang
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
    ?assertEqual(true, member(id(Item, unsigned), Item2)),
    ?assertEqual(true, member(id(Item2, unsigned), Item2)),
    ?assertEqual(false, member(crypto:strong_rand_bytes(32), Item2)).
```

### test_serialize_deserialize_deep_signed_bundle

```erlang
test_serialize_deserialize_deep_signed_bundle() ->
    W = ar_wallet:new(),
    % Test that we can serialize, deserialize, and get the same IDs back.
```

---

*Generated from [ar_bundles.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/ar_bundles.erl)*
