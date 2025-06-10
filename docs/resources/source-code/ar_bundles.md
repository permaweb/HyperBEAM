# [Module ar_bundles.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/ar_bundles.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_bundle_tags-1">add_bundle_tags/1*</a></td><td></td></tr><tr><td valign="top"><a href="#add_list_tags-1">add_list_tags/1*</a></td><td></td></tr><tr><td valign="top"><a href="#add_manifest_tags-2">add_manifest_tags/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ar_bundles_test_-0">ar_bundles_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#assert_data_item-7">assert_data_item/7*</a></td><td></td></tr><tr><td valign="top"><a href="#check_size-2">check_size/2*</a></td><td>Force that a binary is either empty or the given number of bytes.</td></tr><tr><td valign="top"><a href="#check_type-2">check_type/2*</a></td><td>Ensure that a value is of the given type.</td></tr><tr><td valign="top"><a href="#data_item_signature_data-1">data_item_signature_data/1</a></td><td>Generate the data segment to be signed for a data item.</td></tr><tr><td valign="top"><a href="#data_item_signature_data-2">data_item_signature_data/2*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_avro_name-3">decode_avro_name/3*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_avro_tags-2">decode_avro_tags/2*</a></td><td>Decode Avro blocks (for tags) from binary.</td></tr><tr><td valign="top"><a href="#decode_avro_value-4">decode_avro_value/4*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_bundle_header-2">decode_bundle_header/2*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_bundle_header-3">decode_bundle_header/3*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_bundle_items-2">decode_bundle_items/2*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_optional_field-1">decode_optional_field/1*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_signature-1">decode_signature/1*</a></td><td>Decode the signature from a binary format.</td></tr><tr><td valign="top"><a href="#decode_tags-1">decode_tags/1</a></td><td>Decode tags from a binary format using Apache Avro.</td></tr><tr><td valign="top"><a href="#decode_vint-3">decode_vint/3*</a></td><td></td></tr><tr><td valign="top"><a href="#decode_zigzag-1">decode_zigzag/1*</a></td><td>Decode a VInt encoded ZigZag integer from binary.</td></tr><tr><td valign="top"><a href="#deserialize-1">deserialize/1</a></td><td>Convert binary data back to a #tx record.</td></tr><tr><td valign="top"><a href="#deserialize-2">deserialize/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode_avro_string-1">encode_avro_string/1*</a></td><td>Encode a string for Avro using ZigZag and VInt encoding.</td></tr><tr><td valign="top"><a href="#encode_optional_field-1">encode_optional_field/1*</a></td><td>Encode an optional field (target, anchor) with a presence byte.</td></tr><tr><td valign="top"><a href="#encode_signature_type-1">encode_signature_type/1*</a></td><td>Only RSA 4096 is currently supported.</td></tr><tr><td valign="top"><a href="#encode_tags-1">encode_tags/1</a></td><td>Encode tags into a binary format using Apache Avro.</td></tr><tr><td valign="top"><a href="#encode_tags_size-2">encode_tags_size/2*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_vint-1">encode_vint/1*</a></td><td>Encode a ZigZag integer to VInt binary format.</td></tr><tr><td valign="top"><a href="#encode_vint-2">encode_vint/2*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_zigzag-1">encode_zigzag/1*</a></td><td>Encode an integer using ZigZag encoding.</td></tr><tr><td valign="top"><a href="#enforce_valid_tx-1">enforce_valid_tx/1*</a></td><td>Take an item and ensure that it is of valid form.</td></tr><tr><td valign="top"><a href="#finalize_bundle_data-1">finalize_bundle_data/1*</a></td><td></td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>Find an item in a bundle-map/list and return it.</td></tr><tr><td valign="top"><a href="#find_single_layer-2">find_single_layer/2*</a></td><td>An internal helper for finding an item in a single-layer of a bundle.</td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td></td></tr><tr><td valign="top"><a href="#format-2">format/2</a></td><td></td></tr><tr><td valign="top"><a href="#format_binary-1">format_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#format_data-2">format_data/2*</a></td><td></td></tr><tr><td valign="top"><a href="#format_line-2">format_line/2*</a></td><td></td></tr><tr><td valign="top"><a href="#format_line-3">format_line/3*</a></td><td></td></tr><tr><td valign="top"><a href="#hd-1">hd/1</a></td><td>Return the first item in a bundle-map/list.</td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Return the ID of an item -- either signed or unsigned as specified.</td></tr><tr><td valign="top"><a href="#id-2">id/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_signed-1">is_signed/1</a></td><td>Check if an item is signed.</td></tr><tr><td valign="top"><a href="#manifest-1">manifest/1</a></td><td></td></tr><tr><td valign="top"><a href="#manifest_item-1">manifest_item/1</a></td><td>Return the manifest item in a bundle-map/list.</td></tr><tr><td valign="top"><a href="#map-1">map/1</a></td><td>Convert an item containing a map or list into an Erlang map.</td></tr><tr><td valign="top"><a href="#maybe_map_to_list-1">maybe_map_to_list/1*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_unbundle-1">maybe_unbundle/1*</a></td><td></td></tr><tr><td valign="top"><a href="#maybe_unbundle_map-1">maybe_unbundle_map/1*</a></td><td></td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td>Check if an item exists in a bundle-map/list.</td></tr><tr><td valign="top"><a href="#new_item-4">new_item/4</a></td><td>Create a new data item.</td></tr><tr><td valign="top"><a href="#new_manifest-1">new_manifest/1*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize-1">normalize/1</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_data-1">normalize_data/1*</a></td><td>Ensure that a data item (potentially containing a map or list) has a standard, serialized form.</td></tr><tr><td valign="top"><a href="#normalize_data_size-1">normalize_data_size/1*</a></td><td>Reset the data size of a data item.</td></tr><tr><td valign="top"><a href="#ok_or_throw-3">ok_or_throw/3*</a></td><td>Throw an error if the given value is not ok.</td></tr><tr><td valign="top"><a href="#parse_manifest-1">parse_manifest/1</a></td><td></td></tr><tr><td valign="top"><a href="#print-1">print/1</a></td><td></td></tr><tr><td valign="top"><a href="#reset_ids-1">reset_ids/1</a></td><td>Re-calculate both of the IDs for an item.</td></tr><tr><td valign="top"><a href="#run_test-0">run_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#serialize-1">serialize/1</a></td><td>Convert a #tx record to its binary representation.</td></tr><tr><td valign="top"><a href="#serialize-2">serialize/2</a></td><td></td></tr><tr><td valign="top"><a href="#serialize_bundle_data-2">serialize_bundle_data/2*</a></td><td></td></tr><tr><td valign="top"><a href="#sign_item-2">sign_item/2</a></td><td>Sign a data item.</td></tr><tr><td valign="top"><a href="#signer-1">signer/1</a></td><td>Return the address of the signer of an item, if it is signed.</td></tr><tr><td valign="top"><a href="#test_basic_member_id-0">test_basic_member_id/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_bundle_map-0">test_bundle_map/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_bundle_with_one_item-0">test_bundle_with_one_item/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_bundle_with_two_items-0">test_bundle_with_two_items/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_deep_member-0">test_deep_member/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_empty_bundle-0">test_empty_bundle/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_extremely_large_bundle-0">test_extremely_large_bundle/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_no_tags-0">test_no_tags/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_recursive_bundle-0">test_recursive_bundle/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_serialize_deserialize_deep_signed_bundle-0">test_serialize_deserialize_deep_signed_bundle/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_unsigned_data_item_id-0">test_unsigned_data_item_id/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_unsigned_data_item_normalization-0">test_unsigned_data_item_normalization/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_with_tags-0">test_with_tags/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_with_zero_length_tag-0">test_with_zero_length_tag/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to_serialized_pair-1">to_serialized_pair/1*</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr><tr><td valign="top"><a href="#unbundle-1">unbundle/1*</a></td><td></td></tr><tr><td valign="top"><a href="#unbundle_list-1">unbundle_list/1*</a></td><td></td></tr><tr><td valign="top"><a href="#update_ids-1">update_ids/1*</a></td><td>Take an item and ensure that both the unsigned and signed IDs are
appropriately set.</td></tr><tr><td valign="top"><a href="#utf8_encoded-1">utf8_encoded/1*</a></td><td>Encode a UTF-8 string to binary.</td></tr><tr><td valign="top"><a href="#verify_data_item_id-1">verify_data_item_id/1*</a></td><td>Verify the data item's ID matches the signature.</td></tr><tr><td valign="top"><a href="#verify_data_item_signature-1">verify_data_item_signature/1*</a></td><td>Verify the data item's signature.</td></tr><tr><td valign="top"><a href="#verify_data_item_tags-1">verify_data_item_tags/1*</a></td><td>Verify the validity of the data item's tags.</td></tr><tr><td valign="top"><a href="#verify_item-1">verify_item/1</a></td><td>Verify the validity of a data item.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_bundle_tags-1"></a>

### add_bundle_tags/1 * ###

`add_bundle_tags(Tags) -> any()`

<a name="add_list_tags-1"></a>

### add_list_tags/1 * ###

`add_list_tags(Tags) -> any()`

<a name="add_manifest_tags-2"></a>

### add_manifest_tags/2 * ###

`add_manifest_tags(Tags, ManifestID) -> any()`

<a name="ar_bundles_test_-0"></a>

### ar_bundles_test_/0 * ###

`ar_bundles_test_() -> any()`

<a name="assert_data_item-7"></a>

### assert_data_item/7 * ###

`assert_data_item(KeyType, Owner, Target, Anchor, Tags, Data, DataItem) -> any()`

<a name="check_size-2"></a>

### check_size/2 * ###

`check_size(Bin, Sizes) -> any()`

Force that a binary is either empty or the given number of bytes.

<a name="check_type-2"></a>

### check_type/2 * ###

`check_type(Value, X2) -> any()`

Ensure that a value is of the given type.

<a name="data_item_signature_data-1"></a>

### data_item_signature_data/1 ###

`data_item_signature_data(RawItem) -> any()`

Generate the data segment to be signed for a data item.

<a name="data_item_signature_data-2"></a>

### data_item_signature_data/2 * ###

`data_item_signature_data(RawItem, X2) -> any()`

<a name="decode_avro_name-3"></a>

### decode_avro_name/3 * ###

`decode_avro_name(NameSize, Rest, Count) -> any()`

<a name="decode_avro_tags-2"></a>

### decode_avro_tags/2 * ###

`decode_avro_tags(Binary, Count) -> any()`

Decode Avro blocks (for tags) from binary.

<a name="decode_avro_value-4"></a>

### decode_avro_value/4 * ###

`decode_avro_value(ValueSize, Name, Rest, Count) -> any()`

<a name="decode_bundle_header-2"></a>

### decode_bundle_header/2 * ###

`decode_bundle_header(Count, Bin) -> any()`

<a name="decode_bundle_header-3"></a>

### decode_bundle_header/3 * ###

`decode_bundle_header(Count, ItemsBin, Header) -> any()`

<a name="decode_bundle_items-2"></a>

### decode_bundle_items/2 * ###

`decode_bundle_items(RestItems, ItemsBin) -> any()`

<a name="decode_optional_field-1"></a>

### decode_optional_field/1 * ###

`decode_optional_field(X1) -> any()`

<a name="decode_signature-1"></a>

### decode_signature/1 * ###

`decode_signature(Other) -> any()`

Decode the signature from a binary format. Only RSA 4096 is currently supported.
Note: the signature type '1' corresponds to RSA 4096 - but it is is written in
little-endian format which is why we match on `<<1, 0>>`.

<a name="decode_tags-1"></a>

### decode_tags/1 ###

`decode_tags(X1) -> any()`

Decode tags from a binary format using Apache Avro.

<a name="decode_vint-3"></a>

### decode_vint/3 * ###

`decode_vint(X1, Result, Shift) -> any()`

<a name="decode_zigzag-1"></a>

### decode_zigzag/1 * ###

`decode_zigzag(Binary) -> any()`

Decode a VInt encoded ZigZag integer from binary.

<a name="deserialize-1"></a>

### deserialize/1 ###

`deserialize(Binary) -> any()`

Convert binary data back to a #tx record.

<a name="deserialize-2"></a>

### deserialize/2 ###

`deserialize(Item, X2) -> any()`

<a name="encode_avro_string-1"></a>

### encode_avro_string/1 * ###

`encode_avro_string(String) -> any()`

Encode a string for Avro using ZigZag and VInt encoding.

<a name="encode_optional_field-1"></a>

### encode_optional_field/1 * ###

`encode_optional_field(Field) -> any()`

Encode an optional field (target, anchor) with a presence byte.

<a name="encode_signature_type-1"></a>

### encode_signature_type/1 * ###

`encode_signature_type(X1) -> any()`

Only RSA 4096 is currently supported.
Note: the signature type '1' corresponds to RSA 4096 -- but it is is written in
little-endian format which is why we encode to `<<1, 0>>`.

<a name="encode_tags-1"></a>

### encode_tags/1 ###

`encode_tags(Tags) -> any()`

Encode tags into a binary format using Apache Avro.

<a name="encode_tags_size-2"></a>

### encode_tags_size/2 * ###

`encode_tags_size(Tags, EncodedTags) -> any()`

<a name="encode_vint-1"></a>

### encode_vint/1 * ###

`encode_vint(ZigZag) -> any()`

Encode a ZigZag integer to VInt binary format.

<a name="encode_vint-2"></a>

### encode_vint/2 * ###

`encode_vint(ZigZag, Acc) -> any()`

<a name="encode_zigzag-1"></a>

### encode_zigzag/1 * ###

`encode_zigzag(Int) -> any()`

Encode an integer using ZigZag encoding.

<a name="enforce_valid_tx-1"></a>

### enforce_valid_tx/1 * ###

`enforce_valid_tx(List) -> any()`

Take an item and ensure that it is of valid form. Useful for ensuring
that a message is viable for serialization/deserialization before execution.
This function should throw simple, easy to follow errors to aid devs in
debugging issues.

<a name="finalize_bundle_data-1"></a>

### finalize_bundle_data/1 * ###

`finalize_bundle_data(Processed) -> any()`

<a name="find-2"></a>

### find/2 ###

`find(Key, Map) -> any()`

Find an item in a bundle-map/list and return it.

<a name="find_single_layer-2"></a>

### find_single_layer/2 * ###

`find_single_layer(UnsignedID, TX) -> any()`

An internal helper for finding an item in a single-layer of a bundle.
Does not recurse! You probably want `find/2` in most cases.

<a name="format-1"></a>

### format/1 ###

`format(Item) -> any()`

<a name="format-2"></a>

### format/2 ###

`format(Item, Indent) -> any()`

<a name="format_binary-1"></a>

### format_binary/1 * ###

`format_binary(Bin) -> any()`

<a name="format_data-2"></a>

### format_data/2 * ###

`format_data(Item, Indent) -> any()`

<a name="format_line-2"></a>

### format_line/2 * ###

`format_line(Str, Indent) -> any()`

<a name="format_line-3"></a>

### format_line/3 * ###

`format_line(RawStr, Fmt, Ind) -> any()`

<a name="hd-1"></a>

### hd/1 ###

`hd(Tx) -> any()`

Return the first item in a bundle-map/list.

<a name="id-1"></a>

### id/1 ###

`id(Item) -> any()`

Return the ID of an item -- either signed or unsigned as specified.
If the item is unsigned and the user requests the signed ID, we return
the atom `not_signed`. In all other cases, we return the ID of the item.

<a name="id-2"></a>

### id/2 ###

`id(Item, Type) -> any()`

<a name="is_signed-1"></a>

### is_signed/1 ###

`is_signed(Item) -> any()`

Check if an item is signed.

<a name="manifest-1"></a>

### manifest/1 ###

`manifest(Map) -> any()`

<a name="manifest_item-1"></a>

### manifest_item/1 ###

`manifest_item(Tx) -> any()`

Return the manifest item in a bundle-map/list.

<a name="map-1"></a>

### map/1 ###

`map(Tx) -> any()`

Convert an item containing a map or list into an Erlang map.

<a name="maybe_map_to_list-1"></a>

### maybe_map_to_list/1 * ###

`maybe_map_to_list(Item) -> any()`

<a name="maybe_unbundle-1"></a>

### maybe_unbundle/1 * ###

`maybe_unbundle(Item) -> any()`

<a name="maybe_unbundle_map-1"></a>

### maybe_unbundle_map/1 * ###

`maybe_unbundle_map(Bundle) -> any()`

<a name="member-2"></a>

### member/2 ###

`member(Key, Item) -> any()`

Check if an item exists in a bundle-map/list.

<a name="new_item-4"></a>

### new_item/4 ###

`new_item(Target, Anchor, Tags, Data) -> any()`

Create a new data item. Should only be used for testing.

<a name="new_manifest-1"></a>

### new_manifest/1 * ###

`new_manifest(Index) -> any()`

<a name="normalize-1"></a>

### normalize/1 ###

`normalize(Item) -> any()`

<a name="normalize_data-1"></a>

### normalize_data/1 * ###

`normalize_data(Bundle) -> any()`

Ensure that a data item (potentially containing a map or list) has a standard, serialized form.

<a name="normalize_data_size-1"></a>

### normalize_data_size/1 * ###

`normalize_data_size(Item) -> any()`

Reset the data size of a data item. Assumes that the data is already normalized.

<a name="ok_or_throw-3"></a>

### ok_or_throw/3 * ###

`ok_or_throw(TX, X2, Error) -> any()`

Throw an error if the given value is not ok.

<a name="parse_manifest-1"></a>

### parse_manifest/1 ###

`parse_manifest(Item) -> any()`

<a name="print-1"></a>

### print/1 ###

`print(Item) -> any()`

<a name="reset_ids-1"></a>

### reset_ids/1 ###

`reset_ids(Item) -> any()`

Re-calculate both of the IDs for an item. This is a wrapper
function around `update_id/1` that ensures both IDs are set from
scratch.

<a name="run_test-0"></a>

### run_test/0 * ###

`run_test() -> any()`

<a name="serialize-1"></a>

### serialize/1 ###

`serialize(TX) -> any()`

Convert a #tx record to its binary representation.

<a name="serialize-2"></a>

### serialize/2 ###

`serialize(TX, X2) -> any()`

<a name="serialize_bundle_data-2"></a>

### serialize_bundle_data/2 * ###

`serialize_bundle_data(Map, Manifest) -> any()`

<a name="sign_item-2"></a>

### sign_item/2 ###

`sign_item(RawItem, X2) -> any()`

Sign a data item.

<a name="signer-1"></a>

### signer/1 ###

`signer(Tx) -> any()`

Return the address of the signer of an item, if it is signed.

<a name="test_basic_member_id-0"></a>

### test_basic_member_id/0 * ###

`test_basic_member_id() -> any()`

<a name="test_bundle_map-0"></a>

### test_bundle_map/0 * ###

`test_bundle_map() -> any()`

<a name="test_bundle_with_one_item-0"></a>

### test_bundle_with_one_item/0 * ###

`test_bundle_with_one_item() -> any()`

<a name="test_bundle_with_two_items-0"></a>

### test_bundle_with_two_items/0 * ###

`test_bundle_with_two_items() -> any()`

<a name="test_deep_member-0"></a>

### test_deep_member/0 * ###

`test_deep_member() -> any()`

<a name="test_empty_bundle-0"></a>

### test_empty_bundle/0 * ###

`test_empty_bundle() -> any()`

<a name="test_extremely_large_bundle-0"></a>

### test_extremely_large_bundle/0 * ###

`test_extremely_large_bundle() -> any()`

<a name="test_no_tags-0"></a>

### test_no_tags/0 * ###

`test_no_tags() -> any()`

<a name="test_recursive_bundle-0"></a>

### test_recursive_bundle/0 * ###

`test_recursive_bundle() -> any()`

<a name="test_serialize_deserialize_deep_signed_bundle-0"></a>

### test_serialize_deserialize_deep_signed_bundle/0 * ###

`test_serialize_deserialize_deep_signed_bundle() -> any()`

<a name="test_unsigned_data_item_id-0"></a>

### test_unsigned_data_item_id/0 * ###

`test_unsigned_data_item_id() -> any()`

<a name="test_unsigned_data_item_normalization-0"></a>

### test_unsigned_data_item_normalization/0 * ###

`test_unsigned_data_item_normalization() -> any()`

<a name="test_with_tags-0"></a>

### test_with_tags/0 * ###

`test_with_tags() -> any()`

<a name="test_with_zero_length_tag-0"></a>

### test_with_zero_length_tag/0 * ###

`test_with_zero_length_tag() -> any()`

<a name="to_serialized_pair-1"></a>

### to_serialized_pair/1 * ###

`to_serialized_pair(Item) -> any()`

<a name="type-1"></a>

### type/1 ###

`type(Item) -> any()`

<a name="unbundle-1"></a>

### unbundle/1 * ###

`unbundle(Item) -> any()`

<a name="unbundle_list-1"></a>

### unbundle_list/1 * ###

`unbundle_list(Item) -> any()`

<a name="update_ids-1"></a>

### update_ids/1 * ###

`update_ids(Item) -> any()`

Take an item and ensure that both the unsigned and signed IDs are
appropriately set. This function is structured to fall through all cases
of poorly formed items, recursively ensuring its correctness for each case
until the item has a coherent set of IDs.
The cases in turn are:
- The item has no unsigned_id. This is never valid.
- The item has the default signature and ID. This is valid.
- The item has the default signature but a non-default ID. Reset the ID.
- The item has a signature. We calculate the ID from the signature.
- Valid: The item is fully formed and has both an unsigned and signed ID.

<a name="utf8_encoded-1"></a>

### utf8_encoded/1 * ###

`utf8_encoded(String) -> any()`

Encode a UTF-8 string to binary.

<a name="verify_data_item_id-1"></a>

### verify_data_item_id/1 * ###

`verify_data_item_id(DataItem) -> any()`

Verify the data item's ID matches the signature.

<a name="verify_data_item_signature-1"></a>

### verify_data_item_signature/1 * ###

`verify_data_item_signature(DataItem) -> any()`

Verify the data item's signature.

<a name="verify_data_item_tags-1"></a>

### verify_data_item_tags/1 * ###

`verify_data_item_tags(DataItem) -> any()`

Verify the validity of the data item's tags.

<a name="verify_item-1"></a>

### verify_item/1 ###

`verify_item(DataItem) -> any()`

Verify the validity of a data item.

