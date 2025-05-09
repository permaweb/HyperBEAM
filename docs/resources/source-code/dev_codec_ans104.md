# [Module dev_codec_ans104.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_ans104.erl)




Codec for managing transformations from `ar_bundles`-style Arweave TX
records to and from TABMs.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td>Sign a message using the <code>priv_wallet</code> key in the options.</td></tr><tr><td valign="top"><a href="#committed-3">committed/3</a></td><td>Return a list of committed keys from an ANS-104 message.</td></tr><tr><td valign="top"><a href="#committed_from_trusted_keys-3">committed_from_trusted_keys/3*</a></td><td></td></tr><tr><td valign="top"><a href="#content_type-1">content_type/1</a></td><td>Return the content type for the codec.</td></tr><tr><td valign="top"><a href="#deduplicating_from_list-1">deduplicating_from_list/1*</a></td><td>Deduplicate a list of key-value pairs by key, generating a list of
values for each normalized key if there are duplicates.</td></tr><tr><td valign="top"><a href="#deserialize-1">deserialize/1</a></td><td>Deserialize a binary ans104 message to a TABM.</td></tr><tr><td valign="top"><a href="#do_from-1">do_from/1*</a></td><td></td></tr><tr><td valign="top"><a href="#duplicated_tag_name_test-0">duplicated_tag_name_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#encoded_tags_to_map-1">encoded_tags_to_map/1*</a></td><td>Convert an ANS-104 encoded tag list into a HyperBEAM-compatible map.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>Convert a #tx record into a message map recursively.</td></tr><tr><td valign="top"><a href="#from_maintains_tag_name_case_test-0">from_maintains_tag_name_case_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Return the ID of a message.</td></tr><tr><td valign="top"><a href="#normal_tags-1">normal_tags/1*</a></td><td>Check whether a list of key-value pairs contains only normalized keys.</td></tr><tr><td valign="top"><a href="#normal_tags_test-0">normal_tags_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#only_committed_maintains_target_test-0">only_committed_maintains_target_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#quantity_field_is_ignored_in_from_test-0">quantity_field_is_ignored_in_from_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#quantity_key_encoded_as_tag_test-0">quantity_key_encoded_as_tag_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#restore_tag_name_case_from_cache_test-0">restore_tag_name_case_from_cache_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#serialize-1">serialize/1</a></td><td>Serialize a message or TX to a binary.</td></tr><tr><td valign="top"><a href="#signed_duplicated_tag_name_test-0">signed_duplicated_tag_name_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#simple_to_conversion_test-0">simple_to_conversion_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#tag_map_to_encoded_tags-1">tag_map_to_encoded_tags/1*</a></td><td>Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,
recreating the original order of the tags.</td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td>Internal helper to translate a message to its #tx record representation,
which can then be used by ar_bundles to serialize the message.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td>Verify an ANS-104 commitment.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="commit-3"></a>

### commit/3 ###

`commit(Msg, Req, Opts) -> any()`

Sign a message using the `priv_wallet` key in the options.

<a name="committed-3"></a>

### committed/3 ###

`committed(Msg, Req, Opts) -> any()`

Return a list of committed keys from an ANS-104 message.

<a name="committed_from_trusted_keys-3"></a>

### committed_from_trusted_keys/3 * ###

`committed_from_trusted_keys(Msg, TrustedKeys, Opts) -> any()`

<a name="content_type-1"></a>

### content_type/1 ###

`content_type(X1) -> any()`

Return the content type for the codec.

<a name="deduplicating_from_list-1"></a>

### deduplicating_from_list/1 * ###

`deduplicating_from_list(Tags) -> any()`

Deduplicate a list of key-value pairs by key, generating a list of
values for each normalized key if there are duplicates.

<a name="deserialize-1"></a>

### deserialize/1 ###

`deserialize(Binary) -> any()`

Deserialize a binary ans104 message to a TABM.

<a name="do_from-1"></a>

### do_from/1 * ###

`do_from(RawTX) -> any()`

<a name="duplicated_tag_name_test-0"></a>

### duplicated_tag_name_test/0 * ###

`duplicated_tag_name_test() -> any()`

<a name="encoded_tags_to_map-1"></a>

### encoded_tags_to_map/1 * ###

`encoded_tags_to_map(Tags) -> any()`

Convert an ANS-104 encoded tag list into a HyperBEAM-compatible map.

<a name="from-1"></a>

### from/1 ###

`from(Binary) -> any()`

Convert a #tx record into a message map recursively.

<a name="from_maintains_tag_name_case_test-0"></a>

### from_maintains_tag_name_case_test/0 * ###

`from_maintains_tag_name_case_test() -> any()`

<a name="id-1"></a>

### id/1 ###

`id(Msg) -> any()`

Return the ID of a message.

<a name="normal_tags-1"></a>

### normal_tags/1 * ###

`normal_tags(Tags) -> any()`

Check whether a list of key-value pairs contains only normalized keys.

<a name="normal_tags_test-0"></a>

### normal_tags_test/0 * ###

`normal_tags_test() -> any()`

<a name="only_committed_maintains_target_test-0"></a>

### only_committed_maintains_target_test/0 * ###

`only_committed_maintains_target_test() -> any()`

<a name="quantity_field_is_ignored_in_from_test-0"></a>

### quantity_field_is_ignored_in_from_test/0 * ###

`quantity_field_is_ignored_in_from_test() -> any()`

<a name="quantity_key_encoded_as_tag_test-0"></a>

### quantity_key_encoded_as_tag_test/0 * ###

`quantity_key_encoded_as_tag_test() -> any()`

<a name="restore_tag_name_case_from_cache_test-0"></a>

### restore_tag_name_case_from_cache_test/0 * ###

`restore_tag_name_case_from_cache_test() -> any()`

<a name="serialize-1"></a>

### serialize/1 ###

`serialize(Msg) -> any()`

Serialize a message or TX to a binary.

<a name="signed_duplicated_tag_name_test-0"></a>

### signed_duplicated_tag_name_test/0 * ###

`signed_duplicated_tag_name_test() -> any()`

<a name="simple_to_conversion_test-0"></a>

### simple_to_conversion_test/0 * ###

`simple_to_conversion_test() -> any()`

<a name="tag_map_to_encoded_tags-1"></a>

### tag_map_to_encoded_tags/1 * ###

`tag_map_to_encoded_tags(TagMap) -> any()`

Convert a HyperBEAM-compatible map into an ANS-104 encoded tag list,
recreating the original order of the tags.

<a name="to-1"></a>

### to/1 ###

`to(Binary) -> any()`

Internal helper to translate a message to its #tx record representation,
which can then be used by ar_bundles to serialize the message. We call the
message's device in order to get the keys that we will be checkpointing. We
do this recursively to handle nested messages. The base case is that we hit
a binary, which we return as is.

<a name="verify-3"></a>

### verify/3 ###

`verify(Msg, Req, Opts) -> any()`

Verify an ANS-104 commitment.

