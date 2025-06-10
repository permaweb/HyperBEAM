# [Module hb_message.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_message.erl)




This module acts an adapter between messages, as modeled in the
AO-Core protocol, and their uderlying binary representations and formats.

<a name="description"></a>

## Description ##

Unless you are implementing a new message serialization codec, you should
not need to interact with this module directly. Instead, use the
`hb_ao` interfaces to interact with all messages. The `dev_message`
module implements a device interface for abstracting over the different
message formats.

`hb_message` and the HyperBEAM caches can interact with multiple different
types of message formats:

- Richly typed AO-Core structured messages.
- Arweave transations.
- ANS-104 data items.
- HTTP Signed Messages.
- Flat Maps.

This module is responsible for converting between these formats. It does so
by normalizing messages to a common format: `Type Annotated Binary Messages`
(TABM). TABMs are deep Erlang maps with keys than only contain either other
TABMs or binary values. By marshalling all messages into this format, they
can easily be coerced into other output formats. For example, generating a
`HTTP Signed Message` format output from an Arweave transaction. TABM is
also a simple format from a computational perspective (only binary literals
and O(1) access maps), such that operations upon them are efficient.

The structure of the conversions is as follows:

<pre>
Arweave TX/ANS-104 ==> dev_codec_ans104:from/1 ==> TABM
HTTP Signed Message ==> dev_codec_httpsig_conv:from/1 ==> TABM
Flat Maps ==> dev_codec_flat:from/1 ==> TABM

TABM ==> dev_codec_structured:to/1 ==> AO-Core Message
AO-Core Message ==> dev_codec_structured:from/1 ==> TABM

TABM ==> dev_codec_ans104:to/1 ==> Arweave TX/ANS-104
TABM ==> dev_codec_httpsig_conv:to/1 ==> HTTP Signed Message
TABM ==> dev_codec_flat:to/1 ==> Flat Maps
...
</pre>

Additionally, this module provides a number of utility functions for
manipulating messages. For example, `hb_message:sign/2` to sign a message of
arbitrary type, or `hb_message:format/1` to print an AO-Core/TABM message in
a human-readable format.

The `hb_cache` module is responsible for storing and retrieving messages in
the HyperBEAM stores configured on the node. Each store has its own storage
backend, but each works with simple key-value pairs. Subsequently, the
`hb_cache` module uses TABMs as the internal format for storing and
retrieving messages.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#basic_map_codec_test-1">basic_map_codec_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#binary_to_binary_test-1">binary_to_binary_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#commit-2">commit/2</a></td><td>Sign a message with the given wallet.</td></tr><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td></td></tr><tr><td valign="top"><a href="#commitment-2">commitment/2</a></td><td>Extract a commitment from a message given a <code>committer</code> ID, or a spec
message to match against.</td></tr><tr><td valign="top"><a href="#commitment-3">commitment/3</a></td><td></td></tr><tr><td valign="top"><a href="#committed-1">committed/1</a></td><td>Return the list of committed keys from a message.</td></tr><tr><td valign="top"><a href="#committed-2">committed/2</a></td><td></td></tr><tr><td valign="top"><a href="#committed-3">committed/3</a></td><td></td></tr><tr><td valign="top"><a href="#committed_empty_keys_test-1">committed_empty_keys_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#committed_keys_test-1">committed_keys_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#complex_signed_message_test-1">complex_signed_message_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#convert-3">convert/3</a></td><td>Convert a message from one format to another.</td></tr><tr><td valign="top"><a href="#convert-4">convert/4</a></td><td></td></tr><tr><td valign="top"><a href="#deep_multisignature_test-0">deep_multisignature_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#deeply_nested_committed_keys_test-0">deeply_nested_committed_keys_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#deeply_nested_message_with_content_test-1">deeply_nested_message_with_content_test/1*</a></td><td>Test that we can convert a 3 layer nested message into a tx record and back.</td></tr><tr><td valign="top"><a href="#deeply_nested_message_with_only_content-1">deeply_nested_message_with_only_content/1*</a></td><td></td></tr><tr><td valign="top"><a href="#default_keys_removed_test-0">default_keys_removed_test/0*</a></td><td>Test that the filter_default_keys/1 function removes TX fields
that have the default values found in the tx record, but not those that
have been set by the user.</td></tr><tr><td valign="top"><a href="#default_tx_list-0">default_tx_list/0</a></td><td>Get the ordered list of fields as AO-Core keys and default values of
the tx record.</td></tr><tr><td valign="top"><a href="#default_tx_message-0">default_tx_message/0*</a></td><td>Get the normalized fields and default values of the tx record.</td></tr><tr><td valign="top"><a href="#empty_string_in_tag_test-1">empty_string_in_tag_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_balance_table-2">encode_balance_table/2*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_large_balance_table_test-1">encode_large_balance_table_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_small_balance_table_test-1">encode_small_balance_table_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#filter_default_keys-1">filter_default_keys/1</a></td><td>Remove keys from a map that have the default values found in the tx
record.</td></tr><tr><td valign="top"><a href="#find_target-3">find_target/3</a></td><td>Implements a standard pattern in which the target for an operation is
found by looking for a <code>target</code> key in the request.</td></tr><tr><td valign="top"><a href="#format-1">format/1</a></td><td>Format a message for printing, optionally taking an indentation level
to start from.</td></tr><tr><td valign="top"><a href="#format-2">format/2</a></td><td></td></tr><tr><td valign="top"><a href="#from_tabm-4">from_tabm/4*</a></td><td></td></tr><tr><td valign="top"><a href="#generate_test_suite-1">generate_test_suite/1*</a></td><td></td></tr><tr><td valign="top"><a href="#get_codec-2">get_codec/2*</a></td><td>Get a codec from the options.</td></tr><tr><td valign="top"><a href="#hashpath_sign_verify_test-1">hashpath_sign_verify_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#id-1">id/1</a></td><td>Return the ID of a message.</td></tr><tr><td valign="top"><a href="#id-2">id/2</a></td><td></td></tr><tr><td valign="top"><a href="#id-3">id/3</a></td><td></td></tr><tr><td valign="top"><a href="#large_body_committed_keys_test-1">large_body_committed_keys_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Check if two maps match, including recursively checking nested maps.</td></tr><tr><td valign="top"><a href="#match-3">match/3</a></td><td></td></tr><tr><td valign="top"><a href="#match_modes_test-0">match_modes_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#match_test-1">match_test/1*</a></td><td>Test that the message matching function works.</td></tr><tr><td valign="top"><a href="#matchable_keys-1">matchable_keys/1*</a></td><td></td></tr><tr><td valign="top"><a href="#message_suite_test_-0">message_suite_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#message_with_large_keys_test-1">message_with_large_keys_test/1*</a></td><td>Test that the data field is correctly managed when we have multiple
uses for it (the 'data' key itself, as well as keys that cannot fit in
tags).</td></tr><tr><td valign="top"><a href="#message_with_simple_embedded_list_test-1">message_with_simple_embedded_list_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#minimization_test-0">minimization_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#minimize-1">minimize/1</a></td><td>Remove keys from the map that can be regenerated.</td></tr><tr><td valign="top"><a href="#minimize-2">minimize/2*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_body_list_test-1">nested_body_list_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_empty_map_test-1">nested_empty_map_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_message_with_large_content_test-1">nested_message_with_large_content_test/1*</a></td><td>Test that the data field is correctly managed when we have multiple
uses for it (the 'data' key itself, as well as keys that cannot fit in
tags).</td></tr><tr><td valign="top"><a href="#nested_message_with_large_keys_and_content_test-1">nested_message_with_large_keys_and_content_test/1*</a></td><td>Check that large keys and data fields are correctly handled together.</td></tr><tr><td valign="top"><a href="#nested_message_with_large_keys_test-1">nested_message_with_large_keys_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_structured_fields_test-1">nested_structured_fields_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize-1">normalize/1*</a></td><td>Return a map with only the keys that necessary, without those that can
be regenerated.</td></tr><tr><td valign="top"><a href="#print-1">print/1</a></td><td>Pretty-print a message.</td></tr><tr><td valign="top"><a href="#print-2">print/2*</a></td><td></td></tr><tr><td valign="top"><a href="#priv_survives_conversion_test-1">priv_survives_conversion_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#recursive_nested_list_test-1">recursive_nested_list_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#restore_priv-2">restore_priv/2*</a></td><td>Add the existing <code>priv</code> sub-map back to a converted message, honoring
any existing <code>priv</code> sub-map that may already be present.</td></tr><tr><td valign="top"><a href="#run_test-0">run_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#set_body_codec_test-1">set_body_codec_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#sign_node_message_test-1">sign_node_message_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_deep_message_test-1">signed_deep_message_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_list_test-1">signed_list_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_message_encode_decode_verify_test-1">signed_message_encode_decode_verify_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_message_with_derived_components_test-1">signed_message_with_derived_components_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_nested_data_key_test-1">signed_nested_data_key_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_only_committed_data_field_test-1">signed_only_committed_data_field_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_with_inner_signed_message_test-1">signed_with_inner_signed_message_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#signers-1">signers/1</a></td><td>Return all of the committers on a message that have 'normal', 256 bit,
addresses.</td></tr><tr><td valign="top"><a href="#simple_nested_message_test-1">simple_nested_message_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#single_layer_message_to_encoding_test-1">single_layer_message_to_encoding_test/1*</a></td><td>Test that we can convert a message into a tx record and back.</td></tr><tr><td valign="top"><a href="#structured_field_atom_parsing_test-1">structured_field_atom_parsing_test/1*</a></td><td>Structured field parsing tests.</td></tr><tr><td valign="top"><a href="#structured_field_decimal_parsing_test-1">structured_field_decimal_parsing_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#tabm_ao_ids_equal_test-1">tabm_ao_ids_equal_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_codecs-0">test_codecs/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to_tabm-3">to_tabm/3*</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>Return the type of an encoded message.</td></tr><tr><td valign="top"><a href="#uncommitted-1">uncommitted/1</a></td><td>Return the unsigned version of a message in AO-Core format.</td></tr><tr><td valign="top"><a href="#unsigned_id_test-1">unsigned_id_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#verify-1">verify/1</a></td><td>wrapper function to verify a message.</td></tr><tr><td valign="top"><a href="#verify-2">verify/2</a></td><td></td></tr><tr><td valign="top"><a href="#with_commitments-2">with_commitments/2</a></td><td>Filter messages that do not match the 'spec' given.</td></tr><tr><td valign="top"><a href="#with_commitments-3">with_commitments/3*</a></td><td></td></tr><tr><td valign="top"><a href="#with_only_committed-1">with_only_committed/1</a></td><td>Return a message with only the committed keys.</td></tr><tr><td valign="top"><a href="#with_only_committed-2">with_only_committed/2</a></td><td></td></tr><tr><td valign="top"><a href="#with_only_committers-2">with_only_committers/2</a></td><td>Return the message with only the specified committers attached.</td></tr><tr><td valign="top"><a href="#without_commitments-2">without_commitments/2</a></td><td>Filter messages that match the 'spec' given.</td></tr><tr><td valign="top"><a href="#without_commitments-3">without_commitments/3*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="basic_map_codec_test-1"></a>

### basic_map_codec_test/1 * ###

`basic_map_codec_test(Codec) -> any()`

<a name="binary_to_binary_test-1"></a>

### binary_to_binary_test/1 * ###

`binary_to_binary_test(Codec) -> any()`

<a name="commit-2"></a>

### commit/2 ###

`commit(Msg, WalletOrOpts) -> any()`

Sign a message with the given wallet.

<a name="commit-3"></a>

### commit/3 ###

`commit(Msg, Wallet, Format) -> any()`

<a name="commitment-2"></a>

### commitment/2 ###

`commitment(Committer, Msg) -> any()`

Extract a commitment from a message given a `committer` ID, or a spec
message to match against. Returns only the first matching commitment, or
`not_found`.

<a name="commitment-3"></a>

### commitment/3 ###

`commitment(CommitterID, Msg, Opts) -> any()`

<a name="committed-1"></a>

### committed/1 ###

`committed(Msg) -> any()`

Return the list of committed keys from a message.

<a name="committed-2"></a>

### committed/2 ###

`committed(Msg, Committers) -> any()`

<a name="committed-3"></a>

### committed/3 ###

`committed(Msg, List, Opts) -> any()`

<a name="committed_empty_keys_test-1"></a>

### committed_empty_keys_test/1 * ###

`committed_empty_keys_test(Codec) -> any()`

<a name="committed_keys_test-1"></a>

### committed_keys_test/1 * ###

`committed_keys_test(Codec) -> any()`

<a name="complex_signed_message_test-1"></a>

### complex_signed_message_test/1 * ###

`complex_signed_message_test(Codec) -> any()`

<a name="convert-3"></a>

### convert/3 ###

`convert(Msg, TargetFormat, Opts) -> any()`

Convert a message from one format to another. Taking a message in the
source format, a target format, and a set of opts. If not given, the source
is assumed to be `structured@1.0`. Additional codecs can be added by ensuring they
are part of the `Opts` map -- either globally, or locally for a computation.

The encoding happens in two phases:
1. Convert the message to a TABM.
2. Convert the TABM to the target format.

The conversion to a TABM is done by the `structured@1.0` codec, which is always
available. The conversion from a TABM is done by the target codec.

<a name="convert-4"></a>

### convert/4 ###

`convert(Msg, TargetFormat, SourceFormat, Opts) -> any()`

<a name="deep_multisignature_test-0"></a>

### deep_multisignature_test/0 * ###

`deep_multisignature_test() -> any()`

<a name="deeply_nested_committed_keys_test-0"></a>

### deeply_nested_committed_keys_test/0 * ###

`deeply_nested_committed_keys_test() -> any()`

<a name="deeply_nested_message_with_content_test-1"></a>

### deeply_nested_message_with_content_test/1 * ###

`deeply_nested_message_with_content_test(Codec) -> any()`

Test that we can convert a 3 layer nested message into a tx record and back.

<a name="deeply_nested_message_with_only_content-1"></a>

### deeply_nested_message_with_only_content/1 * ###

`deeply_nested_message_with_only_content(Codec) -> any()`

<a name="default_keys_removed_test-0"></a>

### default_keys_removed_test/0 * ###

`default_keys_removed_test() -> any()`

Test that the filter_default_keys/1 function removes TX fields
that have the default values found in the tx record, but not those that
have been set by the user.

<a name="default_tx_list-0"></a>

### default_tx_list/0 ###

`default_tx_list() -> any()`

Get the ordered list of fields as AO-Core keys and default values of
the tx record.

<a name="default_tx_message-0"></a>

### default_tx_message/0 * ###

`default_tx_message() -> any()`

Get the normalized fields and default values of the tx record.

<a name="empty_string_in_tag_test-1"></a>

### empty_string_in_tag_test/1 * ###

`empty_string_in_tag_test(Codec) -> any()`

<a name="encode_balance_table-2"></a>

### encode_balance_table/2 * ###

`encode_balance_table(Size, Codec) -> any()`

<a name="encode_large_balance_table_test-1"></a>

### encode_large_balance_table_test/1 * ###

`encode_large_balance_table_test(Codec) -> any()`

<a name="encode_small_balance_table_test-1"></a>

### encode_small_balance_table_test/1 * ###

`encode_small_balance_table_test(Codec) -> any()`

<a name="filter_default_keys-1"></a>

### filter_default_keys/1 ###

`filter_default_keys(Map) -> any()`

Remove keys from a map that have the default values found in the tx
record.

<a name="find_target-3"></a>

### find_target/3 ###

`find_target(Self, Req, Opts) -> any()`

Implements a standard pattern in which the target for an operation is
found by looking for a `target` key in the request. If the target is `self`,
or not present, the operation is performed on the original message. Otherwise,
the target is expected to be a key in the message, and the operation is
performed on the value of that key.

<a name="format-1"></a>

### format/1 ###

`format(Item) -> any()`

Format a message for printing, optionally taking an indentation level
to start from.

<a name="format-2"></a>

### format/2 ###

`format(Bin, Indent) -> any()`

<a name="from_tabm-4"></a>

### from_tabm/4 * ###

`from_tabm(Msg, TargetFormat, OldPriv, Opts) -> any()`

<a name="generate_test_suite-1"></a>

### generate_test_suite/1 * ###

`generate_test_suite(Suite) -> any()`

<a name="get_codec-2"></a>

### get_codec/2 * ###

`get_codec(TargetFormat, Opts) -> any()`

Get a codec from the options.

<a name="hashpath_sign_verify_test-1"></a>

### hashpath_sign_verify_test/1 * ###

`hashpath_sign_verify_test(Codec) -> any()`

<a name="id-1"></a>

### id/1 ###

`id(Msg) -> any()`

Return the ID of a message.

<a name="id-2"></a>

### id/2 ###

`id(Msg, Committers) -> any()`

<a name="id-3"></a>

### id/3 ###

`id(Msg, RawCommitters, Opts) -> any()`

<a name="large_body_committed_keys_test-1"></a>

### large_body_committed_keys_test/1 * ###

`large_body_committed_keys_test(Codec) -> any()`

<a name="match-2"></a>

### match/2 ###

`match(Map1, Map2) -> any()`

Check if two maps match, including recursively checking nested maps.
Takes an optional mode argument to control the matching behavior:
`strict`: All keys in both maps be present and match.
`only_present`: Only present keys in both maps must match.
`primary`: Only the primary map's keys must be present.

<a name="match-3"></a>

### match/3 ###

`match(Map1, Map2, Mode) -> any()`

<a name="match_modes_test-0"></a>

### match_modes_test/0 * ###

`match_modes_test() -> any()`

<a name="match_test-1"></a>

### match_test/1 * ###

`match_test(Codec) -> any()`

Test that the message matching function works.

<a name="matchable_keys-1"></a>

### matchable_keys/1 * ###

`matchable_keys(Map) -> any()`

<a name="message_suite_test_-0"></a>

### message_suite_test_/0 * ###

`message_suite_test_() -> any()`

<a name="message_with_large_keys_test-1"></a>

### message_with_large_keys_test/1 * ###

`message_with_large_keys_test(Codec) -> any()`

Test that the data field is correctly managed when we have multiple
uses for it (the 'data' key itself, as well as keys that cannot fit in
tags).

<a name="message_with_simple_embedded_list_test-1"></a>

### message_with_simple_embedded_list_test/1 * ###

`message_with_simple_embedded_list_test(Codec) -> any()`

<a name="minimization_test-0"></a>

### minimization_test/0 * ###

`minimization_test() -> any()`

<a name="minimize-1"></a>

### minimize/1 ###

`minimize(Msg) -> any()`

Remove keys from the map that can be regenerated. Optionally takes an
additional list of keys to include in the minimization.

<a name="minimize-2"></a>

### minimize/2 * ###

`minimize(RawVal, ExtraKeys) -> any()`

<a name="nested_body_list_test-1"></a>

### nested_body_list_test/1 * ###

`nested_body_list_test(Codec) -> any()`

<a name="nested_empty_map_test-1"></a>

### nested_empty_map_test/1 * ###

`nested_empty_map_test(Codec) -> any()`

<a name="nested_message_with_large_content_test-1"></a>

### nested_message_with_large_content_test/1 * ###

`nested_message_with_large_content_test(Codec) -> any()`

Test that the data field is correctly managed when we have multiple
uses for it (the 'data' key itself, as well as keys that cannot fit in
tags).

<a name="nested_message_with_large_keys_and_content_test-1"></a>

### nested_message_with_large_keys_and_content_test/1 * ###

`nested_message_with_large_keys_and_content_test(Codec) -> any()`

Check that large keys and data fields are correctly handled together.

<a name="nested_message_with_large_keys_test-1"></a>

### nested_message_with_large_keys_test/1 * ###

`nested_message_with_large_keys_test(Codec) -> any()`

<a name="nested_structured_fields_test-1"></a>

### nested_structured_fields_test/1 * ###

`nested_structured_fields_test(Codec) -> any()`

<a name="normalize-1"></a>

### normalize/1 * ###

`normalize(Map) -> any()`

Return a map with only the keys that necessary, without those that can
be regenerated.

<a name="print-1"></a>

### print/1 ###

`print(Msg) -> any()`

Pretty-print a message.

<a name="print-2"></a>

### print/2 * ###

`print(Msg, Indent) -> any()`

<a name="priv_survives_conversion_test-1"></a>

### priv_survives_conversion_test/1 * ###

`priv_survives_conversion_test(Codec) -> any()`

<a name="recursive_nested_list_test-1"></a>

### recursive_nested_list_test/1 * ###

`recursive_nested_list_test(Codec) -> any()`

<a name="restore_priv-2"></a>

### restore_priv/2 * ###

`restore_priv(Msg, EmptyPriv) -> any()`

Add the existing `priv` sub-map back to a converted message, honoring
any existing `priv` sub-map that may already be present.

<a name="run_test-0"></a>

### run_test/0 * ###

`run_test() -> any()`

<a name="set_body_codec_test-1"></a>

### set_body_codec_test/1 * ###

`set_body_codec_test(Codec) -> any()`

<a name="sign_node_message_test-1"></a>

### sign_node_message_test/1 * ###

`sign_node_message_test(Codec) -> any()`

<a name="signed_deep_message_test-1"></a>

### signed_deep_message_test/1 * ###

`signed_deep_message_test(Codec) -> any()`

<a name="signed_list_test-1"></a>

### signed_list_test/1 * ###

`signed_list_test(Codec) -> any()`

<a name="signed_message_encode_decode_verify_test-1"></a>

### signed_message_encode_decode_verify_test/1 * ###

`signed_message_encode_decode_verify_test(Codec) -> any()`

<a name="signed_message_with_derived_components_test-1"></a>

### signed_message_with_derived_components_test/1 * ###

`signed_message_with_derived_components_test(Codec) -> any()`

<a name="signed_nested_data_key_test-1"></a>

### signed_nested_data_key_test/1 * ###

`signed_nested_data_key_test(Codec) -> any()`

<a name="signed_only_committed_data_field_test-1"></a>

### signed_only_committed_data_field_test/1 * ###

`signed_only_committed_data_field_test(Codec) -> any()`

<a name="signed_with_inner_signed_message_test-1"></a>

### signed_with_inner_signed_message_test/1 * ###

`signed_with_inner_signed_message_test(Codec) -> any()`

<a name="signers-1"></a>

### signers/1 ###

`signers(Msg) -> any()`

Return all of the committers on a message that have 'normal', 256 bit,
addresses.

<a name="simple_nested_message_test-1"></a>

### simple_nested_message_test/1 * ###

`simple_nested_message_test(Codec) -> any()`

<a name="single_layer_message_to_encoding_test-1"></a>

### single_layer_message_to_encoding_test/1 * ###

`single_layer_message_to_encoding_test(Codec) -> any()`

Test that we can convert a message into a tx record and back.

<a name="structured_field_atom_parsing_test-1"></a>

### structured_field_atom_parsing_test/1 * ###

`structured_field_atom_parsing_test(Codec) -> any()`

Structured field parsing tests.

<a name="structured_field_decimal_parsing_test-1"></a>

### structured_field_decimal_parsing_test/1 * ###

`structured_field_decimal_parsing_test(Codec) -> any()`

<a name="tabm_ao_ids_equal_test-1"></a>

### tabm_ao_ids_equal_test/1 * ###

`tabm_ao_ids_equal_test(Codec) -> any()`

<a name="test_codecs-0"></a>

### test_codecs/0 * ###

`test_codecs() -> any()`

<a name="to_tabm-3"></a>

### to_tabm/3 * ###

`to_tabm(Msg, SourceFormat, Opts) -> any()`

<a name="type-1"></a>

### type/1 ###

`type(TX) -> any()`

Return the type of an encoded message.

<a name="uncommitted-1"></a>

### uncommitted/1 ###

`uncommitted(Bin) -> any()`

Return the unsigned version of a message in AO-Core format.

<a name="unsigned_id_test-1"></a>

### unsigned_id_test/1 * ###

`unsigned_id_test(Codec) -> any()`

<a name="verify-1"></a>

### verify/1 ###

`verify(Msg) -> any()`

wrapper function to verify a message.

<a name="verify-2"></a>

### verify/2 ###

`verify(Msg, Committers) -> any()`

<a name="with_commitments-2"></a>

### with_commitments/2 ###

`with_commitments(Spec, Msg) -> any()`

Filter messages that do not match the 'spec' given. The underlying match
is performed in the `only_present` mode, such that match specifications only
need to specify the keys that must be present.

<a name="with_commitments-3"></a>

### with_commitments/3 * ###

`with_commitments(Spec, Msg, Opts) -> any()`

<a name="with_only_committed-1"></a>

### with_only_committed/1 ###

`with_only_committed(Msg) -> any()`

Return a message with only the committed keys. If no commitments are
present, the message is returned unchanged. This means that you need to
check if the message is:
- Committed
- Verifies
...before using the output of this function as the 'canonical' message. This
is such that expensive operations like signature verification are not
performed unless necessary.

<a name="with_only_committed-2"></a>

### with_only_committed/2 ###

`with_only_committed(Msg, Opts) -> any()`

<a name="with_only_committers-2"></a>

### with_only_committers/2 ###

`with_only_committers(Msg, Committers) -> any()`

Return the message with only the specified committers attached.

<a name="without_commitments-2"></a>

### without_commitments/2 ###

`without_commitments(Spec, Msg) -> any()`

Filter messages that match the 'spec' given. Inverts the `with_commitments/2`
function, such that only messages that do _not_ match the spec are returned.

<a name="without_commitments-3"></a>

### without_commitments/3 * ###

`without_commitments(Spec, Msg, Opts) -> any()`

