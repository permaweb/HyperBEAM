# [Module hb_message_test_vectors.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_message_test_vectors.erl)




A battery of test vectors for message codecs, implementing the
`message@1.0` encoding and commitment APIs.

<a name="description"></a>

## Description ##
Additionally, this module
houses tests that ensure the general functioning of the `hb_message` API.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#basic_message_codec_test-2">basic_message_codec_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#binary_to_binary_test-2">binary_to_binary_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#bundled_and_unbundled_ids_differ_test-2">bundled_and_unbundled_ids_differ_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#bundled_ordering_test-2">bundled_ordering_test/2*</a></td><td>Ensure that a httpsig@1.0 message which is bundled and requests an
invalid ordering of keys is normalized to a valid ordering.</td></tr><tr><td valign="top"><a href="#codec_roundtrip_conversion_is_idempotent_test-2">codec_roundtrip_conversion_is_idempotent_test/2*</a></td><td>Ensure that converting a message to a codec, then back to TABM multiple
times results in the same message being returned.</td></tr><tr><td valign="top"><a href="#codec_test_suite-1">codec_test_suite/1*</a></td><td></td></tr><tr><td valign="top"><a href="#committed_empty_keys_test-2">committed_empty_keys_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#committed_keys_test-2">committed_keys_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#complex_signed_message_test-2">complex_signed_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#deep_multisignature_test-0">deep_multisignature_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#deep_typed_message_id_test-2">deep_typed_message_id_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#deeply_nested_committed_keys_test-0">deeply_nested_committed_keys_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#deeply_nested_message_with_content_test-2">deeply_nested_message_with_content_test/2*</a></td><td>Test that we can convert a 3 layer nested message into a tx record and back.</td></tr><tr><td valign="top"><a href="#deeply_nested_message_with_only_content-2">deeply_nested_message_with_only_content/2*</a></td><td></td></tr><tr><td valign="top"><a href="#default_keys_removed_test-0">default_keys_removed_test/0*</a></td><td>Test that the filter_default_keys/1 function removes TX fields
that have the default values found in the tx record, but not those that
have been set by the user.</td></tr><tr><td valign="top"><a href="#empty_body_test-2">empty_body_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#empty_string_in_nested_tag_test-2">empty_string_in_nested_tag_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_balance_table-3">encode_balance_table/3*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_large_balance_table_test-2">encode_large_balance_table_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_small_balance_table_test-2">encode_small_balance_table_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#find_multiple_commitments_test_disabled-0">find_multiple_commitments_test_disabled/0*</a></td><td></td></tr><tr><td valign="top"><a href="#hashpath_sign_verify_test-2">hashpath_sign_verify_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#id_of_deep_message_and_link_message_match_test-2">id_of_deep_message_and_link_message_match_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#id_of_linked_message_test-2">id_of_linked_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#is_idempotent-3">is_idempotent/3*</a></td><td>Tests a message transforming function to ensure that it is idempotent.</td></tr><tr><td valign="top"><a href="#large_body_committed_keys_test-2">large_body_committed_keys_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#match_modes_test-0">match_modes_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#match_test-2">match_test/2*</a></td><td>Test that the message matching function works.</td></tr><tr><td valign="top"><a href="#message_with_large_keys_test-2">message_with_large_keys_test/2*</a></td><td>Test that the data field is correctly managed when we have multiple
uses for it (the 'data' key itself, as well as keys that cannot fit in
tags).</td></tr><tr><td valign="top"><a href="#message_with_simple_embedded_list_test-2">message_with_simple_embedded_list_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#minimization_test-0">minimization_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_body_list_test-2">nested_body_list_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_empty_map_test-2">nested_empty_map_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_message_with_large_content_test-2">nested_message_with_large_content_test/2*</a></td><td>Test that the data field is correctly managed when we have multiple
uses for it (the 'data' key itself, as well as keys that cannot fit in
tags).</td></tr><tr><td valign="top"><a href="#nested_message_with_large_keys_and_content_test-2">nested_message_with_large_keys_and_content_test/2*</a></td><td>Check that large keys and data fields are correctly handled together.</td></tr><tr><td valign="top"><a href="#nested_message_with_large_keys_test-2">nested_message_with_large_keys_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#nested_structured_fields_test-2">nested_structured_fields_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#priv_survives_conversion_test-2">priv_survives_conversion_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#recursive_nested_list_test-2">recursive_nested_list_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#run_test-0">run_test/0*</a></td><td>Test invocation function, making it easier to run a specific test.</td></tr><tr><td valign="top"><a href="#set_body_codec_test-2">set_body_codec_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#sign_deep_message_from_lazy_cache_read_test-2">sign_deep_message_from_lazy_cache_read_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#sign_links_test-2">sign_links_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#sign_node_message_test-2">sign_node_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_deep_message_test-2">signed_deep_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_list_test-2">signed_list_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_message_encode_decode_verify_test-2">signed_message_encode_decode_verify_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_message_with_derived_components_test-2">signed_message_with_derived_components_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_nested_data_key_test-2">signed_nested_data_key_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_nested_message_with_child_test-2">signed_nested_message_with_child_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_non_bundle_is_bundlable_test-2">signed_non_bundle_is_bundlable_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_only_committed_data_field_test-2">signed_only_committed_data_field_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#signed_with_inner_signed_message_test-2">signed_with_inner_signed_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#simple_nested_message_test-2">simple_nested_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#simple_signed_nested_message_test-2">simple_signed_nested_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#single_layer_message_to_encoding_test-2">single_layer_message_to_encoding_test/2*</a></td><td>Test that we can convert a message into a tx record and back.</td></tr><tr><td valign="top"><a href="#specific_order_deeply_nested_signed_message_test-2">specific_order_deeply_nested_signed_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#specific_order_signed_message_test-2">specific_order_signed_message_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#structured_field_atom_parsing_test-2">structured_field_atom_parsing_test/2*</a></td><td>Structured field parsing tests.</td></tr><tr><td valign="top"><a href="#structured_field_decimal_parsing_test-2">structured_field_decimal_parsing_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#suite_name-1">suite_name/1*</a></td><td>Create a name for a suite from a codec spec.</td></tr><tr><td valign="top"><a href="#suite_test_-0">suite_test_/0*</a></td><td>Organizes a test battery for the <code>hb_message</code> module and its codecs.</td></tr><tr><td valign="top"><a href="#suite_test_opts-0">suite_test_opts/0*</a></td><td>Return a set of options for testing, taking the codec name as an
argument.</td></tr><tr><td valign="top"><a href="#suite_test_opts-1">suite_test_opts/1*</a></td><td></td></tr><tr><td valign="top"><a href="#tabm_conversion_is_idempotent_test-2">tabm_conversion_is_idempotent_test/2*</a></td><td>Ensure that converting a message to/from TABM multiple times repeatedly
does not alter the message's contents.</td></tr><tr><td valign="top"><a href="#test_codecs-0">test_codecs/0*</a></td><td>Return a list of codecs to test.</td></tr><tr><td valign="top"><a href="#test_opts-1">test_opts/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_suite-0">test_suite/0*</a></td><td></td></tr><tr><td valign="top"><a href="#unsigned_id_test-2">unsigned_id_test/2*</a></td><td></td></tr><tr><td valign="top"><a href="#verify_nested_complex_signed_test-2">verify_nested_complex_signed_test/2*</a></td><td>Check that a nested signed message with an embedded typed list can
be further nested and signed.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="basic_message_codec_test-2"></a>

### basic_message_codec_test/2 * ###

`basic_message_codec_test(Codec, Opts) -> any()`

<a name="binary_to_binary_test-2"></a>

### binary_to_binary_test/2 * ###

`binary_to_binary_test(Codec, Opts) -> any()`

<a name="bundled_and_unbundled_ids_differ_test-2"></a>

### bundled_and_unbundled_ids_differ_test/2 * ###

`bundled_and_unbundled_ids_differ_test(Codec, Opts) -> any()`

<a name="bundled_ordering_test-2"></a>

### bundled_ordering_test/2 * ###

`bundled_ordering_test(Codec, Opts) -> any()`

Ensure that a httpsig@1.0 message which is bundled and requests an
invalid ordering of keys is normalized to a valid ordering.

<a name="codec_roundtrip_conversion_is_idempotent_test-2"></a>

### codec_roundtrip_conversion_is_idempotent_test/2 * ###

`codec_roundtrip_conversion_is_idempotent_test(Codec, Opts) -> any()`

Ensure that converting a message to a codec, then back to TABM multiple
times results in the same message being returned. This test differs from its
TABM form, as it shuttles (`to-from-to-...`), while the TABM test repeatedly
encodes in a single direction (`to->to->...`).

<a name="codec_test_suite-1"></a>

### codec_test_suite/1 * ###

`codec_test_suite(Codecs) -> any()`

<a name="committed_empty_keys_test-2"></a>

### committed_empty_keys_test/2 * ###

`committed_empty_keys_test(Codec, Opts) -> any()`

<a name="committed_keys_test-2"></a>

### committed_keys_test/2 * ###

`committed_keys_test(Codec, Opts) -> any()`

<a name="complex_signed_message_test-2"></a>

### complex_signed_message_test/2 * ###

`complex_signed_message_test(Codec, Opts) -> any()`

<a name="deep_multisignature_test-0"></a>

### deep_multisignature_test/0 * ###

`deep_multisignature_test() -> any()`

<a name="deep_typed_message_id_test-2"></a>

### deep_typed_message_id_test/2 * ###

`deep_typed_message_id_test(Codec, Opts) -> any()`

<a name="deeply_nested_committed_keys_test-0"></a>

### deeply_nested_committed_keys_test/0 * ###

`deeply_nested_committed_keys_test() -> any()`

<a name="deeply_nested_message_with_content_test-2"></a>

### deeply_nested_message_with_content_test/2 * ###

`deeply_nested_message_with_content_test(Codec, Opts) -> any()`

Test that we can convert a 3 layer nested message into a tx record and back.

<a name="deeply_nested_message_with_only_content-2"></a>

### deeply_nested_message_with_only_content/2 * ###

`deeply_nested_message_with_only_content(Codec, Opts) -> any()`

<a name="default_keys_removed_test-0"></a>

### default_keys_removed_test/0 * ###

`default_keys_removed_test() -> any()`

Test that the filter_default_keys/1 function removes TX fields
that have the default values found in the tx record, but not those that
have been set by the user.

<a name="empty_body_test-2"></a>

### empty_body_test/2 * ###

`empty_body_test(Codec, Opts) -> any()`

<a name="empty_string_in_nested_tag_test-2"></a>

### empty_string_in_nested_tag_test/2 * ###

`empty_string_in_nested_tag_test(Codec, Opts) -> any()`

<a name="encode_balance_table-3"></a>

### encode_balance_table/3 * ###

`encode_balance_table(Size, Codec, Opts) -> any()`

<a name="encode_large_balance_table_test-2"></a>

### encode_large_balance_table_test/2 * ###

`encode_large_balance_table_test(Codec, Opts) -> any()`

<a name="encode_small_balance_table_test-2"></a>

### encode_small_balance_table_test/2 * ###

`encode_small_balance_table_test(Codec, Opts) -> any()`

<a name="find_multiple_commitments_test_disabled-0"></a>

### find_multiple_commitments_test_disabled/0 * ###

`find_multiple_commitments_test_disabled() -> any()`

<a name="hashpath_sign_verify_test-2"></a>

### hashpath_sign_verify_test/2 * ###

`hashpath_sign_verify_test(Codec, Opts) -> any()`

<a name="id_of_deep_message_and_link_message_match_test-2"></a>

### id_of_deep_message_and_link_message_match_test/2 * ###

`id_of_deep_message_and_link_message_match_test(Codec, Opts) -> any()`

<a name="id_of_linked_message_test-2"></a>

### id_of_linked_message_test/2 * ###

`id_of_linked_message_test(Codec, Opts) -> any()`

<a name="is_idempotent-3"></a>

### is_idempotent/3 * ###

`is_idempotent(Func, Msg, Opts) -> any()`

Tests a message transforming function to ensure that it is idempotent.
Runs the conversion a total of 3 times, ensuring that the result remains
unchanged. This function takes transformation functions that result in
`{ok, Res}`-form messages, as well as bare message results.

<a name="large_body_committed_keys_test-2"></a>

### large_body_committed_keys_test/2 * ###

`large_body_committed_keys_test(Codec, Opts) -> any()`

<a name="match_modes_test-0"></a>

### match_modes_test/0 * ###

`match_modes_test() -> any()`

<a name="match_test-2"></a>

### match_test/2 * ###

`match_test(Codec, Opts) -> any()`

Test that the message matching function works.

<a name="message_with_large_keys_test-2"></a>

### message_with_large_keys_test/2 * ###

`message_with_large_keys_test(Codec, Opts) -> any()`

Test that the data field is correctly managed when we have multiple
uses for it (the 'data' key itself, as well as keys that cannot fit in
tags).

<a name="message_with_simple_embedded_list_test-2"></a>

### message_with_simple_embedded_list_test/2 * ###

`message_with_simple_embedded_list_test(Codec, Opts) -> any()`

<a name="minimization_test-0"></a>

### minimization_test/0 * ###

`minimization_test() -> any()`

<a name="nested_body_list_test-2"></a>

### nested_body_list_test/2 * ###

`nested_body_list_test(Codec, Opts) -> any()`

<a name="nested_empty_map_test-2"></a>

### nested_empty_map_test/2 * ###

`nested_empty_map_test(Codec, Opts) -> any()`

<a name="nested_message_with_large_content_test-2"></a>

### nested_message_with_large_content_test/2 * ###

`nested_message_with_large_content_test(Codec, Opts) -> any()`

Test that the data field is correctly managed when we have multiple
uses for it (the 'data' key itself, as well as keys that cannot fit in
tags).

<a name="nested_message_with_large_keys_and_content_test-2"></a>

### nested_message_with_large_keys_and_content_test/2 * ###

`nested_message_with_large_keys_and_content_test(Codec, Opts) -> any()`

Check that large keys and data fields are correctly handled together.

<a name="nested_message_with_large_keys_test-2"></a>

### nested_message_with_large_keys_test/2 * ###

`nested_message_with_large_keys_test(Codec, Opts) -> any()`

<a name="nested_structured_fields_test-2"></a>

### nested_structured_fields_test/2 * ###

`nested_structured_fields_test(Codec, Opts) -> any()`

<a name="priv_survives_conversion_test-2"></a>

### priv_survives_conversion_test/2 * ###

`priv_survives_conversion_test(Codec, Opts) -> any()`

<a name="recursive_nested_list_test-2"></a>

### recursive_nested_list_test/2 * ###

`recursive_nested_list_test(Codec, Opts) -> any()`

<a name="run_test-0"></a>

### run_test/0 * ###

`run_test() -> any()`

Test invocation function, making it easier to run a specific test.
Disable/enable as needed.

<a name="set_body_codec_test-2"></a>

### set_body_codec_test/2 * ###

`set_body_codec_test(Codec, Opts) -> any()`

<a name="sign_deep_message_from_lazy_cache_read_test-2"></a>

### sign_deep_message_from_lazy_cache_read_test/2 * ###

`sign_deep_message_from_lazy_cache_read_test(Codec, Opts) -> any()`

<a name="sign_links_test-2"></a>

### sign_links_test/2 * ###

`sign_links_test(Codec, Opts) -> any()`

<a name="sign_node_message_test-2"></a>

### sign_node_message_test/2 * ###

`sign_node_message_test(Codec, Opts) -> any()`

<a name="signed_deep_message_test-2"></a>

### signed_deep_message_test/2 * ###

`signed_deep_message_test(Codec, Opts) -> any()`

<a name="signed_list_test-2"></a>

### signed_list_test/2 * ###

`signed_list_test(Codec, Opts) -> any()`

<a name="signed_message_encode_decode_verify_test-2"></a>

### signed_message_encode_decode_verify_test/2 * ###

`signed_message_encode_decode_verify_test(Codec, Opts) -> any()`

<a name="signed_message_with_derived_components_test-2"></a>

### signed_message_with_derived_components_test/2 * ###

`signed_message_with_derived_components_test(Codec, Opts) -> any()`

<a name="signed_nested_data_key_test-2"></a>

### signed_nested_data_key_test/2 * ###

`signed_nested_data_key_test(Codec, Opts) -> any()`

<a name="signed_nested_message_with_child_test-2"></a>

### signed_nested_message_with_child_test/2 * ###

`signed_nested_message_with_child_test(Codec, Opts) -> any()`

<a name="signed_non_bundle_is_bundlable_test-2"></a>

### signed_non_bundle_is_bundlable_test/2 * ###

`signed_non_bundle_is_bundlable_test(Codec, Opts) -> any()`

<a name="signed_only_committed_data_field_test-2"></a>

### signed_only_committed_data_field_test/2 * ###

`signed_only_committed_data_field_test(Codec, Opts) -> any()`

<a name="signed_with_inner_signed_message_test-2"></a>

### signed_with_inner_signed_message_test/2 * ###

`signed_with_inner_signed_message_test(Codec, Opts) -> any()`

<a name="simple_nested_message_test-2"></a>

### simple_nested_message_test/2 * ###

`simple_nested_message_test(Codec, Opts) -> any()`

<a name="simple_signed_nested_message_test-2"></a>

### simple_signed_nested_message_test/2 * ###

`simple_signed_nested_message_test(Codec, Opts) -> any()`

<a name="single_layer_message_to_encoding_test-2"></a>

### single_layer_message_to_encoding_test/2 * ###

`single_layer_message_to_encoding_test(Codec, Opts) -> any()`

Test that we can convert a message into a tx record and back.

<a name="specific_order_deeply_nested_signed_message_test-2"></a>

### specific_order_deeply_nested_signed_message_test/2 * ###

`specific_order_deeply_nested_signed_message_test(RawCodec, Opts) -> any()`

<a name="specific_order_signed_message_test-2"></a>

### specific_order_signed_message_test/2 * ###

`specific_order_signed_message_test(RawCodec, Opts) -> any()`

<a name="structured_field_atom_parsing_test-2"></a>

### structured_field_atom_parsing_test/2 * ###

`structured_field_atom_parsing_test(Codec, Opts) -> any()`

Structured field parsing tests.

<a name="structured_field_decimal_parsing_test-2"></a>

### structured_field_decimal_parsing_test/2 * ###

`structured_field_decimal_parsing_test(Codec, Opts) -> any()`

<a name="suite_name-1"></a>

### suite_name/1 * ###

`suite_name(CodecSpec) -> any()`

Create a name for a suite from a codec spec.

<a name="suite_test_-0"></a>

### suite_test_/0 * ###

`suite_test_() -> any()`

Organizes a test battery for the `hb_message` module and its codecs.

<a name="suite_test_opts-0"></a>

### suite_test_opts/0 * ###

`suite_test_opts() -> any()`

Return a set of options for testing, taking the codec name as an
argument. We do not presently use the codec name in the test, but we may
wish to do so in the future.

<a name="suite_test_opts-1"></a>

### suite_test_opts/1 * ###

`suite_test_opts(OptsName) -> any()`

<a name="tabm_conversion_is_idempotent_test-2"></a>

### tabm_conversion_is_idempotent_test/2 * ###

`tabm_conversion_is_idempotent_test(Codec, Opts) -> any()`

Ensure that converting a message to/from TABM multiple times repeatedly
does not alter the message's contents.

<a name="test_codecs-0"></a>

### test_codecs/0 * ###

`test_codecs() -> any()`

Return a list of codecs to test. Disable these as necessary if you need
to test the functionality of a single codec, etc.

<a name="test_opts-1"></a>

### test_opts/1 * ###

`test_opts(X1) -> any()`

<a name="test_suite-0"></a>

### test_suite/0 * ###

`test_suite() -> any()`

<a name="unsigned_id_test-2"></a>

### unsigned_id_test/2 * ###

`unsigned_id_test(Codec, Opts) -> any()`

<a name="verify_nested_complex_signed_test-2"></a>

### verify_nested_complex_signed_test/2 * ###

`verify_nested_complex_signed_test(Codec, Opts) -> any()`

Check that a nested signed message with an embedded typed list can
be further nested and signed. We then encode and decode the message. This
tests a large portion of the complex type encodings that HyperBEAM uses
together.

