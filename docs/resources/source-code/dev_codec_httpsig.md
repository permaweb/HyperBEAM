# [Module dev_codec_httpsig.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_httpsig.erl)




This module implements HTTP Message Signatures as described in RFC-9421
(https://datatracker.ietf.org/doc/html/rfc9421), as an AO-Core device.

<a name="description"></a>

## Description ##
It implements the codec standard (from/1, to/1), as well as the optional
commitment functions (id/3, sign/3, verify/3). The commitment functions
are found in this module, while the codec functions are relayed to the
`dev_codec_httpsig_conv` module.
<a name="types"></a>

## Data Types ##




### <a name="type-authority_state">authority_state()</a> ###


<pre><code>
authority_state() = #{component_identifiers =&gt; [<a href="#type-component_identifier">component_identifier()</a>], sig_params =&gt; <a href="#type-signature_params">signature_params()</a>, key =&gt; binary()}
</code></pre>




### <a name="type-component_identifier">component_identifier()</a> ###


<pre><code>
component_identifier() = {item, {string, binary()}, {binary(), integer() | boolean() | {string | token | binary, binary()}}}
</code></pre>




### <a name="type-fields">fields()</a> ###


<pre><code>
fields() = #{binary() | atom() | string() =&gt; binary() | atom() | string()}
</code></pre>




### <a name="type-request_message">request_message()</a> ###


<pre><code>
request_message() = #{url =&gt; binary(), method =&gt; binary(), headers =&gt; <a href="#type-fields">fields()</a>, trailers =&gt; <a href="#type-fields">fields()</a>, is_absolute_form =&gt; boolean()}
</code></pre>




### <a name="type-response_message">response_message()</a> ###


<pre><code>
response_message() = #{status =&gt; integer(), headers =&gt; <a href="#type-fields">fields()</a>, trailers =&gt; <a href="#type-fields">fields()</a>}
</code></pre>




### <a name="type-signature_params">signature_params()</a> ###


<pre><code>
signature_params() = #{atom() | binary() | string() =&gt; binary() | integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_content_digest-1">add_content_digest/1</a></td><td>If the <code>body</code> key is present, replace it with a content-digest.</td></tr><tr><td valign="top"><a href="#add_derived_specifiers-1">add_derived_specifiers/1</a></td><td>Normalize key parameters to ensure their names are correct.</td></tr><tr><td valign="top"><a href="#add_sig_params-2">add_sig_params/2*</a></td><td>Add the signature parameters to the authority state.</td></tr><tr><td valign="top"><a href="#address_to_sig_name-1">address_to_sig_name/1*</a></td><td>Convert an address to a signature name that is short, unique to the
address, and lowercase.</td></tr><tr><td valign="top"><a href="#authority-3">authority/3*</a></td><td>A helper to validate and produce an "Authority" State.</td></tr><tr><td valign="top"><a href="#bin-1">bin/1*</a></td><td></td></tr><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td>Main entrypoint for signing a HTTP Message, using the standardized format.</td></tr><tr><td valign="top"><a href="#committed-3">committed/3</a></td><td>Return the list of committed keys from a message.</td></tr><tr><td valign="top"><a href="#committed_from_body-1">committed_from_body/1*</a></td><td>Return the list of committed keys from a message that are derived from
the body components.</td></tr><tr><td valign="top"><a href="#committed_id_test-0">committed_id_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#derive_component-3">derive_component/3*</a></td><td>Given a Component Identifier and a Request/Response Messages Context
extract the value represented by the Component Identifier, from the Messages
Context, specifically a "Derived" Component within the Messages Context,
and return the normalized form of the identifier, along with the extracted
encoded value.</td></tr><tr><td valign="top"><a href="#derive_component-4">derive_component/4*</a></td><td></td></tr><tr><td valign="top"><a href="#derive_component_error_query_param_no_name_test-0">derive_component_error_query_param_no_name_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#derive_component_error_req_param_on_request_target_test-0">derive_component_error_req_param_on_request_target_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#derive_component_error_status_req_target_test-0">derive_component_error_status_req_target_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#do_committed-4">do_committed/4*</a></td><td></td></tr><tr><td valign="top"><a href="#extract_dictionary_field_value-2">extract_dictionary_field_value/2*</a></td><td>Extract a value from a Structured Field, and return the normalized field,
along with the encoded value.</td></tr><tr><td valign="top"><a href="#extract_field-3">extract_field/3*</a></td><td>Given a Component Identifier and a Request/Response Messages Context
extract the value represented by the Component Identifier, from the Messages
Context, specifically a field on a Message within the Messages Context,
and return the normalized form of the identifier, along with the extracted
encoded value.</td></tr><tr><td valign="top"><a href="#extract_field_value-2">extract_field_value/2*</a></td><td>Extract values from the field and return the normalized field,
along with encoded value.</td></tr><tr><td valign="top"><a href="#find_byte_sequence_param-1">find_byte_sequence_param/1*</a></td><td></td></tr><tr><td valign="top"><a href="#find_id-1">find_id/1*</a></td><td>Find the ID of the message, which is the hmac of the fields referenced in
the signature and signature input.</td></tr><tr><td valign="top"><a href="#find_key_param-1">find_key_param/1*</a></td><td></td></tr><tr><td valign="top"><a href="#find_name_param-1">find_name_param/1*</a></td><td></td></tr><tr><td valign="top"><a href="#find_request_param-1">find_request_param/1*</a></td><td></td></tr><tr><td valign="top"><a href="#find_sf_param-3">find_sf_param/3*</a></td><td>Given a parameter Name, extract the Parameter value from the HTTP
Structured Field data structure.</td></tr><tr><td valign="top"><a href="#find_strict_format_param-1">find_strict_format_param/1*</a></td><td></td></tr><tr><td valign="top"><a href="#find_trailer_param-1">find_trailer_param/1*</a></td><td></td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td></td></tr><tr><td valign="top"><a href="#hmac-1">hmac/1*</a></td><td>Generate the ID of the message, with the current signature and signature
input as the components for the hmac.</td></tr><tr><td valign="top"><a href="#id-3">id/3</a></td><td></td></tr><tr><td valign="top"><a href="#identifier_to_component-3">identifier_to_component/3*</a></td><td>Given a Component Identifier and a Request/Response Messages Context
extract the value represented by the Component Identifier, from the Messages
Context, and return the normalized form of the identifier, along with the
extracted encoded value.</td></tr><tr><td valign="top"><a href="#join_signature_base-2">join_signature_base/2*</a></td><td></td></tr><tr><td valign="top"><a href="#join_signature_base_test-0">join_signature_base_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#lower_bin-1">lower_bin/1*</a></td><td></td></tr><tr><td valign="top"><a href="#multicommitted_id_test-0">multicommitted_id_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_component_identifiers-1">normalize_component_identifiers/1*</a></td><td>Takes a list of keys that will be used in the signature inputs and
ensures that they have deterministic sorting, as well as the coorect
component identifiers if applicable.</td></tr><tr><td valign="top"><a href="#public_keys-1">public_keys/1</a></td><td></td></tr><tr><td valign="top"><a href="#remove_derived_specifiers-1">remove_derived_specifiers/1</a></td><td>Remove derived specifiers from a list of component identifiers.</td></tr><tr><td valign="top"><a href="#reset_hmac-1">reset_hmac/1</a></td><td>Ensure that the commitments and hmac are properly encoded.</td></tr><tr><td valign="top"><a href="#sf_encode-1">sf_encode/1*</a></td><td>Attempt to encode the data structure into an HTTP Structured Field.</td></tr><tr><td valign="top"><a href="#sf_encode-2">sf_encode/2*</a></td><td></td></tr><tr><td valign="top"><a href="#sf_item-1">sf_item/1*</a></td><td>Attempt to parse the provided value into an HTTP Structured Field Item.</td></tr><tr><td valign="top"><a href="#sf_parse-1">sf_parse/1*</a></td><td>Attempt to parse the binary into a data structure that represents
an HTTP Structured Field.</td></tr><tr><td valign="top"><a href="#sf_parse-2">sf_parse/2*</a></td><td></td></tr><tr><td valign="top"><a href="#sf_signature_param-1">sf_signature_param/1*</a></td><td>construct the structured field Parameter for the signature parameter,
checking whether the parameter name is valid according RFC-9421.</td></tr><tr><td valign="top"><a href="#sf_signature_params-2">sf_signature_params/2*</a></td><td>construct the structured field List for the
"signature-params-line" part of the signature base.</td></tr><tr><td valign="top"><a href="#sig_name_from_dict-1">sig_name_from_dict/1*</a></td><td></td></tr><tr><td valign="top"><a href="#sign_auth-3">sign_auth/3*</a></td><td>using the provided Authority and Request/Response Messages Context,
create a Name, Signature and SignatureInput that can be used to additional
signatures to a corresponding HTTP Message.</td></tr><tr><td valign="top"><a href="#signature_base-3">signature_base/3*</a></td><td>create the signature base that will be signed in order to create the
Signature and SignatureInput.</td></tr><tr><td valign="top"><a href="#signature_components_line-3">signature_components_line/3*</a></td><td>Given a list of Component Identifiers and a Request/Response Message
context, create the "signature-base-line" portion of the signature base.</td></tr><tr><td valign="top"><a href="#signature_params_line-2">signature_params_line/2*</a></td><td>construct the "signature-params-line" part of the signature base.</td></tr><tr><td valign="top"><a href="#signature_params_line_test-0">signature_params_line_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td></td></tr><tr><td valign="top"><a href="#trim_and_normalize-1">trim_and_normalize/1*</a></td><td></td></tr><tr><td valign="top"><a href="#trim_ws-1">trim_ws/1*</a></td><td>Recursively trim space characters from the beginning of the binary.</td></tr><tr><td valign="top"><a href="#trim_ws_end-2">trim_ws_end/2*</a></td><td></td></tr><tr><td valign="top"><a href="#trim_ws_test-0">trim_ws_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#upper_bin-1">upper_bin/1*</a></td><td></td></tr><tr><td valign="top"><a href="#validate_large_message_from_http_test-0">validate_large_message_from_http_test/0*</a></td><td>Ensure that we can validate a signature on an extremely large and complex
message that is sent over HTTP, signed with the codec.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td>Verify different forms of httpsig committed messages.</td></tr><tr><td valign="top"><a href="#verify_auth-2">verify_auth/2*</a></td><td>same verify/3, but with an empty Request Message Context.</td></tr><tr><td valign="top"><a href="#verify_auth-3">verify_auth/3*</a></td><td>Given the signature name, and the Request/Response Message Context
verify the named signature by constructing the signature base and comparing.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_content_digest-1"></a>

### add_content_digest/1 ###

`add_content_digest(Msg) -> any()`

If the `body` key is present, replace it with a content-digest.

<a name="add_derived_specifiers-1"></a>

### add_derived_specifiers/1 ###

`add_derived_specifiers(ComponentIdentifiers) -> any()`

Normalize key parameters to ensure their names are correct.

<a name="add_sig_params-2"></a>

### add_sig_params/2 * ###

`add_sig_params(Authority, X2) -> any()`

Add the signature parameters to the authority state

<a name="address_to_sig_name-1"></a>

### address_to_sig_name/1 * ###

<pre><code>
address_to_sig_name(Address::binary()) -&gt; binary()
</code></pre>
<br />

Convert an address to a signature name that is short, unique to the
address, and lowercase.

<a name="authority-3"></a>

### authority/3 * ###

<pre><code>
authority(ComponentIdentifiers::[binary() | <a href="#type-component_identifier">component_identifier()</a>], SigParams::#{binary() =&gt; binary() | integer()}, PubKey::{}) -&gt; <a href="#type-authority_state">authority_state()</a>
</code></pre>
<br />

A helper to validate and produce an "Authority" State

<a name="bin-1"></a>

### bin/1 * ###

`bin(Item) -> any()`

<a name="commit-3"></a>

### commit/3 ###

`commit(MsgToSign, Req, Opts) -> any()`

Main entrypoint for signing a HTTP Message, using the standardized format.

<a name="committed-3"></a>

### committed/3 ###

`committed(RawMsg, Req, Opts) -> any()`

Return the list of committed keys from a message. The message will have
had the `commitments` key removed and the signature inputs added to the
root. Subsequently, we can parse that to get the list of committed keys.

<a name="committed_from_body-1"></a>

### committed_from_body/1 * ###

`committed_from_body(Msg) -> any()`

Return the list of committed keys from a message that are derived from
the body components.

<a name="committed_id_test-0"></a>

### committed_id_test/0 * ###

`committed_id_test() -> any()`

<a name="derive_component-3"></a>

### derive_component/3 * ###

`derive_component(Identifier, Req, Res) -> any()`

Given a Component Identifier and a Request/Response Messages Context
extract the value represented by the Component Identifier, from the Messages
Context, specifically a "Derived" Component within the Messages Context,
and return the normalized form of the identifier, along with the extracted
encoded value.

This implements a portion of RFC-9421
See https://datatracker.ietf.org/doc/html/rfc9421#name-derived-components

<a name="derive_component-4"></a>

### derive_component/4 * ###

`derive_component(X1, Req, Res, Subject) -> any()`

<a name="derive_component_error_query_param_no_name_test-0"></a>

### derive_component_error_query_param_no_name_test/0 * ###

`derive_component_error_query_param_no_name_test() -> any()`

<a name="derive_component_error_req_param_on_request_target_test-0"></a>

### derive_component_error_req_param_on_request_target_test/0 * ###

`derive_component_error_req_param_on_request_target_test() -> any()`

<a name="derive_component_error_status_req_target_test-0"></a>

### derive_component_error_status_req_target_test/0 * ###

`derive_component_error_status_req_target_test() -> any()`

<a name="do_committed-4"></a>

### do_committed/4 * ###

`do_committed(SigInputStr, Msg, Req, Opts) -> any()`

<a name="extract_dictionary_field_value-2"></a>

### extract_dictionary_field_value/2 * ###

`extract_dictionary_field_value(StructuredField, Key) -> any()`

Extract a value from a Structured Field, and return the normalized field,
along with the encoded value

<a name="extract_field-3"></a>

### extract_field/3 * ###

`extract_field(X1, Req, Res) -> any()`

Given a Component Identifier and a Request/Response Messages Context
extract the value represented by the Component Identifier, from the Messages
Context, specifically a field on a Message within the Messages Context,
and return the normalized form of the identifier, along with the extracted
encoded value.

This implements a portion of RFC-9421
See https://datatracker.ietf.org/doc/html/rfc9421#name-http-fields

<a name="extract_field_value-2"></a>

### extract_field_value/2 * ###

`extract_field_value(RawFields, X2) -> any()`

Extract values from the field and return the normalized field,
along with encoded value

<a name="find_byte_sequence_param-1"></a>

### find_byte_sequence_param/1 * ###

`find_byte_sequence_param(Params) -> any()`

<a name="find_id-1"></a>

### find_id/1 * ###

`find_id(Msg) -> any()`

Find the ID of the message, which is the hmac of the fields referenced in
the signature and signature input. If the message already has a signature-input,
directly, it is treated differently: We relabel it as `x-signature-input` to
avoid key collisions.

<a name="find_key_param-1"></a>

### find_key_param/1 * ###

`find_key_param(Params) -> any()`

<a name="find_name_param-1"></a>

### find_name_param/1 * ###

`find_name_param(Params) -> any()`

<a name="find_request_param-1"></a>

### find_request_param/1 * ###

`find_request_param(Params) -> any()`

<a name="find_sf_param-3"></a>

### find_sf_param/3 * ###

`find_sf_param(Name, Params, Default) -> any()`

Given a parameter Name, extract the Parameter value from the HTTP
Structured Field data structure.

If no value is found, then false is returned

<a name="find_strict_format_param-1"></a>

### find_strict_format_param/1 * ###

`find_strict_format_param(Params) -> any()`

<a name="find_trailer_param-1"></a>

### find_trailer_param/1 * ###

`find_trailer_param(Params) -> any()`

<a name="from-1"></a>

### from/1 ###

`from(Msg) -> any()`

<a name="hmac-1"></a>

### hmac/1 * ###

`hmac(Msg) -> any()`

Generate the ID of the message, with the current signature and signature
input as the components for the hmac.

<a name="id-3"></a>

### id/3 ###

`id(Msg, Params, Opts) -> any()`

<a name="identifier_to_component-3"></a>

### identifier_to_component/3 * ###

`identifier_to_component(Identifier, Req, Res) -> any()`

Given a Component Identifier and a Request/Response Messages Context
extract the value represented by the Component Identifier, from the Messages
Context, and return the normalized form of the identifier, along with the
extracted encoded value.

Generally speaking, a Component Identifier may reference a "Derived" Component,
a Message Field, or a sub-component of a Message Field.

Since a Component Identifier is itself a Structured Field, it may also specify
parameters, which are used to describe behavior such as which Message to
derive a field or sub-component of the field, and how to encode the value as
part of the signature base.

<a name="join_signature_base-2"></a>

### join_signature_base/2 * ###

`join_signature_base(ComponentsLine, ParamsLine) -> any()`

<a name="join_signature_base_test-0"></a>

### join_signature_base_test/0 * ###

`join_signature_base_test() -> any()`

<a name="lower_bin-1"></a>

### lower_bin/1 * ###

`lower_bin(Item) -> any()`

<a name="multicommitted_id_test-0"></a>

### multicommitted_id_test/0 * ###

`multicommitted_id_test() -> any()`

<a name="normalize_component_identifiers-1"></a>

### normalize_component_identifiers/1 * ###

`normalize_component_identifiers(ComponentIdentifiers) -> any()`

Takes a list of keys that will be used in the signature inputs and
ensures that they have deterministic sorting, as well as the coorect
component identifiers if applicable.

<a name="public_keys-1"></a>

### public_keys/1 ###

`public_keys(Commitment) -> any()`

<a name="remove_derived_specifiers-1"></a>

### remove_derived_specifiers/1 ###

`remove_derived_specifiers(ComponentIdentifiers) -> any()`

Remove derived specifiers from a list of component identifiers.

<a name="reset_hmac-1"></a>

### reset_hmac/1 ###

`reset_hmac(RawMsg) -> any()`

Ensure that the commitments and hmac are properly encoded

<a name="sf_encode-1"></a>

### sf_encode/1 * ###

`sf_encode(StructuredField) -> any()`

Attempt to encode the data structure into an HTTP Structured Field.
This is the inverse of sf_parse.

<a name="sf_encode-2"></a>

### sf_encode/2 * ###

`sf_encode(Serializer, StructuredField) -> any()`

<a name="sf_item-1"></a>

### sf_item/1 * ###

`sf_item(SfItem) -> any()`

Attempt to parse the provided value into an HTTP Structured Field Item

<a name="sf_parse-1"></a>

### sf_parse/1 * ###

`sf_parse(Raw) -> any()`

Attempt to parse the binary into a data structure that represents
an HTTP Structured Field.

Lacking some sort of "hint", there isn't a way to know which "kind" of
Structured Field the binary is, apriori. So we simply try each parser,
and return the first invocation that doesn't result in an error.

If no parser is successful, then we return an error tuple

<a name="sf_parse-2"></a>

### sf_parse/2 * ###

`sf_parse(Rest, Raw) -> any()`

<a name="sf_signature_param-1"></a>

### sf_signature_param/1 * ###

`sf_signature_param(X1) -> any()`

construct the structured field Parameter for the signature parameter,
checking whether the parameter name is valid according RFC-9421

See https://datatracker.ietf.org/doc/html/rfc9421#section-2.3-3

<a name="sf_signature_params-2"></a>

### sf_signature_params/2 * ###

`sf_signature_params(ComponentIdentifiers, SigParams) -> any()`

construct the structured field List for the
"signature-params-line" part of the signature base.

Can be parsed into a binary by simply passing to hb_structured_fields:list/1

See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4

<a name="sig_name_from_dict-1"></a>

### sig_name_from_dict/1 * ###

`sig_name_from_dict(DictBin) -> any()`

<a name="sign_auth-3"></a>

### sign_auth/3 * ###

<pre><code>
sign_auth(Authority::<a href="#type-authority_state">authority_state()</a>, Req::<a href="#type-request_message">request_message()</a>, Res::<a href="#type-response_message">response_message()</a>) -&gt; {ok, {binary(), binary(), binary()}}
</code></pre>
<br />

using the provided Authority and Request/Response Messages Context,
create a Name, Signature and SignatureInput that can be used to additional
signatures to a corresponding HTTP Message

<a name="signature_base-3"></a>

### signature_base/3 * ###

`signature_base(Authority, Req, Res) -> any()`

create the signature base that will be signed in order to create the
Signature and SignatureInput.

This implements a portion of RFC-9421 see:
https://datatracker.ietf.org/doc/html/rfc9421#name-creating-the-signature-base

<a name="signature_components_line-3"></a>

### signature_components_line/3 * ###

`signature_components_line(ComponentIdentifiers, Req, Res) -> any()`

Given a list of Component Identifiers and a Request/Response Message
context, create the "signature-base-line" portion of the signature base

<a name="signature_params_line-2"></a>

### signature_params_line/2 * ###

`signature_params_line(ComponentIdentifiers, SigParams) -> any()`

construct the "signature-params-line" part of the signature base.

See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4

<a name="signature_params_line_test-0"></a>

### signature_params_line_test/0 * ###

`signature_params_line_test() -> any()`

<a name="to-1"></a>

### to/1 ###

`to(Msg) -> any()`

<a name="trim_and_normalize-1"></a>

### trim_and_normalize/1 * ###

`trim_and_normalize(Bin) -> any()`

<a name="trim_ws-1"></a>

### trim_ws/1 * ###

`trim_ws(Bin) -> any()`

Recursively trim space characters from the beginning of the binary

<a name="trim_ws_end-2"></a>

### trim_ws_end/2 * ###

`trim_ws_end(Value, N) -> any()`

<a name="trim_ws_test-0"></a>

### trim_ws_test/0 * ###

`trim_ws_test() -> any()`

<a name="upper_bin-1"></a>

### upper_bin/1 * ###

`upper_bin(Item) -> any()`

<a name="validate_large_message_from_http_test-0"></a>

### validate_large_message_from_http_test/0 * ###

`validate_large_message_from_http_test() -> any()`

Ensure that we can validate a signature on an extremely large and complex
message that is sent over HTTP, signed with the codec.

<a name="verify-3"></a>

### verify/3 ###

`verify(MsgToVerify, Req, Opts) -> any()`

Verify different forms of httpsig committed messages. `dev_message:verify`
already places the keys from the commitment message into the root of the
message.

<a name="verify_auth-2"></a>

### verify_auth/2 * ###

`verify_auth(Verifier, Msg) -> any()`

same verify/3, but with an empty Request Message Context

<a name="verify_auth-3"></a>

### verify_auth/3 * ###

`verify_auth(X1, Req, Res) -> any()`

Given the signature name, and the Request/Response Message Context
verify the named signature by constructing the signature base and comparing

