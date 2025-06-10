# [Module dev_codec_httpsig_conv.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_httpsig_conv.erl)




A codec for the that marshals TABM encoded messages to and from the
"HTTP" message structure.

<a name="description"></a>

## Description ##

Every HTTP message is an HTTP multipart message.
See https://datatracker.ietf.org/doc/html/rfc7578

For each TABM Key:

The Key/Value Pair will be encoded according to the following rules:
"signatures" -> {SignatureInput, Signature} header Tuples, each encoded
as a Structured Field Dictionary
"body" ->
- if a map, then recursively encode as its own HyperBEAM message
- otherwise encode as a normal field
_ -> encode as a normal field

Each field will be mapped to the HTTP Message according to the following
rules:
"body" -> always encoded part of the body as with Content-Disposition
type of "inline"
_ ->
- If the byte size of the value is less than the ?MAX_TAG_VALUE,
then encode as a header, also attempting to encode as a
structured field.
- Otherwise encode the value as a part in the multipart response
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#boundary_from_parts-1">boundary_from_parts/1*</a></td><td>Generate a unique, reproducible boundary for the
multipart body, however we cannot use the id of the message as
the boundary, as the id is not known until the message is
encoded.</td></tr><tr><td valign="top"><a href="#commitments_from_signature-4">commitments_from_signature/4*</a></td><td>Populate the <code>/commitments</code> key on the TABM with the dictionary of
signatures and their corresponding inputs.</td></tr><tr><td valign="top"><a href="#do_to-2">do_to/2*</a></td><td></td></tr><tr><td valign="top"><a href="#encode_body_keys-1">encode_body_keys/1*</a></td><td>Encode a list of body parts into a binary.</td></tr><tr><td valign="top"><a href="#encode_body_part-3">encode_body_part/3*</a></td><td>Encode a multipart body part to a flat binary.</td></tr><tr><td valign="top"><a href="#encode_http_msg-1">encode_http_msg/1*</a></td><td>Encode a HTTP message into a binary.</td></tr><tr><td valign="top"><a href="#extract_hashpaths-1">extract_hashpaths/1*</a></td><td>Extract all keys labelled <code>hashpath*</code> from the commitments, and add them
to the HTTP message as <code>hashpath*</code> keys.</td></tr><tr><td valign="top"><a href="#field_to_http-3">field_to_http/3*</a></td><td>All maps are encoded into the body of the HTTP message
to be further encoded later.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>Convert a HTTP Message into a TABM.</td></tr><tr><td valign="top"><a href="#from_body-4">from_body/4*</a></td><td></td></tr><tr><td valign="top"><a href="#from_body_parts-3">from_body_parts/3*</a></td><td></td></tr><tr><td valign="top"><a href="#group_ids-1">group_ids/1*</a></td><td>Group all elements with:
1.</td></tr><tr><td valign="top"><a href="#group_maps-1">group_maps/1*</a></td><td>Merge maps at the same level, if possible.</td></tr><tr><td valign="top"><a href="#group_maps-3">group_maps/3*</a></td><td></td></tr><tr><td valign="top"><a href="#group_maps_flat_compatible_test-0">group_maps_flat_compatible_test/0*</a></td><td>The grouped maps encoding is a subset of the flat encoding,
where on keys with maps values are flattened.</td></tr><tr><td valign="top"><a href="#group_maps_test-0">group_maps_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#hashpaths_from_message-1">hashpaths_from_message/1*</a></td><td></td></tr><tr><td valign="top"><a href="#inline_key-1">inline_key/1*</a></td><td>given a message, returns a binary tuple:
- A list of pairs to add to the msg, if any
- the field name for the inlined key.</td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td>Convert a TABM into an HTTP Message.</td></tr><tr><td valign="top"><a href="#to-2">to/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ungroup_ids-1">ungroup_ids/1*</a></td><td>Decode the <code>ao-ids</code> key into a map.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="boundary_from_parts-1"></a>

### boundary_from_parts/1 * ###

`boundary_from_parts(PartList) -> any()`

Generate a unique, reproducible boundary for the
multipart body, however we cannot use the id of the message as
the boundary, as the id is not known until the message is
encoded. Subsequently, we generate each body part individually,
concatenate them, and apply a SHA2-256 hash to the result.
This ensures that the boundary is unique, reproducible, and
secure.

<a name="commitments_from_signature-4"></a>

### commitments_from_signature/4 * ###

`commitments_from_signature(Map, HPs, RawSig, RawSigInput) -> any()`

Populate the `/commitments` key on the TABM with the dictionary of
signatures and their corresponding inputs.

<a name="do_to-2"></a>

### do_to/2 * ###

`do_to(Binary, Opts) -> any()`

<a name="encode_body_keys-1"></a>

### encode_body_keys/1 * ###

`encode_body_keys(PartList) -> any()`

Encode a list of body parts into a binary.

<a name="encode_body_part-3"></a>

### encode_body_part/3 * ###

`encode_body_part(PartName, BodyPart, InlineKey) -> any()`

Encode a multipart body part to a flat binary.

<a name="encode_http_msg-1"></a>

### encode_http_msg/1 * ###

`encode_http_msg(Httpsig) -> any()`

Encode a HTTP message into a binary.

<a name="extract_hashpaths-1"></a>

### extract_hashpaths/1 * ###

`extract_hashpaths(Map) -> any()`

Extract all keys labelled `hashpath*` from the commitments, and add them
to the HTTP message as `hashpath*` keys.

<a name="field_to_http-3"></a>

### field_to_http/3 * ###

`field_to_http(Httpsig, X2, Opts) -> any()`

All maps are encoded into the body of the HTTP message
to be further encoded later.

<a name="from-1"></a>

### from/1 ###

`from(Bin) -> any()`

Convert a HTTP Message into a TABM.
HTTP Structured Field is encoded into it's equivalent TABM encoding.

<a name="from_body-4"></a>

### from_body/4 * ###

`from_body(TABM, InlinedKey, ContentType, Body) -> any()`

<a name="from_body_parts-3"></a>

### from_body_parts/3 * ###

`from_body_parts(TABM, InlinedKey, Rest) -> any()`

<a name="group_ids-1"></a>

### group_ids/1 * ###

`group_ids(Map) -> any()`

Group all elements with:
1. A key that ?IS_ID returns true for, and
2. A value that is immediate
into a combined SF dict-_like_ structure. If not encoded, these keys would
be sent as headers and lower-cased, losing their comparability against the
original keys. The structure follows all SF dict rules, except that it allows
for keys to contain capitals. The HyperBEAM SF parser will accept these keys,
but standard RFC 8741 parsers will not. Subsequently, the resulting `ao-cased`
key is not added to the `ao-types` map.

<a name="group_maps-1"></a>

### group_maps/1 * ###

`group_maps(Map) -> any()`

Merge maps at the same level, if possible.

<a name="group_maps-3"></a>

### group_maps/3 * ###

`group_maps(Map, Parent, Top) -> any()`

<a name="group_maps_flat_compatible_test-0"></a>

### group_maps_flat_compatible_test/0 * ###

`group_maps_flat_compatible_test() -> any()`

The grouped maps encoding is a subset of the flat encoding,
where on keys with maps values are flattened.

So despite needing a special encoder to produce it
We can simply apply the flat encoder to it to get back
the original message.

The test asserts that is indeed the case.

<a name="group_maps_test-0"></a>

### group_maps_test/0 * ###

`group_maps_test() -> any()`

<a name="hashpaths_from_message-1"></a>

### hashpaths_from_message/1 * ###

`hashpaths_from_message(Msg) -> any()`

<a name="inline_key-1"></a>

### inline_key/1 * ###

`inline_key(Msg) -> any()`

given a message, returns a binary tuple:
- A list of pairs to add to the msg, if any
- the field name for the inlined key

In order to preserve the field name of the inlined
part, an additional field may need to be added

<a name="to-1"></a>

### to/1 ###

`to(Bin) -> any()`

Convert a TABM into an HTTP Message. The HTTP Message is a simple Erlang Map
that can translated to a given web server Response API

<a name="to-2"></a>

### to/2 * ###

`to(TABM, Opts) -> any()`

<a name="ungroup_ids-1"></a>

### ungroup_ids/1 * ###

`ungroup_ids(Msg) -> any()`

Decode the `ao-ids` key into a map.

