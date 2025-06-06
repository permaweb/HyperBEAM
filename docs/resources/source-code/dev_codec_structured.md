# [Module dev_codec_structured.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_structured.erl)




A device implementing the codec interface (to/1, from/1) for
HyperBEAM's internal, richly typed message format.

<a name="description"></a>

## Description ##

This format mirrors HTTP Structured Fields, aside from its limitations of
compound type depths, as well as limited floating point representations.

As with all AO-Core codecs, its target format (the format it expects to
receive in the `to/1` function, and give in `from/1`) is TABM.

For more details, see the HTTP Structured Fields (RFC-9651) specification.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode_ao_types-2">decode_ao_types/2</a></td><td>Parse the <code>ao-types</code> field of a TABM and return a map of keys and their
types.</td></tr><tr><td valign="top"><a href="#decode_value-2">decode_value/2</a></td><td>Convert non-binary values to binary for serialization.</td></tr><tr><td valign="top"><a href="#encode_ao_types-2">encode_ao_types/2</a></td><td>Generate an <code>ao-types</code> structured field from a map of keys and their
types.</td></tr><tr><td valign="top"><a href="#encode_value-1">encode_value/1</a></td><td>Convert a term to a binary representation, emitting its type for
serialization as a separate tag.</td></tr><tr><td valign="top"><a href="#from-3">from/3</a></td><td>Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).</td></tr><tr><td valign="top"><a href="#implicit_keys-1">implicit_keys/1*</a></td><td>Find the implicit keys of a TABM.</td></tr><tr><td valign="top"><a href="#implicit_keys-2">implicit_keys/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_list_from_ao_types-2">is_list_from_ao_types/2</a></td><td>Determine if the <code>ao-types</code> field of a TABM indicates that the message
is a list.</td></tr><tr><td valign="top"><a href="#linkify_mode-2">linkify_mode/2*</a></td><td>Discern the linkify mode from the request and the options.</td></tr><tr><td valign="top"><a href="#list_encoding_test-0">list_encoding_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#to-3">to/3</a></td><td>Convert a TABM into a native HyperBEAM message.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="commit-3"></a>

### commit/3 ###

`commit(Msg, Req, Opts) -> any()`

<a name="decode_ao_types-2"></a>

### decode_ao_types/2 ###

`decode_ao_types(Msg, Opts) -> any()`

Parse the `ao-types` field of a TABM and return a map of keys and their
types

<a name="decode_value-2"></a>

### decode_value/2 ###

`decode_value(Type, Value) -> any()`

Convert non-binary values to binary for serialization.

<a name="encode_ao_types-2"></a>

### encode_ao_types/2 ###

`encode_ao_types(Types, Opts) -> any()`

Generate an `ao-types` structured field from a map of keys and their
types.

<a name="encode_value-1"></a>

### encode_value/1 ###

`encode_value(Value) -> any()`

Convert a term to a binary representation, emitting its type for
serialization as a separate tag.

<a name="from-3"></a>

### from/3 ###

`from(Bin, Req, Opts) -> any()`

Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).

<a name="implicit_keys-1"></a>

### implicit_keys/1 * ###

`implicit_keys(Req) -> any()`

Find the implicit keys of a TABM.

<a name="implicit_keys-2"></a>

### implicit_keys/2 ###

`implicit_keys(Req, Opts) -> any()`

<a name="is_list_from_ao_types-2"></a>

### is_list_from_ao_types/2 ###

`is_list_from_ao_types(Types, Opts) -> any()`

Determine if the `ao-types` field of a TABM indicates that the message
is a list.

<a name="linkify_mode-2"></a>

### linkify_mode/2 * ###

`linkify_mode(Req, Opts) -> any()`

Discern the linkify mode from the request and the options.

<a name="list_encoding_test-0"></a>

### list_encoding_test/0 * ###

`list_encoding_test() -> any()`

<a name="to-3"></a>

### to/3 ###

`to(Bin, Req, Opts) -> any()`

Convert a TABM into a native HyperBEAM message.

<a name="verify-3"></a>

### verify/3 ###

`verify(Msg, Req, Opts) -> any()`

