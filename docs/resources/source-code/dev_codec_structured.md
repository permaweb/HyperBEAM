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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td></td></tr><tr><td valign="top"><a href="#committed-3">committed/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode_value-2">decode_value/2</a></td><td>Convert non-binary values to binary for serialization.</td></tr><tr><td valign="top"><a href="#encode_value-1">encode_value/1</a></td><td>Convert a term to a binary representation, emitting its type for
serialization as a separate tag.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).</td></tr><tr><td valign="top"><a href="#implicit_keys-1">implicit_keys/1</a></td><td>Find the implicit keys of a TABM.</td></tr><tr><td valign="top"><a href="#list_encoding_test-0">list_encoding_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#parse_ao_types-1">parse_ao_types/1*</a></td><td>Parse the <code>ao-types</code> field of a TABM and return a map of keys and their
types.</td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td>Convert a TABM into a native HyperBEAM message.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="commit-3"></a>

### commit/3 ###

`commit(Msg, Req, Opts) -> any()`

<a name="committed-3"></a>

### committed/3 ###

`committed(Msg, Req, Opts) -> any()`

<a name="decode_value-2"></a>

### decode_value/2 ###

`decode_value(Type, Value) -> any()`

Convert non-binary values to binary for serialization.

<a name="encode_value-1"></a>

### encode_value/1 ###

`encode_value(Value) -> any()`

Convert a term to a binary representation, emitting its type for
serialization as a separate tag.

<a name="from-1"></a>

### from/1 ###

`from(Bin) -> any()`

Convert a rich message into a 'Type-Annotated-Binary-Message' (TABM).

<a name="implicit_keys-1"></a>

### implicit_keys/1 ###

`implicit_keys(Req) -> any()`

Find the implicit keys of a TABM.

<a name="list_encoding_test-0"></a>

### list_encoding_test/0 * ###

`list_encoding_test() -> any()`

<a name="parse_ao_types-1"></a>

### parse_ao_types/1 * ###

`parse_ao_types(Msg) -> any()`

Parse the `ao-types` field of a TABM and return a map of keys and their
types

<a name="to-1"></a>

### to/1 ###

`to(Bin) -> any()`

Convert a TABM into a native HyperBEAM message.

<a name="verify-3"></a>

### verify/3 ###

`verify(Msg, Req, Opts) -> any()`

