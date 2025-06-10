# [Module hb_escape.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_escape.erl)




Escape and unescape mixed case values for use in HTTP headers.

<a name="description"></a>

## Description ##
This is necessary for encodings of AO-Core messages for transmission in
HTTP/2 and HTTP/3, because uppercase header keys are explicitly disallowed.
While most map keys in HyperBEAM are normalized to lowercase, IDs are not.
Subsequently, we encode all header keys to lowercase %-encoded URI-style
strings because transmission.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Decode a URI-encoded string back to a binary.</td></tr><tr><td valign="top"><a href="#decode_keys-1">decode_keys/1</a></td><td>Return a message with all of its keys decoded.</td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Encode a binary as a URI-encoded string.</td></tr><tr><td valign="top"><a href="#encode_keys-1">encode_keys/1</a></td><td>URI encode keys in the base layer of a message.</td></tr><tr><td valign="top"><a href="#escape_byte-1">escape_byte/1*</a></td><td>Escape a single byte as a URI-encoded string.</td></tr><tr><td valign="top"><a href="#escape_unescape_identity_test-0">escape_unescape_identity_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#escape_unescape_special_chars_test-0">escape_unescape_special_chars_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#hex_digit-1">hex_digit/1*</a></td><td></td></tr><tr><td valign="top"><a href="#hex_value-1">hex_value/1*</a></td><td></td></tr><tr><td valign="top"><a href="#percent_escape-1">percent_escape/1*</a></td><td>Escape a list of characters as a URI-encoded string.</td></tr><tr><td valign="top"><a href="#percent_unescape-1">percent_unescape/1*</a></td><td>Unescape a URI-encoded string.</td></tr><tr><td valign="top"><a href="#unescape_specific_test-0">unescape_specific_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#uppercase_test-0">uppercase_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

`decode(Bin) -> any()`

Decode a URI-encoded string back to a binary.

<a name="decode_keys-1"></a>

### decode_keys/1 ###

`decode_keys(Msg) -> any()`

Return a message with all of its keys decoded.

<a name="encode-1"></a>

### encode/1 ###

`encode(Bin) -> any()`

Encode a binary as a URI-encoded string.

<a name="encode_keys-1"></a>

### encode_keys/1 ###

`encode_keys(Msg) -> any()`

URI encode keys in the base layer of a message. Does not recurse.

<a name="escape_byte-1"></a>

### escape_byte/1 * ###

`escape_byte(C) -> any()`

Escape a single byte as a URI-encoded string.

<a name="escape_unescape_identity_test-0"></a>

### escape_unescape_identity_test/0 * ###

`escape_unescape_identity_test() -> any()`

<a name="escape_unescape_special_chars_test-0"></a>

### escape_unescape_special_chars_test/0 * ###

`escape_unescape_special_chars_test() -> any()`

<a name="hex_digit-1"></a>

### hex_digit/1 * ###

`hex_digit(N) -> any()`

<a name="hex_value-1"></a>

### hex_value/1 * ###

`hex_value(C) -> any()`

<a name="percent_escape-1"></a>

### percent_escape/1 * ###

`percent_escape(Cs) -> any()`

Escape a list of characters as a URI-encoded string.

<a name="percent_unescape-1"></a>

### percent_unescape/1 * ###

`percent_unescape(Cs) -> any()`

Unescape a URI-encoded string.

<a name="unescape_specific_test-0"></a>

### unescape_specific_test/0 * ###

`unescape_specific_test() -> any()`

<a name="uppercase_test-0"></a>

### uppercase_test/0 * ###

`uppercase_test() -> any()`

