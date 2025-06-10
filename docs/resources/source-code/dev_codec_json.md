# [Module dev_codec_json.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_json.erl)




A simple JSON codec for HyperBEAM's message format.

<a name="description"></a>

## Description ##
Takes a
message as TABM and returns an encoded JSON string representation.
This codec utilizes the httpsig@1.0 codec for signing and verifying.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td></td></tr><tr><td valign="top"><a href="#committed-1">committed/1</a></td><td></td></tr><tr><td valign="top"><a href="#content_type-1">content_type/1</a></td><td>Return the content type for the codec.</td></tr><tr><td valign="top"><a href="#deserialize-3">deserialize/3</a></td><td>Deserialize the JSON string found at the given path.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>Decode a JSON string to a message.</td></tr><tr><td valign="top"><a href="#serialize-3">serialize/3</a></td><td>Serialize a message to a JSON string.</td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td>Encode a message to a JSON string.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="commit-3"></a>

### commit/3 ###

`commit(Msg, Req, Opts) -> any()`

<a name="committed-1"></a>

### committed/1 ###

`committed(Msg) -> any()`

<a name="content_type-1"></a>

### content_type/1 ###

`content_type(X1) -> any()`

Return the content type for the codec.

<a name="deserialize-3"></a>

### deserialize/3 ###

`deserialize(Base, Req, Opts) -> any()`

Deserialize the JSON string found at the given path.

<a name="from-1"></a>

### from/1 ###

`from(Map) -> any()`

Decode a JSON string to a message.

<a name="serialize-3"></a>

### serialize/3 ###

`serialize(Base, Msg, Opts) -> any()`

Serialize a message to a JSON string.

<a name="to-1"></a>

### to/1 ###

`to(Msg) -> any()`

Encode a message to a JSON string.

<a name="verify-3"></a>

### verify/3 ###

`verify(Msg, Req, Opts) -> any()`

