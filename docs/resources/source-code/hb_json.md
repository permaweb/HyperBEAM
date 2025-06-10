# [Module hb_json.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_json.erl)




Wrapper for encoding and decoding JSON.

<a name="description"></a>

## Description ##
Supports maps and Jiffy's old
`ejson` format. This module abstracts the underlying JSON library, allowing
us to switch between libraries as needed in the future.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td>Takes a JSON string and decodes it into an Erlang term.</td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td>Takes a term in Erlang's native form and encodes it as a JSON string.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

`decode(Bin) -> any()`

Takes a JSON string and decodes it into an Erlang term.

<a name="decode-2"></a>

### decode/2 ###

`decode(Bin, Opts) -> any()`

<a name="encode-1"></a>

### encode/1 ###

`encode(Term) -> any()`

Takes a term in Erlang's native form and encodes it as a JSON string.

