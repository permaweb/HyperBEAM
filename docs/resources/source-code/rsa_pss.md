# [Module rsa_pss.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/rsa_pss.erl)




Distributed under the Mozilla Public License v2.0.

Copyright (c) 2014-2015, Andrew Bennett

__Authors:__ Andrew Bennett ([`andrew@pixid.com`](mailto:andrew@pixid.com)).

<a name="description"></a>

## Description ##
Original available at:
https://github.com/potatosalad/erlang-crypto_rsassa_pss
<a name="types"></a>

## Data Types ##




### <a name="type-rsa_digest_type">rsa_digest_type()</a> ###


<pre><code>
rsa_digest_type() = md5 | sha | sha224 | sha256 | sha384 | sha512
</code></pre>




### <a name="type-rsa_private_key">rsa_private_key()</a> ###


<pre><code>
rsa_private_key() = #RSAPrivateKey{}
</code></pre>




### <a name="type-rsa_public_key">rsa_public_key()</a> ###


<pre><code>
rsa_public_key() = #RSAPublicKey{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dp-2">dp/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ep-2">ep/2*</a></td><td></td></tr><tr><td valign="top"><a href="#int_to_bit_size-1">int_to_bit_size/1*</a></td><td></td></tr><tr><td valign="top"><a href="#int_to_bit_size-2">int_to_bit_size/2*</a></td><td></td></tr><tr><td valign="top"><a href="#int_to_byte_size-1">int_to_byte_size/1*</a></td><td></td></tr><tr><td valign="top"><a href="#int_to_byte_size-2">int_to_byte_size/2*</a></td><td></td></tr><tr><td valign="top"><a href="#mgf1-3">mgf1/3*</a></td><td></td></tr><tr><td valign="top"><a href="#mgf1-5">mgf1/5*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_to_key_size-2">normalize_to_key_size/2*</a></td><td></td></tr><tr><td valign="top"><a href="#pad_to_key_size-2">pad_to_key_size/2*</a></td><td></td></tr><tr><td valign="top"><a href="#sign-3">sign/3</a></td><td></td></tr><tr><td valign="top"><a href="#sign-4">sign/4</a></td><td></td></tr><tr><td valign="top"><a href="#verify-4">verify/4</a></td><td></td></tr><tr><td valign="top"><a href="#verify_legacy-4">verify_legacy/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dp-2"></a>

### dp/2 * ###

`dp(B, X2) -> any()`

<a name="ep-2"></a>

### ep/2 * ###

`ep(B, X2) -> any()`

<a name="int_to_bit_size-1"></a>

### int_to_bit_size/1 * ###

`int_to_bit_size(I) -> any()`

<a name="int_to_bit_size-2"></a>

### int_to_bit_size/2 * ###

`int_to_bit_size(I, B) -> any()`

<a name="int_to_byte_size-1"></a>

### int_to_byte_size/1 * ###

`int_to_byte_size(I) -> any()`

<a name="int_to_byte_size-2"></a>

### int_to_byte_size/2 * ###

`int_to_byte_size(I, B) -> any()`

<a name="mgf1-3"></a>

### mgf1/3 * ###

`mgf1(DigestType, Seed, Len) -> any()`

<a name="mgf1-5"></a>

### mgf1/5 * ###

`mgf1(DigestType, Seed, Len, T, Counter) -> any()`

<a name="normalize_to_key_size-2"></a>

### normalize_to_key_size/2 * ###

`normalize_to_key_size(Bits, A) -> any()`

<a name="pad_to_key_size-2"></a>

### pad_to_key_size/2 * ###

`pad_to_key_size(Bytes, Data) -> any()`

<a name="sign-3"></a>

### sign/3 ###

<pre><code>
sign(Message, DigestType, PrivateKey) -&gt; Signature
</code></pre>

<ul class="definitions"><li><code>Message = binary() | {digest, binary()}</code></li><li><code>DigestType = <a href="#type-rsa_digest_type">rsa_digest_type()</a> | atom()</code></li><li><code>PrivateKey = <a href="#type-rsa_private_key">rsa_private_key()</a></code></li><li><code>Signature = binary()</code></li></ul>

<a name="sign-4"></a>

### sign/4 ###

<pre><code>
sign(Message, DigestType, Salt, PrivateKey) -&gt; Signature
</code></pre>

<ul class="definitions"><li><code>Message = binary() | {digest, binary()}</code></li><li><code>DigestType = <a href="#type-rsa_digest_type">rsa_digest_type()</a> | atom()</code></li><li><code>Salt = binary()</code></li><li><code>PrivateKey = <a href="#type-rsa_private_key">rsa_private_key()</a></code></li><li><code>Signature = binary()</code></li></ul>

<a name="verify-4"></a>

### verify/4 ###

<pre><code>
verify(Message, DigestType, Signature, PublicKey) -&gt; boolean()
</code></pre>

<ul class="definitions"><li><code>Message = binary() | {digest, binary()}</code></li><li><code>DigestType = <a href="#type-rsa_digest_type">rsa_digest_type()</a> | atom()</code></li><li><code>Signature = binary()</code></li><li><code>PublicKey = <a href="#type-rsa_public_key">rsa_public_key()</a></code></li></ul>

<a name="verify_legacy-4"></a>

### verify_legacy/4 ###

`verify_legacy(Message, DigestType, Signature, PublicKey) -> any()`

