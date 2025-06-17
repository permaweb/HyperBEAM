# [Module dev_codec_httpsig.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_codec_httpsig.erl)




This module implements HTTP Message Signatures as described in RFC-9421
(https://datatracker.ietf.org/doc/html/rfc9421), as an AO-Core device.

<a name="description"></a>

## Description ##
It implements the codec standard (from/1, to/1), as well as the optional
commitment functions (id/3, sign/3, verify/3). The commitment functions
are found in this module, while the codec functions are relayed to the
`dev_codec_httpsig_conv` module.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_content_digest-2">add_content_digest/2</a></td><td>If the <code>body</code> key is present and a binary, replace it with a
content-digest.</td></tr><tr><td valign="top"><a href="#commit-3">commit/3</a></td><td>Commit to a message using the HTTP-Signature format.</td></tr><tr><td valign="top"><a href="#committed_id_test-0">committed_id_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#from-3">from/3</a></td><td></td></tr><tr><td valign="top"><a href="#key_present-2">key_present/2*</a></td><td>Calculate if a key or its <code>+link</code> TABM variant is present in a message.</td></tr><tr><td valign="top"><a href="#keys_to_commit-3">keys_to_commit/3*</a></td><td>Derive the set of keys to commit to from a <code>commit</code> request and a
base message.</td></tr><tr><td valign="top"><a href="#maybe_bundle_tag_commitment-3">maybe_bundle_tag_commitment/3*</a></td><td>Annotate the commitment with the <code>bundle</code> key if the request contains
it.</td></tr><tr><td valign="top"><a href="#multicommitted_id_test-0">multicommitted_id_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_for_encoding-3">normalize_for_encoding/3</a></td><td>Given a base message and a commitment, derive the message and commitment
normalized for encoding.</td></tr><tr><td valign="top"><a href="#opts-1">opts/1*</a></td><td>Generate the <code>Opts</code> to use during AO-Core operations in the codec.</td></tr><tr><td valign="top"><a href="#serialize-2">serialize/2</a></td><td>A helper utility for creating a direct encoding of a HTTPSig message.</td></tr><tr><td valign="top"><a href="#serialize-3">serialize/3</a></td><td></td></tr><tr><td valign="top"><a href="#sign_and_verify_link_test-0">sign_and_verify_link_test/0*</a></td><td>Test that we can sign and verify a message with a link.</td></tr><tr><td valign="top"><a href="#signature_base-3">signature_base/3*</a></td><td>create the signature base that will be signed in order to create the
Signature and SignatureInput.</td></tr><tr><td valign="top"><a href="#signature_components_line-3">signature_components_line/3*</a></td><td>Given a list of Component Identifiers and a Request/Response Message
context, create the "signature-base-line" portion of the signature base.</td></tr><tr><td valign="top"><a href="#signature_params_line-2">signature_params_line/2*</a></td><td>construct the "signature-params-line" part of the signature base.</td></tr><tr><td valign="top"><a href="#to-3">to/3</a></td><td></td></tr><tr><td valign="top"><a href="#validate_large_message_from_http_test-0">validate_large_message_from_http_test/0*</a></td><td>Ensure that we can validate a signature on an extremely large and complex
message that is sent over HTTP, signed with the codec.</td></tr><tr><td valign="top"><a href="#verify-3">verify/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_content_digest-2"></a>

### add_content_digest/2 ###

`add_content_digest(Msg, Opts) -> any()`

If the `body` key is present and a binary, replace it with a
content-digest.

<a name="commit-3"></a>

### commit/3 ###

`commit(Msg, Req, Opts) -> any()`

Commit to a message using the HTTP-Signature format. We use the `type`
parameter to determine the type of commitment to use. If the `type` parameter
is `signed`, we default to the rsa-pss-sha512 algorithm. If the `type`
parameter is `unsigned`, we default to the hmac-sha256 algorithm.

<a name="committed_id_test-0"></a>

### committed_id_test/0 * ###

`committed_id_test() -> any()`

<a name="from-3"></a>

### from/3 ###

`from(Msg, Req, Opts) -> any()`

<a name="key_present-2"></a>

### key_present/2 * ###

`key_present(Key, Msg) -> any()`

Calculate if a key or its `+link` TABM variant is present in a message.

<a name="keys_to_commit-3"></a>

### keys_to_commit/3 * ###

`keys_to_commit(Base, Req, Opts) -> any()`

Derive the set of keys to commit to from a `commit` request and a
base message.

<a name="maybe_bundle_tag_commitment-3"></a>

### maybe_bundle_tag_commitment/3 * ###

`maybe_bundle_tag_commitment(Commitment, Req, Opts) -> any()`

Annotate the commitment with the `bundle` key if the request contains
it.

<a name="multicommitted_id_test-0"></a>

### multicommitted_id_test/0 * ###

`multicommitted_id_test() -> any()`

<a name="normalize_for_encoding-3"></a>

### normalize_for_encoding/3 ###

`normalize_for_encoding(Msg, Commitment, Opts) -> any()`

Given a base message and a commitment, derive the message and commitment
normalized for encoding.

<a name="opts-1"></a>

### opts/1 * ###

`opts(RawOpts) -> any()`

Generate the `Opts` to use during AO-Core operations in the codec.

<a name="serialize-2"></a>

### serialize/2 ###

`serialize(Msg, Opts) -> any()`

A helper utility for creating a direct encoding of a HTTPSig message.

This function supports two modes of operation:
1. `format: binary`, yielding a raw binary HTTP/1.1-style response that can
either be stored or emitted raw accross a transport medium.
2. `format: components`, yielding a message containing `headers` and `body`
keys, suitable for use in connecting to HTTP-response flows implemented
by other servers.

Optionally, the `index` key can be set to override resolution of the default
index page into HTTP responses that do not contain their own `body` field.

<a name="serialize-3"></a>

### serialize/3 ###

`serialize(Msg, Req, Opts) -> any()`

<a name="sign_and_verify_link_test-0"></a>

### sign_and_verify_link_test/0 * ###

`sign_and_verify_link_test() -> any()`

Test that we can sign and verify a message with a link. We use

<a name="signature_base-3"></a>

### signature_base/3 * ###

`signature_base(EncodedMsg, Commitment, Opts) -> any()`

create the signature base that will be signed in order to create the
Signature and SignatureInput.

This implements a portion of RFC-9421 see:
https://datatracker.ietf.org/doc/html/rfc9421#name-creating-the-signature-base

<a name="signature_components_line-3"></a>

### signature_components_line/3 * ###

`signature_components_line(Req, Commitment, Opts) -> any()`

Given a list of Component Identifiers and a Request/Response Message
context, create the "signature-base-line" portion of the signature base

<a name="signature_params_line-2"></a>

### signature_params_line/2 * ###

`signature_params_line(RawCommitment, Opts) -> any()`

construct the "signature-params-line" part of the signature base.

See https://datatracker.ietf.org/doc/html/rfc9421#section-2.5-7.3.2.4

<a name="to-3"></a>

### to/3 ###

`to(Msg, Req, Opts) -> any()`

<a name="validate_large_message_from_http_test-0"></a>

### validate_large_message_from_http_test/0 * ###

`validate_large_message_from_http_test() -> any()`

Ensure that we can validate a signature on an extremely large and complex
message that is sent over HTTP, signed with the codec.

<a name="verify-3"></a>

### verify/3 ###

`verify(Base, Req, RawOpts) -> any()`

