# HTTP Message Signatures

[![Node.js CI](https://github.com/dhensby/node-http-message-signatures/actions/workflows/nodejs.yml/badge.svg)](https://github.com/dhensby/node-http-message-signatures/actions/workflows/nodejs.yml)

This library provides a way to perform HTTP message signing as per the HTTP Working Group draft specification for
[HTTP Message Signatures](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures).

HTTP Message Signatures are designed to provide a way to verify the authenticity and integrity of *parts* of an HTTP
message by performing a deterministic serialisation of components of an HTTP Message. More details can be found in the
specifications.

## Specifications

Two specifications are supported by this library:

1. [HTTP Working Group spec](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures)
2. [Network Working Group spec](https://datatracker.ietf.org/doc/html/draft-cavage-http-signatures)

## Approach

As the Network WG specification is now expired and superseded by the HTTP WG one. This library takes a
"HTTP WG" approach. This means that most support and maintenance will go into the HTTP WG
implementation and syntax. The syntax is then back-ported to the legacy specification as much as possible.

## Caveats

The specifications are in draft and are liable to change over time, introducing new features and removing existing ones.
The aim is to support the [latest version of the specification](https://datatracker.ietf.org/doc/html/draft-richanna-http-message-signatures)
and not to try to support each version in isolation. However, this library was last updated against
[revision 13](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures-13) of the HTTP WG specification.

## Limitations in compliance with the specification

As with many libraries and environments, HTTP Requests and Responses are abstracted away from the
developer. This fact is noted in the specification. As such (in compliance with the specification),
consumers of this library should take care to make sure that they are processing signatures that
only cover fields/components whose values can be reliably resolved. Below is a list of limitations
that you should be aware of when selecting a list of parameters to sign or accept.

### Derived component limitations

Many of the derived components are expected to be sourced from what are effectively http2 pseudo
headers. However, if the application is not running in http2 mode or the message being signed is
not being built as a http2 message, then some of these pseudo headers will not be available to the
application and must be derived from a URL.

#### @request-target

The [`@request-target`](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures#section-2.2.5)
component is intended to be the equivalent to the "request target portion of the request line".
See the specification for examples of what this means. In Node.js, this line in requests is automatically
constructed for consumers, so it's not possible to know for certainty what this will be. For incoming
requests, it is possible to extract, but for simplicity’s sake this library does not process the raw
headers for the incoming request and, as such, cannot calculate this value with certainty. It is
recommended that this component is avoided.

### Multiple message component contexts

As described in [section 7.4.4](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures#section-7.4.4)
it is deemed that complex message context resolution is outside the scope of this library.

This means that it is the responsibility of the consumer of this library to construct the equivalent
message context for signatures that need to be reinterpreted based on other signer contexts.


### Padding attacks

As described in [section 7.5.7](https://datatracker.ietf.org/doc/html/draft-ietf-httpbis-message-signatures-13#section-7.5.7)
it is expected that the Node.js application has taken steps to ensure that headers are valid and not
"garbage". For this library to take on that obligation would be to widen the scope of the library to
a complete HTTP Message validator.

## Examples

> NB: These examples show the "minimal" signature implementation. That is, they provider a proof of possession of the
key by the sender, but don't provide any integrity over the message. To do that, you must add HTTP fields / components
to the signing object. Please see the tests for further examples, or the type definitions.

### Signing a request (Node.js)

This library has built-in signers/verifiers for Node.js using the native `cryto` package to perform all the required
cryptographic operations. However, this is designed to be easily replaced with any other crypto library/runtime 
including `SubtleCrypto` or even a hosted KMS (Key Management Service).

```js
const { httpbis: { signMessage }, createSigner } = require('http-message-signatures');

(async () => {
    // create a signing key using Node's built in crypto engine.
    // you can supply RSA kets, ECDSA, or ED25519 keys.
    const key = createSigner('sharedsecret', 'hmac-sha256', 'my-key-id');
    // minimal signing of a request - more aspects of the request can be signed by providing additional
    // parameters to the first argument of signMessage.
    const signedRequest = await signMessage({
        key,
    }, {
        method: 'POST',
        url: 'https://example.com',
        headers: {
            'content-type': 'application/json',
            'content-digest': 'sha-512=:YMAam51Jz/jOATT6/zvHrLVgOYTGFy1d6GJiOHTohq4yP+pgk4vf2aCsyRZOtw8MjkM7iw7yZ/WkppmM44T3qg==:',
            'content-length': '19',
        },
        body: '{"hello": "world"}\n',
    });
    // signedRequest now has the `Signature` and `Signature-Input` headers
    console.log(signedRequest);
})().catch(console.error);
```

This will output the following object (note the new `Signature` and `Signature-Input` headers):

```js
{
  method: 'POST',
  url: 'https://example.com',
  headers: {
    'content-type': 'application/json',
    'content-digest': 'sha-512=:YMAam51Jz/jOATT6/zvHrLVgOYTGFy1d6GJiOHTohq4yP+pgk4vf2aCsyRZOtw8MjkM7iw7yZ/WkppmM44T3qg==:',
    'content-length': '19',
    'Signature': 'sig=:RkplfaUzQ4xIkSVP9hT+Y55yAYX9VwSeHmjS5X7d0fE=:',
    'Signature-Input': 'sig=();keyid="my-key-id";alg="hmac-sha256";created=1700669009;expires=1700669309'
  },
  body: '{"hello": "world"}\n'
}
```

### Signing with your own signer

It's possible to provide your own signer (this is useful if you're using a secure enclave or key
management service). To do so, you must create an object that conforms to the `SigningKey` interface.

For example, using SubtleCrypto:

```js
const { webcrypto: crypto } = require('node:crypto');

function createMySigner() {
    return {
        id: 'my-key-id',
        alg: 'hmac-sha256',
        async sign(data) {
            const key = await crypto.subtle.importKey('raw', Buffer.from('sharedsecret'), {
                name: 'HMAC',
                hash: 'SHA-256',
            }, true, ['sign', 'verify']);
            return Buffer.from(await crypto.subtle.sign('HMAC', key, data));
        },
    };
}
```

### Verifying a request

Verifying a message requires that there is a key-store that can be used to look-up keys based on the signature parameters,
for example via the signatures `keyid`.

```js
const { httpbis: { verifyMessage }, createVerifier } = require('http-message-signatures');

(async () => {
    // an example keystore for looking up keys by ID
    const keys = new Map();
    keys.set('my-key-id', {
        id: 'my-key-id',
        algs: ['hmac-sha256'],
        // as with signing, you can provide your own verifier here instead of using the built-in helpers
        verify: createVerifier('sharedsecret', 'hmac-sha256'),
    });
    // minimal verification
    const verified = await verifyMessage({
        // logic for finding a key based on the signature parameters
        async keyLookup(params) {
            const keyId = params.keyid;
            // lookup and return key - note, we could also lookup using the alg too (`params.alg`)
            // if there is no key, `verifyMessage()` will throw an error
            return keys.get(keyId);
        },
    }, {
        method: 'POST',
        url: 'https://example.com',
        headers: {
            'content-type': 'application/json',
            'content-digest': 'sha-512=:YMAam51Jz/jOATT6/zvHrLVgOYTGFy1d6GJiOHTohq4yP+pgk4vf2aCsyRZOtw8MjkM7iw7yZ/WkppmM44T3qg==:',
            'content-length': '19',
            'signature': 'sig=:RkplfaUzQ4xIkSVP9hT+Y55yAYX9VwSeHmjS5X7d0fE=:',
            'signature-input': 'sig=();keyid="my-key-id";alg="hmac-sha256";created=1700669009;expires=1700669309',
        },
    });
    console.log(verified);
})().catch(console.error);
```

### Verifying a response with request components

The HTTP Message Signatures specification allows for responses to reference parts of the request and incorporate them
within the signature, tightly binding the response to the request. If you expect that request bound signatures will be
used, you can provide the request as an optional parameter to the `verifyMessage()` method:

```js
const { httpbis: { verifyMessage }, createVerifier } = require('http-message-signatures');

(async () => {
    // an example keystore for looking up keys by ID
    const keys = new Map();
    keys.set('my-key-id', {
        id: 'my-key-id',
        alg: 'hmac-sha256',
        // as with signing, you can provide your own verifier here instead of using the built-in helpers
        verify: createVerifier('sharedsecret', 'hmac-sha256'),
    });
    // minimal verification
    const verified = await verifyMessage({
        // logic for finding a key based on the signature parameters
        async keyLookup(params) {
            const keyId = params.keyid;
            // lookup and return key - note, we could also lookup using the alg too (`params.alg`)
            // if there is no key, `verifyMessage()` will throw an error
            return keys.get(keyId);
        },
    }, {
        // the response
        status: 200,
        headers: {
            'content-type': 'application/json',
            'content-digest': 'sha-512=:YMAam51Jz/jOATT6/zvHrLVgOYTGFy1d6GJiOHTohq4yP+pgk4vf2aCsyRZOtw8MjkM7iw7yZ/WkppmM44T3qg==:',
            'content-length': '19',
            'signature': 'sig=:RkplfaUzQ4xIkSVP9hT+Y55yAYX9VwSeHmjS5X7d0fE=:',
            'signature-input': 'sig=();keyid="my-key-id";alg="hmac-sha256";created=1700669009;expires=1700669309',
        },
    }, {
        // the request
        method: 'POST',
        url: 'https://example.com',
        headers: {
            'content-type': 'application/json',
            'content-digest': 'sha-512=:YMAam51Jz/jOATT6/zvHrLVgOYTGFy1d6GJiOHTohq4yP+pgk4vf2aCsyRZOtw8MjkM7iw7yZ/WkppmM44T3qg==:',
            'content-length': '19',
            'signature': 'sig=:RkplfaUzQ4xIkSVP9hT+Y55yAYX9VwSeHmjS5X7d0fE=:',
            'signature-input': 'sig=();keyid="my-key-id";alg="hmac-sha256";created=1700669009;expires=1700669309',
        },
    });
    console.log(verified);
})().catch(console.error);
```

### Verifying with your own verifier

As with signing, it's possible to provide your own verifier (this is useful if you're running in an environment that
may not have access to Node.js' native `crypto` package). To do so, you must create an object that conforms to the
`VerifyingKey` interface.

For example, using SubtleCrypto:

```js
const { webcrypto: crypto } = require('node:crypto');
const { httpbis: { verifyMessage } } = require('http-message-signatures');

(async () => {
    // an example keystore for looking up keys by ID
    const keys = new Map();
    keys.set('my-key-id', {
        id: 'my-key-id',
        alg: 'hmac-sha256',
        // provide a custom verify function
        async verify(data, signature, parameters) {
            const key = await crypto.subtle.importKey('raw', Buffer.from('sharedsecret'), {
                name: 'HMAC',
                hash: 'SHA-256',
            }, true, ['sign', 'verify']);
            return crypto.subtle.verify('HMAC', key, signature, data);
        },
    });
    // minimal verification
    const verified = await verifyMessage({
        // logic for finding a key based on the signature parameters
        async keyLookup(params) {
            const keyId = params.keyid;
            // lookup and return key - note, we could also lookup using the alg too (`params.alg`)
            // if there is no key, `verifyMessage()` will throw an error
            return keys.get(keyId);
        },
    }, {
        // the request
        method: 'POST',
        url: 'https://example.com',
        headers: {
            'content-type': 'application/json',
            'content-digest': 'sha-512=:YMAam51Jz/jOATT6/zvHrLVgOYTGFy1d6GJiOHTohq4yP+pgk4vf2aCsyRZOtw8MjkM7iw7yZ/WkppmM44T3qg==:',
            'content-length': '19',
            'signature': 'sig=:RkplfaUzQ4xIkSVP9hT+Y55yAYX9VwSeHmjS5X7d0fE=:',
            'signature-input': 'sig=();keyid="my-key-id";alg="hmac-sha256";created=1700669009;expires=1700669309',
        },
    });
    console.log(verified);
})().catch(console.error);
```
