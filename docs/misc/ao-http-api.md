# AO-Core HTTP API Design notes
### Date: 11 Jan, 2025.

The AO-Core protocol is designed to layer a computation system on top of
HTTP Semantics. As such, it offers syntax for HTTP requests that allow users to
easily manipulate and traverse through the computation graph. This document
describes the semantics and syntax of the AO-Core HTTP API.

## Semantics and syntax

- All paths for typical computations start with a base hashpath:
```
GET /hashpath1/...
```
- All keys after the base are interpreted as AO-Core messages individually, in 
a chain. For example, `GET /hashpath1/key1/key2` is equivalent to:
```
GET /hashpath(hashpath1, key1)/key2
```
- Each path segment is interpreted as a key to resolve upon the message for the
resolution of the previous message. The `key` is taken as the full path for
that message during that resolution.
- When AO-Core is not given a base hashpath (the first key is not a 43 character
base64URL encoded string), the request message is assumed to be its own base
message, with the path applied alone as the request (`Message2`).
- Query parameters are treated as equivalent to headers. A query parameter
replaces a header of the same name, unless the request's `signature-input`
names that header. If the request has no `path` key or header, its query
parameters are also added to the request, replacing any keys of the same name,
signed or not.
- When not otherwise specified, a query parameter or header is interpreted as
being present in all messages in the path, in addition to the path key.
- `+` in path segments delimits the `key` and an optional message dictionary. 
For example, `GET /hashpath1/key1+dict1=val1/key2` adds `dict1=val1` to the
message for the resolution of the `key1` key.
- `N.KeyName` in headers or query parameters is interpreted as `KeyName` in the
message dictionary for the resolution of the `N`th key.
- `Key~DevName` in a path segment is interpreted as executing the message with
the `Device` set to `DevName`.
- A dictionary key of form `Key|Type` is interpreted as a type annotation for
`Key`, which can be used to parse the value of the key using HTTP Structured
Fields (RFC 8941).
- Parentheses in key or path segments are interpreted as the result of the 
enclosed path, as if it were an independent invocation. For example, 
`GET /hashpath1/path2+key1=(/hashpath2/key2)` implies that `key1` in the 
message for the resolution of `path2` is the result of the invocation of 
`/hashpath2/key2`.

## Requests and replies

The module documentation of `hb_http_server` and `hb_http` gives these rules
in full.

- A message crosses HTTP in one of two forms. In the `httpsig@1.0` form, the
HTTP message is the message: its keys are the headers and the body, and its
commitments are in `signature-input` and `signature` (RFC 9421). In the form of
any other codec, the body holds the whole message, encoded by that codec, and
the `content-type` gives the codec. A media type of the type `application`
gives a device by its subtype:
```
application/x.ao.<name>.<major>.<minor>    <name>@<major>.<minor>
application/x.ao.<name>.<major>            <name>@<major>.0
application/x.ao.<name>                    <name>@1.0
application/<name>                         <name>@1.0
```
For example, `application/x.ao.ans104` and `application/ans104` give
`ans104@1.0`, `application/x.ao.arweave-block.2.9` gives `arweave-block@2.9`,
and `application/json` gives `json@1.0`. The subtype of the last form is the
name whole: `application/vnd.api+json` gives `vnd.api+json@1.0`. A device is a
codec if the node has it and it exports `to`, `from`, `serialize` and
`deserialize`, as `json@1.0`, `ans104@1.0`, `tx@1.0` and `flat@1.0` do. The
node calls only these functions of a codec.
- A request, or a reply that a client receives, with a `signature-input`
header is decoded with `httpsig@1.0` (RFC 9421), whatever its `content-type`.
Otherwise, a `content-type` header that gives a codec means that the codec
decodes the body alone. Any other `content-type`, or none, means that
`httpsig@1.0` decodes the headers and the body. Nothing else selects the codec:
a `content-type` in the query of a request does not. The node replies with a
415 to a request whose `content-type` gives a device that is not a codec. A
client decodes a reply in a codec only if a range of its request's `accept`,
or of the `accept` in the query of its path, gives the same codec: a JSON
document that a server sends unasked is read by `httpsig@1.0`, and its body
keeps its bytes.
- If a request has commitments, the node keeps only its committed keys. Every
commitment must verify, or the node replies with a 400. The commitments of an
unsigned request are removed.
- The node sets `method` and `host` from the HTTP request. It adds `path`,
`accept` and `accept-bundle` to a request that does not have them.
- A device that authorizes a request by its signer must only read the keys that
the signer committed, after verifying them.
- The HTTP status of a reply is the result's `status`, if that is an integer
from 100 to 599. Otherwise, it is the code of the result's Erlang status. The
node does not add a `status` key to the reply.
- The codec of a reply is given by the request's `accept` (RFC 9110): its media
ranges in order of preference, by q-value and then by their order, of which the
first that the node can serve gives the codec. A range of the result's own
`content-type` gives `httpsig@1.0`, which sends the result's body unchanged. A
range that gives a codec gives that codec. Any other range, such as `*/*` or
`text/html`, is passed over. If no range can be served, or there is no
`accept`, the reply is in `httpsig@1.0`. For example:
```
accept                              computed result    stored JSON document
application/json, text/plain, */*   json@1.0           the document
application/x.ao.ans104             ans104@1.0         ans104@1.0
text/html, */*;q=0.8                httpsig@1.0        the document
```
- A reply in `httpsig@1.0` sends the result's own `content-type` as its
`content-type`, so a stored page or image is served as itself. A `content-type`
of the result of the form `application/x.ao.<name>` is sent in a body part
instead.
A result without commitments is sent with its unsigned commitment over its own
keys. A client keeps only the keys of an `httpsig@1.0` reply that at least one
of its commitments covers.
- A reply in any other codec is the result, encoded by the codec, with the type
and subtype of the range as its `content-type`: `accept: application/json` gives
`content-type: application/json`. `ans104@1.0` and `tx@1.0` carry their own
signature. `json@1.0` and `flat@1.0` carry every commitment in the message's
`commitments` key.
- A node with `force-signed` set signs the `hashpath` of a computed result,
committing only that key. A node without `force-signed` signs it in the same
way for a request whose `accept-signature` (RFC 9421) asks for it: a member
with the inner list `("hashpath")` and the URI of the node's committer as its
`keyid`. The field is a preference: the node ignores any other member, and a
reply without a `hashpath`, such as a stored message, is sent as it is without
the field.
- If the reply has no `status` key, the client adds the HTTP status as
`status`.
- HyperBEAM's client, `hb_http`, sends a request in the codec of its caller's
`http-codec` option, or in `httpsig@1.0` without one. A key of the message does
not change the codec.

## Example workflows

Get the current routes:
```
curl http://host:port/Routes
```

Set a route for a specific path:
```
curl -X POST http://host:port/Routes -H "Priority: 10" \
    -H "Node: http://host:port" -H "Template: /Some/New/Path"
```

Schedule a message on a process:
```
curl -X POST http://host:port/ProcID/Schedule -H "Action: Transfer" \
    -H "Recipient: Address" -H "Quantity: 1337"
```

Run a WASM executable:
```
curl -X POST http://host:port/Init/Compute -H "Device: WASM-64/1.0" \
    -H "2.WASM-Function: fac" -H "2.WASM-Params: [10]" -d @test/test-64.wasm
```

...returning response with a `Hashpath` header. To execute further messages
on the result, use the `Hashpath` as the base hashpath for the next message:
```
curl http://host:port/Hashpath/Compute/Results -H "WASM-Function: fac" \
    -H "WASM-Params: [11]" -d @test/test-64.wasm
```

Assign a message from one process's outbox to the schedule of another:
```
curl -X POST http://host:port/ProcID2/Schedule+Message= \
    (/ProcID1/Compute+Slot=1/Results/Outbox/1)
```

This request is equivalent to:
```
curl http://host:port/Hashpath/Compute/Results?WASM-Function=fac&WASM-Params=[11]
```

Load the WASM computation from an existing hashpath:
```
curl -X POST http://host:port/Init/Compute \
    -H "1.WASM-Image|Resolve: /OldHashpath/Snapshot/WASM" \
    -H "2.WASM-Function: fac" -H "2.WASM-Params: [10]"
```

To fork an existing process to use a new device, we can use the `~` key operator:
```
curl -X POST http://host:port/Schedule~Process/2.0/(/ProcID/Now)+Method=POST
```

To run multiple computations in the same request, with separate headers for
each, we should be able to use `Header#N` modifiers:
```
curl -X POST http://host:port/Init/Compute/Compute -H "Device: WASM-64/1.0" \
    -H "2.WASM-Function: fac" -H "2.WASM-Params: [10]" \
    -H "3.WASM-Function: fac" -H "3.WASM-Params: [11]" \
    -d @test/test-64.wasm
```

This request is equivalent to:
```
curl http://host:port/Init+Device=WASM-64/1.0+Image=ID/Compute+ \
    WASM-Function=fac+WASM-Params=[10]/Compute+WASM-Function=fac&WASM-Params=[11]
```

Gather the node's known commitments (including its own) on a process output:
```
curl http://host:port/ProcID/Compute+Slot=1/Results/Commitments
```

Dry-run a message on top of an AO process state:
```
curl -X POST http://host:port/ProcID/Compute \
    -H "Action: Balance" -H "Recipient: Address"
```
