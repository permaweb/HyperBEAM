# `tunnel@1.0` — reverse HTTP tunnel to a node behind a one-way network

- **Device name:** `tunnel@1.0`
- **Depends-on:** `message@1.0` (commitments, `verify`), `httpsig@1.0` (the
  commitment device of every control message that this device signs),
  `meta@1.0` (`is-operator`), `metering@1.0` (`consume`), and the node's HTTP
  request lifecycle (the `on/request` hook, request resolution and the reply
  status rules of the HTTP server).
- **Status:** Draft

## 1. Overview

`tunnel@1.0` serves an HTTP node that cannot accept connections (the
**origin**) through a node that can (the **broker**). The origin opens
outbound long-poll requests to the broker (**registrations**). When a caller
sends the broker a request for the origin, the broker returns that request as
the reply to one registration. The origin resolves it through its own HTTP
request lifecycle, as if the caller had sent it directly, and posts the reply
back to the broker in a signed **envelope**. The broker then replies to the
caller with the origin's reply.

A caller asks the broker for an origin in one of three ways:

1. **Host:** the request's `host` is `<label>.<tunnel-host>`, where `<label>`
   is the base32 form of the origin's address.
2. **Explicit target:** the request carries `tunnel-target`, as a key or as a
   query parameter.
3. **Call:** the caller posts to `/~tunnel@1.0/call` an envelope with the
   origin's address as `tunnel-target` and the application request, unchanged
   and possibly signed, as `request`.

Each request is dispatched **at most once**. A timeout means that the origin
may have executed the request; the tunnel never retries application work.

## 2. Concepts & terminology

- **Address:** a 32-byte wallet address, given as its 43-character base64url
  form. A value that does not decode to 32 bytes is not an address.
- **Host label:** the 52-character, lowercase, unpadded RFC 4648 base32 form of
  the 32 bytes of an address.
- **Broker scope:** each HTTP node has its own broker state, keyed by the
  node's `http-server` option, or by the address of its wallet when it has no
  `http-server`. Two listeners in one runtime never share registrations,
  requests or workers.
- **Control message:** a message that this device reads controls from:
  a registration, an envelope, a work message or the controls of `connect`.
- **Signed controls:** the controls that one commitment of a message covers.
  To read signed controls with a set of required keys, the device loads the
  message (the HTTP request as it was received, when the HTTP server gives it
  as the private `http-request`), checks that all of its commitments, and those
  of the messages that it contains, verify, and selects the first commitment
  that has a `committer` whose address is an ID and whose committed keys
  include every required key. The signed controls are the keys that this
  commitment covers, and the **signer** is its committer's address. Keys that
  the commitment does not cover are never read. If no commitment qualifies, or
  the message does not verify, the result is `403` (§8).
- **Work message:** the reply that the broker gives to a registration when it
  dispatches a request to it: `tunnel-request-id`, `tunnel-target`, `request`
  and `status: 200`, signed by the broker.
- **Request ID (`tunnel-request-id`):** the base64url form of 32 random bytes
  that the broker assigns to each request that it queues.
- **Presence:** the broker considers an address present while it has a
  registration for the address whose caller is alive, while it has a dispatched
  request for the address, or for 1000 ms after a registration for the address
  expires or a response for the address is delivered. The grace period lets
  requests arrive while a worker registers again.
- **Pool:** the workers that an origin runs for one peer. A pool is identified
  by the pair of the peer URL and the origin's address.

## 3. Device interface

- **Dispatch shape:** explicit keys. The device exports exactly `register`,
  `response`, `call`, `request`, `status` and `connect`. It has no default
  handler; any other key resolves under `message@1.0`.
- **HTTP forms:** callers reach the keys as `POST /~tunnel@1.0/register`,
  `POST /~tunnel@1.0/response`, `POST /~tunnel@1.0/call`,
  `GET /~tunnel@1.0/status` and `POST /~tunnel@1.0/connect`. `request` is an
  `on/request` hook:

  ```erlang
  #{ <<"device">> => <<"tunnel@1.0">>, <<"path">> => <<"request">> }
  ```

- **Node options:**

  | Option | Default | Use |
  | --- | --- | --- |
  | `tunnel-host` | none | The only parent domain of tunnel hosts. |
  | `tunnel-register-timeout` | `45000` | Wait of a registration, in ms. |
  | `tunnel-response-timeout` | `300000` | Wait of a caller, in ms. |
  | `tunnel-retry-time` | `1000` | Pause of a worker after a failure, in ms. |
  | `tunnel-max-workers` | `32` | Largest `workers` of one `connect`. |
  | `tunnel-max-client-workers` | `32` | Workers of the node, retiring included. |
  | `tunnel-client-retire-timeout` | `300000` | Deadline of a retiring worker. |

- **Timeouts:** a timeout is a number of milliseconds that is not negative, or
  `infinity`. An integer given as a binary is read as that integer.

## 4. Resolved keys (normative)

### `register` — wait for a request for the signer's address

- **Reads:** signed controls with the required key `address`; the optional
  `tunnel-timeout` of those controls; the node option
  `tunnel-register-timeout`.
- **Behaviour:**
  1. Read the signed controls. `address` MUST equal the signer, or the result
     is `403`.
  2. The wait is the controls' `tunnel-timeout`, else the node option. An
     invalid wait is `400` (`Invalid tunnel registration timeout.`).
  3. Queue the registration behind the other registrations for the address,
     and dispatch to it, in order, any request that is queued for the address
     (§6).
  4. When a request is dispatched to the registration, reply with the work
     message, signed by the broker (§5). When the wait ends first, reply
     `{ok, #{ <<"status">> => 204 }}`.
  5. Charge the seconds that the registration waited to `metering@1.0` as the
     resource `tunnel-seconds`. Outside a metering session this does nothing.
- **Side effects:** the registration takes part in dispatch until it ends.
  A registration whose wait expires marks the address present for 1000 ms.

### `response` — deliver an origin's reply

- **Reads:** signed controls with the required keys `tunnel-request-id`,
  `status` and `response`.
- **Behaviour:**
  1. Read the signed controls.
  2. If no dispatched request has the ID (it was never dispatched, it was
     answered, or its deadline passed), the result is `404`
     (`Unknown tunnel request.`).
  3. If the request was dispatched to an address other than the signer, the
     result is `403` (`Tunnel response signer does not match its address.`)
     and the request stays dispatched.
  4. Otherwise the request is answered. If `response` has no `status` key, it
     is given the envelope's `status`. The caller of the request receives the
     response, and the address is marked present for 1000 ms. If the caller
     has ended, the response is discarded and the result is `404`.
- **Returns:** `{ok, #{ <<"status">> => 202, <<"tunnel-request-id">> => ID }}`.

### `call` — send an envelope's request to an origin

- **Reads:** `tunnel-target` and `request`, each from the request and then the
  base; the wait from `tunnel-timeout` of the request, then of the base, then of
  the query of the request's `path`, then the node option
  `tunnel-response-timeout`.
- **Behaviour:**
  1. `tunnel-target` MUST be an address (`400`, `Invalid tunnel address.`).
  2. `request` MUST be present (`400`, `A tunnel request is required.`).
  3. Load `request` with its links and check that its commitments, and those
     of the messages that it contains, verify. It is carried unchanged.
  4. Bound the wait by the node option `tunnel-response-timeout`: a request's
     `tunnel-timeout` of `infinity`, or a wait past the limit, becomes the
     limit, so a request cannot occupy a worker past the node's own limit.
  5. Queue the request for the address, whether or not the address is present,
     and wait for its response or the end of the wait (§6).
- **Returns:** `{ok, Response}` with the origin's reply, or an error of §8.

### `request` — route a request that the node receives

- **Reads:** the hook message's `request` (the request as the HTTP server
  parsed it), the base's `host`, and the node options `tunnel-host` and
  `tunnel-response-timeout`.
- **Behaviour:**
  1. If the path of the request, up to its first `?`, has exactly the parts
     `~tunnel@1.0` and `call`, return `{ok, Hook}`: the call is resolved as a
     call.
  2. **Host routing:** take the request's `host`, without a port, in lowercase
     and without a trailing `.`. Split it at its first `.` into a label and a
     parent. The request is for the address that the label decodes to if the
     label has 52 characters, it decodes to 32 bytes, and either there is no
     tunnel host (the base's `host`, else the node option `tunnel-host`), or
     the parent equals the tunnel host. A host that has no parent is accepted
     only when there is no tunnel host.
  3. **Explicit routing:** otherwise, if the request has `tunnel-target` (as a
     key of the request, else of the base, else as a parameter of the query of
     its path), the request is for that address. A value that is not an address
     ends the request with `400` (`Invalid tunnel address.`).
  4. A request for no address, and a host-routed request for an address that is
     not present, return `{ok, Hook}` unchanged: the next handler serves it.
  5. Prepare the forwarded request: load the request with its links, check that
     its commitments verify, remove the keys `tunnel-target` and
     `tunnel-timeout`, and, for host routing, `host` unless a commitment of
     the request covers it, and remove the query parameters `tunnel-target`
     and `tunnel-timeout` from its `path`. If the result does not verify, the
     request ends with `400`
     (`Tunnel routing must preserve the request commitments.`). The first
     label of a routing `host` is the base32 form of the origin's address,
     which an origin with `name@1.0` would read as a name.
  6. The wait is `tunnel-timeout` of the request, then of the base, then of the
     query, then the node option `tunnel-response-timeout`, bounded by that
     node option: a request's `infinity`, or a wait past the limit, becomes the
     limit, so a request cannot occupy a worker past the node's own limit.
  7. Queue the request and wait for its response (§6).
  8. Return `{error, Response}` with the origin's reply, or with the error of
     §8. The request ends: the HTTP server replies with this message, and no
     later `on/request` handler runs.
- **Reply status:** the HTTP status of the reply is the response's `status`
  (§4 `response`, step 4).

### `status` — report this node's broker

- **Returns:** `{ok, Status}` where `Status` has:

  | Key | Value |
  | --- | --- |
  | `registered` | Waiting registrations per address. |
  | `pending` | Queued, undispatched requests per address. |
  | `open-registrations` | Waiting registrations. |
  | `open-requests` | Queued and dispatched requests. |
  | `dispatched-requests` | Dispatched requests without a response. |
  | `open-client-pools` | Pools that this node runs. |
  | `open-client-workers` | Workers of those pools that are not retiring. |
  | `retiring-client-workers` | Workers that stop after their current work. |

### `connect` — start, resize or stop a pool

- **Reads:** the controls; the node options `tunnel-max-workers`,
  `tunnel-max-client-workers` and `tunnel-client-retire-timeout`.
- **Controls:** if the options of the caller contain the node's wallet (as the
  node message of an `on/start` hook does), the controls are the base merged
  with the request, and need no signature. Otherwise (for example, an HTTP
  request, whose options never contain the node's wallet, or code that a
  request runs) the controls are the signed controls with the required key
  `peer`, and their signer MUST be an operator of the node according to
  `meta@1.0`'s `is-operator`; else the result is `403`.
- **Behaviour:**
  1. `peer`, the URL of the broker, MUST be a binary (`400`,
     `A tunnel peer is required.`).
  2. `workers` (default `1`) MUST be an integer from `0` to
     `tunnel-max-workers`, or any integer from `0` when that is `infinity`
     (`400`, `Invalid tunnel worker count.`).
  3. `address`, if given, MUST be the node's address (`400`,
     `The tunnel address must be this node's wallet.`).
  4. Resize the pool for the peer and the node's address to `workers`:
     - Start one worker for each missing worker. All workers of the node,
       retiring workers included, MUST NOT exceed
       `tunnel-max-client-workers` (`429`,
       `Tunnel client worker limit exceeded.`).
     - Retire each excess worker: it stops after its current registration or
       request, and is stopped after `tunnel-client-retire-timeout` ms.
     - With `0` workers, the pool is removed; its workers retire.
     - A connect at the current size changes nothing.
- **Returns:** `{ok, #{ <<"status">> => 202, <<"body">> =>
  <<"Tunnel client pool resized.">>, <<"workers">> => Workers,
  <<"address">> => Address, <<"peer">> => Peer }}`.
- **Workers:** each worker repeats the following until it is retired:
  1. Read the node's current options.
  2. Post a registration to `<peer>/~tunnel@1.0/register`: the message
     `codec-device: httpsig@1.0`, `address` (the node's address) and
     `tunnel-timeout` (the node option `tunnel-register-timeout`), signed by
     the node (§5). The client's send and receive timeouts are the wait plus
     5000 ms.
  3. On `204`, continue. On a work message, read its signed controls with the
     required keys `tunnel-request-id`, `tunnel-target` and `request`.
     `tunnel-target` MUST be the node's address.
  4. Execute `request` (§4.1) and post the envelope `codec-device:
     httpsig@1.0`, `tunnel-request-id`, `status` and `response`, signed by the
     node, to `<peer>/~tunnel@1.0/response`. A post that gets a 5xx reply, or
     whose connection fails, is made again, up to 3 times, after
     `tunnel-retry-time` ms, then twice and three times that. A post that gets
     any other reply is not made again: a 404 after a failed post means that
     the broker delivered the response.
  5. After any failure, wait `tunnel-retry-time` ms and continue.

  A worker that ends for any other reason than retirement is replaced after
  1000 ms. The request that it was executing is not dispatched again.

### 4.1 Execution at the origin

The origin resolves a carried request as its HTTP server resolves a request
that it receives:

1. A request given as a sequence of messages is converted to its singleton
   form.
2. If the node option `force-signed-requests` is set and the request has no
   signers, the result is `400` (`A signed application request is required.`).
3. The request is resolved with the HTTP server's request resolution for the
   path up to its first `?`: the node's `on/request` hooks, its
   initialization rule, the options of HTTP requests and its `on/response`
   hooks.
4. The reply is the message that the HTTP server would reply with: a list is
   the `body` of a message whose `ao-result` is `body`, any other result is
   forced into a message, and the message is loaded with its links (a caller
   cannot reach the origin to load them) and its private keys are removed.
5. The reply status is the HTTP status of that message: its `status` key if
   that is an HTTP status, otherwise the status of the result.
6. If resolution raises an exception, the reply is
   `#{ <<"body">> => <<"Tunnel execution failed.">> }` with the status that
   the HTTP server gives the exception (`400` for an invalid request or
   commitments, `404` for a message that cannot be found, `500` otherwise).

## 5. Data formats & encodings

- All keys are binary, lowercase and hyphenated.
- Addresses and request IDs are base64url; host labels are base32 as in §2.
  Nothing is hex.
- Control messages that this device signs (registrations, work messages and
  envelopes) are committed with `httpsig@1.0` over all of their keys, after
  their links are loaded and their private keys removed, as one bundle, so
  that the messages that they contain stay inside them.
- Application messages (`request`, `response`) are carried as they are, with
  all of their commitments.
- The work message is `tunnel-request-id`, `tunnel-target`, `request` and
  `status: 200`.

## 6. Ordering, freshness & caching

- Registrations and requests for an address queue in arrival order. Each
  request is dispatched to the oldest registration whose caller is alive, at
  most once. A registration or request whose caller has ended is discarded.
- A queued request waits until its wait ends (`504`), or, for a host-routed
  request, until the address is no longer present (`404`). An explicit or
  `call` request waits even while its address is not present.
- A dispatched request waits for its response until its wait ends, even if its
  caller ends. A dispatched request whose wait is `infinity` ends when its
  caller ends.
- After a request's wait ends, a response for it is unknown (`404`).
- No result is cached by this device.

## 7. Security & authority

- A registration is accepted only with the signature of the address that it
  registers.
- A response is accepted only with the signature of the address to which its
  request was dispatched, over its ID, its status and the response.
- A pool is changed only by the node itself, or with the signature of an
  operator of the node.
- Keys that a control's commitment does not cover are never read, so unsigned
  additions cannot change a signed control.
- The broker removes routing controls from a request only when its commitments
  still verify. The HTTP server keeps only the committed keys of a signed
  request, so a signed request is routed by its host or sent with `call`.
- Neither node signs an application message that it carries: a reply is the
  origin's reply with its own commitments. The envelope's signature is checked
  by the broker and is not given to the caller.
- A worker accepts work from the peer that it connected to, without checking
  the signer of the work message. Use HTTPS to authenticate the peer; TLS ends
  at the broker.
- Delivery is at most once. A `504` does not mean that the request was not
  executed.

## 8. Errors

| Condition | Status | `body` |
| --- | --- | --- |
| No qualifying signature, or a wrong signer | 403 | `Tunnel control signature required.` |
| A response from another address | 403 | `Tunnel response signer does not match its address.` |
| A response for an unknown request | 404 | `Unknown tunnel request.` |
| An invalid target | 400 | `Invalid tunnel address.` |
| A call without `request` | 400 | `A tunnel request is required.` |
| An invalid registration wait | 400 | `Invalid tunnel registration timeout.` |
| An invalid request wait | 400 | `Invalid tunnel request controls.` |
| Routing that breaks commitments | 400 | `Tunnel routing must preserve the request commitments.` |
| A `connect` without `peer` | 400 | `A tunnel peer is required.` |
| An invalid `workers` | 400 | `Invalid tunnel worker count.` |
| Another node's `address` | 400 | `The tunnel address must be this node's wallet.` |
| Invalid pool options | 400 | `Invalid tunnel client controls.` |
| Too many workers | 429 | `Tunnel client worker limit exceeded.` |
| A caller's wait ends | 504 | `Timed out waiting for tunneled response.` |
| A host-routed address is not present | 404 | `Requested tunnel proxy is unavailable.` |
| The broker stops | 503 | `Tunnel broker is stopping.` |

All errors are returned as `{error, Message}`, where `Message` has `status`
and `body`. A `504` or `503` also has `reason` (`tunnel-timeout` or
`tunnel-unavailable`), `tunnel-target` and `tunnel-request-id`. A `404` for an
address that is not present also has `reason` (`tunnel-unavailable`) and
`tunnel-target`, and a `404` for an unknown request has `tunnel-request-id`.

## 9. Composition

- **Broker:** add the `request` hook to the node's `on/request` handlers
  before the handlers that serve the same hosts (such as name resolution). A
  request that the tunnel does not route reaches the next handler unchanged.
- **Origin:** add `connect` to the node's `on/start` handlers:

  ```erlang
  #{
      <<"device">> => <<"tunnel@1.0">>,
      <<"path">> => <<"connect">>,
      <<"peer">> => <<"https://tunnel.example">>,
      <<"workers">> => 4
  }
  ```

  The origin needs no inbound port.
- **Metering:** a node whose requests `metering@1.0` meters can charge the
  `tunnel-seconds` of registrations.

## 10. Conformance (normative checklist)

1. The device exports exactly `register`, `response`, `call`, `request`,
   `status` and `connect`.
2. Signed controls are read only from one commitment that covers all required
   keys; uncovered keys are ignored; a message that does not verify is `403`.
3. `register` requires `address` to be its signer, waits for at most its wait,
   and returns the signed work message or `204`.
4. Each request is dispatched at most once, to the oldest live registration
   for its address, in arrival order.
5. `response` accepts only the dispatched address's signature over
   `tunnel-request-id`, `status` and `response`; unknown IDs are `404`; a
   response without `status` takes the envelope's `status`.
6. The `request` hook routes by host before `tunnel-target`, passes requests
   that it does not route and host-routed requests for absent addresses
   unchanged, removes routing controls only while commitments verify, and ends
   a routed request with `{error, Response}`.
7. A tunneled request has the reply of the same request sent directly to the
   origin: the same HTTP status, message and commitments, except as the open
   questions below give.
8. `connect` without the node's wallet requires an operator's signature;
   `workers` from `0` to `tunnel-max-workers`; a resize starts or retires only
   the difference; `0` removes the pool.
9. Broker state is separate for each HTTP node.

## 11. Out of scope

- DNS and TLS certificates for tunnel hosts.
- The HTTP codecs and content negotiation, which the HTTP server applies to
  each hop.
- The internal representation of the broker's state.

## Open questions

- **Status keys that are not HTTP statuses.** A broker ends a routed request
  from its `on/request` hook, and a hook can end a request only with an error.
  The HTTP server replies to an error with the message's `status` if that is an
  HTTP status, and `400` otherwise. A reply whose `status` key is not an HTTP
  status (`status: pending`) is therefore `400` through a tunnel, and its
  direct status otherwise.
- **Signatures on hashpaths.** An origin does not sign the hashpath of a
  computed reply (`force-signed`, or `accept-signature`) through a tunnel.
- **Exceptions.** A request whose resolution raises an exception at the origin
  has the body `Tunnel execution failed.`, rather than the error message that
  the origin's HTTP server gives it.
- **Host routing removes `host`.** An origin receives a host-routed request
  without its `host`, unless the request signed it, so a reply that depends on
  the request's `host` differs from a direct reply.
- **No execution deadline for other blocking.** A tunneled request's wait is
  bounded by the node's `tunnel-response-timeout` (§4), so a nested tunnel call
  to an absent target cannot occupy a worker past the node's limit. An
  execution that blocks for another reason has no deadline, and occupies its
  worker until the worker is retired or stopped.
- **Registrations can be posted again.** A signed registration is not bound
  to a broker or a time, so a copy of one can be posted again, to its broker
  or another, and receive a request for its address. Only the origin can
  answer that request.
- **One commitment per control.** When several commitments of a control
  message qualify, the device uses the first in the order of the commitments
  map, and does not try the others.
