# `meta@1.0` — the HyperBEAM meta device

- **Device name:** `meta@1.0`
- **Depends-on:** `message@1.0` (the base device against which results are
  forced into messages, commitments are read/written, and signers are
  determined). The `message@1.0` spec is provided to reimplementers. The node's
  request handler (§3), not this device, invokes two **request hooks**,
  `request` and `response`; the hook mechanism is described behaviourally in
  §2 and §5 (no hook *device* spec is required to implement `meta@1.0`).
- **Status:** Draft

## 1. Overview

`meta@1.0` is **not** the node entry point. The node server hands each parsed
request to its request handler, `hb_http_server:resolve_request/3`, not to
this device, and the request handler runs the whole request lifecycle —
**request → preprocess → resolve → postprocess → reply**. A request reaches
`meta@1.0` by its path, as for any other device.

The device exposes a small set of **operator/introspection keys**: reading
and (admin-only) updating the node's own configuration message (`info`),
reporting the node's software version and build provenance (`build`), and
answering whether a request is authorised by the node operator
(`is-operator`).

A node MUST route inbound requests through the request handler. The
substrate's content-negotiation (request parsing and response encoding) sits
*outside* this device and is described here only where it affects the
device's contract (§6).

## 2. Concepts & terminology

- **Node message (`NodeMsg`):** the node's own configuration message — a map of
  options that parameterise the whole node (operator address, wallet, stores,
  routes, hooks, payment config, etc.). It is supplied to the device on every
  invocation as the options/configuration context, and is the message that
  `info` reads and (under admin authority, §7) updates. Its full key set is
  open-ended and out of scope; this spec pins only the keys `meta@1.0` itself
  reads or writes.
- **Operator:** the identity authorised to change the node message, unless an
  `admin` option gives another (§7). Resolved as: the node message's
  `operator` option if set; else the committer URI of the node's configured
  wallet if a wallet exists; else none, and the node is **`unclaimed`**. See
  §7 for the exact authority test.
- **Unclaimed node:** a node with no effective operator (no `operator` option
  and no wallet), or whose `operator` is `unclaimed`. On an unclaimed node,
  the operator-authority test passes for **no** caller (§7).
- **Request singleton:** the parsed, normalised form of one inbound request —
  one message, which `hb_singleton:from/2` splits into a **sequence (list) of
  messages** to be resolved in order by the substrate's resolver (the same
  "resolve-many" form `apply@1.0`/the substrate consume). The exact parse from
  wire form to the singleton is the substrate's job; the request handler (§3)
  receives the singleton and the request target path.
- **Hook:** a configurable extension point the node message names under its `on`
  configuration. The request handler (§3) fires two hooks by name — `request`
  (before resolution) and `response` (after resolution). A hook is invoked
  with a request message and returns `{ok, Result}` to continue, or an error
  to halt.
  See §5 for the exact hook request/response shape and halt semantics.
- **Initialised / permanent:** node-message lifecycle states governing whether
  requests may run and whether the node message may still change. See §3 and §7.
- **Internal representation** of any message here (how maps are stored) is out
  of scope; the contract is defined over logical key/value content, resolution
  results, and externally observable HTTP-level behaviour.

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device exports exactly three
  resolvable keys — `info`, `build`, `is-operator` — and answers no others as a
  normal device. Any other key addressed *to* `meta@1.0` as a device is not
  handled by this device's own logic. (Implementation note: because the device
  exposes an explicit export set, the message-manipulation/inspection keys
  (`keys`, `set`, …) are not relevant here as they are on default-handler
  devices; they are simply not exported.)
- **Request-handling entry point (not part of this device):** the node
  server's **request handler**, `hb_http_server:resolve_request/3`, is the
  function the node invokes for every inbound request. It takes the request
  target path, the request singleton and the **node message**, and returns
  `{Request, {ErlStatus, Result}}` where `Result` is the message to encode
  back to the caller for `Request`. This entry point is described
  normatively in §4 as the *lifecycle*; it is never reached by ordinary path
  resolution.
- **Message shapes:**
  - The **request handler** consumes the request target path and the parsed
    request singleton, which it splits into a list of messages. No specific
    keys are required of the caller beyond what the addressed downstream
    device requires; the path selects the downstream device and key.
  - `info` reads the request's **`method`** (`GET`-like vs `POST`) to decide
    read vs update, and on update reads the keys of the request that the
    admin signed (§7) as the set of node-message keys to merge.
  - `is-operator` reads the request's optional **`body`** (the message whose
    signers are tested), falling back to the request itself.
  - `build` reads nothing from the request.

## 4. Request lifecycle (normative)

The request handler MUST implement the following pipeline. Inputs: the request
target path `Path`, the request singleton `Req` and the node message
`NodeMsg`.

1. **Parse.** Convert `Req` into `Msgs` (an ordered list of messages) using
   the substrate's request-parsing (`hb_singleton:from/2`). (The substrate
   selects the response codec from the request's `accept` when it encodes
   the reply; see §6.)

2. **Initialisation gate.** Read the node message's `initialized` option
   (default: `true`, from the default node message). This gate runs before
   step 1.
   - If `initialized` is `false`, the handler MUST refuse to run general
     computation. The single permitted request is one whose `Path` is
     exactly `/~meta@1.0/info`, with any method; it runs the whole lifecycle
     below, hooks included. Otherwise return error message
     **`Node must be initialized before use.`** (with an error status, §8).
     A node whose `initialized` is any other value (including the string
     `permanent`) skips this gate and proceeds.

3. **Pre-process (`request` hook).** Invoke the `request` hook with the hook
   request shape of §5, carrying the request singleton as `request` and the
   parsed sequence as the hook **body**. The hook's outcome decides what
   happens next:
   - **Halt:** if the hook returns an **error**, the lifecycle stops
     immediately; that error (wrapped with a status, §8) is the response. The
     resolver is NOT run and the `response` hook is NOT run.
   - **Empty redirect:** if the hook returns `{ok, []}` (an empty body /
     sequence), the handler MUST return a redirect message: status **307**,
     `body` = `Redirecting to default request.`, and `location` = the node
     message's `default-request` option, defaulting to
     **`/~hyperbuddy@1.0/index`**. (This is the node's "default index" / landing
     behaviour.)
   - **Continue (possibly rewritten):** otherwise the hook returns
     `{ok, NewBody}`; `NewBody` (a message sequence) **replaces** the request to
     be resolved. A hook MAY return the body unchanged (pass-through) or a
     modified sequence (e.g. add/replace keys on the messages); the modified
     sequence is what gets resolved. This is how a pre-processor rewrites a
     request.

4. **Resolve.** Resolve the (post-pre-process) message sequence with the
   substrate's resolver, in **message-forcing** mode (the result MUST be coerced
   to a message — see `message@1.0`). The resolution runs against the
   then-current node options (a pre-processor that changed node configuration is
   visible to the resolution), without the node's key material (`priv-wallet`,
   `identities`) and with `http-extra-opts` merged over them. The result keeps
   the resolver's Erlang status; §8 gives its HTTP status.

5. **Post-process (`response` hook).** Invoke the `response` hook with the hook
   request shape of §5, carrying the `request` that the `request` hook returned
   (default: the request singleton) as `request` and the **resolution result**
   as the hook **body**. As with the pre-processor:
   - an **error** return halts and becomes the response (wrapped with a status);
   - an `{ok, NewBody}` return makes `NewBody` the result to return — the
     post-processor MAY pass the result through unchanged or rewrite/replace it.

6. **Optional signing.** When it encodes the reply, the substrate signs only the
   result's `hashpath` with the node's key, in `httpsig@1.0`, if the node
   message's `force-signed` option is `true` or the request's
   `accept-signature` asks for it, and the message that is sent is the result
   of its hashpath and has no `hashpath` key of its own. A result that already
   carries a commitment gets this commitment as well. A reply in `httpsig@1.0`
   of a result with no commitments also carries its unsigned commitment.

7. **Return.** Return the `request` that the `request` hook returned (default:
   the request singleton) with `{ErlStatus, Result}`. No `status` key is added
   to `Result`; the HTTP status of the reply is that of §8. The substrate then
   encodes `Result` back to the caller per the codec of the request's
   `accept` (§6).

A node MUST NOT skip the hooks: if no hook is configured for a name, the hook is
a no-op that passes its body through unchanged (continue). A node MUST NOT run
the resolver if the `request` hook halted, and MUST NOT run the `response` hook
if the `request` hook halted or gave an empty body (step 3), or the resolver
raised an exception; it runs the `response` hook on a result of any status.

## 5. Hooks: request shape & halt semantics (normative)

Both hooks are invoked with a **hook request message** of exactly this shape:

```
request => <the request singleton; for the response hook, the request that
            the request hook returned (default: the request singleton)>
body    => <the pre-process input OR the post-process input>
```

- For the **`request`** hook, `body` is the parsed message **sequence** to be
  resolved.
- For the **`response`** hook, `body` is the **resolution result** message.

A hook implementation returns one of:
- `{ok, #{ body := NewBody, … }}` — **continue**; `NewBody` becomes the new
  body (the sequence to resolve, or the result to return). The node takes the
  hook's `body` and, from the `request` hook, its `request` (default: the
  request singleton): the `response` hook gets that `request`, and the reply
  is encoded for it. Other keys of the hook's return are ignored.
- `{error, _}` — **halt**; the error becomes the response (status-wrapped, §8).
- `{failure, _}` — **halt**; the failure becomes the response (`failure` →
  500, §8).
- Any other return — treated as an **error** (halt).

The hook name (`request` / `response`) and the node's hook configuration binding
are part of the node message (`on` configuration); the request handler (§3)
only fires the hook by name and interprets the result per the rules above. The
two hook invocations re-read the (possibly updated) node options around the
resolve step: a `request` hook that mutates node configuration is reflected in
the options used for resolution and for the `response` hook.

## 6. Content negotiation & node-server context

Content negotiation is performed by the substrate **around** this device, not by
the device itself, but it forms part of the device's operating contract:

1. The inbound wire request is parsed into the request singleton before the
   handler runs (§4 step 1).
2. The substrate selects a **response codec** from the request's `accept`
   (content-negotiation) information when it encodes the reply
   (`hb_http_server:accept_to_codec/3`); it does not place the codec in the
   node message. The final response encoding uses this codec; the signature
   of §4 step 6 is in `httpsig@1.0`. At most 100 media ranges are
   considered, in preference order; later ranges are not served.
3. The allowed request methods at the node boundary are `GET`, `POST`, `PUT`,
   `DELETE`, `OPTIONS`, `PATCH`; CORS-preflight (`OPTIONS`) is handled by the
   substrate and never reaches this device. This device distinguishes only
   `POST` (update) from non-`POST` (read) for the `info` key (§`info`).

A node MUST take the reply codec from the request's `accept`; it MUST NOT
hard-code a single response codec.

## 7. Operator authority (normative)

Two related notions appear; pin both precisely.

### Resolving the operator
The effective operator is computed from the node message as:
1. If the `operator` option is set, use it.
2. Else, if the node has a configured wallet, use the URI of its committer
   (`hb_opts:committer/1`).
3. Else, the node has no operator: it is **`unclaimed`**.

An operator, which `operator` may give as an address or in any form that
`hb_committer:uri/2` reads, is compared as that URI. A node message `operator`
value of `unclaimed` (atom or binary) matches no signer: the node is
unclaimed.

### The authority test (used to gate node-message updates)
A request is **authorised as operator** iff the effective operator's URI is a
member of the request's **committers** (i.e. the request is signed by the
operator). On an **unclaimed** node no caller passes.

This is failure-closed: a request that is unsigned, or signed only by
non-operators, is **not** authorised.

### Permanence
The node message MAY be sealed by setting `initialized` to the string
**`permanent`**. While permanent:
- the node message MUST NOT be changed by any caller (even the operator);
- an update attempt MUST fail with the error message
  `The node message of this machine is already permanent. It cannot be changed.`
  (§8), and a permanence check inside the adopt path independently rejects with
  `Node message is already permanent.`

### `is-operator` (exported key) vs. the internal admin gate
The **exported** `is-operator` key (§`is-operator`) answers the authority test
above for an arbitrary request and returns a boolean. The **update path**
(`POST info`) applies its admin gate before allowing a change:
`lib_meta:is_authorized(<<"node-message">>, admin, Request, NodeMsg)`. The
admin is the `admin` option, defaulting to the effective operator. The gate
passes iff the admin's commitments on the request verify and cover
`type: node-message`, and it returns only the keys that the admin signed,
which are the keys adopted. (A node MAY also define stricter identity notions
— e.g. "is the original initiator who first configured the node" — but the
only authority `meta@1.0` requires for node-message updates is the admin
gate.)

## 8. Resolved keys (normative)

### `info` — read or admin-update the node message
- **Reads:** the request `method`; on update, the request's commitments and the
  keys they cover (including `type`); the node message (`admin`, `operator`,
  `initialized`, wallet, `node-history`, and all configuration keys).
- **Behaviour:**
  - **Read (`method` ≠ `POST`):** Return the node message as a message, with:
    1. **Private keys removed.** Any key that is private (per `message@1.0`'s
       private-key definition: any key that begins with `priv`, in any case)
       MUST NOT appear in the result.
    2. **Unencodable values redacted.** Any value that is not encodable into a
       message (e.g. an opaque runtime tuple/handle) MUST be replaced by the
       string `Unencodable value.` Maps and lists are filtered recursively.
    3. **Dynamic keys added.** If the node has a wallet, add `address` = the
       wallet's address (human-readable form). If the node message carries an
       `identities` map, add to each identity an `address` derived from that
       identity's wallet. These dynamic keys are computed per-request and are not
       stored in the node message.
    The reply's HTTP status is 200 (§8 status rules below); no `status` key is
    added.
  - **Update (`method` = `POST`):** a rejection is returned as
    `{error, #{ body => <reason> }}` and a success as `{ok, ResultMessage}`;
    neither carries a `status` key, and the HTTP status is that of §8.
    1. If the node is **permanent** (§7), reject: return `{error, #{ body =>
       <<"The node message of this machine is already permanent. It cannot
       be changed.">> }}` (HTTP 400).
    2. Else apply the **admin gate** (§7) to the request. If it fails, return
       `{error, #{ body => not_authorized }}` (HTTP 403) and make **no**
       change.
    3. Else **adopt** the keys that the admin signed as a node-message update
       (see *Adopting a node message* below). On success return a message
       whose `body` is a human
       summary of the form `Node message updated. History: <N> updates.` and
       whose `history-length` is `N`, the new node-history length. On failure
       return `{error, #{ body => <reason> }}`.
- **Returns:** `{ok, Message}`, or `{error, #{ body => <reason> }}`; neither
  carries a `status` key.
- **Side effects (update only):** mutates the live node configuration and
  appends to `node-history` (see below). No cache/store writes of its own.

**Adopting a node message (the update mechanism).** Given an authorised,
non-permanent update request, the node message is updated by:
1. Re-checking permanence (reject with `Node message is already permanent.` if
   now permanent).
2. Taking the keys of the request that the admin signed, without their
   commitments.
3. **Shallow-merging** those keys over the current node message (request values
   overwrite existing options).
4. **Appending** the update (with private keys reset and any incoming
   `node-history` key stripped) to the node message's `node-history` list — so
   `node-history` is the ordered list of accepted configuration changes, oldest
   first. The first entry is the request that first configured the node.
5. **Preserving** the node's server-identity binding (the node MUST NOT let a
   caller overwrite the internal `http-server` reference via the merge).
A node MUST persist the merged options as the live configuration for subsequent
requests.

### `build` — node software version & provenance
- **Reads:** nothing from `Base`/`Req`; reads compile-time build constants.
- **Behaviour:** Return a fixed message identifying the node software and the
  source it was built from. The message MUST contain:
  - `node` = the node implementation name (the constant string `HyperBEAM`),
  - `version` = the node software version,
  - `source` = the full source commit hash the node was built from,
  - `source-short` = the abbreviated commit hash,
  - `build-time` = the build timestamp.
  The long and short commit hashes are reported as **separate** keys (the short
  hash's length is not fixed across build toolchains, so it is not derived from
  the long hash by truncation at a fixed width). Individual fields are
  addressable (e.g. resolving `build/version` yields just the version).
- **Returns:** `{ok, Message}`.
- **Side effects:** none.

### `is-operator` — is this request authorised as operator?
- **Reads:** the request's `body` (the message to test) if present, else the
  request itself; the node message's effective operator (§7).
- **Behaviour:** Apply the **operator authority test** of §7 to the selected
  message's committers and return the boolean outcome. On an **unclaimed** node
  the answer is `false` for any caller; on a claimed node it is `true` iff the
  operator's URI is among the committers.
- **Returns:** `{ok, Boolean}`.
- **Side effects:** none.

### Status wrapping (applies to every returned result)
The node adds no `status` key to a result; `hb_http:status_code/2` gives the
HTTP status of the reply in this order of precedence:
1. If the result message carries a `status` that is an integer from 100 to
   599, or the binary encoding of one, use it.
2. Else, for an `error` or `failure` whose `body` is an atom with a code of
   its own in the list below, use that code.
3. Else map the Erlang-style outcome class to a default HTTP code:
   - `ok` → **200**
   - `created` → **201**
   - `error` / `client_error` / `no_viable_responses` (including
     `{no_viable_responses, _}`) → **400**
   - `not_found` → **404**
   - `unauthorized` → **401**
   - `forbidden` / `not_authorized` → **403**
   - `failure` → **500**
   - `unavailable` → **503**
   - any other outcome → **200**.
When the result is not a map (a bare value/binary), it is sent as the
message `#{ ao-result => body, body => <value> }`.

## 9. Data formats & encodings

- Operators and committers are compared as the URIs that `hb_committer:uri/2`
  gives; addresses are **base64url** (human-readable) form, never hex.
- The response codec comes from the request's `accept` (§6) and is used for
  encoding; the node's signature (§4 step 6) is in `httpsig@1.0`.
- `node-history` is an **ordered list** of accepted update requests (oldest
  first); its length is reported by `history-length` on a successful update.
- `info` read output is a plain message: private keys removed, unencodable
  values replaced by the binary `Unencodable value.`, dynamic `address` keys
  added as described.
- All node-message keys in this spec are lowercase, hyphenated
  binaries on the wire (`operator`, `initialized`, `node-history`,
  `default-request`, `force-signed`, `http-server`, `identities`, `address`,
  `version`, `source`, `source-short`, `build-time`).

## 10. Ordering, freshness & caching

- The request handler is **stateful with respect to node configuration**: an
  accepted `POST info` changes the node message seen by *subsequent* requests.
  Within a single request, the resolve step and the `response` hook observe the
  node options **as they stand after** the `request` hook (so a pre-processor
  that updates configuration is visible downstream in that same request).
- `build` is deterministic and constant for a given binary.
- `is-operator` and `info` (read) are pure functions of the request and the
  current node message.
- This device performs no result caching of its own; whether resolution results
  are cached is governed by the substrate's freshness controls and node
  configuration, independent of this device.

## 11. Security & authority

- **Failure-closed updates.** Node-message updates require admin authority
  (§7). An unauthorised request MUST NOT change any configuration and MUST
  return an authorisation error. An **unclaimed** node with no `admin` option
  passes no caller.
- **Permanence is irreversible.** Once `initialized = permanent`, no update —
  including by the operator — may change the node message.
- **Privacy.** The `info` read MUST NOT leak private keys (wallets, secrets) or
  unencodable runtime handles. Private-key filtering follows `message@1.0`.
- **Signing replies.** `hb_http_server` sets `force-signed` to `true`
  on every node that it starts (`set_default_opts/1`). With `force-signed` set,
  or for a request whose `accept-signature` asks for it, the node signs only
  the result's `hashpath` (§4 step 6), also on an already-committed result.
- **Hooks are trusted node configuration.** The `request`/`response` hooks can
  inspect, rewrite, or halt any request/response; they are part of the operator's
  node configuration, not user input, and are the node's global
  interception/authorisation point.

## 12. Errors

All errors are returned with the HTTP status of §8. The conditions:

- `Node must be initialized before use.` — a request whose path is not
  `/~meta@1.0/info` reached an uninitialised node (status 400 via `error`).
- `The node message of this machine is already permanent. It cannot be changed.`
  — a `POST info` on a permanent node.
- `Node message is already permanent.` — the adopt path independently detected
  permanence.
- `not_authorized` (an atom `body`, HTTP 403) — a `POST info` whose request
  failed the admin gate (§7).
- Any error returned by the `request` or `response` **hook** — propagated as the
  response unchanged (status-wrapped). A hook returning `{failure, _}` halts
  as a `failure` (500, §8); any other non-`{ok,_}`, non-`{error,_}` value is
  treated as an error.
- Resolver errors — the HTTP status of §8 for the resolver's Erlang status
  (400 for `error`, including `{error, {no_viable_responses, _}}`), or the
  message's own status if present.

(Error *strings* above are human-readable bodies; their associated numeric
`status` is set by §8. Where this spec gives a HyperBEAM-style outcome class
name — `not_found`, `no_viable_responses`, etc. — it denotes the
status-mapping class, not a wire string.)

## 13. Composition

- **Node boundary.** The node server's request handler
  (`hb_http_server:resolve_request/3`), not `meta@1.0`, is invoked once per
  inbound request and *wraps* all downstream resolution. Devices, `meta@1.0`
  among them, compose *inside* its resolve step (step 4), reached by the
  request path.
- **Hook composition.** The `request`/`response` hooks let a node insert global
  behaviour (auth, accounting, rewriting, redirection) around every request
  without changing downstream devices. Returning `{ok, []}` from the `request`
  hook is the idiom for "send the caller to the default index".
- **`info` as the bootstrap surface.** Before initialisation only the path
  `/~meta@1.0/info` is reachable; initialising the node is done by `POST`ing
  to `info`. `info` is the single admin-gated control surface for the whole
  node configuration.
- **Delegation to `message@1.0`.** Forcing results into messages, reading
  committers, computing addresses, filtering private keys, and committing
  (signing) all follow `message@1.0` semantics.

## 14. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each externally observable
via the node's request boundary:

1. The device exports exactly the keys `info`, `build`, `is-operator`, and
   answers no other key as a normal device.
2. For every inbound request, the node runs **parse → `request` hook →
   resolve → `response` hook → reply**, in that order, with each reply
   carrying the HTTP status of §8.
3. A `request` hook returning `{error, _}` halts the request: the resolver and
   the `response` hook do **not** run, and the hook's error (status-wrapped) is
   the response.
4. A `request` hook returning `{ok, NewBody}` with a non-empty body causes
   `NewBody` to be resolved instead of the original sequence (a pre-processor can
   add a key to a request and have it take effect — e.g. resolving a path the
   original request did not name).
5. A `request` hook returning `{ok, []}` produces a **307** redirect whose
   `location` is the `default-request` option, defaulting to
   `/~hyperbuddy@1.0/index`.
6. A `response` hook can rewrite or halt the result; both hooks receive a request
   of shape `#{ request := <request>, body := <input> }`; the returned `body`
   of each is used, and the returned `request` of the `request` hook
   (default: the request singleton) is the `request` of the `response` hook.
7. On an **uninitialised** node, only `meta@1.0/info` is served; every other
   request returns the error `Node must be initialized before use.`
8. `GET`-style `info` returns the node message with private keys removed,
   unencodable values replaced by `Unencodable value.`, and (when a wallet
   exists) a dynamic `address` key added; the requested non-private config key is
   readable from the result.
9. `POST info` signed by the admin (§7) over `type: node-message` merges the
   keys that the admin signed into the node message, appends them to
   `node-history`, preserves the server-identity binding, and returns
   `history-length` = the new history length; the change is visible to
   subsequent `GET info` reads.
10. `POST info` **not** signed by the admin (§7) makes no change (the attempted
    key does not appear on a later `GET info`, and `node-history` does not
    grow) and returns an authorisation error.
11. An **unclaimed** node with no `admin` option cannot be claimed: a
    `POST info` that sets `operator`, signed or not, makes no change, and
    `node-history` does not grow.
12. Once `initialized = permanent`, no `POST info` (operator or otherwise)
    changes the node message; the update returns the permanence error and a later
    read shows the pre-permanence values.
13. `is-operator` returns `false` for any caller on an unclaimed node and `true`
    iff the operator is a committer of the tested message (the request body, or
    the request) on a claimed node.
14. `build` returns `node = HyperBEAM`, a `version`, a full `source` hash, a
    separate `source-short` hash, and a `build-time`; each field is individually
    addressable (e.g. `build/version`).
15. Status mapping follows §8: `ok`→200, `created`→201, `not_found`→404,
    `unauthorized`→401, `forbidden`/`not_authorized`→403, `failure`→500,
    `unavailable`→503, `error`/`client_error`/`no_viable_responses`→400,
    otherwise→200; an explicit message `status` from 100 to 599 overrides
    the default.
16. When `force-signed` is configured, the node signs the `hashpath` of each
    message it sends that is the result of its hashpath and has no `hashpath`
    key of its own, also when the message is already committed; it signs no
    other message.
17. The response codec is taken from the request's `accept`, not hard-coded.

## 15. Out of scope

- The internal representation of the node message and of any request/result
  message.
- The full set of node configuration options (only the keys `meta@1.0` itself
  reads/writes are pinned here); the meaning of options consumed by *other*
  devices (routes, stores, payment, ACLs, etc.) is defined by those devices.
- The wire-level request parsing and response encoding (content negotiation) —
  performed by the substrate around this device; only its contract surface
  (`accept`, allowed methods, the parsed-singleton input) is specified here.
- The cryptographic details of commitment/verification (see `message@1.0` and
  the commitment device).
- The hook *device(s)* a node binds under its `on` configuration: their internal
  behaviour is their own; this spec pins only the request/response shape and
  halt/continue contract `meta@1.0` relies on.
- Performance, storage strategy, and result-cache configuration.

## 16. Open questions

- **`not_authorized` status code.** The unauthorised-update path returns
  `{error, #{ body => not_authorized }}`, which §8 maps to HTTP **403**.
- **`default-request` vs `default-index` naming.** The redirect target option is
  named `default-request` (value `/~hyperbuddy@1.0/index`). A separate
  `default-index` notion exists in node configuration but does not drive this
  device's redirect; this spec pins `default-request` as the operative key.
- **Hook configuration binding.** This spec pins the hook *request/response
  contract* but not *how* a node names the device that backs a `request` /
  `response` hook (the `on` configuration structure). Two nodes could wire hooks
  via different configuration encodings yet both satisfy this spec, provided the
  fired hook observes the §5 contract.
- **Identity-derived addresses.** The dynamic per-identity `address` added under
  `identities` on an `info` read depends on each identity's own wallet/address
  resolution; the exact identity-record shape is governed by whatever device
  populates `identities` and is not pinned here.
