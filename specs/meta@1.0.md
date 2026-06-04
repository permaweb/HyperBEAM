# `meta@1.0` — the node entry-point device

- **Device name:** `meta@1.0`
- **Depends-on:** `message@1.0` (the base device against which results are
  forced into messages, commitments are read/written, and signers are
  determined). The `message@1.0` spec is provided to reimplementers. This device
  also invokes two **request hooks** named `request` and `response`; the hook
  mechanism is described behaviourally in §2 and §5 (no hook *device* spec is
  required to implement `meta@1.0`).
- **Status:** Draft

## 1. Overview

`meta@1.0` is the **node entry point**: the device a node binds to handle
*every* incoming external request before any other device runs. It is not a
device a user normally addresses by path during ordinary resolution; instead the
node hands each parsed request to this device's request-handling entry point,
and the device drives the whole request lifecycle —
**request → preprocess → resolve → postprocess → reply**.

Around that lifecycle it also exposes a small set of **operator/introspection
keys**: reading and (operator-only) updating the node's own configuration
message (`info`), reporting the node's software version and build provenance
(`build`), and answering whether a request is authorised by the node operator
(`is-operator`).

A node MUST route inbound requests through this device. The substrate's
content-negotiation (request parsing and response encoding) sits *outside* this
device and is described here only where it affects the device's contract (§6).

## 2. Concepts & terminology

- **Node message (`NodeMsg`):** the node's own configuration message — a map of
  options that parameterise the whole node (operator address, wallet, stores,
  routes, hooks, payment config, etc.). It is supplied to the device on every
  invocation as the options/configuration context, and is the message that
  `info` reads and (under operator authority) updates. Its full key set is
  open-ended and out of scope; this spec pins only the keys `meta@1.0` itself
  reads or writes.
- **Operator:** the identity authorised to change the node message. Resolved as:
  the node message's `operator` option if set; else the address of the node's
  configured wallet if a wallet exists; else the sentinel **`unclaimed`**. See
  §7 for the exact authority test.
- **Unclaimed node:** a node whose effective operator is `unclaimed` (no
  `operator` option and no wallet). On an unclaimed node, the operator-authority
  test passes for **any** caller (§7) — this is the mechanism by which a node is
  first "claimed" (an unauthenticated caller sets `operator`, after which only
  that operator may make further changes).
- **Request singleton:** the parsed, normalised form of one inbound request — a
  **sequence (list) of messages** to be resolved in order by the substrate's
  resolver (the same "resolve-many" form `apply@1.0`/the substrate consume). The
  exact parse from wire form to this sequence is the substrate's job; this device
  receives the already-parsed sequence plus the original raw request.
- **Hook:** a configurable extension point the node message names under its `on`
  configuration. `meta@1.0` fires two hooks by name — `request` (before
  resolution) and `response` (after resolution). A hook is invoked with a
  request message and returns `{ok, Result}` to continue, or an error to halt.
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
- **Request-handling entry point (not a resolved key):** in addition to the
  three keys, the device provides a **request handler** — the function the node
  invokes for every inbound request. It takes the **node message** and the
  **raw request** and returns `{ok, Result}` where `Result` is the message to
  encode back to the caller. This entry point is described normatively in §4 as
  the *lifecycle*; it is invoked by the node server, never reached by ordinary
  path resolution.
- **Message shapes:**
  - The **request handler** consumes the raw inbound request and the parsed
    request singleton (a list of messages). No specific keys are required of the
    caller beyond what the addressed downstream device requires; the path
    selects the downstream device and key.
  - `info` reads the request's **`method`** (`GET`-like vs `POST`) to decide
    read vs update, and on update reads the (signed) request body as the set of
    node-message keys to merge.
  - `is-operator` reads the request's optional **`body`** (the message whose
    signers are tested), falling back to the request itself.
  - `build` reads nothing from the request.

## 4. Request lifecycle (normative)

The request handler MUST implement the following pipeline. Inputs: the node
message `NodeMsg` and the raw inbound request `Req`.

1. **Parse.** Convert `Req` into the request singleton `Msgs` (an ordered list
   of messages) using the substrate's request-parsing. (The substrate also
   negotiates a commitment/response codec and supplies it to the handler in
   `NodeMsg`; see §6.)

2. **Initialisation gate.** Read the node message's `initialized` option
   (default: treated as not-initialised / `false`).
   - If the node is **not initialised**, the handler MUST refuse to run general
     computation. The single permitted request is a read of this device's own
     `info` key: if the parsed sequence addresses device `meta@1.0` at path
     `info`, serve `info` (§4.a / the `info` key, §`info` below); otherwise
     return error message **`Node must be initialized before use.`** (with an
     error status, §8). A node whose `initialized` is any truthy/initialised
     value (including the string `permanent`) skips this gate and proceeds.

3. **Pre-process (`request` hook).** Invoke the `request` hook with the hook
   request shape of §5, carrying the raw request and the parsed sequence as the
   hook **body**. The hook's outcome decides what happens next:
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
   visible to the resolution). The raw result is wrapped with an HTTP status per
   §8.

5. **Post-process (`response` hook).** Invoke the `response` hook with the hook
   request shape of §5, carrying the original raw request as `request` and the
   **resolution result** as the hook **body**. As with the pre-processor:
   - an **error** return halts and becomes the response (wrapped with a status);
   - an `{ok, NewBody}` return makes `NewBody` the result to return — the
     post-processor MAY pass the result through unchanged or rewrite/replace it.

6. **Optional signing.** If the node message's `force-signed` option is set
   (truthy), and the result has **no** committers (is unsigned), the handler
   MUST commit (sign) the result with the node's identity before returning it. A
   result that already carries a commitment MUST NOT be re-signed. If
   `force-signed` is not set, the result is returned uncommitted.

7. **Return.** Return `{ok, Result}`. Every returned message MUST carry a
   `status` key (§8). The substrate then encodes `Result` back to the caller per
   the negotiated codec (§6).

A node MUST NOT skip the hooks: if no hook is configured for a name, the hook is
a no-op that passes its body through unchanged (continue). A node MUST NOT run
the resolver if the `request` hook halted, and MUST NOT run the `response` hook
if either the `request` hook or the resolver path halted.

## 5. Hooks: request shape & halt semantics (normative)

Both hooks are invoked with a **hook request message** of exactly this shape:

```
request => <the original raw inbound request singleton>
body    => <the pre-process input OR the post-process input>
```

- For the **`request`** hook, `body` is the parsed message **sequence** to be
  resolved.
- For the **`response`** hook, `body` is the **resolution result** message.

A hook implementation returns one of:
- `{ok, #{ body := NewBody, … }}` — **continue**; `NewBody` becomes the new
  body (the sequence to resolve, or the result to return). Only the hook's
  `body` is taken; other keys of the hook's return are ignored by `meta@1.0`.
- `{error, _}` — **halt**; the error becomes the response (status-wrapped, §8).
- Any other return — treated as an **error** (halt).

The hook name (`request` / `response`) and the node's hook configuration binding
are part of the node message (`on` configuration); `meta@1.0` only fires the
hook by name and interprets the result per the rules above. The two hook
invocations re-read the (possibly updated) node options around the resolve step:
a `request` hook that mutates node configuration is reflected in the options used
for resolution and for the `response` hook.

## 6. Content negotiation & node-server context

Content negotiation is performed by the substrate **around** this device, not by
the device itself, but it forms part of the device's operating contract:

1. The inbound wire request is parsed into the request singleton before the
   handler runs (§4 step 1).
2. The substrate selects a **commitment/response codec** from the request's
   `accept` (content-negotiation) information and places it into the node
   message handed to the handler under the **`commitment-device`** key. Any
   signing the handler performs (§4 step 6) and the final response encoding use
   this negotiated codec.
3. The allowed request methods at the node boundary are `GET`, `POST`, `PUT`,
   `DELETE`, `OPTIONS`, `PATCH`; CORS-preflight (`OPTIONS`) is handled by the
   substrate and never reaches this device. This device distinguishes only
   `POST` (update) from non-`POST` (read) for the `info` key (§`info`).

An implementer of `meta@1.0` MUST treat the negotiated codec as supplied input
(read it from `commitment-device` when committing/encoding); it MUST NOT
hard-code a single response codec.

## 7. Operator authority (normative)

Two related notions appear; pin both precisely.

### Resolving the operator address
The effective operator address is computed from the node message as:
1. If the `operator` option is set, use it.
2. Else, if the node has a configured wallet, use that wallet's **address**.
3. Else, the operator is the sentinel **`unclaimed`**.

Addresses are compared in their **human-readable (base64url) form**. A node
message `operator` value of the literal string `unclaimed` is equivalent to the
unclaimed sentinel.

### The authority test (used to gate node-message updates)
A request is **authorised as operator** iff:
- the effective operator is **`unclaimed`** (any caller passes — the unclaimed
  case), **OR**
- the effective operator's address is a member of the request's **committers**
  (i.e. the request is signed by the operator).

This is failure-closed for a claimed node: a request that is unsigned, or signed
only by non-operators, is **not** authorised.

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
(`POST info`) applies the *same* operator-or-unclaimed test internally as its
admin gate before allowing a change. (A node MAY also define stricter
identity notions — e.g. "is the original initiator who first configured the node"
— but the only authority `meta@1.0` requires for node-message updates is the
operator-or-unclaimed test.)

## 8. Resolved keys (normative)

### `info` — read or operator-update the node message
- **Reads:** the request `method`; on update, the request body (its keys) and
  its committers; the node message (`operator`, `initialized`, wallet,
  `node-history`, and all configuration keys).
- **Behaviour:**
  - **Read (`method` ≠ `POST`):** Return the node message as a message, with:
    1. **Private keys removed.** Any key that is private (per `message@1.0`'s
       private-key definition: `private`, `private.*`/`private-*`, and the
       legacy `priv`/`priv_*`/`priv.*` forms) MUST NOT appear in the result.
    2. **Unencodable values redacted.** Any value that is not encodable into a
       message (e.g. an opaque runtime tuple/handle) MUST be replaced by the
       string `Unencodable value.` Maps and lists are filtered recursively.
    3. **Dynamic keys added.** If the node has a wallet, add `address` = the
       wallet's address (human-readable form). If the node message carries an
       `identities` map, add to each identity an `address` derived from that
       identity's wallet. These dynamic keys are computed per-request and are not
       stored in the node message.
    The result MUST carry a `status` of 200 (§ status rules below).
  - **Update (`method` = `POST`):** every outcome below (success OR rejection) is
    returned as `{ok, ResultMessage}` where `ResultMessage` is a **forced message**
    `#{ status => <code>, body => <text> }` — NEVER an Erlang `{error, Binary}`
    tuple. The "status-wrapped error" phrasing means *a message carrying an error
    status*, not the `{error, _}` return form.
    1. If the node is **permanent** (§7), reject: return `{ok, #{ status => 400,
       body => <<"The node message of this machine is already permanent. It cannot
       be changed.">> }}`.
    2. Else apply the **operator authority test** (§7) to the request. If it
       fails, return `{ok, #{ status => 400, body => <<"Unauthorized">> }}` (400 by
       the default error mapping; an implementation MAY map it to 401 — see Open
       questions) and make **no** change.
    3. Else **adopt** the request as a node-message update (see *Adopting a node
       message* below). On success return a message whose `body` is a human
       summary of the form `Node message updated. History: <N> updates.` and
       whose `history-length` is `N`, the new node-history length. On failure
       return the status-wrapped adoption error.
- **Returns:** `{ok, Message}` with a `status`, or a status-wrapped error
  message.
- **Side effects (update only):** mutates the live node configuration and
  appends to `node-history` (see below). No cache/store writes of its own.

**Adopting a node message (the update mechanism).** Given an authorised,
non-permanent update request, the node message is updated by:
1. Re-checking permanence (reject with `Node message is already permanent.` if
   now permanent).
2. Taking the request's **uncommitted** content (commitments are stripped — the
   stored configuration is the values, not the signature envelope).
3. **Shallow-merging** those keys over the current node message (request values
   overwrite existing options).
4. **Appending** the update (with private keys reset and any incoming
   `node-history` key stripped) to the node message's `node-history` list — so
   `node-history` is the ordered list of accepted configuration changes, oldest
   first. The first entry is the request that first configured/claimed the node.
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
  the answer is `true` for any caller; on a claimed node it is `true` iff the
  operator's address is among the committers.
- **Returns:** `{ok, Boolean}`.
- **Side effects:** none.

### Status wrapping (applies to every returned result)
Every result this device returns MUST carry a numeric `status`. The status is
determined in this order of precedence:
1. If the result message already commits to / carries a `status`, keep it.
2. Else if the message body or a `status` field denotes a status (an integer
   status, an atom status name, or a binary convertible to an integer or to a
   known status name), use that.
3. Else map the Erlang-style outcome class to a default HTTP code:
   - `ok` → **200**
   - `created` → **201**
   - `error` / `client-error` / `no-viable-responses` (including
     `{no_viable_responses, _}`) → **400**
   - `not_found` → **404**
   - `unauthorized` → **401**
   - `forbidden` → **403**
   - `failure` → **500**
   - `unavailable` → **503**
   - any other outcome → **200**.
When the result is not a map (a bare value/binary), it MUST be wrapped as a
message `#{ status => <code>, body => <value> }`.

## 9. Data formats & encodings

- Operator/committer addresses are **base64url** (human-readable) form, never
  hex; comparisons are on that form.
- The negotiated response/commitment codec is named under `commitment-device`
  in the node message (§6); signing and encoding use it.
- `node-history` is an **ordered list** of accepted update requests (oldest
  first); its length is reported by `history-length` on a successful update.
- `info` read output is a plain message: private keys removed, unencodable
  values replaced by the binary `Unencodable value.`, dynamic `address` keys
  added as described.
- All node-message keys this device reads/writes are lowercase, hyphenated
  binaries on the wire (`operator`, `initialized`, `node-history`,
  `default-request`, `force-signed`, `commitment-device`, `http-server`,
  `identities`, `address`, `version`, `source`, `source-short`, `build-time`).

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

- **Failure-closed updates.** Node-message updates require operator authority
  (§7). On a *claimed* node an unauthorised request MUST NOT change any
  configuration and MUST return an authorisation error. The only "open" case is
  the **unclaimed** node, by design, so the node can be claimed once.
- **Permanence is irreversible.** Once `initialized = permanent`, no update —
  including by the operator — may change the node message.
- **Privacy.** The `info` read MUST NOT leak private keys (wallets, secrets) or
  unencodable runtime handles. Private-key filtering follows `message@1.0`.
- **Signing on the way out is opt-in.** Results are returned unsigned unless
  `force-signed` is configured; even then, an already-committed result is left
  as-is (never double-signed).
- **Hooks are trusted node configuration.** The `request`/`response` hooks can
  inspect, rewrite, or halt any request/response; they are part of the operator's
  node configuration, not user input, and are the node's global
  interception/authorisation point.

## 12. Errors

All errors are returned as status-wrapped messages (§8). The conditions:

- `Node must be initialized before use.` — a non-`info` request reached an
  uninitialised node (status 400 via `error`).
- `The node message of this machine is already permanent. It cannot be changed.`
  — a `POST info` on a permanent node.
- `Node message is already permanent.` — the adopt path independently detected
  permanence.
- `Unauthorized` — a `POST info` whose request failed the operator authority
  test on a claimed node.
- Any error returned by the `request` or `response` **hook** — propagated as the
  response unchanged (status-wrapped). A hook returning a non-`{ok,_}`,
  non-`{error,_}` value is treated as an error.
- Resolver errors (including `no-viable-responses`) — status-wrapped to 400 (or
  the message's own status if present).

(Error *strings* above are human-readable bodies; their associated numeric
`status` is set by §8. Where this spec gives a HyperBEAM-style outcome class
name — `not_found`, `no-viable-responses`, etc. — it denotes the status-mapping
class, not a wire string.)

## 13. Composition

- **Node boundary device.** `meta@1.0` is the outermost device of a node: it is
  invoked once per inbound request and *wraps* all downstream resolution. Other
  devices compose *inside* its resolve step (step 4), reached by the request
  path.
- **Hook composition.** The `request`/`response` hooks let a node insert global
  behaviour (auth, accounting, rewriting, redirection) around every request
  without changing downstream devices. Returning `{ok, []}` from the `request`
  hook is the idiom for "send the caller to the default index".
- **`info` as the bootstrap surface.** Before initialisation only
  `meta@1.0/info` is reachable; claiming/initialising the node is done by
  `POST`ing to `info`. After a node is claimed, `info` is the single
  operator-gated control surface for the whole node configuration.
- **Delegation to `message@1.0`.** Forcing results into messages, reading
  committers, computing addresses, filtering private keys, and committing
  (signing) all follow `message@1.0` semantics.

## 14. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each externally observable
via the node's request boundary:

1. The device exports exactly the keys `info`, `build`, `is-operator`, and
   answers no other key as a normal device.
2. Every inbound request is driven through **parse → `request` hook → resolve →
   `response` hook → reply**, in that order, with each returned message carrying
   a numeric `status`.
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
   of shape `#{ request := <raw request>, body := <input> }` and only their
   returned `body` is used.
7. On an **uninitialised** node, only `meta@1.0/info` is served; every other
   request returns the error `Node must be initialized before use.`
8. `GET`-style `info` returns the node message with private keys removed,
   unencodable values replaced by `Unencodable value.`, and (when a wallet
   exists) a dynamic `address` key added; the requested non-private config key is
   readable from the result.
9. `POST info` signed by the operator (or any caller on an **unclaimed** node)
   merges the request's uncommitted keys into the node message, appends the
   request to `node-history`, preserves the server-identity binding, and returns
   `history-length` = the new history length; the change is visible to subsequent
   `GET info` reads.
10. `POST info` **not** signed by the operator on a **claimed** node makes no
    change (the attempted key does not appear on a later `GET info`, and
    `node-history` does not grow) and returns an authorisation error.
11. A node can be **claimed** from `unclaimed`: a caller sets `operator`, after
    which only that operator may make further changes (the history grows by one
    per accepted change).
12. Once `initialized = permanent`, no `POST info` (operator or otherwise)
    changes the node message; the update returns the permanence error and a later
    read shows the pre-permanence values.
13. `is-operator` returns `true` for any caller on an unclaimed node and `true`
    iff the operator is a committer of the tested message (the request body, or
    the request) on a claimed node.
14. `build` returns `node = HyperBEAM`, a `version`, a full `source` hash, a
    separate `source-short` hash, and a `build-time`; each field is individually
    addressable (e.g. `build/version`).
15. Status mapping follows §8: `ok`→200, `created`→201, `not_found`→404,
    `unauthorized`→401, `forbidden`→403, `failure`→500, `unavailable`→503,
    error/no-viable-responses→400, otherwise→200; an explicit message `status`
    overrides the default.
16. When `force-signed` is configured, an otherwise-unsigned result is signed
    before return; an already-committed result is not re-signed.
17. The response/commitment codec is taken from the negotiated
    `commitment-device` supplied with the node message, not hard-coded.

## 15. Out of scope

- The internal representation of the node message and of any request/result
  message.
- The full set of node configuration options (only the keys `meta@1.0` itself
  reads/writes are pinned here); the meaning of options consumed by *other*
  devices (routes, stores, payment, ACLs, etc.) is defined by those devices.
- The wire-level request parsing and response encoding (content negotiation) —
  performed by the substrate around this device; only its contract surface
  (`commitment-device`, allowed methods, the parsed-singleton input) is pinned.
- The cryptographic details of commitment/verification (see `message@1.0` and
  the commitment device).
- The hook *device(s)* a node binds under its `on` configuration: their internal
  behaviour is their own; this spec pins only the request/response shape and
  halt/continue contract `meta@1.0` relies on.
- Performance, storage strategy, and result-cache configuration.

## 16. Open questions

- **`Unauthorized` status code.** The default outcome→status mapping (§8) maps a
  plain `error` to **400**, so the unauthorised-update path yields HTTP 400 even
  though a dedicated `unauthorized → 401` mapping exists. An implementation that
  tagged the failure as `unauthorized`/`forbidden` would return 401/403 instead.
  The reference returns the `Unauthorized` *string* with the generic 400 mapping;
  reimplementers should match that (400) unless a future revision pins 401.
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
