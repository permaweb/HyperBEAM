# `relay@1.0` — relay/proxy a request to another node or HTTP endpoint

- **Device name:** `relay@1.0`
- **Depends-on:** `message@1.0` (target selection, `commit`/`verify`, identity-key semantics), `httpsig@1.0` (default commitment device used when re-signing a relayed request). Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`relay@1.0` is the node's **outbound HTTP client device**. It takes a request,
constructs an outbound HTTP message from it, dispatches that message to either an
explicitly named peer or a destination chosen by the node's routing table, and
returns the remote peer's response. It is the protocol's bridge from the AO-Core
message world to arbitrary HTTP(S) endpoints (other AO nodes or traditional web
services).

It offers a **synchronous** mode (`call` — wait for and return the remote
response) and an **asynchronous** mode (`cast` — fire-and-forget, return
immediately). A third key (`request`) is a rewriter that turns *any* inbound
request into a `call` relay, so a node can be configured as a pure forwarding
proxy. The device can optionally **re-sign** (commit) the outbound request with
the node's own key before dispatch, gated by node policy.

## 2. Concepts & terminology

- **Relay target (the outbound message):** the AO-Core message this device sends
  over HTTP. It is derived from a **base target** (§3, "target selection") with
  HTTP routing fields (`path`, `method`, optionally `body`, `device`) overlaid,
  `commitments` stripped, and — if requested and permitted — a fresh commitment
  applied. The internal representation is out of scope; what is normative is the
  set of keys present on the outbound message and their values.
- **Peer:** an explicitly supplied destination for the request — a node URL or
  host (e.g. `https://example.com` or another AO node's address). When a peer is
  given, the device dispatches directly to it. When no peer is given, the device
  hands the outbound message to the node's HTTP request machinery, which selects
  the destination from the node's **routing table** (see the router device's
  spec; route selection is out of scope here).
- **Routing table:** the node-level configuration mapping request templates to
  destination peers/strategies. This device does not implement routing; it relies
  on the substrate's HTTP request path to apply it when no explicit peer is given.
- **Commit / re-sign:** producing a signed commitment over the outbound message
  using the node's wallet, via the `commit` operation of `message@1.0`
  (cryptography delegated to the default commitment device, `httpsig@1.0`). This
  attests the relayed request as having been issued by *this* node.
- **`get-first` precedence:** several routing fields are resolved by trying an
  **ordered list of candidate sources** and taking the **first** that yields a
  value; if none yields a value the field is **absent** (the sentinel "not
  found"). The ordered lists are pinned per field in §4; an implementation MUST
  evaluate them in the stated order and stop at the first hit.
- **`M1` / base message vs `Req` / request message:** `M1` is the message the
  device is bound to (the message carrying `device => relay@1.0`); `Req` is the
  request message for this resolution step (carries the `path` segment, e.g.
  `call`, and per-call routing fields). Both are read as candidate sources for
  routing fields; the exact per-field source order is in §4.

## 3. Device interface

- **Dispatch shape:** **explicit-keys.** The device answers exactly the three
  keys named in §4 (`call`, `cast`, `request`). It installs **no**
  default/catch-all handler and declares no additional exported keys; any other
  key (including the message-manipulation keys `keys`/`set`/`set-path`/`remove`
  and the commitment keys `id`/`commit`/`verify`) is not captured by this device
  and resolves under the base identity device (`message@1.0`) for the message it
  is bound to. There is therefore nothing to exclude from a default handler
  (there is no default handler).

- **Target selection (shared by `call` and `cast`):** the **base target** is
  selected from the request via the standard `target` indirection:
  - If `Req` has no `target` key, or `target` is the literal binary `self`, the
    base target is the **base message `M1`** (the original message).
  - Otherwise `target` names a key: the base target is the value of that key in
    `Req`; if that key is absent, it falls back to the value of `Req`'s `body`
    key.

  This is the same target-selection rule defined by `message@1.0`. The base
  target is then transformed into the outbound message per §4.`call`.

- **Message shape — routing fields (read from `M1`, the base target, and `Req`):**
  all optional, each resolved by a pinned precedence list (§4). The recognised
  fields are: `path` / `relay-path`, `method` / `relay-method`, `body` /
  `relay-body`, `peer`, `relay-device`, and the commit toggle `commit-request` /
  `relay-commit-request`. Two further inputs are read from the base target only:
  `http-client` (select the outbound HTTP client) and the implicit
  `commitments`/`device` keys that are rewritten on the outbound message.

## 4. Resolved keys (normative)

### `call` — synchronous relay (send and return the response)

- **Reads:**
  - The base target (target selection, §3).
  - The following routing fields, each resolved by **first-match** over the pinned
    candidate source list (sources written as `{message, key}`; "target" means the
    base target read **as a `message@1.0` message**, i.e. its plain key values;
    `M1` is the base message; `Req` is the request message). A field with no hit
    is **absent**:
    - **relay path** (the outbound `path`): in order —
      `{M1, path}`, `{target, path}`, `{Req, relay-path}`, `{M1, relay-path}`.
    - **relay device** (the outbound `device`): in order —
      `{M1, relay-device}`, `{target, relay-device}`, `{Req, relay-device}`.
    - **peer**: in order — `{M1, peer}`, `{target, peer}`, `{Req, peer}`.
    - **relay method** (the outbound `method`): in order —
      `{M1, method}`, `{target, method}`, `{Req, relay-method}`,
      `{M1, relay-method}`, `{Req, method}`.
    - **relay body** (the outbound `body`): in order —
      `{M1, body}`, `{target, body}`, `{Req, relay-body}`, `{M1, relay-body}`,
      `{Req, body}`.
    - **commit toggle** (`commit-request`), resolved with **default `false`**: in
      order — `{target, commit-request}`, `{Req, relay-commit-request}`,
      `{M1, relay-commit-request}`, `{Req, commit-request}`, `{M1, commit-request}`.
  - The base target's `http-client` key (outbound client selection), if present.
  - The node options: `relay-allow-commit-request` (policy gate for re-signing,
    default `false`) and `relay-http-client` (the node's default outbound HTTP
    client when the request does not name one).

- **Behaviour:** Construct the **outbound message** from the base target, then
  dispatch it:
  1. Start from the base target.
  2. If a **relay body** was found, set the outbound `body` to it; otherwise leave
     the base target's `body` (if any) unchanged.
  3. Set the outbound `method` to the resolved **relay method** and the outbound
     `path` to the resolved **relay path** (both keys are always written, even if
     their resolved value is the absent sentinel — see Open questions).
  4. If a **relay device** was found, set the outbound `device` to it; otherwise
     **remove** the `device` key from the outbound message entirely (so the
     dispatched message carries no device override and is treated as a plain
     message by the receiver).
  5. **Remove `commitments`** from the outbound message (the relayed message is
     not forwarded carrying the caller's signatures).
  6. **Commit gate:** coerce the commit toggle to a boolean.
     - If the toggle is **`true`**: consult the node option
       `relay-allow-commit-request`.
       - If that option is **`true`**, **commit** (sign) the outbound message with
         the node's key (the `message@1.0` `commit` operation; default commitment
         device `httpsig@1.0`), then **verify** that the freshly committed message
         validates under **all** its commitments — the implementation MUST require
         this verification to succeed. The committed message becomes the outbound
         message.
       - If that option is **`false`** (or unset), the device MUST fail with
         `relay-commit-request-not-allowed` (§8) and dispatch nothing.
     - If the toggle is **`false`**: leave the outbound message uncommitted.
  7. **Verify** the outbound message (the `message@1.0` `verify` operation): the
     implementation MUST require this to succeed before dispatch. (For an
     uncommitted message this confirms there are no dangling/invalid commitments;
     for a re-committed message this is in addition to the post-commit
     verification in step 6.)
  8. **Select the outbound HTTP client:** if the base target carries an
     `http-client` value, use it; otherwise use the node option
     `relay-http-client`. (The set of valid client identifiers is substrate
     configuration and out of scope.)
  9. **Dispatch.** Build the dispatch options by adding to `Opts` the selected
     HTTP client and the **full-response flag**: `Opts#{ <<"http-client">> =>
     <Client>, <<"http-only-result">> => false }`. **The `http-only-result` flag
     (a hyphenated binary key) MUST be set to `false`** — the substrate's HTTP
     request machinery defaults it to `true`, which yields a **status-only**
     result; `false` is what makes the dispatch return the **full response
     message** required by step 10. Then:
     - If **no peer** was found: hand the outbound message to the node's HTTP
       request machinery (the single-argument request form), which selects the
       destination via the node's routing table and sends it.
     - If a **peer** was found: dispatch the outbound message **directly** to that
       peer, using the resolved **relay method** and **relay path** as the HTTP
       method and request path against the peer.
  10. On a successful response message `R`, **remove the `set-cookie` key** from
      `R` and return `{ok, R}`. On a failed dispatch, return the underlying error
      unchanged.

- **Returns:** `{ok, ResponseMessage}` — the remote peer's response, with
  `set-cookie` stripped — or an error (either `relay-commit-request-not-allowed`,
  or the underlying transport/resolution error propagated unchanged). MUST NOT
  invent a default response on failure.

- **Side effects:** an **outbound HTTP(S) request** to a peer (chosen explicitly
  or via the routing table) — the device's defining side effect. Optionally a
  **commitment (signing)** of the outbound message with the node's key. No cache
  or store write of its own.

### `cast` — asynchronous relay (fire-and-forget)

- **Reads:** identical inputs to `call` (the asynchronous execution reads the
  same `M1`, `Req`, and node options when it runs).
- **Behaviour:** Initiate the **same relay operation as `call`** asynchronously
  — i.e. perform exactly the `call` construction-and-dispatch described above,
  but **without waiting** for it to complete — and return immediately. The remote
  response (and any error, including `relay-commit-request-not-allowed`) is
  **discarded**: it is neither returned to the caller nor surfaced as the result.
- **Returns:** `{ok, <<"OK">>}` — always, immediately, regardless of whether the
  asynchronous relay later succeeds or fails.
- **Side effects:** the same outbound HTTP(S) request (and optional commitment) as
  `call`, performed asynchronously after the key has already returned.

### `request` — rewrite an inbound request into a relay (proxy hook)

- **Reads:** the request message's `request` key (the original inbound request
  to be proxied). The base message is ignored.
- **Behaviour:** Return a message whose `body` is a **two-element ordered list**
  that, when resolved, performs a `relay@1.0/call` of the original request:
  1. The first element selects the relay device: `#{ device => relay@1.0 }`.
  2. The second element invokes the relay: a message with
     `path => call`, `target => body`, and `body => <the original request>`
     (the value of `Req`'s `request` key). `target => body` instructs `call`'s
     target selection to use that element's `body` (the original request) as the
     base target.
- **Returns:** `{ok, #{ body => [ ... ] }}` — the rewrite message described above.
  MUST NOT error.
- **Side effects:** none (it performs no network call itself; it produces the
  message that, once resolved, will). This key is intended to be installed as the
  node's `on`/`request` hook (its conceptual role is the node's request
  *preprocessor*): when so installed, it intercepts every inbound request and
  rewrites it into a `call` relay, turning the node into a forwarding proxy whose
  destinations come from its routing table.

## 5. Data formats & encodings

- All keys are binary, lowercase, hyphenated. Routing-field key names are exactly:
  `path`, `relay-path`, `method`, `relay-method`, `body`, `relay-body`, `peer`,
  `relay-device`, `commit-request`, `relay-commit-request`, `http-client`,
  `target`, `device`, `commitments`. The node option keys are
  `relay-allow-commit-request` and `relay-http-client`. (Implementations whose
  option/key layer canonicalises case or underscore MUST resolve these to the
  same keys as their hyphenated binary forms.)
- The `request` rewrite emits the literal binaries `relay@1.0` (device),
  `call` (path), `body` (target), and `OK` (the `cast` return). The `target`
  sentinel for "use the original message" is the literal binary `self`.
- The commit toggle is interpreted as a boolean: it is coerced to the atoms
  `true`/`false`. Any non-`true` coercion is treated as `false`. The absence of
  the toggle defaults to `false`.
- The device does not itself derive IDs, hashpaths, or commitment shapes; when it
  re-signs, the commitment shape and IDs are exactly those produced by the
  underlying `commit` operation (`message@1.0` + `httpsig@1.0`) and are
  base64url-encoded per those specs. No value originating in this device is
  hex-encoded.
- The response is returned as the response **message** received from the peer
  (the full message, not a status-only summary), modified only by the removal of
  its `set-cookie` key.

## 6. Ordering, freshness & caching

- `call` is **not** a pure function: it performs a network request, so its result
  depends on the remote peer's state at dispatch time. Re-resolving the same path
  re-issues the request.
- The routing-field precedence lists (§4) are evaluated **in the pinned order**;
  the first source that yields a value wins, and ordering of these candidate
  sources is significant and MUST be preserved.
- The `request` rewrite's body is an **ordered** two-element list: the
  device-switch element MUST precede the `call` element. Reordering changes the
  meaning.
- This device performs no result caching of its own. Because `call` produces a
  value that varies per request (and per remote state), a node that caches HTTP
  resolution results MUST be configured so that `~relay@1.0/call` reads are not
  served from a stale result cache when fresh relaying is required. This is node
  configuration, not device behaviour.

## 7. Security & authority

- **Re-signing is failure-closed and policy-gated.** The device re-signs the
  outbound request **only** when both (a) the request asks for it
  (`commit-request`/`relay-commit-request` resolves truthy) **and** (b) the node
  operator has enabled `relay-allow-commit-request`. If re-signing is requested
  but not enabled, the device MUST fail with
  `relay-commit-request-not-allowed` and dispatch nothing — it MUST NOT silently
  dispatch the request unsigned, nor silently sign it. This prevents a relay node
  from being coerced into lending its identity to arbitrary requests unless the
  operator has opted in.
- A re-signed outbound message MUST be **verified** to validate under all its
  commitments before dispatch; an unverifiable message MUST NOT be sent.
- The caller's incoming `commitments` are **always stripped** from the outbound
  message: the relay never forwards a request carrying the caller's signatures.
  Either the outbound message is unsigned, or it carries a fresh commitment from
  *this* node (under the gate above) — never the caller's.
- The response has its `set-cookie` key removed before being returned, so cookies
  set by the remote endpoint are not propagated back through the relay to the
  caller.
- The device itself imposes no authentication on *who* may invoke `call`/`cast`
  — any caller able to resolve the path may trigger an outbound request. Operators
  exposing this device (especially as a `request`/proxy hook) MUST treat it as a
  capability that lets callers cause the node to make outbound network requests,
  and constrain destinations via the routing table accordingly.

## 8. Errors

- `relay-commit-request-not-allowed` — raised by `call` (and, discarded, by the
  asynchronous `cast`) when the request asks to commit the relayed message
  (commit toggle truthy) but the node option `relay-allow-commit-request` is not
  enabled. No request is dispatched. **The raised value is the Erlang `throw` of
  the bare *atom* `relay_commit_request_not_allowed`** (underscored — the
  hyphenated `relay-commit-request-not-allowed` above is the condition's
  human-readable name; the value actually thrown is the underscored atom). It is
  surfaced as a thrown/raised error condition, **not** a returned `{error, _}`
  tuple; an implementation MUST abort the relay and MUST NOT dispatch.
- Any error arising from the outbound dispatch — the peer being unreachable,
  returning a transport/resolution error, route selection failing, etc. — is
  **propagated unchanged** as the result of `call`. This device defines no error
  atom of its own for that case and does not re-wrap the underlying error.
- A failed post-commit or pre-dispatch **verification** is a hard failure of
  `call` (the implementation requires verification to succeed); the message MUST
  NOT be dispatched if verification does not hold.
- `cast` has **no observable error path**: it always returns `{ok, <<"OK">>}`
  immediately; any error from the asynchronous relay is discarded.
- `request` has no error path: it always returns its rewrite message.

## 9. Composition

- **Proxy / preprocessor pattern.** Installing `relay@1.0`'s `request` key as the
  node's `on`/`request` hook converts the node into a forwarding proxy: every
  inbound request is rewritten (via the two-element device-switch + `call` body)
  into a `relay@1.0/call` of the original request, whose destination is chosen by
  the node's routing table. The `commit-request` toggle can be threaded through
  this rewrite (or set at the node) so the proxy re-signs forwarded requests with
  the node's key when policy allows — the basis for "this node signs requests on
  behalf of an unsigned client and forwards them to an executor that requires
  signatures".
- **Device-switching in the returned body.** The `request` rewrite relies on the
  substrate resolving a list whose first element sets `device => relay@1.0`, so
  that the subsequent `call` element is resolved under this device. This is the
  standard returned-value device-switch mechanism.
- **`target` indirection.** Because `call`/`cast` use the standard `target`
  selection of `message@1.0`, a caller can relay either the bound message itself
  (default / `target = self`) or a sub-message named by `target` (or carried in
  `body`, as the `request` rewrite does with `target = body`). This lets one
  resolution wrap an arbitrary inner message as the thing to relay.
- **Routing table.** With no explicit `peer`, `call` defers destination choice to
  the router; with an explicit `peer` it bypasses routing and dispatches directly.
  Both are first-class.

## 10. Conformance (normative checklist)

1. The device answers exactly three resolvable keys — `call`, `cast`, `request`
   — and installs no default/catch-all handler; a request for any other key is
   not captured by this device.
2. `call` and `cast` select their **base target** via the standard `target`
   rule: absent `target` or `target = self` → the bound message; otherwise the
   `target`-named key of the request, falling back to the request's `body`.
3. `call` resolves the outbound **path**, **method**, **body**, **device**, and
   **peer** by first-match over the exact ordered candidate-source lists in §4
   (`call`), stopping at the first source that yields a value; a field with no hit
   is absent.
4. The outbound message has its `method` and `path` set from the resolved relay
   method/path; its `body` set from the relay body **only when one was found**
   (otherwise the base target's body is left as-is); its `device` set from the
   relay device when found, **and otherwise removed**; and its `commitments`
   **always removed**.
5. The commit toggle defaults to `false` and is resolved by first-match over the
   exact list in §4. When it is truthy **and** the node option
   `relay-allow-commit-request` is enabled, `call` commits (signs) the outbound
   message with the node's key and requires the committed message to verify under
   all its commitments before dispatch.
6. When the commit toggle is truthy but `relay-allow-commit-request` is **not**
   enabled, `call` fails with `relay-commit-request-not-allowed` and dispatches
   nothing.
7. Before dispatch, `call` requires the outbound message to verify; an
   unverifiable outbound message is not dispatched.
8. The outbound HTTP client is the base target's `http-client` if present, else
   the node option `relay-http-client`.
9. With **no** resolved peer, `call` dispatches the outbound message via the
   node's routing machinery (destination chosen by the routing table); with a
   resolved **peer**, `call` dispatches directly to that peer using the resolved
   method and path. Both request the full response message (not a status-only
   result).
10. On success, `call` returns `{ok, R}` where `R` is the peer's response message
    **with its `set-cookie` key removed**; on a dispatch failure, `call`
    propagates the underlying error unchanged.
11. `cast` initiates the same relay as `call` asynchronously, does not wait for
    it, discards its result and any error, and returns `{ok, <<"OK">>}`
    immediately and unconditionally.
12. `request` returns `{ok, #{ body => L }}` where `L` is the ordered two-element
    list `[ #{ device => relay@1.0 }, #{ path => call, target => body,
    body => <the request's `request` value> } ]`, and never errors.
13. End-to-end: a node whose routing table maps a path to an executor peer, with
    `relay-allow-commit-request` enabled and the `request` hook installed with
    `commit-request` set, accepts an **unsigned** inbound request, re-signs it
    with the node's key, forwards it to the executor (which requires signed
    requests), and returns the executor's response — demonstrating the
    commit-on-relay proxy path. (Cf. the synchronous `GET
    /~relay@1.0/call?method=GET&relay-path=https://example.com/` returning the
    remote response message.)

## 11. Out of scope

- The internal representation of messages, the base target, and commitments.
- **The node routing table and route selection** — which peer a route resolves
  to, route templates, strategies (e.g. nearest-node selection), and how the
  HTTP request machinery applies them when no explicit peer is given. This device
  only specifies *that* it defers to routing when no peer is supplied; the router
  has its own specification.
- The set of valid `http-client` / `relay-http-client` identifiers and the
  transport mechanics (protocol version, headers, timeouts, retries, connection
  reuse) of the outbound request — substrate configuration. Only the method, path,
  destination-selection, full-response requirement, and `set-cookie` stripping are
  constrained.
- The cryptographic details of re-signing (delegated to `message@1.0` `commit` /
  `httpsig@1.0`) and of `verify`.
- The mechanism by which the `request` key is installed as a node hook and how
  the substrate resolves the returned device-switching body list.
- Result-cache configuration of the hosting node (see §6).
- Performance and concurrency behaviour (including how many concurrent `cast`
  relays a node may run).

## Open questions

- **Absent `path`/`method` are still written to the outbound message.** Steps 3
  of `call` set the outbound `method` and `path` unconditionally — including when
  the resolved value is the "not found" sentinel (e.g. a `call` with no `path`
  anywhere and no `relay-path`). The source does not special-case this, so the
  outbound message can carry a sentinel-valued `method`/`path`; what the HTTP
  layer does with such a value (reject, treat as a default like `GET`/`/`, or
  error) is determined downstream and is not pinned here. In practice a usable
  relay supplies at least a path; a reimplementer should match this "always set,
  even if absent" behaviour but need not invent a default. Flagged because a
  stricter contract might require defaulting or erroring on a missing path/method.
- **`http-client` is read from the base target only.** The outbound client is
  taken from the base target's `http-client` key (not from `Req` or `M1` via a
  precedence list like the other fields), falling back to the node option. A
  reimplementer should not generalise this to a multi-source precedence list.
- **Asynchronous-mode failure is silent.** `cast` returns `{ok, <<"OK">>}` even
  when the underlying relay will fail (including the policy error
  `relay-commit-request-not-allowed`). There is deliberately no channel by which a
  `cast` caller learns the outcome; flagged in case observability of async relays
  is expected.
- **Informative-doc drift.** Pre-existing informative documentation refers to a
  `requires-sign` field and a `preprocess` key name. The authoritative behaviour
  is the `commit-request` / `relay-commit-request` toggle (gated by
  `relay-allow-commit-request`) and the `request` key. Reimplementers MUST follow
  the key/field names pinned in this spec, not the informative doc.
