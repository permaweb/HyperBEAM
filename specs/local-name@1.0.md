# `local-name@1.0` — node-local name registry & resolver

- **Device name:** `local-name@1.0`
- **Depends-on:** `message@1.0` (the base identity device whose `get`/`keys`/`set` behaviour the looked-up names message and the excluded keys fall through to; commitment/`signers` semantics for authority), `name@1.0` (the multi-resolver host this device plugs into as a resolver), and `meta@1.0` (the node-meta device whose `is-operator` key this device delegates the register authority check to — §7). All three specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`local-name@1.0` is a **node-local name registry**: it lets the node operator
bind short, human-chosen names to values (typically pointers to
content-addressed messages) and resolve those names back to their values. Names
are persisted in the node's durable store under a device-namespaced path and
mirrored into a fast in-memory index carried on the node options, so resolution
does not have to hit durable storage on every request.

It is the node's **own private naming authority**, complementing global naming
schemes (e.g. ArNS): only the operator may register a name, and the names are
meaningful only on this node. Its primary role is to act as one **resolver**
behind `name@1.0` — when a node lists this device among its name resolvers, a
bare name lookup against `name@1.0` consults this registry. It also answers
`register`/`lookup` directly when addressed as `~local-name@1.0`.

## 2. Concepts & terminology

- **Name:** a caller-chosen binary key (conventionally lowercase, hyphenated) under
  which a value is registered — e.g. `my-app`, `home`. A name is normalised (§5.1)
  before it is stored or looked up — note normalisation coerces type to binary but
  does **not** case-fold (§5.1), so an upper-cased name keeps its case.
- **Value:** the binary (or message) bound to a name. In normal use the value is a
  **pointer**: the identifier (base64url) of, or a link to, a content-addressed
  message in the node's store, so that resolving the name and then loading yields
  that message. The device itself imposes no constraint on the value's content; it
  stores and returns whatever was registered.
- **Registry namespace:** the durable-store path prefix under which this device
  keeps its names. It is the exact device name, the binary `local-name@1.0`. Each
  registered name occupies the path `local-name@1.0/<normalised-name>` (§5.2).
- **Names message:** a single message whose keys are the registered (original,
  pre-normalisation) names and whose values are the corresponding stored values.
  Resolving a name is resolving that key against this message. It is the
  observable form of the registry; its construction from durable storage is §5.3.
- **Names index (node option):** a node-level option, addressed as the binary,
  lowercase, hyphenated key `local-names`, holding the current names message. It
  is a cache of the durable registry consulted first on lookup so that durable
  storage need not be read per request (§6). It is node state, not per-message
  state.
- **Operator:** the node's controlling identity — the address configured as the
  node operator, or, if none is configured, the address of the node's own signing
  key; a node with neither is **unclaimed**. Operator authority is evaluated by
  delegating to the node-meta device's `is-operator` contract (§7).

## 3. Device interface

### 3.1 Dispatch shape

**Default-handler.** The device answers two **named** keys, `lookup` and
`register` (§4), and installs a **default handler** that treats *any other* key
as a name to look up: resolving key `K` (other than `lookup`/`register`) returns
the value registered under name `K`, exactly as if `lookup` had been called with
`key = K` (§4, `lookup`). This is what lets the device serve as a `name@1.0`
resolver — a resolver is asked for an arbitrary key and must return its value.

The default handler MUST be installed such that the following keys are
**excluded** from it and fall through to the base identity device
(`message@1.0`): `keys` and `set`. These are the message-inspection/mutation keys
needed to operate on the bound message itself; capturing them as "names" would
break binding the device onto a path and `set`-ing on it. (The exclude set is
**exactly** `keys` and `set` — no more. Other reserved keys such as
`id`/`commit`/`verify`/`set-path`/`remove` are **not** excluded: the default
handler captures each as a name like any other key, returning the registered
value or `not_found` — it does **not** forward them to the base device. This
matches the sibling `name@1.0` device's minimal exclude set. A name that
collides with a `message@1.0` operation key is a pathological, **unpinned**
corner — resolving such a name against the names message may invoke that key's
`message@1.0` semantics rather than a clean value/`not_found`; operators SHOULD
NOT register names that collide with reserved keys.)

### 3.2 Message shapes

All keys are lowercase, hyphenated, binary on the wire.

- **`lookup`** operates on the **request** message `Req`:
  - `key` (binary, required when `lookup` is addressed by name) — the name to
    resolve. When the device is reached through its default handler, the resolved
    key (the path segment) is the name, and `key` need not be supplied by the
    caller.
  - `load` (boolean-ish, optional) — a pass-through directive consumed by the
    enclosing `name@1.0` resolver, controlling whether a resolved pointer is
    dereferenced from durable storage (see `name@1.0`; default there is "load").
    This device does not itself act on `load`; it returns the stored value, and
    `name@1.0` decides whether to load it. (Reimplementers MUST NOT strip or
    require `load`; it is transparent here.)
  The base message is not read by `lookup`.

- **`register`** operates on the **request** message `Req`:
  - `key` (binary, required) — the name to register. Normalised before storage
    (§5.1).
  - `value` (required) — the value to bind to the name (typically a pointer).
  - The request MUST be **committed by the operator** for the registration to be
    authorised (§7). The operator check inspects the request's signers / the
    request carried in `body`.
  The base message is not read by `register`.

## 4. Resolved keys (normative)

`Base` is the message bound to `local-name@1.0`; `Req` is the per-step request.

### `lookup` — resolve a name to its value

- **Reads:** `key` from `Req` (the name); the node's **names message** (from the
  `local-names` node option, else loaded from durable storage — §5.3). Does not
  read `Base`.
- **Behaviour:**
  1. Obtain the current names message: read the `local-names` node option; if it
     is absent, **load** the registry from durable storage into a names message
     and use that (§5.3). (Loading also populates `local-names` for subsequent
     lookups — §6.)
  2. Resolve `key` **against the names message** using the base identity device's
     `get` behaviour (`message@1.0` `get`): return the value stored under that
     name. Per `message@1.0`, this lookup is exact-match first, then
     case-insensitive on the **lookup key only** (so it reaches an upper-cased
     stored name only by exact spelling — see §5.1); a present name returns its
     value, an absent name yields `not_found`.
- **Returns:** `{ok, Value}` for a registered name; `{error, not_found}` for an
  unregistered name (the `not_found` arises from the base identity device's `get`
  of a missing key). The value is returned **as stored** (no dereferencing — that
  is `name@1.0`'s `load` concern).
- **Side effects:** MAY, as a consequence of step 1 on a cold index, **read the
  durable registry and populate the `local-names` node option** (a node-state
  update; see §6). No content is written. No commitment.

### default handler — name-as-key lookup

- **Reads / Behaviour / Returns:** identical to `lookup` with `key` bound to the
  requested key (the path segment). Resolving `~local-name@1.0/<name>` returns the
  value registered under `<name>`, or `{error, not_found}`. The keys `keys` and
  `set` are excluded (§3.1) and are NOT treated as names.
- This is the surface `name@1.0` drives when this device is a resolver: asked for
  key `K`, the device returns `K`'s registered value (or `not_found`, which
  `name@1.0` treats as "this resolver did not match").

### `register` — bind a name to a value (operator only)

- **Reads:** `key` and `value` from `Req`; the request's commitment/signers for
  the authority check; the configured operator identity (node option / node key).
  Does not read `Base`.
- **Behaviour:**
  1. **Authorise.** Determine whether the request is from the **operator** by
     delegating to the node-meta `is-operator` contract (§7), passing the request
     so its signers can be checked. If the node is **unclaimed** (no operator and
     no node key), the check passes for any caller.
     - If **not** the operator → return the authorisation error (§8): an error
       whose `status` is `403` and whose `message` is the binary `Unauthorized.`.
       **No** name is registered.
  2. **Register** (operator confirmed): perform the unconditional registration
     (§5.2):
     a. Write `value` into the node's content store, obtaining its storage path /
        identifier.
     b. **Link** that stored value at the registry path
        `local-name@1.0/<normalised-key>` (§5.1–§5.2), so a read of that path
        resolves to the value.
     c. **Reload** the registry into the names index: rebuild the names message
        from durable storage and update the `local-names` node option (and the
        running HTTP-server options, if a server is running) so subsequent
        lookups see the new name (§5.3, §6).
  3. On a **storage-write failure** in step 2a, registration fails (§8): the
     result is `not_found` (no name is registered, no link made).
- **Returns:** `{ok, <<"Registered.">>}` on success; the `403`/`Unauthorized.`
  error when the caller is not the operator; `not_found` when the durable write
  failed.
- **Side effects (on authorised success):** a **content-store write** of the
  value, a **link** at `local-name@1.0/<normalised-key>`, and a **node-options
  mutation** updating `local-names` (and the HTTP-server options if applicable).
  No commitment is produced by this device.

## 5. Data formats & encodings

### 5.1 Name normalisation

A name is **normalised** before it is used as a registry path segment (on
register) and when the registry is enumerated (on load). Normalisation is the
AO-Core key coercion `hb_ao:normalize_key/1`: it coerces a non-binary key (atom,
integer, char-list) to its binary form, but **a binary key is returned
unchanged** — in particular it does **NOT** lower-case or otherwise case-fold the
name. So a name supplied as the binary `My-App` normalises to `My-App` and is
stored at the path `local-name@1.0/My-App` (its case preserved); the names
message (§5.3) keys it under that same enumerated form. Reimplementers MUST use
this exact coercion on both the write side (path segment) and the
read/enumeration side, so the storage segment and the names-message key agree
byte-for-byte.

**Case sensitivity (consequence, normative).** Because normalisation does not
lower-case, the registry key keeps the registrant's case. Lookups resolve against
the names message with `message@1.0` `get` semantics (§4), which match the
requested name **exactly first, then case-insensitively by lower-casing the
*lookup* key** and comparing it against the **raw** (un-lowercased) stored keys.
Therefore a name stored **lower-case** (e.g. `my-app`) is found by any case
(`my-app`, `My-App`, `MY-APP`); but a name stored with **upper-case** letters
(e.g. `My-App`) is found **only by its exact spelling** `My-App` — `my-app` does
**not** find it. An operator who wants case-insensitive reach MUST register the
name in lower case.

### 5.2 Registry path

Each registered name is stored at the durable-store path formed by joining the
**registry namespace** and the normalised name with a single `/`:

```
local-name@1.0/<normalised-name>
```

`local-name@1.0` is the literal device-name binary (NOT a content ID, NOT
`~`-prefixed). The path is a **link** to the stored value: reading the path
follows the link and returns the value's content. Registration writes the value
to the content store first, then creates this link to it. (Storing the value
content-addressed and linking the human path to it is what makes the registry a
name→value index rather than a copy.)

### 5.3 Building the names message (load)

To build the names message from durable storage:
1. **Enumerate** the registry namespace: list the entries directly under the path
   `local-name@1.0`. Each entry corresponds to one registered name.
2. For each listed entry name `N`: compute its normalised form (§5.1), read the
   value at `local-name@1.0/<normalised-N>`. If the read **succeeds**, the names
   message gets `N => <value>`; if it **fails**, the names message gets
   `N => not_found` (the entry is recorded with a not-found sentinel rather than
   omitted).
3. The resulting message (keys = listed entry names, values = read values or the
   `not_found` sentinel) is the **names message**.

The keys of the names message are the names **as enumerated from storage**;
lookups resolve against this message via the base identity device's `get`
(exact-then-case-insensitive). The internal representation of the names message
is out of scope — only that it maps each registered name to its stored value (or
`not_found`).

### 5.4 Values and IDs

- Names, the `local-names` option key, the registry namespace and path segments
  are **plain lowercase hyphenated binaries**.
- Any identifier the value contains (e.g. a content ID pointer) is **base64url**,
  never hex — but the encoding of the value is the registrant's concern; this
  device stores and returns the value verbatim.
- This device produces no commitments, hashpaths, or IDs of its own.

## 6. Ordering, freshness & caching

- **Names index as cache.** `lookup` consults the `local-names` node option
  first; only when it is **absent** does it load from durable storage (and
  populate the option). A successful `register` proactively reloads the index, so
  a name is visible to subsequent lookups immediately after it is registered.
  The index is **node state mirroring durable storage**; it is not a `name@1.0`
  result cache.
- **Authority for fresh reads.** `lookup` reads the `local-names` option in a
  manner that does NOT fall back to node-wide config defaults for that key (it is
  a node-local runtime value); an absent option means "index not yet built", which
  triggers a load, not a config default.
- **Determinism / ordering.** A `lookup` of a fixed name against a fixed registry
  is deterministic. The order in which names are enumerated when building the
  names message is **unspecified**; callers MUST NOT depend on enumeration order.
  Name resolution does not depend on enumeration order (it is a keyed `get`).
- **Mutable at a constant path.** A given name's value can change (re-register)
  and the set of names grows over time, so the registry — and any `~local-name@1.0/<name>`
  read — is **mutable at a constant path**. A node that caches HTTP resolution
  results MUST be configured so these reads are not served stale from a result
  cache (a node-configuration concern, not device behaviour).

## 7. Security & authority

- **Register is operator-gated, failure-closed.** Only the **operator** may
  register a name. The device determines operator status by delegating to the
  node-meta `is-operator` contract: the request's **signers** (committers) are
  compared against the node's operator address (the configured operator, else the
  node's own signing-key address). A request not signed by the operator is
  **denied** with the `403`/`Unauthorized.` error and changes nothing. Concretely,
  the check resolves the `is-operator` key on a `meta@1.0`-bound message, passing
  the committed register request under `body` —
  `hb_ao:resolve(#{<<"device">> => <<"meta@1.0">>}, #{<<"path">> =>
  <<"is-operator">>, <<"body">> => Req}, Opts#{<<"hashpath">> => ignore})` — and
  branches on the contract's return: `{ok, true}` (operator, **or** unclaimed
  node) → register; `{ok, false}` → deny. The signer extraction from `body` is
  `meta@1.0`'s concern (see its spec); this device only reads the boolean.
- **Unclaimed node.** If the node has **no** configured operator **and no** signing
  key (it is *unclaimed*), the operator check passes for **any** caller, so an
  unclaimed node permits registration by anyone. (This mirrors the node-meta
  `is-operator` semantics; on a claimed node, registration requires the operator's
  signature.) Reimplementers MUST NOT add an independent authority model — the
  gate is exactly "is the request from the operator (or is the node unclaimed)".
- **Lookup is unauthenticated.** Resolving a name (`lookup` / default / via
  `name@1.0`) requires **no** commitment and is answerable for any caller. It
  discloses only values the operator chose to register. An unregistered name
  returns `not_found`, not an error that leaks registry contents.
- **No signature is produced.** Registration writes to the node's store and
  mutates node state but does not sign anything; the stored value's own
  commitments (if any) are the registrant's.

## 8. Errors

- `not_found` — returned by `lookup`/the default handler when the requested name
  is not registered. This is the base identity device's `get` result for a missing
  key, which through `hb_ao:resolve` is observed as **`{error, not_found}`** (§4
  Returns). Separately, `register` returns the **bare atom `not_found`** when the
  **durable write of the value failed** (registration could not be persisted) — a
  different surface (a bare atom, not the `{error, _}` envelope) distinguished by
  the operation (register vs lookup), not by the atom itself.
- The **authorisation error** — returned by `register` when the caller is **not**
  the operator: a message-shaped error carrying `status = 403` and `message =`
  the binary `Unauthorized.`. (This is a structured error value, not a hyphenated
  atom; the exact `status` integer `403` and the exact message binary
  `Unauthorized.` are normative.)
- This device defines **no** error for an unauthenticated `lookup` (lookup never
  requires authority).
- Errors arising inside the delegated operator check or the storage layer (other
  than the write-failure mapped to `not_found` above) propagate as encountered;
  this device does not re-wrap them.

## 9. Composition

- **As a `name@1.0` resolver (primary).** A node lists this device among its name
  resolvers (the `name@1.0` resolver list). When `name@1.0` resolves a bare name,
  it asks each resolver in turn for that key; this device answers via its default
  handler, returning the registered value or `not_found` (a non-match, so
  `name@1.0` proceeds to the next resolver). Whether the returned pointer is then
  **loaded** from storage is governed by `name@1.0`'s `load` directive, not by
  this device. Thus `GET /~name@1.0/<name>` (with the node configured) yields the
  value this device registered for `<name>`, optionally dereferenced.
- **Direct addressing.** Addressed as `~local-name@1.0`, the device exposes
  `register` and `lookup` directly: `POST /~local-name@1.0/register` (operator-
  signed, with `key`+`value`) and `GET /~local-name@1.0/lookup?key=<name>` (or
  `GET /~local-name@1.0/<name>` via the default handler).
- **Excluded keys delegate.** Because `keys` and `set` are excluded from the
  default handler, binding `~local-name@1.0` onto a message and resolving **those
  two keys** reaches the base identity device (`message@1.0`) for that message,
  not the registry. The other reserved keys (`id`/`commit`/`verify`/`set-path`/
  `remove`) are **NOT** excluded — the default handler captures them as names and
  returns the registered value or `not_found`, not the base device's operation
  (§3.1).
- **Operator tooling.** Other node-internal components MAY perform an
  **unconditional** registration (the §5.2 register without the operator check) to
  seed names programmatically; this bypass is not exposed as a resolvable key and
  is not part of the public request surface. The public `register` key always
  enforces the operator gate.

## 10. Conformance (normative checklist)

An implementation MUST exhibit the following externally observable behaviours:

1. The device answers the named keys `lookup` and `register`, installs a
   **default handler** that resolves any other key as a name lookup, and
   **excludes** `keys` and `set` from that handler so they fall through to the
   base identity device.
2. `lookup` with `key = N` returns `{ok, V}` when `N` is a registered name with
   value `V`, and `{error, not_found}` when `N` is not registered. The same holds
   for the default handler reached as `~local-name@1.0/N`.
3. Name lookup against the names message follows base-identity-device `get`
   semantics: an exact match first, then a case-insensitive match that lower-cases
   the **lookup** key only (§5.1). Because the device does NOT lower-case on
   register, a name stored **lower-case** (`my-app`) is found by `my-app`,
   `My-App`, and `MY-APP`; a name stored with upper-case (`My-App`) is found
   **only** by the exact `My-App`.
4. `register`, when the request is **committed by the operator**, writes the
   `value` to the content store, links it at the durable path
   `local-name@1.0/<normalised-key>`, refreshes the in-node names index, and
   returns `{ok, <<"Registered.">>}`. Immediately afterwards, a `lookup` of that
   name returns the registered value.
5. `register` from a **non-operator** caller (on a claimed node) registers nothing
   and returns the structured error with `status = 403` and `message =
   Unauthorized.`.
6. On an **unclaimed** node (no operator and no node key), `register` succeeds for
   any caller (the operator gate passes).
7. `register` returns the bare atom `not_found` if the durable write of the value
   fails, and in that case stores no name and creates no link.
8. The registry namespace is the literal binary `local-name@1.0`; each name is
   stored at `local-name@1.0/<normalised-name>` as a link to the value;
   reads of that path return the value's content. The name is normalised
   identically on write and on enumeration.
9. The names index is held on the node option `local-names`; `lookup` consults it
   first and builds it from durable storage only when absent; a successful
   `register` updates it. Resolution does not depend on name-enumeration order.
10. `lookup` requires **no** authentication; `register` is the only authority-
    gated key; an unregistered-name lookup yields `not_found` rather than an
    error exposing the registry.
11. The device produces no commitments/IDs of its own; all names, the namespace,
    and the `local-names` option key are lowercase hyphenated binaries; any
    identifier inside a value is base64url, never hex; the exact `403` /
    `Unauthorized.` error and the exact `Registered.` success binary are used.
12. When listed as a `name@1.0` resolver, resolving a name through `name@1.0`
    returns the value this device registered for that name (a `not_found` from
    this device is a non-match that lets `name@1.0` try the next resolver);
    dereferencing of the returned pointer is `name@1.0`'s `load` concern, not this
    device's.

## 11. Out of scope

- The internal representation of the names message, the names index, links, and
  stored values (only the observable registry path `local-name@1.0/<normalised-name>`,
  the `local-names` option key, and the keyed-`get` resolution semantics are
  normative).
- The cryptographic details of the operator/signer check (see `message@1.0`
  commitment/`signers` semantics and the node-meta `is-operator` contract) — only
  that registration succeeds iff the request is the operator's (or the node is
  unclaimed).
- The mechanics of `name@1.0` resolver iteration, matching, and the `load`
  dereferencing of a resolved pointer (see `name@1.0`) — this spec constrains only
  what value this device returns for a given name.
- The durable store backend, the HTTP-server option propagation mechanism, and
  the result-cache configuration a node must apply for these mutable-at-constant-path
  reads (§6).
- Performance, concurrency (e.g. simultaneous register + load), and storage
  strategy.
- The byte layout / semantics of a registered value beyond "returned verbatim,
  optionally a pointer the enclosing `name@1.0` loads".

## Open questions

- **`load` ownership.** The `load` directive that appears on lookup requests is
  defined and consumed by `name@1.0` (it controls dereferencing of a resolved
  pointer), not by this device, which returns the stored value verbatim. The spec
  pins `load` as transparent here; the precise dereferencing rules live in the
  `name@1.0` spec. Flagged because a reimplementer seeing `load` on a `lookup`
  request must NOT act on it at this layer.
- **Write-failure → `not_found`.** A failed durable write of the value during
  `register` surfaces as the bare atom `not_found` rather than a descriptive
  error. This is observably the same atom that an unregistered `lookup` returns;
  the two are distinguished by the operation (register vs lookup), not by the
  value. Flagged as a slightly surprising error shape an implementer must
  reproduce for parity.
- **Operator-check source of signers.** The authority check evaluates the request
  in `body` (the committed request envelope) for its signers, consistent with the
  node-meta `is-operator` contract; the exact unwrapping (whole request vs its
  `body`) is that contract's concern. An implementer MUST defer to the node-meta
  `is-operator` semantics rather than re-deriving signer extraction here.
