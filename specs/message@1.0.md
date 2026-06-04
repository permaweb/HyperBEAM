# `message@1.0` — the identity / base message device

- **Device name:** `message@1.0`
- **Depends-on:** `httpsig@1.0` (default commitment & ID device), `structured@1.0` (TABM conversion). Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`message@1.0` is the **identity device**: the default device bound to every
AO-Core message that does not name another. It exposes a message's own map as
resolvable keys (reading a key returns that key's value) and implements the
small set of **reserved keys** that every message supports regardless of its
device — inspection (`keys`), mutation (`set`, `set-path`, `remove`), and the
commitment surface (`id`, `commitments`, `committers`, `committed`, `verify`).
Higher-level devices delegate to `message@1.0` for these operations; the actual
cryptography of `id`/`commit`/`verify` is delegated onward to a **commitment
device** (default `httpsig@1.0`).

It is the most fundamental device in AO-Core: a correct implementation is the
precondition for almost every other device.

## 2. Concepts & terminology

- **Message:** a map of binary, lowercase, hyphenated keys to values (binaries,
  numbers, or nested messages). The internal representation is out of scope; the
  behaviour below is defined over the message's logical key/value content.
- **Private keys (normative — exact rule):** a key is **private** iff its binary
  form **begins with the literal prefix `priv`** (a case-sensitive byte-prefix
  test). So `priv`, `priv_foo`, `priv.bar`, `priv-x`, `private`, `private.data`,
  `privacy`, and `privateer` are ALL private (they start with `priv`), while
  `Private` (capital `P`) is NOT private (the test is case-sensitive) and neither
  is `pri`. There is **no** lower-casing and **no** delimited-segment logic —
  it is a plain `priv`-prefix match. Private keys are **never** returned by `get`,
  never listed by `keys`, and never committed.
- **Commitment:** an entry attesting to (a subset of) a message's keys. A
  message MAY carry a `commitments` key: a map from **commitment ID** (a
  base64url 43-char identifier) to a **commitment message**. A commitment
  message has at least `commitment-device` (the device that produced and can
  verify it) and, for signed commitments, `committer` (the signer's address) and
  `committed` (the list of keys it covers). Unsigned commitments (content IDs)
  have a `commitment-device` but **no** `committer`.
- **Committer / signer:** the address (`committer`) named in a signed
  commitment.
- **TABM (Type-Annotated Binary Message):** the flat, all-binary normal form a
  message is converted to before IDs/commitments are computed (see the
  `structured@1.0` spec). IDs are computed over the TABM form.

## 3. Device interface

- **Dispatch shape & mechanism (normative — get this exactly right or the device
  fails to answer its own keys):** `message@1.0` is a **default-handler** device.
  Resolution dispatches a key as follows: **if the key names one of the reserved
  keys below, it is answered by that key's specific behaviour; every other key is
  answered by the default `get` handler** (which returns the key's value from the
  message). Concretely, each reserved key is a distinct operation the device MUST
  expose so the resolver dispatches to it **by name** (taking the
  `(Base, Req, Opts)` inputs); a single catch-all `get` answers the rest. An
  implementation MUST ensure the reserved keys are individually dispatchable — if
  they are folded into the catch-all (or omitted), resolving `set`/`keys`/`id`/…
  will fail to resolve (e.g. "could not resolve key") or return the wrong thing.
- **Reserved keys:** `id`, `commitments`, `committers`, `committed`, `keys`,
  `path`, `set`, `set-path`, `remove`, `verify`, `commit`. (`commit` is reached
  through the commitment surface; see §4.) `path` is the reserved key carrying
  the current path segment being resolved and is never returned as data by
  `get`.
- **Anti-recursion (normative — a naïve delegation infinite-loops):** the
  cryptographic keys `id`, `commit`, `verify`, `committed` delegate to a
  **commitment device** (default `httpsig@1.0`). An implementation MUST perform
  this delegation by invoking the **commitment device directly** (resolve the
  `commit`/`verify` key with the commitment device bound, e.g. against a message
  whose `device` is the commitment device) — it MUST NOT delegate by calling the
  generic message-level `id`/`commit`/`verify`/`committed` operations, because
  those re-enter `message@1.0` and recurse without bound. Selection, accumulation,
  invalidation, and committer-filtering (this spec's §4) are implemented **in this
  device**; only the leaf cryptography is delegated to the commitment device.

## 4. Resolved keys (normative)

### `get` (the default handler) — read a key
- **Reads:** the requested key name; the message (`Base`).
- **Behaviour:** Return the value stored under the key.
  1. If the key is **private** (§2), MUST return error `not_found`.
  2. Else if the key is present **exactly**, return its value.
  3. Else perform a **case-insensitive** lookup: lower-case the **requested** key
     only, then look it up against the message's keys **as stored** — the stored
     keys are NOT lower-cased (key normalisation coerces a key's type to binary
     but does not case-fold). Consequently a key stored **lower-case** is matched
     by a request of any case, whereas a key stored with **upper-case** letters is
     matched **only** by its exact spelling. On a match return that value.
  4. Else return error `not_found`.
- **Returns:** `{ok, Value}` or error `not_found`.
- Hyphen and underscore are NOT folded by `get` (only case is); however callers
  routinely normalise keys before lookup, so an implementation SHOULD treat keys
  case-insensitively and MAY treat `-`/`_` as equivalent at the resolver layer.

### `keys` — list public keys
- **Returns:** `{ok, List}` where `List` is every **public** (non-private) key of
  the message, **excluding** the `commitments` key. Order is unspecified.

### `set` — deep-merge new values
- **Reads:** `Base`; the request message `Req` whose non-reserved keys are the
  new values; optional `set-mode` in `Req` (`deep` default, or `explicit`).
- **Behaviour:**
  1. Determine the keys to set: every public, non-reserved key of `Req` whose
     value is not the Erlang-undefined sentinel. The reserved keys (`id`,
     `commitments`, `committers`, `keys`, `path`, `set`, `remove`, `verify`,
     `set-mode`) are NOT treated as values.
  2. A request value of the sentinel `unset` **removes** that key from the base.
  3. Merge: with `set-mode = explicit`, shallow-merge new values over the base.
     With `set-mode = deep` (default), **deep-merge** — for a key whose base and
     new values are both messages, recursively `set` the new sub-message onto the
     base sub-message; otherwise the new value replaces the base value.
  4. **Private** keys of the base are preserved across the merge.
  5. **Commitment invalidation:** if any key being set is a **committed** key
     (appears in the message's committed-key set, §`committed`) AND its new value
     differs from the committed value, the result MUST have its `commitments`
     removed (the message is no longer validly signed). If every overwritten
     committed key keeps an equal value, commitments are retained.
- **Returns:** `{ok, NewMessage}`.

### `set-path` — set the reserved `path` key
- **Reads:** the new path value from the request's **`value`** field (primary);
  if `value` is absent, fall back to the request `body`. (The `value` field is the
  canonical source — a request `#{ <<"path">> => <<"set-path">>, <<"value">> => V }`
  sets `path` to `V`.)
- **Behaviour:** Sets the `path` key to that value, which `set` cannot do because
  `path` is reserved. If the new value differs from the current `path` and `path`
  is a committed key, commitments MUST be removed. `unset` removes `path`.
- **Read-back contract:** the stored `path` is an ordinary **readable** field of
  the result — resolving the key `path` (i.e. `get`/`hb_ao:get(<<"path">>, M)`)
  MUST return the value just set. `path` is reserved only against being *written*
  as bulk data through `set`; it is NOT private and MUST NOT be stripped or hidden
  by `get`/`keys`. (The resolver consumes a *request's* `path` as the routing key;
  that is unrelated to the message's own stored `path` value, which persists.)
- **Returns:** `{ok, NewMessage}`.

### `remove` — delete keys
- **Reads:** `item` (single key) or `items` (list of keys) from `Req`.
- **Behaviour:** Equivalent to `set` with each named key mapped to `unset`.
- **Returns:** `{ok, NewMessage}`.

### `id` — content/commitment identifier
- **Reads:** `Base`; optional `committers` and `commitment-ids` selectors in
  `Req`; optional `id-device` in `Base`.
- **Behaviour:**
  1. Select the **relevant commitments** per the selector rules in §4.`committers`/`commitment-ids`
     resolution (the "commitment selection" algorithm). Default selection when
     no selector is given: the commitments produced by the default commitment
     device that have **no** `committer` (i.e. the unsigned/content commitment).
  2. **If the selected set is empty:** (re)compute the ID. Convert the message
     (without `commitments`) to TABM and ask the **ID device** to produce an
     `unsigned` commitment; the ID is that commitment's ID. The ID device is
     `id-device` if set, else the `commitment-device` shared by all commitments,
     else `httpsig@1.0`. If the resolved ID device is `message@1.0` itself, use
     `httpsig@1.0` (avoid infinite recursion).
  3. **If the selected set is non-empty:** the ID is the **accumulation** of the
     selected commitment IDs: decode each ID to its 32-byte native form and
     combine by modular addition over a 256-bit accumulator (initial value 0),
     then re-encode (base64url, 43 chars). This accumulation is **commutative
     and associative**: the combined ID is independent of commitment order, a
     single ID accumulates to itself, and the combined ID does NOT encode
     ordering.
- **Returns:** `{ok, Id}` — a 43-character base64url string.
- For a **binary** base (an ID/path string rather than a map), `id` returns the
  human-readable form of that value's hashpath.
- For a **list** base, `id` returns the id of the list's **TABM form** — convert
  the list *through the structured codec* (`to_tabm` via `structured@1.0`), which
  renders a numbered message **including its `.="list"` `ao-types` marker**, then
  take the (unsigned content) id of that. Do NOT use a bare `list_to_numbered_message`
  that omits the `.="list"` marker: the id would differ, and since the codec offloads
  a committed list to a `committed+link` whose target id **is** this list-id, a wrong
  id stores the list under one id but links it under another → cache reads hit a
  dangling link (`necessary_message_not_found`). The platform invokes `id` with a
  *list* exactly when content-addressing such a key list (e.g. a commitment's
  `committed`); this clause must be byte-exact with the codec's link-id derivation.
- **Loading lazy commitments (cache reads):** before selecting commitments, if the
  base's `commitments` field is an unresolved **link** (the base was read back from
  a store, where the codec offloaded the commitments sub-message to a link), it
  MUST be **recursively loaded to its value first** — materialising any chained
  link-to-link — and the operation then proceeds on the fully-loaded commitments.
  `id`, `verify`, and `committed` all share this rule; it is what makes a
  commit → cache-write → cache-read → (`id`/`verify`) round-trip succeed.

### `commitments` — the commitments sub-map
- Returns the message's `commitments` map (possibly empty). Used as a selector
  surface by `id`/`verify`/`committed`.

### `committers` — list signer addresses
- **Reads:** `Base` (its `commitments`).
- **Behaviour:** Return the `committer` value of every commitment that has one.
- **Returns:** `{ok, [Address]}` (empty if uncommitted). Order unspecified.

### `committed` — list committed keys
- **Reads:** `Base`; the same `committers`/`commitment-ids` selectors as `id`;
  optional `raw` flag in `Req`.
- **Behaviour:** For each selected commitment, take its `committed` key list
  (a TABM-encoded ordered list, decode to the ordered key list). Return the keys
  that appear in **every** selected commitment's list (intersection). Unless
  `raw` is true, strip any TABM link suffix (`+link`) from each key so the result
  matches the keys' ordinary device-level names.
- **Returns:** `{ok, [Key]}`.

### `verify` — check commitments
- **Reads:** the target message (from `Req` body / `target`, else `Base`); the
  `committers`/`commitment-ids` selectors.
- **Behaviour:** Select the commitment IDs to verify (same selection rules; the
  default for `verify` is **all** commitments). For each, merge the
  commitment's keys into the request and ask that commitment's
  `commitment-device` to `verify` the TABM-form base. Return `true` only if
  **every** selected commitment verifies; `false` if any fails.
- **Lazy commitments (cache round-trip):** the target may be read back from a
  store, in which case its `commitments` field is an unresolved **link**. Before
  selecting/verifying, the device MUST apply the shared lazy-load rule from §`id`:
  if `commitments` is a link, **recursively load it to its value**
  (`ensure_all_loaded` — materialising chained link-to-link), then build the TABM
  base from the fully-loaded message (private `priv`-prefixed keys excluded). With
  this, a commit → cache-write → cache-read → `verify` round-trip returns `true`,
  and `id(signed)` of the read-back message equals the pre-write 43-char value.
  (Skipping the recursive load fails with a "necessary message not found" on a
  dangling link.)
- **Returns:** `{ok, Boolean}`.

### `commit` — produce a commitment (via the commitment surface)
- **Reads:** the target message; optional `commitment-device` in `Req` (else the
  node's configured default commitment device); optional `type`
  (`signed` default, or `unsigned`).
- **Behaviour:** Convert the target to TABM, then delegate to the named
  commitment device's `commit`, passing `type`. The device adds a commitment to
  the `commitments` map. Re-encode the result from TABM back to structured form.
  The device key is NOT written into the message — it is recorded inside the
  commitment as `commitment-device`.
- **Returns:** `{ok, CommittedMessage}`.

### Commitment selection (`committers` + `commitment-ids`) — normative
The reserved selector keywords `all` and `none` arrive **as binaries**
(`<<"all">>` / `<<"none">>`) when they come over the wire or from another device
(e.g. the commitment device asking this device for `committed` with
`committers => <<"all">>`); a literal committer **address** or commitment **ID**
is also a binary. An implementation MUST treat `<<"all">>`/`<<"none">>` as the
keywords (not as addresses) and any other binary as a literal address/ID.
Given `Req` selectors, compute the set of commitment IDs to operate on:
- `commitment-ids`: `none` → none; `all` → every commitment ID; a list/single ID
  → those IDs.
- `committers`: `none` → none; `all` → the IDs of all commitments that have a
  `committer`; a list/single address → the IDs of commitments whose `committer`
  matches. If a requested committer address has **no** commitment, the operation
  MUST error (`requested-committers-not-found`).
- The result is the **union** of the two. Default when neither selector is
  given: `id`/`committed` default to the unsigned default-device commitment (the
  one with `commitment-device = httpsig@1.0` and no `committer`); `verify`
  defaults to **all** commitments.

## 5. Data formats & encodings

- IDs and committer addresses are **base64url** (43 chars for 32-byte values),
  never hex.
- The combined-ID accumulation operates on the 32-byte native decodings of the
  IDs, modular-added into a 256-bit big-endian accumulator initialised to 0.
- IDs are computed over the **TABM** form of the message with `commitments`
  removed (the content ID) or as reported by the commitment device (signed IDs).
- `committed` lists are stored TABM-encoded as a numbered/ordered map; decode to
  an ordered list of keys.

## 6. Ordering, freshness & caching

- `id` accumulation is order-independent and deterministic.
- The device performs no caching of its own; it operates purely on the supplied
  message and request.
- `keys`/`committers` return order is unspecified; callers MUST NOT depend on it.

## 7. Security & authority

- Setting or removing a value that is covered by an existing commitment, with a
  different value, MUST invalidate (drop) the commitments — a message's signature
  must never appear to cover content it does not.
- Private keys are never exposed (`get`/`keys`) and never committed.
- `verify` is failure-closed: any selected commitment that does not verify makes
  the whole result `false`. A request naming a committer with no commitment is an
  error, not a silent `false`.

## 8. Errors

- `not_found` — `get` of a missing or private key.
- `requested-committers-not-found` — `verify`/`id`/`committed` named a committer
  with no matching commitment.
- `multiple-id-devices` — the message's commitments disagree on `commitment-device`
  and no `id-device` disambiguates, so an ID device cannot be chosen.

## 9. Composition

- Every device that does not implement a reserved key itself inherits it from
  `message@1.0`: resolving `set`/`keys`/`id`/`commit`/`verify` on any message
  reaches this device's behaviour unless the message's own device overrides it.
- Default-handler devices MUST exclude the mutation/inspection keys (`keys`,
  `set`, `set-path`, `remove`) so those fall through to `message@1.0` rather than
  being captured — otherwise binding a device onto a path or `set`ing on it
  breaks.

## 10. Conformance (normative checklist)

1. Resolving a present public key returns its value; a private key (`private`,
   `private.*`, `priv_*`) returns `not_found`; `keys` omits private keys and
   `commitments`.
2. Key lookup is case-insensitive **on the request side** (RFC-9110): for a key
   stored lower-case (the §2 convention), `GET .../Key` and `.../key` return the
   same value; a key stored with upper-case letters is matched only by its exact
   spelling (§4 `get` step 3 — stored keys are not case-folded).
3. `set` of a new key adds it; `set` of an existing **uncommitted** key
   overwrites it; `set` to `unset` removes it; deep-merge recurses into nested
   message values; `explicit` mode shallow-merges.
4. `set`/`set-path`/`remove` that changes the value of a **committed** key drops
   all commitments; a `set` that rewrites a committed key with an equal value
   keeps commitments.
5. Private keys survive a `set` merge; reserved keys in the request are not set
   as data.
6. `id` of an uncommitted message equals the ID the commitment device assigns to
   its unsigned commitment over the TABM form (content-addressed; stable across
   re-encodings).
7. `id` of a message with N signed commitments equals the order-independent
   accumulation of the selected commitment IDs; permuting the commitments yields
   the identical ID; selecting a single commitment yields that commitment's ID.
8. `committers` returns exactly the `committer` addresses of commitments that
   have one; `[]` for an uncommitted message.
9. `committed` returns the intersection of selected commitments' committed-key
   lists, with `+link` suffixes stripped unless `raw` is set.
10. `verify` returns `true` iff every selected commitment verifies under its own
    `commitment-device`; tampering with any committed key makes `verify` `false`;
    naming a committer with no commitment errors.
11. The default commitment/ID device is `httpsig@1.0`; a message may override the
    ID device via `id-device`, and per-commitment via `commitment-device`; an ID
    device that resolves back to `message@1.0` falls back to `httpsig@1.0`.

## 11. Out of scope

- The internal representation of messages, commitments, and links.
- The cryptographic details of any specific commitment device (see
  `httpsig@1.0`).
- The exact TABM byte layout (see `structured@1.0`).
- Performance and storage strategy.
