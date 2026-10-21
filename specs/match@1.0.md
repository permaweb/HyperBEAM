# `match@1.0` — a reverse index for finding message IDs by key/value

- **Device name:** `match@1.0`
- **Depends-on:** `message@1.0` (identity/base device, content-ID derivation), `structured@1.0` (TABM conversion of values and of the base message). Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`match@1.0` maintains a **reverse index** that maps a `(key, value)` pair to
the set of message identifiers whose content contains that pair, and answers
"which stored messages have this key set to this value?" queries against that
index. Building the index turns a content scan over the whole message store
into a small number of constant-cost lookups, so a caller searching for
messages matching a template pays roughly `O(keys × log(store-size))` instead
of scanning every stored message.

The device is the index half of a search subsystem: a higher-level discovery
device (see `query@1.0`) constructs a match template and calls this device's
`all` key to resolve it. The index is *populated as a side effect of writing
messages to the cache* — the platform's cache-write path invokes this device's
index-writing surface for every message it stores, when a match index is
configured. This spec defines that side-effect contract and the read contract;
it does not define how messages get into the cache in the first place.

## 2. Concepts & terminology

- **Match index:** a store-backed set of records, one per `(key, value, id)`
  triple, recording that the message identified by `id` carries `key = value`.
  The index lives in a node-configured **index store**, which MAY be the same
  store the messages themselves live in, or a separate store.
- **Index address:** the store path under which a `(key, value)` triple is
  recorded. Addresses are namespaced under the device's own prefix so they
  never collide with message content. The exact byte layout is normative and is
  given in §5.
- **Value-path:** the **content-addressed** representation of a value, used as
  the value component of an index address. Two values that are equal in their
  on-the-wire (binary) form MUST produce the same value-path, so a query value
  and an indexed value match iff they are byte-equal after normalisation. The
  derivation is normative and is given in §5.
- **Index group:** the store path `prefix&key=value-path` (no trailing id). It
  is a **group** (a directory / composite node) whose member names are the ids
  recorded for that pair. Listing the group yields those ids.
- **Index record:** the store path `prefix&key=value-path/id`. Its stored value
  is the **empty binary** (`<<>>`); the record's existence — not its content —
  is the indexed fact.
- **Message id:** a content or commitment identifier of a message, **base64url**
  (never hex). The ids written for a message are exactly the id set the cache
  layer computes for it (its uncommitted content id plus every commitment id);
  this device records each of them as a member of the relevant groups, so a
  match resolves whether the caller knows the message by its content id or by
  any of its commitment ids.
- **Match template / match spec:** a message whose public, non-private keys are
  the `(key, value)` constraints to satisfy. `all` answers a template; the
  default handler answers one key of a base message.

The device's internal representation, and the internal representation of any
message it indexes, are **out of scope** (§11). Only the observable store
addresses, the values written, and the resolved results are constrained.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device answers the explicit key
  `all`, and a default handler answers **any other key** as "match this single
  key of the base message". The following keys are **excluded** from the default
  handler and therefore fall through to the base `message@1.0` device rather
  than being treated as index lookups: `set`, `remove`, `id`, `verify`,
  `write`. An implementation MUST exclude exactly these so that (a) message
  mutation (`set`/`remove`), identity (`id`) and commitment checking (`verify`)
  keep their ordinary `message@1.0` meaning on an indexed base, and (b) `write`
  is not reachable as an index lookup (it is the index-writing surface, §4).
- **Index-writing surface:** the device also exposes an index-writing operation
  (here named **write**, §4) that is **not** a resolved key reachable by path —
  it is excluded from the default handler and is invoked directly by the
  platform's cache-write path. A reimplementation MUST provide this operation
  and MUST NOT expose it as a path-addressable key.
- **Message shape(s):**
  - For `all`: the **base** message is the match template. Every public,
    non-private key of the template is a constraint. There are no required keys;
    an empty template is valid (see `all`). The request message carries only the
    invocation path and is otherwise ignored.
  - For the default handler: the **base** message must contain the key being
    resolved; that key's value is the constraint. The request message is
    ignored.
  - For the index-writing surface: it receives a **list of ids**, a **base
    message** (the message being indexed), and node options.
- **Key names** are matched after type-coercion to binary (see §5); they are
  **case-sensitive** — keys are recorded and looked up by their exact bytes, not
  case-folded. `private` keys (per `message@1.0`) and a message's commitments
  are never indexed (§4, §5).

## 4. Resolved keys (normative)

### `all` — intersect matches for a whole template

- **Signature:** (`Base` = match template, `Req` ignored) → `{ok, [id]}` |
  `{error, not_found}`.
- **Reads:** every public, non-private key of `Base` and its value. MUST first
  reduce `Base` to its indexable form: drop the message's commitments (index the
  **uncommitted** content) and drop all private keys. The remaining keys are the
  constraints. **The reduction drops ONLY commitments and private keys — it does
  NOT strip routing/binding keys such as `device` or `path`** (concretely
  `hb_message:uncommitted(hb_private:reset(Base))`, nothing more). Consequently a
  template that still carries its own `device => match@1.0` binding would make
  `device` a spurious constraint that no message indexed, so `all` would return
  `{error, not_found}`. **Callers therefore MUST invoke `all` with a CLEAN
  template** whose routing/control keys are already removed — that stripping is
  the caller's responsibility (e.g. `query@1.0` strips `path`/`commitments`/its
  control keys, §9; the platform invokes `all` with the bare template, not via a
  `device`-bound path resolve).
- **Behaviour:**
  1. If the reduced template has **no keys**, return `{ok, []}` (the empty
     template matches nothing — it returns an empty list, NOT an error).
  2. Otherwise, for each constraint key, compute the set of ids recorded for
     that `(key, value)` pair (the same lookup the default handler performs,
     below).
  3. Return the **intersection** of all per-key id sets: an id is in the result
     iff it appears in the recorded set for **every** constraint.
- **Ordering / tie-break:** the result list is ordered by the **first
  constraint key's** recorded id list, filtered to those ids also present for
  every other key. Concretely: take the first key's id list in store order, then
  keep only ids that also matched every subsequent key, preserving the first
  key's order. Duplicates are not introduced. The iteration order over the
  template's keys is unspecified (any consistent traversal is acceptable), but
  the *result* order is pinned to the first-traversed key's list. Callers SHOULD
  NOT depend on the cross-key ordering beyond this rule.
- **Returns:** `{ok, [id]}` where each id is a base64url identifier. May be an
  empty list (empty template, or no message satisfies all constraints — see
  Errors for the distinction).
- **Errors:** `not-found` if the lookup for **any** constraint key fails to
  resolve a list (i.e. that pair has no group at all in the index). Note: a key
  whose group exists but is empty is not itself an error at the lookup layer; an
  empty intersection is returned as `{ok, []}` only via the empty-template path
  — a non-empty template all of whose groups exist but share no id yields
  `{ok, []}`, while a template naming a pair with no group yields
  `{error, not_found}`. (See §8.)
- **Side effects:** none. `all` is read-only.

### default handler — match one key of the base

- **Signature:** (`Key`, `Base`, `Req` ignored) → `{ok, [id]}` |
  `{error, not_found}`, where `Key` is the resolved key name.
- **Reads:** the value of `Key` in `Base`. (The base is taken as given for the
  single-key handler; the caller is expected to have provided the constraint
  value under that key.)
- **Behaviour:**
  1. Read the value `V` stored under `Key` in `Base`.
  2. Compute the value-path of `V` (§5) and the index address
     `prefix&normalised-key=value-path` (§5).
  3. **List** the members of that group in the index store. Each member name is
     a message id.
- **Returns:** `{ok, [id]}` — the recorded ids (possibly empty if the group
  exists but has no members). `{error, not_found}` if the group cannot be listed
  (no such group in the store).
- **Side effects:** none. Read-only.

### write — populate the index for a message (index-writing surface)

> Not a path-addressable key. Invoked by the cache-write path for each stored
> message. Described here because its observable store writes are part of the
> device's contract.

- **Signature:** (`IDs` = list of message ids, `Base` = message being indexed,
  node options) → `ok` | `{skip, reason}`.
- **Reads:** the configured index store (§6); `IDs`; the indexable form of
  `Base`.
- **Behaviour:**
  1. Resolve the index store (§6). If no store is configured (the resolved store
     is empty), return `{skip, <<"No store configured for match index.">>}` and
     write nothing.
  2. Reduce `Base` to its indexable form: drop commitments (index the
     **uncommitted** message) and drop all private keys. The remaining map is
     the set of `(key, value)` pairs to index.
  3. For **each** `(rawKey, value)` pair of the reduced message, in any order:
     a. Compute the **normalised key** (type-coerce `rawKey` to its binary form;
        do NOT case-fold) and the **value-path** of `value` (§5).
     b. Ensure the **group** `prefix&key=value-path` exists in the store
        (create it as a group/composite if absent).
     c. For **each** id in `IDs`, write an **index record** at
        `prefix&key=value-path/id` whose stored value is the empty binary
        `<<>>`.
- **Returns:** `ok` on success; `{skip, reason}` when no store is configured.
- **Side effects:** creates one group per distinct indexed `(key, value)` pair
  and one empty-valued record per `(key, value, id)` triple, in the index store.
  No commitments are produced; no network calls. Writes are **idempotent**:
  re-indexing the same message with the same ids re-creates the same groups and
  re-writes the same empty records (no duplication of group members).

## 5. Data formats & encodings

All index addresses and value-paths are byte-exact; a wrong byte yields a
different address and a silent mismatch. Implementations MUST reproduce the
following exactly.

### 5.1 Index address layout

Let `PREFIX` be the device's reserved namespace literal:

```
~match@1.0
```

(the device name with a leading `~`). The two address forms are:

- **Group:** `PREFIX` `&` `KEY` `=` `VALUE-PATH`
- **Record:** `PREFIX` `&` `KEY` `=` `VALUE-PATH` `/` `ID`

i.e. the literal byte sequence `~match@1.0&<key>=<value-path>` for a group, and
that same string followed by `/<id>` for a record. The separators `&`, `=`, `/`
are literal single bytes. `KEY`, `VALUE-PATH`, and `ID` are inserted verbatim
(see normalisation below). The `/` makes records the members of the group, so
listing the group returns the ids.

### 5.2 Key normalisation (type-coercion, NOT case-folding)

A key written to or queried in the index is first coerced to its binary form:

- a binary is used **unchanged** (including its exact case — keys are
  **case-sensitive**);
- an atom is coerced to its textual binary form;
- an integer is coerced to its base-10 textual binary form;

and otherwise reduced to a binary by the platform's standard key coercion. The
device MUST NOT lower-case, hyphen/underscore-fold, or otherwise alter the
bytes of a binary key. (`Key` and `key` therefore index/query **different**
buckets.)

### 5.3 Value-path derivation (content-addressing of the value)

The `VALUE-PATH` component is a content-addressed encoding of the constraint
value `V`, computed by type:

- **Binary `V`:** `VALUE-PATH = "data/" ++ H` where `H` is the **base64url**
  encoding of the SHA-256 hash of the raw bytes of `V` (the value's default
  hashpath). `H` is the 43-character base64url form of the 32-byte digest.
- **Map `V`:** `VALUE-PATH` is the **uncommitted content id** of `V` (its
  `message@1.0` id over the value as a message, with commitments discarded) — a
  base64url identifier.
- **List `V` that is a printable (string-like) list:** treat it as the binary
  obtained by flattening it to a byte string, then apply the **binary** rule
  above.
- **List `V` that is not string-like:** convert the list to its TABM form (via
  `structured@1.0`) and apply the **map** rule (take its content id).
- **Any other `V`:** coerce `V` to a binary path representation, then apply the
  **binary** rule.

Consequences an implementer MUST preserve:
- Equal-on-the-wire scalar values share a value-path, so a query value matches
  an indexed value **iff they are byte-equal** after the above normalisation.
  In particular, a constraint value is matched by its **binary** form: a query
  whose value is the binary `"42"` matches an indexed integer `42` only if the
  integer was itself indexed via the binary path — i.e. matching is over the
  binary representation, not the typed value. (This mirrors the cache's
  "match on binary representation" rule so that type-annotation keys that only
  partially match do not break a match.)
- Two distinct messages with identical uncommitted content produce the same map
  value-path and so are indexed/queried under the same value bucket.

### 5.4 Identifiers

- All ids are **base64url**, never hex. A 32-byte id is its 43-character
  base64url form.
- The id set written for a message (the `IDs` argument to the index-writing
  surface) is the cache layer's full id set for that message: its uncommitted
  content id together with every commitment id it carries. This device records
  each id independently, so any one of them resolves the same match. This device
  does not itself compute that set; it records whatever ids it is handed.

## 6. Ordering, freshness & caching

- **Index store selection (normative precedence).** The store that receives
  index writes, and that lookups read, is chosen as follows from node options:
  1. If a **local `match-index`** option is set, it selects the store, UNLESS it
     is exactly equal to the **global `match-index`** value while a **local
     `store`** is also set — in which case the local `store` is used (so that a
     caller supplying its own `store` indexes into that same store rather than
     the global index). Otherwise the local `match-index` value is used
     directly.
  2. Else, if a local `store` is set (and no distinct local `match-index`), use
     that `store` (index alongside the caller's messages).
  3. Else, fall back to the **global `match-index`** node configuration.
  The selected value is then interpreted:
  - `false` → **no index store**: the index is disabled. Writes return
    `{skip, …}` and write nothing; the read surface MUST treat the empty store
    as "no groups" (lookups do not resolve).
  - `true` → use the node's normally configured `store`.
  - a single store definition → use it (as a one-element store list).
  - a list of store definitions → use it as given.
  An absent/empty selection (`[]`) is a clean no-op for writes (skip) and yields
  no matches for reads.
- **Default configuration.** A default node configures `match-index` to its
  primary store, i.e. the index is **on by default** and lives in the primary
  message store. (A reimplementation MAY default it off, but MUST honour the
  precedence and the `false`/`true`/store interpretations above when a value is
  supplied.)
- **Determinism.** Given a fixed index store, `all` and the default handler are
  deterministic functions of the store contents and the constraints. The result
  list order is pinned by the first-key rule (§4 `all`) and, for the
  single-key handler, by the store's listing order of the group's members
  (store-defined; callers MUST NOT assume sorted order).
- **Freshness.** The index reflects only messages whose write triggered an
  index write into the selected store. There is no expiry, invalidation, or
  reconciliation: records persist until the underlying store removes them. The
  device performs no result caching of its own.
- **Mutability at constant path.** An index group's membership grows as more
  messages carrying the pair are written; the group path is constant while its
  listing changes. A node serving `all`/single-key results over HTTP and caching
  resolution results MUST disable result caching for these paths (node
  configuration), or repeated queries can observe stale membership.

## 7. Security & authority

- **No commitments, no authority checks.** This device neither produces nor
  verifies commitments. Indexing and lookup are unauthenticated operations over
  store contents; anyone who can write to the cache (and thus trigger indexing)
  can add index records, and anyone who can read the index store can query it.
  Index records carry the **empty binary** as their value, so they leak no
  content beyond the existence of the `(key, value, id)` association.
- **Failure-closed on missing store.** With no configured index store, writes
  are a no-op (`{skip, …}`) and reads resolve nothing — the device never invents
  matches. It never falls back to a full store scan itself (that fallback, if
  any, belongs to the calling discovery layer).
- **Private data is never indexed.** Private keys (per `message@1.0`) and a
  message's commitments are stripped before indexing, so they never appear as an
  index key, value, or value-path, and are never matchable.
- **Match is over binary representation.** Because values are matched by their
  normalised binary/content form (§5.3), a caller cannot use type annotations to
  smuggle a near-match: only byte-equal (or content-id-equal) values match.

## 8. Errors

- `not-found` — returned by:
  - the **default handler**, when the `(key, value)` group cannot be listed in
    the index store (no such group exists);
  - **`all`**, when the lookup for **any** constraint key returns
    `{error, not_found}` (that pair has no group), i.e. at least one constraint
    is unsatisfiable because it was never indexed.
  When the platform's higher-level match entry point wraps this device, it
  treats a `{ok, []}` (empty result) as equivalent to a miss; but at this
  device's own boundary the two outcomes are distinct: an existing-but-disjoint
  set of constraints yields `{ok, []}`, whereas a never-indexed constraint pair
  yields `{error, not_found}`.
- `{skip, <<"No store configured for match index.">>}` — returned by the
  index-writing surface when no index store is configured. This is a skip, not
  an error: the calling cache-write path continues normally.

No other error atoms are defined by this device. **The error value returned is
the Erlang `{error, not_found}` — the atom is the *underscored* `not_found`**
(the hyphenated `not-found` used as the condition's name in the prose above is
the human-readable label; the value actually returned is `{error, not_found}`).

## 9. Composition

- **Index population is a cache-write hook.** The platform's message-cache write
  path invokes this device's index-writing surface for every stored message when
  a match index is configured, passing the message's full id set and the message
  itself. A reimplementation of the cache-write path MUST perform this call (or
  an equivalent that produces the identical store records of §5) so that
  subsequently written messages are findable. The index-writing surface is the
  contract; the cache-write path is the caller.
- **Query delegation.** A discovery device (`query@1.0`) builds a match template
  — typically by converting a user-supplied template message to its TABM form
  and stripping meta keys (`path`, `commitments`, and its own control keys) —
  and resolves this device's `all` key to obtain candidate ids, then loads/shapes
  the matching messages itself. This device returns **ids only**; turning ids
  into messages, counting them, returning the first, etc., is the caller's job.
- **Default-handler hygiene.** Because the device uses a default handler, it
  MUST exclude `set`, `remove`, `id`, `verify`, and `write` (§3) so those resolve
  with their ordinary `message@1.0`/index-writing meaning rather than being
  captured as single-key index lookups. Any other key resolved on a message bound
  to this device is interpreted as "match this key against the index".
- **Store sharing.** Because the index can be directed into the same store as
  the messages (§6), this device composes with the ordinary message store: a
  caller that supplies its own `store` gets both its messages and their index in
  that store, and queries against that store see them.

## 10. Conformance (normative checklist)

An implementation MUST exhibit every behaviour below; each is checkable by
configuring an index store, indexing messages, and resolving `all` / a single
key, or by code review of the store-selection / address-layout paths.

1. **Dispatch & excludes.** The device answers `all` and a default handler for
   any other key; `set`, `remove`, `id`, `verify`, and `write` are NOT captured
   by the default handler and resolve to their `message@1.0`/index-writing
   meaning instead.
2. **Index-writing surface.** Indexing a message writes, into the configured
   index store, one group at `~match@1.0&<key>=<value-path>` per distinct public
   non-private `(key, value)` pair of the **uncommitted** message, and one record
   at `~match@1.0&<key>=<value-path>/<id>` (value = empty binary) for **every**
   id supplied. No record is written for a private key or for the message's
   commitments.
3. **No-store skip.** With no index store configured, the index-writing surface
   writes nothing and returns a skip carrying the exact text
   `No store configured for match index.`; lookups resolve nothing.
4. **Address layout (byte-exact).** Group and record paths are exactly
   `~match@1.0&<key>=<value-path>` and `…/<id>`, with literal `&`, `=`, `/`
   separators; `<key>` is the type-coerced **case-preserving** key bytes.
5. **Value-path derivation.** A binary value's value-path is
   `data/<base64url-sha256-of-bytes>`; a map value's is its uncommitted content
   id; a string-like list is treated as a binary; a non-string list is its TABM
   content id; any other value is coerced to a binary path then hashed. Equal-
   on-the-wire values share a value-path; identifiers are base64url, never hex.
6. **Single-key match.** Resolving any non-excluded key `K` on a base message
   returns `{ok, ids}` listing exactly the ids recorded at
   `~match@1.0&<K'>=<value-path(base[K])>` (where `K'` is the normalised key),
   or `{error, not_found}` if that group does not exist.
7. **Template intersection.** `all` over a multi-key template returns the
   **intersection** of the per-key id sets — an id appears iff it is recorded
   for every constraint key.
8. **Result ordering.** `all`'s result order follows the first-traversed
   constraint key's recorded id list, filtered to the intersection, without
   introducing duplicates.
9. **Empty template vs. missing pair.** `all` over a template with **no**
   indexable keys returns `{ok, []}`. `all` whose constraints all have groups
   but share no id returns `{ok, []}`. `all` naming any pair with **no** group
   returns `{error, not_found}`.
10. **Idempotent / additive index.** Re-indexing the same message with the same
    ids leaves the same groups and records (no duplicate members); indexing more
    messages under the same pair adds members to the existing group at a constant
    path.
11. **Store-selection precedence.** A local `match-index` option selects the
    index store, except that a local `store` overrides a local `match-index`
    equal to the global `match-index` value; absent a local `match-index`, a
    local `store` is used; absent both, the global `match-index` is used.
    `false` disables (skip writes / no matches), `true` uses the configured
    `store`, and a store value/list is used as the index store.
12. **Read-only queries.** Neither `all` nor the single-key handler writes to any
    store, produces commitments, or makes network calls.
13. **Privacy.** No private key or commitment of an indexed message is ever
    written as an index key, value-path, or record, and so is never matchable.

## 11. Out of scope

- The internal representation of the index, of messages, and of value-paths
  (only the observable store addresses and recorded values are constrained).
- The exact TABM byte layout used when converting list/map values or the base
  message to their content-addressed form (see `structured@1.0`).
- The cryptographic details of id/commitment computation (see `message@1.0` and
  its commitment device); this device records whatever id set it is handed.
- The behaviour of the calling discovery device (`query@1.0`) — template
  construction, result shaping (count / first / messages), and any full-store
  fallback when the index is absent.
- The behaviour of the cache-write path beyond its obligation to invoke the
  index-writing surface with the message's id set.
- Concrete store backends, their listing order, persistence, eviction, and
  performance characteristics.

## Open questions

- **`not-found` vs. empty list at the discovery boundary.** At this device's own
  boundary, `all` distinguishes `{error, not_found}` (a constraint pair that was
  never indexed) from `{ok, []}` (constraints that exist but share no id, or an
  empty template). The platform's higher-level match entry point collapses both
  a `not-found` and a zero-length `{ok, []}` into a single "no match" outcome.
  The spec pins the device-level distinction (observable when the device is
  invoked directly), and notes the collapse as caller behaviour; an implementer
  targeting only the higher-level entry point could treat the two
  interchangeably without observable difference there. This is an inherent
  property of the source, not a guess — flagged so reimplementers know the
  distinction is real but only visible at the device boundary.
