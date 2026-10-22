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
index-writing surface for every message it stores. This spec defines that
side-effect contract and the read contract; it does not define how messages get
into the cache in the first place.

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
- **Index group:** the store path `prefix/key=value-path` (no trailing id). It
  is a **group** (a directory / composite node) whose member names are the
  native keys recorded for that pair (§5.1). Listing the group yields their
  entries.
- **Index record:** the store path `prefix/key=value-path/native-key` (§5.1).
  Its stored value is the **empty binary** (`<<>>`); the record's existence —
  not its content — is the indexed fact.
- **Message id:** a signed commitment identifier of a message, **base64url**
  (never hex). The ids written for a message are exactly the signed commitment
  ids the cache layer computes for it (`signed-ids`); this device records each
  of them as a member of the relevant groups.
- **Match template / match spec:** a message whose public, non-private keys are
  the `(key, value)` constraints to satisfy. `all` answers a template; the
  default handler answers one key of a base message.

The device's internal representation, and the internal representation of any
message it indexes, are **out of scope** (§11). Only the observable store
addresses, the values written, and the resolved results are constrained.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device answers the explicit keys
  `all`, `locate`, `check`, `index`, `key`, `entry`, `entries`, `row`, `member`
  and `members`, and a default handler answers **any other key** as "match this
  single key of the base message". The following keys are **excluded** from the
  default handler and therefore fall through to the base `message@1.0` device,
  and are not treated as index lookups: `set`, `remove`, `id`, `verify`.
  An implementation MUST exclude exactly these so that message mutation
  (`set`/`remove`), identity (`id`) and commitment checking (`verify`) keep
  their ordinary `message@1.0` meaning on an indexed base.
- **Index-writing surface:** the device also exposes an index-writing key,
  **`index`** (§4), that the platform's cache-write path resolves as its
  `cache-write` hook. A reimplementation MUST provide this key and MUST answer
  a request without the private `hook-caller` value `kernel` with status 401.
- **Message shape(s):**
  - For `all`: the **base** message is the match template. Every public,
    non-private key of the template is a constraint. There are no required keys;
    an empty template is valid (see `all`). The request message may add
    `predicates` and the bounds `direction`, `limit`, `from`, `after` and `to`;
    its other keys are ignored.
  - For the default handler: the **base** message must contain the key being
    resolved; that key's value is the constraint. The request message is read
    as for `all`.
  - For the index-writing surface: the **base** is the hook handler, and the
    request carries the message being indexed, as TABM, in `body` and its
    signed commitment ids in `signed-ids`.
- **Key names** are matched after type-coercion to binary (see §5); they are
  **case-sensitive** — keys are recorded and looked up by their exact bytes
  (escaped as a path component, §5.1), not case-folded. `private` keys (per
  `message@1.0`) and a message's commitments are never indexed (§4, §5), other
  than through a computed pair such as `committer`.

## 4. Resolved keys (normative)

### `all` — intersect matches for a whole template

- **Signature:** (`Base` = match template, `Req` = predicates and bounds) →
  `{ok, [id]}` | `{error, not_found}`.
- **Reads:** every public, non-private key of `Base` and its value. MUST first
  reduce `Base` to its indexable form: drop the message's commitments (index the
  **uncommitted** content) and drop all private keys. The remaining keys are the
  constraints. **The reduction drops ONLY commitments, private keys and
  `ao-types` — it does NOT strip routing/binding keys such as `device` or
  `path`** (concretely `hb_message:uncommitted(hb_private:reset(Base))`,
  converted to TABM by `structured@1.0`, without its `ao-types`). Consequently a
  template that still carries its own `device => match@1.0` binding would make
  `device` a spurious constraint that no message indexed, so `all` would return
  `{ok, []}`. **Callers therefore MUST invoke `all` with a CLEAN
  template** whose routing/control keys are already removed — that stripping is
  the caller's responsibility (e.g. `query@1.0` strips `path`/`commitments`/its
  control keys, §9; the platform invokes `all` with the bare template, not via a
  `device`-bound path resolve).
- **Behaviour:**
  1. If the reduced template has **no keys** and the request has no
     `predicates`, return `{ok, []}` (the empty template matches nothing — it
     returns an empty list, NOT an error).
  2. Otherwise, for each constraint key, compute the set of ids recorded for
     that `(key, value)` pair (the same lookup the default handler performs,
     below).
  3. Return the **intersection** of all per-key id sets: an id is in the result
     iff it appears in the recorded set for **every** constraint.
- **Ordering / tie-break:** the result list is ordered by the offset, then the
  id, of the matching entries (§5.1): ascending, or descending with `direction`
  `desc`. Duplicates are not introduced. The iteration order over the
  template's keys is unspecified (any consistent traversal is acceptable).
  Callers SHOULD NOT depend on the cross-key ordering beyond this rule.
- **Returns:** `{ok, [id]}` where each id is a base64url identifier. May be an
  empty list (empty template, or no message satisfies all constraints — see
  Errors for the distinction).
- **Errors:** `not-found` if a pair is given and no index store of `local`
  scope is configured. A template with a pair that has no group yields
  `{ok, []}`, as does a non-empty template all of whose groups exist but share
  no id. (See §8.)
- **Side effects:** none. `all` is read-only.

### default handler — match one key of the base

- **Signature:** (`Key`, `Base`, `Req` = predicates and bounds) →
  `{ok, [id]}` | `{error, not_found}`, where `Key` is the resolved key name.
- **Reads:** the value of `Key` in `Base`. (The base is taken as given for the
  single-key handler; the caller is expected to have provided the constraint
  value under that key.)
- **Behaviour:**
  1. Read the value `V` stored under `Key` in `Base`.
  2. Compute the value-path of `V` (§5) and the index address
     `prefix/normalised-key=value-path` (§5).
  3. **List** the members of that group in the index store. Each member is an
     entry whose `id` is a message id.
- **Returns:** `{ok, [id]}` — the recorded ids (empty if the group does not
  exist or has no members). `{error, not_found}` if `Base` has no `Key`, or no
  index store of `local` scope is configured.
- **Side effects:** none. Read-only.

### `index` — populate the index for a message (index-writing surface)

> The cache-write path resolves this key as the node's `cache-write` hook, for
> each stored message. Described here because its observable store writes are
> part of the device's contract.

- **Signature:** (`Base` = the hook handler, `Req` = the hook request, node
  options) → `{ok, Req}` | `{error, Error}`.
- **Reads:** the private `hook-caller` of `Req`; the configured index store
  (§6); `signed-ids`; the indexable form of `body`, and its private `offset`
  (default `-1`).
- **Behaviour:**
  1. If the private `hook-caller` of `Req` is not `kernel`, return status 401:
     `{error, #{<<"status">> => 401, <<"body">> => <<"Unauthorized caller.">>}}`
  2. Resolve the index store (§6). If no store is configured (the resolved store
     is empty), or `signed-ids` is empty, return `{ok, Req}` and write nothing.
  3. For **each** id in `signed-ids`, reduce `body` with only that id's
     commitment to its indexable form: drop commitments (index the
     **uncommitted** message) and drop all private keys. Add the pairs that
     `match-paths` and `match-all-paths` compute; a computed name replaces the
     field of the same name.
  4. For **each** `(rawKey, value)` pair of the reduced message, in any order:
     a. Compute the **normalised key** (type-coerce `rawKey` to its binary form;
        do NOT case-fold) and the **value-path** of `value` (§5).
     b. Write an **index record** at `prefix/key=value-path/native-key`, the
        native key of the id's entry (§5.1), whose stored value is the empty
        binary `<<>>`.
- **Returns:** `{ok, Req}` on success, and when no store is configured or no
  store takes the records; the 401 error above; or the store's error.
- **Side effects:** one empty-valued record per `(key, value, id)` triple, in
  the index store. No commitments are produced. Writes are **idempotent**:
  re-indexing the same message with the same ids and offset re-writes the same
  empty records (no duplication of group members).

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

- **Group:** `PREFIX` `/` `KEY` `=` `VALUE-PATH`
- **Record:** `PREFIX` `/` `KEY` `=` `VALUE-PATH` `/` `NATIVE-KEY`

i.e. the literal byte sequence `~match@1.0/<key>=<value-path>` for a group, and
that same string followed by `/<native-key>` for a record. The separators `/`,
`=`, `/` are literal single bytes. `KEY` is escaped as a single path component
(`%` as `%25`, `/` as `%2f`, and the keys `.` and `..` as `%2e` and `%2e%2e`);
`VALUE-PATH` is inserted verbatim (see normalisation below). `NATIVE-KEY`
encodes an entry: its `offset` (20 zero-padded decimal digits, or `-1` or
`infinity`), then its 43-character base64url signed `id`, then its
`commitment-device`; an entry without an id is its offset alone. The `/` makes
records the members of the group, so listing the group returns their entries.

### 5.2 Key normalisation (type-coercion, NOT case-folding)

A key written to or queried in the index is first coerced to its binary form:

- a binary is used **unchanged** (including its exact case — keys are
  **case-sensitive**);
- an atom is coerced to its textual binary form;
- an integer is coerced to its base-10 textual binary form;

and otherwise reduced to a binary by the platform's standard key coercion. The
device MUST NOT lower-case, hyphen/underscore-fold, or otherwise alter the
bytes of a binary key, other than the path-component escaping of §5.1. (`Key`
and `key` therefore index/query **different** buckets.)

### 5.3 Value-path derivation (content-addressing of the value)

The `VALUE-PATH` component is a content-addressed encoding of the constraint
value `V`, computed by type:

- **Binary `V`:** `VALUE-PATH = H` where `H` is the **base64url**
  encoding of the SHA-256 hash of the raw bytes of `V` (the value's default
  hashpath). `H` is the 43-character base64url form of the 32-byte digest.
- **Map `V`:** `VALUE-PATH` is the **uncommitted content id** of `V` (its
  `message@1.0` id over the value as a message, with commitments discarded) — a
  base64url identifier.
- **List `V`:** convert the list to its TABM form (via `structured@1.0`) and
  apply the **map** rule.
- **Any other `V`:** encode `V` to a binary by `structured@1.0`, then apply the
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
- The id set written for a message (the `signed-ids` of the index-writing
  request) is the cache layer's signed commitment ids for that message. This
  device records each id independently, with the pairs of the message with only
  that id's commitment. This device does not itself compute that set; it
  records whatever ids it is handed.

## 6. Ordering, freshness & caching

- **Index store selection (normative precedence).** The store that receives
  index writes, and that lookups read (`all` and the default handler read only
  its stores of `local` scope), is chosen as follows from node options:
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
  - `false` → **no index store**: the index is disabled. Writes return the hook
    request unchanged and write nothing; lookups with a pair return
    `{error, not_found}`.
  - `true` → use the node's normally configured `store`.
  - a single store definition → use it (as a one-element store list).
  - a list of store definitions → use it as given.
  An absent/empty selection (`[]`) is a clean no-op for writes (the hook request
  is returned unchanged) and yields `{error, not_found}` for lookups with a
  pair.
- **Default configuration.** A default node configures `match-index` to its
  primary store, i.e. the index is **on by default** and lives in the primary
  message store. (A reimplementation MAY default it off, but MUST honour the
  precedence and the `false`/`true`/store interpretations above when a value is
  supplied.)
- **Determinism.** Given a fixed index store, `all` and the default handler are
  deterministic functions of the store contents and the constraints. The result
  list order of `all` and the single-key handler is the offset, then id, order
  of §4 `all`.
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

- **No commitments.** This device neither produces nor verifies commitments.
  `index` serves only the kernel's `cache-write` hook: a request without the
  private `hook-caller` value `kernel` gets status 401. Lookup is an
  unauthenticated operation over store contents; anyone who can write to the
  cache (and thus trigger indexing) can add index records, and anyone who can
  read the index store can query it. Index records carry the **empty binary**
  as their value, so they leak no content beyond the existence of the
  `(key, value, id)` association, its offset and its commitment device.
- **Failure-closed on missing store.** With no configured index store, writes
  are a no-op (`{ok, Req}`) and reads resolve nothing — the device never invents
  matches. It never falls back to a full store scan itself (that fallback, if
  any, belongs to the calling discovery layer).
- **Private data is never indexed.** Private keys (per `message@1.0`) and a
  message's commitments are stripped before indexing, so they never appear as an
  index key, value, or value-path, and are never matchable, other than through a
  computed pair such as `committer` (§4 `index`).
- **Match is over binary representation.** Because values are matched by their
  normalised binary/content form (§5.3), a caller cannot use type annotations to
  smuggle a near-match: only byte-equal (or content-id-equal) values match.

## 8. Errors

- `not-found` — returned by:
  - the **default handler**, when `Base` has no value for the key;
  - the **default handler** and **`all`**, when a pair is given and no index
    store of `local` scope is configured.
  When the platform's higher-level match entry point wraps this device, it
  treats a `{ok, []}` (empty result) as equivalent to a miss; but at this
  device's own boundary the two outcomes are distinct: constraints that share
  no id, or a constraint pair with no group, yield `{ok, []}`, whereas a
  missing index store yields `{error, not_found}`.
- `{error, #{<<"status">> => 401, <<"body">> => <<"Unauthorized caller.">>}}`
  — returned by `index` to a request without the private `hook-caller` value
  `kernel`. With no index store configured, `index` returns `{ok, Req}`: the
  calling cache-write path continues normally.

No other error atoms are defined by `all` and the default handler. **The error
value returned is the Erlang `{error, not_found}` — the atom is the
*underscored* `not_found`** (the hyphenated `not-found` used as the condition's
name in the prose above is the human-readable label; the value actually
returned is `{error, not_found}`).

## 9. Composition

- **Index population is a cache-write hook.** The platform's message-cache write
  path invokes this device's index-writing surface for every stored message as
  the node's `cache-write` hook, passing the message's TABM in `body` and its
  signed commitment ids in `signed-ids`. A reimplementation of the cache-write
  path MUST perform this call (or an equivalent that produces the identical
  store records of §5) so that subsequently written messages are findable. The
  index-writing surface is the contract; the cache-write path is the caller.
- **Query delegation.** A discovery device (`query@1.0`) builds a match template
  — typically by converting a user-supplied template message to its TABM form
  and stripping meta keys (`path`, `commitments`, and its own control keys) —
  and resolves this device's `all` key to obtain candidate ids, then loads/shapes
  the matching messages itself. `all` returns **ids only**; turning ids
  into messages, counting them, returning the first, etc., is the caller's job.
- **Default-handler hygiene.** Because the device uses a default handler, it
  MUST exclude `set`, `remove`, `id` and `verify` (§3) so those resolve with
  their ordinary `message@1.0` meaning and are not captured as single-key
  index lookups. Any other key that the device does not answer explicitly (§3),
  resolved on a message bound to this device, is interpreted as "match this key
  against the index".
- **Store sharing.** Because the index can be directed into the same store as
  the messages (§6), this device composes with the ordinary message store: a
  caller that supplies its own `store` gets both its messages and their index in
  that store, and queries against that store see them.

## 10. Conformance (normative checklist)

An implementation MUST exhibit every behaviour below; each is checkable by
configuring an index store, indexing messages, and resolving `all` / a single
key, or by code review of the store-selection / address-layout paths.

1. **Dispatch & excludes.** The device answers the explicit keys of §3 and a
   default handler for any other key; `set`, `remove`, `id` and `verify` are
   NOT captured by the default handler and resolve to their `message@1.0`
   meaning instead.
2. **Index-writing surface.** Indexing a message writes, into the configured
   index store, for **every** signed id supplied, one record at
   `~match@1.0/<key>=<value-path>/<native-key>` (value = empty binary) per
   public non-private `(key, value)` pair of the **uncommitted** message, and
   per pair computed with only that id's commitment. No record is written for a
   private key or for the message's `commitments`.
3. **No-store skip.** With no index store configured, the index-writing surface
   writes nothing and returns `{ok, Req}`; lookups with a pair return
   `{error, not_found}`.
4. **Address layout (byte-exact).** Group and record paths are exactly
   `~match@1.0/<key>=<value-path>` and `…/<native-key>`, with literal `/`, `=`,
   `/` separators; `<key>` is the type-coerced **case-preserving** key bytes,
   escaped as a path component (§5.1).
5. **Value-path derivation.** A binary value's value-path is
   `<base64url-sha256-of-bytes>`; a map value's is its uncommitted content id;
   a list is its TABM content id; any other value is encoded to a binary by
   `structured@1.0` then hashed. Equal-on-the-wire values share a value-path;
   identifiers are base64url, never hex.
6. **Single-key match.** Resolving any non-excluded key `K` that the device
   does not answer explicitly (§3) on a base message returns `{ok, ids}` listing
   exactly the ids recorded at `~match@1.0/<K'>=<value-path(base[K])>` (where
   `K'` is the normalised key), or `{ok, []}` if that group does not exist.
7. **Template intersection.** `all` over a multi-key template returns the
   **intersection** of the per-key id sets — an id appears iff it is recorded
   for every constraint key.
8. **Result ordering.** `all`'s result order follows the offset, then the id,
   of the entries in the intersection, without introducing duplicates.
9. **Empty template vs. missing pair.** `all` over a template with **no**
   indexable keys, and a request without `predicates`, returns `{ok, []}`.
   `all` whose constraints all have groups but share no id returns `{ok, []}`.
   `all` with any pair that has **no** group returns `{ok, []}`.
10. **Idempotent / additive index.** Re-indexing the same message with the same
    ids and offset leaves the same groups and records (no duplicate members);
    indexing more messages under the same pair adds members to the existing
    group at a constant path.
11. **Store-selection precedence.** A local `match-index` option selects the
    index store, except that a local `store` overrides a local `match-index`
    equal to the global `match-index` value; absent a local `match-index`, a
    local `store` is used; absent both, the global `match-index` is used.
    `false` disables (skip writes / no matches), `true` uses the configured
    `store`, and a store value/list is used as the index store.
12. **Read-only queries.** Neither `all` nor the single-key handler writes to any
    store, produces commitments, or makes network calls.
13. **Privacy.** No private key of an indexed message is ever written as an
    index key, value-path, or record, and so is never matchable.

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
  boundary, `all` distinguishes `{error, not_found}` (no index store) from
  `{ok, []}` (constraints that share no id or have no group, or an empty
  template). The platform's higher-level match entry point collapses both
  a `not-found` and a zero-length `{ok, []}` into a single "no match" outcome.
  The spec pins the device-level distinction (observable when the device is
  invoked directly), and notes the collapse as caller behaviour; an implementer
  targeting only the higher-level entry point could treat the two
  interchangeably without observable difference there. This is an inherent
  property of the source, not a guess — flagged so reimplementers know the
  distinction is real but only visible at the device boundary.
