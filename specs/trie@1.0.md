# `trie@1.0` — prefix-trie message device

- **Device name:** `trie@1.0`
- **Depends-on:** `message@1.0` (reserved-key surface: `commit`, `verify`, `id`, `commitments`, deep-merge `set` semantics, private-key handling). Its spec is provided to reimplementers.
- **Status:** Draft

## 1. Overview

`trie@1.0` stores an associative map of binary string keys to values inside a
single AO-Core message, organised as a **radix (PATRICIA) prefix trie**: keys
that share a leading byte-sequence are factored into nested submessages keyed by
the *remaining* suffix, so a balances table whose keys are e.g. `aa` and `ab`
becomes a nested structure `a -> { a -> ..., b -> ... }`. The device exposes
`set` (insert/update one or more keys), `get` (look one key up by descending the
trie), and `keys` (enumerate every stored key). The *observable nested message
structure* is normative and pinned here; the in-memory representation an
implementation uses is not. The device delegates all signing, ID and
verification behaviour to the `message@1.0` reserved-key surface — a tried map
is an ordinary, fully-committable AO-Core message.

Its purpose is to make large key spaces (thousands of entries, e.g. token
balances) representable and updatable as a tree of small submessages rather than
one flat message, while remaining indistinguishable from a plain message for the
purposes of `get`/`set`/`keys`/commitment.

## 2. Concepts & terminology

- **Stored key / value:** a logical entry of the associative map. A stored key
  is a non-empty binary string (an arbitrary byte sequence; lookups compare
  bytes, so non-ASCII bytes are permitted). A value is any AO-Core value
  (binary, number, or nested message) other than the sentinels reserved by the
  `set` request shape (§3).
- **Edge label:** a key of a trie node that names a child edge. It is the byte
  suffix shared by all keys reachable through that edge, relative to the path
  already consumed by ancestors. Edge labels are arbitrary binary strings of one
  or more bytes; they are NOT restricted to single characters.
- **Branch node:** a submessage (a nested map) representing an internal trie
  node. Its keys are edge labels (plus optionally the reserved `node-value`
  marker, plus the structural keys excluded in §3).
- **`node-value`:** the reserved key (`<<"node-value">>`) under which a branch
  node carries the value of the stored key that terminates *exactly at that
  node* (i.e. a stored key that is a strict prefix of other stored keys, or that
  equals the concatenated path to a node that also has children). Presence of
  `node-value` makes a branch node a **terminal**.
- **Implicit leaf node:** an optimisation. When an edge leads to a stored key
  that has no descendants, the device does NOT create a child submessage with a
  `node-value`; instead the edge label maps **directly to the value** (a
  non-map). So `{ <<"zebra">> => 0 }` stores the value `0` for key `zebra`
  directly, with no wrapping submessage. An edge label whose value is a map is a
  branch; an edge label whose value is a non-map is an implicit leaf.
- **Excluded keys:** the structural/metadata keys that are NOT edge labels and
  do not name stored entries (§3).
- **Terminal:** a node position that holds a value for some stored key — either
  an implicit leaf (the value itself) or a branch carrying `node-value`.

## 3. Device interface

- **Dispatch shape:** **default-handler.** The device explicitly implements
  `set`, `get` and `keys`. Every other key reaching the device is treated as a
  **single-key trie lookup** by the default handler — resolving key `K` against a
  tried base returns the value stored under `K` (see `get`, §4). Because the
  default handler answers arbitrary keys, an implementation MUST route the
  message-manipulation/inspection keys it does not itself define
  (`set-path`, `remove`, `id`, `commitments`, `committers`, `committed`,
  `verify`, `commit`, and `path`) through to the base `message@1.0` device rather
  than treating them as trie lookups. `keys` and `set` are defined by this device
  (below) and supersede the `message@1.0` versions. The `path` key is never a
  stored entry.
- **Structural / excluded keys.** The following keys, when present in any node
  map, are NEVER interpreted as edge labels and are skipped during edge
  enumeration (and therefore never traversed by `get`, never produced by `keys`,
  and never used to split prefixes by `set`):
  `node-value`, `device`, `commitments`, `priv`, `hashpath`.
  (`device` is the device binding; `commitments`/`priv`/`hashpath` are the
  commitment and private metadata a committed/cached message carries;
  `node-value` is the terminal-value marker.) A conforming implementation MUST
  exclude exactly this set when enumerating a node's edges.
- **Base message shape:** the device operates on a message whose `device` is
  `trie@1.0`. An empty trie is just `{ device: trie@1.0 }`. After any `set` the
  base also carries the trie structure (edge labels / `node-value` / implicit
  leaves) and a `commitments` map (§4 `set`).
- **`set` request shape:** a request message whose `path` is `set`. Every key of
  the request **other than `path`** is an `(stored-key, value)` pair to
  insert/update. There is no separate "values" envelope — the request keys *are*
  the stored keys. Multiple keys MAY be supplied in one request (bulk insert).
- **`get`/default request shape:** the key to look up is supplied either as the
  resolved key name (the default-handler path segment) or as a `key` field in the
  request. A lookup MUST be given a key; absent one it errors (§8).

## 4. Resolved keys (normative)

### `set` — insert/update one or more stored keys
- **Reads:** `Base` (the current trie); from `Req`, every key except `path`
  (each is a `(stored-key → value)` pair to apply); the node's configured signing
  wallet (for the resulting commitment).
- **Behaviour:**
  1. Take the request keys excluding `path`. The relative order in which the
     pairs are applied MUST NOT affect the resulting trie topology or stored
     values (insertion is order-independent — see §6).
  2. For each `(K, V)` apply the **insertion algorithm** (below) to the trie,
     threading the updated trie into the next pair.
  3. After all pairs are applied, produce the committed result:
     a. Strip all existing commitments from the trie and from every nested
        submessage (the structure changed, so prior signatures no longer apply),
        yielding the fully-uncommitted structure.
     b. Commit **every node bottom-up**: each branch submessage (leaves first,
        then their parents, up to the root) gets its **own** unsigned content
        commitment of type `hmac-sha256` (an `hmac-sha256` keyed-hash over that
        node, not a wallet signature), added via the `message@1.0`/commitment
        surface — concretely `hb_message:commit(Node, Opts, #{<<"type">> =>
        <<"unsigned">>})`, whose result carries a `type = hmac-sha256`,
        no-`committer` commitment. A single top-level commit is NOT sufficient: it
        converts children to links but leaves grandchildren unpersisted, so a trie
        of depth ≥ 3 fails to read back (`necessary_message_not_found`) and §7's
        "every nested submessage independently verifies" is unmet.
     c. **Cache-write every committed node** (not just the root): each node's
        committed content must be persisted so its link target resolves. (A
        bottom-up commit+`hb_cache:write` per node satisfies both b and c at once.)
  4. Return the committed trie.
- **Insertion algorithm** (defines the observable topology). Let the trie node
  under consideration be `N`, and let `S` be the still-unmatched **suffix** of
  the stored key `K` at this depth (initially `K`; as descent proceeds, the bytes
  already consumed by ancestor edge labels are removed from the front). Compute
  the **longest common byte-prefix** between `S` and each edge label of `N`
  (edges per §3; comparison is byte-by-byte, i.e. 8-bit chunks), and take the
  edge `L` with the longest match of length `m` bytes (`m = 0` if no edge shares
  even one leading byte). Then:
  - **No match (`m = 0`):**
    - If `S` is empty (`K` ended exactly at `N`): set `N.node-value := V`.
    - Else: add an **implicit leaf** edge — `N[S] := V` (the suffix maps directly
      to the value, no submessage).
  - **Full match of the edge label (`m = byte-length(L)`):** the whole edge label
    is a prefix of `S`. Let the child be `C = N[L]`.
    - If `C` is a **branch** (a map): recurse into `C` with the suffix `S`
      advanced past `L` (i.e. drop the first `m` bytes), and replace `N[L]` with
      the result.
    - If `C` is an **implicit leaf** (a non-map value):
      - If `byte-length(S) = byte-length(L)` (the key is exactly this leaf): update
        in place — `N[L] := V`.
      - Else (`S` is longer than `L`): promote the leaf to a branch. Replace
        `N[L]` with a new submessage `{ node-value: C, S' : V }` where `S'` is `S`
        with its first `m` bytes removed — i.e. the old leaf's value moves under
        `node-value`, and the new key is added as an implicit leaf under the
        remaining suffix.
  - **Partial match (`0 < m < byte-length(L)`):** the edge label and `S` share a
    proper prefix but then diverge — the **node-split** case. Remove edge `L`
    (with its child subtrie `C`). Let `Lp` be the first `m` bytes of `L`, `Ls` the
    rest of `L`, and `Ss` be `S` with its first `m` bytes removed. Insert a new
    branch under the common prefix:
    - If `Ss` is non-empty: `N[Lp] := { Ls : C, Ss : V }` — the displaced subtrie
      keeps the rest of its old label (`Ls -> C`), and the new value is an
      implicit leaf under `Ss`.
    - If `Ss` is empty (`S` ended exactly at the split point): `N[Lp] :=
      { Ls : C, node-value : V }`.
- **Returns:** `{ok, CommittedTrie}` — the updated, committed trie message.
- **Side effects:** writes the committed trie to the content-addressed store
  (§6). Drops prior commitments and re-commits.

### `get` — look up one stored key
- **Reads:** the key to look up (the resolved key name, or a `key` field of
  `Req`); `Base` (the trie).
- **Behaviour:** Descend the trie matching `K` byte-for-byte. Maintain the count
  of key bytes consumed by ancestor edge labels; let `S` be the unconsumed suffix
  of `K`.
  1. **Whole key consumed** (no suffix remains): return the current node's
     `node-value` if present, else `not_found`.
  2. Otherwise compute the longest common byte-prefix between `S` and the node's
     edge labels (§3), giving edge `L` with match length `m`:
     - `m = 0`: return `not_found`.
     - `m = byte-length(L)` (full edge match): let `C = N[L]`.
       - `C` is an **implicit leaf** (non-map): return `C` **iff**
         `byte-length(S) = byte-length(L)` (the key terminates exactly at this
         leaf); otherwise return `not_found` (a longer key that merely shares a
         prefix with a leaf — e.g. looking up `card` when only `car` is a leaf —
         is absent).
       - `C` is a **branch** (map): recurse into `C`, advancing past `L`.
     - `0 < m < byte-length(L)` (partial edge match): return `not_found` (the key
       diverges inside an edge label).
- **Returns:** `{ok, Value}` for a present key. For an **absent** key the device
  handler MUST return the two-tuple **`{error, not_found}`** (NOT the bare atom
  `not_found`). This is load-bearing: a handler that returns the bare atom
  `not_found` is wrapped by the resolver as `{ok, not_found}` — a *successful*
  result — which is wrong. Returning `{error, not_found}` makes `hb_ao:resolve`
  surface `{error, not_found}` and `hb_ao:get` (with a default) collapse it to the
  bare atom. The sentinel atom itself is the unhyphenated `not_found`, NOT a
  hyphenated binary.
- **Errors:** if no key is supplied at all (neither resolved name nor `key`
  field), return **`{error, <<"'key' parameter is required for trie lookup.">>}`**
  (a 2-tuple `{error, Binary}`, not a bare binary; §8).
- **Side effects:** none.

### `keys` — enumerate every stored key
- **Reads:** `Base` (the trie).
- **Behaviour:** Walk the whole trie, accumulating the full byte path to every
  **terminal**:
  - a branch node carrying `node-value` contributes the concatenated path that
    reaches it (a stored key terminating at that node);
  - a branch node with **no edges and no `node-value`** is also treated as a
    terminal contributing its path (an edge-less node position is a stored key);
  - every **implicit leaf** edge contributes `path ++ edge-label` (the leaf's
    full key);
  - excluded keys (§3) are never followed and never contribute.
- **Returns:** `{ok, List}` (or the bare list at the resolution layer) of all
  stored keys, each a binary. **Order is unspecified** — callers MUST NOT depend
  on it. Each stored key appears exactly once. The number of returned keys equals
  the number of distinct stored entries.
- **Side effects:** none.

## 5. Data formats & encodings

- **Keys on the wire** are binary, byte-for-byte. Prefix matching and key
  equality are **byte-exact**: no case folding, no hyphen/underscore folding, no
  Unicode normalisation. (This differs from `message@1.0`'s case-insensitive
  `get`; a trie lookup is exact. An implementation MUST NOT case-fold stored
  keys.) Comparison proceeds in 8-bit (one-byte) chunks.
- **Edge labels** are the byte suffixes described in §2/§4; their concatenation
  along a root-to-terminal path reconstructs the stored key exactly.
- **`node-value`** holds the terminal value verbatim (any AO-Core value).
- **Implicit leaf** edges hold the value verbatim as a non-map.
- **Commitments / IDs** are produced and encoded entirely by the `message@1.0`
  surface: IDs are base64url, never hex. The `set` result carries an
  `hmac-sha256`-type content commitment. The trie message (committed) is a normal
  AO-Core message and is content-addressable and verifiable like any other.
- The structural keys `node-value`, `device`, `commitments`, `priv`, `hashpath`
  are reserved and MUST NOT be used as stored keys (they would be skipped as
  edges and so could not be stored or retrieved as data). `path` is likewise
  reserved by the request shape and cannot be a stored key set via `set`.

## 6. Ordering, freshness & caching

- **Insertion order independence (normative).** The resulting trie — both its
  observable nested topology and the value stored for every key — MUST be
  identical regardless of the order in which keys are inserted, whether across
  multiple `set` calls or within one bulk `set`. Inserting `{a, b, c}` in any
  permutation, or one-at-a-time vs. in bulk, yields a structurally matching trie.
- **Update determinism.** Re-`set`ting an existing key replaces its value in
  place and MUST NOT change the topology (it does change `node-value`/leaf
  values, and re-commits).
- **Key-count monotonicity.** A single `set` of a key not already present
  increases the stored-key count by exactly one; a `set` of a key already present
  leaves the count unchanged. A `set` MUST NOT decrease the stored-key count.
  (Bulk `set` of N new keys increases the count by N.)
- **Side effects of `set`.** `set` writes the committed trie to the node's
  content-addressed store and (re)commits it. `get`/`keys` perform no writes and
  no commitment.
- **Freshness.** The device performs no result caching of its own; it operates on
  the supplied trie. A trie is mutable at no fixed external path — each `set`
  produces a new committed message with a new ID — so the constant-path staleness
  concern does not apply to the trie itself.

## 7. Security & authority

- A tried map is a normal AO-Core message. After `set`, the trie (and its nested
  submessages) carry an `hmac-sha256` content commitment, so the structure is
  integrity-checked and verifiable via the `message@1.0` `verify` surface: a
  `set` result MUST be a well-committed, valid message, and every nested
  submessage in the result MUST likewise verify.
- Because `set` rebuilds the structure, it **drops any pre-existing commitments**
  before re-committing — a trie's commitment never appears to cover a structure
  it does not. There is no authority check on `get`/`set` beyond the ordinary
  message/commitment rules inherited from `message@1.0`; the device does not gate
  callers by committer.
- `get` is failure-closed in the sense that an absent or partially-matching key
  yields `not_found`, never a neighbouring value: a prefix of a stored key, a
  superstring of a stored key, and a key diverging inside an edge label all
  return `not_found`.
- Private keys (per `message@1.0`: `priv*`/`private*`) are not stored entries —
  the `priv` structural exclusion (§3) keeps private metadata out of edge
  enumeration, so private keys are never traversed, returned, or enumerated.

## 8. Errors

- `not_found` — returned by `get`/the default lookup for any key that is not a
  stored entry: a key whose descent terminates with no `node-value`; a key that
  shares only a partial prefix with an edge label; a key that extends past an
  implicit leaf; or a key with no matching first-byte edge. This is the
  resolution-layer not-found sentinel (unhyphenated atom `not_found`; surfaces as
  `{error, not_found}` from a `resolve`).
- `'key' parameter is required for trie lookup.` — a binary error message
  returned by `get` when invoked with no key at all (neither a resolved key name
  nor a `key` request field). (This message is a human-readable binary, not a
  hyphenated atom; reproduce it verbatim for byte-compatibility.)
- All commitment/verification/ID errors are those of the `message@1.0` surface
  the device delegates to (e.g. `requested-committers-not-found`); the trie
  device introduces none of its own beyond the two above.

## 9. Composition

- **Inherited message surface.** Every reserved key the trie device does not
  itself implement (`id`, `commitments`, `committers`, `committed`, `verify`,
  `commit`, `set-path`, `remove`, `path`) MUST fall through to `message@1.0`, so
  a tried message is committable, verifiable, ID-addressable and cache-storable
  exactly like a plain message. The device adds `set`/`get`/`keys` and a
  trie-lookup default handler on top of that surface.
- **Reference equivalence to `message@1.0`.** For the externally observable map
  contract, a `trie@1.0` message MUST behave identically to a `message@1.0`
  message with the same logical key/value content: a `get` of any key returns the
  same result under both devices; a `set` of a key makes that key retrievable
  under both; the set of `keys` is the same multiset (modulo order). The trie
  device differs only in internal nesting (and the byte-exact, non-case-folding
  lookup of §5), never in which `(key → value)` pairs are observable.
- **Nested values are inert data.** The submessages a trie creates are plain
  nested maps; resolving deeper structure does not re-enter the trie device
  except via the trie's own `get`/`keys` descent. A consumer that wants the raw
  nested structure reads it as ordinary message data.
- **Bulk update.** Supplying many keys in one `set` is the supported pattern for
  loading or updating a large table atomically (single re-commit, single store
  write), and is equivalent in outcome to applying the keys individually.

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following, each checkable via public
`set`/`get`/`keys`/commit/verify:

1. **Radix splitting, not per-character.** `set`ting keys that share a leading
   byte sequence factors the shared bytes into a single edge whose label is the
   whole shared prefix. E.g. starting from empty and inserting
   `{ car:31337, card:90210, cardano:666, carmex:8675309, camshaft:777,
   zebra:0 }` (in any order) yields a structure matching:
   `{ zebra: 0, ca: { mshaft: 777, r: { node-value: 31337, mex: 8675309,
   d: { node-value: 90210, ano: 666 } } } }`. The shared `ca`, then `r`, then `d`
   are single multi-/single-byte edges — NOT one node per character.
2. **Implicit leaves.** A stored key with no descendants is stored as a direct
   `edge-label → value` (a non-map) at its parent, not as a `{ node-value: V }`
   submessage. (`zebra: 0` above is a direct value; `car`'s value lives under
   `node-value` only because `car` has descendants.)
3. **`node-value` marks prefix terminals.** A stored key that is a strict prefix
   of other stored keys (or otherwise terminates at a branch that has children)
   stores its value under `node-value` at that branch. E.g. inserting `toronto`
   then `to` yields `{ to: { node-value: 2, ronto: 1 } }`.
4. **Node split on partial divergence.** Inserting a key that shares a proper
   prefix of an existing edge label splits the edge: from
   `{ to: { node-value, ronto: 1, wn: 4 } }`, inserting `torrent` produces
   `{ to: { r: { rent: 5, onto: 1 }, node-value, wn: 4 } }` (the `ronto` edge
   splits at `r`).
5. **Leaf promotion.** Inserting a key that extends past an existing implicit
   leaf promotes that leaf's value under `node-value` of a new branch. Concretely,
   given a branch `a: { pple: 3, node-value: 7 }` (a stored `apple` leaf
   alongside `a`'s own value), inserting `app` splits the `pple` edge at the
   shared `pp` prefix, yielding `a: { pp: { le: 3, node-value: 8 },
   node-value: 7 }` — the displaced `apple` value becomes the `le` implicit leaf
   and the new `app` value is the branch's `node-value`. The symmetric case
   (a non-map leaf whose full edge is matched by a longer key) moves the old leaf
   value under `node-value` and adds the new suffix as an implicit leaf.
6. **Order independence.** Building a trie from a given key set in forward order,
   in reverse order, one key at a time, or all keys in one bulk `set`, all
   produce a structurally matching trie with identical stored values.
7. **`get` exact descent.** `get` returns the stored value for each inserted key
   and `not_found` for: a strict prefix of a stored key that is not itself stored
   (`ca`, `c` when only `car…` are stored); a one-byte-longer superstring of a
   leaf (`cardd`, `zebraa`); a key that diverges inside an edge (`cardan`,
   `cardana`, `carm`); and any unrelated key (`z`). (Verified by the canonical
   `car`/`card`/`cardano`/`carmex`/`camshaft`/`zebra` set.)
8. **Update in place.** Re-`set`ting an existing key changes only its stored
   value (and re-commits); the topology is unchanged and the key remains
   retrievable with the new value. A bulk re-set of every existing key updates
   every value with no topology change and no key-count change.
9. **`keys` completeness.** `keys` returns exactly the set of stored keys, each
   once, in unspecified order; its length equals the stored-entry count.
   Structural/excluded keys (`device`, `node-value`, `commitments`, `priv`,
   `hashpath`) never appear.
10. **Key-count bounds.** A `set` of a key not present increases the `keys` count
    by exactly one; a `set` of a present key leaves the count unchanged; a `set`
    never decreases the count.
11. **Commitment after `set`.** The message returned by `set` (and every nested
    submessage within it) is a valid, well-committed message under the
    `message@1.0`/commitment surface — `verify` of the result is `true`, and the
    result carries an `hmac-sha256`-type content commitment. Prior commitments are
    dropped and the structure re-committed.
12. **Exact, non-folding keys.** Lookup is byte-exact: keys are not case-folded
    and `-`/`_` are not equivalent (unlike `message@1.0`'s case-insensitive
    `get`). A key with no key supplied at all errors with the
    `'key' parameter is required for trie lookup.` binary.
13. **Inherited surface.** `id`, `commit`, `verify`, `commitments`, `committers`,
    `committed`, `set-path`, `remove`, and `path`-binding behave per
    `message@1.0` (the trie device does not capture them).

## 11. Out of scope

- The **internal/in-memory representation** of the trie. Only the observable
  nested message structure (edge labels, `node-value`, implicit leaves) and the
  `get`/`set`/`keys` results are constrained. Specifically, an implementation MAY
  use any internal node bookkeeping, provided a `set` result, viewed as a
  message, matches the topology pinned in §4/§10.
- The exact **byte layout / ID derivation / commitment cryptography** of the
  resulting message — all delegated to `message@1.0` and its commitment device.
- **Performance and storage strategy** (the node-count optimisation from
  implicit leaves is an implementation property; the spec constrains *results*,
  not node counts, except where node counts are observable via `keys`).
- The choice of **radix.** This spec pins radix-256 (byte-/8-bit-chunk
  comparison), which is the only normalisable, defined behaviour. Sub-byte radices
  (radix-2/4/16) are NOT specified and MUST NOT be assumed by reimplementers.
- Behaviour for **reserved keys used as stored keys** (`node-value`, `device`,
  `commitments`, `priv`, `hashpath`, `path`): these MUST NOT be used as stored
  keys; behaviour if they are is undefined.

## Open questions

- **`keys` order.** The reference returns keys in a specific traversal order
  (siblings in node-map order, value-bearing branch before its children), but
  node-map ordering is not a stable wire property, so the spec leaves `keys`
  order unspecified. If a deterministic order is ever required, it must be pinned
  explicitly (e.g. lexicographic) — reimplementers MUST NOT rely on the
  reference's incidental order.
- **`get` of the empty key (`<<>>`).** The reference's descent treats a
  zero-length key as "whole key consumed at the root", returning the root's
  `node-value` (normally absent → `not_found`). The empty key cannot be inserted
  via `set` as an implicit leaf (a zero-length suffix sets `node-value` at the
  root), so `set` with an empty-string key would set the root's `node-value`.
  Whether the empty key is a legitimate stored key is not exercised by the
  reference tests and is left unspecified; reimplementers SHOULD treat it as
  out-of-contract.
- **Non-string / non-binary values under split/promotion.** All reference cases
  store scalar (number/binary) values; a value that is *itself a map* placed at a
  key that later becomes a branch would collide conceptually with the branch's
  child edges. The reference does not distinguish a stored map-value from a
  branch except via `node-value`/implicit-leaf position, so storing a map as a
  leaf value at a key that subsequently gains descendants is ambiguous and left
  unspecified.
- **`set` with a `path` value other than `set`.** The device strips `path` from
  the insertable set regardless of its value; only `path: set` is the documented
  invocation. Behaviour when the trie device's `set` is reached via a different
  path value is not separately specified.
