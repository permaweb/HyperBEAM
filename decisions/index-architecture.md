# Decision: where the published-index format lives in HyperBEAM

## The prompt, as understood

Implement the published-index spec (docs/misc/published-arweave-indexes.md)
surgically: `hb_store:write`s in the appropriate format from `~match@1.0`,
`from`/`limit` keys on `hb_store:list` so cursors work across multiple
stores at once, orchestration for `~query@1.0` (maybe replacing/re-using
`hb_store:match`), arlmdb extended to read the DUP* container, and a live
demo over a >=10M-item index published to Arweave.

## The layering

Format knowledge lives at exactly two altitudes, and nowhere else:

- **Item bytes** (the 21-byte offset item, the 17-byte match item, the
  predicate hash): owned by the protocol/consumer modules —
  `hb_store_arweave_offset` for offset items, `dev_match` for match items.
- **Container** (single <<0>> key, DUPFIXED dup set, P_LEAF2 leaves): owned
  by the store backends — `hb_store_lmdb_set` (writable, local, via elmdb)
  and `hb_store_arlmdb` (read-only, from the weave).

`hb_store.erl` itself learns nothing about items, hashes, prefixes or
normalization. (The avoided branch's central failure was a path-transform
DSL in the kernel; we do not repeat it.)

## Store API

- `hb_store:list/3`'s request-map form gains two optional keys:
  `<<"from">>` (inclusive lower bound) and `<<"limit">>` (max results).
  The existing path form is unchanged; the map passes through
  `call_function` untouched, so no kernel plumbing changes.
- For a **sorted-set store**, `<<"list">>` names an item *prefix* (raw
  binary); the result is the ascending run of items >= max(prefix-extended
  from, prefix), capped at limit, all items sharing the prefix. Items are
  opaque binaries to the store.
- For hierarchical stores, from/limit bound the *result* (a child walk
  cannot be work-bounded; this is a fact about '/'-delimited names).
- Multi-store cursor merging is the *caller's* job (`~match@1.0`):
  first-store-wins `do_call_function` semantics are untouched. A scan asks
  each store in the list for a page and k-way-merges by item order;
  point lookups fall through the store list as today.
- `hb_store:match/2,3` is left as-is (two callers; it is the generic
  template-scan fallback). The spec's intersection lives in `~match@1.0`
  as leapfrog over cursored lists — it *re-uses* `hb_store:list` rather
  than replacing `hb_store:match`.

## New store backend: `hb_store_lmdb_set`

A small dedicated module (not a mode of `hb_store_lmdb`): an LMDB 1.0 env
opened dupsort|dupfixed with 64 KiB pages, one main key <<0>>, items as
duplicates. write = insert item(s); list = positioned dup scan honouring
prefix/from/limit; read = membership/point. No links, no groups, no path
semantics — which is why it is not `hb_store_lmdb` (that module's job is
the hierarchical message cache; grafting a second data model into it is
how the avoided branch got to +791 lines).

`hb_store_lmdb` itself: moves to LMDB 1.0 implicitly with the new elmdb
(0.9 files rebuild; no compat), gains `<<"page-size">>` defaulting 65536,
and its `read-only` becomes a real MDB_RDONLY open.

## Index instances and node opts

- **Offset index**: the existing `<<"index-store">>` of `hb_store_arweave`
  can now name sorted-set stores (local writable `hb_store_lmdb_set` head,
  published `hb_store_arlmdb` snapshots behind). Item encode/decode:
  `hb_store_arweave_offset`.
- **Match index**: a new `<<"match-store">>` opt naming the sorted-set
  store list for spec-format match rows. The existing `<<"match-index">>`
  (path rows, generic AO-Core match over arbitrary local messages) stays:
  it serves `hb_cache:match` consumers (device loading, native queries)
  that have nothing to do with weave offsets. The two answer different
  questions: "which local messages match this template" vs "which weave
  offsets carry this predicate".

## Writes

`hb_cache:write`'s existing `{match, AllIDs, Msg}` op is the hook. Row
construction is owned by `dev_match` (the hb_cache/dev_match duplication
collapses into exported helpers on the device, fixing the drifted
`store/1` asymmetry found in recon). For a message whose weave offset is
known (present in the offset index / supplied by the writer, as the bulk
indexer does), `dev_match` emits one 17-byte row per predicate:

    predicate = <<"~match@1.0/", LowerCaseKey/binary, "=", Value/binary>>
    row = <<(first 10 bytes of sha256(predicate))/binary, Offset:49, 0:7>>

for each tag, plus owner, recipient (when present), bundled-in (when
present) — via plain `hb_store:write(MatchStore, Row, <<>>)` (or the
batched form). Messages with no weave offset get no sorted-set rows; they
remain covered by the path-row index and the existing pending/ephemeral
cursor forms.

## Reads (~query@1.0)

`dev_query_arweave` compiles tags/owners/recipients to predicate hashes and
runs the spec §5 leapfrog over `match-store` cursors (from = cursor offset,
limit-bounded), each predicate's cursor being the k-way merge across the
store list. Height ranges become offset bounds over the ordering (block
index gives height→offset; no rows needed). `ids` go to the offset index.
Result offsets are resolved to items by reading the bytes at the offset
(id = sha256(signature); ID recomputation makes truncated-hash collisions
detected, not believed). Query shapes the index cannot serve (value
inequality, unindexed filters) fall back to today's materialised path.
Pagination stays `offset=<N>` cursors — already the format on edge.
