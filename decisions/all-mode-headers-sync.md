# `all` mode: pin the assignment path, and sync by headers

## The brief as I understand it

Two directives, plus an integration:

1. Every assignment must reach an execution device through `compute`, so the
   devices can be plain `-export([compute/3])` and inherit `~message@1.0` for
   everything else.
2. Restore the swap `deposit` for fungible-supply tokens, in under 100 lines.
3. Integrate `4385dc35f`'s copycat `mode=headers` so a fresh node syncs fast.
   Copycat stays minimal and generic; the scheduler makes safe use of it.

## What `all` mode does today

`discover(ProcID, <<"all">>, From, To, Opts)`:

1. `ensure_offsets(From, To)` -> `~copycat@1.0/arweave&mode=shallow`, which
   indexes a weave **offset** for every transaction in the range and walks into
   bundles. This is the slow part of a fresh sync.
2. `enumerate_blocks/4` reads the block headers and returns `{Height, TXID}`
   pairs in canonical chain order.
3. `base_layer_blocks/2` looks each id up in the offset index to (a) put an
   `offset` on the assignment and (b) skip anything whose codec is not
   `tx@1.0`, and fails the whole range with 503 if any id is unindexed.

## The observation

All-mode discovery does not need offsets at all.

- **Ordering** is already `enumerate_blocks`' own: blocks ascending, then each
  block's transactions in the order the block lists them. The existing doc
  comment says so outright, and says the offset order is *not* total, because
  every data-free transaction shares the offset of the one before it. So the
  offset is not what orders `all` mode -- it is only recorded.
- **The codec filter** is a no-op here: `block.txs` lists layer-1 transactions
  only. Bundled data items live inside them and are never enumerated.
- **The recorded `offset`** has no reader. Nothing outside the scheduler reads
  an assignment's `offset` key.

What `all` mode genuinely needs from the indexer is that each transaction's
**header** is cached locally, so `read_tx_header/2` can serve it. That is
precisely, and only, what `mode=headers` provides.

## Decision

- `all` mode syncs with `mode=headers` and builds assignments straight from
  `enumerate_blocks/4`. `base_layer_blocks/2`, the offset lookups and the 503
  path go. State mode keeps `mode=shallow` and its offset ordering untouched.
- All-mode assignment detail becomes `#{ block-height, path: compute }`.
  Pinning `path` here is the correctly-scoped fix for directive 1: it is only
  reachable from `discover(_, <<"all">>, _, _, _)` and `slot_zero(<<"all">>,
  ...)`, so `~scheduler@1.0` and state mode -- where a caller legitimately
  schedules a path -- are untouched. `Extra` wins the merge in
  `write_assignment/5`, so `lib_scheduler:base_assignment/4` needs no change.
  Rationale: in `all` mode the message was never addressed to the process, so
  its tags are data, never routing.
- The `/block2` binary decoder moves to `lib_arweave_common`, which is already
  where shared Arweave wire-format decoding lives. That keeps the copycat
  device itself to the mode plumbing and the header cache.

## Why this is safe

- Headers mode caches data-free headers. The scheduler reads headers and never
  data (see `read_tx_header/2`), and both execution devices read only tags and
  the `tx@1.0` commitment, so nothing reaches for a body that is not there.
- `mode=headers` validates each header against its id and signature before
  caching, so a lying node cannot inject a transaction into a block.
- The block's own `txs` list is the authority for membership and order; the
  `/block2` response is checked against it, and against the block id, before
  any of it is used.
- Slot 0 loses its `offset` for the same reason as every other slot.

## Rejected

- Changing `lib_scheduler:base_assignment/4`: it is shared with
  `~scheduler@1.0`, where a scheduled `path` is a real feature.
- Leaving the guard in the devices (`info/0` + `default`, `set/3`, `keys/3`):
  it leaves a stranger able to choose which key a slot resolves, and costs
  ~90 lines of boilerplate across two modules to half-defend against it.
- Teaching copycat anything about schedulers.

## Addendum: what `mode=headers` is actually good for (measured)

The cherry-picked mode, used unconditionally, was a **pessimisation**. Measured
on a fresh node syncing a real `all`-mode process (45 blocks, 110 slots),
best of three:

| pre-index      | time  | /block2 | /tx | downloaded |
|----------------|-------|---------|-----|------------|
| `mode=headers` | 5.13s | 15      | 206 | 19.5 MB    |
| `mode=shallow` | 3.98s | 0       | 183 | 17.2 MB    |
| none           | 2.74s | 0       | 111 | 11.4 MB    |

Two independent causes, both found by instrumenting the real path:

1. **`/block2` inlines nothing outside a peer's block cache.** Counted directly:
   historical blocks returned `inlined: 0, bare: 110`; blocks at the tip
   returned `inlined: 122, bare: 0`. A bare id is just the id, so every header
   still had to be fetched one by one -- after paying for the `/block2`
   response, which is ~550 KB whatever it contains, because it ships the
   block's PoA chunk.
2. **The port cached only `data_size = 0` headers.** In `all` mode every
   transaction is a slot, data-bearing ones included, so each of those was
   fetched by the index pass, discarded, and fetched again by
   `read_tx_header/2` -- which is why `/tx` was 206 for 110 slots.

## What we do instead

- **Copycat**: delete `cache_data_free_header/2` and call the module's existing
  `cache_tx_header/2`, which every other mode already uses and which caches any
  header, data-bearing or not. That is one fetch per transaction instead of
  two, and it is less code, not more.
- **Scheduler**: `mode=headers` earns its request only where a peer still holds
  the block, so the scheduler asks for it only within a horizon of the chain
  tip and leaves older blocks to be fetched on demand -- exactly once each, by
  the `read_tx_header/2` call that mints the assignment. The policy is the
  caller's; copycat stays a generic indexer that knows nothing about
  schedulers.

This is the split the brief asked for, and it makes both cases fast: backfill
pays nothing it does not use, and a node following the tip replaces N per-
transaction fetches per block with one request.

## What is left on the table (measured, not fixed here)

With the above in place, a fresh sync of the 45-block fixture range is 2.35s
(from 3.98s), and `/tx` fetches fall from 206 to 112 -- exactly one per slot.

A fresh sync of a *large* backfill is still bounded by something else entirely.
Syncing ~1300 blocks of history downloaded **971 MB in 10 minutes**, of which
the transaction headers are a small fraction: `enumerate_blocks/4` needs each
block's `txs` list, and the only way to get it from the peers we reach is to
download the whole block -- ~740 KB, nearly all of it the proof-of-access
chunk. 1443 block fetches, 15k transaction headers.

Two leads, both left alone deliberately:

- `/block/height/<h>/txs` would be a few hundred bytes instead of 740 KB, but
  every peer tried answers `421 Subfield block querying is disabled on this
  node.`
- `/block2/height/<h>` is 553 KB against `/block`'s 740 KB for the same block,
  *and* carries the transaction headers inline near the tip. Switching the
  block cache itself to `/block2` would cut a quarter of the bytes off every
  backfill and make the headers free where they are available. That is a change
  to how `~copycat@1.0` fetches blocks for every mode, not just this one, so it
  wants its own PR and its own evidence rather than being smuggled in here.
