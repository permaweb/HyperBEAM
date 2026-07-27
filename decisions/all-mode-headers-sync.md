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
