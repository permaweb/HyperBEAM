# Arweave scheduler `block2` ingestion

## Prompt

Optimize the global Arweave scheduler synchronization, preferably by importing
the old flow's `block2` route, then complete a live sync and demonstrate buys
and sales of test assets without touching Dumdumz or names.

## Issue

The global walker fetches the JSON block and then issues one `tx` request for
every L1 TX. Block `1969831`, for example, contains 93 TXs. The requests are
parallelized, but the work remains one network round-trip and one gateway parse
per TX. The Arweave `block2` endpoint can return the same signed TX headers in a
single response, with bare TX IDs for entries the upstream node cannot inline.

The scheduler must retain the JSON block's TX order as the source of ordinates,
verify that the `block2` block ID and TX IDs match it exactly, and verify every
inlined signed TX before indexing it. A malformed or mismatched response must
not advance the global frontier.

## Options

1. Reintroduce Copycat `headers` mode and call it once per block. This brings a
   large Copycat patch, a second set of block markers, and an unnecessary device
   boundary into the scheduler's single global walk.
2. Parse only TX IDs and data sizes from `block2`, then fetch every data-free
   header individually. This reduces request count but throws away the signed
   headers already present in the response.
3. Import the proven `block2` header decoder into the scheduler synchronizer.
   Validate its block/TX layout against the JSON block, use each valid inlined
   header directly, and fall back to the existing individual header request for
   bare IDs only.
4. Retain the authenticated per-TX reads, but fetch, validate, and index a small
   batch of independent blocks concurrently. Commit the global frontier only
   after every block and target link in that batch succeeds.

## Decision

Use option 4. A real 93-TX historical block returned 93 bare IDs from `block2`
and zero inlined headers. This is the endpoint's intended behavior: full TXs are
served only from the Arweave node's bounded recent block cache. A current
confirmed block did inline all 10 TXs, but that cannot accelerate the requested
10,000-block historical catch-up and does not justify importing roughly 250
lines of binary protocol parsing into the scheduler.

Batching preserves the existing authenticated header path and changes only the
unit of concurrency and frontier commitment. An initial eight-block prototype
improved cold-network samples from 1.647–1.914 seconds to 0.630–0.951 seconds,
but its nested four-block by 16-header worker fan-out caused transient upstream
misses in a sustained live run. The production pipeline keeps concurrent block
fetches while applying one global 32-worker ceiling to all headers in the batch,
with bounded per-object retries. The 32-worker pipeline advanced 456 historical
blocks in 66 seconds (6.9 blocks/second) against the direct gateway without a
failed batch. Any partial header or target writes are
content-addressed/idempotent, while the durable global record advances only
when the entire batch has completed.

The live node uses a fresh `hb_store_volatile` store from rollout block
`1968888`, as requested to side-step the unrelated node-wide LMDB problem.
That store completed all 10,362 blocks through its initial confirmed frontier
and now follows the tip. The final Bazar/process replay therefore exercises the
batched implementation over the complete rollout interval rather than a
partial synthetic sample.
