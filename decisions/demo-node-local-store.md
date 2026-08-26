# Decision: demo nodes carry a `local-store` on the arweave store

## The prompt, as understood

W5 boots a node whose offset/match indexes are the published arlmdb
containers behind writable heads, then benchmarks GraphQL query pace,
distinguishing first-touch from steady-state honestly.

## The issue

The first benchmark run (point-id class) measured 130 ranged weave reads
per query -- 26 arlmdb descents x ~5 pages -- with steady-state identical
to cold. The trace shows each GraphQL query re-loads the item from the
weave many times (per resolver field), and nothing retains the loaded
message: `hb_store_arweave:read/3` only consults `read_local_cache` and
writes back via `maybe_cache` when the store's `local-store` key is set,
which the node-shape sketch (and `test_env_with_match_store/0`) omitted.

## Options

1. Benchmark as-is: honest about the config used, but it measures a
   misconfigured node -- every field resolution pays a full remote load.
2. Add `<<"local-store">>` (the node's writable lmdb head) to the
   arweave store opts, matching `test_env_with_blocks/2`'s layering, and
   re-run. Index page reads stay remote (arlmdb caches nothing); item
   loads become write-through-cached, which is the documented intent.

## Decision

Option 2. The `local-store` key is existing kernel behaviour, not new
code; the demo's stated shape ("hb_cache caches resolved ITEMS locally,
so repeat queries get faster") is only true with it set. Both demo nodes
are rebooted with fresh store dirs and the corrected opts; the correctness
evidence from the first boot remains valid and is kept.
