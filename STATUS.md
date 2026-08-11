# `impr/arweave-consensus-cache` -- status

Branch: `impr/arweave-consensus-cache`, from `feat/arweave-block-validation`
(`936403e41`). Worktree: `.claude/worktrees/remote-control-75002e`.

The "Arweave consensus cache" brief, implemented: the validator's durable state
is one `arweave-block@2.9` message per block, linked into a chain, carrying
placements, recording the checks that established it, published in an order
whose last step is what makes a block's presence mean "finished".

## Baseline (before any change)

```
HB_PORT=8801 rebar3 device test --devices dev_arweave                  -> 73 passed
HB_PORT=8802 rebar3 device test --devices dev_arweave_block,\
  dev_arweave_block_index,dev_arweave_history,dev_arweave_merkle,\
  dev_arweave_spora,dev_arweave_tx,dev_arweave_vdf,dev_arweave_wallets -> 71 passed
```

## What changed

| Area | Change |
|---|---|
| Chain state | Merged into the block message; `lib_arweave_state` now reads one |
| Chain shape | `previous` as a device key; the anchor window is a walk |
| Validation | Eleven named checks; `validation/checks` on every stored block |
| Selection | `profile` / `verify`, with unknown names and broken sets refused |
| Placements | New `lib_arweave_placement`; `~arweave@2.9/placement` |
| Publication | Ordered: transactions, placements+offsets, components+block, hash |
| Components | Version chains on the account tree and the block index |
| Histories | `~arweave-history@2.9` gains `push` and `to-binary` |
| History | `~arweave@2.9/backfill`, index-relative `materialize` |
| Settlement | `arweave-settled-transaction` hook, per-block markers |
| Store | `hb_store_arweave:read_location/3`; `no_store` is a no-op |
| Layout | `lib_arweave_paths` owns every durable name |

## Verification

```
HB_PORT=8917 rebar3 device test --devices dev_arweave,dev_arweave_block,\
  dev_arweave_block_index,dev_arweave_history,dev_arweave_merkle,\
  dev_arweave_spora,dev_arweave_tx,dev_arweave_vdf,dev_arweave_wallets
  -> All 166 tests passed          (144 at the baseline; 22 new)

HB_PORT=8850 rebar3 device test --devices <the above>,dev_query,dev_copycat,dev_bundler
  -> All 266 tests passed

HB_PORT=8920 rebar3 device test --with-core
  -> Failed: 5.  Passed: 3645.

rebar3 dialyzer
  -> 239 warnings, none in any module this branch adds or changes.
     `hb_store_arweave' -- the only core module touched -- is clean.
```

The five are `push@1.0:test_push_prompts_encoding_change` (`{bad_peer,<<"/">>}`)
and four `scheduler@1.0:http_get_legacy_*` (`{badmatch,{error,<<>>}}`). They
fail identically at the base commit `936403e41`
(`HB_PORT=8903 rebar3 device test --devices dev_push,dev_scheduler` ->
`Failed: 5. Passed: 38.`), reach external services, and touch nothing this
branch changes.

Four vectors were mutation-checked -- reverted the fix, confirmed the vector
fails, restored: the anchor-window walk, the cache round trip, the settlement
chain end, and the refusing hook.

### Against mainnet

```
rebar3 device test --devices dev_arweave --test all:live_account_transition
  bootstrap: 343115 ms
  applying 1976743 onto 1976742, 0 transactions, accounts=present -> 153327 ms
  applying 1976744 onto 1976743, 3 transactions, accounts=present ->  17335 ms
  Test passed.          (the account root equals the block's signed wallet-list)

rebar3 device test --devices dev_arweave --test all:live_sync
  sync from 1976742: applied 3, now at height 1976745; second pass applied 0
  sync from 1976745: applied 3, now at height 1976748; second pass applied 0
  Test passed.

rebar3 device test --devices dev_arweave --test all:live_settle
  first pass: settled 35; second pass: 0
  Test passed.

rebar3 device test --devices dev_arweave --test all:live_account_transition
  applying 1976748 onto 1976747, 1 transactions, accounts=present -> 149299 ms
  Test passed.          (onto a parent this branch's own sync published)
```

The store those probes left behind, as the publication order requires:

```
_build/arweave-test-vectors/~arweave@2.9/
  blocks      56   (checkpoint + 49 anchor-window headers + 6 applied)
  placements  20
  settled     38
  tip          1
  accounts-anchor 1
```

## Decisions

- [The validated block alias is namespaced](decisions/block-hash-alias-ownership.md)
- [`previous` is a device key, not a stored link](decisions/previous-is-a-key-not-a-link.md)
- [Historical materialisation takes an explicit `from`](decisions/backfill-has-no-frontier.md)
- [A fetched transaction is kept in two forms](decisions/two-forms-of-a-transaction.md)
- [The history entries keep the shape they already have](decisions/history-entry-shape.md)

## Log

- Read the subsystem, `hb_cache`/`hb_link`/`hb_hook` and the two stores.
- Implemented the brief; deterministic vectors added alongside each piece.
- The live mainnet probes found two defects the deterministic vectors did not.
  Both now have vectors that fail without the fix.
  1. A stored `+link` whose target is a name rather than a content identifier
     raises the moment the message is read back and re-normalised -- which
     `hb_ao:resolve/3' does to its base after every successful call, so it was
     every block a node applies onto. See
     `decisions/previous-is-a-key-not-a-link.md`;
     `stored_block_survives_a_round_trip_test` is the vector.
  2. The settlement walk collected the hash below the oldest block the node
     holds, then failed to read it -- and because the list is announced
     oldest-first, that one failure meant nothing settled at all. The walk now
     reads each block as it collects it and stops where the chain does. The
     vector is the wide-batch half of
     `settled_transactions_reach_the_hook_test`.

  Both are the same shape of mistake: a synthetic chain has no bottom and no
  cache round trip, and a real one has both.
