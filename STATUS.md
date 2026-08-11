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
| Chain shape | `previous` links the parent by hash; the anchor window is a walk |
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
| Transactions | One form, `tx@1.0`; it gains `denomination` and a `format` fix |

## Verification

```
HB_PORT=8948 rebar3 device test --devices dev_arweave,dev_arweave_block,\
  dev_arweave_block_index,dev_arweave_history,dev_arweave_merkle,\
  dev_arweave_spora,dev_arweave_tx,dev_arweave_vdf,dev_arweave_wallets
  -> All 166 tests passed          (144 at the baseline; 22 new)

HB_PORT=8951 rebar3 device test --devices dev_tx,dev_query,dev_copycat,\
  dev_bundler,dev_ans104
  -> All 156 tests passed

HB_PORT=9400 rebar3 device test --with-core          -> Failed: 7.  Passed: 3648.
  and the same, at the base commit, immediately after -> Failed: 7.  Passed: 3620.

rebar3 dialyzer
  -> 239 warnings, the same count as the baseline, and none in any module
     this branch adds or changes. `hb_util`, `dev_router` and
     `hb_store_arweave` -- the core modules touched -- carry only the
     warnings they carried before.
```

Five of the seven are the same on both sides:
`push@1.0:test_push_prompts_encoding_change` and four
`scheduler@1.0:http_get_legacy_*`. The other two differ between the runs --
`b32-name@1.0` and `bundler@1.0` here, `hb_store_gateway` twice at the base --
because every test beyond the shared five reaches an external service and the
set that fails is a different draw each time.

Run the suite on a loaded machine and that draw grows sharply: 21 failures on
this branch and 10 at the base in one such pair, with 8 and 17 hackney
`checkout_timeout`s respectively. It is connection-pool exhaustion under
concurrency, not the gateway refusing -- no run recorded a 429, and the one
404 came from a local Cowboy test node whose store fetch had failed, for a
transaction that answers 200 on every direct request. Compare a suspect run
against the base commit *back to back*; an hour apart measures the machine.

Seven vectors were mutation-checked -- reverted the fix, confirmed the vector
fails, restored: the anchor-window walk, the cache round trip, the settlement
chain end, the refusing hook, and the three the corrections added. Dropping
`exclude_format_tag/3` fails `dev_tx:format_1_test`; dropping `denomination`
from `?BASE_FIELDS` fails `denomination_test`; dropping the block-hash clause
of `hb_util:native_id/1` fails `block_hash_is_an_id_test_parallel` with
`function_clause`.

### Against mainnet

A cold bootstrap and three probes, on a freshly emptied store. The default
EUnit timeout does not cover a cold bootstrap, hence `--timeout 5400`; the
machine was under other load throughout, so the timings are upper bounds.

```
rebar3 device test --devices dev_arweave --timeout 5400 \
  --test all:live_account_transition
  bootstrap: 614578 ms
  applying 1976855 onto 1976854, 5 transactions, accounts=present
  apply: 274560 ms
  Test passed.          (the account root equals the block's signed wallet-list)

rebar3 device test --devices dev_arweave --timeout 5400 --test all:live_sync
  reusing chain at height 1976854
  block 1976855 carries 5 transactions
  sync from 1976854: applied 3, now at height 1976857
  second sync: 315 ms, applied 0
  Test passed.

rebar3 device test --devices dev_arweave --timeout 5400 --test all:live_settle
  announced 0 blocks this pass          (`sync` had already settled them)
  newest settled block: DXUlKxgmGkeRCHLL1gOueGOqkVzsLFxULszLCrZXLeUDNPIrFPxlirpgWyjd5nxm
  Test passed.
```

Five real mainnet transactions went through the single `tx@1.0` form and were
validated against the block that carries them: the transaction root, every
signature, the weave arithmetic and the account transition all agree, which is
the round trip being byte-exact rather than merely parseable.

The store those probes left behind. A validated block is named by its bare
hash, so the layout is 64 character links at the root and nothing under
`~arweave@2.9/blocks`:

```
_build/arweave-test-vectors/
  <64-char links>  53   (checkpoint + anchor-window headers + applied blocks)
  <43-char links>  15   (transactions, by their Arweave identifier)
  ~arweave@2.9/
    placements     15
    settled        35
    tip             1
    accounts-anchor 1
```

## Decisions

- [The bare block hash names the validated block](decisions/block-hash-alias-ownership.md)
- [Historical materialisation takes an explicit `from`](decisions/backfill-has-no-frontier.md)
- [A transaction has one message form](decisions/one-form-of-a-transaction.md)
- [The history entries keep the shape they already have](decisions/history-entry-shape.md)

## Log

- Read the subsystem, `hb_cache`/`hb_link`/`hb_hook` and the two stores.
- Implemented the brief; deterministic vectors added alongside each piece.
- The live mainnet probes found two defects the deterministic vectors did not.
  Both now have vectors that fail without the fix.
  1. A stored `+link` whose target is a name rather than a content identifier
     raises the moment the message is read back and re-normalised -- which
     `hb_ao:resolve/3` does to its base after every successful call, so it was
     every block a node applies onto. See
     `decisions/block-hash-alias-ownership.md`;
     `stored_block_survives_a_round_trip_test` is the vector.
  2. The settlement walk collected the hash below the oldest block the node
     holds, then failed to read it -- and because the list is announced
     oldest-first, that one failure meant nothing settled at all. The walk now
     reads each block as it collects it and stops where the chain does. The
     vector is the wide-batch half of
     `settled_transactions_reach_the_hook_test`.

  Both are the same shape of mistake: a synthetic chain has no bottom and no
  cache round trip, and a real one has both.

- Two decisions were reversed on review, and each reversal was swept for scar
  tissue rather than patched over.
  1. **The block hash names the block.** `?IS_ID` now admits the 64 characters
     an Arweave block hash encodes to, so a link may name a block by hash and
     `previous` is a stored link again rather than a device key. Widening the
     guard left two converters short of its domain --  `hb_util:native_id/1`
     and `human_id/1` raised on a block hash, and `dev_router:binary_to_bignum/1`
     assumed every ID was 256 bits, which was already wrong for an Ethereum
     address. All three are total now;
     `hb_util:block_hash_is_an_id_test_parallel` is the vector.
  2. **A transaction has one message form.** `~arweave-tx@2.9` reads and writes
     `tx@1.0`, and `lib_arweave_tx` is the boundary between that message and the
     `#tx{}` record. Closing the gap found two real defects in `tx@1.0`: it
     dropped `denomination`, and it wrote a format 1 transaction`s `format`
     field back as a tag, so no format 1 transaction round-tripped. `dev_tx``s
     `denomination_test` and `format_1_test` are the vectors.
