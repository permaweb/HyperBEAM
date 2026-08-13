We are aligned. I will build a clean replacement from `edge`, with no legacy schedules, no compatibility extractor, and no Copycat changes. I will wait for your “yes” before creating the branch or editing anything.

## Protocol

`Assign-To` is the correct instruction name. On the wire it is `Assign-To`; normalized internally it is `<<"assign-to">>`.

Its value is a comma-separated list:

```text
Assign-To: process-a,process-b,process-c
```

The synchronizer will:

1. Take the real L1 target, if non-empty.
2. Split `Assign-To` on commas.
3. Trim surrounding ASCII whitespace.
4. Decode and canonicalize each member as a 32-byte base64URL address.
5. Ignore empty or invalid members.
6. Deduplicate the complete set, including the real target.

We will accept one `Assign-To` tag; duplicate tag instances will not be interpreted as another list. Complexity becomes proportional to the total declared targets, but Arweave’s tag-size limit provides a natural bound and this remains the correct complexity class. :codex-annotation{index="1"}

Every resulting target receives the transaction in the same canonical global order. All synthetic assignments will use `path: compute`; transaction path-like tags remain data rather than selecting execution-device keys.

Only data-free L1 transactions participate.

## Store schema

The complete scheduler-owned schema will be:

```text
~arweave-scheduler@1.0/
  sync/
    global                         -> <GlobalStateID>
    process/<ProcessID>            -> <ProcessStateID>

  targets/
    <Address>/<Ordinate>           -> <TXID>

  assignments/
    <ProcessID>/<Slot>             -> <AssignmentID>
```

`targets`, not `routes`, is the right name. Erlang variables and documentation will use `TXID`, not `TxId`, `tx-id`, or “TX ID” as two conceptual words. The target path links directly to the cached header’s TXID, so no redundant `tx-id` message field is required. :codex-annotation{index="8"}

### Global sync state

`sync/global` links to one content-addressed message:

```erlang
#{
    <<"from">> => FromHeight,
    <<"to">> => ToHeight,
    <<"block-hash">> => ToBlockHash
}
```

The meanings are:

- `from`: first completely indexed block, inclusive.
- `to`: last completely indexed block, inclusive.
- `block-hash`: canonical hash of block `to`.

There is no separate `synced-hash` or `indexed-from`. `block-hash` states exactly what the hash represents.

The global state link advances only after every transaction in the block has been examined and every target link has been written. Therefore `to` always means complete, not merely attempted. On interruption, the unfinished block is replayed idempotently because target keys are deterministic ordinates.

### Per-process sync state

`sync/process/<ProcessID>` likewise links to one content-addressed message:

```erlang
#{
    <<"process">> => ProcessID,
    <<"spawn-ordinate">> => SpawnOrdinate,
    <<"synced-to">> => SyncedToHeight,
    <<"next-slot">> => NextSlot
}
```

The fields mean:

- `spawn-ordinate`: the process transaction’s exact `{height, in-block-index}`.
- `synced-to`: the last complete block whose relevant target entries have been materialized into this process’s contiguous assignment slots.
- `next-slot`: the next contiguous AO slot to allocate.

I recommend `synced-to` as a block height rather than another ordinate. Process materialization will advance only across complete blocks, just like global synchronization. Therefore the comparison is direct:

```text
process.synced-to == global.to
```

If equal, there is no work and we do not list targets or assignments.

The exact ordinate is needed only at the lower boundary, to exclude transactions occurring earlier than or equal to the process spawn in the same block. A block height is better for the upper frontier, including empty blocks where no terminal transaction ordinate exists.

There is no separate `materialized-through`, `materialized-hash`, or process hash. Once all assignment writes succeed, the process state link advances to the new `synced-to`. The global `block-hash` authenticates that frontier. If interrupted before the link advances, the same deterministic assignments are safely rewritten at the same slots.

When process synchronization is needed, it lists `targets/<ProcessID>`, parses and numerically sorts the ordinates, and selects entries in the new block range. It never lists existing assignments. This may reread a process’s old target names when the global frontier advances, but those sets are expected to be tiny; retaining this simple operation is preferable to adding a kernel-level range cursor in this patch. :codex-annotation{index="9"}

### Ordinate

An ordinate is:

```text
<BlockHeight>-<ZeroBasedTransactionIndex>
```

It is ordered numerically as `{Height, Index}`, never by raw ASCII ordering and never by weave offset.

Example:

```text
1966084-23
```

The canonical source of the index is the TXID’s position in the block’s `txs` list.

We will not create `positions/<TXID>`. The only proposed use was locating a process spawn. That can be done once during process initialization using the existing transaction-status lookup for the height, followed by locating the TXID in that block’s canonical transaction list. Persisting another global TXID index is unnecessary. :codex-annotation{index="7"}

## Synchronization flow

A single node-local serialized synchronizer will:

1. Read `sync/global`.
2. Find the confirmed upper height.
3. Walk from `global.to + 1` through that height.
4. Read each block’s canonical TXID list.
5. Load each transaction header without data.
6. Skip transactions whose data size is non-zero.
7. Derive the deduplicated target set from native target plus `Assign-To`.
8. Link every `targets/<Address>/<Ordinate>` to the TXID.
9. Write and link the new global state after the block is complete.
10. Continue until the confirmed frontier is reached.

There will be one fixed rollout `from` height, configured for this new protocol. Every reissued process must spawn strictly within the globally indexable range. Future fresh nodes begin at the same boundary, so correctness never depends on the date the node first starts.

Process initialization then:

1. Locates the process’s spawn block and exact ordinate.
2. Requires the global index to cover that block.
3. Writes slot 0 from the process transaction.
4. Creates process sync state with:
   - `spawn-ordinate = H-I`
   - `synced-to = H - 1`
   - `next-slot = 1`
5. Materializes target entries strictly after the spawn ordinate through `global.to`.
6. Writes assignments in ordinate order at contiguous slots.
7. Advances the linked process sync state.

Each assignment will contain at least:

```erlang
#{
    <<"process">> => ProcessID,
    <<"slot">> => Slot,
    <<"path">> => <<"compute">>,
    <<"block-height">> => Height,
    <<"block-index">> => Index,
    <<"body">> => {link, TXID, ...}
}
```

## No legacy behavior

There will be:

- No `order-id` compatibility extractor.
- No dense-prefix preservation.
- No historical backfill mode.
- No reinterpretation of existing `scheduler-mode=all` processes.
- No migration code.
- No alternate behavior selected by `scheduler-mode`.

The current test processes are dead from the new scheduler’s perspective. The clean protocol requires newly issued processes after the rollout boundary. :codex-annotation{index="2"} :codex-annotation{index="5"}

A strict rejection of a process spawned before `global.from` is a protocol invariant, not compatibility machinery. It prevents an accidentally incomplete schedule from being presented as valid.

## Swap timing

Lazy expiry is the intended behavior, and I agree it is good news.

Relevant later transactions carry `block-height`, so reservations expire deterministically before the next meaningful state transition. A quiet process does no work merely to update a display of time. We will document and test that behavior without adding deadline ticks or read-time clock machinery. :codex-annotation{index="3"}

## Repository shape

After approval, my first action will be:

```text
git switch -c feat/fast-rwe-scheduler edge
```

Starting from `edge` means Copycat is clean automatically; there will be no staged revert and no intermediate broken implementation. The correct scheduler replaces the current attempt in one coherent patch. :codex-annotation{index="4"} :codex-annotation{index="6"}

I will selectively implement or bring across only what the final system needs:

- A rewritten `dev_arweave_scheduler`.
- A small internal `dev_arweave_scheduler_sync`.
- A rewritten `dev_arweave_scheduler_cache` implementing the linked state schema.
- The shared scheduler/process helpers genuinely required by the interface.
- `dev_arweave_swap`.
- `dev_carrier`.
- Surgical `~process@1.0` changes only where required.
- Tests for the final protocol.

I will not cherry-pick the old feature commits wholesale. In particular, the initial diff will not touch:

- `dev_copycat_arweave`;
- query/GraphQL indexing;
- Arweave offset storage;
- Copycat modes;
- legacy schedule migration.

## Acceptance criteria

Before the commit, I will verify:

- Native target routing.
- One and multiple comma-separated `Assign-To` targets.
- Whitespace trimming, invalid-member handling, and full deduplication.
- Native target plus identical `Assign-To` produces one assignment.
- Data-bearing transactions produce no targets.
- Canonical `{height,index}` ordering, including multiple data-free transactions sharing a weave offset.
- Exact same-block spawn filtering.
- Global sync resume and idempotent block replay.
- Process sync state comparison avoids work when caught up.
- Contiguous local slots despite sparse global ordinates.
- Lazy reservation expiry on the next relevant transaction.
- Carrier and token/swap payment flow end to end.
- No Copycat diff against `edge`.
- Focused EUnit tests followed by the complete `rebar3 eunit` suite.
- The final diff and commit contents before claiming completion.

I have already read [AGENTS.md](/Users/sam/.codex/worktrees/6010/hyperbeam/AGENTS.md), [CONTRIBUTING.md](/Users/sam/.codex/worktrees/6010/hyperbeam/CONTRIBUTING.md), and the [hacking guide](/Users/sam/.codex/worktrees/6010/hyperbeam/docs/misc/hacking-on-hyperbeam.md), and will follow their surgical and full-validation requirements throughout.

---

## Overnight status

- 2026-08-13: Created `feat/fast-arweave-scheduler` directly from `edge`; implementation started.
- 2026-08-13: Replaced the experimental scheduler with a global confirmed-block
  walker and sparse per-address target index. The synchronizer verifies every
  fetched header's signature and TXID, routes only data-free transactions, and
  persists the global frontier only after all target links for that block have
  landed.
- 2026-08-13: Added exact numeric ordinates, same-block spawn filtering,
  contiguous process slot materialization, serialized/idempotent frontier
  updates, and an O(1) caught-up process-state check. A short node-local ETS
  record prevents 60,000 caught-up callers from rereading the global state or
  chain tip independently.
- 2026-08-13: Added an internal scheduler-owned `blocks/<height>` cache outside
  the protocol-facing schema above. It stores only blocks already validated by
  the synchronizer and avoids repeating the spawn-block lookup for reissued
  processes; it is not a second completeness record.
- 2026-08-13: Ported the swap and carrier devices onto sparse `Assign-To`
  schedules, retained intentional lazy expiry, added carrier-to-swap payment
  coverage, and removed the obsolete dense mainnet stories.
- 2026-08-13: Restored the experimental branch's device-neutral
  `~process@1.0` cache policy. Slotless `/now` serves a cached process according
  to request `max-age` or node `process-now-max-age`, preserves
  `only-if-cached`, and contains no Arweave-scheduler-specific branch.
- 2026-08-13: The coordinated rollout choice and rationale are recorded in
  [decisions/arweave-scheduler-rollout.md](decisions/arweave-scheduler-rollout.md).

## Validation

- `rebar3 as test compile` — passed.
- `rebar3 device test -d dev_arweave_scheduler,dev_arweave_swap,dev_carrier --timeout-multiplier 20`
  — all 62 tests passed using isolated volatile scheduler/test stores.
- `rebar3 device test -d dev_process -t dev_process_cache:process_cache_suite_test_ --timeout-multiplier 20`
  — all six filesystem and volatile-store cases passed, including the generic
  `max-age` freshness cases; the three LMDB variants encountered the node-wide
  LMDB `not_found` condition described below.
- A fresh `hb_store_volatile` mainnet run indexed confirmed block `1978888`,
  resumed through `1978889`, checked the second block's `previous_block`
  linkage, verified every fetched signed TXID/header, and replayed the terminal
  frontier identically:

  ```text
  LIVE_FIRST=#{from => 1978888,to => 1978888,
               block-hash => HkY7iGmXRmS9SN4C2OLc58H6PyjCTIl8rCyCAC5Ywz8wvRJ13WOXsyVtXGrPOk2d}
  LIVE_SECOND=#{from => 1978888,to => 1978889,
                block-hash => 2_I6HZrxyoCPX8ClCKZVdlX6HHk3lVdqDR3av425xWAKODR4NUlmVyqTkO0f08yr}
  LIVE_REPLAY_EQUAL=true
  ```

- `rebar3 eunit` completed with 939 passes and 36 failures. Thirty-five were
  the node-wide LMDB `no_space`/`not_found` condition (confirmed independently
  by a minimal LMDB open), which is outside this patch and was explicitly
  directed to be side-stepped with `hb_store_volatile`. The remaining unchanged
  `hb_beamr` throughput assertion passed once and missed its timing threshold
  twice on the loaded host.
- The extra all-device packaged sweep reached 2,555 passes before 33 unrelated
  default-store link failures and a legacy process-test timeout cancelled the
  run. Every changed scheduler, swap, carrier, and process test passed in that
  sweep; the isolated volatile runs above are the authoritative final result.
- `git diff --check` passes, and `git diff edge -- '*copycat*'` is empty.

---

## Live network continuation — 2026-08-13

Current commander's intent (verbatim):

> - Optimize the sync speed (can we just important the block2 route that the old flow used).
> - Once in sync, demonstrate purchases and sales of test assets. Do not touch Dumdumz or names.

- Integrated the upstream `fix/arweave-scheduler-sync-integrity` merge at
  `6c8cce6290a78c60550fb1c0518ab2448015b82f` before restarting the node.
- The source default and live rollout boundary are block `1968888`, preserving
  the requested 10,000-block history window and the older test assets.
- A new `hb_store_volatile` scheduler store completed the full contiguous
  interval from `1968888` through the confirmed frontier. It first reached
  `1979249` at `2026-08-13T17:33:30Z` and has continued following the tip; a
  later explicit sync reached `1979256`.
- The live pass exposed four concrete integrity issues now covered by tests:
  faithful format-one TX reconstruction, valid zero-length Arweave data
  responses, lossless preservation of duplicate normalized tags, and complete
  loading of cross-store block links before the scheduler caches a block.
- The `rebar3 eunit-all`, resumed mainnet sync, and browser-level purchase/sale
  acceptance gates have all been exercised; exact results are recorded below.
- The temporary store holding the earlier `1969830` frontier did not survive;
  the current node therefore started a fresh contiguous pass at `1968888`.
- The live node is PID `16225` on `http://localhost:10001`, backed by
  `hb_store_volatile` as requested. Its exact runtime policy is eight-block
  batches, four block fetchers, one global limit of 32 header workers, and 20
  attempts at 250 ms for transient block/header transport failures.
- Historical `/block2` responses were measured and inspected against Arweave's
  implementation. They contain bare TXIDs once a block leaves the node's
  bounded recent-block cache, so importing the binary decoder cannot accelerate
  this 10,000-block backfill. The evidence and decision are recorded in
  [decisions/arweave-scheduler-block2.md](decisions/arweave-scheduler-block2.md).
- The first live pipeline exposed and removed a nested `4 * 16` header fan-out.
  Batches now share one header-worker ceiling. The global completeness record
  still advances only after the whole validated batch and all target links land.
- Direct gateway reads and a 32-worker global header ceiling advanced the live
  frontier from `1973511` to `1973967` in 66 seconds (6.9 blocks/second) without
  a failed batch. The eight-block atomic batch remains unchanged, and 32 is now
  the source default and live-node configuration.
- Bazar is running at
  `http://127.0.0.1:10002/k0IR1XuD-WUq93xVu4RF3iMO-hfxDyyzC15EuKt6XtY/?node=http%3A%2F%2Flocalhost%3A10001#/discover`.
  It uses a local static mirror only for application assets; every AO process
  read is directed to the live node on port `10001`.
- The purchase/sale proof asset is `LightSteelBlue`
  (`2KIwI3P4Rjq-y6euW9WOHweyrWZSp0hU6k1-HrRMZYg`), an `ASSET` token and neither
  a Dumdumz nor name process. Its offer, registration, and payment are at blocks
  `1979176`, `1979217`, and `1979222`; the payment carries the required
  `assign-to` instruction. No transaction was created or sent to any process.
- The local process execution reached `at-slot: 3` and `swap-height: 1979222`.
  Its final linked balance table is seller `kaYP9b...tKiFI = 0`, buyer
  `vZY2XY..._rKF0 = 1`, and the linked order table is empty. This proves the
  sale and purchase settled from the sparse schedule rather than merely
  appearing in an external index.
- Bazar now opens that exact process through the local node at
  `#/asset/created-assets/2KIwI3P4Rjq-y6euW9WOHweyrWZSp0hU6k1-HrRMZYg`.
  It displays `Owned by vZY2XY..._rKF0`, supply `1 / 1`, no current order, and
  the 0.11 AR listing plus purchase submission in Activity. The page reports
  that its current state was requested via `localhost:10001`.

### Final validation

- `rebar3 as test compile` — passed.
- `rebar3 device test -d dev_tx,dev_arweave --timeout 300` — all 69 passed.
- `rebar3 device test -d dev_arweave_scheduler --timeout 300` — all 11 passed,
  including the real-network format-one verification case.
- `rebar3 device test -d dev_arweave_swap,dev_carrier --timeout 300` — all 56
  passed.
- `rebar3 eunit-all` completed the entire core and packaged-device test list:
  3,586 passed and five failed. The failures are outside the patch: one
  `dev_push` remote-route expectation returned `{error,{bad_peer,<<"/">>}}`,
  and four legacy `dev_scheduler` live-HTTP cases returned `{error,<<>>}`. The
  runner then exited 139 during its known LMDB teardown. None of the five
  failing modules is modified here; every changed package passed its focused
  run above.
