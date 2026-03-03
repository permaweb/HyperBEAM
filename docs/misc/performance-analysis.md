# HyperBEAM Performance Analysis

Notes on execution overhead, profiling findings, and planned optimizations for
the `genesis-wasm@1.0` compute path.

---

## Instrumentation

`dev_process:compute_slot` emits a `computed_slot` log event with per-phase
timing and LMDB counters. Key fields:

| Field | Description |
|-------|-------------|
| `prep_ms` | Time to load process state from cache |
| `execution_ms` | Time inside `dev_process_lib:run_as(<<"execution">>, ...)` |
| `store_ms` | Time to write result state to LMDB |
| `wasm_cu_ms` | Time for the genesis-wasm HTTP roundtrip (CU eval) only |
| `exec_lmdb_reads` | LMDB reads during prep + exec phases |
| `exec_lmdb_read_us` | µs spent on those reads |
| `exec_lmdb_writes` | LMDB writes during prep + exec phases |
| `exec_lmdb_write_us` | µs spent on those writes |
| `store_lmdb_*` | Same counters for the store phase only |

`wasm_cu_ms` is captured via `timer:tc` around the `do_relay` HTTP call in
`dev_delegated_compute:do_compute`, stashed in the process dictionary, and
read back in `compute_slot`.

LMDB phase separation uses `hb_store_lmdb:take_stats()` which resets the
per-process accumulators — call it between phases to get independent counts.

---

## Execution Time Breakdown (slot ~548 000, Save-Observations action)

```
execution_ms:     ~1620 ms   (100%)
├─ wasm_cu_ms:     ~730 ms   (45%)  genesis-wasm HTTP roundtrip
├─ exec_lmdb_read_us: ~100ms  (6%)  ~1300 LMDB reads
├─ exec_lmdb_write_us:  ~2ms  (<1%) ~680 LMDB writes
└─ unaccounted:    ~790 ms   (49%)  hb_ao resolution chain overhead
```

The 49% unaccounted is the `hb_ao:resolve` pipeline: `normalize_keys` calls,
message serialization, and the dedup trie commit (see below).

---

## Fixed: Double `patch@1.0` in `dev_genesis_wasm`

### Root cause

`compute/3` called `patch@1.0` on the result of `delegate_request`, but
`do_compute` (called by `delegate_request`) already applied `patch@1.0` as
its final step. The second call was structurally always a no-op:

- After the first patch, `/results/outbox` contains only non-PATCH messages.
- The second patch finds no PATCH messages → `ToWrite = {}` → no root changes.
- But it still ran 4 `hb_ao:set` calls on the full ~620 KB state.

### Fix (`dev_genesis_wasm.erl`)

Removed the redundant outer `patch@1.0` from `compute/3`. The function now
returns `{ok, Res}` directly.

Also added `hashpath => ignore` on the inner `patch@1.0` call in `do_compute`:
patch is an intermediate transformation; cryptographic path linking at stage 9
of the resolution pipeline is not needed and adds an extra `normalize_keys`
pass over the full state.

### Result

~37 ms improvement (avg execution_ms: 1656 → 1619, ~2.2%).

---

## Dedup Trie: Cost Analysis

### Current implementation (`dev_dedup.erl`)

Every new (non-duplicate) message triggers:

1. `hb_ao:resolve(DedupTrie, #{ path => set, SubjectID => Slot }, Opts)`
   — the `trie@1.0` `set` handler calls `hb_message:commit` on the **entire
   trie** (HMAC-SHA256 over every node), then `hb_cache:write(CommittedTrie)`
   which writes all trie nodes to LMDB.
2. `hb_ao:resolve(M1, #{ path => set, <<"dedup">> => NewDedupTrie }, Opts)`
   — updates the process state with the new trie.

At slot 548 000, the dedup trie has 66 entries. A 66-entry binary trie has
~300–400 intermediate nodes, which accounts for the majority of the ~680
`exec_lmdb_writes` seen every slot — even when the same slot's message was
already seen (the check itself resolves through the trie's `hb_ao:get` path,
which may still trigger normalization).

### Planned fix: Flat LMDB dedup

Replace the in-state trie with direct `hb_store:write` calls:

```
Key:   <<"dedup-", ProcID/binary, "-", SubjectID/binary>>
Value: <<SlotNumber/binary>>
```

**Important implementation notes:**
- Use `hb_store:write/3` directly — NOT `hb_cache:write`, NOT `hb_ao:set`.
  `hb_cache:write` content-addresses and creates link entries; a previous
  attempt that used the wrong write path caused LMDB to balloon from 8 MB to
  30 MB because each entry generated O(trie_size) LMDB records.
- Remove `<<"dedup">>` from M1 explicitly before returning so the state
  snapshot no longer carries the old trie. If you don't do this,
  `dev_process_cache:write` drags the full trie into LMDB on every checkpoint
  write.
- For migration: check LMDB for the flat key first; if not found, fall back to
  reading the old in-state trie. After the first few slots all live message IDs
  will be in LMDB and the old trie becomes irrelevant.

Expected result: dedup LMDB writes drop from ~680/slot to ~0–1/slot (only new
unique message IDs require a write).

---

## LMDB Write Structure

`hb_store_lmdb:write(Store, Key, Value)` calls `elmdb:put(DB, Key, Value)`
directly — **one LMDB record per write**. No intermediate group/directory
entries are created automatically.

`hb_store:path(Store, [A, B, C])` just joins to `<<"A/B/C">>`. Writing to
that key writes one record. Intermediate paths like `<<"A">>` or `<<"A/B">>`
are NOT created unless you explicitly call `hb_store:make_group/2` for each.

`make_group(Store, Path)` writes the special marker value `<<"group">>` at
`Path` — one extra LMDB record, not a tree of records.

This means flat LMDB dedup entries are tiny (key ~90 bytes + value ~5 bytes)
and do not multiply like content-addressed cache entries do.

---

## Pending Optimizations

### 1. Skip-if-exists in `hb_cache:do_write_message` — ABANDONED

**Attempted and reverted.** Two implementations tried:

1. Check `hb_store:type` before `calculate_all_ids` — execution_ms 700→14000ms
2. Check `hb_store:type` after `hb_message:id(none)` but before `calculate_all_ids` — same regression

**Root cause of regression**: LMDB reads cost ~76µs each; writes cost only ~3µs
each in this workload. Adding one `hb_store:type` read-check per node
(~680 nodes/slot × 76µs = ~52ms overhead) far exceeds the ~2ms of LMDB writes
saved by skipping. The type check will always be net negative.

**Lesson**: The LMDB write overhead is negligible (~2ms/slot). Skip-if-exists
is not worth pursuing. The real bottleneck is `hb_ao:resolve` CPU overhead.

### 2. Flat LMDB dedup (see above)

### 3. Remaining `hb_ao` resolution overhead (~790 ms/slot)

Each `hb_ao:resolve` call runs `normalize_keys` at stage 1 on the full process
state. `do_compute` makes three such calls (dedup, delegated-compute, patch).
Each `hb_ao:set` inside `dev_patch:move` also normalizes. Together these
account for a significant fraction of the unaccounted time.

#### normalize_keys profiling result — NOT the bottleneck

Instrumented via `timed_normalize_keys` wrapper in `hb_ao.erl` and
`hb_ao:take_normalize_stats/0` read in `dev_process:compute_slot`.

Results across slots 553949–553958 (Save-Observations action):

```
normalize_keys_count: 463 per slot (constant)
normalize_keys_us:    ~3–11 ms per slot (avg ~5 ms)
execution_ms:         ~900–2400 ms per slot
```

normalize_keys is **~0.3–0.5% of execution_ms** — negligible. It is NOT
the source of the unaccounted ~790 ms. The 463 calls/slot at ~10µs each = 5ms.

The unaccounted time must be elsewhere in the resolution pipeline. Leading
candidates to investigate next:

- `hb_message:id/3` calls (HMAC-SHA256) during `hb_cache:write` — called for
  every map node on every slot, ~690 writes × (HMAC cost)
- `dev_patch:move` / `hb_ao:set` for each outbox message key
- `assignments_to_aos2` JSON serialization before the CU HTTP call
