# Deploying the Arweave bulk indexer

A guide for the operator taking over deployment: how to put the indexer on
each fleet machine, run it over that machine's storage modules, monitor it,
and verify what it produced. Written 2026-08-26 from the state of the build;
re-read the STATUS.md of each named branch for anything that landed after.

## 1. What you are deploying

The indexer scans UNPACKED Arweave storage modules on local disk and emits
the published-index rows of `docs/misc/published-arweave-indexes.md` (read
it first — the row formats, container layout and audit checklist are the
contract):

- one 21-byte offset item per data item (`id:80|type:4|offset:50|length:34`);
- 17-byte match rows (`sha256(predicate)[0..9] | offset:49 | 0:7`) under the
  final predicate vocabulary: one row per wire tag (lower-cased name, raw
  value), `committer`, `field-target` (when present), `parent` (nested
  bundle items only), `commitment-device=ans104@1.0`; the codec tags
  `bundle-format`/`bundle-version` get no rows;
- exclusion by weave-offset intervals (`redstone-exclusion-intervals.bin`,
  sha256 `c4e0145cb046b0e570ddc953b6be188e20471a36d1d8886269c1b22b72a818f2`,
  1,187,180 sorted big-endian `<<Start:64, End:64>>` pairs): items whose
  start offset falls in a covered range get no rows at all and are skipped
  before parsing. The file's sha256 is recorded in every run's metadata.

Pipeline shape: manifest pass (tx boundaries from the node's chunk index)
→ scan pass (bounded parallel sequential readers, tx-clipped reads with
read-ahead, per-item parse in a C NIF with an Erlang oracle fallback)
→ per-worker sorted runs → k-way merge → `offset.items`/`match.items`
(sorted fixed-width rows) → spec container built with elmdb's single-txn
sorted append. Runs and `.items` from different machines merge with one
further k-way pass, so per-machine outputs are the collection unit.

## 2. Code and branches

| what | where | tip at writing |
|---|---|---|
| indexer | HyperBEAM branch `feat/arweave-index-scanner` | `708baaa2b` (+ the interval-exclusion commit landing now) |
| query/store stack | branch `claude/arweave-indexing-implementation-f3ef8c` | `f0899dde9` |
| elmdb (LMDB 1.0, dup sets) | `~/src/elmdb`, branch `feat/dup-sets` of elmdb-rs | `b2690e2` |

The scanner branch is merged into `claude/arweave-indexing-implementation-
f3ef8c` (9a74db9e1) — deploy from that one branch. One follow-up commit (the
interval-exclusion implementation) lands on `feat/arweave-index-scanner`
and folds in with `git merge feat/arweave-index-scanner`. NOTHING is
pushed anywhere — deployment is by rsync of a worktree. The indexer modules
are `src/core/lib/lib_arweave_index_*.erl` (+ `_test_vectors`), the NIF is
`native/lib_arweave_index_item/`, the chunk-storage layer is the vendored
`src/core/lib/arweave/*` + `src/preloaded/arweave/lib_arweave_*` (GPLv2 —
keep `LICENSE.md`/`VENDOR.md` alongside). Each branch worktree carries its
own STATUS.md and decisions/ — the operational history lives there.

## 3. Machine prerequisites

- **OTP 28.** On dev-2 the default `/usr/local/bin` toolchain is OTP 27 and
  fails with "corrupt atom table" — build and run with
  `PATH=/home/hb/otp-28.0/bin:$PATH` (find/install the equivalent per
  machine).
- **A C compiler + OpenSSL headers.** The NIF builds `-O3` and links
  libcrypto (sha hardware). No Rust needed for scanning.
- **elmdb is only needed for the final container-build step**, not for
  scan/merge. Where needed: HyperBEAM remaps its checkouts dir to
  `src/forge`, so wire the untracked symlink
  `src/forge/elmdb -> <elmdb feat/dup-sets checkout>`. Two traps:
  every compile with the checkout strips the elmdb pin from the worktree's
  rebar.lock (restore with `git checkout rebar.lock`; never commit a lock
  missing the pin), and on macOS a rebuilt `.so` dies SIGKILL
  (Code Signature Invalid) until `codesign -f -s -` is run on both built
  copies. Linux needs neither.
- Disk for output: budget ~22 B per indexed item for offset rows and
  ~17 B x ~10 rows per item for match rows, roughly ~0.2 GB per million
  items combined, plus transient run files of similar size.

## 4. Inputs per machine

1. **Unpacked storage modules.** Directory naming
   `storage_modules/storage_module_<N>_unpacked` (or
   `storage_module_<size>_<N>_unpacked`). The indexer reads
   `chunk_storage/<start-offset>` files (2,097,176,000 B: 8,000 slots of
   3-byte prefix + 256 KiB chunk) directly with pread. In an unpacked
   module a zero prefix means an empty slot. PACKED modules are useless to
   the scanner — unpack first (see `/home/hb/unpack-scratch` on dev-2 for
   the verified sector-band converter and its journal/resume pattern;
   replica_2_9 entropy is footprint-interleaved across the whole partition,
   so use that band converter, never a naive file-by-file pass).
2. **A tx-boundary manifest.** Derived by folding a chunk index:
   - On a HyperBEAM-miner machine (dev-2): the miner's LMDB index at
     `<data-dir>/index` — READ-ONLY. Note the index keys keep the StoreID
     spelling from sync time (`…replica.2.9`) even after the module
     directory is renamed `_unpacked`.
   - On a raw Arweave node: the node's RocksDB (`ar_sync_record_db`,
     `ar_data_sync_db`/chunks_index) via the vendored import path,
     strictly read-only. **This source is designed and vectored but was
     NOT exercised on a real node yet — shake it down on ONE machine and
     compare manifest counts against `GET /data_sync_record` style totals
     before trusting the fleet.**
   The gateway "enrichment" join is no longer needed (parents are
   nested-only).
3. **The exclusion intervals file.** Copy to each machine; VERIFY the
   sha256 above before every run — the run metadata records it, and a
   wrong/truncated file silently changes what gets indexed.

## 5. Running

Configuration knobs (see `lib_arweave_index_*` moduledocs for the exact
message keys): data dir, module id(s), worker count, from/to offset bounds,
output dir, intervals file path.

- **Workers**: 24 on NVMe-class machines (dev-2's ceiling was BEAM-side
  scaling, not disk); **1-2 per spindle on rotational disks** — more only
  causes seek thrash; the parse keeps up regardless (compute ceiling
  ~4-5 GB/s vs ~0.2 GB/s per spindle).
- Etiquette on shared machines: `nice -n 5` (or lower), never touch
  processes you did not start, leave ~8 cores headroom, check `uptime`
  before ramping.
- **Detached launch pattern** (survives your session; see
  `/home/hb/arweave-indexer/w8-launch.sh` on dev-2 for the working
  template): `setsid nohup bash <launch>.sh > /dev/null 2>&1 < /dev/null &`
  where the script sets PATH (OTP 28), a unique `HB_PORT`, holds rebar3's
  stdin open (fifo feeder — non-TTY `rebar3 shell` exits when stdin
  closes), runs the scan+merge driver, and ends with `erlang:halt(0)`
  (`init:stop()` leaves the escript spinning). One-line evals only; io is
  swallowed, so all output goes to log files.
- Scan front-to-back: output is usable as a growing prefix, and a machine
  mid-unpack can consume below a highwater cursor (dev-2's convention:
  `storage_modules/UNPACK-CURSOR` holds the offset below which files are
  fully converted; read only below it).

Expected rates (measured on dev-2, 24 workers, 24 real cores):
full 3.6 TB partition in **699.9 s = 5.13 GB/s weave-relative** under the
final vocabulary with interval exclusion (1.12 TB physically read; 603 GB
of covered ranges skipped unread); ~12 min merge follows. Fleet math: ~50 TB/machine
across 10 machines is hours, not days, against the 72 h network goal;
spinning-disk machines contribute at ~0.2 GB/s x spindles.

## 6. Monitoring

- `tail -f <output>/…-shell.log` — the driver's progress log.
- `ls <output>/runs | wc -l` — run files accumulate per worker during the
  scan; the merge phase then produces `offset.items` / `match.items`.
- Completion: the launch process tree exits on its own; the scan report
  (final counters map) and merge report files exist.
- The counters to watch in every run report: `items`, `rows` (match ≈ 10x
  offset), `items-excluded-intervals` (RedStone), `items-malformed`
  (~0.004% on real data — investigate anything much higher),
  `items-in-holes` (real sync gaps; ~2-3% on dev-2 — rescan after the
  module fills to recover), `read-gbps`, `bytes-read`, `wall-ms`.
- Watch load and memory the first minutes after launch; the pipeline is
  bounded (per-worker run buffers) and RSS should plateau.

## 7. Verifying a machine's output

Follow the W8 pattern (tools referenced in the scanner branch STATUS;
working copies on dev-2 at `/home/hb/arweave-indexer/w8-*.erl`):
1. Bounded sanity span first (a few GiB), independently re-derive rows for
   the span (the `w8_walk.erl` reference walker pattern:
   `ar_bundles:deserialize_header` + direct hashing — no scanner code) and
   assert set-equality against the pipeline's output.
2. Sample ~10 items: decode their offset items and every match row, verify
   each row exists in the output by exact seek, and cross-check 2-3 ids
   against arweave.net GraphQL. Two EXPECTED diffs vs gateways: no parent
   rows for top-level items (L1 containment is a deferred base-layer
   pass), and Ethereum committers are EIP-55 addresses where gateways show
   the sha-of-key form.
3. Exclusion spot-check: items sampled inside exclusion intervals should
   show the RedStone tag signature at ~100%; outside, near 0%.
4. For final containers only: the spec §8 audit (magic/version-3/64 KiB
   pages/DUPSORT|DUPFIXED flags/md_pad/strictly-ascending, offsets under
   the weave size) — an audit tool pattern exists
   (`auditidx.erl`/`idcheck.erl` in the coordination session's scratchpad
   `tool/` directory) built on the branch's `hb_lmdb_page`.

## 8. Collecting and merging fleet output

Ship each machine's `offset.items`/`match.items` (sorted fixed-width rows —
they concatenate-merge trivially). One further k-way pass produces the
network-wide sorted sets; the container build
(`lib_arweave_index_runs:container/3`-style entry on the scanner branch)
writes the spec §1 container through elmdb `put_batch_append` with
`page_size` 65536 — deterministic: identical input rows give sha256-
identical files. Then the §8 audit, then publication (the working
publisher is `arpub.erl` in the coordination session scratchpad `tool/`;
tags per spec §1 + the vocabulary amendment; seeding is resumable via the
saved `.tx` file). Wallet spend needs the maintainer's sign-off each time.

## 9. Known gaps and open items

- **L1 transaction tags**: headers are not in chunk_storage, so tagged L1
  txs (the pre-bundle-era weave) have offset rows but no match rows yet —
  a deliberate deferral; a base-layer header pass covers it later.
- **Limestone**: candidate signature identified (the `sequence-sessions-*`
  tag family, ~2% of items / ~16% of rows in the dense sample) — awaiting
  the maintainer's ruling; today only the RedStone intervals exclude.
- **RocksDB manifest source untested on a real node** (see §4.2).
- **Sub-256 KiB unpacked chunks below the strict-split threshold
  (30,607,159,107,830, ~30.6 TB)** live in a node's RocksDB, not
  chunk_storage — early-weave partitions scanned from chunk files alone
  will miss them; plan those partitions with the RocksDB chunk source.
- Modules synced by a HyperBEAM miner keep old-StoreID index keys after
  renaming to `_unpacked` (§4.2).

## 10. dev-2 specifics (the reference machine)

`hb@dev-2.forward.computer` — 24c/48t EPYC, 375 GB RAM, weave RAID at
`/mnt/arweave-weave` (~6-8 GB/s aggregate). Deployment at
`/home/hb/arweave-indexer`; unpacked module 106 at
`…/storage_modules/storage_module_106_unpacked`; module 105 is still
replica_2_9-packed — LEAVE IT. Do not touch: the ob node (port 8780,
screen `ob-node`), `ob0-hyperbeam.service`, `hyperbeam-agent.service`, the
hb node pid on ports 80/443, node.js on 3001/3002, the 8734→21934
redirect, `~/arweave-suite`, `/mnt/arweave-weave/data/index` (read-only).
Unpack evidence and the band converter: `/home/hb/unpack-scratch`.
