# Published Arweave indexes — overnight epic status

Started 2026-08-25, unattended overnight mode. Coordinator session running in
worktree `arweave-indexing-implementation-f3ef8c` (branch
`claude/arweave-indexing-implementation-f3ef8c`, based on `edge` @ `14e9f68a6`).

## The brief (verbatim)

> You will need to orchestrate multiple agents on the following overnight,
> unattended mode epic. It is truly exciting work that will make decentralized
> discoverability of billions of content IDs on Arweave possible rapidly, using
> just the smallest machines.
>
> Please look at the associated spec and the build a new branch from edge and
> the `elmdb` that it uses (via a new worktree in `~/src/elmdb`) that implements
> precisely the features that we need in order for this mechanism to work?
> Including: DUPFIXED, DUPSORTED support, `hb_store:write`'s with the
> appropriate format in `~match@1.0`, the new `from` and `limit` keys needed in
> the `hb_store:list` to support using cursors in multiple stores at once -- as
> well as the infrastructure to orchestrate that for `~query@1.0` (maybe
> replacing/re-using `hb_store:match`'s API?), LMDB 1.0 by default with 64kb
> pages, as well as ArLMDB modified to support the DUP* modes that are needed,
> too. `feat/arlmdb` is a robust implementation of ArLMDB 1.0 that we should
> incorporate and add the DUP* features to. There is also another branch in the
> HyperBEAM repo that attempted some of the wider points discussed here, but I
> was thoroughly unimpressed with its work. DO NOT follow its lead -- forge
> your own path, aside the `feat/arlmdb` branch's work.
>
> Instead, using the spec attached and -- **critically** adhering to the
> principles of `CONTRIBUTING.md` and re-reading the suggested reading in
> `AGENTS.md` regularly (no sub-agents may attempt any work without first doing
> this) build a pure, clean, elegant, and simple version of this in the optimal
> hyperBEAM style. Keep each edit as surgical as possible.
>
> Commander's intent #1: Implement the specification as surgically as possible,
> then demonstrate it working against the real mainnet with an index you upload
> to Arweave that covers atleast 10m indexed messages using this format.
> Demonstrate in-practice a node that uses your branch and a match-store with
> your uploaded DB as an `arlmdb` store (upload using
> `~/Documents/hyperbeam-key.json` please), and benchmark the pace at which it
> is able to handle GraphQL queries.
>
> Commander's intent #2: In a separate, parallel thread of work, please
> orchestrate an agent to build an _extremely_ fast Arweave data indexer using
> sequential reads of the dataset over Arweave node storage modules with a
> bounded set of parallel threads at our discretion (more on NVMe machines,
> less on spinning disk). Assume that the data will be unpacked. Please use the
> necessary components of `claude/mining-gossip-production-815e10` (ONLY the
> necessary parts!) carefully cherrypicked, such that you can request
> chunks/ranges of sequential chunks from an unpacked replica if present on
> disk? There is a machine at `hb@dev-2.forward.computer` that you can use to
> test this scanner. It has a deployment of the other branch for testing of
> mining. While you work you should make sure that node is terminated (careful
> of the other live, prod HB nodes on that system!) and _unpacking_ one of the
> storage modules back to its original form, such that you have an example to
> test your indexer against.
>
> Your indexer will need to be executed on ~10 machines with NVMe and spinning
> disk drives in the next week and yield the entire GraphQL index for the
> Arweave network. It is imperative that your indexer:
>
> 1. Generates correct indexes that are both `~query@1.0/arweave` compliant,
> but also generally compliant with the AO-Core execution model (remember the
> required reading in `AGENTS.md` and `CONTRIBUTING.md`!!). The index should be
> as we would expect to see if we had simply
> hb_cache:write(hb_util:ok(`hb_cache:read(ID, Opts)), Opts)` each ID in turn
> with a `match-store` attached. It should be a generic, clean AO-Core cache,
> which also _happens_ to serve the legacy Arweave GraphQL interface well.
> AO-Core primarily, but with Arweave GraphQL support too.
> 2. It might be lightning fast. We should be thinking in terms of how many
> GB/s we can process on a single machine. Do not try to deserialize each ID
> properly, do not try to validate them. It is acceptable not to index the
> bundle TXs themselves, too. Just a clean, **rapidly** built ANS-104 message
> index that efficiently reads chunk after chunk on spinning disks, and
> many(!) times in parallel on NVMe disks, then writes to the LMDB. The LMDB
> inserts will not be sorted to start with, so we should potentially write
> first into a different mode and then have a single pass thereafter to merge
> the LMDBs that we generate into one of the right type. For the write process,
> you should simply make sure that you output a sorted DB at the end, such that
> it can be rapidly merged once all of the partitions have individually been
> scanned. Acceptance criteria: You must be able to demonstrate indexing
> speeds over 4 GB/s on the test node across its NVMe storage modules. Ideally
> closer to 8-10 GB/s (context: our first deployment of this build needs to
> scan the entire network in ~72 hours or so, with the only caveat being that
> we can skip Redstone/Limestone TXIDs). Once commander's intent #1 is also
> ready, you should be able to reformat your rapidly generated LMDB 1.0 DB to
> the appropriate DUPFIXED | DUPSORTED form and show it being used with the
> intent#1 branch to answer GraphQL queries rapidly.
>
> I would suggest using separate worktrees for both strands of work and you
> acting as the coordinator between a significant number of carefully managed
> subagents. Remember to write this prompt in full in your STATUS.md and don't
> let any of the sub-agents forget the CONTRIBUTING.md/AGENTS.md at any time.
>
> Start now in unattended overnight mode, pursuing the stated goals until
> completion. Godspeed!

The format specification is `/Users/sam/Downloads/FORMATarweaveindexes.md`
(copied to `docs/misc/published-arweave-indexes.md` on this branch for agents).

## Ground truth established so far

- The branch to avoid is `impr/match-offsets` (contains "move `elmdb' to LMDB
  1.0, duplicate sets and positioned cursors" and its own arlmdb variant).
  Surveyed for lessons only; not followed.
- `feat/arlmdb` (robust, to incorporate): `src/core/lib/hb_lmdb_page.erl`
  (pure LMDB 1.0 page format) + `src/core/store/hb_store_arlmdb.erl` (locator,
  descent, cursor) + committed fixture + three published DBs, based on edge @
  2d90c17dc. No DUP*/P_LEAF2 support yet.
- `elmdb` checkouts: `~/src/elmdb-rs` (main, currently on `feat/c` — a C NIF
  with LMDB 1.0 + encryption), `~/src/elmdb-rs-write`, `~/src/elmdb-neo`.
  HyperBEAM `edge` pins the Rust NIF: permaweb/elmdb-rs `feat/read-prefix` @
  `faa7623`. New worktree for our work goes at `~/src/elmdb`.
- Edge already ships `src/preloaded/query/`: `dev_query.erl`,
  `dev_query_arweave.erl`, `dev_query_graphql.erl`, `dev_match.erl`.

## Workstreams

| id | what | where | state |
|---|---|---|---|
| W0 | Coordination, STATUS, decisions | this worktree | running |
| W1 | elmdb: LMDB 1.0 default, 64 KiB pages, DUPSORT/DUPFIXED/APPENDDUP/GET_BOTH_RANGE, from/limit cursors | `~/src/elmdb` (branch `feat/dup-sets` off feat/read-prefix @ ae2f1f2) | agent running |
| W2 | `hb_store_lmdb` on new elmdb (page-size 65536 default, real read-only); new `hb_store_lmdb_set` sorted-set store; hb_store doc/cleanups; hb_store_opts recursion | worktree `store-set` (to create) | gated on W1 |
| W3 | arlmdb DUP* read support (P_LEAF2 sub-DB + P_SUBP, positioned cursor, list from/limit) on top of feat/arlmdb | worktree `arlmdb-dup` (branch `feat/arlmdb-dup`) | agent running |
| W4 | `~match@1.0` spec-format rows + leapfrog + multi-store cursor merge; `~query@1.0`/GraphQL paging over it; collapse dev_match/hb_cache duplication | this worktree, after W2+W3 merge | design fixed (decisions/index-architecture.md) |
| W5 | 10M+ item index build (from W6 scan output), upload to Arweave, live node demo + GraphQL benchmark | this worktree | pending |
| W6 | Fast indexer: sequential chunk scanner over unpacked storage modules, >=4 GB/s on dev-2, sorted runs + DUPFIXED merge | worktree `arweave-fast-indexer` (branch `feat/arweave-index-scanner`) | agent running |
| W7 | dev-2: unpack module 106 in place (verify-first), publish highwater cursor for W6 | dev-2 via ssh | agent running |

Recon reports for all agents live at
`<session scratchpad>/recon/*.md` (elmdb, store-query, arlmdb, chunk-storage,
match-offsets-postmortem + copies of the decision docs).

Key recon facts: the avoided branch is `impr/match-offsets` (post-mortem on
file); prior session's scratchpad at
`/private/tmp/claude-501/-Users-sam-src-hyperbeam--claude-worktrees-arweave-lmdb-store-ec474d/7c249981-541c-44bd-bbca-a5b0db3f29e5/scratchpad/`
still holds the pinned LMDB 1.0 checkout, DUPFIXED fixture builders
(mkkeyonly.c = the exact spec container), a dupread.erl prototype and the
arpub.erl publisher. Wallet ggltHF0C… holds 50.23 AR.

## Decisions

Recorded in `decisions/` as they are made.

## dev-2 facts (W7)

- 48-core EPYC 9254, 375 GB RAM (page cache ~366 GB), Ubuntu, uptime 369 d.
- Weave storage: `/mnt/arweave-weave` = md127 RAID0 of sdc+sdd (3.5 TB each,
  non-rotational), 97% full, 282 GB free. Root sda2 has 438 GB free; sdb
  (745 GB) is present, unpartitioned and unmounted — treating as not ours.
- Two storage modules, both `replica.2.9` packed to address `uaV-x-DG…0ePA`:
  `storage_module_105_*` and `_106_*`, 3.3 TB each, layout
  `chunk_storage/<abs-weave-offset>` files of 2,097,176,000 bytes
  (= 8,000 slots x 262,147 = 3-byte prefix + 256 KiB chunk), 1,719 files each.
  Module 105 covers offsets from 377,999,065,088,000 (~378 TB).
- Measured: 4 parallel O_DIRECT sequential readers each sustain 1.6 GB/s
  (≈6.4 GB/s aggregate) on md127. The ≥4 GB/s target is physically reachable.
- Mining test node (`claude/mining-gossip-production-815e10` deployment,
  `/home/hb/arweave-miner`, port 8942, pid 48661): terminated 2026-08-25
  ~00:05 (TERM ignored, KILL applied; pid gone, port free). Left alone: ob
  node port 8780 (screen `ob-node`, 75 d), `ob0-hyperbeam.service`, hb node
  pid 1005486, nginx, node.js on 3001/3002.

## Log

- 2026-08-25 ~00:00 Session start. Read spec, CONTRIBUTING.md, AGENTS.md.
  Identified `impr/match-offsets` as the avoided branch; surveyed feat/arlmdb
  STATUS. Launched recon agents R1-R5 (elmdb, store/query stack, feat/arlmdb,
  mining-gossip chunk storage, match-offsets post-mortem).
- 2026-08-25 ~00:05 dev-2 recon done (facts above); mining test node
  terminated with evidence. Storage modules stable for indexer work.
- 2026-08-25 ~00:30 Launched builders W1 (elmdb), W3 (arlmdb DUP*), W6
  (indexer), W7 (dev-2 unpack) after committing the feat/arlmdb cherry-pick
  (d5d153a7d) and the decisions.
- 2026-08-25 ~01:10 **W1 complete**: elmdb `feat/dup-sets` @ ~/src/elmdb —
  LMDB 1.0 vendored (sha256-verified at openldap bac0ccf), {page_size},
  read_only, no_subdir, dupsort/dupfixed, dup-aware (K,V) overlay,
  put_batch_append (mdb_load pattern), read_dups (GET_BOTH_RANGE,
  from/prefix/limit/direction). All 200 tests green; clean-clone build
  verified. Bench: 10M x 17 B appends at 14.3-14.6 Mrows/s, file
  171,245,568 B = 17.125 B/row (byte-identical across runs). Launched W2
  (hb_store_lmdb page-size/read-only + hb_store_lmdb_set + hb_store_opts
  recursion + two narrow hb_store.erl cleanups) in worktree store-set.
- 2026-08-25 ~01:15 **W3 complete**: feat/arlmdb-dup — hb_lmdb_page admits
  DUPSORT|DUPFIXED (pair only), P_LEAF2 + P_SUBP handled, F_SUBDATA/
  F_DUPDATA references, item/3 + item_seek/3; hb_store_arlmdb dispatches
  container vs path semantics, GET_BOTH_RANGE point lookups + prefix-bounded
  set scans with exact read accounting. 3 committed fixtures (512 B promoted
  depth-3, sub-page, 64 KiB with 3,853 items/leaf = the spec figure); two
  published containers (czOyExU5…7xU4 promoted, ka3n3rUq…xXhQ sub-page),
  0.0059 AR total. Full suite: 1007 passed. Merged into this branch
  (e68d91258). Launched W4 (dev_match spec rows + leapfrog + GraphQL paging)
  in worktree match-query; it merges feat/store-set when W2 lands.
  Known gap logged by W3: no backward set scan in arlmdb yet (DESC paging) —
  W4 instructed to close or bound it.
- 2026-08-25 ~01:50 **W2 complete**, merged (feat/store-set): hb_store_lmdb
  on LMDB 1.0 with `page-size` (default 65536) + real read-only;
  new hb_store_lmdb_set (~205 impl lines; byte-stable files, sha256-identical
  across rebuilds; bulk append 0.9 M items/s; from/limit paging at 22-26k
  pages/s); hb_store_opts recursion into store/stores/index-store/
  local-store; hb_store list docs rewritten + dead scope branch deleted;
  escript code path fix for checkout builds. eunit-all: Failed 5 /
  Passed 3576, A/B-identical to pristine edge (zero regressions).
  Integration facts: this repo remaps `_checkouts` to `src/forge` — the
  elmdb checkout is the untracked symlink `src/forge/elmdb -> ~/src/elmdb`;
  compiles with the checkout strip the elmdb pin from rebar.lock (restore
  before committing; repin once elmdb feat/dup-sets is pushed); 0.9-format
  store dirs (cache-mainnet/lmdb, _build/preloaded-store,
  _build/device-test-store) must be deleted once — they rebuild as v3.
  Merged tree verified here: compile + 24 lmdb/set tests green. W4 messaged
  to merge feat/store-set and continue.
- 2026-08-25 ~01:55 W7 decision logged (decisions/dev2-unpack-banding.md):
  replica_2_9 entropy is sector-interleaved across the whole partition, so
  file-by-file unpack would cost 1024x the RandomX work (~500 h). Chosen:
  8 sector bands (~420 GiB each, ~29 min entropy per band at 40 workers,
  ~4-6 h total), byte-level verified idempotent writes, entropy-only slots
  zeroed so nonzero prefix == real chunk. Verification gate passed 39/39
  (30/30 external arweave.net byte-matches) BEFORE any in-place write.
  UNPACK-CURSOR advances at band boundaries (8 steps).
- 2026-08-26 ~02:05 **W4 complete**, merged (feat/match-query, fc76f5858):
  spec §2 packed codec in hb_store_arweave_offset (varint stays local-KV for
  pending/relative); match row construction collapsed into hb_cache
  (exported helpers; drifted dev_match:store/1 fixed by delegation — core
  cannot call preloaded modules, Forge renames them); new
  `<<"match-store">>`/match_store opt (default []); write_match_items on the
  existing {match, IDs, Msg} op, offset via the local offset index only;
  ~match@1.0/locate = leapfrog both directions, k-way layer merge + dedupe;
  backward walks added to hb_store_lmdb_set (elmdb direction) AND
  hb_store_arlmdb (prev_leaf/rightmost mirror); dev_query_arweave
  index_connection: tags/owner/recipient predicates, height→offset windows,
  both sorts, honest hasNextPage (walk-based, page+1, continues past
  unresolvable offsets), count exact up to query_arweave_max_index_count
  (default 10,000). Full eunit 1024 passed; eunit-all Failed 5/Passed 3600 =
  the A/B baseline. Leapfrog bench (50k rows): ~150k single-predicate
  25-row pages/s; 2.9-4k intersect pages/s.
  Known gap: first full-stack locate over an arlmdb layer awaits W5's
  published spec-format container (committed fixtures are pre-spec items).
- 2026-08-26 ~02:10 dev-2: W7 band 0/8 done in 18.6 min; UNPACK-CURSOR
  382,050,032,156,672 (~450 GB unpacked + verified 30/30). Two real edge
  cases found + handled with cryptographic adjudication: orphan slots
  (packed chunk absent from index AND sync records — gateway byte-match
  before deciphering) and all-zero chunks (encipher to exactly their
  entropy — index leaf adjudicates before zeroing). Full eunit-all running
  here as the integration gate on the merged tree (83 core store/cache
  tests already green).

## Morning follow-ups for Sam

- elmdb `feat/dup-sets` (~/src/elmdb, 5+ commits) is LOCAL. To make fresh
  clones/CI work, push it to permaweb/elmdb-rs and repin rebar.config
  (currently: untracked symlink src/forge/elmdb + old pin in rebar.lock).
  Pushing is external — left for you.
- Same for the four HyperBEAM branches (this coordination branch +
  feat/arlmdb-dup, feat/store-set, feat/match-query merged into it, and
  feat/arweave-index-scanner) — all local, nothing pushed.
- 2026-08-26 ~02:40 Integration gate on the merged coordination branch:
  eunit-all Failed: 5. Passed: 3600. — exactly the A/B-verified pre-existing
  baseline (dev_scheduler legacy-net x4, dev_push x1). Zero regressions with
  W2+W3+W4 all merged.
- 2026-08-26 ~04:55Z Session-limit outage killed the W6/W7 agent loops;
  their detached dev-2 processes ran on unattended. Verified: W7 bands 1-5/8
  done (cursor 384300192366592, ~2.3 TB unpacked), converter alive on band
  6; W6 scan3 = 900 GB in 764 s (1.18 GB/s under entropy load), 26.1M items
  / 246M match rows, amplification fixed (0.93x). Both agents resumed with
  state + the upload-budget decision (decisions/demo-upload-budget.md:
  ~11M-item contiguous slice, ~23 AR, since the full match container would
  cost ~47 of the 50.23 AR held).
- 2026-08-26 ~05:45Z **W7 complete**: module 106 fully unpacked and renamed
  to storage_module_106_unpacked. 8 sector-band passes, 4 h 56 min total
  (fused rxsquared entropy: 99.6 MiB/s/core, 1,978 MiB/s at 40 workers,
  ~29 min/full-module pass). Verification: pre-write gate 39/39 (30/30
  gateway byte-matches), every written slot verified during conversion,
  post-conversion 60/60 whole-module (24/24 gateway). Anomalies resolved:
  1 orphan slot (gateway-adjudicated), 2,234 all-zero chunks (entropy-
  equality adjudicated), index-only entries zeroed. Chunk-index key format
  + old-StoreID spelling documented for the indexer. Box free; evidence in
  /home/hb/unpack-scratch on dev-2.
- 2026-08-26 ~08:0xZ W5 underway: demo containers built by W6 (11,000,000
  offset items, 112,527,983 match rows + 8.02M bundled-in rows; slice bound
  O = 381,948,870,323,729; spec §8 audits pass; deterministic). Offset
  container PUBLISHED and MINED: gXk2EYyhGKG_ZAeyhQZtGko11CHqU7H8Ysc2h7P-6s8
  (232,259,584 B at weave offset 390,058,031,227,126, block 1,987,823).
  Match container (1,915,879,424 B) seeding. Parity fixes applied to
  hb_cache:match_predicates: tag rows derive from commitments original-tags
  when present (adds bundle-format/version rows, drops structured-view
  target/anchor rows) — full test rerun pending upload completion.
  W6b launched: profile + header-scan NIF to close the 4 GB/s criterion
  (W6 measured 2.60 GB/s full-partition, parse-bound on 24 real cores,
  disks ~8 GB/s).
- 2026-08-26 ~13:0xZ **W5 complete** — live demo + benchmark done; full
  report at `<session scratchpad>/demo-report.md`. Ground truth: 7 items
  decoded from the local offset container across the id space, ids
  recomputed from weave bytes (7/7 prefix match), 26/26 predicate rows
  found in the match container. Live nodes (8844 + 8855, separate BEAMs,
  fresh heads, nothing shared but the published containers) answered
  tag / tag+owner / tag∩tag / ids / paged / absent-predicate GraphQL
  queries with exact ground-truth equality; page-2 cursors replayed
  across nodes byte-identically in both sort orders. Two defects found
  by the first benchmark run, fixed + committed: 8f6cf7499 (hb_cache
  match_offset walked remote index layers on every cache write — ~120
  wasted ranged reads/query) and 87cd824be (arlmdb descended the sub-DB
  for over-width prefixes — ~5 wasted reads/lookup); affected suites
  73/73 + dev_query 40/40. Benchmarks (N=30, c=1/c=8): remote steady
  point-id 3.4/19.6 qps at 7 reads/query (spec §4 predicts 5-6; the +2
  is the native-ID KV probe), selective tag 3.7/15.3 qps at exactly 5
  reads, common-tag pages 0.37/1.8 qps at 48 reads (the always-computed
  `count` walk is ~40 of them), intersect 0.11/0.49 qps at 260 reads
  (leapfrog re-descends per seek). Same bytes served locally: point-id
  588 qps @ 8.8 ms — the format is round-trip-bound, not compute-bound.
  Biggest follow-ups, in order: count-on-demand, layer-caching the 5
  constant top pages (7→~2 reads), probe-avoidance for cache-resident
  edges. Demo config note: the arweave store needs `local-store` set
  (decisions/demo-node-local-store.md).
- 2026-08-26 ~08:2xZ **Both containers published and MINED**:
  offset gXk2EYyhGKG_ZAeyhQZtGko11CHqU7H8Ysc2h7P-6s8 (232,259,584 B, block
  1,987,823, weave offset 390,058,031,227,126); match
  9IEf9h9l_w6qW2M05hjMrikRB7MkrkqNMSYkOVUrV8U (1,915,879,424 B, 7,309
  chunks seeded 0 failed, block 1,987,824). Spend ~24.03 AR; wallet holds
  26.20 AR. W5 demo agent launched: live node with both containers as
  arlmdb stores behind writable heads, end-to-end GraphQL proofs, and the
  query-pace benchmark (remote arlmdb vs local set-store A/B).
  eunit-all rerunning after the original-tags parity fix.
- 2026-08-26 ~08:5xZ eunit-all after the original-tags parity fix:
  Failed: 5. Passed: 3600. — the pre-existing baseline exactly. Fix is
  clean; branch tip 31f344022.
- 2026-08-26 ~09:5xZ Second usage-limit outage killed the W5/W6b agent
  loops near their finish lines (W5: all remote benchmark classes done,
  local-ceiling node remaining; W6b: NIF benchmark walls in hand, two
  proofs still running). Both resumed with state notes; their detached
  processes survived the outage.
- 2026-08-26 ~10:4xZ Final gate after the W5 fixes: rerun with full log =
  Failed: 5. Passed: 3600. — the exact A/B baseline (push encoding-change
  x1, scheduler legacy-net x4). The transient 6th in the previous run was a
  network flake. Tree is clean at 9da82fbd6 (+docs).
- 2026-08-26 ~11:0xZ **W6b complete — the last acceptance criterion is
  MET**: per-item parse moved into lib_arweave_index_item NIF (one call per
  item: header walk, RedStone check, sha256, owner addresses, row build;
  Erlang oracle retained for fallback inputs). Full 3.6 TB partition, cold,
  24 workers: 4.11 GB/s in 875 s (W6 Erlang: 2.60/1384 s); 4.69 GB/s at 48
  workers on the dense prefix; warm 4.5+. Full-partition rescan counters
  identical and merged artifacts sha256-identical to the Erlang scanner
  (76,112,123 items / 705,656,183 rows). eunit 998/998. One commit
  56cc7d1d2 on feat/arweave-index-scanner. dev-2 left clean.

## Final state

Both commander intents are demonstrated end-to-end with evidence. All
agents terminated; all demo nodes torn down; dev-2 carries only the
unpacked module 106 + the indexer deployment + kept artifacts
(out-scan5, slice-final, manifests). Full evidence index:
<session scratchpad>/demo-report.md, w5/evidence/, eunit-all-final.log;
dev-2:/home/hb/unpack-scratch/. Morning follow-ups listed above (pushes
and repins are yours).
- 2026-08-26 (evening) Vocabulary ruling applied (Sam): predicates are
  commitment-extension keys — committer (was owner), field-target (was
  recipient), parent (was bundled-in; scanner emits nested-bundle parents
  only, no enrichment dependency), + commitment-device=ans104@1.0 per
  commitment; bundle-format/bundle-version tags filtered (legacy queries
  translate in ~query@1.0/arweave). L1 header row source deferred to a
  later base-layer pass. Node side committed (b0f1c78c0), 83 core tests
  green, eunit-all gate running. W8 agent applying the same vocabulary to
  the scanner (NIF+oracle+vectors), then dev-2 sanity sample + full scan
  relaunch. macOS note: rebuilt elmdb checkout .so needs codesign -f -s -
  (Code Signature Invalid SIGKILL otherwise) — hit again tonight.
- 2026-08-26 ~20:45Z New ruling: RedStone exclusion moves from
  tag-signature detection to weave-offset intervals
  (~/Downloads/redstone-exclusion-intervals.bin: 1,187,180 sorted BE
  <<Start:64,End:64>> pairs, 19 MB, sha256 c4e0145c…72a818f2). Items in
  covered ranges are fully excluded (no offset or match rows) and skipped
  before parsing — expected throughput gain since parse was the ceiling.
  W8 agent resumed to implement, validate boundary semantics empirically,
  cross-check against the tag-based scan8 baseline (214,885,035 excluded
  in partition 106), and relaunch the full scan as out-scan9.
- 2026-08-26 ~21:2xZ Deployment/operations guide committed
  (docs/misc/arweave-indexer-deployment.md, e163fa1fb). scan8 final:
  full 3.6 TB partition in 748.5 s (~4.8 GB/s weave-relative, 1.93 GB/s
  physical after tx-clipping), merge complete; artifacts kept as the
  tag-based baseline. W8 agent died in a credits outage mid-interval-work
  (Erlang side + new lib_arweave_index_exclude.erl + NIF edit already in
  its tree); resumed to finish build/tests/validation and launch the
  interval-based full rescan (out-scan9).
- 2026-08-26 ~21:5xZ **Branches consolidated**: feat/arweave-index-scanner
  merged into this branch (9a74db9e1; one trivial conflict in
  build-preloaded-store.escript resolved to the scanner side). One
  HyperBEAM branch now carries both intents; elmdb feat/dup-sets remains
  its own repo branch. Cross-branch modules verified together (55 tests).
  When W8s in-flight interval-exclusion commit lands on
  feat/arweave-index-scanner, fold it in with:
  git merge feat/arweave-index-scanner
  PRs are Sams; nothing pushed.
