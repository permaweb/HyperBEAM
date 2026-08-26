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
