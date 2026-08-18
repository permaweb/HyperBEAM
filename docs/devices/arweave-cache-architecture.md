# Arweave state and cache architecture

HyperBEAM's Arweave stack contains durable consensus state, derived indexes,
computed process results, and temporary validation work. Calling all four a
"cache" hides the most important property of the design: they have different
owners and different authority.

The durable account graph and block index are not accelerations over some more
authoritative in-memory value. They are the locally validated state. Their AO
links and native Arweave roots are both cryptographic facts. By contrast, the
scheduler index, process-result cache, and VDF timeline can be rebuilt from a
more authoritative input.

This document describes those boundaries, the content-addressed graphs inside
them, their stable aliases, and their behaviour across updates, forks,
interrupted writes, and restarts. The public consensus API and validation rules
are documented under [`~arweave@2.9`](arweave-at-2-9.md).

## The four kinds of state

| Kind | Authority | Durable | Examples |
|---|---|---:|---|
| Consensus state | Locally validated Arweave chain | Yes | Blocks, transactions, placements, accounts, block index, histories |
| Derived index | A selected, settled consensus snapshot | Yes | Arweave scheduler targets and assignments |
| Computed result | A process and its schedule | Yes | `~process@1.0` results by slot |
| Validation work | A currently validated VDF epoch | No | Ahead-of-tip outputs and checkpoint buffers |

The ownership rule is simple:

1. Consensus may depend only on validated consensus state and explicit
   operator trust roots.
2. A derived index may read consensus through its public AO device, but
   consensus never reads the derived index.
3. A process result may depend on a schedule, but neither the scheduler nor
   consensus treats that result as authoritative input.
4. Temporary work may save computation, but its absence or loss may never
   change a validation result.

## Storage domains

```text
                         untrusted peer bytes
                                  |
                    decode transport in application code
                                  |
                                  v
                  +----------------------------------+
                  | Arweave consensus validation    |
                  +----------------------------------+
                                  |
                                  v
        +----------------------------------------------------+
        | main store (production: LMDB)                      |
        |                                                    |
        | validated block DAG and selected-tip alias         |
        | committed transactions and placement messages      |
        | AO-linked account Patricia graphs                   |
        | AO-linked ordered block indexes                     |
        | reward and block-time history tails                 |
        | ordinary AO process results                         |
        +----------------------------------------------------+
                    | public AO reads only
                    | tip / validated / placement
                    v
        +----------------------------------------------------+
        | scheduler store (separate LMDB)                    |
        |                                                    |
        | copied data-free transaction headers               |
        | fork-bound target records                           |
        | global and per-process frontiers                    |
        | dense process assignments                           |
        +----------------------------------------------------+

        validated VDF anchor ----> bounded BEAM-heap timeline
                                    (no durable writes)

        transaction placement ---> optional compact offset store
                                    (retrieval index, not consensus)
```

The scheduler store must not share a physical `{store-module, name}` identity
with the main store. A same-store fallback would make ownership accidental and
would let tests pass while production links resolve through the wrong domain.

The preloaded-device and loaded-device stores belong to HyperBEAM's device
loader. They are not part of Arweave consensus or scheduling state.

## Content objects, links, and aliases

The implementation uses three identifiers for different jobs:

| Construct | Meaning |
|---|---|
| AO content ID | The identity of an immutable message or binary |
| Arweave native root or ID | A protocol commitment such as a SHA-384 wallet root or block hash |
| Store alias | A stable name selecting the object currently in a role |

An AO content ID authenticates the complete AO message, including structural
metadata and its links. A native Arweave commitment authenticates exactly the
preimage the Arweave protocol defines. These are deliberately not conflated.

For example, an Arweave account-tree branch root is a 48-byte SHA-384 value. It
does not commit to the AO field names, child-link encodings, or locally useful
routing metadata. The ordinary 32-byte AO ID authenticates those details while
the branch's `root` field records and verifies the native commitment. Widening
the global AO ID definition to admit Arweave roots would erase this distinction
and change unrelated devices.

Aliases answer a mutable question: which immutable object currently has a
role? Examples include:

- the selected validated tip;
- the latest known placement for a transaction;
- the published scheduler frontier; and
- the computed result occupying a process slot.

Changing an alias does not mutate either its old or new target.

### How `hb_cache` stores a graph

`hb_cache` writes Type-Annotated Binary Messages through `hb_store`:

1. Large binary content is deduplicated under content-derived storage keys.
2. A message ID names a group containing immediate fields and links to nested
   objects.
3. Commitment IDs may link to the same uncommitted message content.

A read is therefore not guaranteed to return a fully materialized Erlang map.
Nested values may be lazy `{link, ID, Options}` tuples. Code that operates on
AO messages uses `hb_maps` and loads only the fields required by the rule.

The `priv` section is not persisted. It may carry a call-local acceleration,
such as a materialized Patricia skeleton or history window, but correctness
must survive its loss on every cache round trip and process restart.

### Link authentication

A structural link is accepted only when it resolves to a canonical AO content
ID and the loaded object's discard-mode ID equals that target.

Cached parent messages commonly carry a lazy field link whose first target is
the parent's field hashpath. One `hb_cache:read/2` resolves that field path to
the nested object's canonical ID. An eager link already carries that ID. Both
forms are authenticated before their message is interpreted.

Raw store paths such as `data/<hash>` are not AO content IDs. Treating them as
message fields would expose storage implementation details, make graphs
non-portable between stores, and let a caller select arbitrary stored bytes.

## Match indexes are not structural persistence

Generic match indexes are valuable for user messages and transactions because
they support queries by owner, target, or tag. They are not useful for internal
consensus nodes.

Writing every Patricia node, block-index page, component state, and containing
block through the generic match index would multiply store paths without
creating a meaningful query surface. Internal structural writes therefore use
`match-index => false`. The containing block carries explicit component links,
so publication does not recursively rematerialize those graphs.

This suppression is call-local. It must not disable normal transaction
indexing or change the operator's store configuration globally.

## The validated block DAG

The main store is the only owner of canonical Arweave chain state. Peer I/O,
validation, fork choice, block publication, and placements terminate there.

### Stable consensus names

| Path or identifier | Value |
|---|---|
| `<64-character-indep-hash>` | Locally validated block message |
| `~arweave@2.9/tip` | Selected validated block |
| `~arweave@2.9/placements/<txid>` | Latest known placement |
| `~arweave@2.9/settled/<indep-hash>` | Completed settlement marker |
| `~arweave@2.9/accounts-anchor` | Trusted checkpoint account state |
| `<43-character-txid>` | Committed `tx@1.0` transaction |

The bare block hash is a validated-publication alias, not an AO content ID.
`arweave-block@2.9/previous` reads the scalar `previous-block` and resolves it
through the consensus namespace explicitly.

### Block graph

```text
validated block
|
+-- previous-block          scalar Arweave block hash
+-- txs                     ordered transaction IDs from the header
+-- transactions            ordered links to placement messages
+-- block-index             AO link to the index state at this block
+-- accounts                AO link to the account state at this block
+-- reward-history          AO link to newest reward-history entry
+-- block-time-history      AO link to newest block-time entry
+-- validation/checks       checks establishing this materialization
`-- canonical header fields, proofs, roots, VDF data, difficulty, ...
```

The block DAG owns history. Account and block-index states do not need their
own predecessor fields or rollback APIs: the parent block already links the
parent versions. A fork shares every block and component below its branch point
and carries different component links above it.

### Publication boundary

A block is published in dependency order:

1. Write every committed transaction and read it back by transaction ID.
2. Write each placement, publish its placement alias, and write an offset when
   a configured Arweave offset store exists.
3. Read back and verify the already-persisted block component states.
4. Replace component maps in the block with explicit AO links.
5. Write the block message without generic match indexing.
6. Link it under its bare independent hash and read that alias back.
7. Move the selected-tip alias only after fork choice selects it.

The block-hash alias is the profile's completion marker. These steps are a
logical publication protocol, not a store-wide transaction. An interruption
may leave immutable, unaliased content; retrying writes the same IDs and
finishes the publication. It must never expose a hash alias whose dependencies
are missing.

## Transactions, placements, and offsets

A layer-one transaction has one durable message form: the committed
`tx@1.0` message consensus validated. Its signed transaction ID resolves to
that content, and normal match indexes may also make it queryable by semantic
fields.

A placement records one occurrence of a transaction:

| Field | Meaning |
|---|---|
| `id` | Transaction ID |
| `block` | Containing block hash |
| `height` | Containing block height |
| `position` | Zero-based position in block transaction order |
| `data-root` | Transaction data root |
| `data-size` | Transaction data size |
| `start-offset` | First weave byte occupied by its data |
| `transaction` | Link to the committed transaction |

The block retains its ordered placement messages forever. The mutable
placement alias is only the latest placement written for that transaction. A
consumer requiring the selected placement compares `placement/block` with the
hash at `placement/height` in the selected tip's block index.

The optional offset store answers a narrower retrieval question: where bytes
may be fetched from the weave. It is not consensus state and does not replace
placements.

## Account Patricia graph

The account state is a persistent AO rendering of Arweave's Merkle-Patricia
tree. It does not store first-byte buckets, transport pages, Erlang terms, or a
second version history.

```text
account state
|
+-- device = arweave-wallets@2.9
+-- root   = native Arweave SHA-384 wallet root
`-- tree   = AO link to immutable Patricia root node
              |
              +-- page: bounded, sorted accounts for one complete subtree
              `-- branch: optional terminal account, child metadata, one AO link
                    |
                    `-- children: bounded AO-linked radix vector
                          |
                          `-- ordinary AO links to child nodes
```

The two commitments have separate responsibilities:

- the node's native SHA-384 `root` is recomputed using Arweave's exact account
  and branch hashing rules;
- the node's AO content ID authenticates its complete topology, routing fields,
  semantic fields, and child content references.

Every node carries an authenticated account count. Counts are not part of
Arweave's native commitment, but they determine one canonical AO
representation: a complete subtree of at most 256 accounts is one compact
page; a larger subtree is its native Patricia branch. Crossing the bound
therefore splits deterministically, and falling back below it merges
deterministically. The 256-account bound keeps leaves compact while preserving
the native Patricia fanout and sparse-transition cost.

A page body is the canonical ordered encoding of full account keys and account
values. Arweave's historical state includes an empty key and shorter keys as
well as the usual 32-byte addresses, so key length is part of the committed
entry rather than a device-level restriction. The page's count, native root,
ordering, byte encoding, and relationship to the traversal prefix are all
checked on load. The prefix is not stored in the page, so the same content can
remain shared when Patricia path compression elides a transparent internal node.

A branch body contains only the ordered native metadata for each child: prefix,
SHA-384 root, and account count. When the branch's compressed key is itself an
account key, the branch also carries that terminal account as an ordinary AO
submessage. Its `children` field is an ordinary AO link to a persistent radix-32
vector. A small vector is one numbered leaf of ordinary child-node links. A
larger vector is an index of ordinary links to bounded width-32 vector leaves.
Consequently generic AO traversal, loading, and copying can discover every
dependency without understanding the wallet device, while no message has more
than 32 link fields. When the ordered child prefixes are unchanged, updating one
native child path-copies only its vector leaf, the vector index when present,
and the Patricia branch. A prefix-set change rebuilds that branch's bounded
vector, because it changes the vector's canonical order.

No additional commitment device is necessary. The ordinary AO content ID
commits the exact node bytes and child references; the recorded Arweave root is
independently recomputed to commit the consensus semantics. Neither identifier
is reinterpreted as the other.

No native-root-to-node pseudo-index is required. The parent link is the graph.
The block's `accounts` link names the version used for that block.

### Sparse transitions

Block application touches a small set of accounts. The device hydrates only
the Patricia paths and bounded pages containing those addresses. Untouched
children remain authenticated `{root, count, vector locator}` stubs from the
prior validated state; their account bodies are not loaded.

The in-memory skeleton privately remembers each hydrated native-node reference,
its vector position, and its authenticated child-vector link. Arweave's
Patricia `UpdateMap` identifies nodes whose hashes changed. Persistence proceeds
bottom-up:

1. reuse the remembered AO content reference for an unchanged node;
2. persist a changed node after its changed children;
3. elide any non-root, no-value node with one child;
4. verify each persisted native root and AO identity on readback; and
5. return a history-free state linking the new root node.

This is path-copy persistence. A sparse update creates work proportional to
the changed Patricia paths, not to the total number of accounts. Restoring the
same semantic tree produces the same root-node and state IDs because neither
contains ancestry metadata.

### Reads and verification

`get` authenticates each node on the traversed path, including its AO identity,
native root, child ordering, count, and prefix relationship. It does not load
unrelated branches.

`verify` recursively authenticates the whole graph, rebuilds the vendored
Patricia tree, recomputes the native wallet root, and checks every subtree
count and the canonical page frontier. A missing child, mismatched root,
invalid prefix, non-canonical account or body, forged count, oversized page,
unnecessary branch, or cross-store dependency fails closed.

Transport decoding remains outside the device. `/wallet_list` ETF pages are
parsed and bounded by the consensus application, converted to generic account
messages, then folded through public `insert` and `finalize` calls.

## Block-index graph

The block index stores one semantic triplet per height:

```text
{indep-hash, weave-size, tx-root}
```

Arweave's `hash-list-merkle` is a linear SHA-384 prefix accumulator over those
entries. It is not a Merkle search tree and cannot name or authenticate local
search nodes. The implementation therefore keeps the native accumulator and
the AO search topology as separate commitments.

```text
block-index state
|
+-- device = arweave-block-index@2.9
+-- length = semantic entry count
+-- root   = native Arweave prefix accumulator
+-- completed -> persistent ordered tree
|                  +-- bounded branch metadata
|                  `-- immutable dense leaf pages
`-- tail      -> one bounded partial leaf
```

Each dense leaf is one compact AO object, and each branch is a separate AO
message. A leaf records its starting height/weave/root, ending weave/root, and
up to 128 canonical 89-byte entries. Branches carry up to 32 ordinary child
links plus bounded count and maximum-weave metadata for selection.

The completed tree contains only full, immutable leaves. Normal live appends
replace the bounded tail and the small state message. When the tail becomes a
full leaf, it joins the completed tree and only the rightmost branch path is
copied. Thus every block rewrites bounded state, with an occasional bounded
rightmost branch path.

`at` descends by subtree entry counts. `bounds` descends by subtree maximum
weave sizes and binary-searches one leaf for the first entry whose weave size
is strictly above the requested offset. Forks reuse completed leaves and every
unchanged branch by AO ID.

`verify` walks leaves in order and recomputes the native accumulator. It also
validates all routing metadata, leaf padding, height/weave/root continuity,
branch fanout, and the unique bottom-up topology. A graph cannot pass root
verification while routing `at` or `bounds` somewhere else.

Peer `/block_index2` bytes are decoded by consensus application code into an
ordered AO message. The device exposes the semantic `append`, `at`, `bounds`,
`root`, and `verify` operations over that committed structure.

## Reward and block-time histories

The two protocol histories are persistent linked lists of immutable semantic
entries. Each new head contains its decoded value, capped length, and an AO
link to the previous entry.

This is the right use of a predecessor link: the history itself is a temporal
sequence whose native commitment depends on its ordered window. A parent block
and every agreeing fork share the same tail.

The windows are large, so a head may carry a fully materialized value list in
its private section together with the exact store configuration that supplied
it. This is an in-message, non-persistent acceleration. A cold read walks links
once; a sequential extension prepends one value. A different store cannot use
materialization from a store whose dependencies it cannot resolve.

## Derived scheduler store

The Arweave scheduler consumes consensus; it does not duplicate it. It captures
one selected tip and the block index carried by that tip, reads exact validated
blocks through public `~arweave@2.9` paths, and derives only schedule data.

The scheduler store contains:

| Path or ID | Value |
|---|---|
| `~arweave-scheduler@1.0/sync/global` | Published global frontier |
| `~arweave-scheduler@1.0/sync/process/<process>` | Per-process frontier |
| `~arweave-scheduler@1.0/targets/<address>/<ordinate>` | Fork-bound target record |
| `~arweave-scheduler@1.0/assignments/<process>/<slot>` | Dense assignment |
| `<txid>` | Fully copied data-free transaction header |

It stores no Arweave blocks and has no gateway fetch layer.

### Snapshot and frontier

One indexing pass captures a tip hash, tip height, and that tip's block-index
link. It indexes only through `tip-height - CHECKPOINT_DEPTH`. Every height is
resolved against the captured index and fetched through
`validated&block=<captured-hash>`.

Target records contain the selected block hash as well as the transaction. This
matters if execution stops after target publication and the chain reorganizes
before retry: materialization rejects an orphan target whose block hash differs
from the durable snapshot's index.

Headers and targets are fully written and read back before the global frontier
alias advances. Assignments are fully written and read back before the
per-process frontier advances. Consequently every slot below `next-slot` must
exist; a schedule read treats a hole as corruption rather than a successful
truncated result.

### Cross-store copying

A transaction read from consensus may contain lazy links into the main store.
The scheduler fully materializes it under main-store options, writes the needed
data-free form under scheduler-store options, then loads it again from the
scheduler store. Carrying a link tuple with an old store hint is not a copy:
caller options deliberately override those hints.

## Process-result cache

`~process@1.0` stores computed results in the main store's process-local scope:

```text
computed/<process>/slot/<slot>   -> computed state
computed/<process>/<message-id>  -> the same state
computed/<process>/latest        -> freshness marker
```

The freshness marker is a content-addressed `{slot, timestamp}` message. It is
published only after the result and its slot/message aliases are readable.
Failed execution therefore cannot make an old result appear fresh.

If a marker is missing or names an unreadable slot, the cache scans descending
numeric slot aliases for the newest readable result. There is no independent
filesystem or process-dictionary state.

## VDF timeline

The VDF timeline is deliberately not an `hb_store` graph. It is a bounded BEAM
process holding ahead-of-tip values for one exact anchor, seed, difficulty, and
entropy-reset interval:

```text
#{
  anchor => ValidatedOutput,
  at => ValidatedStep,
  seed => EpochSeed,
  difficulty => Difficulty,
  reset => NextEntropyReset,
  steps => #{ Step => {Output, CheckpointBuffer} }
}
```

At most 4,096 steps are retained. Each entry owns a 32-byte output and a
768-byte checkpoint buffer, for 3,276,800 bytes of raw bounded payload before
BEAM bookkeeping.

The timeline advances only after block validation. A caller receives work only
for an exact seed/difficulty/step match. Missing, late, malformed, or mismatched
work falls back to native verification. Restarting loses performance work, not
consensus state.

## Forks, interruptions, and restart

Immutable objects plus last-published aliases replace repair journals:

| Interruption | Visible durable result | Recovery |
|---|---|---|
| Account/block-index node before parent state | Unreferenced immutable node | Retry reuses its ID |
| Transaction before placement | Committed transaction without that occurrence | Block publication retries |
| Placement before block hash | Placement object or mutable alias | Block remains unpublished |
| Block content before hash alias | Unaliased block content | Retry links the same content |
| Published block before tip move | Complete non-selected branch | Fork choice may select it later |
| Scheduler header before targets | Harmless copied header | Scheduler retry reuses it |
| Scheduler targets before global frontier | Invisible to old frontier and fork-bound | Retry verifies or filters them |
| Assignment before process frontier | Harmless future slot | Retry rewrites and advances last |
| Process result before freshness marker | Readable by explicit slot only | Later success publishes freshness |
| VDF timeline loss | No durable change | Full verification runs while it rebuilds |

A restart discards every private memo and the VDF timeline. All durable public
operations must still return the same roots, entries, accounts, schedules, and
process states using only LMDB content and links.

## Growth model

The structures avoid per-object filesystem files. A production LMDB domain
normally has a fixed, small set of database files even with millions of keys.

| Structure | Incremental shape |
|---|---|
| Validated block | One header/state message plus explicit component and placement links |
| Transaction | One committed `tx@1.0` object, deduplicated by commitments |
| Placement | One small occurrence message and latest-placement alias |
| Account graph | Changed Patricia paths and pages; bounded child-vector paths, or one rebuilt branch vector when its prefix set changes |
| Block index | One semantic entry in the bounded tail; occasional right-edge branch copy |
| Reward history | One immutable head sharing its tail |
| Block-time history | One immutable head when the protocol adds an entry |
| Scheduler | Targeted data-free headers, fork-bound targets, and materialized assignments |
| Process cache | Application-dependent computed states by retained slot |
| VDF timeline | Fixed maximum of 4,096 output/checkpoint pairs |

LMDB `capacity` is an address-space ceiling, not current disk use. Storage
evaluation should count logical keys and values as well as the database's
physical high-water size. In particular, a structurally elegant graph is not
acceptable if generic cache expansion turns one semantic update into thousands
of unrelated pseudo-index paths.

## Invariants

1. A bare Arweave block hash names only a locally validated publication.
2. The selected tip moves only after the selected block and dependencies are
   readable.
3. AO IDs authenticate storage topology; Arweave roots authenticate Arweave
   semantic preimages. Neither substitutes for the other.
4. Account state is a persistent Patricia graph, not account buckets or peer
   pages.
5. Block-index state separates the native linear accumulator from its ordered
   AO search topology.
6. The block DAG, not duplicated component ancestry, owns historical versions.
7. Internal consensus nodes do not populate generic match indexes.
8. Raw store paths are never accepted as graph dependencies.
9. History materialization is private, non-persistent, and store-scoped.
10. Scheduler and main store identities do not overlap.
11. Scheduler targets are bound to the block hash that authenticated them.
12. Global and process frontiers publish after everything they expose.
13. Process freshness advances only after a successful readable result.
14. VDF lookahead can save work but can never make missing validation pass.
