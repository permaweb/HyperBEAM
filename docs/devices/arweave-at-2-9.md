# `~arweave@2.9`

`~arweave@2.9` adds pull-based, post-2.9 Arweave validation to a HyperBEAM
node. It bootstraps from a trusted state checkpoint, fetches new blocks from
peers, validates them locally, and stores each validated block in `hb_cache`
as an `arweave-block@2.9` message.

The existing gateway keys (`tx`, `chunk`, `raw`, `price`, `block`, `current`)
remain peer-backed. Use `validated` when the answer must have been checked by
this node.

## The consensus cache

A validated block is one message, filed under its own Arweave block hash. It
carries the block header exactly as the protocol defines it, and alongside it
the state a header does not express, as AO-Core links:

| Key | Meaning |
|---|---|
| `previous-block` | Scalar hash of the block below this one |
| `transactions` | One placement per transaction, in the block's order |
| `block-index` | The block index as it stood after this block |
| `accounts` | The account tree as it stood after this block |
| `reward-history` | The newest entry of the reward history |
| `block-time-history` | The newest entry of the block-time history |
| `validation` | The checks that established this block |

`~arweave@2.9/tip` points at the selected head. An Arweave block hash is 48
bytes, not an AO content identifier, so `previous-block` remains a scalar.
`arweave-block@2.9/previous` resolves that hash explicitly through the validated
consensus namespace. This also lets `backfill` materialise a parent after a
child that already names it.

There is no separate chain-state message and no duplicated recent-block
window. Transaction-anchor rules walk `previous-block` hashes from the block
being extended and stop at the oldest locally held ancestor.

The block DAG owns component history. An account tree and block index carry no
second predecessor or rollback chain: the parent block already links their
parent versions. Reward and block-time histories are different because the
semantic value being committed is itself an ordered history window. They are
persistent linked lists of immutable entries, so an extension writes one head
and every agreeing fork shares the same tail.

A block's presence under its hash is the completion marker. Publication writes
and reads back transactions and placements; reads back and verifies the
already-persisted component states; replaces component maps with explicit AO
links; writes the block without generic structural match indexes; and links the
block hash last. A block that reads back through that alias has all dependencies
required by its validation profile. A pass
interrupted earlier may leave unaliased content-addressed objects, which a
retry writes at the same identifiers.

Blocks fetched from a gateway by `~arweave@2.9/block` are a different thing and
live elsewhere, under `~arweave@2.9/block/hash/<hash>` and
`~arweave@2.9/block/height/<n>`. Nothing checks them, so they never occupy the
bare hash, which only validated blocks are published under. That separation is
what `validated` rests on, and `~query@1.0` reads both: a block identifier it is
given resolves to whichever of the two the node holds.

### Component graphs

The account state is an AO-linked rendering of Arweave's Merkle-Patricia tree.
Every immutable node carries the exact native SHA-384 root Arweave computes.
Small complete subtrees are canonical bounded pages; larger native branches
carry canonical child prefix/root/count metadata and link to a bounded
persistent radix vector of ordinary AO child links. This keeps high-fanout
branches generically traversable without placing hundreds of links in one
message. A branch whose compressed key is also an account key carries that
terminal account directly, which preserves Arweave's empty and short historical
keys as well as ordinary 32-byte addresses. Sparse updates persist only the
Patricia paths, vector paths, and pages the transition invalidated. The state
stores neither peer wallet-list pages nor first-byte account buckets.

The block index separates two different structures. Its `root` is Arweave's
linear SHA-384 prefix accumulator over
`{indep-hash, weave-size, tx-root}` entries. Its AO links form a persistent
ordered search tree used by height and weave-offset lookups. Completed bounded
leaves are immutable and shared; one bounded tail absorbs ordinary live
appends, so each append changes only bounded state.

AO IDs authenticate the complete storage topology. Native roots authenticate
the protocol preimages. Arweave's 48-byte roots are not treated as global AO
IDs, and internal graph nodes do not populate generic match indexes. See
[Arweave state and cache architecture](arweave-cache-architecture.md) for the
full ownership and persistence model.

## Transactions and placements

A layer-one transaction has one message form: the committed `tx@1.0` message.
The consensus checks convert it to a `#tx{}` record, and publication writes it
unchanged -- so the generic query device finds it by owner, target or tag like
any other message, `hb_cache` links it under its Arweave transaction
identifier, and what a check ran over is what a query returns.

The commitment holds the owner, the signature, its type and the transaction
identifier; the fields keep the codec's own spelling, `anchor` for Arweave's
`last_tx` and `data_root`/`data_size` underscored; the tags are the message's
keys, with their exact bytes and case preserved in the commitment.

A **placement** records where a source transaction occurs in the chain: the
block that included it, its position in that block, its data root and size, and
the offset its bytes begin at in the weave. The current placement of a
transaction is at `~arweave@2.9/placements/<txid>`, and
`GET /~arweave@2.9/placement&tx=<txid>` answers with it.

A placement is not a claim that the block carrying it is still selected. A
consumer that needs one checks the placement's `block` against the hash the
tip's block index records at its `height`. A reorganisation replaces the alias
and deletes nothing: the block that carried the old placement still links it.

Placements are a different thing from byte offsets and are stored separately.
`hb_store_arweave_offset` answers "where can these bytes be fetched" for every
data item on the weave, in as few bytes as its encoding allows. Consensus
consumers read placements through `~arweave@2.9`; byte retrieval remains a
store concern.

## Settled transactions

When a block passes beyond Arweave's allowed reorganisation depth it can no
longer be reorganised away, and each of its transactions is announced on the
`arweave-settled-transaction` hook. The hook request is the placement, which
carries both the weave location and a link to the transaction, so a handler
needs no second lookup.

This is how archive work (ANS-104 discovery, bundle indexing, data fetching)
is attached without coupling it to consensus validation. Configure handlers in
the node message:

```json
{ "on": { "arweave-settled-transaction": { "device": "copycat@1.0" } } }
```

Announcement is idempotent per block hash and transaction: a block is marked at
`~arweave@2.9/settled/<indep-hash>` only once all of its transactions have been
announced, and a failure is retried on the next pass. Nothing in settlement
writes to the chain, so it can never move the consensus tip backward.

## Validation boundary

Bootstrap is a trusted-state join, not historical replay. The checkpoint
commits to the block index and account root, but HyperBEAM does not replay the
transitions that produced the checkpoint state. An explicit checkpoint hash
therefore trusts the complete state carried at that point. This is the same
fundamental join boundary used by an Arweave node loading state from trusted
peers. HyperBEAM binds the newest reward-history element to the checkpoint and
its parent; the standard Arweave join checks a 50-element recent window. Both
trust the older history tail supplied at join.

After bootstrap, every accepted block is checked against the locally stored
parent. The checks are grouped under eleven stable names, and every stored
block records the ones that established it under `validation/checks`:

| Check | Covers |
|---|---|
| `linkage` | Parent, height, and declared previous cumulative difficulty |
| `identity` | Block signature, and the identifier as the hash of it |
| `fields` | Every deterministic field the parent header determines |
| `block-index` | The block-index root extends the parent's with its entry |
| `transactions` | Transaction signatures, ordering, root, and weave arithmetic |
| `pow` | Proof of work, at the difficulty the solution type requires |
| `poa` | Both proofs of access, against recomputed recall bytes |
| `vdf` | The complete VDF interval and its checkpoints |
| `accounts` | Transaction admission, and the exact signed `wallet-list` root |
| `reward-history` | The committed `reward-history-hash` |
| `block-time-history` | The committed `block-time-history-hash`, and the VDF retarget |

Account validation is required by default. A bootstrap fails if peers cannot
serve the selected checkpoint's own wallet tree; it does not substitute a tree
from another height.

A checkpoint block records `identity`, `block-index`, `reward-history`,
`block-time-history` and `accounts`: at a trusted-state join those are the
checks that establish each carried component against the hash the checkpoint's
own header commits to it under. Everything a parent is needed for is absent
from the list, which is the join's trust boundary stated in the record.

### Selective verification

`apply`, `validate` and `materialize` take an optional `profile`, or an
explicit `verify` list of the check names above. The default everywhere is
`full`.

| Profile | Checks |
|---|---|
| `full` | All eleven |
| `archive` | `linkage`, `identity`, `block-index`, `transactions` |
| `headers` | `identity` |

A name the device does not know, and a set that omits a check another reads
from -- `accounts` without `transactions`, `poa` without `pow` -- are both
refused. Silently narrowing either would produce a block whose
`validation/checks` was accurate and whose validation was weaker than the
caller asked for.

## Historical materialisation

`GET /~arweave@2.9/backfill&from=<height>` materialises blocks below the ones
the node holds, downwards, checked against the block index its selected tip
carries. That index's root was committed to by a header this node validated, so
the hash, weave size and transaction root it records at every height from
genesis are as trustworthy as the tip -- and a serving peer cannot substitute
another block or another transaction set, however far below the join the
request reaches.

For each height, the device resolves the expected hash from the index, fetches
the header as untrusted bytes, recomputes its identity, fetches and verifies
each transaction, recomputes the transaction root and weave placement, and
publishes the block only once every requested check and index write has
succeeded.

| Parameter | Default | Meaning |
|---|---:|---|
| `from` | required | The highest height to materialise |
| `count` | `arweave-backfill-batch` | How many heights to walk down, clamped to that ceiling |
| `profile` | `archive` | `archive` or `headers` |
| `verify` | -- | An explicit check list instead of a profile |

There is no frontier to resume from, by design: the index says what every
height should hold, a block already published is skipped, and a pass that stops
early is repeated by re-issuing the same request. Asking `backfill` for a check
that reads state below the join (the VDF chain, the proofs, the account
transition, the parent-derived fields) is refused rather than quietly omitted.

A materialised block retains every semantic field of the Arweave header,
proofs included. Application code can therefore reserialize it with the
Arweave wire codec without making transport encoding part of the device. That
is roughly half a mebibyte per block; an operator backfilling a long range
should size the store for it.

## Devices

| Device | Responsibility |
|---|---|
| `~arweave@2.9` | Peer I/O, bootstrap, sync, fork choice, local lookups |
| `~arweave-block@2.9` | Complete block state transition and block hashes |
| `~arweave-block-index@2.9` | Persistent ordered weave index and native accumulator |
| `~arweave-merkle@2.9` | Offset-indexed Merkle path validation |
| `~arweave-spora@2.9` | Recall ranges, RandomX packing, and proof of access |
| `~arweave-history@2.9` | The two carried histories, as persistent linked lists |
| `~arweave-tx@2.9` | Transaction admission and weave-root rules |
| `~arweave-mining@2.9` | Recall range search, solutions, and block production |
| `~arweave-storage@2.9` | Storage modules: chunks on disk, packing, and the sync record |
| `~arweave-vdf@2.9` | Nonce-limiter chain, seeds, and difficulty |
| `~arweave-wallets@2.9` | AO-linked Arweave Patricia graph and sparse updates |

## Bootstrap and sync

Configure a persistent store, block sources in `arweave-untrusted-peers`, and
one bootstrap trust root:

- `arweave-checkpoint-block`: an explicit block hash; or
- `arweave-trusted-peers`: peers that must agree on a shared ancestor near the
  tip.

Then call `GET /~arweave@2.9/bootstrap` once. Bootstrap verifies the selected
block's identity, reconstructs and verifies its block index, fetches the
checkpoint histories, verifies the checkpoint's account tree against its signed
root, and materialises the blocks of the transaction-anchor window below it as
headers so that the chain reaches back far enough for the anchor rules to read.

Bootstrap against mainnet takes minutes -- five, on a well-connected host --
and HTTP is the usual way to ask for it. Cowboy closes an idle connection after
`idle_timeout`, which defaults to 300 000 ms, and a request that outlasts it
returns an empty body while the bootstrap goes on and commits. Raise
`idle_timeout` in the node message before bootstrapping over HTTP, or read the
result from a second call: a node that has a chain answers `already-bootstrapped`
rather than starting another.

Schedule `GET /~arweave@2.9/sync` with `~cron@1.0` after bootstrap. A typical
interval is 30 seconds. Sync is idempotent: each validated block is published
under its own hash, and the tip moves only after that block has been written.

`GET /~arweave@2.9/tip` returns the selected local tip. Fork choice follows
Arweave's cumulative-difficulty and checkpoint-depth rules.

`GET /~arweave@2.9/validated&block=<indep-hash>` returns a block produced by
this node, or `not-validated`. It never contacts a peer. In contrast,
`GET /~arweave@2.9/block&block=<indep-hash>` is a gateway read and may return an
unvalidated peer response.

Bootstrap is refused after a chain has been established. An operator can set
`arweave-force-bootstrap` in the node message to re-anchor deliberately; the
request itself cannot enable this option.

## Node options

| Key | Default | Meaning |
|---|---:|---|
| `arweave-checkpoint-block` | unset | Explicit trusted checkpoint hash |
| `arweave-checkpoint-depth` | `30` | Shared-ancestor distance below peer tip |
| `arweave-trusted-peers` | `[]` | Sources allowed to establish the checkpoint |
| `arweave-untrusted-peers` | `[]` | Sources for blocks and transactions after bootstrap |
| `arweave-force-bootstrap` | `false` | Permit an operator-initiated re-anchor |
| `arweave-sync-batch` | `50` | Maximum blocks applied by one sync call |
| `arweave-backfill-batch` | `50` | Ceiling on the heights one backfill call materialises |
| `arweave-settle-batch` | `50` | Maximum blocks whose transactions one sync call announces; `0` disables the hook |
| `arweave-peer-workers` | `8` | Concurrent peer fetch workers |
| `arweave-peer-timeout` | `60000` | Peer response timeout in milliseconds |
| `arweave-peer-connect-timeout` | `10000` | Peer connect timeout in milliseconds |
| `arweave-randomx-mode` | `light` | RandomX `light` or `fast` mode |
| `arweave-max-vdf-workers` | `max(1, schedulers div 2)` | Node-wide native VDF worker ceiling |
| `arweave-vdf-threads` | worker ceiling | Requested VDF workers; clamped to the ceiling |
| `arweave-vdf-timeline` | `false` | Compute a bounded in-memory VDF timeline ahead of the validated tip and reuse matching work |
| `arweave-require-accounts` | `true` | Refuse states without a verified account tree |

The worker ceiling is a node option. Caller-supplied messages may select fewer
workers but cannot create more native threads than the operator permits.

## Tests

Deterministic device suites use generated block-index, Merkle, account,
transaction, SPoRA, and VDF boundary vectors and require no checked-in mainnet
fixtures. The consensus cache is covered without a peer by building blocks and
transactions locally: `~arweave-block@2.9/materialize` is checked against
synthetic index entries, publication and settlement against a synthetic chain,
and `backfill` against a task-owned HyperBEAM micro-node serving real block
bytes over the Arweave peer paths.

```shell
rebar3 device test --devices dev_arweave,dev_arweave_block,\
  dev_arweave_block_index,dev_arweave_history,dev_arweave_merkle,\
  dev_arweave_spora,dev_arweave_tx,dev_arweave_vdf,dev_arweave_wallets
```

Public peers prune historical wallet lists, so the full real-state integration
test hydrates a recent checkpoint into `_build/arweave-test-vectors`, finds a
transaction-bearing child, applies it, and asserts that the resulting account
root equals the child's signed `wallet-list`:

```shell
rebar3 device test --devices dev_arweave \
  --test all:live_account_transition --timeout 5400
```

The same store is reused by the other probes, which are worth running after any
change to publication or the chain's shape:

| Probe | Establishes |
|---|---|
| `live_bootstrap` | A trusted-state join end to end, and the tip it leaves |
| `live_account_transition` | One real block applied, its account root equal to the signed `wallet-list` |
| `live_sync` | A sync pass: blocks applied, published, and the tip moved |
| `live_settle` | Settlement: the hook run per placement, and the markers that stop it repeating |

## Scope

Supported blocks start at the Arweave 2.9 fork. The subsystem does not gossip,
implement pre-2.9 proof formats, or replay from genesis. Two parts of an Arweave
node live beside it and are documented separately: `~arweave-storage@2.9` holds
the weave, and `~arweave-mining@2.9` searches it. Neither announces what it
finds. It also has no persistent VDF server. With `arweave-vdf-timeline`
disabled, validation recomputes each child's VDF interval. When enabled, one
bounded process computes ahead of the validated tip and reuses only exact
output and checkpoint matches from the same seed and difficulty; all absent or
mismatched work falls back to ordinary verification. When a long block gap
exceeds the header's 10,800-step suffix, the omitted prefix is computed
sequentially before that suffix is verified.

Native builds currently target macOS arm64 and Linux x86-64. The RandomX light
mode is intended for validators; fast mode has a substantially larger memory
footprint.
