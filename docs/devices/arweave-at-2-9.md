# `~arweave@2.9`

`~arweave@2.9` gives a HyperBEAM node access to the Arweave network. Beyond
relaying queries to a peer, it can **validate Arweave blocks itself** and keep a
verified chain in the node's own stores, reachable through `hb_cache`.

This page covers the validation subsystem. The relay keys (`tx`, `chunk`,
`raw`, `price`, `block`, `current`) are unchanged and are configured by the
`/arweave` route in the node message.

## What "validated" means here

A block is accepted only after the node has checked it itself. Nothing is taken
on a peer's word except at bootstrap, and even there only a single block hash is
trusted — everything else fetched is checked against it.

Each block is validated against its parent across **31 sequential checks** plus
the account transition, which asserts four further quantities of its own. Between
them they can reject a block in **44 distinct ways** — countable as the distinct
`invalid-*` messages `dev_arweave_block.erl` can emit:

```bash
awk '/^%%% Tests\./{exit} 1' src/preloaded/arweave/dev_arweave_block.erl \
  | grep -oE '<<"invalid-[a-z0-9-]+">>' | sort -u | wc -l
```

(The `awk` stops at the test section; counting the whole file also counts the
messages the mutation tests name in their assertions.)

The checks cover the block
signature and independent hash, the proof of access (SPoRA) including chunk
unpacking, the full VDF chain, difficulty and retargeting, the transaction set
and its Merkle root, the block-index root, and the account-state transition
including the `wallet_list` root.

The last of those is worth calling out. `wallet_list` is a Merkle-Patricia root
over every Arweave account *after* the block is applied. Reproducing it means
reproducing the entire economic transition — every transfer, fee, and mining
reward — to the winston. If the node's arithmetic is wrong anywhere, the root
does not match and the block is rejected. Mainnet is the oracle.

## Devices

The subsystem is deliberately split, so each piece is usable on its own.

| Device | Responsibility |
|---|---|
| `~arweave@2.9` | Peer I/O, `bootstrap`, `sync`, `tip`, `validated` |
| `~arweave-block@2.9` | Block codec, hashing, and `apply` — the state transition |
| `~arweave-spora@2.9` | Storage proofs: recall ranges, packing and unpacking, `H0`/`H1`/`H2` |
| `~arweave-vdf@2.9` | Nonce limiter: VDF chain verification, seeds, difficulty |
| `~arweave-merkle@2.9` | Generic offset-indexed Merkle path validation |
| `~arweave-block-index@2.9` | The `{indep-hash, weave-size, tx-root}` index over the weave |
| `~arweave-wallets@2.9` | The account tree |
| `~arweave-tx@2.9` | Transaction validation |

`~arweave-merkle@2.9` carries no Arweave-specific knowledge at all — it
validates a path against a root at an offset under a named ruleset, and is
reusable for any offset-indexed tree.

## Keeping a node in sync

Validation is pull-only. The node never gossips; it asks peers for blocks and
checks them. In production, point `~cron@1.0` at it:

```
POST /~cron@1.0/every
  cron-path: /~arweave@2.9/sync
  interval:  30-seconds
```

`sync` is idempotent and resumable by construction: a block whose chain state is
already stored is neither fetched nor applied, so an interrupted pass leaves a
consistent tree and the next pass continues where it stopped.

`GET /~arweave@2.9/validated&block=<indep-hash>` returns the chain state this
node produced for a block, or `not-validated` if it has not verified it. It
never falls back to a peer.

Note the contrast with `GET /~arweave@2.9/block&block=<id>`, which is a gateway
key: on a cache miss it fetches from a peer and returns the answer **unverified**.
Both live on a device that validates blocks, so which one you ask decides
whether the answer means anything. Use `validated` when you need a block this
node checked.

`GET /~arweave@2.9/tip` returns the chain state at the tip of the heaviest
eligible branch, chosen with Arweave's own fork-choice rule — strictly greater
`cumulative-diff` wins, an equal one keeps the incumbent, and a branch is
eligible only if it forks no deeper than 18 blocks below the tip.

## Bootstrapping

`GET /~arweave@2.9/bootstrap` establishes the initial chain state. This is the
only moment the node trusts anything.

Two modes:

- **Checkpoint** — set `arweave-checkpoint-block` in the node message to a block
  hash. This is the `bitcoind` model, and the trustless configuration.
- **Shared ancestor** — with `arweave-trusted-peers` set, the node takes the
  block those peers agree on at `arweave-checkpoint-depth` below the tip.

Only `arweave-trusted-peers` may decide where a shared-ancestor bootstrap
anchors; a node that sets neither that nor `arweave-checkpoint-block` is told
so rather than quietly asking whichever peers a previous bootstrap happened to
find. A node given a checkpoint block needs no trusted peers at all.

In both cases everything else is verified against that block: the block index
against its `hash-list-merkle`, the account tree against its `wallet-list`, the
transaction anchor window against the identifiers in that index, and the reward
and block-time histories against their respective hashes. So the trusted input
is one hash, and roughly 176 MB of index plus 300,000-odd accounts are checked
against it.

Bootstrap then populates `arweave-untrusted-peers`, which is where blocks come
from afterwards — every one of them validated.

### Bootstrapping twice

`bootstrap` is refused once the node has a validated chain of its own, with
`already-bootstrapped`, before any peer is contacted. Without that guard it is a
repeatable trust reset: every call re-asks the peers where the chain starts and
moves the tip onto their answer, discarding a chain the node had checked for
itself. The rule is having a chain at all, not having a longer one — a
checkpoint a few blocks ahead is still blocks taken on a peer's word that the
node could have validated, and that is the case that actually occurred in
testing. `sync` is what extends a chain; `bootstrap` is what starts one. Pass
Set `arweave-force-bootstrap` in the **node message** to re-anchor anyway — a
node stranded too far behind to close the gap needs a way out.

It is deliberately not accepted from the request. `bootstrap` is reachable by
anyone the node answers, and forcing it discards a chain the node validated for
itself and re-anchors on whatever the peer set says. Honouring `force` from the
request would leave the subsystem's one guarded trust boundary open to any
caller who could reach the port — refusing the operator's accidental second
bootstrap while waving through a stranger's deliberate one.

### The transaction anchor window

A transaction anchors on a block within 50 of the one carrying it, and may not
repeat a transaction already inside that window. Both rules read the chain
state's `recent-blocks`, so bootstrap seeds it with the checkpoint and the 49
blocks below it — about 28 MB of headers, fetched in parallel and each
re-identified by recomputing its `indep-hash`. Without it every block-anchored
transaction on the network is refused, which is to say every real block.

### Cost

Validating the VDF chain is deliberately expensive; that is what a verifiable
delay function is for. On current mainnet a block costs roughly **130
CPU-seconds** of VDF verification against a ~122-second block interval, so
*keeping up* costs about **1.1 cores** while *catching up* is linear in blocks.
Measured end to end with 14 threads, a mainnet block lands in 17–43 seconds.

That figure depends on the verification path reaching the crypto-extension
kernel. The node runs a known-answer test at load and logs which kernel it
installed:

```
VDF verify kernel fused ARM
VDF verify kernel OpenSSL
```

The second line means the self-test failed or the hardware lacks the
extensions, and verification runs ~6.4× slower — about 7 cores to keep up.
Raise `arweave-vdf-threads` accordingly.

This is why the checkpoint defaults to a recent height rather than the 2.9 fork:
validating the ~373,000 blocks since the fork would take on the order of 13,500
CPU-hours.

There is also a hard limit, independent of cost. Bootstrap needs the reward and
block-time histories, and peers serve those only for the last 50 blocks
(measured: HTTP 200 at depth 50, 404 at depth 60). **A checkpoint deeper than
that fails bootstrap with `history-unavailable`**, so the checkpoint mode cannot
currently reach further back than the shared-ancestor mode. `bootstrap` logs an
estimate of what a chosen checkpoint will cost before spending any of it.

## Node message options

| Key | Default | Meaning |
|---|---|---|
| `arweave-checkpoint-block` | `[]` | Bootstrap checkpoint block hash |
| `arweave-checkpoint-depth` | `30` | Default checkpoint, in blocks below the tip |
| `arweave-trusted-peers` | `[]` | The only peers that may establish a checkpoint |
| `arweave-force-bootstrap` | `false` | Re-anchor over an already-validated chain. Node message only — never read from the request |
| `arweave-untrusted-peers` | `[]` | Block and transaction sources; populated by `bootstrap` |
| `arweave-sync-batch` | `50` | Maximum blocks advanced per `sync` pass |
| `arweave-randomx-mode` | `light` | `light` or `fast` |
| `arweave-vdf-threads` | `schedulers div 2` | VDF verification parallelism |
| `arweave-require-accounts` | `true` | Refuse to advance the chain without an account tree |

`arweave-require-accounts` is the difference between validating a block and
validating a *chain*. `~arweave-block@2.9/validate` answers for a single
transition and names the mode it ran in, so a caller inspecting one block can
act on the answer either way. `apply` returns the state the next block is
checked against, and by then the distinction is gone — so at the default it
refuses outright, with `accounts-not-checked`, rather than carrying a weaker
state forward.

Setting it `false` gives consensus-only validation: proof of access, proof of
work, VDF, difficulty and the block's own commitments are all still checked, but
nothing about who may spend what. That is the mode the staged trust model needs
for pre-2.9 work types, which arrive with no account tree to spend from. It is
not a performance setting, and a node syncing mainnet should not use it.

`light` RandomX uses roughly 518 MiB and costs about 0.24 s per entropy
generation — two per block, so under half a percent of one core. `fast` is
~8× quicker per operation but needs ~6.6 GiB of datasets and tens of seconds to
initialise, which validation does not need.

## Two validation modes, and how to tell them apart

Account and transaction validation need the account tree. Bootstrap adopts it at
the deepest block a peer still serves it for — peers prune the wallet list at
roughly 100 blocks — and verifies its root against that block's signed
`wallet-list` before adopting it.

If no peer serves a usable tree, the node still validates every block at the
consensus level (signature, `indep-hash`, proof of access, the VDF chain,
difficulty and every field check) but **cannot** check the account state
transition, the `wallet-list` root, or per-transaction signatures, balances,
anchors and fees.

That is a materially weaker mode, so it is recorded rather than silent: the
chain state carries a marker for which mode produced it, and `sync` emits it per
block. **Check it.** A node quietly running in the reduced mode looks identical
from the outside to one doing full validation.

## Scope

Supported: post-2.9 blocks, which is everything from height 1,602,350 onward.

Not implemented: pre-2.9 proof formats, mining, chunk storage beyond the chunks
carried in proofs, a persistent VDF server, and validation back to genesis. A
checkpoint below the 2.9 fork is refused rather than silently accepted, because
the node cannot check those proofs.

Transaction replay checks (`tx_already_in_weave`, mempool checks) are out of
scope, as they require a mempool the node does not keep.
