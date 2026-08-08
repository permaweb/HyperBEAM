# `~arweave@2.9`

`~arweave@2.9` adds pull-based, post-2.9 Arweave validation to a HyperBEAM
node. It bootstraps from a trusted state checkpoint, fetches new blocks from
peers, validates them locally, and stores the resulting chain states in
`hb_cache`.

The existing gateway keys (`tx`, `chunk`, `raw`, `price`, `block`, `current`)
remain peer-backed. Use `validated` when the answer must have been checked by
this node.

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
parent. The pipeline covers:

- block identity, signature, timestamp, difficulty, and cumulative work;
- proof of work and both proof-of-access paths;
- the complete VDF interval and seed/difficulty transitions, computing any
  prefix omitted by the header's bounded step list;
- transaction identities, signatures, anchors, fees, balances, and Merkle root;
- block-index continuity and root; and
- the account transition and exact signed `wallet-list` root.

Account validation is required by default. A bootstrap fails if peers cannot
serve the selected checkpoint's own wallet tree; it does not substitute a tree
from another height.

## Devices

| Device | Responsibility |
|---|---|
| `~arweave@2.9` | Peer I/O, bootstrap, sync, fork choice, local lookups |
| `~arweave-block@2.9` | Block codec and complete state transition |
| `~arweave-block-index@2.9` | Weave index construction and proofs |
| `~arweave-merkle@2.9` | Offset-indexed Merkle path validation |
| `~arweave-spora@2.9` | Recall ranges, RandomX packing, and proof of access |
| `~arweave-tx@2.9` | Transaction codec and admission rules |
| `~arweave-vdf@2.9` | Nonce-limiter chain, seeds, and difficulty |
| `~arweave-wallets@2.9` | Patricia account tree, sparse updates, rollback |

## Bootstrap and sync

Configure a persistent store, block sources in `arweave-untrusted-peers`, and
one bootstrap trust root:

- `arweave-checkpoint-block`: an explicit block hash; or
- `arweave-trusted-peers`: peers that must agree on a shared ancestor near the
  tip.

Then call `GET /~arweave@2.9/bootstrap` once. Bootstrap verifies the selected
block's identity, reconstructs and verifies its block index, fetches the
checkpoint histories and transaction-anchor window, and verifies the
checkpoint's account tree against its signed root.

Schedule `GET /~arweave@2.9/sync` with `~cron@1.0` after bootstrap. A typical
interval is 30 seconds. Sync is idempotent: each validated state is indexed by
block hash, and the tip moves only after that state has been written.

`GET /~arweave@2.9/tip` returns the selected local tip. Fork choice follows
Arweave's cumulative-difficulty and checkpoint-depth rules.

`GET /~arweave@2.9/validated&block=<indep-hash>` returns a state produced by
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
| `arweave-peer-workers` | `8` | Concurrent peer fetch workers |
| `arweave-peer-timeout` | `60000` | Peer response timeout in milliseconds |
| `arweave-peer-connect-timeout` | `10000` | Peer connect timeout in milliseconds |
| `arweave-randomx-mode` | `light` | RandomX `light` or `fast` mode |
| `arweave-max-vdf-workers` | `max(1, schedulers div 2)` | Node-wide native VDF worker ceiling |
| `arweave-vdf-threads` | worker ceiling | Requested VDF workers; clamped to the ceiling |
| `arweave-require-accounts` | `true` | Refuse states without a verified account tree |

The worker ceiling is a node option. Caller-supplied messages may select fewer
workers but cannot create more native threads than the operator permits.

## Tests

Deterministic device suites use generated block-index, Merkle, account,
transaction, SPoRA, and VDF boundary vectors and require no checked-in mainnet
fixtures.

Public peers prune historical wallet lists, so the full real-state integration
test hydrates a recent checkpoint into `_build/arweave-test-vectors`, finds a
transaction-bearing child, applies it, and asserts that the resulting account
root equals the child's signed `wallet-list`:

```shell
rebar3 device test --devices dev_arweave \
  --test all:live_account_transition --timeout 1800
```

The same store is reusable for subsequent live sync checks.

## Scope

Supported blocks start at the Arweave 2.9 fork. The subsystem does not mine,
gossip, retain the weave, implement pre-2.9 proof formats, or replay from
genesis. It also has no persistent VDF server. Validation recomputes each
child's VDF interval; when a long block gap exceeds the header's 10,800-step
suffix, the omitted prefix is computed sequentially before that suffix is
verified.

Native builds currently target macOS arm64 and Linux x86-64. The RandomX light
mode is intended for validators; fast mode has a substantially larger memory
footprint.
