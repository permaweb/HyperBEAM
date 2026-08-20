# `~arweave-mining@2.9`

`~arweave-mining@2.9` mines Arweave blocks. It takes a validated parent block,
searches the recall ranges a nonce-limiter step unlocks, and produces the
signed child block a solution entitles it to -- checked, before it is returned,
by the same `~arweave-block@2.9` validation every block this node accepts
passes.

Mining is expressed the way the rest of `~arweave@2.9` is: as bounded,
idempotent resolutions. There is no mining server, no worker pool and no chunk
cache. A pass is a request, and an operator schedules it with `~cron@1.0` in
the same breath as `sync`.

## What a solution is

A nonce-limiter step, a mining partition and a mining address determine one
RandomX hash:

```
H0 = RandomX(output || partition || seed || address || packing-difficulty)
```

`H0` picks two recall ranges: the first inside the miner's own partition, the
second anywhere in the weave below the partition upper bound. Each range holds
`?RECALL_RANGE_SIZE div packing-difficulty` bytes -- 2.5 MiB at the post-2.9
packing difficulty of 10, which is ten chunks, which is 320 nonces.

Nonce `N` addresses the 8 KiB sub-chunk at index `N rem 32` of the chunk
containing byte `range-start + (N div 32) * 256 KiB`. Both hashes are taken
over that sub-chunk in its **packed** form:

```
H1 = sha256(H0 || sha256(H0 || N || packed-sub-chunk-1))
H2 = sha256(H0 || sha256(H1 || packed-sub-chunk-2))
```

`H1` alone is a solution if it clears the one-chunk difficulty, which is a
hundred times the block's own. Otherwise the same nonce's sub-chunk from the
second range gives `H2`, which need only clear the block's difficulty. A
one-chunk solution is always permitted -- it is not a fallback for a missing
second range, it is simply a hundred times rarer.

Both thresholds are `ar_node_utils:h1_passes_diff_check/3` and
`h2_passes_diff_check/3`: the miner's acceptance test is, literally, the
function the validator will judge the block with.

## What a miner must hold

A nonce is worth nothing without the packed bytes it addresses. A partition is
3.6 TB of chunks packed for one address, and a pass over one nonce-limiter step
reads two 2.5 MiB recall ranges of it and hashes them. A node mines from what it
holds; `~arweave-storage@2.9` is what holds it.

The device reads the weave through two keys on a message the caller supplies,
and `~arweave-storage@2.9` is the source when the caller names none:

```
GET <weave>/range&range-start=<byte>&packing-difficulty=<d>&packing=<p>&address=<a>
    -> packing, chunks: [{absolute-end-offset, chunk}]

GET <weave>/chunk-proof&offset=<byte>&packing=<p>&address=<a>
    -> chunk, unpacked-chunk, data-path, tx-path, absolute-end-offset, chunk-size
```

`range` is what a pass hashes: one resolution reads a whole recall range, which
is one read of one chunk file, and the chunks come back in the packed form the
partition holds them in. It carries no proofs, because the Merkle paths are
needed only for the one nonce that met the difficulty.

`chunk-proof` is what that nonce's solution carries. It is asked once per
solution, and it is where the expensive half happens: a proof of access at
packing difficulty one or above carries the whole 256 KiB unpacked chunk beside
the 8 KiB packed sub-chunk that was hashed, and deciphering one replica-2.9
chunk is thirty-two 8 MiB RandomX runs. A pass that unpacked every chunk it
examined would spend its entire budget on nonces that found nothing.

A span the source holds nothing in is a hole rather than a failure: the nonces
it covers yield nothing and the pass continues, which is the conclusion a miner
missing part of its own partition reaches too. An *answer* this node cannot use
is different. A source that answers in a packing other than the one it was asked
for ends the pass naming both, because nothing downstream could tell: a nonce
slices the sub-chunk it addresses out of whatever bytes it is given, so an
unpacked answer would read as a partition that holds nothing rather than as a
misconfigured source.

`absolute-end-offset` comes from the source and is never derived. It is the
offset the packing is keyed on, and the transaction's own Merkle layout
determines it: below the strict data split threshold, chunks are not aligned to
256 KiB buckets and no arithmetic over the recall byte recovers it. A post-2.9
recall byte may land anywhere in the weave, so both layouts are live for a
miner.

Before it is carried into a block, the packed sub-chunk a proof declares is
required to be the sub-chunk the pass hashed, at the index the nonce addresses.
That check is the seam between the two reads: a pass hashes bytes a range read
out of a chunk file, and a proof is built from bytes an index placed in the
weave. If those disagree -- an index ahead of its data, a chunk file written
under a different address -- the block would carry a proof of a chunk nothing
hashed, and the difference is invisible until a validator rejects it.

`max-nonces` caps what one pass will spend on each range -- a ceiling on cost,
not a cursor: every pass enumerates a range from its first nonce, so a bound
shortens a search rather than moving it.

## A pass

`GET /~arweave-mining@2.9/mine` is one bounded pass: it advances the nonce
limiter beyond the block being extended, searches each partition at each step,
and answers with the signed block the first solution entitles the node to --
checked, before it is answered, by the same validation every block this node
accepts passes. A miner that returned a block it had not checked would be
asking its operator to publish one this node itself would reject.

| Key | Default | Meaning |
|---|---|---|
| `parent` | the node's tip | The block being extended |
| `steps` | `1` | Nonce-limiter steps beyond the parent this pass walks |
| `partitions` | those the weave holds | The partitions searched at each step |
| `max-nonces` | the whole range | Nonces spent on each range of each step |
| `transactions` | none | The transactions the block carries |
| `weave` | `~arweave-storage@2.9` | The source the chunks are read from |
| `timestamp` | now | The moment the block is mined at |

The timestamp is not decoration: the retarget rule derives the difficulty from
it, so the search and the block it produces are run against one value.

A pass does nothing outside itself. It does not publish the block, move the
tip, or announce anything -- a node that adopted a block it had not announced
would be a fork of one. What it does do is run the block through the
`arweave-mined-block` hook, which is where an operator attaches whatever
announcement their deployment calls for:

```json
{ "on": { "arweave-mined-block": { "device": "<your-device>" } } }
```

The handler is given the block message. What it does with it -- reserializing
it with the Arweave wire codec and posting it to peers, writing it somewhere,
or counting it -- is outside this device. What it *returns* is discarded: the
pass answers with the block it checked, so a handler cannot substitute a
message nothing validated under a result saying it was mined. A handler that
fails fails the pass, which is loud rather than quiet, and a pass is
deterministic in its parent and timestamp, so repeating it reaches the same
block. This is the same seam `arweave-settled-transaction` uses, for the same
reason: consensus work and the work hanging off it should not be one thing.

## Scope

The device mines. Storing the weave, packing it and keeping the sync record are
`~arweave-storage@2.9`'s; validating and following the chain are
`~arweave@2.9`'s. It does not gossip, implements neither coordinated mining nor
pool mining, and keeps no hashrate statistics.

The node's own key signs what it mines, and Arweave derives a mining address
from an RSA key differently from the way it derives one from an ECDSA key. A
node whose wallet is not RSA produces blocks whose signature its own `identity`
check rejects, so mining is an RSA-key deployment. It fails closed.

It runs no nonce limiter of its own. It does not need one: with
`arweave-vdf-timeline` set, the node already runs the nonce limiter alongside
the weave, anchored on each validated block and computing forward from it at
close to real time. A pass takes the steps that timeline holds and computes
only what it does not -- an epoch it has not been re-anchored past, a
difficulty that has since retargeted, or a timeline that is not running at all,
each of which costs a pass what it always did rather than failing.

That is the difference between a pass that keeps pace with the live timeline
and one that cannot. `ar_vdf:compute/3` is compiled against the portable SHA-2
implementation; the timeline picks the fastest of three, having self-tested it
against that reference at the difficulty in use. Consuming its steps is not a
shortcut past any rule: it offers a step only under the seed and difficulty it
computed it with, the nonce limiter is one chain shared by every block at a
step, and what a pass builds from them goes through `verify-chain` before it is
answered with.

## Tests

The deterministic vectors build a synthetic weave, mine over it, and check the
solution against the proof-of-access device and the difficulty rules. One of
them builds a real storage module on a temporary directory the way a node builds
one -- entropy written into the chunk file slots, then chunks enciphered into
the slots their own Merkle proofs place them at -- mines over it and over a
source answering from memory, and requires the two solutions to agree in every
public field: the nonce, both recall bytes, both solution hashes, both
preimages, and both proofs of access. A module that placed a chunk one bucket
out, enciphered it with another bucket's entropy, or indexed it at an offset its
files do not hold would differ in at least one of them.

The block production vectors build a parent carrying real account, index and
history state, and require this node's own `~arweave-block@2.9` validation to
accept what was produced under the `full` profile -- first from a solution made
by hand, which establishes the producer alone, and then from `mine`, which
establishes the whole path including the nonce limiter walk and the difficulty
the search was run against.

```shell
rebar3 device test --devices dev_arweave_mining,dev_arweave_block,\
  dev_arweave_spora,dev_arweave_storage
```

One probe holds the device against the network it implements. It takes two
blocks mainnet already accepted -- one solved from two chunks and one, a
hundred times rarer, solved from a single chunk -- and for each recomputes the
mining entropy from the step output and the seed of the block below it, derives
the recall bytes, recomputes the solution hash from the packed sub-chunks the
proofs carry, checks each hash against the difficulty its kind is held to, and
packs the block's own unpacked chunks at the offsets the weave reports,
requiring each to reproduce the packed sub-chunk the block declares. The
two-chunk block's recall bytes fall on opposite sides of the strict data split
threshold, so both weave layouts are covered.

```shell
rebar3 device test --devices dev_arweave_spora \
  --test all:live_reproduces_mainnet
```

A packing that differs from the network's by one byte, or an entropy keyed on
an offset derived rather than read, produces blocks nothing accepts. Neither
failure is visible to a suite that only checks the node against itself.

The other probe closes the loop: it takes a block mainnet accepted, builds a
storage module around each of its two recall bytes, prepares and stores them
through their own Merkle proofs, and requires a search of that storage, at the
block's own difficulty, to find the block's own nonce, recall bytes, solution
hash, hash preimage and both proofs of access. A partition this node packed, an
index this node built, a search this node ran, and the answer the network had
already agreed on.

```shell
rebar3 device test --devices dev_arweave_mining \
  --test all:live_mines_a_mainnet_solution
```
