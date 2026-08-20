# `~arweave-storage@2.9`

`~arweave-storage@2.9` holds the weave. It owns the storage modules an Arweave
node keeps its share of the data in: the chunk files on disk, the replica-2.9
entropy they are packed with, the index that places each chunk in the weave, and
the record of which offsets are held.

This is what makes a miner a miner. A partition is 3.6 TB of chunks packed for
one address, and a pass over one nonce-limiter step reads two 2.5 MiB recall
ranges of it and hashes them. Nothing about that is affordable from a peer: the
chunks a peer serves are unpacked, and packing one 8 KiB sub-chunk costs an 8
MiB RandomX run.

## On-disk compatibility

The layout is the Arweave node's own. Point `arweave-data-dir` at a data
directory an Arweave node filled and this node reads what is there:

```
<arweave-data-dir>/
    index/                                  this node's, built by `import'
    storage_modules/<StoreID>/
        chunk_storage/<ChunkFileStart>      the chunks, as the node wrote them
        chunk_storage/prepare_replica_2_9_cursor    whichever node prepares
        rocksdb/                            the node's own indexes, never written
        hyperbeam_sync_cursor               this node's, written by `sync'
```

Point a node at a directory an Arweave node filled and two files inside a
module's own directory are all it writes there. `hyperbeam_sync_cursor` is this
node's and means nothing to an Arweave node. `prepare_replica_2_9_cursor` is
the Arweave node's file in the Arweave node's format, and preparing a partition
is what writes it -- which is the point: either node may carry on where the
other stopped. Nothing writes into `rocksdb`, and nothing rewrites a cursor a
pass did not move, so a node whose modules are complete leaves them alone. The
index this node builds is not among them: it is one store under `index`, beside
`storage_modules` rather than inside any of them.

`<StoreID>` is `ar_storage_module:id/1`'s spelling, vendored:
`storage_module_<Bucket>_<Address>.replica.2.9` where the bucket size is the
partition size, and `storage_module_<BucketSize>_<Bucket>_<Address>.replica.2.9`
where it is not. A chunk file is named for the first weave offset it covers, a
multiple of the chunk group size (2 097 152 000 bytes by default), and holds a
dense array of 262 147-byte slots: a three-byte big-endian offset within the
bucket, then 262 144 bytes of chunk. A zero prefix means the slot has never been
written; a chunk whose offset within its bucket really is zero is written as
262 144, so that it reads back as written rather than as absent.

What an Arweave node keeps in RocksDB beside those files -- the Merkle paths
that place each chunk, and the sync records -- this node keeps in a store of its
own under `index`. Nothing writes into the node's `rocksdb` directory, so a data
directory stays readable by the node that built it, and `import` builds one from
the other.

## Packing, and why a partition is `prepare`d

A replica-2.9 chunk is enciphered by exclusive-or with 256 KiB of entropy
assembled from thirty-two separate 8 MiB blobs, one per sub-chunk. Each blob is
a RandomX run keyed on the address, the entropy partition and the entropy index,
and each is sliced across 1024 chunks distributed through the partition -- the
*footprint*. Generating the entropy for one chunk alone therefore costs the same
thirty-two runs as generating it for the 1024 that share those blobs.

So the entropy is written first, into the chunk file slots themselves. A slot
then holds either raw entropy waiting for data, or data already enciphered with
it, and the bytes alone do not say which -- the sync records do:

| record | meaning |
|---|---|
| `ar_chunk_storage_replica_2_9_5_entropy` | this bucket's entropy is written |
| `ar_chunk_storage_replica_2_9_1_unpacked` | a chunk is waiting here for it |
| `ar_chunk_storage` | this bucket holds a real chunk |
| `ar_data_sync` | this range is synced, at this packing |

Either half may arrive first. A chunk stored before the entropy is kept
unenciphered under the `unpacked_padded` record, and the preparation pass
enciphers it in place when it reaches that bucket; a chunk stored after is
enciphered with the entropy already in the slot. The cursor recording how far a
partition is prepared is the file an Arweave node keeps, in the format it keeps
it in, so a partition either node prepared is one the other can carry on
preparing.

## Keys

| Key | Meaning |
|---|---|
| `modules` | every configured storage module: where it is, what it holds, how much is synced, whether it is prepared |
| `range` | the packed chunks of a span of the weave, for a miner to hash |
| `chunk` | the packed bytes of the chunk holding a byte |
| `chunk-proof` | that chunk, its unpacked form, and the two Merkle paths that place it |
| `sync-record` | the intervals each module holds, under each record it keeps them in |
| `prepare` | generate and store the entropy for more of a module's range |
| `sync` | fetch and store more of a module's range from peers |
| `store` | store one chunk with the proof that places it in the weave |
| `import` | build a module's index and records from an Arweave node's RocksDB |

`range` and `chunk-proof` are the contract a miner reads a weave through; see
[`~arweave-mining@2.9`](arweave-mining-at-2-9.md). `range` answers with raw
bytes and `chunk-proof` with base64url, and the split is not an accident: a
range is a bulk read whose bytes are hashed and dropped, while a proof's fields
go into a block header, which is base64url throughout. Encoding a 2.5 MiB range
costs a third of what reading it does, and a pass reads two of them per step.

The consequence is that `range` and `chunk` are binary answers: ask for them
under a codec that carries bytes, not as JSON, which has no way to spell a
chunk. It is part of the contract rather than a detail, so a miner refuses a
range whose chunks are not a chunk's worth of bytes rather than hashing text
and reporting a partition that holds nothing.

Every key that names a module takes `module`, which is its identifier -- the
directory it lives in. A node with one module needs no such argument; a node
with several is told to name one, and told which there are.

| Key | Arguments |
|---|---|
| `range` | `range-start`, `size`, `packing-difficulty`, `packing`, `address` |
| `chunk`, `chunk-proof` | `offset`, `packing`, `address` |
| `prepare` | `module`, `footprints` |
| `sync` | `module`, `chunks` |
| `store` | `offset`, `chunk`, `tx-path`, `data-path`, `module`, and optionally `tx-root`, `block-start-offset`, `block-size` |
| `import` | `module`, `source` |

`store` is where every rule about placing a chunk lives. Where the chunk goes is
read out of the proof, never out of the request: the two Merkle paths are walked
against the tx root of the block that wrote it, and the absolute end offset, the
chunk size, the data root and the offset within the transaction are what that
walk resolved to. A chunk whose bytes do not hash to the leaf the paths name, or
whose size the leaf does not give it, is refused. A caller that could name the
offset could put any bytes in any slot of a partition this node then mines.

The block the paths are walked against is read from the block index this node
validated itself, for the same reason. A caller may name it -- a node whose
chain does not reach an offset has nothing to read it from -- and a caller that
names it where the chain does reach is checked against the index rather than
believed. Bounds the index disagrees with are `bounds-not-indexed`, and nothing
is written.

## Configuration

| Option | Default | Meaning |
|---|---|---|
| `arweave-data-dir` | `arweave-data` | The directory storage modules live under |
| `arweave-storage-modules` | none | The modules this node holds |
| `arweave-chunk-group-size` | 2 097 152 000 | Bytes of chunk data per chunk file |
| `arweave-prepare-footprints` | 1 | Entropy footprints one `prepare` pass generates |
| `arweave-packing-workers` | half the schedulers | Entropies generated at once |
| `arweave-randomx-mode` | `light` | `fast` builds the RandomX dataset: minutes to start, an order of magnitude faster to pack |
| `arweave-storage-batch` | 100 | Chunks one `sync` pass fetches |
| `arweave-weave` | `~arweave@2.9` | Where `sync` asks for a chunk this node does not hold |
| `arweave-storage-index` | `<arweave-data-dir>/index` | The store every module's index and sync records are kept in. One for all of them, sized at 64 GiB a module: every key names its own module, and an LMDB environment reserves its whole capacity in address space when it opens -- at the store's own 2 TiB default a node runs out at the sixty-fourth |

A storage module entry names a `bucket`, and optionally a `bucket-size` (the
partition size by default -- the only size a miner may use), a `packing`
(`replica-2-9` by default) and an `address` (the node's own by default):

```json
{ "arweave-storage-modules": [ { "bucket": 27020, "packing": "replica-2-9" } ] }
```

## Running one

Every pass is a bounded, idempotent resolution, scheduled with `~cron@1.0` in
the same breath as `~arweave@2.9/sync`:

```
GET /~cron@1.0/every?interval=30-seconds&cron-path=~arweave@2.9/sync
GET /~cron@1.0/every?interval=1-seconds&cron-path=~arweave-storage@2.9/prepare
GET /~cron@1.0/every?interval=1-seconds&cron-path=~arweave-storage@2.9/sync
GET /~cron@1.0/every?interval=1-seconds&cron-path=~arweave-mining@2.9/mine
```

A `~cron@1.0/every` worker sleeps its interval *after* the call returns, so a
pass can never overlap itself. Every mutation of one storage module additionally
runs through a single runner for that module, because the writes to a chunk
file, its index and its sync record are one operation and two passes
interleaving them would leave a record claiming bytes that were never written.

Preparing a partition is the expensive part of standing a miner up: 3.6 TB is
14 063 footprints, and a footprint is thirty-two RandomX runs. It is done once.

## Importing an existing node's data

`~arweave-storage@2.9/import` reads an Arweave node's RocksDB metadata and
writes this node's index and sync records from it. The chunk files are
byte-compatible and are read in place -- only the metadata moves, so nothing is
re-downloaded and nothing is re-packed. The source is opened read-only and
nothing is ever written to it, so the node that built it goes on reading it and
an import may be run again. What an import writes is this node's own `index`,
which is not in the module's directory at all.

RocksDB is the dependency of one rebar3 profile and of nothing else, so a node
that never imports never links it. Whether it is there is asked at run time,
and an import on a node without it says which build would have one:

```shell
rebar3 as rocksdb compile
GET /~arweave-storage@2.9/import?module=storage_module_27020_<address>.replica.2.9
```

## Tests

```shell
rebar3 device test --devices dev_arweave_storage
```

The deterministic vectors build storage modules on real temporary directories,
store real chunks through real Merkle proofs, prepare a replica-2.9 module and
check the packed bytes against the protocol's own derivation of the entropy
rather than against the device's -- and then check the resulting proof of access
through `~arweave-spora@2.9/validate`, which is the check a block validator
applies.

The live probes sync real chunks from the network into a temporary storage
module and read them back proven.

```shell
rebar3 device test --devices dev_arweave_storage --test all:live_syncs_mainnet_weave
```
