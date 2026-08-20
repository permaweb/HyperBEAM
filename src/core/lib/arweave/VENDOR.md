# Arweave consensus sources

This directory contains the Arweave consensus code needed by the post-2.9
validation devices.

| Field | Value |
|---|---|
| Upstream | `https://github.com/ArweaveTeam/arweave` |
| Commit | `50e47de6d054afefdee112fa124695eb8d0176fc` |
| Release | `2.9.6-alpha1` (`RELEASE_NUMBER 92`) |
| Source root | `apps/arweave/src` and `apps/arweave/include` |
| License | GNU GPL v2; see `LICENSE.md` in this directory |

The `ar_` modules retain upstream naming, formatting, records, and tests where
they were copied whole. HyperBEAM style is used by the `dev_arweave_*` and
`lib_arweave_*` integration modules, not retrofitted into this directory.

## Included surface

The port includes the pure consensus paths for:

- block serialization, signed hashes, difficulty, rewards, retargeting, and
  block-time history;
- transaction verification, replay-window checks, pricing, and account
  transitions;
- Patricia and Merkle trees, proof of access, RandomX packing, and replica 2.9;
- nonce-limiter/VDF verification;
- the on-disk layout of a storage module: where a chunk sits in a chunk file,
  which entropy enciphers it, and the interval algebra the sync record is; and
- the RandomX and VDF Erlang NIF wrappers.

The corresponding records and constants are in:

```text
src/core/include/ar.hrl
src/core/include/ar_block.hrl
src/core/include/ar_chunk_storage.hrl
src/core/include/ar_consensus.hrl
src/core/include/ar_inflation.hrl
src/core/include/ar_poa.hrl
src/core/include/ar_pricing.hrl
src/core/include/ar_vdf.hrl
src/core/include/ar_wallets.hrl
```

## Adaptation boundary

This is a focused validator port, not an Arweave node embedded in HyperBEAM.
The upstream process, storage, event, mining-pool, peer, metrics, and mempool
services are not included. Their consensus-pure operations are called with
explicit state instead.

Non-obvious local changes carry `VENDOR:` comments at the call site. The main
classes are:

1. Arweave include paths use HyperBEAM's `src/core/include` path.
2. NIF wrappers load from `code:priv_dir(hb)` and omit Arweave logging macros.
3. Process-owned RandomX packing state and VDF sessions are passed explicitly;
   persistent consensus state is represented by AO-Core messages and links.
4. `#tx.last_tx` call sites use HyperBEAM's existing `#tx.anchor` spelling.
5. `LOCALNET`, `AR_TEST`, `TESTNET`, metrics, event, and debug shortcuts are
   removed from production consensus paths.
6. `ar_serialize` contains the block, proof, history, and transaction subset
   required by the devices rather than the upstream HTTP/node surface.
7. `ar_wallet` and `ar_tx` retain HyperBEAM's existing AO transaction support
   while adding the upstream L1 verification functions used here.
8. The storage modules are reduced to their format surface. `ar_chunk_storage`
   keeps the offset, bucket and position arithmetic and the 262147-byte record
   layout; `ar_storage_module` keeps the identifier, range and packing;
   `ar_entropy_gen` keeps the footprint offsets, the entropy keys and the slice
   combination. The gen_servers, ETS indexes, prometheus counters, device locks
   and `ar_kv` calls around them are not ported: the file handles, the write
   ordering, the index and the sync record are `lib_arweave_chunks`,
   `lib_arweave_chunk_index` and `lib_arweave_sync_record`, and the data
   directory and chunk group size are passed in rather than read from
   `arweave_config`.
9. `ar_intervals` is copied whole, including its tests. Only `jiffy:encode/1`
   differs, replaced by `hb_json:encode/1`.

The source diff against the pinned checkout is the authority. A same-named
function without a local note must not be assumed byte-identical, because some
modules are intentionally reduced to the validator surface.

## Updating

1. Check out the new Arweave revision separately and record its full commit.
2. Diff every included `.erl` and `.hrl` against its upstream counterpart with
   `git diff --no-index`; review consensus constants and hash preimages first.
3. Reapply only the adaptation classes above, preserving or adding a `VENDOR:`
   comment for every non-mechanical semantic change.
4. Repin the native sources documented in
   `native/arweave_randomx/VENDOR.md` and `native/arweave_vdf/VENDOR.md`.
5. Run the deterministic device suites and the explicit live account
   transition:

```shell
rebar3 device test --devices \
  dev_arweave,dev_arweave_block,dev_arweave_block_index,\
dev_arweave_merkle,dev_arweave_mining,dev_arweave_spora,\
dev_arweave_storage,dev_arweave_tx,dev_arweave_vdf,dev_arweave_wallets

rebar3 device test --devices dev_arweave \
  --test all:live_account_transition --timeout 1800
```

The packing has one further check that no amount of self-consistency can
replace. `dev_arweave_spora_test_vectors:mainnet_entropy_test/0` generates the
entropy for one bucket of the mainnet weave and compares its hash against the
entropy an upstream Arweave node actually enciphered that bucket with -- derived
from that node's own disk, by holding the same range twice, once packed and once
not, and taking the difference. Repin it if the entropy derivation changes
upstream; a mismatch there means blocks nothing accepts.

```shell
rebar3 device test --devices dev_arweave_spora --test all:mainnet_entropy_test
```

The live vector is required because public peers prune historical wallet lists;
the repository intentionally carries no frozen mainnet account fixture.
