# Arweave storage-layout sources

This directory contains the Arweave chunk-storage arithmetic needed to read
the storage modules an Arweave node writes.

| Field | Value |
|---|---|
| Upstream | `https://github.com/ArweaveTeam/arweave` |
| Commit | `50e47de6d054afefdee112fa124695eb8d0176fc` |
| Release | `2.9.6-alpha1` (`RELEASE_NUMBER 92`) |
| Source root | `apps/arweave/src` and `apps/arweave/include` |
| License | GNU GPL v2; see `LICENSE.md` in this directory |

The `ar_` modules retain upstream naming, formatting, records, and tests
where they were copied whole. HyperBEAM style is used by the `lib_arweave_*`
integration modules, not retrofitted into this directory.

## Included surface

The on-disk layout of a storage module: how a module directory is named for
its weave range and packing (`ar_storage_module`), and where a chunk sits in
a chunk file (`ar_chunk_storage`). The corresponding constants are in:

```text
src/core/include/ar_chunk_storage.hrl
src/core/include/ar_consensus.hrl
```

Each file carries a `VENDOR:` note where it deviates from upstream; the
deviations are confined to reading configuration through arguments instead
of the Arweave node's own configuration store.
