# Decision: producing an unpacked storage module on dev-2

## The prompt, as understood

The indexer assumes unpacked data. dev-2's two modules (105, 106; 3.3 TB
each) are replica_2_9-packed to the miner's address. Sam: "make sure that
node is terminated ... and unpacking one of the storage modules back to its
original form, such that you have an example to test your indexer against."

## Constraints

- md127 (the weave RAID) has 282 GB free; root has 438 GB. A full second
  copy of a module cannot exist anywhere on the machine.
- Unpacking replica_2_9 requires regenerating the packing entropy
  (RandomX-based) — CPU-bound; throughput must be measured before sizing
  the plan. The mining deployment at ~/arweave-miner has the built NIFs.
- The packed replicas are re-syncable from the public weave, but resyncing
  + re-preparing 3.3 TB costs days-weeks of wall clock.

## Decision

1. Verify first: unpack a sample (a few GB) into scratch space and verify
   every unpacked chunk against its merkle proofs from the node's chunk
   index (data_path → data_root), plus spot-checks against arweave.net
   /chunk. No in-place writes until sampled verification passes 100%.
2. Measure entropy/unpack throughput on ~40 cores (nice'd — other tenants
   share dev-2), and size the conversion to what finishes overnight:
   in-place conversion of module 106, file by file (read 2 GB chunk file,
   unpack, write converted file alongside on the same fs, fsync, rename
   over, preserving sparseness), renaming the module directory to
   storage_module_106_unpacked at the end. Module 105 stays packed.
3. If measured throughput cannot finish 3.3 TB overnight, convert as much
   of 106 as the night allows, front-loaded (the indexer reads
   sequentially; a contiguous unpacked prefix of the module is a complete
   test surface), and leave the converter running.

In-place conversion destroys the packed copy of 106. This is judged
authorized by the brief ("unpacking one of the storage modules back to its
original form") and bounded: the data is public and re-syncable; the miner
that used it is terminated per the same brief; 105 remains as a packed
example. The verification gate above is what makes the overwrite safe.
