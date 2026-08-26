# Decision: sector-band conversion instead of strict file-by-file

## The prompt, as understood

Convert module 106 in place "file by file, sequentially from the LOWEST
offset file upward", atomic-rename per file, highwater UNPACK-CURSOR so the
indexer can consume a growing unpacked prefix.

## The issue

replica_2_9 entropy geometry makes strict file-by-file conversion
infeasible: one footprint (32 x 8 MiB RandomX-squared entropies) covers
1024 buckets spaced one sector (3,515,875,328 B) apart across the WHOLE
3.6 TB partition — consecutive buckets in one file belong to 8000
DIFFERENT footprints. Converting one 2 GB file alone costs 8000x32 entropy
runs to use 1/1024 of their output: 1024x the minimal RandomX work
(~500 h at 40 cores for the module). Buffering unused slices needs ~3.3 TB
of RAM/disk; neither exists (282 GB free on md127).

## Options

1. File-by-file, per-chunk entropy: correct shape, 1024x compute. Months.
2. Single footprint-major pass (the prepare order): minimal compute
   (~29 min entropy at 40 workers), but no file completes until the very
   end — no usable prefix for the indexer while running, and in-place
   scattered writes with no atomicity story.
3. Sector bands: N passes over all 13,412 footprints, each pass XORing
   only the slices for 1024/N contiguous sectors. Compute = N x 29 min;
   the module converts front-to-back in 3.35/N TiB steps, cursor advances
   at band boundaries.

## Decision

Option 3 with N=8 (128 sectors, ~420 GiB per band): first unpacked prefix
lands within the first hour, total ~4-6 h. Atomic per-file rename is
replaced by a stronger invariant: byte-level slot adjudication makes every
write verified (sha256 vs the indexed data-path leaf chunk id, or
byte-equality with assembled entropy before zeroing) and idempotent, so a
crash at any point is recovered by re-running the current group (journal
checkpoints every 8 groups; torn 4-KiB-boundary writes are detected and
healed). Entropy-only slots are zeroed (prefix 0 + zero body) so the
converted module reads as a true unpacked module: nonzero prefix == real
chunk. UNPACK-CURSOR = absolute offset below which all files are fully
converted; advances 8 times.

Verification gate passed before any write: 39/39, incl. 30/30 external
byte-matches against arweave.net. See STATUS-unpack.md.
