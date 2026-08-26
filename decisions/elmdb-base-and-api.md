# Decision: elmdb base lineage and API surface

## The prompt, as understood

Build "the elmdb that it [HyperBEAM] uses" — via a new worktree in
`~/src/elmdb` — with: DUPFIXED + DUPSORT support, LMDB 1.0 by default with
64 KiB pages, and the positioned-cursor reads that `hb_store:list`'s new
`from`/`limit` keys and the published-index format need.

## Options

1. Base on the pinned Rust lineage (`feat/read-prefix`, which HyperBEAM edge
   pins at faa7623; local tip ae2f1f2 adds ordered direct batch writes).
2. Base on `feat/c` (C NIF, LMDB 1.0 + chacha8 encryption) — a different
   lineage that edge does not use, forked pre-read_prefix, built for the
   LapEE encrypted-store effort.
3. Reuse `feat/dupsort-cursors` (the avoided branch's elmdb).

## Decision

Option 1: new branch `feat/dup-sets` off `feat/read-prefix` @ ae2f1f2, in
worktree `~/src/elmdb`. "The elmdb that it uses" names the pinned lineage;
switching lineages (option 2) is a larger integration change (build hooks,
re-derived read paths) that the brief does not ask for. Option 3 is ruled
out by instruction; we forge our own implementation. Where our API happens
to converge with LMDB's own vocabulary (dupsort/dupfixed flags, append
writes), that is convergence on LMDB's names, not adoption of the avoided
branch's work.

LMDB 1.0 C sources are vendored from openldap/openldap @
bac0ccfc4fbe17867349357987557bfdf1b680df (libraries/liblmdb) — the exact
pin `feat/arlmdb` verified the page format against. Vendoring is required
either way: crates.io has no LMDB-1.0 sys crate. "LMDB 1.0 by default"
falls out of the vendoring: every env this elmdb opens is 1.0/format 3.
Existing 0.9 store files fail with `version_mismatch`; stores are derived
data, so the migration is a rebuild — no compat shims.

## API surface (the contract W2 builds against)

- `env_open/2` gains `{page_size, N}` (power of two, 512..65536; applied via
  `mdb_env_set_pagesize` before the map exists), `read_only` (MDB_RDONLY),
  and `no_subdir` (MDB_NOSUBDIR: path names the data file itself).
- `db_open/2` gains `dupsort` and `dupfixed` (dupfixed implies dupsort).
- `put/3` on a dupsort DB adds a duplicate under the key (LMDB semantics);
  the write overlay is keyed by {Key, Value} pair there, plain Key otherwise.
- A sorted bulk append path writing with MDB_APPEND/MDB_APPENDDUP in one
  transaction, erroring on out-of-order input (blends with the existing
  put_batch/put_batch_direct family).
- A positioned duplicate read: seek within one key's duplicate set to the
  first value >= From (MDB_GET_BOTH_RANGE), walk forward (or backward) up
  to Limit values. This is the store-side primitive under `hb_store:list`'s
  `from`/`limit`.
- `list/2` keeps today's semantics; a bounded variant may bound the result
  (a hierarchical child walk cannot be work-bounded — child keys are not
  contiguous).

Everything else (read_prefix, match, iterators, env lifecycle) unchanged.
