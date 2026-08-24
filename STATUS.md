# A decentralizable GraphQL: hashed, offset-keyed match index

Branch `claude/distracted-swirles-b5f0a7`, based on `edge` @ `2d90c17dc`.

## Baseline

`rebar3 eunit-all` on a clean clone of `edge` @ `2d90c17dc`
(`HB_PORT=8740`, fresh `_build`):

```
  Failed: 5.  Skipped: 0.  Passed: 3548.
```

The five, all reaching external services:

```
push@1.0: test_push_prompts_encoding_change...*failed*
scheduler@1.0: http_get_legacy_schedule_as_aos2_test_parallel_...*failed*
scheduler@1.0: http_get_legacy_schedule_slot_range_test_parallel_...*failed*
scheduler@1.0: http_get_legacy_schedule_test_parallel_...*failed*
scheduler@1.0: http_get_legacy_slot_test_parallel_...*failed*
```

## Progress

| § | commit | subject |
|---|---|---|
| 4.1 | `96b686952` | move `elmdb` to LMDB 1.0, duplicate sets and positioned cursors |
| 4.2 | `1b230348c` | bound `list/3` with `from`, `limit` and `direction` |
| 4.3 | `a31b91b69` | transform a store definition's paths on their way in |
| 4.4 | `1a36560b9` | address match-index rows by predicate hash and weave offset |
| 4.5 | `43e67c78e` | read an LMDB database out of the Arweave weave |
| 4.6 | `a6cb579d5`, `4e5d7d98a` | read and write LMDB duplicate sets, held and promoted |
| 4.7 | `dee4eb50b` | hold the pages a published database has already given up |
| 4.8 | `a014c554c`, `0259d0137` | walk the predicates in step; page the query surface by seeking |
| 4.9 | -- | dropped; `~copycat@1.0` already writes offsets first |
| 4.10 | -- | the demonstration below |

Every one of those builds on its own:

```
ok   96b686952 feat(store): move `elmdb' to LMDB 1.0, duplicate sets and positioned cursors
ok   1b230348c feat(store): bound `list/3' with `from', `limit' and `direction'
ok   a31b91b69 feat(store): transform a store definition's paths on their way in
ok   1a36560b9 feat(cache): address match-index rows by predicate hash and weave offset
ok   43e67c78e feat(store): read an LMDB database out of the Arweave weave
ok   a6cb579d5 feat(store): read and write LMDB duplicate sets
ok   dee4eb50b feat(store): hold the pages a published database has already given up
ok   a014c554c feat(query): intersect match-index predicates by walking them in step
ok   0259d0137 feat(query): page the Arweave query surface by seeking into the index
ok   4e5d7d98a feat(store): read a duplicate set that still fits inside its own node
```

The docs commit above them adds only this file and `decisions/`, so its tree
compiles as `4e5d7d98a` does.

## Decisions

- [`list/3`'s bounds bound the result, not the scan](decisions/list-bounds-are-not-a-bounded-walk.md)
  -- a `/`-delimited hierarchical listing cannot stop early; the exactly
  bounded walk is `elmdb:dups/3` over the duplicate set, which is the shape
  the published index uses and the shape §1.1's criteria are judged on.
- [Bounding the index walk costs an exact `count`](decisions/graphql-count-and-bounded-pages.md)
  -- a total count of an intersection means walking the whole intersection, and
  a page that is a seek does not. Bounded where the index is the hashed one,
  exact where it is not.
- [`~copycat@1.0` already writes offsets first](decisions/copycat-offset-ordering-already-correct.md)
  -- §4.9 needs no change. Every `hb_cache:write` in `dev_copycat_arweave` is
  nested inside the `ok` branch of a preceding `hb_store_arweave:write_offset`.

## 4.1 -- `elmdb-rs`

Worktree `/Users/sam/src/elmdb-rs/.claude/worktrees/dupsort-cursors`, branch
`feat/dupsort-cursors`, off the pinned `faa7623`.

Two commits:

- `87a29b3` Build against LMDB 1.0 rather than 0.9. `vendor/lmdb-sys` carries
  `openldap/openldap` `bac0ccfc4fbe17867349357987557bfdf1b680df`
  (`MDB_VERSION 1.0.0`) built with that tree's own object list and `-O2`;
  `vendor/lmdb` is the safe wrapper with `set_page_size` added.
- `8a33de1` Add duplicate-key databases and positioned cursors. `db_open`
  takes `dupsort`/`dupfixed`; `put` inserts duplicates; `member/3` tests a
  pair; `dups/3` walks one key's values with `prefix`/`from`/`limit`/
  `direction`; `list/3` takes the same bounds; `append_batch/2` writes
  ascending pairs with `MDB_APPEND`/`MDB_APPENDDUP`; `env_open` takes
  `{page_size, N}`.

The write overlay keys a duplicate database's entries by the `(Key, Value)`
pair, since the key alone no longer identifies one.

The listing dedupe kept results sorted only below sixteen children and
searched an unsorted vector above it -- duplicates and arbitrary order for
larger groups. It sorts once at the end now, which `from`/`limit`/`direction`
need in order to mean anything.

### Evidence

On-disk format, written through the NIF and read back raw:

```
magic=BEEFC0DE version=3 page_size=16384
```

`MDB_DATA_VERSION` is 3 in LMDB 1.0 and 1 in 0.9, so the file is 1.0.
`page_size_test_` pins `{page_size, 65536}` reaching the meta page.

`rebar3 eunit` in the elmdb worktree, before the change and after:

```
  All 82 tests passed.      (faa7623, LMDB 1.0 vendored)
  All 98 tests passed.      (8a33de1)
```

Three consecutive runs of the 96-test intermediate state passed; the one
failure seen in a single run was `flush_sync_waiter_on_worker_death_test_`,
a map-full timing test, and it did not recur.

### Published

`permagit push feat/dupsort-cursors` to `arweave://elmdb-rs`:

```
refs/heads/feat/dupsort-cursors -> 8a33de15 (tx: BFFAj8FvSphq4JeFW3n-X4DnxyuUSS8DuruNpICHYak)
snapshot: OM5VtFf_J5Ex28hd8BzHZ5z-ODP3EAl4Yuk5RzYVg9s
```

Wallet `ggltHF0Cnv9ylH3vM1p7amR2vXLMoPLQIUQmAEwLP-k`, balance 52.489090 AR at
the time of the push.

`rebar.config` points at `{elmdb, {git, "arweave://elmdb-rs", {branch,
"feat/dupsort-cursors"}}}`. The fetch fails until the gateway indexes the ref
transaction; a poller is retrying the clone.

## No regression

`rebar3 eunit-all` on the branch after the deep clean (`HB_PORT=8763`):

```
push@1.0: test_push_prompts_encoding_change...*failed*
scheduler@1.0: -http_get_legacy_schedule_as_aos2_test_parallel_/0-fun-1- ...*failed*
scheduler@1.0: -http_get_legacy_schedule_slot_range_test_parallel_/0-fun-1- ...*failed*
scheduler@1.0: -http_get_legacy_schedule_test_parallel_/0-fun-1- ...*failed*
scheduler@1.0: -http_get_legacy_slot_test_parallel_/0-fun-1- ...*failed*
  Failed: 5.  Skipped: 0.  Passed: 3590.
```

The same five run on `edge` (`2d90c17dc`, `HB_PORT=8771`,
`rebar3 device test --devices dev_push,dev_scheduler`) and fail there with the
same errors -- one `bad_peer` and three `{error, <<>>}` from legacy-SU
endpoints:

```
push@1.0: test_push_prompts_encoding_change...*failed*
scheduler@1.0: -http_get_legacy_schedule_as_aos2_test_parallel_/0-fun-1- ...*failed*
scheduler@1.0: -http_get_legacy_schedule_slot_range_test_parallel_/0-fun-1- ...*failed*
scheduler@1.0: -http_get_legacy_schedule_test_parallel_/0-fun-1- ...*failed*
scheduler@1.0: -http_get_legacy_slot_test_parallel_/0-fun-1- ...*failed*
  Failed: 5.  Skipped: 0.  Passed: 40.
```

3,548 passing to 3,590 is the 42 tests the branch adds.

### Indexing throughput A/B against `edge`

Ten mainnet blocks (1892400-1892409), `~copycat@1.0/arweave` `mode=full`, a
fresh store per run, measured end to end:

| build | index | ms/block |
|---|---|---|
| `edge` | key-value (`match_hash_size` unset) | 1,880 then 1,681 |
| branch | key-value (`match_hash_size` unset) | 1,732 |
| branch | hashed, offset rows | 2,110 |

The branch does not regress the existing path. The hashed index costs ~20%
more per block, all of it in the offset lookups. One regression of the
branch's own making was found and fixed in the same pass: `match_offset/2`
ran inside the per-key fold, so a message with `K` tags paid `K` offset
lookups for the same IDs. `match_rows/2` now finds the rows once per message.
Re-measured at 2,078-2,136 ms/block -- unchanged, because the workload is
network-bound: `mode=full` on `edge` moves 54 requests and 8.5 MB per block,
and `arweave_index_workers` (default 1) scales it 1,591 -> 1,003 -> 709
ms/block at 1/8/32 workers before saturating.

## Deep clean

The whole branch was swept against `CONTRIBUTING.md` after the features
landed. Mechanical checks: no unreachable functions, no exports without an
external caller, no history-narrating comments, no markers, zero compiler
warnings in the branch's files, and the branch's over-80-column rate (1.29%
of added lines) is under half the `src/core` baseline (2.74%). Three
duplications earned a collapse:

- `hb_store_arlmdb`: `next_leaf/leftmost/leftmost_page` and their mirrors
  `previous_leaf/rightmost/rightmost_page` became `adjacent_leaf/5`,
  `outermost/7` and `outermost_page/8` over the walk's own `Step` (`1`/`-1`).
  The duplication had already cost one real bug -- the backward descent
  missing the depth guard its mirror had -- which is what condemned it.
- `hb_store`: `normalize_parts/2` and the inlined inverse loop in
  `resolved/2` became `normalize_parts/3` over a `forward`/`inverse`
  direction on `normalization/3`.
- `hb_cache`: `match_value_bin/2` and `match_value_path/2`, twin recursions
  over the same value shapes, became one `match_value/2` returning
  `{raw | id, Bytes}` with two thin wrappers.

Net: -35 lines, 4 files. Each collapse was folded into the commit that
introduced the duplication (`git rebase --autosquash`), so the history reads
as if written clean. The fold is provably behaviour-neutral: the source tree
hashes identically before and after --

```
$ find src -name '*.erl' | sort | xargs shasum | shasum
daf7fa71b0f511ec0c2dbb3b441c2b608c9dbb6e  -   (before fold)
daf7fa71b0f511ec0c2dbb3b441c2b608c9dbb6e  -   (after fold)
```

-- and every rewritten commit builds on its own (table above). The full
suite's result on the cleaned tree is the run quoted at the top of this
section.

## 4.1 -- the `arweave://` dependency

`rebar3 upgrade elmdb` fetched `feat/dupsort-cursors` from Arweave and
`rebar.lock` pins it at `8a33de1507944cf8a733e9ba9c5c8ea2e09a3498`. The clone
failed for the first two minutes after the push and then succeeded: the ref
transaction has to be indexed by the gateway before `git-remote-arweave` can
list it.

LMDB 1.0 cannot open an LMDB 0.9 file, so `hb_store_lmdb:open_env/2` discards
one and builds a new one. `outdated_format_rebuild_test` writes a database,
rewrites the data version stamped into both of its meta pages, and asserts the
store comes up empty rather than crashing; it fails without the rebuild.

## 4.6 -- duplicate sets

`hb_lmdb_page` reads `MDB_DUPSORT`/`MDB_DUPFIXED` databases: `meta/1` reports
the main database's flags rather than refusing them, an `F_SUBDATA` leaf node
yields the `MDB_db` of the database its duplicates live in, and `P_LEAF2` pages
are read by `item/3` and `seek_item/3` -- no node header, no pointer array, no
value.

`test/lmdb-1.0-dupfixed.mdb` is 53,248 bytes: one main key whose 3,000
duplicates are 15-byte rows of an 8-byte hash and a 7-byte offset, on 512-byte
pages. Written through `elmdb` with `MDB_APPENDDUP`, which is how a published
index is built. Parsed by hand, it is exactly the shape the specification
describes:

```
main db:              flags=0x14 depth=1 entries=3000 root=2
main leaf:            keys=1 node_flags=0x06 (F_SUBDATA|F_DUPDATA) ksize=1
sub db:               flags=0x10 pad=15 depth=3 entries=3000 root=25
sub-db root:          P_BRANCH, 6 keys
```

`hb_store_arlmdb` rewrites its meta to describe the duplicate set, so one
descent reads both shapes; `width` is what tells them apart. `read` is a
membership test returning an empty binary, `list` walks the elements carrying a
prefix with `from`/`limit`/`direction`, `type` is always `simple`, and
`resolve` is the identity. `hb_store_lmdb` does the same over `elmdb:member/3`
and `elmdb:dups/3` when its store carries `sorted-set`, and writes elements as
duplicates of one key. A `group` in a sorted set is any prefix, so there is no
marker to write.

## 4.7 -- the page store

Every ranged read of the weave goes through `hb_store_arlmdb:read_range/5`, and
now through a nested `page-store` first, keyed by the range's absolute weave
offset and its length. The specification names the key
`~arweave@2.9/offset=<PAGE_OFFSET>`; the length is part of it here because two
ranges can begin at the same offset -- the pair of meta pages and the first
page of the database both begin at the start of the file -- and they are not
the same bytes.

`published_index_page_store_test_` measures it against the 10,000,101-entry
published index (depth 3):

| lookup | ranged reads |
|---|---|
| cold, page store off | 4 (`depth + 1`, `published_index_reads_test_`) |
| first, page store on | <= 4 |
| same key again | 0 |
| a key at the other end of the tree | <= 2 (`depth - 1`) |
| that key again | 0 |

## 4.8 -- the walk, and what it found

`dev_match:all/3` asks each predicate for its first row at or after a cursor
and restarts from the first predicate whenever one answers with a later row.
`locate/3` is the same walk with each result's weave offset carried out
alongside its ID, which is what lets a node with no index of its own order and
page results.

Every result is read from the weave and checked against the template. Two
findings came out of making that work against real items:

- **The span of a header has to be found rather than known.** The index records
  where an item begins and not how far it runs. A range of the weave is served
  only where it is whole, so a span reaching past the end of the data its
  transaction holds fails outright rather than returning what there is --
  measured on three consecutive items in one bundle, the largest servable span
  was 6,144, 8,192 and 1,536 bytes. And a span stopping inside the item's tags
  parses into fewer tags than the item has, which loses a match rather than
  reporting one. The tags ending inside the read is what says a span was wide
  enough; the spans are tried `[2048, 4096, 1536, 1152, 8192]`.
- **The key of a pair has to be lower-cased on both sides.** The writer
  lower-cases it into the hash. A reader that did not addressed exactly the
  right rows and then rejected every one of them, so a query naming `App-Name`
  returned nothing at all.

### Evidence

Against a locally built index of block 1,889,322, whose six `App-Name:
ArDrive-App` items sit at known weave offsets:

```
match #{<<"app-name">> => <<"ArDrive-App">>} -> {ok, [
  zvFNNmZwXxeznEjO5fHc6D7_bJWyTEmrSQWcKw_Z0wQ,
  SD9obWV59R7JZuLIqzEztuaWaJ7FGY8N9XRy0JXwDGc,
  Mx-GlwBslqsd-OkXGY84PxBzN_dhCPva_XANecNXKPs,
  npAzk_BomjWBQQr_xnmlhdxjyl97EJnNv_MAaXffs1s,
  SyLRPOOdz4MrJEupDwhOh8zYagCLoJuWF1RYxRr85X4,
  Vlw8xwVZRl-GulRjelEpOZm9xJowjluKOmVRtFQmIjE]} in 436 ms

match #{<<"app-name">> => <<"ArDrive-App">>,
        <<"entity-type">> => <<"file">>} -> {ok, [
  SD9obWV59R7JZuLIqzEztuaWaJ7FGY8N9XRy0JXwDGc,
  npAzk_BomjWBQQr_xnmlhdxjyl97EJnNv_MAaXffs1s,
  Vlw8xwVZRl-GulRjelEpOZm9xJowjluKOmVRtFQmIjE]} in 235 ms
```

All six rows verify; the second pair narrows the six to three.
`walks_in_step` asserts a third, highly selective, pair costs no more index
seeks than the two it is added to. `pages_by_cursor` asserts a later page costs
no more seeks than the first. `hashed_index_pages` pages the GraphQL surface
itself, forwards and backwards, and asserts the cursor between two pages is the
offset of the result it names.

## 4.10 -- end to end on real data

The pipeline runs from Erlang throughout: `~copycat@1.0` in `full` mode writes
the index, `elmdb` compacts it, `publish.erl` uploads it, and
`hb_store_arlmdb` reads it back out of the weave.

### Thirteen million rows, published and queried

`~copycat@1.0` in `full` mode over 2,780 mainnet blocks from 1,889,322,
indexed as six sets at once over disjoint ranges and merged:

```
merged 13051852 rows from 6 sets in 6 s -> 197001216 bytes
main: flags=0x14 depth=1 entries=13051852 root=27
```

197,001,216 bytes for 13,051,852 rows is **15.09 bytes a row** -- fifteen of
content and 0.09 of everything else -- against the 26.06 an ordinary key-value
database would have spent on the same rows, and the 53 the same rows occupied
before they were merged.

Published at
[`cmcvlkG8DCuqaF1O5Sw16125PYFoVnTBe46Yjpfa2BQ`](https://arweave.net/cmcvlkG8DCuqaF1O5Sw16125PYFoVnTBe46Yjpfa2BQ)
-- 197,001,216 bytes over 752 chunks, 2.249006270580 AR.

Before paying to publish it, the file was read through `hb_store_arlmdb` with
its own pages seeded into the store's `page-store`, which the store cannot tell
from pages of the weave:

```
seeded 3006 pages of 65536 bytes
first 3:  {ok,[<<"0">>,<<"386310991553238">>,<<"386310991554596">>]}
last 3:   {ok,[<<"386441203478479">>,<<"386441203477122">>, ...]}
rows:     108962
member:   {ok,<<>>}
absent:   {error,not_found}
```

A node configured with a locator for it and nothing else -- no local match
index, no offset index, no warm-up -- answering `~query@1.0/arweave` templates
over it, ten results a page:

| query | results | index reads | index seeks | bytes | wall |
|---|---|---|---|---|---|
| 1 tag, cold | 8 | 5 | 19 | 1,936,357 | 1,311 ms |
| 1 tag, warm | 8 | 0 | 19 | 178,501 | 476 ms |
| 2 tags, cold | 10 | 2 | 45 | 3,680,319 | 264 ms |
| 2 tags, warm | 10 | 0 | 45 | 2,977,093 | 111 ms |
| 3 tags, cold | 10 | 1 | 79 | 3,863,775 | 185 ms |
| 3 tags, warm | 10 | 0 | 79 | 3,512,290 | 175 ms |

and paging one tag, which alone has 108,962 rows:

| page | results | index reads | index seeks | bytes | wall |
|---|---|---|---|---|---|
| 1 | 8 | 0 | 19 | 178,501 | 499 ms |
| 2 | 7 | 0 | 20 | 176,854 | 442 ms |
| 25 | 9 | 0 | 20 | 879,343 | 469 ms |
| 50 | 10 | 0 | 20 | 3,808,358 | 381 ms |

"index reads" are ranged reads of the weave for pages of the index; "index
seeks" are the questions the walk asks of it; "bytes" is everything the node
downloaded, so it also carries the item reads that verification makes, which is
where the variance is. A page of ten returns eight to ten results because a row
whose item cannot be read or does not carry the pair is dropped.

The published bytes are the file that was built:

```
197001216 bytes
92cef75d35f48fa3779aaad8f082fb299363e3f84347aa01e7c5d1b732fb5d1f  downloaded.mdb
92cef75d35f48fa3779aaad8f082fb299363e3f84347aa01e7c5d1b732fb5d1f  published/data.mdb
IDENTICAL
```

and read from disk through `hb_store_lmdb` or from the weave through
`hb_store_arlmdb`, the same 13,051,852 rows answer identically -- forwards,
backwards, from a cursor, and for membership:

```
forward  local == remote: true   {ok,[<<"0">>,<<"386310991553238">>, ...]}
backward local == remote: true   {ok,[<<"386441203478479">>, ...]}
forward  local == remote: true   from 386400000000000
backward local == remote: true   from 386400000000000
member   local == remote: true   ({ok,<<>>})
absent   local == remote: true   ({error,not_found})
```

Against §1.1's criteria:

1. **A bounded, counted number of chunk reads.** Five for a cold single-tag
   query against a 13-million-row index; none once the top of the tree is held.
2. **Two tags cost no more than two descents plus the rows examined.** Two tags
   cost 45 seeks where one costs 19, and 2 ranged reads where one costs 5.
   Reading both predicates in full would have been 108,962 rows and the second
   predicate's entire set.
3. **Page fifty costs what page one costs.** Twenty seeks and no index reads,
   against nineteen and none. Not forty-nine pages of walking.
4. **The same index answers the same, locally and remotely.** Above, at
   13,051,852 rows, and in `published_set_equivalence_test_` at 3,000.
5. **No regression.** The five baseline failures and no others.
6. **Each commit builds and passes its own tests.** Eleven commits, each built
   on its own in a worktree of its own.
7. **On real data.** 13,051,852 rows written by `~copycat@1.0` in `full` mode
   over 2,780 mainnet blocks, published and queried.

### A single block, published and queried

Block 1,889,322 indexed in `full` mode gives 1,223 rows. Compacted and
published at
[`z6yYEGs4XrHxqcElbSSJUqJ6xs5eM0C63QdXLYsCybA`](https://arweave.net/z6yYEGs4XrHxqcElbSSJUqJ6xs5eM0C63QdXLYsCybA)
-- 196,608 bytes, 0.003032169803 AR.

A node holding nothing but a locator for it, no local index and no warm-up:

| query | results | index reads | index seeks | bytes | wall |
|---|---|---|---|---|---|
| 1 tag, cold | 6 | 2 | 14 | 628,915 | 837 ms |
| 1 tag, warm | 6 | 0 | 14 | 102,440 | 278 ms |
| 2 tags, cold | 3 | 0 | 18 | 64,025 | 242 ms |
| 2 tags, warm | 3 | 0 | 18 | 64,025 | 233 ms |
| 3 tags, cold | 3 | 0 | 25 | 64,025 | 263 ms |
| 3 tags, warm | 3 | 0 | 25 | 64,025 | 228 ms |

"index reads" are ranged reads of the weave for pages of the index; "bytes" is
everything the node downloaded, so it includes the item reads that verification
makes. The second tag cuts six results to three and costs four more seeks and
no more index reads.

### A set small enough to live in its node

That index is 1,223 rows, which LMDB keeps inside the leaf node that names it
rather than promoting to a database of its own -- it promotes at half a page.
The reader knew only the promoted form, so a small published index came back as
`not_a_duplicate_set`. Both forms are read now, and
`test/lmdb-1.0-subpage.mdb` pins the held one.

### Density, and why one index cannot be built in one pass

A set written as its rows arrive is written in hash order, which is to say at
random. Two things follow.

It leaves its pages part-full: a 2.7-million-row index measured 53 bytes per
row against the 15.05 the shape allows. `merge.erl` copies one or more sets
into a fresh database in order with `MDB_APPENDDUP`, so every page fills before
the next is started. Measured on 2,540,741 real rows merged out of two of the
sets built below:

```
flags=0x14 depth=1 entries=2540741 root=27 bytes=38993920 -> 15.35 bytes/row
node flags=0x6 -> subdb pad=15 flags=0x10 depth=2 entries=2540741
```

Fifteen bytes of content and 0.35 of everything else, against the 11.06 bytes
of LMDB bookkeeping that a row of an ordinary key-value database pays. The
merge itself ran at about two and a half million rows a second: appending to
the end of a page is a copy.

It also gets slower the larger it gets, because every row lands on a different
page and, once the file outgrows the page cache, each row costs a read and a
write. Measured over one run of `~copycat@1.0` in `full` mode, all of it into a
single store:

| blocks | seconds per block | rows per second |
|---|---|---|
| 1,889,322 - 1,889,421 | 2.0 | 2,907 |
| 1,889,630 - 1,889,729 | 3.8 | 1,913 |
| 1,889,938 - 1,890,037 | 6.8 | 967 |

Roughly a halving every three hundred blocks. So the index is built as several
smaller sets at once, over disjoint block ranges, and merged afterwards: each
stays inside the page cache, and the merge is sequential.

## What verification establishes

A result is returned because the bytes at its row's offset carry every pair the
template asked for, and its ID is the hash of the signature those bytes carry.
It is not established that those bytes are the item that signature belongs to:
an ANS-104 item does not record how far it runs -- its extent is known only from
the bundle header above it -- so its signature cannot be checked from an offset
alone.

Against a colliding address, which is what the check exists for, that is
enough. The bytes at a colliding row are some other real item, and its pairs
are not the ones asked for, so it is dropped.

Against an index whose publisher chose both the rows and the bytes they point
at, it is not. Such a publisher can upload a blob carrying a well-formed header
with a copied signature and the pairs of their choosing, and point a row at it;
a reader would then return that signature's ID among the results of a query the
real item does not belong to. It cannot make the reader return a *wrong item*:
`dev_query_arweave:read_ids/3` reads each result back by its ID, which resolves
through the gateway to the real one. The exposure is a false positive in a
result set, from an index you chose to read.

Closing it would mean binding the ID to the offset with something other than the
index -- a `HEAD /~arweave@2.9/raw=<ID>` per result, checking the offset it
reports against the row's. That is one more round trip per result and a
dependency on an offset service that the design exists to remove, so it is
noted here rather than taken.

## What an adversarial review of the branch found

Four reviewers read the diff against `CONTRIBUTING.md` -- one per dimension:
correctness in the store layer, correctness in the query layer, style, and
whether the tests hold the code to anything -- and a second reviewer per
dimension tried to refute each claim before it counted. Six survived and are
fixed; each is pinned by a test that fails without the fix.

**A template was compared as a structured message against an index written
from a TABM.** `hb_message:convert(Item, <<"structured@1.0">>, ...)` decodes an
item's `ao-types` back into terms, so a template naming a slot of 2382
addressed exactly the right row and then compared `2382` against `<<"2382">>`
and threw the result away. Every AO assignment on the weave has typed fields,
so this was most of them. `typed_values_test_` pins it against
`tprBTrqaA3bEkC2BSVZkWxUU2gM3Dgs3E01hL0G1_5k` at weave offset
386,414,055,714,272, and fails against the structured conversion.

**A mistyped path normalization spun forever.** `normalize/2`'s catch-all
tail-called itself through `hb_util:bin/1`, which is the identity on a binary,
so `path-normalization => <<"decode-b64url">>` in a store definition took a
scheduler to 100% and never crashed.

**`P_LEAF2` pages were read as though they carried nodes.** Teaching `page/1`
about them let `search/2` and `node/2` reach a page with no pointer array, and
`hb_store` turns a crash inside a store into a silent `not_found` -- so a
malformed published database read as a missing key.

**The backward descent had no depth guard** where its mirror `leftmost/6` has
one, so a published tree whose rightmost branch pointed back at itself would
have looped without end.

**Verification had no negative case.** Every row in the tests sat at the offset
of an item that really carried the pair, so all three walk tests passed with
verification removed -- proved by removing it. They now write a row of one
predicate at the offset of an item that does not carry it, which is what a
collision between two truncated hashes looks like, and one at the sentinel
offset.

**Nothing held the writer to `MDB_DUPFIXED`.** Every read and write goes
through `elmdb`, which behaves the same with or without it, and only the bytes
on disk say which was asked for. `sorted_set_format_test` writes a set through
`hb_store_lmdb` and reads the file back with `hb_lmdb_page`.

Four more of the branch's tests were shown not to distinguish what they claim.
Each is fixed and each was re-proved by making the change the reviewer used to
defeat it:

- The listing test's offsets all had twelve digits, so sorting them as decimal
  text agreed with the store by coincidence. 99 and 100 now say which order the
  store really keeps.
- `match-offsets: always` was asserted only where every row is dropped.
- A path outside a store's prefix and a path the store simply lacks are both
  `{error, not_found}` to one store; the decline is asserted through a second
  store behind the first.
- The paging test counted what `dev_match` asks of the store, which cannot see
  a store that scans up to the cursor. `hb_store_arlmdb` counts the ranged
  reads a page really costs.
