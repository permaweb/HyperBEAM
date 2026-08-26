# Published Arweave indexes — format specification

Two indexes, published as Arweave transactions, read in place by byte-range
requests. Both are LMDB 1.0 databases with identical structure and differ only
in item layout.

## 1. Container

| | |
|---|---|
| format | LMDB 1.0, `MDB_DATA_VERSION` 3, 64-bit little-endian |
| page size | 65,536 (`mdb_env_set_pagesize`) |
| main database | `MDB_DUPSORT｜MDB_DUPFIXED`, exactly **one** entry |
| main key | `<<0>>` (one byte) |
| contents | the whole index is that key's duplicate set |
| build | `MDB_APPENDDUP`, items in ascending order, one pass |
| tags | `content-type: application/x-lmdb`, `lmdb-page-size: 65536`, `lmdb-data-version: 3`, `index-kind: offset｜match`, `index-version: 2` |

The single main-DB entry carries `F_SUBDATA`; its 48-byte data is an `MDB_db`
giving the sub-database's `md_pad` (item size), `md_root` and `md_depth`. The
sub-database's leaves are `P_LEAF2`: **no node header, no slot array, no
even-alignment padding**. Item *i* is at `page + 24 + i * md_pad`; the item
count is `lower >> 1`.

Measured overhead: **0.05 bytes per item** (17.05 against 17.00 raw, 10 M rows).

Items are fixed width and big-endian, so `memcmp` order is numeric on the
leading field. All reserved bits are zero.

## 2. Offset index — 21-byte items

Maps a data-item ID to its byte range in the weave.

```
bit  167..88   id        80   first 10 bytes of hb_util:native_id(ID)
bit   87..84   type       4   0=tx@1.0 1=ans102 2=ans104@1.0 3=httpsig@1.0
bit   83..34   offset    50   absolute weave offset of the first byte (1.1 PB)
bit   33..0    length    34   byte length of the item (17 GB)
```

Lookup: `MDB_GET_BOTH_RANGE` on `<<Id:80, 0:88>>`, then confirm the leading 80
bits match.

Excluded: pending markers, relative/bundle-child forms, and any row whose
offset exceeds the weave or whose length exceeds 2³⁴.

Truncation to 10 bytes is safe because a collision is detected, not believed:
`hb_store_arweave:load_item/4` and `load_tx/4` recompute the deserialized
item's ID and return `{error, {id_mismatch, _, _}}` on mismatch.

## 3. Match index — 17-byte items

Maps a `key=value` predicate to the weave offsets of matching items.

```
bit  135..56   hash      80   first 10 bytes of SHA-256(predicate)
bit   55..7    offset    49   absolute weave offset of the item (562 TB)
bit    6..0    reserved   7   zero
```

`predicate = <<"~match@1.0/", LowerCaseKey/binary, "=", Value/binary>>`. The
key is lower-cased; the value is **not** normalised.

Every predicate key is a key of the base message under commitment-extension
resolution: indexing `Key=Value` asserts that resolving the message at `Key`
yields `Value`. One row per indexed predicate per item:

| predicate key | value |
|---|---|
| each tag name | tag value |
| `committer` | committer address, per commitment |
| `field-target` | target address, when present |
| `parent` | containing bundle item id (nested items) |
| `commitment-device` | `ans104@1.0` |

The encoding tags the codec consumes (`bundle-format`, `bundle-version`) get
no rows; the legacy query surface translates requests for them. Legacy
`owners`/`recipients`/`bundledIn` arguments map onto `committer`,
`field-target` and `parent` inside `~query@1.0/arweave`.

`id` needs no row — the offset index answers it. Block height needs no row — it
is a range over the offset ordering.

Because `hash` leads and `offset` follows, every row for one predicate is
contiguous and ordered by weave position, which is approximately chronological.

## 4. Reading

Page size divides the 256 KiB Arweave chunk, and a transaction's data begins
exactly on a chunk boundary (verified on three transactions). **One page read
is therefore always exactly one chunk, never two.**

| index | items/leaf | sub-DB depth | cold reads | warm |
|---|---|---|---|---|
| offset (3.7 G items) | 3,119 | 3 | 5 | 1–2 |
| match (27.9 G rows) | 3,853 | 4 | 6 | 1–2 |

Cold includes the meta page and the main-DB leaf. Those two, the sub-DB root
and L3 are constant for an immutable file: caching them (5 pages, 320 KB) gives
2 reads; caching L2 as well (37 MB offset, 196 MB match) gives 1.

## 5. Operations

**Point lookup** — seek, compare the leading field, read the item.

**Range scan** — seek `<<Hash:80, From:49, 0:7>>`, walk forward while the
leading 80 bits are unchanged, stop at `limit`. Leaf pages are 99.93%
physically contiguous under `MDB_APPENDDUP`, so a scan is a contiguous byte
range and can be fetched with concurrency.

**Intersection** — leapfrog. Set `Cursor = 0`; for each predicate seek
`from = Cursor, limit = 1`; if it returns `O > Cursor` set `Cursor = O` and
restart; when all predicates return `Cursor`, emit it, set `Cursor = Cursor+1`.
Never reads more of any predicate than the candidates it rejects.

**Pagination** — the cursor is the weave offset. Stateless, and valid across
any node and any layering.

**Layering** — a delta is another index of the same shape. Point lookups fall
through the store list; scans add one cursor to the same k-way merge. Cursors
are layer-independent because the offset is global.

## 6. Limits

- **Exact match only.** Hashing destroys value ordering: no `value > x`, no
  prefix match, no sort other than by offset.
- **No `data` field from the match index.** ANS-104 items do not self-delimit;
  `deserialize_header/1` takes the data as "everything remaining". Returning
  `data` or `data.size` needs the offset index. The item's **ID, owner, target,
  anchor and tags are all recoverable from one chunk** at the offset, since
  `id = sha256(signature)` and every header field is self-delimiting.
- **RedStone items are excluded by policy** (86% of items; tag signature
  `dataFeedId/dataServiceId/signerAddress/timestamp/type`). Queries for them
  return no results rather than an error. This is a contract, not a bug.
- **A published index is a snapshot.** Correctness comes from a local writable
  store ahead of it; republication is a bootstrap optimisation.

## 7. Sizes

At 3.7 G non-RedStone items, 5.5 tags each (77-item sample, ±20%):

| index | rows | size | cost |
|---|---|---|---|
| offset | 3.7 G | 78 GB | 870 AR |
| match | 27.9 G | 476 GB | 5,320 AR |
| | | **554 GB** | **6,190 AR** |

## 8. Audit checklist

1. Meta page: magic `0xBEEFC0DE`, version 3, page size 65,536.
2. `dbs[1].md_flags` has `MDB_DUPSORT｜MDB_DUPFIXED`; main DB has 1 entry.
3. That entry's node carries `F_SUBDATA`; its `MDB_db.md_pad` is 21 or 17.
4. Sub-DB leaves have flags `P_LEAF｜P_LEAF2`; items per page equal
   `(65536 - 24) div md_pad` — 3,119 or 3,853.
5. Items strictly ascending, no duplicates, reserved bits zero.
6. Every offset < weave size; every length < 2³⁴.
7. Spot-check: resolve known transaction IDs and compare against
   `GET /tx/<id>/offset`.
