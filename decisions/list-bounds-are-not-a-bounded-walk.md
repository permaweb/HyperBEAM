# `list/3`'s `from`/`limit` bound the result, not the scan

## The prompt as I understand it

§4.2 asks `hb_store:list/3` to take `from`, `limit` and `direction`, and says
of `hb_store_lmdb`: "seek once and read `limit` rows, never the whole child
list". §1.1's criteria 1-3 ask for a bounded, counted number of chunk reads per
query, and for page 50 to cost what page 1 costs.

## The issue

A hierarchical `list` returns *distinct child names*: the first path component
of every key under the prefix. It cannot stop after seeing `limit` of them.

Keys are ordered by `memcmp`. A child name may contain a byte below `/`
(0x2f) -- `-` (0x2d) is one, and every base64url name is full of them. So the
keys belonging to one child are **not contiguous**:

    h/a        -> child "a"
    h/a-b      -> child "a-b"
    h/a/x      -> child "a"

Child `a` appears, then `a-b`, then `a` again. Two consequences:

- The scan cannot skip forward past a child it has already emitted, because a
  different, smaller child may sit inside that range.
- First-appearance order is not sorted order. If `h/a` did not exist, `a-b`
  would be seen before `a`, so truncating the first `limit` seen would return
  the wrong set.

Any early stop is unsafe. The bound is fundamental to `/`-delimited names, not
an implementation gap.

## Options

1. **Scan the prefix, sort, truncate.** Correct. Costs what `list/2` costs
   today, so nothing regresses; `from` still cuts the start of the scan because
   every child at or after `from` has all of its keys at or after
   `prefix ++ from`.
2. **Stop at `limit` distinct children.** Bounded, and wrong for any name
   holding a byte below `/`.
3. **Forbid such names.** Not ours to forbid: they are message IDs.

## What I picked, and why

Option 1 for the hierarchical listing, and a separate exactly-bounded walk for
the shape the index actually uses.

The index of §4.4 is not hierarchical. It is a sorted set of fixed-width rows
in one LMDB duplicate set, and there `limit` *is* exact: `elmdb:dups/3` seeks
once with `MDB_GET_BOTH_RANGE` and reads no more elements than it returns.
Criteria 1-3 are judged on the published index, which is that shape, so the
bounded walk exists exactly where the criteria need it.

`elmdb:list/3` and `hb_store:list/3` say in their docs that `limit` bounds the
result rather than the work, and point at the walk that bounds both.

## Consequence for the store chain

`hb_store:do_call_function/5` stops at the first store that does not answer
`{error, not_found}`. A bounded page is an answer, so a paginated `list` is
served by one store rather than merged across the chain -- the same rule
`read/3` already follows. Documented rather than changed: merging pages across
stores would need a total order across stores that does not exist.
