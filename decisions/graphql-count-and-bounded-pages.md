# Bounding the index walk from GraphQL costs an exact `count`

## The prompt as I understand it

§4.8 asks that `~query@1.0`'s pagination be a seek: "the GraphQL `after:`
cursor is the offset". §1.1's third criterion is that "pagination by cursor
costs the same at page 50 as at page 1", and §4.10 measures "one tag paged to
page 50" through `~query@1.0/arweave`.

## The issue

`dev_query_arweave:connection/3` reports `count` as `length(Ordered)`: the
number of results the query found, before the cursor drop and before the page
slice. An exact count of an intersection requires walking the whole
intersection. A page that is a seek does not walk it. The two cannot both hold.

Three further things are entangled with it:

- `drop_to_cursor/3` scans the ordered list for the cursor and returns the
  tail. If the walk has already seeked past the cursor, the cursor is not in
  the list and the scan drops everything.
- `annotate_ids/2` derives each result's offset from
  `hb_store_arweave:read_offset/3`, which needs the node's own ID-to-offset
  index. A node with nothing but a published match index has no such index, and
  `annotate_ids` returns `unavailable` -- so the results come back unsorted and
  without cursors, which is the configuration §1.1's first criterion names.
- Bounding is only sound when `tags` is the whole of the filter. A page of one
  filter, intersected afterwards with another, is not a page of the two.

## Options

1. **Bound the walk; `count` becomes the size of the page window.** Constant
   cost. `count` stops being a total for any result set larger than a page.
2. **Pass `from` but not `limit`.** Page fifty costs the results after it
   rather than all of them -- better, not constant -- and `count` becomes the
   number of results after the cursor.
3. **Leave the GraphQL surface alone.** The bounded walk exists and is reachable
   through `hb_cache:match/3`; `~query@1.0` keeps materializing.

## What I picked, and why

Option 1, and only where the index is the hashed one. A node whose
`match-hash-size` is unset keeps the index whose predicates have no shared
ordering; that index has to be read in full to be intersected at all, so
nothing is given up by leaving its `count` exact. A node with the hashed index
is running the design this specification describes, and its criteria ask for
constant-cost pages in as many words.

`count` is already approximate on that surface: it is computed before
`read_ids/3` drops the IDs that cannot be read, so it already reports more
edges than a client can page through. Making it the count of a page window is
a change in kind rather than in accuracy, and it is the honest number a
cursor-paginated API can produce.

`drop_to_cursor` is skipped where the walk has seeked, since the seek has
already done its work.

The offset annotation is the piece that does not follow: a node with no
ID-to-offset index cannot annotate. That is recorded in `STATUS.md` as the
remaining gap, with what closing it needs -- the walk already knows each
result's offset and would have to carry it out rather than discard it, which
changes `hb_cache:match/2`'s contract from a list of IDs to a list of
annotations.
