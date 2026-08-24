# `~copycat@1.0` already writes offsets first, so §4.9 is not a commit

## The prompt as I understand it

§4.9 asks whether `~copycat@1.0` in `full` mode writes an item's weave offset
before it writes the item's contents. If it writes contents first, `~match@1.0`
looks for an offset that is not there yet and every index row is written
without one, which makes the index useless for the offset-keyed design. The
spec says to check first and to drop the commit if the order is already right.

## What the code does

Every `hb_cache:write` in `src/preloaded/query/dev_copycat_arweave.erl` sits
inside the `ok ->` branch of a preceding `hb_store_arweave:write_offset`:

- `index_full_bundle_items/7` (`:728-745`): `write_offset` is the `case`
  scrutinee, `hb_cache:write` is at `:742` inside its success clause.
- `process_tx/4` (`:440-457`): `write_offset` at `:441`, `cache_tx_header/2`
  at `:456`.
- `process_pending_tx/2` (`:624-644`): `write_offset` at `:628`,
  `index_pending_children/5` at `:631`.
- The shallow path at `:494-544` writes offsets and no contents at all.

One level down, `hb_cache:run_write_ops/4` (`src/core/resolver/hb_cache.erl`
`:461-470`) flushes the pending content writes and only then calls
`write_match_index/3`. So the order is offset, contents, match rows.

## Why the order is also the right one

`dev_query_arweave:do_filter_offset_annotated/3` (`:621`) drops any result
whose annotation has no `offset` when a `block` range is given. Writing
contents first would open a window where a matchable message has no offset and
is silently missing from block-filtered results.
`transactions_query_filter_by_block_excludes_unknown_offsets_test_parallel`
(`src/preloaded/query/dev_query_test_vectors.erl:780-812`) pins that
behaviour. The reverse window -- an offset for contents not yet readable --
is handled by `read_ids/3` skipping unreadable IDs (`:262-270`).

## Decision

No commit for §4.9. Recorded here and in `STATUS.md` instead of making a change
that would only reorder writes that are already in the right order.
