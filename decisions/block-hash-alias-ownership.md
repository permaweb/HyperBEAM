# The bare block hash names the validated block

## The prompt as I understand it

The brief says a stored `arweave-block@2.9` message is reached by its Arweave
block hash: "`previous` targets the previous Arweave block hash. `hb_cache`
links that hash to the corresponding AO-Core block message. The link can
therefore be written before the older block has been downloaded, and becomes
traversable when that message is present." Publication step 4 is "Link the
Arweave block hash to that message", and presence under that name is the
completion marker a scheduler checks.

## The issue

Two things stood in the way of naming a block by its bare hash.

A stored `+link` value has to satisfy `?IS_ID`. `hb_cache:read/2` returns every
`+link` key as `{link, "<MsgID>/<key>+link", lazy => true}`, and
`hb_link:normalize/3` resolves such a link by reading that key and requiring
what comes back to be an identifier. Re-normalisation is not a corner:
`hb_ao:resolve/3` converts its base to TABM after a successful call, so *any*
successful resolution against a block read back from the cache re-normalises
its links. A 64-character block hash was not an identifier, so the link raised.

Separately, `dev_arweave_cache:write/2` already linked both the `indep_hash`
and the solution `hash` of every block `~arweave@2.9/block` fetched from a
*gateway* -- peer claims that nothing has checked -- and `dev_query_arweave`
read `blocks(ids: [...])` straight through `hb_cache:read/2`.

## Decision

`?IS_ID` accepts 64 characters, which is an Arweave block hash and nothing
else: an AO-Core message identifier is 43, and the two cannot collide. A block
therefore links its parent by name, and the link is written before the parent
exists -- which is the only form that works in both directions of
construction, because `backfill` materialises a parent *after* the block that
names it.

The bare hash belongs to the consensus cache alone. It is the strongest
identity in the subsystem and it names the strongest claim the node can make:
a block it validated and finished indexing. The gateway cache moved under
`~arweave@2.9/block/hash/<hash>`, beside the `~arweave@2.9/block/height/<n>` it
already used, and `~query@1.0` follows it there -- resolving a block id through
`~arweave@2.9/block` with `only-if-cached`, so a query still answers only from
what the node holds. Sharing the name was not an option: whichever wrote last
would decide what `~arweave@2.9/validated` said, and a walk of `previous` links
could leave the validated chain.

Widening the guard is not free: `?IS_ID` is a claim that the ID converters can
handle a value, and two of them could not. `hb_util:native_id/1` and
`human_id/1` had no clause for a block hash, and `dev_router:binary_to_bignum/1`
matched a native ID as exactly 256 bits -- which was already wrong for the 42
character Ethereum address `?IS_ID` had admitted all along. All three are total
over the guard's domain now, with `hb_util:block_hash_is_an_id_test_parallel`
holding the pairing.

A walk past the oldest block a node holds raises, as any unresolvable link
does. That is the same event as a link to a block not yet downloaded, because
it *is* that: the difference between "the chain ends here" and "this part has
not been fetched yet" is `backfill`, and nothing the reader can tell apart.
The consensus rules that must not raise -- the transaction-anchor window --
walk by `previous-block` hash and stop where the chain does; see
`lib_arweave_state:recent_blocks/2`.
