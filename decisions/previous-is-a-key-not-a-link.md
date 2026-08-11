# `previous` is a device key, not a stored link

## The prompt as I understand it

"`~arweave@2.9/tip` links directly to the selected `arweave-block@2.9` message
... Consequently, `tip/previous/previous/previous` resolves three blocks behind
the selected tip using ordinary AO-Core traversal." And: "`previous` targets the
previous Arweave block hash. `hb_cache` links that hash to the corresponding
AO-Core block message. The link can therefore be written before the older block
has been downloaded, and becomes traversable when that message is present."

## The issue

A stored `+link` value has to be a content identifier. `hb_cache:read/2` returns
every `+link` key as `{link, "<MsgID>/<key>+link", lazy => true}`, and
`hb_link:normalize/3` resolves such a link by reading that key and *requiring*
what comes back to satisfy `?IS_ID` -- 32, 42 or 43 bytes. An Arweave block hash
is 64 characters, and a store path naming one is longer still.

Resolution tolerates the difference: `hb_cache:ensure_loaded/3` follows a lazy
link to any target. Re-normalisation does not, and re-normalisation is not a
corner: `hb_ao:resolve/3` computes a hashpath over its base after a successful
call, which converts the base to TABM. So *any* successful resolution against a
block read back from the cache raised
`{could_not_read_lazy_link, {key, <<"previous">>}, ...}`.

Found by the live mainnet probe, on the first block it applied onto a stored
parent -- which is every block a running node applies.

## Options

1. Relax `?IS_ID` in `hb_link:normalize/3` so a lazy link may name anything, as
   `ensure_loaded/3` already allows. This is the smallest change and arguably
   fixes an inconsistency between two halves of the same mechanism -- but it is
   the kernel, and CONTRIBUTING is explicit that the kernel is not to be
   modified where the application layer suffices.
2. Point `previous` at the parent's content identifier. Works for a chain built
   upwards, and cannot work for one filled in downwards: `backfill`
   materialises a parent *after* the block that names it, so at write time
   there is no identifier to point at. Omitting the link there would leave
   backfilled history unwalkable, which is most of what backfill is for.
3. Drop the stored key and make `previous` a key of `~arweave-block@2.9` that
   reads the header's own `previous-block' hash and resolves the block
   published under it.

## Decision

Option 3.

The traversal the brief asks for is preserved exactly --
`tip/previous/previous/previous` resolves three blocks back through ordinary
AO-Core resolution -- and it now works in both directions of construction,
before and after the parent exists, because the name is resolved at read time
rather than frozen at write time. A block that names a parent this node does not
hold answers `no-previous-block`, which is the truth, where a dangling link
raised.

It also removes a stored field. `previous-block` is already the durable
relationship -- it is a consensus field of the header, signed by the block
producer -- and `~arweave@2.9/blocks/<indep-hash>` is already the name that
resolves it. A second copy of the same edge, in a form that cannot survive a
cache round trip, was adding nothing.

The same applies to the account tree, whose `previous` link named
`~arweave-wallets@2.9/trees/<root>` and would have failed identically. It keeps
`previous-root` -- the identity a block header and a peer both name a tree by --
and gains a `previous` key over it. The block index is unaffected: the version it
links is a content identifier, because an index state exists before the state
derived from it.
