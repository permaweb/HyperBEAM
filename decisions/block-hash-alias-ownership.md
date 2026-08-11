# The validated block alias is namespaced, not the bare block hash

## The prompt as I understand it

The brief says a stored `arweave-block@2.9` message is reached by its Arweave
block hash: "`previous` targets the previous Arweave block hash. `hb_cache`
links that hash to the corresponding AO-Core block message", and publication
step 4 is "Link the Arweave block hash to that message". Block-message presence
under that name is the completion marker a scheduler checks.

## The issue

The bare block hash is already taken. `dev_arweave_cache:write/2` links both the
`indep_hash` and the solution `hash` of every block `~arweave@2.9/block` fetches
from a *gateway* -- peer claims that nothing has checked -- and
`dev_query_arweave` depends on that link: `blocks(ids: [...])` resolves each
identifier straight through `hb_cache:read/2`, and
`dev_query_test_vectors:simple_blocks_query_test_parallel` pins it.

Two writers on one name is not a namespace collision to be tidied up, it is a
correctness problem: whichever ran last would decide what
`~arweave@2.9/validated` answered -- a key whose entire purpose is to answer
only for blocks this node verified -- and `tip/previous/previous` could walk out
of the validated chain into a gateway response.

## Options

1. Give the bare hash to the consensus cache and move the gateway cache under
   its own namespace. Tried first; it breaks the query vector above, and
   CONTRIBUTING rule 1 does not permit that. It also degrades rather than fixes
   the query: the two carry different shapes -- the gateway's is a `json@1.0`
   projection with `previous_block`, the consensus one is the canonical message
   with `previous-block` -- so a validated block answering that query would
   report a null parent.
2. Write both, and let the last writer win the bare name. Silently weakens
   `validated`, which is the one thing that must not happen.
3. Namespace the validated alias as `~arweave@2.9/blocks/<indep-hash>`, and
   leave the gateway cache alone.

## Decision

Option 3.

The substance the brief asks for is unchanged: the block hash names the block
message, `previous` targets it, and the alias appears only once every index
derived from the block is written, so its presence is the completion marker. The
prefix is the same one every other durable name in this subsystem already
carries -- `~arweave@2.9/tip`, `~arweave@2.9/placements/<txid>`,
`~arweave@2.9/settled/<indep-hash>` -- and it is what makes the namespace
exclusively the consensus cache's, which is what `validated` rests on.

The store layout is owned by one module, `lib_arweave_paths`, declared as a
library of both `~arweave@2.9` and `~arweave-block@2.9`, because a block's
`previous` link is built by the block device and followed by the sync device. A
path spelled out in two places is a path that can drift, and drift here means a
chain that cannot be walked.
