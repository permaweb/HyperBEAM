# Historical materialisation takes an explicit `from`, and records no frontier

## The prompt as I understand it

"`~arweave@2.9` supports downloading earlier blocks after joining from either a
trusted block or trusted peers. Request parameters select a named profile or an
explicit set of checks." And, of publication: "There is no additional
index-progress message. Block-message presence is the completion marker."

## The issue

`sync` is resumable without bookkeeping because it walks *up* from the tip and
stops at the first block it already holds. A backfill walks *down*, and the
block below the lowest one it holds is not something it can find cheaply: the
walk to the current frontier is as long as everything already materialised, so
on a node that has backfilled a hundred thousand blocks it is a hundred thousand
cache reads per pass.

So either the device records a frontier, or the caller says where to start.

## Options

1. Record a frontier -- a `~arweave@2.9/backfill-frontier` naming the lowest
   height materialised. Cheap, and it makes `backfill` a bare, repeatable cron
   task. It is also a second kind of progress state beside the blocks, which the
   brief has one of on purpose.
2. Default `from` to the bottom of the anchor window a bootstrap wrote. It never
   advances: every pass re-checks the same `count` heights and does nothing.
3. Require `from`, and make a repeated request the way to resume: a height whose
   block is already published costs one cache read and is skipped, so re-issuing
   the same request continues from wherever the last one stopped.

## Decision

Option 3.

The block index already says what every height from genesis should hold, so a
backfill has an authoritative work list without needing to remember anything --
and the blocks themselves already say which parts of it are done. Adding a
frontier would be adding progress state to a design whose whole point is that
the blocks are the progress state, in exchange for saving the caller an
arithmetic they are better placed to do: an operator backfilling knows what
range they want.

The cost is that a cron driving `backfill` has to compute `from` rather than
call it bare. That is a line of configuration, and it is visible, which a
frontier would not be.

## Addendum: the historical path defaults to `archive`, not `full`

The brief says of selective verification that "the default remains full
validation". On the live path it does: `apply` and `validate` run all eleven
checks unless told otherwise.

`backfill` cannot. Every check `full` adds over `archive` reads state below the
node's join -- the parent header the field checks derive from, the carried
histories, the account tree, the mining state the proofs and the VDF chain
replay -- and asking for one is refused with `unavailable-check` rather than
skipped. So `full` there is not a stronger setting, it is an error.

Defaulting to `archive` gives the strongest thing that path can establish, and
the refusal is what stops a caller believing they got more: a request that names
`full`, or any single check outside the four, fails and says which. Requiring the
operator to type `profile=archive` when there is exactly one sensible answer
would buy nothing that the refusal does not already buy.
