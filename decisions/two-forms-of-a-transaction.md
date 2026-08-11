# A fetched transaction is kept in two forms

## The prompt as I understand it

Publication step 1: "Write every `tx@1.0` message and its generic match
entries." And, of the settled hook: "The hook request is the placement message
with its transaction link."

## The issue

There are two message forms of an Arweave layer-one transaction in HyperBEAM,
and they are not interchangeable.

`~arweave-tx@2.9`'s form -- built by `lib_arweave_tx` -- spells Arweave's wire
names (`last-tx`, `data-root`) and carries the signature and owner as ordinary
fields. It is what every consensus check reads, because the checks are ports of
code that works on `#tx{}` records and this is the projection of one.

HyperBEAM's own `tx@1.0` message -- built by `dev_tx` -- spells `anchor` and
`data_root` and carries the signature as a *commitment*. It is what
`hb_cache:write/2` derives alternate identifiers from, so it is the only form
whose Arweave transaction identifier resolves through the cache, and the only
form `~query@1.0` can match by owner or recipient, because both of those are
commitment fields.

Publication needs the second: without it, "its generic match entries" buys
nothing a query can use, and a placement's link to its transaction points at a
name that does not resolve. Validation needs the first.

## Options

1. Write only `~arweave-tx@2.9`'s form, and have the placement link the content
   identifier of that message. The link resolves, but the transaction is not
   findable by owner, target or tag, so the generic query device cannot see the
   transactions the consensus cache holds.
2. Convert one into the other at publication time. The conversion needs the
   `#tx{}` record, and the module that produces one from
   `~arweave-tx@2.9`'s form is packaged with `~arweave-block@2.9` -- calling it
   from `~arweave@2.9` compiles and raises `undef` in the packaged runtime.
3. Build both from the same peer response, once, and let each consumer take the
   one it reads.

## Decision

Option 3.

`dev_arweave_sync:transaction/3` fetches `/tx/<id>` once and returns a pair: the
checked form, which goes to `~arweave-block@2.9/apply`, and the stored form,
which publication writes. Both are derived from the same bytes -- the second
through the vendored JSON parser and the `tx@1.0` codec -- so they cannot
describe different transactions, and the peer is asked once.

The pair is threaded through two functions and named at both ends
(`checked_transactions/1`, `stored_transactions/1`) rather than being smuggled
in a `priv` section, which would put it exactly where the brief says
memoisation may not live.
