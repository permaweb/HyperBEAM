# A transaction has one message form

## The prompt as I understand it

"Please replace `arweave-tx@1.0` with just `tx@1.0`, adding additional
components to it (but keeping its current spelling of the keys)."

Correcting an earlier decision on this branch, which had kept two message forms
of a layer-one transaction and threaded both through the sync path.

## The issue

`~arweave-tx@2.9` had a projection of its own -- `last-tx`, `data-root`, the
signature and owner as ordinary fields -- because the consensus checks are
ports of code that works on `#tx{}` records. HyperBEAM's `tx@1.0` message
spells `anchor` and `data_root` and carries the signature as a *commitment*,
which is what makes the Arweave transaction identifier resolve through the
cache and what `~query@1.0` matches an owner or a recipient by.

Keeping both meant every transaction existed twice, and the two could only be
kept honest by construction rather than by type.

## What `tx@1.0` was missing

1. **`denomination`.** The record has it, `ar_tx` serialises it, and it is part
   of the signature preimage when it is set. `tx@1.0` dropped it, and
   `enforce_valid_tx/1` refused any transaction that stated one -- with a
   comment saying the vendored code did not support denominations, which it
   does. Now a base field like `quantity` and `reward`, with the same
   tag-exclusion rule.
2. **A `format` tag exclusion.** A format 1 transaction carries `format` as a
   field, and the encoder wrote it back as a *tag* as well, because only
   `quantity`, `reward`, `target`, `anchor`, `data_root` and `data_size` had
   exclusion rules. The record that came back carried a tag the signed one
   never had, and no format 1 transaction verified. `format_1_test` is the
   vector; `denomination_test` covers the field above.
3. **The owner address.** `tx@1.0` derives its committer with
   `ar_wallet:to_address/2` on whatever owner bytes the commitment carries,
   unconditionally. `ar_tx:normalize/1` instead answers `not_set` when the
   owner is 512 zero bytes, because that is the `#tx{}` record's own default
   and upstream reads it as "no owner has been set". 512 zero bytes is not a
   valid RSA key -- there is no modulus of zero -- so such a transaction can
   never verify; but for a message that arrived *with a signature* the owner
   was set, and record and message would disagree about who signed it.
   `lib_arweave_tx:to_tx/2` derives the field the way the message did, so the
   two stay the same transaction. `record_and_message_name_one_sender_test`
   holds that pairing.

   This is not a crash guard. `dev_arweave_block:balances/5` encodes each
   sender's address and would raise on the atom, but no such transaction
   reaches it: the transaction check refuses it, that check runs first, and
   `checks/0` makes the account check depend on it, so a set asking for one
   without the other is refused. `materialize_refuses_a_transaction_with_no_owner_test`
   is the vector. `~arweave-tx@2.9/verify` is the one caller that can be
   handed such a transaction directly, because it loads the accounts before
   running any check.

## What this changes at the edges

A `tx@1.0` message keeps its fields in the commitment, so the conversion to a
record is driven by the commitment alone and a key written onto the message
beside it is not part of the transaction. `lib_arweave_tx:to_tx/2` therefore
converts `hb_message:with_only_committed/2` of its argument: a caller resolving
a key on a transaction leaves `device` and a path on the base, and the codec
would otherwise carry those into the tags the signature covers.

Two admission vectors tested field limits with shapes `tx@1.0` cannot
represent -- a 64 byte anchor and a 33 byte target, both of which
`enforce_valid_tx/1` refuses outright. They now use limits that a
well-formed transaction can breach: a fee with more digits than the height
permits, and a transfer with no recipient.

A peer serving such a transaction now raises out of `from-json` rather than
being refused by name. That is the module's existing behaviour for every
malformed peer response -- `peer_block/3` raises the same way on a block
binary the decoder cannot read -- so the set of bodies that raise grew without
the handling of them changing. Refusing a peer's bytes by name, uniformly,
is its own piece of work.

## Decision

One form. `~arweave-tx@2.9` reads and writes `tx@1.0`; `lib_arweave_tx` is the
boundary between that message and the `#tx{}` record, and `dev_arweave_sync`
fetches each transaction once and hands the same message to the block checks
and to publication.

`lib_arweave_tx` is a module rather than two inline calls because two device
packages cross that boundary, and the rule that only the committed keys cross
it must not drift between them. It holds nothing else: the JSON Arweave serves
is `~arweave-tx@2.9`'s own business and moved there, its only caller.
