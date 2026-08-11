%%% @doc The `#tx{}' record boundary for Arweave layer-one transactions.
%%%
%%% Vendored `ar_tx', `ar_block' and `ar_wallet' work on records; every device
%%% above them works on messages. A layer-one transaction has one message form
%%% -- HyperBEAM's own `tx@1.0' -- so this module is a boundary over that codec
%%% rather than a projection of its own, and what a consensus check reads is
%%% what a query returns.
%%%
%%% `tx@1.0' spells the fields as the codec does: `anchor' rather than
%%% Arweave's wire `last_tx', and `data_root'/`data_size' underscored. The tags
%%% are the message's own keys, with their exact bytes and case preserved in the
%%% commitment. The owner, the signature, its type and the transaction
%%% identifier live in that commitment, which is what makes the Arweave
%%% transaction identifier resolve through the cache and what the generic match
%%% index reads a committer out of.
%%%
%%% Two device packages cross this boundary -- `~arweave-tx@2.9' and
%%% `~arweave-block@2.9' -- which is what the module is for. The rule that only
%%% the committed keys cross it is the one thing that must not drift between
%%% them.
%%%
%%% `lib_arweave_common' owns the same boundary for ANS-104 data items, which
%%% are a different shape with a different signature preimage, so the two do
%%% not overlap.
-module(lib_arweave_tx).
-export([to_tx/2, from_tx/2]).
-include("include/hb.hrl").

%% @doc Convert a transaction message into the record the vendored modules
%% take.
%%
%% Only the keys the transaction's commitment covers are converted. A caller
%% resolving a key on a transaction leaves `device' and a path on the base, and
%% the codec turns every key it is handed into a tag -- which would change the
%% very preimage the signature is checked against.
to_tx(Msg, Opts) ->
    with_owner_address(
        hb_message:convert(
            hb_util:ok(hb_message:with_only_committed(Msg, Opts)),
            <<"tx@1.0">>,
            <<"structured@1.0">>,
            Opts
        )
    ).

%% @doc Convert a transaction record into its message form.
from_tx(TX, Opts) ->
    hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts).

%%% Internal functions.

%% @doc Cache the sender's address on the record, as the address the message
%% names its committer by.
%%
%% `tx@1.0' derives that committer with `ar_wallet:to_address/2' on whatever
%% owner bytes the commitment carries, unconditionally. `ar_tx:normalize/1'
%% instead answers `not_set' when the owner is 512 zero bytes, because that is
%% the `#tx{}' record's own default and upstream reads it as "no owner has been
%% set". For a message that arrived with a signature the owner was set -- to
%% zeros, which is no RSA modulus and so a transaction that cannot verify --
%% and the two would then disagree about who signed it. Deriving the field the
%% way the message did keeps the record and the message the same transaction.
with_owner_address(TX) ->
    TX#tx{
        owner_address = ar_wallet:to_address(TX#tx.owner, TX#tx.signature_type)
    }.
