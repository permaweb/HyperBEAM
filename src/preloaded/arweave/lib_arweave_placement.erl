%%% @doc Where a source transaction occurs in an Arweave block.
%%%
%%% A placement is the authenticated answer to "which block included this
%%% transaction, where in it, and over which bytes of the weave". It is derived
%%% from a validated block and its transaction bodies, so every field in it was
%%% checked: the block hash and height by the block's own validation, and the
%%% offsets by the same size-tagged list the transaction root is built over.
%%%
%%% A placement is a distinct thing from a byte offset, and the two are stored
%%% separately on purpose. `hb_store_arweave_offset' answers "where can these
%%% bytes be fetched", in four fields and as few bytes as the encoding allows,
%%% for tens of billions of data items. A placement answers "where does this
%%% source transaction occur in the chain", which a reorganisation can change
%%% and a consumer may need to check against the selected chain.
%%%
%%% Placements are content-addressed like every other message, and the current
%%% one for a transaction is named by an alias under this device's own path
%%% namespace. A reorganisation replaces the alias; it deletes nothing, because
%%% the block that carried the old placement still links it.
-module(lib_arweave_placement).
-export([placements/2, write/2]).
-include("include/hb.hrl").

%% @doc Derive the placement of every transaction in a validated block.
%%
%% `BlockStart' is the weave size the block's parent left behind, which is the
%% offset the block's own bytes begin at. The per-transaction offsets and data
%% roots come from `ar_block:generate_size_tagged_list_from_txs/2' -- the very
%% list the transaction root is built over -- so a placement cannot disagree
%% with the root the block signed. That list is ordered by the weave layout and
%% carries padding entries of its own; `position' is the transaction's index in
%% the block's declared `txs', which is the order the header commits to.
placements(Block, BlockStart) ->
    Layout = layout(Block),
    Hash = hb_util:encode(Block#block.indep_hash),
    [
        placement(TX, Position, Hash, Block, BlockStart, Layout)
    ||
        {Position, TX} <- positioned(Block#block.txs)
    ].

%% @doc Write every placement, alias each under its transaction, and record the
%% transaction's byte offset. Returns the placements as links, in the order they
%% were given, so the block that carries them holds identifiers rather than
%% copies.
write(Placements, Opts) ->
    Store = hb_store_arweave:store_from_opts(Opts),
    collect([ written(Placement, Store, Opts) || Placement <- Placements ], []).

%%% Internal functions.

%% @doc Pair each transaction with its index in the block's declared order.
positioned(TXs) ->
    lists:zip(lists:seq(0, length(TXs) - 1), TXs).

%% @doc Build one transaction's placement.
%%
%% The layout is read without a default. Every transaction of the block is in
%% it -- it is built from the same list -- and a transaction that was not would
%% otherwise be placed at the block's own start offset, which is a wrong answer
%% written into an index rather than a missing one.
placement(TX, Position, Hash, Block, BlockStart, Layout) ->
    ID = hb_util:encode(TX#tx.id),
    {End, DataRoot} = maps:get(TX#tx.id, Layout),
    #{
        <<"id">> => ID,
        <<"block">> => Hash,
        <<"height">> => Block#block.height,
        <<"position">> => Position,
        <<"data-root">> => hb_util:encode(DataRoot),
        <<"data-size">> => TX#tx.data_size,
        <<"start-offset">> => BlockStart + End - TX#tx.data_size,
        <<"transaction">> => to_link(ID)
    }.

%% @doc The end offset and data root of each transaction relative to the start
%% of the block, keyed by transaction identifier. The padding entries the
%% size-tagged list carries belong to the weave layout rather than to any
%% transaction, so they are dropped.
layout(Block) ->
    maps:from_list(
        [
            {TX#tx.id, {End, DataRoot}}
        ||
            {{TX, DataRoot}, End} <-
                ar_block:generate_size_tagged_list_from_txs(
                    Block#block.txs,
                    Block#block.height
                ),
            is_record(TX, tx)
        ]
    ).

%% @doc Write one placement, its alias and its byte offset, and return the link
%% the block carries in its place.
written(Placement, Store, Opts) ->
    maybe
        ID = hb_maps:get(<<"id">>, Placement, <<>>, Opts),
        {ok, MsgID} ?= hb_cache:write(Placement, Opts),
        hb_cache:link(MsgID, lib_arweave_paths:placement(ID), Opts),
        ok ?=
            hb_store_arweave:write_offset(
                Store,
                ID,
                <<"tx@1.0">>,
                hb_maps:get(<<"start-offset">>, Placement, 0, Opts),
                hb_maps:get(<<"data-size">>, Placement, 0, Opts)
            ),
        {ok, to_link(MsgID)}
    end.

to_link(ID) ->
    {link, ID, #{ <<"type">> => <<"link">>, <<"lazy">> => false }}.

%% @doc Reduce a list of per-placement results to one result over the list. A
%% placement that could not be written leaves the block unpublishable, because
%% the block's presence is what says its indexes are complete.
collect([], Written) ->
    {ok, lists:reverse(Written)};
collect([{ok, Link} | Rest], Written) ->
    collect(Rest, [Link | Written]);
collect([Error | _Rest], _Written) ->
    Error.
