%%% @doc The bridge between a stored block message and the carried state the
%%% vendored validators expect.
%%%
%%% A block header does not express the state needed to validate its successor.
%%% Six of the field checks read values that are never gossiped: the reward
%%% history, the block-time history, the account tree, the block index, and the
%%% transaction identifiers of the recent blocks. A stored block message carries
%%% all of them alongside its own header fields, so
%%% `~arweave-block@2.9/apply' is a pure function from one stored block to the
%%% next.
%%%
%%% Every component is a link, so a block is cheap to write and cheap to pass
%%% around, and a check that does not consult a component never loads it.
%%%
%%% The two histories are persistent linked lists of immutable entries, one
%%% message per element, held by `lib_arweave_history'. They are 21,600 and
%%% 64,850 elements long and both shift by one element per block, so what a
%%% block costs is the single entry it adds: the tail below it is shared,
%%% unchanged and under the same identifiers, with the block it extended and
%%% with every branch that reached the same element.
%%%
%%% The recent-block window is not carried at all. A block names its parent in
%%% `previous-block', so the window is the chain itself, read back one block at
%%% a time and stopped at the anchor depth. Duplicating it into every block
%%% would store the same fifty summaries fifty times over, and they are already
%%% in the blocks.
%%%
%%% Every component defaults to `[]'. An empty account tree disables the
%%% account and transaction checks. `~arweave-block@2.9/apply' refuses to
%%% carry such a block forward unless `arweave-require-accounts' is explicitly
%%% false, so a chain cannot be built in that mode by accident. Which checks a
%%% block was validated by is recorded on the block itself, under
%%% `validation/checks', so the weaker mode is visible in the record rather
%%% than inferred from an absence.
-module(lib_arweave_state).
-export([materialize_histories/2, previous_block/2, accounts/2]).
-export([block_index/2]).
-export([block_anchors/2, recent_transactions/2]).
-export([next/2, next_reward_history/3, next_block_time_history/4]).
-include("include/hb.hrl").

%% @doc Carry both materialized history windows privately in a block state.
%% A state read from storage pays one walk; the blocks extended from it reuse
%% the windows without putting them in the durable message.
materialize_histories(Block, Opts) ->
    Block#{
        <<"reward-history">> =>
            lib_arweave_history:materialize(
                reward_history_head(Block, Opts),
                Opts
            ),
        <<"block-time-history">> =>
            lib_arweave_history:materialize(
                block_time_history_head(Block, Opts),
                Opts
            )
    }.

%% @doc Return a stored block as a header record carrying the two histories,
%% which the vendored field checks read off the parent rather than taking as
%% arguments. The proofs are left empty: no check performed against the parent
%% consults them, so its chunks are never loaded.
previous_block(Block, Opts) ->
    Header = lib_arweave_block:to_header(Block, Opts),
    Header#block{
        reward_history = reward_history(Block, Opts),
        block_time_history = block_time_history(Block, Opts)
    }.

%% @doc Return the account tree state, or `[]' when none is anchored.
accounts(Block, Opts) ->
    hb_maps:get(<<"accounts">>, Block, [], Opts).

%% @doc Return the block index state, or `[]' when none is loaded.
block_index(Block, Opts) ->
    hb_maps:get(<<"block-index">>, Block, [], Opts).

%% @doc Return the reward history as the list of
%% `{Address, HashRate, Reward, Denomination}' elements `ar_rewards' works on.
reward_history(Block, Opts) ->
    lib_arweave_history:values(reward_history_head(Block, Opts), Opts).

%% @doc Return the block-time history as the list of
%% `{BlockInterval, VDFInterval, ChunkCount}' elements `ar_block_time_history'
%% works on.
block_time_history(Block, Opts) ->
    lib_arweave_history:values(block_time_history_head(Block, Opts), Opts).

%% @doc Return the blocks a transaction may anchor against, newest first,
%% beginning with the block given and walking its ancestors to the depth an
%% anchor may reach back.
%%
%% The walk follows each block's `previous-block' hash and stops at the first
%% ancestor this node does not hold. A block may name a parent it has not
%% downloaded -- every node that joined from a checkpoint has one at the bottom
%% of its chain. Stopping there is what makes the window correct: a block this
%% node does not hold is not one a transaction may anchor against.
recent_blocks(Block, Opts) ->
    recent_blocks(Block, ar_block:get_max_tx_anchor_depth(), Opts).

recent_blocks([], _Depth, _Opts) ->
    [];
recent_blocks(_Block, 0, _Opts) ->
    [];
recent_blocks(Block, Depth, Opts) ->
    [Block | recent_blocks(parent(Block, Opts), Depth - 1, Opts)].

%% @doc Return the block hashes a transaction may anchor against, newest first.
block_anchors(Block, Opts) ->
    [
        hb_maps:get(<<"indep-hash">>, Recent, <<>>, Opts)
    ||
        Recent <- recent_blocks(Block, Opts)
    ].

%% @doc Return the identifiers of every transaction in the recent blocks, which
%% a transaction may neither replay nor anchor past.
recent_transactions(Block, Opts) ->
    [
        ID
    ||
        Recent <- recent_blocks(Block, Opts),
        ID <- hb_util:message_to_ordered_list(
            hb_maps:get(<<"txs">>, Recent, [], Opts),
            Opts
        )
    ].

%% @doc Build the block message a validated block is stored as, from the header
%% it arrived with and the components the checks have already computed. Absent
%% components stay absent, so a block validated without an account tree
%% produces one without an account tree.
next(Next, Components) ->
    maps:merge(Next, Components).

%% @doc Extend the reward history with the block's own reward.
%%
%% The element is read out of `ar_rewards:add_element/2' against an empty
%% history rather than assembled here, so the rule that decides what a block
%% contributes stays in the module that owns it and only the element is taken
%% from it.
next_reward_history(Block, Next, Opts) ->
    lib_arweave_history:append(
        <<"reward-history">>,
        hd(ar_rewards:add_element(Next, [])),
        Next#block.height,
        reward_history_head(Block, Opts),
        Opts
    ).

%% @doc Extend the block-time history with the block's own interval, VDF
%% interval and solution type.
%%
%% Below the 2.7 fork a block contributes no element, and the history it
%% inherits is carried forward as the very same head. That rule lives in
%% `ar_block_time_history:update_history/2', which is given a parent carrying no
%% history so that what it returns is the new element alone.
next_block_time_history(Block, Next, Prev, Opts) ->
    Head = block_time_history_head(Block, Opts),
    case ar_block_time_history:update_history(
            Next, Prev#block{ block_time_history = [] }) of
        [] ->
            Head;
        [Element] ->
            lib_arweave_history:append(
                <<"block-time-history">>,
                Element,
                Next#block.height,
                Head,
                Opts
            )
    end.

%%% Internal functions.

%% @doc Read the block a block extends, or `[]' when this node does not hold
%% it. The hash names the stored block message: publication links it there once
%% every index the block needs is written, so a block that reads back is a block
%% this node finished.
parent(Block, Opts) ->
    read(hb_maps:get(<<"previous-block">>, Block, [], Opts), Opts).

read(Hash, Opts) when is_binary(Hash) ->
    case hb_cache:read(lib_arweave_paths:block(Hash), Opts) of
        {ok, Parent} -> Parent;
        _ -> []
    end;
read(_NotAHash, _Opts) ->
    [].

%% @doc Return the newest reward-history entry, or `[]' when the block carries
%% no such history. An absent history is empty, which the checks that consult
%% one reject rather than pass: an empty history hashes to a value no real block
%% declares.
reward_history_head(Block, Opts) ->
    hb_maps:get(<<"reward-history">>, Block, [], Opts).

%% @doc Return the newest block-time-history entry, or `[]' when the block
%% carries no such history.
block_time_history_head(Block, Opts) ->
    hb_maps:get(<<"block-time-history">>, Block, [], Opts).
