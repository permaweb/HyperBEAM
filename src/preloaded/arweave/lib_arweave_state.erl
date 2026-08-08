%%% @doc The bridge between the chain-state message and the carried state the
%%% vendored validators expect.
%%%
%%% A block header does not express the state needed to validate its successor.
%%% Six of the field checks read values that are never gossiped: the reward
%%% history, the block-time history, the account tree, the block index, and the
%%% transaction identifiers of the recent blocks. The chain-state message
%%% carries all of them alongside the block, so `~arweave-block@2.9/apply' is a
%%% pure function from one chain state to the next.
%%%
%%% Every component is a link, so a chain state is cheap to write and cheap to
%%% pass around, and a check that does not consult a component never loads it.
%%%
%%% The two histories are carried in Arweave's own wire encoding, under a
%%% `body' key, rather than as per-element messages. They are 21,650 and 64,850
%%% entries long and both shift by one element per block, so a per-element
%%% representation would rewrite every entry for every block; and the wire form
%%% is exactly what `/reward_history/<BH>' and `/block_time_history/<BH>'
%%% return, so what bootstrap verifies against the block's hash is byte for
%%% byte what it stores. `dev_arweave_merkle' carries its trees the same way,
%%% for the same reason.
%%%
%%% Every component defaults to `[]'. An empty account tree disables the
%%% account and transaction checks. `~arweave-block@2.9/apply' refuses to
%%% carry such a state forward unless `arweave-require-accounts' is explicitly
%%% false, so a chain cannot be built in that mode by accident. Which mode a
%%% block was validated in is recorded on the state it produced, under
%%% `accounts-checked', so that the
%%% weaker mode is visible in the record rather than inferred from an absence.
-module(lib_arweave_state).
-export([previous_block/2, block/2, accounts/2]).
-export([block_index/2]).
-export([reward_history/2, block_time_history/2, recent_blocks/2]).
-export([block_anchors/2, recent_transactions/2]).
-export([next/2, reward_history_message/1, block_time_history_message/1]).
-export([next_recent_blocks/3]).
-include("include/hb.hrl").

%% @doc Return the chain state's block as a header record carrying the two
%% histories, which the vendored field checks read off the parent rather than
%% taking as arguments. The proofs are left empty: no check performed against
%% the parent consults them, so its chunks are never loaded.
previous_block(State, Opts) ->
    Block = lib_arweave_block:to_header(block(State, Opts), Opts),
    Block#block{
        reward_history = reward_history(State, Opts),
        block_time_history = block_time_history(State, Opts)
    }.

%% @doc Return the chain state's block message.
block(State, Opts) ->
    hb_maps:get(<<"block">>, State, #{}, Opts).

%% @doc Return the account tree state, or `[]' when none is anchored.
accounts(State, Opts) ->
    hb_maps:get(<<"accounts">>, State, [], Opts).

%% @doc Return the block index state, or `[]' when none is loaded.
block_index(State, Opts) ->
    hb_maps:get(<<"block-index">>, State, [], Opts).

%% @doc Return the reward history as the list of
%% `{Address, HashRate, Reward, Denomination}' elements `ar_rewards' works on.
reward_history(State, Opts) ->
    history(
        fun ar_serialize:binary_to_reward_history/1,
        hb_maps:get(<<"reward-history">>, State, [], Opts),
        Opts
    ).

%% @doc Return the block-time history as the list of
%% `{BlockInterval, VDFInterval, ChunkCount}' elements `ar_block_time_history'
%% works on.
block_time_history(State, Opts) ->
    history(
        fun ar_serialize:binary_to_block_time_history/1,
        hb_maps:get(<<"block-time-history">>, State, [], Opts),
        Opts
    ).

%% @doc Return the recent blocks, newest first: one message per block carrying
%% its `indep-hash' and the identifiers of its transactions.
recent_blocks(State, Opts) ->
    hb_util:message_to_ordered_list(
        hb_maps:get(<<"recent-blocks">>, State, [], Opts),
        Opts
    ).

%% @doc Return the block hashes a transaction may anchor against, newest first.
block_anchors(State, Opts) ->
    [
        hb_maps:get(<<"indep-hash">>, Block, <<>>, Opts)
    ||
        Block <- recent_blocks(State, Opts)
    ].

%% @doc Return the identifiers of every transaction in the recent blocks, which
%% a transaction may neither replay nor anchor past.
recent_transactions(State, Opts) ->
    [
        ID
    ||
        Block <- recent_blocks(State, Opts),
        ID <- hb_util:message_to_ordered_list(
            hb_maps:get(<<"txs">>, Block, [], Opts),
            Opts
        )
    ].

%% @doc Build the chain state that follows a validated block, from the
%% components the checks have already computed. Absent components stay absent,
%% so a state validated without an account tree produces one without an account
%% tree.
next(State, Components) ->
    maps:merge(State, Components).

%% @doc Represent a reward history as the message the chain state carries.
reward_history_message(History) ->
    #{ <<"body">> => ar_serialize:reward_history_to_binary(History) }.

%% @doc Represent a block-time history as the message the chain state carries.
block_time_history_message(History) ->
    #{ <<"body">> => ar_serialize:block_time_history_to_binary(History) }.

%% @doc Extend the recent-block window with a validated block, trimming it to
%% the depth a transaction anchor may reach back.
next_recent_blocks(State, Block, Opts) ->
    lists:sublist(
        [
            #{
                <<"indep-hash">> =>
                    hb_maps:get(<<"indep-hash">>, Block, <<>>, Opts),
                <<"txs">> => hb_maps:get(<<"txs">>, Block, [], Opts)
            }
        |
            recent_blocks(State, Opts)
        ],
        ar_block:get_max_tx_anchor_depth()
    ).

%%% Internal functions.

%% @doc Decode a history from the wire encoding the chain state carries it in.
%% An absent history is empty, which the checks that consult one reject rather
%% than pass: an empty history hashes to a value no real block declares.
history(_Decode, [], _Opts) ->
    [];
history(Decode, Message, Opts) ->
    {ok, History} = Decode(hb_maps:get(<<"body">>, Message, <<>>, Opts)),
    History.
