%%% @doc Copied and adapted from the arweave codebase.
%%% Should track: https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_block_index.erl
%%%
%%% VENDOR: upstream is 24 ETS operations against a private `block_index'
%%% ordered_set holding `{{WeaveSize, Height, H, TXRoot}}' tuples. HyperBEAM
%%% keeps the block index in `hb_store'/`hb_cache', so none of that plumbing is
%%% vendored. What is vendored is the arithmetic upstream layers on top of the
%%% three ETS seeks it performs, expressed against an explicit lookup function
%%% so any backing store can supply it.
%%%
%%% A `seek()' is a `fun/1' answering three queries. All offsets are absolute
%%% weave offsets and `WeaveSize' is the block's *end* offset, exactly as
%%% upstream stores it.
%%%
%%% <pre>
%%%   Seek({height, Height})            -> entry() | not_found
%%%   Seek({hash, BH})                  -> {Height, entry()} | not_found
%%%   Seek({weave_size_above, Offset})  -> {Height, entry()} | not_found
%%% </pre>
%%%
%%% `{weave_size_above, Offset}' must return the lowest-height entry whose
%%% `WeaveSize' is strictly greater than `Offset' - that is, the block that
%%% contains byte `Offset'. It is upstream's
%%% `ets:next(block_index, {Offset, n, n, n})', where the atom `n' sorts above
%%% every integer so the seek lands past every entry with `WeaveSize =< Offset'.
%%%
%%% `from_list/1' builds a `seek()' over an in-memory block index in upstream's
%%% own representation - a list of `{H, WeaveSize, TXRoot}' triplets sorted from
%%% latest to earliest, whose last element is the genesis block at height 0.
-module(ar_block_index).

-export([empty/0, from_list/1, get_block_bounds/2, get_block_bounds_with_height/2,
		get_element_by_height/2, member/2, get_range/3, get_list/2, get_list_by_hash/2]).

-include("include/ar.hrl").

-type entry() :: {BH :: binary(), WeaveSize :: non_neg_integer(), TXRoot :: binary()}.
-type seek() :: fun((term()) -> term()).

-export_type([entry/0, seek/0]).

%%%===================================================================
%%% Public interface.
%%%===================================================================

%% @doc Return a seek over an empty block index. Every query answers not_found.
-spec empty() -> seek().
empty() ->
	fun(_Query) -> not_found end.

%% @doc Return a seek over the given in-memory block index. BI is a list of
%% {H, WeaveSize, TXRoot} triplets sorted from latest to earliest; the last
%% element is the block at height 0.
-spec from_list([entry()]) -> seek().
from_list(BI) ->
	{ByHeight, ByHash, ByWeaveSize} = index_list(lists:reverse(BI), 0, #{}, #{},
			gb_trees:empty()),
	fun	({height, Height}) ->
			maps:get(Height, ByHeight, not_found);
		({hash, BH}) ->
			case maps:get(BH, ByHash, not_found) of
				not_found ->
					not_found;
				Height ->
					{Height, maps:get(Height, ByHeight)}
			end;
		({weave_size_above, Offset}) ->
			Iterator = gb_trees:iterator_from(Offset + 1, ByWeaveSize),
			case gb_trees:next(Iterator) of
				none ->
					not_found;
				{_WeaveSize, Height, _Iterator2} ->
					{Height, maps:get(Height, ByHeight)}
			end
	end.

%% @doc Return the {H, WeaveSize, TXRoot} triplet for the given Height or not_found.
-spec get_element_by_height(non_neg_integer(), seek()) -> entry() | not_found.
get_element_by_height(Height, Seek) ->
	Seek({height, Height}).

%% @doc Return true if the given block hash is found in the index.
-spec member(binary(), seek()) -> boolean().
member(H, Seek) ->
	Seek({hash, H}) /= not_found.

%% @doc Return {BlockStartOffset, BlockEndOffset, TXRoot} where Offset >= BlockStartOffset,
%% Offset < BlockEndOffset, or not_found if Offset is beyond the block index range.
-spec get_block_bounds(non_neg_integer(), seek()) ->
		{non_neg_integer(), non_neg_integer(), binary()} | not_found.
get_block_bounds(Offset, Seek) ->
	case get_block_bounds_with_height(Offset, Seek) of
		{BlockStart, BlockEnd, TXRoot, _} ->
			{BlockStart, BlockEnd, TXRoot};
		not_found ->
			not_found
	end.

%% @doc Return {BlockStartOffset, BlockEndOffset, TXRoot, Height} where
%% Offset >= BlockStartOffset, Offset < BlockEndOffset.
-spec get_block_bounds_with_height(non_neg_integer(), seek()) ->
		{non_neg_integer(), non_neg_integer(), binary(), non_neg_integer()} | not_found.
get_block_bounds_with_height(Offset, Seek) ->
	case Seek({weave_size_above, Offset}) of
		not_found ->
			?LOG_ERROR([{event, get_block_bounds_offset_out_of_range},
					{offset, Offset}]),
			not_found;
		{0, {_H, WeaveSize, TXRoot}} ->
			{0, WeaveSize, TXRoot, 0};
		{Height, {_H, WeaveSize, TXRoot}} ->
			case Seek({height, Height - 1}) of
				not_found ->
					?LOG_ERROR([{event, get_block_bounds_missing_previous_element},
							{offset, Offset}, {height, Height}]),
					not_found;
				{_PrevH, PrevWeaveSize, _PrevTXRoot} ->
					{PrevWeaveSize, WeaveSize, TXRoot, Height}
			end
	end.

%% @doc Return the list of {H, WeaveSize, TXRoot} triplets for blocks with Height >= Start,
%% =< End, sorted from the largest height to the smallest.
-spec get_range(non_neg_integer(), non_neg_integer(), seek()) ->
		[entry()] | {error, invalid_start}.
get_range(Start, End, _Seek) when Start > End ->
	[];
get_range(Start, End, Seek) ->
	case Seek({height, Start}) of
		not_found ->
			{error, invalid_start};
		Entry ->
			get_range2(Start + 1, End, Seek, [Entry])
	end.

%% @doc Return the list of {H, WeaveSize, TXRoot} triplets up to the given Height (including)
%% sorted from latest to earliest.
-spec get_list(non_neg_integer(), seek()) -> [entry()].
get_list(Height, Seek) ->
	get_list2(0, Height, Seek, []).

%% @doc Return the list of {H, WeaveSize, TXRoot} triplets up to the block with the given
%% hash H (including) sorted from latest to earliest.
-spec get_list_by_hash(binary(), seek()) -> [entry()].
get_list_by_hash(H, Seek) ->
	case Seek({hash, H}) of
		not_found ->
			[];
		{Height, _Entry} ->
			get_list(Height, Seek)
	end.

%%%===================================================================
%%% Private functions.
%%%===================================================================

index_list([], _Height, ByHeight, ByHash, ByWeaveSize) ->
	{ByHeight, ByHash, ByWeaveSize};
index_list([{H, WeaveSize, TXRoot} | BI], Height, ByHeight, ByHash, ByWeaveSize) ->
	%% A block with no transactions does not move the weave, so several
	%% consecutive heights can share one WeaveSize. Upstream's ordered_set is
	%% keyed on the whole {WeaveSize, Height, H, TXRoot} tuple, so those
	%% coexist and its seek lands on the lowest height of the group - the block
	%% that actually contains the byte. Keep the first height seen, since BI is
	%% walked from height 0 upwards.
	ByWeaveSize2 =
		case gb_trees:is_defined(WeaveSize, ByWeaveSize) of
			true ->
				ByWeaveSize;
			false ->
				gb_trees:insert(WeaveSize, Height, ByWeaveSize)
		end,
	index_list(BI, Height + 1,
			ByHeight#{ Height => {H, WeaveSize, TXRoot} },
			ByHash#{ H => Height },
			ByWeaveSize2).

get_range2(Start, End, _Seek, Entries) when Start > End ->
	Entries;
get_range2(Start, End, Seek, Entries) ->
	case Seek({height, Start}) of
		not_found ->
			Entries;
		Entry ->
			get_range2(Start + 1, End, Seek, [Entry | Entries])
	end.

get_list2(Height, MaxHeight, _Seek, Entries) when Height > MaxHeight ->
	Entries;
get_list2(Height, MaxHeight, Seek, Entries) ->
	case Seek({height, Height}) of
		not_found ->
			Entries;
		Entry ->
			get_list2(Height + 1, MaxHeight, Seek, [Entry | Entries])
	end.
