%%% @doc Copied and adapted from the arweave codebase. 
%%% Should track: https://github.com/ArweaveTeam/arweave/blob/master/apps/arweave/src/ar_block.erl
-module(ar_block).

-export([strict_data_split_threshold/0, get_chunk_padded_offset/1, generate_size_tagged_list_from_txs/2]).

-include("include/ar.hrl").

%%%===================================================================
%%% Public interface.
%%%===================================================================

strict_data_split_threshold() -> ?STRICT_DATA_SPLIT_THRESHOLD.

%% @doc Return Offset if it is smaller than or equal to ar_block:strict_data_split_threshold().
%% Otherwise, return the offset of the last byte of the chunk + the size of the padding.
-spec get_chunk_padded_offset(Offset :: non_neg_integer()) -> non_neg_integer().
get_chunk_padded_offset(Offset) ->
	case Offset > ar_block:strict_data_split_threshold() of
		true ->
			ar_poa:get_padded_offset(Offset, ar_block:strict_data_split_threshold());
		false ->
			Offset
	end.

generate_size_tagged_list_from_txs(TXs, Height) ->
	lists:reverse(
		element(2,
			lists:foldl(
				fun(TX, {Pos, List}) ->
					DataSize = TX#tx.data_size,
					End = Pos + DataSize,
					case Height >= ar_fork:height_2_5() of
						true ->
							Padding = ar_tx:get_weave_size_increase(DataSize, Height)
									- DataSize,
							%% Encode the padding information in the Merkle tree.
							case Padding > 0 of
								true ->
									PaddingRoot = <<>>,
									{End + Padding, [{{padding, PaddingRoot}, End + Padding},
											{{TX, get_tx_data_root(TX)}, End} | List]};
								false ->
									{End, [{{TX, get_tx_data_root(TX)}, End} | List]}
							end;
						false ->
							{End, [{{TX, get_tx_data_root(TX)}, End} | List]}
					end
				end,
				{0, []},
				lists:sort(TXs)
			)
		)
	).

get_tx_data_root(#tx{ format = 2, data_root = DataRoot }) ->
    DataRoot;
get_tx_data_root(TX) ->
    (ar_tx:generate_chunk_tree(TX))#tx.data_root.