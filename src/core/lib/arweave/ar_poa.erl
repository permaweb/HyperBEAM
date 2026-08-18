%%% @doc This module implements all mechanisms required to validate a proof of access
%%% for a chunk of data received from the network.
-module(ar_poa).

-export([get_padded_offset/1, get_padded_offset/2]).

-include("include/ar.hrl").

%% @doc Return the smallest multiple of 256 KiB >= Offset
%% counting from ar_block:strict_data_split_threshold().
get_padded_offset(Offset) ->
	get_padded_offset(Offset, ar_block:strict_data_split_threshold()).

%% @doc Return the smallest multiple of 256 KiB >= Offset
%% counting from StrictDataSplitThreshold.
get_padded_offset(Offset, StrictDataSplitThreshold) ->
	Diff = Offset - StrictDataSplitThreshold,
	StrictDataSplitThreshold + ((Diff - 1) div (?DATA_CHUNK_SIZE) + 1) * (?DATA_CHUNK_SIZE).
