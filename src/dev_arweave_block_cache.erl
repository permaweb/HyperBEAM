%%% @doc A module that performs caching operations for the Arweave device, 
%%% focused on ensuring that block metadata is queriable via pseudo-paths.
-module(dev_arweave_block_cache).
-export([latest/1, heights/1, read/2, write/2]).
-export([path/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The pseudo-path prefix which the Arweave block cache should use.
-define(ARWEAVE_BLOCK_CACHE_PREFIX, <<"~arweave@2.9">>).

%% @doc Get the latest block from the cache.
latest(Opts) ->
    case heights(Opts) of
        {ok, []} ->
            ?event(arweave_cache, no_blocks_in_cache),
            not_found;
        {ok, Blocks} ->
            Latest = lists:max(Blocks),
            ?event(arweave_cache, {latest_block_from_cache, {latest, Latest}}),
            {ok, Latest}
    end.

%% @doc Get the list of blocks from the cache.
heights(Opts) ->
    AllBlocks =
        hb_cache:list_numbered(
            hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
                ?ARWEAVE_BLOCK_CACHE_PREFIX,
                <<"block">>,
                <<"height">>
            ]),
            Opts
        ),
    ?event(arweave_cache, {listed_blocks, length(AllBlocks)}),
    {ok, AllBlocks}.

%% @doc Read a block from the cache.
read(Block, Opts) ->
    Res = hb_cache:read(path(Block, Opts), Opts),
    ?event(arweave_cache, {read_block, {reference, Block}, {result, Res}}),
    Res.

%% @doc Return the path of a block that will be used in the cache.
path(Block, Opts) when is_integer(Block) ->
    hb_store:path(hb_opts:get(store, no_viable_store, Opts), [
        ?ARWEAVE_BLOCK_CACHE_PREFIX,
        <<"block">>,
        <<"height">>,
        hb_util:bin(Block)
    ]).

%% @doc Write a block to the cache and create pseudo-paths for it.
write(Block, Opts) ->
    {ok, Height} = hb_maps:find(<<"height">>, Block, Opts),
    {ok, BlockID} = hb_maps:find(<<"indep_hash">>, Block, Opts),
    {ok, BlockHash} = hb_maps:find(<<"hash">>, Block, Opts),
    {ok, MsgID} = hb_cache:write(Block, Opts),
    % Link the independent hash and the dependent hash to the written AO-Core
    % message ID.
    hb_cache:link(MsgID, BlockID, Opts),
    hb_cache:link(MsgID, BlockHash, Opts),
    % Link the block height pseudo-path to the message.
    hb_cache:link(MsgID, path(Height, Opts), Opts),
    ?event(arweave_cache, {wrote_block, {height, Height}, {message_id, MsgID}}),
    {ok, MsgID}.