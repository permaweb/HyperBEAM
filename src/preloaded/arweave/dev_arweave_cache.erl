%%% @doc A module that performs caching operations for the Arweave device,
%%% focused on ensuring that block metadata is queriable via pseudo-paths.
%%%
%%% Everything cached here is a peer's answer to `~arweave@2.9/block', which
%%% nothing has checked, so it is filed entirely under this device's own path
%%% namespace. The bare block hash names the block message
%%% `~arweave@2.9/sync' publishes, whose presence means this node validated the
%%% block and finished indexing it -- one name, one claim. A gateway response
%%% occupying it would make `validated' answer for blocks nobody checked, and
%%% would let a walk of `previous' links leave the validated chain.
%%%
%%% Named `dev_arweave_cache' rather than `dev_arweave_block_cache' because
%%% the Forge packager assigns a helper to its LONGEST matching `dev_*'
%%% prefix. Once `dev_arweave_block' exists as a device root, a module called
%%% `dev_arweave_block_cache' would be packaged under *that* device instead of
%%% `~arweave@2.9', and `dev_arweave''s calls to it would fail with `undef':
%%% the name they refer to would live in another device's package.
-module(dev_arweave_cache).
-export([latest/1, heights/1, read/2, write/2]).
-export([path/2, hash_path/1]).
-include("include/hb.hrl").

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
            hb_path:to_binary([
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
path(Block, _Opts) when is_integer(Block) ->
    hb_path:to_binary([
        ?ARWEAVE_BLOCK_CACHE_PREFIX,
        <<"block">>,
        <<"height">>,
        hb_util:bin(Block)
    ]).

%% @doc Return the path a block fetched from a gateway is filed under by one of
%% its two hashes. A hash arrives in a peer response, and `hb_path:to_binary/1'
%% does not collapse `..', so a value carrying a separator is refused rather
%% than resolved by the filesystem.
hash_path(Hash) when is_binary(Hash) ->
    case binary:match(Hash, [<<"/">>, <<"..">>, <<0>>]) of
        nomatch ->
            hb_path:to_binary([
                ?ARWEAVE_BLOCK_CACHE_PREFIX,
                <<"block">>,
                <<"hash">>,
                Hash
            ]);
        _ ->
            throw({unsafe_block_hash_path, Hash})
    end.

%% @doc Write a block to the cache and create pseudo-paths for it.
write(Block, Opts) ->
    {ok, Height} = hb_maps:find(<<"height">>, Block, Opts),
    {ok, BlockID} = hb_maps:find(<<"indep_hash">>, Block, Opts),
    {ok, BlockHash} = hb_maps:find(<<"hash">>, Block, Opts),
    {ok, MsgID} = hb_cache:write(Block, Opts),
    % Link both the independent hash and the solution hash to the written
    % AO-Core message ID, under this device's own namespace.
    hb_cache:link(MsgID, hash_path(BlockID), Opts),
    hb_cache:link(MsgID, hash_path(BlockHash), Opts),
    % Link the block height pseudo-path to the message.
    hb_cache:link(MsgID, path(Height, Opts), Opts),
    ?event(arweave_cache, {wrote_block, {height, Height}, {message_id, MsgID}}),
    {ok, MsgID}.
