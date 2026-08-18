%%% @doc A module that performs caching operations for the Arweave device,
%%% focused on ensuring that block metadata is queriable via pseudo-paths.
%%%
%%% Everything cached here is a peer's answer to `~arweave@2.9/block', which
%%% nothing has checked, so it is filed entirely under this device's own path
%%% namespace. The bare block hash names the block message
%%% `~arweave@2.9/sync' publishes, whose presence means this node validated the
%%% block and finished indexing it -- one name, one claim. A gateway response
%%% occupying it would make `validated' answer for blocks nobody checked, and
%%% would let `arweave-block@2.9/previous' leave the validated chain.
%%%
-module(lib_arweave_cache).
-export([read/2, write/2, hash_path/1]).
-include("include/hb.hrl").

%% @doc The pseudo-path prefix which the Arweave block cache should use.
-define(ARWEAVE_BLOCK_CACHE_PREFIX, <<"~arweave@2.9">>).

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
            throw({'unsafe-block-hash-path', Hash})
    end.

%% @doc Write a block to the cache and create pseudo-paths for it.
write(Block, Opts) ->
    maybe
        {ok, Height} ?= hb_maps:find(<<"height">>, Block, Opts),
        {ok, BlockID} ?= hb_maps:find(<<"indep_hash">>, Block, Opts),
        {ok, BlockHash} ?= hb_maps:find(<<"hash">>, Block, Opts),
        {ok, MsgID} ?=
            hb_cache:write(
                Block,
                Opts#{ <<"match-index">> => false }
            ),
        % Link both the independent hash and the solution hash to the written
        % AO-Core message ID, under this device's own namespace.
        ok ?= hb_cache:link(MsgID, hash_path(BlockID), Opts),
        ok ?= hb_cache:link(MsgID, hash_path(BlockHash), Opts),
        % Link the block height pseudo-path to the message.
        ok ?= hb_cache:link(MsgID, path(Height, Opts), Opts),
        {ok, _Written} ?= hb_cache:read(path(Height, Opts), Opts),
        ?event(arweave_cache,
            {wrote_block, {height, Height}, {message_id, MsgID}}),
        {ok, MsgID}
    end.
