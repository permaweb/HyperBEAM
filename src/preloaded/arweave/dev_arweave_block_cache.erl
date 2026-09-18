%%% @doc A module that performs caching operations for the Arweave device, 
%%% focused on ensuring that block metadata is queriable via pseudo-paths.
-module(dev_arweave_block_cache).
-export([latest/1, heights/1, read/2, read_offset/2, entries/4]).
-export([write/2, path/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The pseudo-path prefix which the Arweave block cache should use.
-define(ARWEAVE_BLOCK_CACHE_PREFIX, <<"~arweave@2.9">>).
-define(OFFSET_PATH, <<?ARWEAVE_BLOCK_CACHE_PREFIX/binary, "/block/offset">>).
-define(HEIGHT_DIGITS, 20).

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

%% @doc Read the first block whose ending weave offset is at least `Offset'.
read_offset(Offset, Opts) ->
    case entries(asc, {hb_util:int(Offset), 0}, 1, Opts) of
        {ok, [Block]} -> {ok, Block};
        {ok, []} -> {error, not_found};
        Error -> Error
    end.

%% @doc Read block-index entries in weave order from an optional boundary.
entries(Direction, From, Limit, Opts) ->
    Store = hb_opts:get(store, [], Opts),
    Req0 = #{
        <<"list">> => ?OFFSET_PATH,
        <<"direction">> => Direction,
        <<"limit">> => Limit
    },
    Req =
        case From of
            none -> Req0;
            {Offset, Height} ->
                Req0#{ <<"from">> => offset_key(Offset, Height, Opts) }
        end,
    case hb_store:list(Store, Req, Opts) of
        {ok, StoreKeys} ->
            Keys = hb_store_utils:apply_list_bounds(StoreKeys, Req),
            {ok,
                lists:filtermap(
                    fun(Key) ->
                        case hb_cache:read([?OFFSET_PATH, Key], Opts) of
                            {ok, Block} -> {true, Block};
                            _ -> false
                        end
                    end,
                    Keys
                )
            };
        {error, not_found} -> {ok, []};
        Error -> Error
    end.

%% @doc Return the path of a block that will be used in the cache.
path(Block, _Opts) when is_integer(Block) ->
    hb_path:to_binary([
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
    {ok, WeaveSize} = hb_maps:find(<<"weave_size">>, Block, Opts),
    {ok, MsgID} = hb_cache:write(Block, Opts),
    % Link the independent hash and the dependent hash to the written AO-Core
    % message ID.
    hb_cache:link(MsgID, BlockID, Opts),
    hb_cache:link(MsgID, BlockHash, Opts),
    % Link the block height pseudo-path to the message.
    hb_cache:link(MsgID, path(Height, Opts), Opts),
    % Link the block's ending weave offset and height to the message. Height
    % keeps consecutive zero-size blocks distinct.
    hb_cache:link(
        MsgID,
        [?OFFSET_PATH, offset_key(WeaveSize, Height, Opts)],
        Opts
    ),
    ?event(arweave_cache, {wrote_block, {height, Height}, {message_id, MsgID}}),
    {ok, MsgID}.

%% @doc Encode an offset and height into one lexicographically ordered child.
offset_key(Offset, Height, _Opts) ->
    OffsetInt = hb_util:int(Offset),
    OffsetBits = <<OffsetInt:256/unsigned-big-integer>>,
    HeightBin = hb_util:bin(
        io_lib:format("~*..0B", [?HEIGHT_DIGITS, hb_util:int(Height)])
    ),
    <<(binary:encode_hex(OffsetBits))/binary, "-", HeightBin/binary>>.
