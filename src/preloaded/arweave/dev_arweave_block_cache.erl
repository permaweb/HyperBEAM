%%% @doc A module that performs caching operations for the Arweave device, 
%%% focused on ensuring that block metadata is queriable via pseudo-paths.
%%% `arweave-block-store' selects the store for headers, their height/hash
%%% aliases, and the compact block index, defaulting to the node's `store'.
%%% The index's physical group is `~arweave@2.9/blocks', with ordered children
%%% `weave-size=PADDEDEND&height=PADDEDHEIGHT'. Each value is a compact JSON
%%% pair of independent hash and TX root. A separate height pointer selects
%%% the canonical row; the device exposes these stored values as AO-Core messages.
-module(dev_arweave_block_cache).
-export([latest/1, heights/1, read/2, write/2]).
-export([path/2]).
-export([index/2, indexed/2, blocks/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc The pseudo-path prefix which the Arweave block cache should use.
-define(ARWEAVE_BLOCK_CACHE_PREFIX, <<"~arweave@2.9">>).
-define(BLOCKS, <<"~arweave@2.9/blocks">>).

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
            opts(Opts)
        ),
    ?event(arweave_cache, {listed_blocks, length(AllBlocks)}),
    {ok, AllBlocks}.

%% @doc Read a cached header, honoring the compact index's canonical hash.
read(Height, Opts) when is_integer(Height) ->
    case indexed(Height, Opts) of
        {ok, Key} ->
            maybe
                {ok, Value} ?= hb_store:read(
                    <<?BLOCKS/binary, "/", Key/binary>>, opts(Opts)),
                [Hash, _TXRoot] = hb_json:decode(Value),
                read(Hash, Opts)
            end;
        {error, not_found} -> read(path(Height, Opts), Opts)
    end;
read(Block, Opts) ->
    Res = hb_cache:read(path(Block, Opts), opts(Opts)),
    ?event(arweave_cache, {read_block, {reference, Block}, {result, Res}}),
    Res.

%% @doc Return the path of a block that will be used in the cache.
path(Block, _Opts) when is_integer(Block) ->
    hb_path:to_binary([
        ?ARWEAVE_BLOCK_CACHE_PREFIX,
        <<"block">>,
        <<"height">>,
        hb_util:bin(Block)
    ]);
path(Block, _Opts) -> Block.

%% @doc Write a block to the cache and create pseudo-paths for it.
write(Block, RawOpts) ->
    Opts = opts(RawOpts),
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

%% @doc Store a compact block-index entry separately from cached headers.
%% The height pointer selects the canonical row after reindexing. Stores do
%% not expose deletion, so superseded ordered rows are ignored when listing.
index(Entry, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, [], Opts),
    Height = hb_util:int(hb_maps:get(<<"height">>, Entry, not_found, Opts)),
    End = hb_util:int(hb_maps:get(<<"weave-size">>, Entry, not_found, Opts)),
    Key = index_key(End, Height),
    Value = hb_json:encode([
        hb_maps:get(<<"hash">>, Entry, not_found, Opts),
        hb_maps:get(<<"tx-root">>, Entry, not_found, Opts)
    ]),
    maybe
        ok ?= hb_store:group(Store, ?BLOCKS, Opts),
        ok ?= hb_store:write(Store, #{ <<?BLOCKS/binary, "/", Key/binary>> => Value }, Opts),
        hb_store:write(Store, #{ index_height(Height) => Key }, Opts)
    end.

%% @doc Whether this height has a compact index entry, independent of headers.
indexed(Height, Opts) ->
    hb_store:read(index_height(Height), opts(Opts)).

%% @doc Seek compact entries by end weave size and height, inclusively.
%% Both decimal components are padded to 20 digits, as match index offsets are.
blocks(Req, RawOpts) ->
    Opts = opts(RawOpts),
    Direction = hb_util:atom(hb_maps:get(<<"direction">>, Req, asc, Opts)),
    Height = hb_util:int(hb_maps:get(<<"height">>, Req,
        case Direction of asc -> 0; desc -> 99999999999999999999 end, Opts)),
    Seek = case hb_maps:find(<<"weave-size">>, Req, Opts) of
        {ok, End} -> {ok, index_key(hb_util:int(End), Height)};
        error ->
            case hb_maps:is_key(<<"height">>, Req, Opts) of
                true -> indexed(Height, Opts);
                false -> {ok, none}
            end
    end,
    maybe
        {ok, From} ?= Seek,
        index_entries(#{ <<"list">> => ?BLOCKS, <<"from">> => From,
            <<"direction">> => Direction, <<"limit">> => 1 },
            hb_util:int(hb_maps:get(<<"limit">>, Req, 1, Opts)), [], Opts)
    else
        {error, not_found} -> {ok, []};
        Error -> Error
    end.

%% @doc Read bounded ordered rows, skipping entries superseded at their height.
index_entries(_Req, 0, Acc, _Opts) -> {ok, lists:reverse(Acc)};
index_entries(Req, Limit, Acc, Opts) when Limit > 0 ->
    maybe
        {ok, [Key]} ?= hb_store:list(Req, Opts),
        [<<"weave-size=", End/binary>>, <<"height=", Height/binary>>] =
            binary:split(Key, <<"&">>),
        H = hb_util:int(Height),
        Next = case maps:get(<<"direction">>, Req) of
            asc -> index_key(hb_util:int(End), H + 1);
            desc when H > 0 -> index_key(hb_util:int(End), H - 1);
            desc -> index_key(hb_util:int(End) - 1, 99999999999999999999)
        end,
        case indexed(H, Opts) of
            {ok, Key} ->
                {ok, Value} = hb_store:read(<<?BLOCKS/binary, "/", Key/binary>>, Opts),
                [Hash, TXRoot] = hb_json:decode(Value),
                Entry = #{ <<"height">> => H, <<"weave-size">> => hb_util:int(End),
                    <<"hash">> => Hash, <<"tx-root">> => TXRoot },
                index_entries(Req#{ <<"from">> := Next }, Limit - 1,
                    [Entry | Acc], Opts);
            _ -> index_entries(Req#{ <<"from">> := Next }, Limit, Acc, Opts)
        end
    else
        {ok, []} -> {ok, lists:reverse(Acc)};
        {error, not_found} -> {ok, lists:reverse(Acc)};
        Error -> Error
    end.

%% @doc The canonical ordered key at a height, without a full block header.
index_height(Height) ->
    <<"~arweave@2.9/block-index/", (hb_util:bin(Height))/binary>>.

%% @doc A composite child key preserving numeric weave-size and height order.
index_key(End, Height) ->
    iolist_to_binary(io_lib:format(
        "weave-size=~20..0B&height=~20..0B", [End, Height])).

%% @doc Select the block cache's store without changing other node options.
opts(Opts) ->
    Opts#{ <<"store">> =>
        hb_opts:get(arweave_block_store, hb_opts:get(store, [], Opts), Opts) }.
