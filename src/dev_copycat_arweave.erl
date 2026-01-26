%%% @doc A `~copycat@1.0' engine that fetches block data from an Arweave node for
%%% replication. This engine works in _reverse_ chronological order by default,
%%% fetching blocks from the latest known block towards the Genesis block. The
%%% node avoids retrieving blocks that are already present in the cache using
%%% `~arweave@2.9-pre''s built-in caching mechanism.
-module(dev_copycat_arweave).
-export([arweave/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_DEVICE, <<"~arweave@2.9-pre">>).
-define(ARWEAVE_INDEX_PATH, <<?ARWEAVE_DEVICE/binary, "/offset">>).

% GET /~cron@1.0/once&cron-path=~copycat@1.0/arweave

%% @doc Fetch blocks from an Arweave node between a given range, or from the
%% latest known block towards the Genesis block. If no range is provided, we
%% fetch blocks from the latest known block towards the Genesis block.
arweave(_Base, Request, Opts) ->
    {From, To} = parse_range(Request, Opts),
    fetch_blocks(Request, From, To, Opts).

%% @doc Parse the range from the request.
parse_range(Request, Opts) ->
    From =
        case hb_maps:find(<<"from">>, Request, Opts) of
            {ok, Height} -> Height;
            error ->
                {ok, LatestHeight} =
                    hb_ao:resolve(
                        <<?ARWEAVE_DEVICE/binary, "/current/height">>,
                        Opts
                    ),
                LatestHeight
        end,
    To = hb_maps:get(<<"to">>, Request, 0, Opts),
    {From, To}.

%% @doc Fetch blocks from an Arweave node between a given range.
fetch_blocks(Req, Current, Current, _Opts) ->
    ?event(copycat_arweave,
        {arweave_block_indexing_completed,
            {reached_target, Current},
            {initial_request, Req}
        }
    ),
    {ok, Current};
fetch_blocks(Req, Current, To, Opts) ->
    BlockRes =
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/block=",
                (hb_util:bin(Current))/binary
            >>,
            Opts
        ),
    process_block(BlockRes, Req, Current, To, Opts),
    fetch_blocks(Req, Current - 1, To, Opts).

%% @doc Process a block.
process_block(BlockRes, _Req, Current, To, Opts) ->
    case BlockRes of
        {ok, Block} ->
            maybe_index_ids(Block, Opts),
            ?event(
                copycat_short,
                {arweave_block_cached,
                    {height, Current},
                    {target, To}
                }
            );
        {error, not_found} ->
            ?event(
                copycat_short,
                {arweave_block_not_found,
                    {height, Current},
                    {target, To}
                }
            )
    end.

%% @doc Index the IDs of all transactions in the block if configured to do so.
maybe_index_ids(Block, Opts) ->
    case hb_opts:get(arweave_index_ids, false, Opts) of
        false -> ok;
        true ->
            IndexStore = hb_opts:get(arweave_index_store, no_store, Opts),
            BlockEndOffset = hb_util:int(
                hb_maps:get(<<"weave_size">>, Block, 0, Opts)),
            BlockSize = hb_util:int(
                hb_maps:get(<<"block_size">>, Block, 0, Opts)),
            BlockStartOffset = BlockEndOffset - BlockSize,
            ?event(debug_test, {
                {block_end_offset, BlockEndOffset},
                {block_size, BlockSize},
                {block_start_offset, BlockStartOffset}
            }),
            TXs = resolve_tx_headers(hb_maps:get(<<"txs">>, Block, [], Opts), Opts),
            Height = hb_maps:get(<<"height">>, Block, 0, Opts),
            TXsWithData = ar_block:generate_size_tagged_list_from_txs(TXs, Height),
            lists:foreach(fun
                ({{padding, _PaddingRoot}, _EndOffset}) ->
                    ok;
                ({{TX, _TXDataRoot}, EndOffset}) ->
                    ?event(debug_test, {
                        {tx, TX},
                        {end_offset, BlockStartOffset + EndOffset},
                        {tx_size, TX#tx.data_size},
                        {is_bundle_tx, is_bundle_tx(TX, Opts)}
                    }),
                    case is_bundle_tx(TX, Opts) of
                        false -> ok;
                        true ->
                            TXEndOffset = BlockStartOffset + EndOffset,
                            TXStartOffset = TXEndOffset - TX#tx.data_size,
                            hb_store_arweave:write_offset(
                                IndexStore,
                                hb_util:encode(TX#tx.id),
                                true,
                                TXEndOffset,
                                TX#tx.data_size
                            ),
                            {ok, {BundleIndex, HeaderSize}} = download_bundle_header(
                                TXEndOffset, TX#tx.data_size, Opts),
                            ?event(debug_test, {{bundle_index, BundleIndex}, {header_size, HeaderSize}}),
                            lists:foldl(
                                fun({ItemID, Size}, OffsetAcc) ->
                                    ItemEndOffset = OffsetAcc + Size,
                                    ?event(debug_test, {
                                        {item_id, {explicit, hb_util:encode(ItemID)}},
                                        {item_end_offset, ItemEndOffset},
                                        {size, Size}
                                    }),
                                    hb_store_arweave:write_offset(
                                        IndexStore,
                                        hb_util:encode(ItemID),
                                        false,
                                        ItemEndOffset,
                                        Size
                                    ),
                                    ItemEndOffset
                                end,
                                TXStartOffset + HeaderSize,
                                BundleIndex
                            )
                    end
                end,
                TXsWithData
            ),
            ok
    end.

is_bundle_tx(TX, _Opts) ->
    dev_arweave_common:type(TX) =/= binary.

download_bundle_header(EndOffset, Size, Opts) ->
    StartOffset = EndOffset - Size + 1,
    {ok, Chunk} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => StartOffset
        },
        Opts
    ),
    {_ItemsBin, BundleIndex, HeaderSize} = ar_bundles:decode_bundle_header(Chunk),
    {ok, {BundleIndex, HeaderSize}}.

resolve_tx_headers(TXIDs, Opts) ->
    lists:map(
        fun(TXID) ->
            {ok, StructuredTXHeader} =
                hb_ao:resolve(
                    #{ <<"device">> => <<"arweave@2.9-pre">> },
                    #{ 
                        <<"path">> => <<"tx">>,
                        <<"tx">> => TXID,
                        <<"exclude-data">> => true
                    },
                    Opts
                ),
            hb_message:convert(
                StructuredTXHeader,
                <<"tx@1.0">>,
                <<"structured@1.0">>,
                Opts)
        end,
        TXIDs
    ).

%%% Tests

index_ids_test() ->
    %% Test block: https://viewblock.io/arweave/block/1827942
    %% Note: this block includes a data item with an Ethereum signature. This
    %% signature type is not yet (as of Jan 2026) supported by ar_bundles.erl,
    %% however we should still be able to index it (we just can't deserialize
    %% it).
    Store = [hb_test_utils:test_store()],
    StoreOpts = #{ <<"index-store">> => Store },
    Opts = #{
        arweave_index_ids => true,
        arweave_index_store => StoreOpts
    },
    {ok, 1827941} = hb_ao:resolve(
        #{ <<"device">> => <<"copycat@1.0">> },
        #{
            <<"path">> => <<"arweave">>,
            <<"from">> => 1827942,
            <<"to">> => 1827941
        },
        Opts
    ),
    % WbRAQbeyjPHgopBKyi0PLeKWvYZr3rgZvQ7QY3ASJS4 is a bundle signed with
    % an Ethereum signature which is not supported by HB as of Jan 2026.
    % The bundle should be indexed even though we can't deserialized the
    % bundle itself.
    ?assertException(
        error,
        {badmatch, unsupported_tx_format},
        hb_store_arweave:read(
            StoreOpts,
            <<"WbRAQbeyjPHgopBKyi0PLeKWvYZr3rgZvQ7QY3ASJS4">>)
    ),
    % These 3 items are within the WbRAQbeyjPHgopBKyi0PLeKWvYZr3rgZvQ7QY3ASJS4
    % bundle.
    assert_item_read(StoreOpts,
        <<"ATi9pQF_eqb99UK84R5rq8lGfRGpilVQOYyth7rXxh8">>),
    assert_item_read(StoreOpts,
        <<"4VSfUbhMVZQHW5VfVwQZOmC5fR3W21DZgFCyz8CA-cE">>),
    assert_item_read(StoreOpts,
        <<"ZQRHZhktk6dAtX9BlhO1teOtVlGHoyaWP25kAlhxrM4">>),
    % The T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs can be deserialized so
    % we'll verify that some of its items were index and match the version
    % in the deserialized bundle.
    assert_bundle_read(
        StoreOpts,
        <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
        [
            {<<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>, <<"1">>},
            {<<"MgatoEjlO_YtdbxFi9Q7Hxbs0YQVcChddhSS7FsdeIg">>, <<"19">>},
            {<<"z-oKJfhMq5qoVFrljEfiBKgumaJmCWVxNJaavR5aPE8">>, <<"26">>}
        ]
    ),
    % Non-ans104 transaction in the block should not be indexed.
    ?assertEqual({error, not_found},
        hb_store_arweave:read(StoreOpts,
            <<"bXEgFm4K2b5VD64skBNAlS3I__4qxlM3Sm4Z5IXj3h8">>)),
    % Another bundle with an unsupported signature should be indexed even if
    % it can't be deserialized.
    ?assertException(
        error,
        {badmatch, unsupported_tx_format},
        hb_store_arweave:read(
            StoreOpts,
            <<"kK67S13W_8jM9JUw2umVamo0zh9v1DeVxWrru2evNco">>)
    ),
    assert_bundle_read(
        StoreOpts,
        <<"c2ATDuTgwKCcHpAFZqSt13NC-tA4hdA7Aa2xBPuOzoE">>,
        [
            {<<"OBKr-7UrmjxFD-h-qP-XLuvCgtyuO_IDpBMgIytvusA">>, <<"1">>}
        ]
    ),
   ok.

assert_bundle_read(StoreOpts, BundleID, ExpectedItems) ->
    ReadItems =
        lists:map(
            fun({ItemID, _Index}) ->
                assert_item_read(StoreOpts, ItemID)
            end,
            ExpectedItems
        ),
    Bundle = assert_item_read(StoreOpts, BundleID),
    lists:foreach(
        fun({{_ItemID, Index}, Item}) ->
            QueriedItem = hb_ao:get(Index, Bundle, #{}),
            ?assertEqual(hb_maps:without(?AO_CORE_KEYS, Item), hb_maps:without(?AO_CORE_KEYS, QueriedItem))
        end,
        lists:zip(ExpectedItems, ReadItems)
    ),
    ok.

assert_item_read(StoreOpts, ItemID) ->
    {ok, Item} = hb_store_arweave:read(StoreOpts, ItemID),
    ?assert(hb_message:verify(Item, all, #{})),
    ?assertEqual(ItemID, hb_message:id(Item, signed)),
    Item.

