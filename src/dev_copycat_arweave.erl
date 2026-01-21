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
            % maybe_index_ids(Block, Opts),
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
    {ok, Block} = hb_ao:resolve(
        <<
            ?ARWEAVE_DEVICE/binary,
            "/block=5Ya_2_jLshzNodGRVrBwlAhcEowIFgVbvOcN4j_MJI4QfodQ7Nd8ke7CMN9OnpK0"
        >>,
        Opts
    ),
    ?event(debug_test, {Block}),
    ?assertEqual(ok, maybe_index_ids(Block, Opts)),

    % Bundles with unsupprted signatures should still be indexed, but when
    % we go to desiarlize the data it will fail.
    % ?assertEqual({badmatch, unsupported_tx_format},
    %     hb_store_arweave:read(
    %         StoreOpts,
    %         <<"kK67S13W_8jM9JUw2umVamo0zh9v1DeVxWrru2evNco">>)),

    % This is a single item bundle with nested items. The immediately
    % bundled item is indexed, the nested items are not.
    {ok, TX5} = hb_store_arweave:read(StoreOpts, <<"c2ATDuTgwKCcHpAFZqSt13NC-tA4hdA7Aa2xBPuOzoE">>),
    ?assertEqual(<<"871231847">>, hb_maps:get(<<"reward">>, TX5, #{})),
    ?assert(hb_message:verify(TX5, all, #{})),
    TX5Item = hb_ao:get(<<"1">>, TX5,  #{}),
    TX5ItemID = hb_message:id(TX5Item, signed),
    ?assertEqual(<<"OBKr-7UrmjxFD-h-qP-XLuvCgtyuO_IDpBMgIytvusA">>, TX5ItemID),
    TX5ItemRead = hb_store_arweave:read(StoreOpts, <<"XJq09oboLC7Z5LQ0lsg2klHa3pkCW_H1kWeBnrMLmfc">>),
    ?event(debug_test, {{id, TX5ItemID}, {read, TX5ItemRead}}),
    ?assert(hb_message:verify(TX5ItemRead, all, #{})),
    ?assertEqual(TX5Item, TX5ItemRead),


    % ?assert(hb_message:verify(TX5Item, all, #{})),
    % {error not_found} = hb_store_arweave:read(StoreOpts, hb_message:id(TX5Item, signed)),
    ok.