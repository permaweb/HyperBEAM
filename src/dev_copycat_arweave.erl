%%% @doc A `~copycat@1.0' engine that fetches block data from an Arweave node for
%%% replication. This engine works in _reverse_ chronological order by default,
%%% fetching blocks from the latest known block towards the Genesis block. The
%%% node avoids retrieving blocks that are already present in the cache using
%%% `~arweave@2.9-pre''s built-in caching mechanism.
-module(dev_copycat_arweave).
-export([arweave/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

-define(ARWEAVE_DEVICE, <<"~arweave@2.9-pre">>).

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
                case hb_ao:resolve(
                    <<?ARWEAVE_DEVICE/binary, "/current/height">>,
                    Opts
                ) of
                    {ok, LatestHeight} -> LatestHeight;
                    {error, _} -> 0
                end
        end,
    To = hb_maps:get(<<"to">>, Request, 0, Opts),
    {hb_util:int(From), hb_util:int(To)}.

%% @doc Fetch blocks from an Arweave node between a given range.
fetch_blocks(Req, Current, To, _Opts) when Current < To ->
    ?event(copycat_arweave,
        {arweave_block_indexing_completed,
            {reached_target, To},
            {initial_request, Req}
        }
    ),
    {ok, To};
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
            {IndexedItems, SkippedTxs} = maybe_index_ids(Block, Opts),
            ?event(
                copycat_short,
                {arweave_block_cached,
                    {height, Current},
                    {indexed_items, IndexedItems},
                    {skipped_txs, SkippedTxs},
                    {target, To}
                }
            );
        {error, _} = Error ->
            ?event(
                copycat_short,
                {arweave_block_not_found,
                    {height, Current},
                    {target, To},
                    {reason, Error}} 
            )
    end.

%% @doc Index the IDs of all transactions in the block if configured to do so.
maybe_index_ids(Block, Opts) ->
    case hb_opts:get(arweave_index_ids, false, Opts) of
        false -> {0, 0};
        true ->
            IndexStore = hb_opts:get(arweave_index_store, no_store, Opts),
            BlockEndOffset = hb_util:int(
                hb_maps:get(<<"weave_size">>, Block, 0, Opts)),
            BlockSize = hb_util:int(
                hb_maps:get(<<"block_size">>, Block, 0, Opts)),
            BlockStartOffset = BlockEndOffset - BlockSize,
            {TXs, SkippedFromHeaders} = resolve_tx_headers(hb_maps:get(<<"txs">>, Block, [], Opts), Opts),
            Height = hb_maps:get(<<"height">>, Block, 0, Opts),
            TXsWithData = ar_block:generate_size_tagged_list_from_txs(TXs, Height),
            {IndexedItems, SkippedFromBundles} = lists:foldl(fun
                ({{padding, _PaddingRoot}, _EndOffset}, {ItemsAcc, SkippedAcc}) ->
                    {ItemsAcc, SkippedAcc};
                ({{TX, _TXDataRoot}, EndOffset}, {ItemsAcc, SkippedAcc}) ->
                    case is_bundle_tx(TX, Opts) of
                        false -> {ItemsAcc, SkippedAcc};
                        true ->
                            TXID = hb_util:encode(TX#tx.id),
                            TXEndOffset = BlockStartOffset + EndOffset,
                            TXStartOffset = TXEndOffset - TX#tx.data_size,
                            hb_store_arweave:write_offset(
                                IndexStore,
                                TXID,
                                true,
                                TXStartOffset,
                                TX#tx.data_size
                            ),
                            case download_bundle_header(
                                TXEndOffset, TX#tx.data_size, Opts
                            ) of
                                {ok, {BundleIndex, HeaderSize}} ->
                                    _ = lists:foldl(
                                        fun({ItemID, Size}, ItemStartOffset) ->
                                            hb_store_arweave:write_offset(
                                                IndexStore,
                                                hb_util:encode(ItemID),
                                                false,
                                                ItemStartOffset,
                                                Size
                                            ),
                                            ItemStartOffset + Size
                                        end,
                                        TXStartOffset + HeaderSize,
                                        BundleIndex
                                    ),
                                    {ItemsAcc + length(BundleIndex), SkippedAcc};
                                {error, Reason} ->
                                    ?event(
                                        copycat_short,
                                        {arweave_bundle_skipped,
                                            {tx_id, {explicit, TXID}},
                                            {reason, Reason}
                                        }
                                    ),
                                    {ItemsAcc, SkippedAcc + 1}
                            end
                    end
                end,
                {0, 0},
                TXsWithData
            ),
            {IndexedItems, SkippedFromHeaders + SkippedFromBundles}
    end.

is_bundle_tx(TX, _Opts) ->
    dev_arweave_common:type(TX) =/= binary.

download_bundle_header(EndOffset, Size, Opts) ->
    StartOffset = EndOffset - Size + 1,
    case hb_ao:resolve(
        <<
            ?ARWEAVE_DEVICE/binary,
            "/chunk&offset=",
            (hb_util:bin(StartOffset))/binary
        >>,
        Opts
    ) of
        {ok, FirstChunk} ->
            % Most bundle headers can fit in a single chunk, but those with
            % thousands of items might require multiple chunks to fully
            % represent the item index.
            HeaderSize = ar_bundles:bundle_header_size(FirstChunk),
            case header_chunk(HeaderSize, FirstChunk, StartOffset, Opts) of
                {ok, BundleHeader} ->
                    {_ItemsBin, BundleIndex} =
                        ar_bundles:decode_bundle_header(BundleHeader),
                    {ok, {BundleIndex, HeaderSize}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

header_chunk(HeaderSize, FirstChunk, _StartOffset, _Opts)
        when HeaderSize =< byte_size(FirstChunk) ->
    {ok, FirstChunk};
header_chunk(HeaderSize, _FirstChunk, StartOffset, Opts) ->
    hb_ao:resolve(
        <<
            ?ARWEAVE_DEVICE/binary,
            "/chunk&offset=",
            (hb_util:bin(StartOffset))/binary,
            "&length=",
            (hb_util:bin(HeaderSize))/binary
        >>,
        Opts
    ).

resolve_tx_headers(TXIDs, Opts) ->
    lists:foldr(
        fun(TXID, {Acc, SkippedAcc}) ->
            case resolve_tx_header(TXID, Opts) of
                {ok, TX} -> {[TX | Acc], SkippedAcc};
                skip -> {Acc, SkippedAcc + 1}
            end
        end,
        {[], 0},
        TXIDs
    ).

resolve_tx_header(TXID, Opts) ->
    try
        case hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/tx&tx=",
                TXID/binary,
                "&exclude-data=true"
            >>,
            Opts
        ) of
            {ok, StructuredTXHeader} ->
                {ok,
                    hb_message:convert(
                        StructuredTXHeader,
                        <<"tx@1.0">>,
                        <<"structured@1.0">>,
                        Opts)};
            {error, ResolveError} ->
                ?event(
                    copycat_short,
                    {arweave_tx_skipped,
                        {tx_id, {explicit, TXID}},
                        {reason, ResolveError}
                    }
                ),
                skip
        end
    catch
        Class:Reason:_ ->
            ?event(
                copycat_short,
                {arweave_tx_skipped,
                    {tx_id, {explicit, TXID}},
                    {class, Class},
                    {reason, Reason}
                }
            ),
            skip
    end.


%%% Tests

index_ids_test() ->
    %% Test block: https://viewblock.io/arweave/block/1827942
    %% Note: this block includes a data item with an Ethereum signature. This
    %% signature type is not yet (as of Jan 2026) supported by ar_bundles.erl,
    %% however we should still be able to index it (we just can't deserialize
    %% it).
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    {ok, 1827942} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827942&to=1827942">>,
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
    assert_item_read(Opts,
        <<"ATi9pQF_eqb99UK84R5rq8lGfRGpilVQOYyth7rXxh8">>),
    assert_item_read(Opts,
        <<"4VSfUbhMVZQHW5VfVwQZOmC5fR3W21DZgFCyz8CA-cE">>),
    assert_item_read(Opts,
        <<"ZQRHZhktk6dAtX9BlhO1teOtVlGHoyaWP25kAlhxrM4">>),
    % The T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs can be deserialized so
    % we'll verify that some of its items were index and match the version
    % in the deserialized bundle.
    assert_bundle_read(
        Opts,
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
        Opts,
        <<"c2ATDuTgwKCcHpAFZqSt13NC-tA4hdA7Aa2xBPuOzoE">>,
        [
            {<<"OBKr-7UrmjxFD-h-qP-XLuvCgtyuO_IDpBMgIytvusA">>, <<"1">>}
        ]
    ),
   ok.

bundle_header_index_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    TXID = <<"bnMTI7LglBGSaK5EdV_juh6GNtXLm0cd5lkd2q4nlT0">>,
    {ok, #{ <<"body">> := OffsetBody }} =
        hb_http:request(
            #{
                <<"path">> => <<"/arweave/tx/", TXID/binary, "/offset">>,
                <<"method">> => <<"GET">>
            },
            Opts
        ),
    OffsetMsg = hb_json:decode(OffsetBody),
    EndOffset = hb_util:int(maps:get(<<"offset">>, OffsetMsg)),
    Size = hb_util:int(maps:get(<<"size">>, OffsetMsg)),
    {ok, {BundleIndex, _HeaderSize}} =
        download_bundle_header(EndOffset, Size, Opts),
    ?assertEqual(15000, length(BundleIndex)),
    ok.

index_ids_ecdsa_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, 1827904} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=1827904&to=1827904">>,
            Opts
        ),
    assert_bundle_read(
        Opts,
        <<"VNhX_pSANk_8j0jZBR5bh_5jr-lkfbHDjtHd8FKqx7U">>,
        [
            {<<"3xDKhrCQcPuBtcm1ipZS5C9gAfFYClgHuHOHAXGfchM">>, <<"1">>},
            {<<"JantC8f89VE-RidArHnU9589gY5T37NDXnWpI7H_psc">>, <<"7">>}
        ]
    ),
    ok.

non_string_tags_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    ?assertEqual(skip, 
        resolve_tx_header(<<"752P6t4cOjMabYHqzC6hyLhxyo4YKZLblg7va_J21YE">>, Opts)),
    ok.

setup_index_opts() ->
    TestStore = hb_test_utils:test_store(),
    StoreOpts = #{ <<"index-store">> => [TestStore] },
    Store = [
        TestStore,
        #{
            <<"store-module">> => hb_store_fs,
            <<"name">> => <<"cache-mainnet">>
        },
        #{
            <<"store-module">> => hb_store_arweave,
            <<"name">> => <<"cache-arweave">>,
            <<"index-store">> => [TestStore],
            <<"arweave-node">> => <<"https://arweave.net">>
        },
        #{
            <<"store-module">> => hb_store_gateway,
            <<"subindex">> => [
                #{
                    <<"name">> => <<"Data-Protocol">>,
                    <<"value">> => <<"ao">>
                }
            ],
            <<"local-store">> => [TestStore]
        },
        #{
            <<"store-module">> => hb_store_gateway,
            <<"local-store">> => [TestStore]
        }
    ],
    Opts = #{
        store => Store,
        arweave_index_ids => true,
        arweave_index_store => StoreOpts
    },
    {TestStore, StoreOpts, Opts}.

assert_bundle_read(Opts, BundleID, ExpectedItems) ->
    ReadItems =
        lists:map(
            fun({ItemID, _Index}) ->
                assert_item_read(Opts, ItemID)
            end,
            ExpectedItems
        ),
    Bundle = assert_item_read(Opts, BundleID),
    lists:foreach(
        fun({{_ItemID, Index}, Item}) ->
            QueriedItem = hb_ao:get(Index, Bundle, Opts),
            ?assertEqual(hb_maps:without(?AO_CORE_KEYS, Item), hb_maps:without(?AO_CORE_KEYS, QueriedItem))
        end,
        lists:zip(ExpectedItems, ReadItems)
    ),
    ok.

assert_item_read(Opts, ItemID) ->
    {ok, Item} = hb_ao:resolve(ItemID, Opts),
    ?assert(hb_message:verify(Item, all, Opts)),
    ?assertEqual(ItemID, hb_message:id(Item, signed)),
    Item.
