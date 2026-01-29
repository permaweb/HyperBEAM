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

-record(perf, {
    start_time = 0,                      %% monotonic start time (microseconds)
    blocks_processed = 0,                %% count (total)
    resolve_block_time = 0,              %% microseconds
    resolve_tx_count = 0,                %% count (total)
    resolve_tx_time = 0,                 %% microseconds
    items_indexed = 0,                   %% count (total)
    write_offset_time = 0,               %% microseconds
    bundles_processed = 0,               %% count (total)
    download_bundle_header_time = 0,     %% microseconds
    last_block_items_indexed = 0,        %% count (per-block)
    last_block_total_txs = 0,            %% count (per-block)
    last_block_bundle_txs = 0,           %% count (per-block)
    last_block_skipped_txs = 0           %% count (per-block)
}).

% GET /~cron@1.0/once&cron-path=~copycat@1.0/arweave

%% @doc Fetch blocks from an Arweave node between a given range, or from the
%% latest known block towards the Genesis block. If no range is provided, we
%% fetch blocks from the latest known block towards the Genesis block.
arweave(_Base, Request, Opts) ->
    {From, To} = parse_range(Request, Opts),
    Perf = perf_init(),
    fetch_blocks(Request, From, To, Perf, Opts).

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

%% @doc Initialize performance tracker at start of run.
perf_init() ->
    #perf{start_time = erlang:monotonic_time(microsecond)}.

%% @doc Calculate total elapsed time in microseconds.
perf_elapsed(#perf{start_time = Start}) ->
    erlang:monotonic_time(microsecond) - Start.

%% @doc Generic timing wrapper - updates perf record with timing data.
perf_time(Fun, Perf, TimeField) ->
    {Time, Result} = timer:tc(Fun),
    NewPerf = update_perf(Perf, TimeField, Time),
    {Result, NewPerf}.

%% @doc Update perf record with new timing data.
update_perf(Perf, resolve_block_time, Time) ->
    Perf#perf{
        resolve_block_time = Perf#perf.resolve_block_time + Time
    };
update_perf(Perf, resolve_tx_time, Time) ->
    Perf#perf{
        resolve_tx_time = Perf#perf.resolve_tx_time + Time,
        resolve_tx_count = Perf#perf.resolve_tx_count + 1
    };
update_perf(Perf, write_offset_time, Time) ->
    Perf#perf{
        write_offset_time = Perf#perf.write_offset_time + Time
    };
update_perf(Perf, download_bundle_header_time, Time) ->
    Perf#perf{
        download_bundle_header_time = Perf#perf.download_bundle_header_time + Time,
        bundles_processed = Perf#perf.bundles_processed + 1
    }.

%% @doc Calculate percentage of elapsed time that an operation took.
perf_pct(_OperationTime, 0) -> 0.0;
perf_pct(OperationTime, ElapsedTime) ->
    (OperationTime / ElapsedTime) * 100.0.

%% @doc Convert perf record to map for event logging (running totals).
perf_to_stats(Perf) ->
    Elapsed = perf_elapsed(Perf),
    #{
        blocks_processed => Perf#perf.blocks_processed,
        resolve_block_time_us => Perf#perf.resolve_block_time,
        resolve_block_pct => perf_pct(Perf#perf.resolve_block_time, Elapsed),
        resolve_tx_count => Perf#perf.resolve_tx_count,
        resolve_tx_time_us => Perf#perf.resolve_tx_time,
        resolve_tx_pct => perf_pct(Perf#perf.resolve_tx_time, Elapsed),
        items_indexed => Perf#perf.items_indexed,
        write_offset_time_us => Perf#perf.write_offset_time,
        write_offset_pct => perf_pct(Perf#perf.write_offset_time, Elapsed),
        bundles_processed => Perf#perf.bundles_processed,
        download_bundle_header_time_us => Perf#perf.download_bundle_header_time,
        download_bundle_header_pct => perf_pct(Perf#perf.download_bundle_header_time, Elapsed),
        elapsed_us => Elapsed
    }.

%% @doc Get CSV file path from options or use default.
perf_csv_path(Opts) ->
    hb_opts:get(arweave_perf_csv_path, <<"copycat_perf.csv">>, Opts).

%% @doc Write CSV header if file doesn't exist.
perf_csv_write_header(FilePath) ->
    try
        case filelib:is_file(FilePath) of
            false ->
                Header = "height,blocks_processed,resolve_block_time_us,resolve_block_pct,"
                    "resolve_tx_count,resolve_tx_time_us,resolve_tx_pct,"
                    "items_indexed,write_offset_time_us,write_offset_pct,"
                    "bundles_processed,download_bundle_header_time_us,download_bundle_header_pct,"
                    "elapsed_us\n",
                file:write_file(FilePath, Header, [write]);
            true ->
                ok
        end
    catch
        _:_ -> ok
    end.

%% @doc Format perf stats as CSV row.
perf_csv_format_row(Height, PerfStats) ->
    BlocksProcessed = maps:get(blocks_processed, PerfStats),
    ResolveBlockTime = maps:get(resolve_block_time_us, PerfStats),
    ResolveBlockPct = maps:get(resolve_block_pct, PerfStats),
    ResolveTxCount = maps:get(resolve_tx_count, PerfStats),
    ResolveTxTime = maps:get(resolve_tx_time_us, PerfStats),
    ResolveTxPct = maps:get(resolve_tx_pct, PerfStats),
    ItemsIndexed = maps:get(items_indexed, PerfStats),
    WriteOffsetTime = maps:get(write_offset_time_us, PerfStats),
    WriteOffsetPct = maps:get(write_offset_pct, PerfStats),
    BundlesProcessed = maps:get(bundles_processed, PerfStats),
    DownloadBundleHeaderTime = maps:get(download_bundle_header_time_us, PerfStats),
    DownloadBundleHeaderPct = maps:get(download_bundle_header_pct, PerfStats),
    Elapsed = maps:get(elapsed_us, PerfStats),
    io_lib:format("~w,~w,~w,~.2f,~w,~w,~.2f,~w,~w,~.2f,~w,~w,~.2f,~w\n",
        [Height, BlocksProcessed, ResolveBlockTime, ResolveBlockPct,
         ResolveTxCount, ResolveTxTime, ResolveTxPct,
         ItemsIndexed, WriteOffsetTime, WriteOffsetPct,
         BundlesProcessed, DownloadBundleHeaderTime, DownloadBundleHeaderPct,
         Elapsed]).

%% @doc Write perf stats to CSV file.
perf_csv_write(Height, PerfStats, Opts) ->
    try
        FilePath = perf_csv_path(Opts),
        perf_csv_write_header(FilePath),
        RowIoList = perf_csv_format_row(Height, PerfStats),
        Row = iolist_to_binary(RowIoList),
        file:write_file(FilePath, Row, [append])
    catch
        _:_ -> ok
    end.


%% @doc Fetch blocks from an Arweave node between a given range.
fetch_blocks(Req, Current, To, _Perf, _Opts) when Current < To ->
    ?event(copycat_arweave,
        {arweave_block_indexing_completed,
            {reached_target, To},
            {initial_request, Req}
        }
    ),
    {ok, To};
fetch_blocks(Req, Current, To, Perf, Opts) ->
    {BlockRes, Perf1} = perf_time(
        fun() ->
            hb_ao:resolve(
                <<
                    ?ARWEAVE_DEVICE/binary,
                    "/block=",
                    (hb_util:bin(Current))/binary
                >>,
                Opts
            )
        end,
        Perf,
        resolve_block_time
    ),
    Perf2 = Perf1#perf{blocks_processed = Perf1#perf.blocks_processed + 1},
    Perf3 = process_block(BlockRes, Req, Current, To, Perf2, Opts),
    fetch_blocks(Req, Current - 1, To, Perf3, Opts).

%% @doc Process a block.
process_block(BlockRes, _Req, Current, To, PerfAfterBlock, Opts) ->
    case BlockRes of
        {ok, Block} ->
            PerfAfterIndex = maybe_index_ids(Block, PerfAfterBlock, Opts),
            PerfStats = perf_to_stats(PerfAfterIndex),
            ?event(
                copycat_short,
                {arweave_block_cached,
                    {height, Current},
                    {items_indexed, PerfAfterIndex#perf.last_block_items_indexed},
                    {total_txs, PerfAfterIndex#perf.last_block_total_txs},
                    {bundle_txs, PerfAfterIndex#perf.last_block_bundle_txs},
                    {skipped_txs, PerfAfterIndex#perf.last_block_skipped_txs},
                    {target, To}
                }
            ),
            ?event(
                copycat_perf,
                {arweave_block_perf,
                    {height, Current},
                    {perf, PerfStats}
                }
            ),
            perf_csv_write(Current, PerfStats, Opts),
            PerfAfterIndex;
        {error, _} = Error ->
            ?event(
                copycat_short,
                {arweave_block_not_found,
                    {height, Current},
                    {target, To},
                    {reason, Error}} 
            ),
            PerfAfterBlock
    end.

%% @doc Index the IDs of all transactions in the block if configured to do so.
maybe_index_ids(Block, Perf, Opts) ->
    TotalTXs = length(hb_maps:get(<<"txs">>, Block, [], Opts)),
    case hb_opts:get(arweave_index_ids, false, Opts) of
        false -> Perf#perf{
            last_block_items_indexed = 0,
            last_block_total_txs = TotalTXs,
            last_block_bundle_txs = 0,
            last_block_skipped_txs = 0
        };
        true ->
            IndexStore = hb_opts:get(arweave_index_store, no_store, Opts),
            BlockEndOffset = hb_util:int(
                hb_maps:get(<<"weave_size">>, Block, 0, Opts)),
            BlockSize = hb_util:int(
                hb_maps:get(<<"block_size">>, Block, 0, Opts)),
            BlockStartOffset = BlockEndOffset - BlockSize,
            {TXs, SkippedFromHeaders, Perf1} = resolve_tx_headers(hb_maps:get(<<"txs">>, Block, [], Opts), Perf, Opts),
            Perf2 = Perf1,
            Height = hb_maps:get(<<"height">>, Block, 0, Opts),
            TXsWithData = ar_block:generate_size_tagged_list_from_txs(TXs, Height),
            ItemsBefore = Perf2#perf.items_indexed,
            {Perf3, BundleTXs, SkippedFromBundles} = lists:foldl(fun
                ({{padding, _PaddingRoot}, _EndOffset}, {PerfAcc, BundleAcc, SkippedAcc}) ->
                    {PerfAcc, BundleAcc, SkippedAcc};
                ({{TX, _TXDataRoot}, EndOffset}, {PerfAcc, BundleAcc, SkippedAcc}) ->
                    case is_bundle_tx(TX, Opts) of
                        false -> {PerfAcc, BundleAcc, SkippedAcc};
                        true ->
                            TXID = hb_util:encode(TX#tx.id),
                            TXEndOffset = BlockStartOffset + EndOffset,
                            TXStartOffset = TXEndOffset - TX#tx.data_size,
                            {ok, PerfAfterWrite} = perf_time(
                                fun() ->
                                    hb_store_arweave:write_offset(
                                        IndexStore,
                                        TXID,
                                        true,
                                        TXStartOffset,
                                        TX#tx.data_size
                                    )
                                end,
                                PerfAcc,
                                write_offset_time
                            ),
                            {BundleRes, Perf4} = download_bundle_header(
                                TXEndOffset, TX#tx.data_size, PerfAfterWrite, Opts
                            ),
                            case BundleRes of
                                {ok, {BundleIndex, HeaderSize}} ->
                                    {_, Perf5} = lists:foldl(
                                        fun({ItemID, Size}, {ItemStartOffset, PerfFold}) ->
                                            {ok, PerfUpdated} = perf_time(
                                                fun() ->
                                                    hb_store_arweave:write_offset(
                                                        IndexStore,
                                                        hb_util:encode(ItemID),
                                                        false,
                                                        ItemStartOffset,
                                                        Size
                                                    )
                                                end,
                                                PerfFold,
                                                write_offset_time
                                            ),
                                            {ItemStartOffset + Size, PerfUpdated}
                                        end,
                                        {TXStartOffset + HeaderSize, Perf4},
                                        BundleIndex
                                    ),
                                    Perf6 = Perf5#perf{items_indexed = Perf5#perf.items_indexed + length(BundleIndex)},
                                    {Perf6, BundleAcc + 1, SkippedAcc};
                                {error, Reason} ->
                                    ?event(
                                        copycat_short,
                                        {arweave_bundle_skipped,
                                            {tx_id, {explicit, TXID}},
                                            {reason, Reason}
                                        }
                                    ),
                                    {Perf4, BundleAcc + 1, SkippedAcc + 1}
                            end
                    end
                end,
                {Perf2, 0, 0},
                TXsWithData
            ),
            ItemsAfter = Perf3#perf.items_indexed,
            ItemsIndexed = ItemsAfter - ItemsBefore,
            SkippedTXs = SkippedFromHeaders + SkippedFromBundles,
            Perf3#perf{
                last_block_items_indexed = ItemsIndexed,
                last_block_total_txs = TotalTXs,
                last_block_bundle_txs = BundleTXs,
                last_block_skipped_txs = SkippedTXs
            }
    end.

is_bundle_tx(TX, _Opts) ->
    dev_arweave_common:type(TX) =/= binary.

download_bundle_header(EndOffset, Size, Perf, Opts) ->
    {Result, Perf1} = perf_time(
        fun() ->
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
            end
        end,
        Perf,
        download_bundle_header_time
    ),
    {Result, Perf1}.

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

resolve_tx_headers(TXIDs, Perf, Opts) ->
    lists:foldr(
        fun(TXID, {Acc, SkippedAcc, PerfAcc}) ->
            {Res, PerfUpdated} = resolve_tx_header(TXID, PerfAcc, Opts),
            case Res of
                {ok, TX} -> {[TX | Acc], SkippedAcc, PerfUpdated};
                skip -> {Acc, SkippedAcc + 1, PerfUpdated}
            end
        end,
        {[], 0, Perf},
        TXIDs
    ).

resolve_tx_header(TXID, Perf, Opts) ->
    try
        {ResolveRes, Perf1} = perf_time(
            fun() ->
                hb_ao:resolve(
                    <<
                        ?ARWEAVE_DEVICE/binary,
                        "/tx&tx=",
                        TXID/binary,
                        "&exclude-data=true"
                    >>,
                    Opts
                )
            end,
            Perf,
            resolve_tx_time
        ),
        case ResolveRes of
            {ok, StructuredTXHeader} ->
                {{ok,
                    hb_message:convert(
                        StructuredTXHeader,
                        <<"tx@1.0">>,
                        <<"structured@1.0">>,
                        Opts)}, Perf1};
            {error, ResolveError} ->
                ?event(
                    copycat_short,
                    {arweave_tx_skipped,
                        {tx_id, {explicit, TXID}},
                        {reason, ResolveError}
                    }
                ),
                {skip, Perf1}
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
            {skip, Perf}
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
    Perf = perf_init(),
    {{ok, {BundleIndex, _HeaderSize}}, _Perf1} =
        download_bundle_header(EndOffset, Size, Perf, Opts),
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
    Perf = perf_init(),
    {Res, _Perf1} = resolve_tx_header(<<"752P6t4cOjMabYHqzC6hyLhxyo4YKZLblg7va_J21YE">>, Perf, Opts),
    ?assertEqual(skip, Res),
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
