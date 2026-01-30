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
    observe_event(<<"block_indexed">>, fun() ->
        fetch_and_process_block(Current, To, Opts)
    end),
    fetch_blocks(Req, Current - 1, To, Opts).

fetch_and_process_block(Current, To, Opts) ->
    BlockRes = observe_event(<<"block_header">>, fun() ->
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/block=",
                (hb_util:bin(Current))/binary
            >>,
            Opts
        )
    end),
    process_block(BlockRes, Current, To, Opts).

%% @doc Process a block.
process_block(BlockRes, Current, To, Opts) ->
    case BlockRes of
        {ok, Block} ->
            {ItemsIndexed, TotalTXs, BundleTXs, SkippedTXs} = maybe_index_ids(Block, Opts),
            ?event(
                copycat_short,
                {arweave_block_cached,
                    {height, Current},
                    {items_indexed, ItemsIndexed},
                    {total_txs, TotalTXs},
                    {bundle_txs, BundleTXs},
                    {skipped_txs, SkippedTXs},
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
    TotalTXs = length(hb_maps:get(<<"txs">>, Block, [], Opts)),
    case hb_opts:get(arweave_index_ids, false, Opts) of
        false -> {0, TotalTXs, 0, 0};
        true ->
            BlockEndOffset = hb_util:int(
                hb_maps:get(<<"weave_size">>, Block, 0, Opts)),
            BlockSize = hb_util:int(
                hb_maps:get(<<"block_size">>, Block, 0, Opts)),
            BlockStartOffset = BlockEndOffset - BlockSize,
            {TXs, SkippedFromHeaders} = resolve_tx_headers(hb_maps:get(<<"txs">>, Block, [], Opts), Opts),
            Height = hb_maps:get(<<"height">>, Block, 0, Opts),
            TXsWithData = ar_block:generate_size_tagged_list_from_txs(TXs, Height),
            % Filter out padding entries before processing
            ValidTXs = lists:filter(
                fun({{padding, _}, _}) -> false; (_) -> true end,
                TXsWithData
            ),
            {ItemsIndexed, BundleTXs, SkippedFromBundles} = 
                process_txs(ValidTXs, BlockStartOffset, Opts),
            SkippedTXs = SkippedFromHeaders + SkippedFromBundles,
            {ItemsIndexed, TotalTXs, BundleTXs, SkippedTXs}
    end.

%% @doc Apply Fun to each item in Items with parallel workers.
%% Fun takes an item and returns a result.
%% Returns a list of results in the same order as the input items.
%% Uses arweave_index_workers from Opts to determine max concurrency (default 1 = sequential).
parallel_map(Items, Fun, Opts) ->
    MaxWorkers = max(1, hb_opts:get(arweave_index_workers, 1, Opts)),
    Parent = self(),
    ItemsWithRefs = [{Item, make_ref()} || Item <- Items],
    % Spawn initial batch up to MaxWorkers
    {ToSpawn, Remaining} = lists:split(min(length(ItemsWithRefs), MaxWorkers), ItemsWithRefs),
    ActiveRefs = [spawn_worker(ItemWithRef, Fun, Parent) || ItemWithRef <- ToSpawn],
    % Wait for workers to complete and refill pool, collecting results
    ResultsMap = parallel_map_wait(ActiveRefs, Remaining, Fun, MaxWorkers, Parent, #{}),
    % Return results in order by matching refs (inspired by pmap pattern)
    [maps:get(Ref, ResultsMap) || {_Item, Ref} <- ItemsWithRefs].

%% @doc Spawn a worker process for a single item.
spawn_worker({Item, Ref}, Fun, Parent) ->
    spawn(fun() ->
        Result = Fun(Item),
        Parent ! {pmap_work, Ref, Result}
    end),
    Ref.

%% @doc Wait for workers to complete and refill the pool as slots become available.
parallel_map_wait([], [], _Fun, _MaxWorkers, _Parent, ResultsMap) ->
    ResultsMap;
parallel_map_wait(ActiveRefs, Remaining, Fun, MaxWorkers, Parent, ResultsMap) ->
    receive
        {pmap_work, CompletedRef, Result} ->
            % Store result and remove completed ref
            NewResultsMap = ResultsMap#{CompletedRef => Result},
            NewActiveRefs = lists:delete(CompletedRef, ActiveRefs),
            case Remaining of
                [] ->
                    % No more items, just wait for remaining workers
                    parallel_map_wait(NewActiveRefs, [], Fun, MaxWorkers, Parent, NewResultsMap);
                _ ->
                    % Spawn replacement worker
                    [NextItemWithRef | NewRemaining] = Remaining,
                    NextRef = spawn_worker(NextItemWithRef, Fun, Parent),
                    parallel_map_wait([NextRef | NewActiveRefs], NewRemaining, Fun, MaxWorkers, Parent, NewResultsMap)
            end
    end.

%% @doc Process a single transaction and return its contribution to the counters.
%% Returns a map with keys: items_count, bundle_count, skipped_count
process_tx({{padding, _PaddingRoot}, _EndOffset}, _BlockStartOffset, _Opts) ->
    #{items_count => 0, bundle_count => 0, skipped_count => 0};
process_tx({{TX, _TXDataRoot}, EndOffset}, BlockStartOffset, Opts) ->
    case is_bundle_tx(TX, Opts) of
        false -> #{items_count => 0, bundle_count => 0, skipped_count => 0};
        true ->
            IndexStore = hb_opts:get(arweave_index_store, no_store, Opts),
            TXID = hb_util:encode(TX#tx.id),
            TXEndOffset = BlockStartOffset + EndOffset,
            TXStartOffset = TXEndOffset - TX#tx.data_size,
            observe_event(<<"item_indexed">>, fun() ->
                hb_store_arweave:write_offset(
                    IndexStore,
                    TXID,
                    true,
                    TXStartOffset,
                    TX#tx.data_size
                )
            end),
            BundleRes = download_bundle_header(
                TXEndOffset, TX#tx.data_size, Opts
            ),
            case BundleRes of
                {ok, {BundleIndex, HeaderSize}} ->
                    % Batch event tracking: measure total time and count for all write_offset calls
                    {TotalTime, {_, ItemsCount}} = timer:tc(fun() ->
                        lists:foldl(
                            fun({ItemID, Size}, {ItemStartOffset, ItemsCountAcc}) ->
                                hb_store_arweave:write_offset(
                                    IndexStore,
                                    hb_util:encode(ItemID),
                                    false,
                                    ItemStartOffset,
                                    Size
                                ),
                                {ItemStartOffset + Size, ItemsCountAcc + 1}
                            end,
                            {TXStartOffset + HeaderSize, 0},
                            BundleIndex
                        )
                    end),
                    % Single event increment for the batch
                    record_event_metrics(<<"item_indexed">>, ItemsCount, TotalTime),
                    #{items_count => ItemsCount, bundle_count => 1, skipped_count => 0};
                {error, Reason} ->
                    ?event(
                        copycat_short,
                        {arweave_bundle_skipped,
                            {tx_id, {explicit, TXID}},
                            {reason, Reason}
                        }
                    ),
                    #{items_count => 0, bundle_count => 1, skipped_count => 1}
            end
    end.

%% @doc Process transactions: spawn workers and manage the worker pool.
%% This function processes transactions in parallel using parallel_map.
%% When arweave_index_workers <= 1, processes sequentially (one worker at a time).
%% When arweave_index_workers > 1, processes in parallel with the specified concurrency limit.
%% Returns {ItemsIndexed, BundleTXs, SkippedTXs}.
process_txs(ValidTXs, BlockStartOffset, Opts) ->
    Results = parallel_map(
        ValidTXs,
        fun(TXWithData) -> process_tx(TXWithData, BlockStartOffset, Opts) end,
        Opts
    ),
    Aggregated = lists:foldl(
        fun(Result, Acc) ->
            #{
                items_count => maps:get(items_count, Result, 0) + maps:get(items_count, Acc, 0),
                bundle_count => maps:get(bundle_count, Result, 0) + maps:get(bundle_count, Acc, 0),
                skipped_count => maps:get(skipped_count, Result, 0) + maps:get(skipped_count, Acc, 0)
            }
        end,
        #{items_count => 0, bundle_count => 0, skipped_count => 0},
        Results
    ),
    {
        maps:get(items_count, Aggregated, 0),
        maps:get(bundle_count, Aggregated, 0),
        maps:get(skipped_count, Aggregated, 0)
    }.

is_bundle_tx(TX, _Opts) ->
    dev_arweave_common:type(TX) =/= binary.

download_bundle_header(EndOffset, Size, Opts) ->
    observe_event(<<"bundle_header">>, fun() ->
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
    end).

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
    Results = parallel_map(
        TXIDs,
        fun(TXID) -> resolve_tx_header(TXID, Opts) end,
        Opts
    ),
    lists:foldr(
        fun(Res, {Acc, SkippedAcc}) ->
            case Res of
                {ok, TX} -> {[TX | Acc], SkippedAcc};
                skip -> {Acc, SkippedAcc + 1}
            end
        end,
        {[], 0},
        Results
    ).

resolve_tx_header(TXID, Opts) ->
    try
        ResolveRes = observe_event(<<"tx_header">>, fun() ->
            hb_ao:resolve(
                <<
                    ?ARWEAVE_DEVICE/binary,
                    "/tx&tx=",
                    TXID/binary,
                    "&exclude-data=true"
                >>,
                Opts
            )
        end),
        case ResolveRes of
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

%% @doc Record event metrics (count and duration) using hb_event:increment.
record_event_metrics(MetricName, Count, Duration) ->
    hb_event:increment(<<"arweave_block_count">>, MetricName, #{}, Count),
    hb_event:increment(<<"arweave_block_duration">>, MetricName, #{}, Duration).

%% @doc Track an operation's execution time and count using hb_event:increment.
%% Always tracks both count and duration, regardless of success/failure.
observe_event(MetricName, Fun) ->
    {Time, Result} = timer:tc(Fun),
    record_event_metrics(MetricName, 1, Time),
    Result.

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
    Res = resolve_tx_header(<<"752P6t4cOjMabYHqzC6hyLhxyo4YKZLblg7va_J21YE">>, Opts),
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
