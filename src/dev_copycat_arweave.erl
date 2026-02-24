%%% @doc A `~copycat@1.0' engine that fetches block data from an Arweave node for
%%% replication. This engine works in _reverse_ chronological order by default,
%%% fetching blocks from the latest known block towards the Genesis block. The
%%% node avoids retrieving blocks that are already present in the cache using
%%% `~arweave@2.9''s built-in caching mechanism.
-module(dev_copycat_arweave).
-export([arweave/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_DEVICE, <<"~arweave@2.9">>).

% GET /~cron@1.0/once&cron-path=~copycat@1.0/arweave

%% @doc Fetch blocks from an Arweave node between a given range, or from the
%% latest known block towards the Genesis block. If no range is provided, we
%% fetch blocks from the latest known block towards the Genesis block.
arweave(_Base, Request, Opts) ->
    {From, To} = parse_range(Request, Opts),
    Mode = hb_maps:get(<<"mode">>, Request, <<"write">>, Opts),
    case Mode of
        <<"write">>  -> fetch_blocks(Request, From, To, write, Opts);
        <<"update">> -> fetch_blocks(Request, From, To, update, Opts);
        <<"list">>   -> list_index(From, To, Opts);
        _ ->
            {error, <<"Unsupported mode `", (hb_util:bin(Mode))/binary, "`. Supported modes are: write, update, list">>}
    end.

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

%% @doc Check if a transaction ID is indexed in the arweave index store.
is_tx_indexed(TXID, Opts) ->
    IndexStore = hb_opts:get(arweave_index_store, no_store, Opts),
    case IndexStore of
        no_store -> false;
        #{ <<"index-store">> := Store } ->
            case hb_store:read(Store, hb_store_arweave_offset:path(TXID)) of
                {ok, _} -> true;
                not_found -> false
            end
    end.

%% @doc List indexed blocks and transactions in the given range.
%% Returns JSON with block heights as keys, each containing indexed and not-indexed lists.
list_index(From, To, _Opts) when From < To ->
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => hb_json:encode(#{})
    }};
list_index(From, To, Opts) ->
    Result = list_index_blocks(From, To, Opts, #{}),
    JSON = hb_json:encode(Result),
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => JSON
    }}.

%% @doc Iterate through blocks and check index status for each transaction.
list_index_blocks(Current, To, _Opts, Acc) when Current < To ->
    Acc;
list_index_blocks(Current, To, Opts, Acc) ->
    case dev_arweave_block_cache:read(Current, Opts) of
        {ok, Block} ->
            TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
            {IndexedTXs, NotIndexedTXs} = classify_txs(TXIDs, Opts),
            BlockKey = hb_util:bin(Current),
            NewAcc = Acc#{
                BlockKey => #{
                    <<"indexed">> => IndexedTXs,
                    <<"not-indexed">> => NotIndexedTXs
                }
            },
            list_index_blocks(Current - 1, To, Opts, NewAcc);
        not_found ->
            % Block not in cache, skip it
            list_index_blocks(Current - 1, To, Opts, Acc)
    end.

%% @doc Classify transactions as indexed or not-indexed.
classify_txs(TXIDs, Opts) ->
    lists:foldl(
        fun(TXID, {IndexedAcc, NotIndexedAcc}) ->
            case is_tx_indexed(TXID, Opts) of
                true -> {[TXID | IndexedAcc], NotIndexedAcc};
                false -> {IndexedAcc, [TXID | NotIndexedAcc]}
            end
        end,
        {[], []},
        TXIDs
    ).

%% @doc Fetch blocks from an Arweave node between a given range. The `Mode'
%% parameter controls whether each block is processed unconditionally
%% (`write`) or only when it is not already fully indexed
%% (`update`).
fetch_blocks(Req, Current, To, _Mode, _Opts) when Current < To ->
    ?event(copycat_short,
        {arweave_block_indexing_completed,
            {reached_target, To},
            {initial_request, Req}
        }
    ),
    {ok, To};
fetch_blocks(Req, Current, To, Mode, Opts) ->
    case should_process_block(Mode, Current, Opts) of
        true ->
            observe_event(<<"block_indexed">>, fun() ->
                fetch_and_process_block(Current, To, Opts)
            end);
        false ->
            ?event(copycat_short,
                {arweave_block_already_indexed,
                    {height, Current},
                    {target, To}
                }
            )
    end,
    fetch_blocks(Req, Current - 1, To, Mode, Opts).

%% @doc Decide whether a block at the given height needs processing.
%% In `write' mode every block is processed. In `update' mode a block is
%% skipped when it already exists in the block cache and all of its TXs are
%% present in the arweave index store.
should_process_block(write, _Current, _Opts) -> true;
should_process_block(update, Current, Opts) ->
    not is_block_fully_indexed(Current, Opts).

%% @doc Check if a block at a given height is fully indexed. A block is fully
%% indexed when it exists in the block cache and every transaction ID listed in
%% its `txs` field has a corresponding entry in the arweave index store.
is_block_fully_indexed(Height, Opts) ->
    case dev_arweave_block_cache:read(Height, Opts) of
        {ok, Block} ->
            TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
            lists:all(
                fun(TXID) -> is_tx_indexed(TXID, Opts) end,
                TXIDs
            );
        not_found ->
            false
    end.

fetch_and_process_block(Current, To, Opts) ->
    ?event(copycat_debug, {fetching_block, Current}),
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
            ?event(copycat_debug, {{processing_block, Current},
                {indep_hash, hb_maps:get(<<"indep_hash">>, Block, <<>>)}}),
            case maybe_index_ids(Block, Opts) of
                {block_skipped, Results} ->
                    TotalTXs = maps:get(total_txs, Results, 0),
                    ?event(
                        copycat_short,
                        {arweave_block_skipped,
                            {height, Current},
                            {total_txs, TotalTXs},
                            {target, To}
                        }
                    );
                {block_cached, Results} ->
                    ItemsIndexed = maps:get(items_count, Results, 0),
                    TotalTXs = maps:get(total_txs, Results, 0),
                    BundleTXs = maps:get(bundle_count, Results, 0),
                    SkippedTXs = maps:get(skipped_count, Results, 0),
                    ?event(
                        copycat_short,
                        {arweave_block_indexed,
                            {height, Current},
                            {items_indexed, ItemsIndexed},
                            {total_txs, TotalTXs},
                            {bundle_txs, BundleTXs},
                            {skipped_txs, SkippedTXs},
                            {target, To}
                        }
                    )
            end;
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
        false -> 
            {block_skipped, #{
                items_count => 0,
                total_txs => TotalTXs,
                bundle_count => 0,
                skipped_count => 0
            }};
        true ->
            BlockEndOffset = hb_util:int(
                hb_maps:get(<<"weave_size">>, Block, 0, Opts)),
            BlockSize = hb_util:int(
                hb_maps:get(<<"block_size">>, Block, 0, Opts)),
            BlockStartOffset = BlockEndOffset - BlockSize,
            case resolve_tx_headers(hb_maps:get(<<"txs">>, Block, [], Opts), Opts) of
                error ->
                    % Skip entire block if any transaction errors
                    {block_skipped, #{
                        skipped_count => TotalTXs,
                        total_txs => TotalTXs
                    }};
                {ok, TXs} ->
                    Height = hb_maps:get(<<"height">>, Block, 0, Opts),
                    TXsWithData = ar_block:generate_size_tagged_list_from_txs(TXs, Height),
                    % Filter out padding entries before processing
                    ValidTXs = lists:filter(
                        fun({{padding, _}, _}) -> false; (_) -> true end,
                        TXsWithData
                    ),
                    TXResults = process_txs(ValidTXs, BlockStartOffset, Opts),
                    {block_cached, TXResults#{total_txs => TotalTXs}}
            end
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
    IndexStore = hb_opts:get(arweave_index_store, no_store, Opts),
    TXID = hb_util:encode(TX#tx.id),
    TXEndOffset = BlockStartOffset + EndOffset,
    TXStartOffset = TXEndOffset - TX#tx.data_size,
    ?event(copycat_debug, {writing_index,
        {id, {explicit, TXID}},
        {offset, TXStartOffset},
        {size, TX#tx.data_size}
    }),
    observe_event(<<"item_indexed">>, fun() ->
        hb_store_arweave:write_offset(
            IndexStore,
            TXID,
            <<"tx@1.0">>,
            TXStartOffset,
            TX#tx.data_size
        )
    end),
    case is_bundle_tx(TX, Opts) of
        false -> #{items_count => 0, bundle_count => 0, skipped_count => 0};
        true ->
            ?event(copycat_debug, {fetching_bundle_header, 
                {tx_id, {explicit, TXID}},
                {tx_end_offset, TXEndOffset},
                {tx_data_size, TX#tx.data_size}
            }),
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
                                    <<"ans104@1.0">>,
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
%% Returns a map with keys: items_count, bundle_count, skipped_count.
process_txs(ValidTXs, BlockStartOffset, Opts) ->
    Results = parallel_map(
        ValidTXs,
        fun(TXWithData) -> process_tx(TXWithData, BlockStartOffset, Opts) end,
        Opts
    ),
    lists:foldl(
        fun(Result, Acc) ->
            #{
                items_count => maps:get(items_count, Result, 0) + maps:get(items_count, Acc, 0),
                bundle_count => maps:get(bundle_count, Result, 0) + maps:get(bundle_count, Acc, 0),
                skipped_count => maps:get(skipped_count, Result, 0) + maps:get(skipped_count, Acc, 0)
            }
        end,
        #{items_count => 0, bundle_count => 0, skipped_count => 0},
        Results
    ).

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

header_chunk(invalid_bundle_header, _FirstChunk, _StartOffset, _Opts) ->
    {error, invalid_bundle_header};
header_chunk(HeaderSize, FirstChunk, _StartOffset, _Opts)
        when HeaderSize =< byte_size(FirstChunk) ->
    {ok, FirstChunk};
header_chunk(HeaderSize, FirstChunk, StartOffset, Opts) ->
    Res =
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/chunk&offset=",
                (hb_util:bin(StartOffset + byte_size(FirstChunk)))/binary,
                "&length=",
                (hb_util:bin(HeaderSize - byte_size(FirstChunk)))/binary
            >>,
            Opts
        ),
    case Res of
        {ok, OtherChunks} -> {ok, <<FirstChunk/binary, OtherChunks/binary>>};
        Other -> Other
    end.

resolve_tx_headers(TXIDs, Opts) ->
    Results = parallel_map(
        TXIDs,
        fun(TXID) -> resolve_tx_header(TXID, Opts) end,
        Opts
    ),
    case lists:any(fun(Res) -> Res =:= error end, Results) of
        true -> error;
        false ->
            TXs = lists:foldr(
                fun({ok, TX}, Acc) -> [TX | Acc] end,
                [],
                Results
            ),
            {ok, TXs}
    end.

resolve_tx_header(TXID, Opts) ->
    try
        ?event(copycat_debug, {fetching_tx, {explicit, TXID}}),
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
                error
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
            error
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
    assert_item_read(
        <<"ATi9pQF_eqb99UK84R5rq8lGfRGpilVQOYyth7rXxh8">>,
        Opts),
    assert_item_read(
        <<"4VSfUbhMVZQHW5VfVwQZOmC5fR3W21DZgFCyz8CA-cE">>,
        Opts),
    assert_item_read(
        <<"ZQRHZhktk6dAtX9BlhO1teOtVlGHoyaWP25kAlhxrM4">>,
        Opts),
    % The T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs can be deserialized so
    % we'll verify that some of its items were index and match the version
    % in the deserialized bundle.
    assert_bundle_read(
        <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
        [
            {<<"54K1ehEIKZxGSusgZzgbGYaHfllwWQ09-S9-eRUJg5Y">>, <<"1">>},
            {<<"MgatoEjlO_YtdbxFi9Q7Hxbs0YQVcChddhSS7FsdeIg">>, <<"19">>},
            {<<"z-oKJfhMq5qoVFrljEfiBKgumaJmCWVxNJaavR5aPE8">>, <<"26">>}
        ],
        Opts
    ),
    % Non-ans104 data transaction 
    assert_item_read(
        <<"bXEgFm4K2b5VD64skBNAlS3I__4qxlM3Sm4Z5IXj3h8">>,
        Opts),
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
        <<"c2ATDuTgwKCcHpAFZqSt13NC-tA4hdA7Aa2xBPuOzoE">>,
        [
            {<<"OBKr-7UrmjxFD-h-qP-XLuvCgtyuO_IDpBMgIytvusA">>, <<"1">>}
        ],
        Opts
    ),
   ok.

%% @doc Test a bundle header that fits in a single chunk.
small_bundle_header_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    TXID = <<"29TsnbqPQ_7rQ_r4KF5qRr995W1wBw_mTy6WEMy40aw">>,
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
    {ok, {BundleIndex, HeaderSize}} =
        download_bundle_header(EndOffset, Size, Opts),
    ?assertEqual(1704, length(BundleIndex)),
    ?assertEqual(109088, HeaderSize),
    ok.

%% @doc Test a bundle header that doesn't fit in a single chunk.
large_bundle_header_test() ->
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
    {ok, {BundleIndex, HeaderSize}} =
        download_bundle_header(EndOffset, Size, Opts),
    ?assertEqual(15000, length(BundleIndex)),
    ?assertEqual(960032, HeaderSize),
    ok.

invalid_bundle_header_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    TXID = <<"cGNURX2IUt98VKVIeXSfYe6eulNwPEqijaQfvatzd_o">>,
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
    ?assertEqual({error, invalid_bundle_header},
        download_bundle_header(EndOffset, Size, Opts)),
    ok.

invalid_bundle_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1307606,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
            Opts
        ),
    assert_bundle_read(
        <<"8S12ZqO6-_icGkeuH8mFq6x9q7OIoXOqFRGH5k-wshg">>,
        [
            {<<"gintz-t6q_kdeP_IBQVGnp9fgFzs-pPGGehXW-V7ZRk">>, <<"1">>}
        ],
        Opts
    ),
    % L1 TX with bundle tags, but data is not a valid bundle. The L1 TX
    % should still be indexed.
    assert_item_read(<<"cGNURX2IUt98VKVIeXSfYe6eulNwPEqijaQfvatzd_o">>, Opts),
    ok.

block_with_large_integer_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 633719,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
            Opts
        ),
    % This is bundle signed with a solana signature, so only the L1 TX can
    % actually be loaded.
    assert_item_read(<<"UXpcKTl6Mh34eTFSgny4NcIqoUjBcgYIcMqromcS6_Q">>, Opts),
    ok.

% ecdsa_no_data_test() ->
%     {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
%     {ok, 1827904} =
%         hb_ao:resolve(
%             <<"~copycat@1.0/arweave&from=1827904&to=1827904">>,
%             Opts
%         ),
%     assert_bundle_read(
%         Opts,
%         <<"VNhX_pSANk_8j0jZBR5bh_5jr-lkfbHDjtHd8FKqx7U">>,
%         [
%             {<<"3xDKhrCQcPuBtcm1ipZS5C9gAfFYClgHuHOHAXGfchM">>, <<"1">>},
%             {<<"JantC8f89VE-RidArHnU9589gY5T37NDXnWpI7H_psc">>, <<"7">>}
%         ]
%     ),
%     ok.

% ecdsa_with_data_test() ->
%     {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
%     Block = 1720431,
%     fetch_and_process_block(Block, Block, Opts),
%     {ok, Block} =
%         hb_ao:resolve(
%             <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
%             Opts
%         ),
%     ok.

%% @doc Disabled because the test takes ~30 seconds to run.
%% dev_arweave:get_tx_data_tag_exclude_data_test has some test coverage for
%% handling an L1 TX with a data tag. 
tx_with_data_tag_test_disabled() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    Block = 1289677,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
            Opts
        ),
    ?assertException(
        error,
        {badmatch, unsupported_tx_format},
        hb_store_arweave:read(
            StoreOpts,
            <<"ZwsFMXcwuakDuIhskokVHYiOPVcywDUAUTMLAJ72fgw">>)
    ),
    ?assertException(
        error,
        {badmatch, unsupported_tx_format},
        hb_store_arweave:read(
            StoreOpts,
            <<"-8ikoQo3KZkp9Hz_7kNdiUw3Vmn7J2DFslL_rBz0OBY">>)
    ),
    assert_bundle_read(
        <<"0vvttUgGqSsMul8RKIPvBjlwTU5_0x68sZr4uJxgNF8">>,
        [
            {<<"7U7GRZ8cXtKezSQmQmGpJar6haz-uink46i6evxzDCI">>, <<"1">>}
        ],
        Opts
    ),
    assert_item_read(<<"jI0A4BASHaUdCCsdv249BxDX6IlE0Ko391TuI6REATw">>, Opts),
    ok.

tx_with_no_data_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1826700,
    BlockBin = hb_util:bin(Block),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    % Value transfer
    Resolved = hb_ao:resolve(<<"XSQIgyDY1XUJNz79OeRHFaNpJZyaJSBd7XFsjWlZpNU">>, Opts),
    ?assertMatch({ok, _}, Resolved),
    {ok, StructuredTX} = Resolved,
    ?assert(hb_message:verify(StructuredTX, all, Opts)),
    ?assertEqual(
        <<"XSQIgyDY1XUJNz79OeRHFaNpJZyaJSBd7XFsjWlZpNU">>,
        hb_message:id(StructuredTX, signed, Opts)
    ),
    TX = hb_message:convert(
        StructuredTX,
        <<"tx@1.0">>,
        <<"structured@1.0">>,
        Opts),
    ?assertEqual(0, TX#tx.data_size),
    ?assertEqual(538493200840000, TX#tx.quantity),
    % TX with non-ans104 data
    assert_item_read(
        <<"bpd0CzsoTr9-X83sPCx08uNzZC_EgFwb-P8lnHXSeRo">>,
        Opts),
    %% Now list the index using list mode
    {ok, Response} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=list"
            >>,
            Opts
        ),
    JSONBody = maps:get(<<"body">>, Response),
    IndexData = hb_json:decode(JSONBody),
    BlockInfo = maps:get(BlockBin, IndexData),
    %% Verify indexed and not-indexed keys exist
    ?assert(maps:is_key(<<"indexed">>, BlockInfo)),
    ?assert(maps:is_key(<<"not-indexed">>, BlockInfo)),
    ?assertEqual([
            <<"XSQIgyDY1XUJNz79OeRHFaNpJZyaJSBd7XFsjWlZpNU">>,
            <<"bpd0CzsoTr9-X83sPCx08uNzZC_EgFwb-P8lnHXSeRo">>,
            <<"n5rT8Y9Jet7SCnl_M77UrPNUFeud5iKazsn9Sr9gsWA">>,
            <<"hvZlThf1B1tY4wMm4cETSsk8vIkOY3QZRmaBnQSzlVo">>,
            <<"3urwRfVyWN35HE5RHGwOUk6CxkJ_lZOaMY7HZbeJyRs">>
        ], maps:get(<<"indexed">>, BlockInfo)),
    ?assertEqual([ ], maps:get(<<"not-indexed">>, BlockInfo)),
    ok.

non_string_tags_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Res = resolve_tx_header(<<"752P6t4cOjMabYHqzC6hyLhxyo4YKZLblg7va_J21YE">>, Opts),
    ?assertEqual(error, Res),
    ok.

list_index_test() ->
    %% Test block: https://viewblock.io/arweave/block/1827942
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    %% First index the block using write mode
    Block = 1827942,
    BlockBin = hb_util:bin(Block),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    %% Now list the index using list mode
    {ok, Response} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=list"
            >>,
            Opts
        ),
    %% Verify content-type is application/json
    ?assertEqual(<<"application/json">>, maps:get(<<"content-type">>, Response)),
    ?event(debug_test, {response, Response}),
    %% Decode the JSON body
    JSONBody = maps:get(<<"body">>, Response),
    IndexData = hb_json:decode(JSONBody),
    %% Verify the block height is present as a key
    ?assert(maps:is_key(BlockBin, IndexData)),
    BlockInfo = maps:get(BlockBin, IndexData),
    %% Verify indexed and not-indexed keys exist
    ?assert(maps:is_key(<<"indexed">>, BlockInfo)),
    ?assert(maps:is_key(<<"not-indexed">>, BlockInfo)),
    ?assertEqual([
            <<"c2ATDuTgwKCcHpAFZqSt13NC-tA4hdA7Aa2xBPuOzoE">>,
            <<"kK67S13W_8jM9JUw2umVamo0zh9v1DeVxWrru2evNco">>,
            <<"bXEgFm4K2b5VD64skBNAlS3I__4qxlM3Sm4Z5IXj3h8">>,
            <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
            <<"WbRAQbeyjPHgopBKyi0PLeKWvYZr3rgZvQ7QY3ASJS4">>
        ], maps:get(<<"indexed">>, BlockInfo)),
    ?assertEqual([ ], maps:get(<<"not-indexed">>, BlockInfo)),
    ok.

%% @doc Test `mode=update` with three blocks representing three scenarios:
%%   - Block A (1826702): Fully indexed -- should be skipped by update.
%%   - Block B (1826701): Missing entirely (never fetched/cached) -- should
%%     be fetched and indexed.
%%   - Block C (1826700): Block header is cached but at least one TX is not
%%     indexed -- should be re-fetched and fully reindexed.
update_mode_test() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    BlockA = 1826702,
    BlockB = 1826701,
    BlockC = 1826700,
    %% 1. Fully index Block A using mode=write.
    {ok, BlockA} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(BlockA))/binary, "&"
                "to=", (hb_util:bin(BlockA))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    %% 2. Block B: intentionally left un-fetched/un-cached (nothing to do).
    %% 3. Block C: cache the block header but do NOT index TXs.
    %%    We set arweave_index_ids => false so the arweave device caches the
    %%    block but maybe_index_ids/2 skips TX indexing.
    NoIndexOpts = Opts#{ arweave_index_ids => false },
    {ok, BlockC} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(BlockC))/binary, "&"
                "to=", (hb_util:bin(BlockC))/binary, "&"
                "mode=write"
            >>,
            NoIndexOpts
        ),
    %%    Now manually index all but the first TX with dummy offsets so that
    %%    the block appears partially indexed (at least 1 TX is missing).
    {ok, BlockCData} = dev_arweave_block_cache:read(BlockC, Opts),
    TXIDs = hb_maps:get(<<"txs">>, BlockCData, [], Opts),
    ?assert(length(TXIDs) > 1),
    [_SkippedTXID | RestTXIDs] = TXIDs,
    lists:foreach(
        fun(TXID) ->
            hb_store_arweave:write_offset(StoreOpts, TXID, <<"tx@1.0">>, 0, 0)
        end,
        RestTXIDs
    ),
    ?assert(is_block_fully_indexed(BlockA, Opts)),
    ?assertNot(is_block_fully_indexed(BlockB, Opts)),
    ?assertNot(is_block_fully_indexed(BlockC, Opts)),
    %% --- Run mode=update over the full range ---
    {ok, BlockC} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(BlockA))/binary, "&"
                "to=", (hb_util:bin(BlockC))/binary, "&"
                "mode=update"
            >>,
            Opts
        ),
    %% All three blocks should now be fully indexed.
    ?assert(is_block_fully_indexed(BlockA, Opts)),
    ?assert(is_block_fully_indexed(BlockB, Opts)),
    ?assert(is_block_fully_indexed(BlockC, Opts)),
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

assert_bundle_read(BundleID, ExpectedItems, Opts) ->
    ReadItems =
        lists:map(
            fun({ItemID, _Index}) ->
                assert_item_read(ItemID, Opts)
            end,
            ExpectedItems
        ),
    Bundle = assert_item_read(BundleID, Opts),
    lists:foreach(
        fun({{_ItemID, Index}, Item}) ->
            QueriedItem = hb_ao:get(Index, Bundle, Opts),
            ?assertEqual(hb_maps:without(?AO_CORE_KEYS, Item), hb_maps:without(?AO_CORE_KEYS, QueriedItem))
        end,
        lists:zip(ExpectedItems, ReadItems)
    ),
    ok.

assert_item_read(ItemID, Opts) ->
    ?event(debug_test, {resolving, {explicit, ItemID}}),
    Resolved = hb_ao:resolve(ItemID, Opts),
    ?assertMatch({ok, _}, Resolved, ItemID),
    {ok, Item} = Resolved,
    ?event(debug_test, {item, Item}),
    ?assert(hb_message:verify(Item, all, Opts)),
    ?assertEqual(ItemID, hb_message:id(Item, signed)),
    Item.
