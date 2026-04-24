%%% @doc A `~copycat@1.0' engine that fetches block data from an Arweave node for
%%% replication. This engine works in _reverse_ chronological order by default.
%%% If `to' is omitted, it keeps moving downward from `from' until it reaches a
%%% block where at least one TX is already indexed, then stops. If `to' is
%%% provided, every block in the range is processed.
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
    case parse_range(Request, Opts) of
        {error, unavailable} ->
            {error, unavailable};
        {ok, {From, To}} ->
            case hb_maps:get(<<"mode">>, Request, <<"write">>, Opts) of
                <<"write">> -> fetch_blocks(Request, From, To, Opts);
                <<"list">> -> list_index(From, To, Opts);
                Mode ->
                    {error, <<"Unsupported mode `", (hb_util:bin(Mode))/binary, "`. Supported modes are: write, list">>}
            end
    end.

%% @doc Parse the range from the request.
parse_range(Request, Opts) ->
    maybe
        {ok, From} ?=
            case hb_maps:find(<<"from">>, Request, Opts) of
                {ok, FromHeight} -> normalize_height(FromHeight, Opts);
                error -> latest_height(Opts)
            end,
        {ok, To} ?=
            case hb_maps:find(<<"to">>, Request, Opts) of
                {ok, ToHeight} -> normalize_height(ToHeight, Opts);
                error -> {ok, undefined}
            end,
        case From < 0 orelse (is_integer(To) andalso To < 0) of
            true ->
                ?event(copycat_short,
                    {height_resolved_negative,
                        {from, From}, {to, To}}),
                {error, unavailable};
            false ->
                {ok, {From, To}}
        end
    else
        {error, Reason} ->
            ?event(copycat_short,
                {latest_height_failed, {reason, Reason}}),
            {error, unavailable}
    end.

normalize_height(Height, Opts) ->
    RequestedHeight = hb_util:int(Height),
    case RequestedHeight < 0 of
        true ->
            case latest_height(Opts) of
                {ok, Tip} -> {ok, Tip + RequestedHeight};
                {error, _} = Err -> Err
            end;
        false ->
            {ok, RequestedHeight}
    end.

latest_height(Opts) ->
    case hb_ao:resolve(
        <<?ARWEAVE_DEVICE/binary, "/current/height">>,
        Opts
    ) of
        {ok, ResolvedHeight} -> {ok, hb_util:int(ResolvedHeight)};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Check if a transaction ID is indexed in the arweave index store.
is_tx_indexed(TXID, Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store -> false;
        #{ <<"index-store">> := Store } ->
            case hb_store:read(Store, hb_store_arweave_offset:path(TXID), Opts) of
                {ok, _} -> true;
                {error, not_found} -> false
            end
    end.

%% @doc List indexed blocks and transactions in the given range.
%% Returns JSON with block heights as keys, each containing indexed and not-indexed lists.
list_index(From, undefined, Opts) ->
    list_index(From, 0, Opts);
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
    case fetch_block_header(Current, Opts) of
        {ok, Block} ->
            TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
            case TXIDs of
                [] ->
                    list_index_blocks(Current - 1, To, Opts, Acc);
                _ ->
                    {IndexedTXs, NotIndexedTXs} = classify_txs(TXIDs, Opts),
                    case IndexedTXs of
                        [] ->
                            % Do not include blocks with no locally indexed TXs.
                            list_index_blocks(Current - 1, To, Opts, Acc);
                        _ ->
                            BlockKey = hb_util:bin(Current),
                            NewAcc = Acc#{
                                BlockKey => #{
                                    <<"indexed">> => IndexedTXs,
                                    <<"not-indexed">> => NotIndexedTXs
                                }
                            },
                            list_index_blocks(Current - 1, To, Opts, NewAcc)
                    end
            end;
        {error, _} ->
            list_index_blocks(Current - 1, To, Opts, Acc)
    end.

fetch_block_header(Height, Opts) ->
    ?event(debug_copycat, {fetching_block, Height}),
    observe_event(<<"block_header">>, fun() ->
        hb_ao:resolve(
            <<
                ?ARWEAVE_DEVICE/binary,
                "/block=",
                (hb_util:bin(Height))/binary
            >>,
            Opts
        )
    end).

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

%% @doc Fetch blocks from an Arweave node while moving downward from `Current'.
%% If `To' is provided, every block in [`To', `Current'] is processed. If `To'
%% is omitted, stop at the first block where any TX is already indexed.
fetch_blocks(Req, Current, To, _Opts) when is_integer(To), Current < To ->
    ?event(copycat_short,
        {arweave_block_indexing_completed,
            {reached_target, To},
            {initial_request, Req}
        }
    ),
    {ok, To};
fetch_blocks(_Req, Current, undefined, _Opts) when Current < 0 ->
    {ok, 0};
fetch_blocks(Req, Current, undefined, Opts) ->
    BlockRes = fetch_block_header(Current, Opts),
    case is_already_indexed(BlockRes, Opts) of
        true ->
            ?event(copycat_short,
                {arweave_block_indexing_completed,
                    {stop_at_indexed_block, Current},
                    {initial_request, Req}
                }
            ),
            {ok, Current};
        false ->
            observe_event(<<"block_indexed">>, fun() ->
                process_block(BlockRes, Current, undefined, Opts)
            end),
            fetch_blocks(Req, Current - 1, undefined, Opts)
    end;
fetch_blocks(Req, Current, To, Opts) ->
    observe_event(<<"block_indexed">>, fun() ->
        fetch_and_process_block(Current, To, Opts)
    end),
    fetch_blocks(Req, Current - 1, To, Opts).

%% @doc Determine whether a fetched block is considered indexed.
%% A block is indexed when any TX from its `txs' list is in the index.
is_already_indexed({ok, Block}, Opts) ->
    TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
    lists:any(fun(TXID) -> is_tx_indexed(TXID, Opts) end, TXIDs);
is_already_indexed({error, _}, _Opts) ->
    false.

fetch_and_process_block(Current, To, Opts) ->
    BlockRes = fetch_block_header(Current, Opts),
    process_block(BlockRes, Current, To, Opts).

%% @doc Process a block.
process_block(BlockRes, Current, To, Opts) ->
    case BlockRes of
        {ok, Block} ->
            ?event(debug_copycat, {{processing_block, Current},
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
    case hb_opts:get(arweave_index_ids, true, Opts) of
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
    hb_pmap:parallel_map(Items, Fun, MaxWorkers).

%% @doc Process a single transaction and return its contribution to the counters.
%% Returns a map with keys: items_count, bundle_count, skipped_count
process_tx({{padding, _PaddingRoot}, _EndOffset}, _BlockStartOffset, _Opts) ->
    #{items_count => 0, bundle_count => 0, skipped_count => 0};
process_tx({{TX, _TXDataRoot}, EndOffset}, BlockStartOffset, Opts) ->
    IndexStore = hb_store_arweave:store_from_opts(Opts),
    TXID = hb_util:encode(TX#tx.id),
    TXEndOffset = BlockStartOffset + EndOffset,
    TXStartOffset = TXEndOffset - TX#tx.data_size,
    ?event(debug_copycat, {writing_index,
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
            % Lightweight processing of block transactions to depth 2. We
            % can avoid loading the full L1 TX data into memory, and instead
            % only load the bundle header. But as a result we're unable to
            % recurse any deeper than L2 dataitems.
            ?event(debug_copycat, {fetching_bundle_header, 
                {tx_id, {string, TXID}},
                {tx_end_offset, TXEndOffset},
                {tx_data_size, TX#tx.data_size}
            }),
            BundleRes = download_bundle_header(
                TXEndOffset, TX#tx.data_size, Opts
            ),
            case BundleRes of
                {ok, HeaderSize, BundleIndex} ->
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
                    ?event(debug_copycat,
                        {bundle_items_indexed,
                            {tx_id, {string, TXID}},
                            {items_count, ItemsCount}
                        }),
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

%% @doc Check whether a TX header indicates bundle content.
is_bundle_tx(TX, _Opts) ->
    dev_arweave_common:type(TX) =/= binary.

%% @doc Download and decode a bundle header from chunk data.
download_bundle_header(EndOffset, Size, Opts) ->
    observe_event(<<"bundle_header">>, fun() ->
        dev_arweave:bundle_header(EndOffset - Size, Size, Opts)
    end).

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
        ?event(debug_copycat, {fetching_tx, {explicit, TXID}}),
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

index_ids_test_parallel() ->
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
    ?assertMatch(
        {ok, _},
        hb_store_arweave:read(
            StoreOpts,
            #{ <<"read">> => <<"WbRAQbeyjPHgopBKyi0PLeKWvYZr3rgZvQ7QY3ASJS4">> },
            Opts
        )
    ),
    assert_item_read(
        <<"0vy2Ey8bWkSDcRIvWQJjxDeVGYOrTSmYIIhBILJntY8">>,
        Opts),
    assert_item_read(
        <<"2lmrYydmDweX2MgGH39ZEB9hKm2JqGOYmRiG3n_xh8A">>,
        Opts),
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
    % Another bundle with an unsupported avro codec should be indexed even if
    % it can't be deserialized.
    ?assertException(
        error,
        {badmatch,<<"\"address\":\"0x124e64C9Ed898d4A8B130F6ACb76b33E21CD711c\"", _/binary>>},
        hb_store_arweave:read(
            StoreOpts,
            #{ <<"read">> => <<"kK67S13W_8jM9JUw2umVamo0zh9v1DeVxWrru2evNco">> },
            Opts)
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
small_bundle_header_test_parallel() ->
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
    {ok, HeaderSize, BundleIndex} =
        download_bundle_header(EndOffset, Size, Opts),
    ?assertEqual(1704, length(BundleIndex)),
    ?assertEqual(109088, HeaderSize),
    ok.

%% @doc Test a bundle header that doesn't fit in a single chunk.
large_bundle_header_test_parallel() ->
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
    {ok, HeaderSize, BundleIndex} =
        download_bundle_header(EndOffset, Size, Opts),
    ?assertEqual(15000, length(BundleIndex)),
    ?assertEqual(960032, HeaderSize),
    ok.

invalid_bundle_header_test_parallel() ->
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

invalid_bundle_test_parallel() ->
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

block_with_large_integer_test_parallel() ->
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

empty_block_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1865858,
    {ok, Block} =
        hb_ao:resolve(
            <<"~copycat@1.0/arweave&from=", (hb_util:bin(Block))/binary, "&to=", (hb_util:bin(Block))/binary>>,
            Opts
        ),
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
            #{ <<"read">> => <<"ZwsFMXcwuakDuIhskokVHYiOPVcywDUAUTMLAJ72fgw">> },
            Opts)
    ),
    ?assertException(
        error,
        {badmatch, unsupported_tx_format},
        hb_store_arweave:read(
            StoreOpts,
            #{ <<"read">> => <<"-8ikoQo3KZkp9Hz_7kNdiUw3Vmn7J2DFslL_rBz0OBY">> },
            Opts)
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

tx_with_no_data_test_parallel() ->
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

non_string_tags_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Res = resolve_tx_header(<<"752P6t4cOjMabYHqzC6hyLhxyo4YKZLblg7va_J21YE">>, Opts),
    ?assertEqual(error, Res),
    ok.

list_index_test_parallel() ->
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

auto_stop_on_indexed_block_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    IndexedBlock = 1827941,
    Higher1 = IndexedBlock + 1,
    Higher2 = IndexedBlock + 2,
    {ok, IndexedBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(IndexedBlock))/binary, "&"
                "to=", (hb_util:bin(IndexedBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    {ok, IndexedBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(Higher2))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(Higher2, Opts)),
    ?assert(has_any_indexed_tx(Higher1, Opts)),
    ?assert(has_any_indexed_tx(IndexedBlock, Opts)),
    ?assertNot(has_any_indexed_tx(IndexedBlock-1, Opts)),
    ok.

explicit_to_reindexes_all_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    IndexedBlock = 1827942,
    LowerBlock = IndexedBlock - 1,
    {ok, IndexedBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(IndexedBlock))/binary, "&"
                "to=", (hb_util:bin(IndexedBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assertNot(has_any_indexed_tx(LowerBlock, Opts)),
    {ok, LowerBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(IndexedBlock+1))/binary, "&"
                "to=", (hb_util:bin(LowerBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(LowerBlock, Opts)),
    ok.

%% @doc Manually write to the index to simulate a partially indexed block.
%% This should also trigger a stop when the `to` option is omitted.
auto_stop_partial_index_test_parallel() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    Block = 1826700,
    HigherBlock = Block + 1,
    NoIndexOpts = Opts#{
        <<"arweave-index-ids">> => false,
        <<"arweave-index-blocks">> => true
    },
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(Block))/binary, "&"
                "to=", (hb_util:bin(Block))/binary, "&"
                "mode=write"
            >>,
            NoIndexOpts
        ),
    {ok, BlockData} = dev_arweave_block_cache:read(Block, Opts),
    TXIDs = hb_maps:get(<<"txs">>, BlockData, [], Opts),
    ?assert(length(TXIDs) > 0),
    [OneTXID | _] = TXIDs,
    hb_store_arweave:write_offset(StoreOpts, OneTXID, <<"tx@1.0">>, 0, 0),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(HigherBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(HigherBlock, Opts)),
    ?assert(has_any_indexed_tx(Block, Opts)),
    ?assertNot(has_any_indexed_tx(Block-1, Opts)),
    ok.

negative_parse_range_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, Tip} =
        hb_ao:resolve(
            <<?ARWEAVE_DEVICE/binary, "/current/height">>,
            Opts
        ),
    {ok, {NegativeFrom, UndefinedTo}} =
        parse_range(#{ <<"from">> => <<"-3">> }, Opts),
    ?assertEqual(hb_util:int(Tip) - 3, NegativeFrom),
    ?assertEqual(undefined, UndefinedTo),
    {ok, {PositiveFrom, NegativeTo}} =
        parse_range(#{ <<"from">> => <<"10">>, <<"to">> => <<"-3">> }, Opts),
    ?assertEqual(10, PositiveFrom),
    ?assertEqual(hb_util:int(Tip) - 3, NegativeTo),
    ok.

latest_height_failure_test_parallel() ->
    {ok, MockURL, MockHandle} = hb_mock_server:start([
        {"/block/current", block_current, {500, <<"Internal Server Error">>}}
    ]),
    TestStore = hb_test_utils:test_store(),
    Opts = #{
        <<"store">> => [TestStore],
        <<"routes">> => [
            #{
                <<"template">> => <<"^/arweave">>,
                <<"nodes">> => [
                    #{
                        <<"match">> => <<"^/arweave">>,
                        <<"with">> => MockURL,
                        <<"opts">> => #{ <<"http-client">> => httpc }
                    }
                ],
                <<"parallel">> => true,
                <<"stop-after">> => true,
                <<"admissible-status">> => 200
            }
        ]
    },
    try
        ?assertMatch(
            {error, unavailable},
            parse_range(#{}, Opts)
        ),
        ?assertMatch(
            {error, unavailable},
            hb_ao:resolve(
                <<"~copycat@1.0/arweave&mode=write">>, Opts)
        )
    after
        hb_mock_server:stop(MockHandle)
    end.

negative_resolved_height_test_parallel() ->
    {ok, MockURL, MockHandle} = hb_mock_server:start([
        {"/block/current", block_current,
            {200, <<"{\"height\": 5}">>}}
    ]),
    TestStore = hb_test_utils:test_store(),
    Opts = #{
        <<"store">> => [TestStore],
        <<"arweave-index-blocks">> => false,
        <<"routes">> => [
            #{
                <<"template">> => <<"^/arweave">>,
                <<"nodes">> => [
                    #{
                        <<"match">> => <<"^/arweave">>,
                        <<"with">> => MockURL,
                        <<"opts">> => #{ <<"http-client">> => httpc }
                    }
                ],
                <<"parallel">> => true,
                <<"stop-after">> => true,
                <<"admissible-status">> => 200
            }
        ]
    },
    try
        ?assertMatch(
            {error, unavailable},
            parse_range(#{ <<"from">> => <<"-10">> }, Opts)
        )
    after
        hb_mock_server:stop(MockHandle)
    end.

negative_from_index_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, Tip} = latest_height(Opts),
    StopBlock = 1827942,
    StartBlock = 1827943,
    OffsetFromTip = Tip - StartBlock,
    ?assert(OffsetFromTip > 0),
    NegativeFrom = <<"-", (hb_util:bin(OffsetFromTip))/binary>>,
    {ok, StopBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(StopBlock))/binary, "&"
                "to=", (hb_util:bin(StopBlock))/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    {ok, StopBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", NegativeFrom/binary, "&"
                "mode=write"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(StartBlock, Opts)),
    NextBlock = highest_contiguous_indexed_block(StopBlock, 50, Opts),
    ?assertEqual(StartBlock, NextBlock),
    assert_indexed_range(NextBlock, StopBlock, Opts),
    ?assertNot(has_any_indexed_tx(StopBlock - 1, Opts)),
    ?assertNot(has_any_indexed_tx(NextBlock + 1, Opts)),
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
        <<"store">> => Store,
        <<"arweave-index-ids">> => true,
        <<"arweave-index-store">> => StoreOpts
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

has_any_indexed_tx(Height, Opts) ->
    case fetch_block_header(Height, Opts) of
        {ok, Block} ->
            TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
            lists:any(fun(TXID) -> is_tx_indexed(TXID, Opts) end, TXIDs);
        {error, _} ->
            false
    end.

highest_contiguous_indexed_block(StartBlock, MaxLookahead, Opts) ->
    highest_contiguous_indexed_block(
        StartBlock + 1,
        StartBlock + MaxLookahead,
        StartBlock,
        Opts
    ).

highest_contiguous_indexed_block(Current, Max, LastIndexed, _Opts)
        when Current > Max ->
    LastIndexed;
highest_contiguous_indexed_block(Current, Max, LastIndexed, Opts) ->
    case has_any_indexed_tx(Current, Opts) of
        true ->
            highest_contiguous_indexed_block(Current + 1, Max, Current, Opts);
        false ->
            LastIndexed
    end.

assert_indexed_range(From, To, _Opts) when From < To ->
    ok;
assert_indexed_range(From, To, Opts) ->
    ?assert(has_any_indexed_tx(From, Opts)),
    assert_indexed_range(From - 1, To, Opts).
