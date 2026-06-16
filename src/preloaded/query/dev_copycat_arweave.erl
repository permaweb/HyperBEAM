%%% @doc A `~copycat@1.0' engine that fetches block data from an Arweave node for
%%% replication. This engine works in _reverse_ chronological order by default.
%%% If `to' is omitted, it keeps moving downward from `from' until it reaches a
%%% block that is already indexed at the requested mode. If `to' is provided,
%%% every block in the range is processed.
-module(dev_copycat_arweave).
-device_libraries([lib_arweave_common]).
-export([arweave/3]).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ARWEAVE_DEVICE, <<"~arweave@2.9">>).

% GET /~cron@1.0/once&cron-path=~copycat@1.0/arweave

%% @doc Fetch blocks from an Arweave node between a given range, or from the
%% latest known block towards the Genesis block. If no range is provided, we
%% fetch blocks from the latest known block towards the Genesis block.
arweave(_Base, Request, Opts) ->
    case request_mode(Request, Opts) of
        {ok, list} ->
            case parse_range(Request, Opts) of
                {error, unavailable} ->
                    {error, unavailable};
                {ok, {_IncludePending, From, To}} ->
                    list_index(From, To, Opts)
            end;
        {ok, IndexMode} ->
            case parse_range(Request, Opts) of
                {error, unavailable} ->
                    {error, unavailable};
                {ok, {IncludePending, From, To}} ->
                    index_range(
                        Request, IncludePending, From, To, IndexMode, Opts)
            end;
        {error, Mode} ->
            {error, <<"Unsupported mode `", (hb_util:bin(Mode))/binary,
                "`. Supported modes are: shallow, deep, full, list">>}
    end.

request_mode(Request, Opts) ->
    case hb_maps:get(<<"mode">>, Request, <<"shallow">>, Opts) of
        <<"shallow">> -> {ok, shallow};
        <<"deep">> -> {ok, deep};
        <<"full">> -> {ok, full};
        <<"list">> -> {ok, list};
        Mode -> {error, Mode}
    end.

%% @doc Parse the range from the request.
parse_range(Request, Opts) ->
    FromArg = hb_maps:find(<<"from">>, Request, Opts),
    ToArg = hb_maps:find(<<"to">>, Request, Opts),
    maybe
        {ok, Tip} ?= range_tip(FromArg, ToArg, Opts),
        {ok, IncludePendingFrom, From} ?= from_height(FromArg, Tip),
        {ok, IncludePendingTo, To} ?= to_height(ToArg, Tip),
        case From < 0 orelse (is_integer(To) andalso To < 0) of
            true ->
                ?event(copycat_short,
                    {height_resolved_negative,
                        {from, From}, {to, To}}),
                {error, unavailable};
            false ->
                {ok, {IncludePendingFrom orelse IncludePendingTo, From, To}}
        end
    else
        {error, Reason} ->
            ?event(copycat_short,
                {latest_height_failed, {reason, Reason}}),
            {error, unavailable}
    end.

range_tip(FromArg, ToArg, Opts) ->
    case needs_tip(FromArg, true) orelse needs_tip(ToArg, false) of
        true -> latest_height(Opts);
        false -> {ok, undefined}
    end.

needs_tip(error, true) -> true;
needs_tip(error, false) -> false;
needs_tip({ok, <<"pending">>}, _DefaultFrom) -> true;
needs_tip({ok, <<"tip">>}, _DefaultFrom) -> true;
needs_tip({ok, Height}, _DefaultFrom) -> hb_util:int(Height) < 0.

from_height(error, Tip) ->
    {ok, true, Tip};
from_height({ok, Height}, Tip) ->
    normalize_height(<<"from">>, Height, Tip).

to_height(error, _Tip) ->
    {ok, false, undefined};
to_height({ok, Height}, Tip) ->
    normalize_height(<<"to">>, Height, Tip).

normalize_height(<<"to">>, <<"pending">>, Tip) -> {ok, true, Tip + 1};
normalize_height(_Key, <<"pending">>, Tip) -> {ok, true, Tip};
normalize_height(_Key, <<"tip">>, Tip) -> {ok, false, Tip};
normalize_height(_Key, Height, Tip) ->
    RequestedHeight = hb_util:int(Height),
    case RequestedHeight < 0 of
        true -> {ok, false, Tip + RequestedHeight};
        false -> {ok, false, RequestedHeight}
    end.

latest_height(Opts) ->
    case hb_ao:resolve(
        <<?ARWEAVE_DEVICE/binary, "/current/height">>,
        Opts
    ) of
        {ok, ResolvedHeight} -> {ok, hb_util:int(ResolvedHeight)};
        {error, Reason} -> {error, Reason}
    end.

index_range(Request, true, From, To, IndexMode, Opts) ->
    case index_pending(IndexMode, Opts) of
        {ok, PendingRes} ->
            case block_range_empty(From, To) of
                true ->
                    {ok, PendingRes};
                false ->
                    case fetch_blocks(Request, From, To, IndexMode, Opts) of
                        {ok, Stop} ->
                            {ok, PendingRes#{ blocks_stop => Stop }};
                        Error ->
                            Error
                    end
            end;
        Error ->
            case block_range_empty(From, To) of
                true -> Error;
                false -> fetch_blocks(Request, From, To, IndexMode, Opts)
            end
    end;
index_range(Request, false, From, To, IndexMode, Opts) ->
    fetch_blocks(Request, From, To, IndexMode, Opts).

block_range_empty(From, To) when is_integer(To), From < To ->
    true;
block_range_empty(_From, _To) ->
    false.

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
%% is omitted, stop at the first block indexed to the target mode.
fetch_blocks(Req, Current, To, _IndexMode, _Opts) when is_integer(To), Current < To ->
    ?event(copycat_short,
        {arweave_block_indexing_completed,
            {reached_target, To},
            {initial_request, Req}
        }
    ),
    {ok, To};
fetch_blocks(_Req, Current, undefined, _IndexMode, _Opts) when Current < 0 ->
    {ok, 0};
fetch_blocks(Req, Current, undefined, IndexMode, Opts) ->
    case is_block_indexed(Current, IndexMode, Opts) of
        true ->
            stop_at_indexed_block(Req, Current);
        false ->
            BlockRes = fetch_block_header(Current, Opts),
            case IndexMode =:= shallow andalso is_already_indexed(BlockRes, Opts) of
                true ->
                    stop_at_indexed_block(Req, Current);
                false ->
                    observe_event(<<"block_indexed">>, fun() ->
                        process_block(BlockRes, Current, undefined, IndexMode, Opts)
                    end),
                    fetch_blocks(Req, Current - 1, undefined, IndexMode, Opts)
            end
    end;
fetch_blocks(Req, Current, To, IndexMode, Opts) ->
    observe_event(<<"block_indexed">>, fun() ->
        process_block(fetch_block_header(Current, Opts), Current, To, IndexMode, Opts)
    end),
    fetch_blocks(Req, Current - 1, To, IndexMode, Opts).

stop_at_indexed_block(Req, Current) ->
    ?event(copycat_short,
        {arweave_block_indexing_completed,
            {stop_at_indexed_block, Current},
            {initial_request, Req}
        }
    ),
    {ok, Current}.

is_already_indexed({ok, Block}, Opts) ->
    TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
    lists:any(fun(TXID) -> is_tx_indexed(TXID, Opts) end, TXIDs);
is_already_indexed({error, _}, _Opts) ->
    false.

process_block(BlockRes, Current, To, IndexMode, Opts) ->
    case BlockRes of
        {ok, Block} ->
            ?event(debug_copycat, {{processing_block, Current},
                {indep_hash, hb_maps:get(<<"indep_hash">>, Block, <<>>)}}),
            case maybe_index_ids(Block, IndexMode, Opts) of
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
                    case SkippedTXs of
                        0 -> ok = write_block_index(Current, IndexMode, Opts);
                        _ -> ok
                    end,
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

block_indexed_path(Height) ->
    <<"block/", (hb_util:bin(Height))/binary, "/mode">>.

write_block_index(Height, IndexMode, Opts) ->
    #{ <<"index-store">> := Store } = hb_store_arweave:store_from_opts(Opts),
    hb_store:write(
        Store,
        #{ block_indexed_path(Height) => mode_name(IndexMode) },
        Opts
    ).

is_block_indexed(Height, IndexMode, Opts) ->
    case hb_store_arweave:store_from_opts(Opts) of
        no_store ->
            false;
        #{ <<"index-store">> := Store } ->
            case hb_store:read(Store, block_indexed_path(Height), Opts) of
                {ok, Bin} ->
                    mode_rank(Bin) >= mode_rank(IndexMode);
                _ ->
                    false
            end
    end.

mode_name(shallow) -> <<"shallow">>;
mode_name(deep) -> <<"deep">>;
mode_name(full) -> <<"full">>.

mode_rank(shallow) -> 1;
mode_rank(deep) -> 2;
mode_rank(full) -> 3;
mode_rank(<<"shallow">>) -> mode_rank(shallow);
mode_rank(<<"deep">>) -> mode_rank(deep);
mode_rank(<<"full">>) -> mode_rank(full);
mode_rank(_Other) -> 0.

%% @doc Index the IDs of all transactions in the block if configured to do so.
maybe_index_ids(Block, IndexMode, Opts) ->
    TXIDs = hb_maps:get(<<"txs">>, Block, [], Opts),
    TotalTXs = length(TXIDs),
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
            case resolve_tx_headers(TXIDs, Opts) of
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
                    TXResults = process_txs(
                        ValidTXs, BlockStartOffset, IndexMode, Opts),
                    {block_cached, TXResults#{ total_txs => TotalTXs }}
            end
    end.

%% @doc Apply Fun to each item in Items with parallel workers.
%% Fun takes an item and returns a result.
%% Returns a list of results in the same order as the input items.
%% Uses arweave_index_workers from Opts to determine max concurrency (default 1 = sequential).
parallel_map(Items, Fun, Opts) ->
    MaxWorkers = max(1, hb_opts:get(arweave_index_workers, 1, Opts)),
    hb_pmap:parallel_map(Items, Fun, MaxWorkers).

counters(Items, Bundles, Skipped) ->
    #{
        items_count => Items,
        bundle_count => Bundles,
        skipped_count => Skipped
    }.

%% @doc Process a single transaction and return its contribution to the counters.
%% Returns a map with keys: items_count, bundle_count, skipped_count
process_tx({{TX, _TXDataRoot}, EndOffset}, BlockStartOffset, IndexMode, Opts) ->
    ArweaveStore = hb_store_arweave:store_from_opts(Opts),
    TXID = hb_util:encode(TX#tx.id),
    TXEndOffset = BlockStartOffset + EndOffset,
    TXStartOffset = TXEndOffset - TX#tx.data_size,
    ?event(debug_copycat, {writing_index,
        {id, {explicit, TXID}},
        {offset, TXStartOffset},
        {size, TX#tx.data_size}
    }),
    case observe_event(<<"item_indexed">>, fun() ->
        hb_store_arweave:write_offset(
            ArweaveStore,
            TXID,
            <<"tx@1.0">>,
            TXStartOffset,
            TX#tx.data_size
        )
    end) of
        ok ->
            case is_bundle_tx(TX, Opts) of
                false ->
                    counters(0, 0, 0);
                true when IndexMode =/= shallow ->
                    try
                        case hb_store_arweave:read_chunks(
                            TXStartOffset, TX#tx.data_size, Opts) of
                            {ok, BundleData} ->
                                {TotalTime, IndexRes} = timer:tc(
                                    fun() ->
                                        index_full_bundle_bytes(
                                            BundleData,
                                            TXStartOffset,
                                            IndexMode,
                                            ArweaveStore,
                                            Opts
                                        )
                                    end
                                ),
                                case IndexRes of
                                    {ok, ItemsCount} ->
                                        record_event_metrics(
                                            <<"item_indexed">>,
                                            ItemsCount,
                                            TotalTime
                                        ),
                                        counters(ItemsCount, 1, 0);
                                    {error, IndexError} ->
                                        skip_bundle(TXID, IndexError)
                                end;
                            {error, ReadError} ->
                                skip_bundle(TXID, ReadError);
                            not_found ->
                                skip_bundle(TXID, not_found)
                        end
                    catch
                        _:Reason:_ ->
                            skip_bundle(TXID, Reason)
                    end;
                true ->
                    % Shallow confirmed indexing only needs the bundle header, avoiding
                    % a full L1 data download while still writing direct item offsets.
                    ?event(debug_copycat, {fetching_bundle_header, 
                        {tx_id, {string, TXID}},
                        {tx_end_offset, TXEndOffset},
                        {tx_data_size, TX#tx.data_size}
                    }),
                    case download_bundle_header(TXEndOffset, TX#tx.data_size, Opts) of
                        {ok, HeaderSize, BundleIndex} ->
                            % Batch event tracking: measure total time and count for all write_offset calls
                            {TotalTime, IndexItemsRes} = timer:tc(fun() ->
                                lists:foldl(
                                    fun
                                        (_Item, {error, _} = Error) ->
                                            Error;
                                        ({ItemID, Size}, {ItemStartOffset, ItemsCountAcc}) ->
                                            case hb_store_arweave:write_offset(
                                                ArweaveStore,
                                                hb_util:encode(ItemID),
                                                <<"ans104@1.0">>,
                                                ItemStartOffset,
                                                Size
                                            ) of
                                                ok ->
                                                    {add_data_offset(ItemStartOffset, Size),
                                                        ItemsCountAcc + 1};
                                                WriteError ->
                                                    {error, {write_offset_failed, WriteError}}
                                            end
                                    end,
                                    {add_data_offset(TXStartOffset, HeaderSize), 0},
                                    BundleIndex
                                )
                            end),
                            case IndexItemsRes of
                                {error, Reason} ->
                                    skip_bundle(TXID, Reason);
                                {_, ItemsCount} ->
                                    ?event(debug_copycat,
                                        {bundle_items_indexed,
                                            {tx_id, {string, TXID}},
                                            {items_count, ItemsCount}
                                    }),
                                    % Single event record for the batch
                                    record_event_metrics(<<"item_indexed">>, ItemsCount, TotalTime),
                                    counters(ItemsCount, 1, 0)
                            end;
                        {error, Reason} ->
                            skip_bundle(TXID, Reason)
                    end
            end;
        WriteError ->
            ?event(
                copycat_short,
                {arweave_tx_skipped,
                    {tx_id, {explicit, TXID}},
                    {reason, {write_offset_failed, WriteError}}
                }
            ),
            counters(0, 0, 1)
    end.

%% @doc Process transactions: spawn workers and manage the worker pool.
%% This function processes transactions in parallel using parallel_map.
%% When arweave_index_workers <= 1, processes sequentially (one worker at a time).
%% When arweave_index_workers > 1, processes in parallel with the specified concurrency limit.
%% Returns a map with keys: items_count, bundle_count, skipped_count.
process_txs(ValidTXs, BlockStartOffset, IndexMode, Opts) ->
    Results = parallel_map(
        ValidTXs,
        fun(TXWithData) -> process_tx(TXWithData, BlockStartOffset, IndexMode, Opts) end,
        Opts
    ),
    sum_counters(Results).

sum_counters(Results) ->
    lists:foldl(
        fun(Result, Acc) ->
            maps:merge_with(
                fun(_Key, ResultCount, AccCount) -> ResultCount + AccCount end,
                Result,
                Acc
            )
        end,
        counters(0, 0, 0),
        Results
    ).

skip_bundle(EncodedTXID, Reason) ->
    ?event(
        copycat_short,
        {arweave_bundle_skipped,
            {tx_id, {explicit, EncodedTXID}},
            {reason, Reason}
        }
    ),
    counters(0, 1, 1).

index_full_bundle_bytes(BundleData, BundleStartOffset, IndexMode, Store, Opts) ->
    case ar_bundles:decode_bundle_header(BundleData) of
        invalid_bundle_header ->
            {error, invalid_bundle_header};
        {ItemsBin, BundleIndex} ->
            HeaderSize = byte_size(BundleData) - byte_size(ItemsBin),
            index_full_bundle_items(
                BundleIndex,
                ItemsBin,
                add_data_offset(BundleStartOffset, HeaderSize),
                IndexMode,
                Store,
                Opts,
                0
            )
    end.

%% @doc Index unconfirmed transactions from the Arweave mempool.
index_pending(IndexMode, Opts) ->
    case hb_ao:resolve(<<?ARWEAVE_DEVICE/binary, "/pending">>, Opts) of
        {ok, TXIDs} when is_list(TXIDs) ->
            Results = parallel_map(
                TXIDs,
                fun(TXID) -> process_pending_tx(TXID, IndexMode, Opts) end,
                Opts
            ),
            {ok, (sum_counters(Results))#{ total_txs => length(TXIDs) }};
        Error ->
            Error
    end.

process_pending_tx(TXID, IndexMode, Opts) ->
    case resolve_pending_tx_header(TXID, Opts) of
        {ok, TX} ->
            Store = hb_store_arweave:store_from_opts(Opts),
            case hb_store_arweave:write_offset(
                Store, TXID, <<"tx@1.0">>, relative, TX#tx.data_size) of
                ok ->
                    index_pending_children(TXID, TX, IndexMode, Store, Opts);
                WriteError ->
                    ?event(
                        copycat_short,
                        {arweave_pending_tx_skipped,
                            {tx_id, {explicit, TXID}},
                            {reason, {write_offset_failed, WriteError}}
                        }
                    ),
                    counters(0, 0, 1)
            end;
        error ->
            counters(0, 0, 1)
    end.

resolve_pending_tx_header(TXID, Opts) ->
    try
        case hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9">> },
            #{
                <<"path">> => <<"pending">>,
                <<"pending">> => TXID,
                <<"exclude-data">> => true
            },
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
                    {arweave_pending_tx_skipped,
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
                {arweave_pending_tx_skipped,
                    {tx_id, {explicit, TXID}},
                    {class, Class},
                    {reason, Reason}
                }
            ),
            error
    end.

index_pending_children(TXID, TX, IndexMode, Store, Opts) ->
    case is_bundle_tx(TX, Opts) of
        false ->
            counters(0, 0, 0);
        true ->
            Offset = #{ <<"relative">> => TXID, <<"offset">> => 0 },
            case hb_store_arweave:read_chunks(Offset, TX#tx.data_size, Opts) of
                {ok, BundleData} ->
                    case index_full_bundle_bytes(
                        BundleData, Offset, IndexMode, Store, Opts) of
                        {ok, ItemsCount} -> counters(ItemsCount, 1, 0);
                        {error, Reason} -> skip_bundle(TXID, Reason)
                    end;
                {error, Reason} ->
                    skip_bundle(TXID, Reason)
            end
    end.

index_full_bundle_items(
        [], _ItemsBin, _ItemStartOffset, _IndexMode, _Store, _Opts, Count) ->
    {ok, Count};
index_full_bundle_items(
    [{ItemID, Size} | Rest],
    ItemsBin,
    ItemStartOffset,
    IndexMode,
    Store,
    Opts,
    Count
) when byte_size(ItemsBin) >= Size ->
    <<ItemBinary:Size/binary, RestBin/binary>> = ItemsBin,
    EncodedItemID = hb_util:encode(ItemID),
    ParseResult =
        case IndexMode of
            shallow ->
                not_parsed;
            _ ->
                try ar_bundles:deserialize_header(ItemBinary)
                catch _:_ -> error
                end
        end,
    case hb_store_arweave:write_offset(
        Store,
        EncodedItemID,
        <<"ans104@1.0">>,
        ItemStartOffset,
        Size
    ) of
        ok ->
            ok =
                case {IndexMode, ParseResult} of
                    {full, {ok, _, Parsed}} ->
                        LocalOpts = hb_store:scope(Opts, local),
                        Msg = hb_message:convert(
                            Parsed, <<"structured@1.0">>, <<"ans104@1.0">>, LocalOpts),
                        {ok, _Path} = hb_cache:write(Msg, LocalOpts),
                        ok;
                    _ -> ok
                end,
            DescendantRes =
                case {IndexMode =/= shallow, ParseResult} of
                    {true, {ok, HeaderSize, ParsedItem}} ->
                        case is_bundle_tx(ParsedItem, Opts) of
                            true ->
                                index_full_bundle_bytes(
                                    ParsedItem#tx.data,
                                    add_data_offset(ItemStartOffset, HeaderSize),
                                    IndexMode,
                                    Store,
                                    Opts
                                );
                            false ->
                                {ok, 0}
                        end;
                    {true, _} ->
                        {ok, 0};
                    _ ->
                        {ok, 0}
                end,
            case DescendantRes of
                {ok, DescendantCount} ->
                    index_full_bundle_items(
                        Rest,
                        RestBin,
                        add_data_offset(ItemStartOffset, Size),
                        IndexMode,
                        Store,
                        Opts,
                        Count + 1 + DescendantCount
                    );
                {error, _} = Error ->
                    Error
            end;
        WriteError ->
            {error, {write_offset_failed, WriteError}}
    end;
index_full_bundle_items(
        _BundleIndex, _ItemsBin, _ItemStartOffset, _IndexMode,
        _Store, _Opts, _Count) ->
    {error, invalid_bundle_header}.

add_data_offset(#{ <<"relative">> := TXID, <<"offset">> := Offset }, Add) ->
    #{ <<"relative">> => TXID, <<"offset">> => Offset + Add };
add_data_offset(Offset, Add) ->
    Offset + Add.

%% @doc Check whether a TX header indicates bundle content.
is_bundle_tx(TX, _Opts) ->
    ar_tx:type(TX) =/= binary.

%% @doc Download and decode a bundle header from chunk data.
download_bundle_header(EndOffset, Size, Opts) ->
    observe_event(<<"bundle_header">>, fun() ->
        lib_arweave_common:bundle_header(EndOffset - Size, Size, Opts)
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

%% @doc Record event metrics (count and duration) using hb_event:record.
record_event_metrics(MetricName, Count, Duration) ->
    hb_event:record(<<"arweave_block_count">>, MetricName, #{}, Count),
    hb_event:record(<<"arweave_block_duration">>, MetricName, #{}, Duration).

%% @doc Track an operation's execution time and count using hb_event:record.
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
    % This bundle previously triggered the ANS-104 tag-section boundary bug:
    % the decoder ran past the declared tag bytes into the JSON body and
    % crashed with a badmatch on the body content (the `"address":"0x..."'
    % string). With the strict tag-section boundary enforced, the item is
    % decoded and indexed correctly.
    ?assertMatch(
        {ok, _},
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
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(Block))/binary, "&"
                "to=", (hb_util:bin(Block))/binary, "&"
                "mode=deep"
            >>,
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
                "mode=shallow"
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
    %% First index the block using shallow mode
    Block = 1827942,
    BlockBin = hb_util:bin(Block),
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", BlockBin/binary, "&"
                "to=", BlockBin/binary, "&"
                "mode=shallow"
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
                "mode=shallow"
            >>,
            Opts
        ),
    {ok, IndexedBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(Higher2))/binary, "&"
                "mode=shallow"
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
                "mode=shallow"
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
                "mode=shallow"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(LowerBlock, Opts)),
    ok.

auto_stop_partial_index_test_parallel() ->
    {_TestStore, StoreOpts, Opts} = setup_index_opts(),
    IndexedBlock = 1826700,
    HigherBlock = IndexedBlock + 1,
    {ok, BlockData} = fetch_block_header(IndexedBlock, Opts),
    [OneTXID | _] = hb_maps:get(<<"txs">>, BlockData, [], Opts),
    ok = hb_store_arweave:write_offset(
        StoreOpts, OneTXID, <<"tx@1.0">>, 0, 0),
    {ok, IndexedBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(HigherBlock))/binary, "&"
                "mode=shallow"
            >>,
            Opts
        ),
    ?assert(has_any_indexed_tx(HigherBlock, Opts)),
    ?assert(has_any_indexed_tx(IndexedBlock, Opts)),
    ?assertNot(has_any_indexed_tx(IndexedBlock-1, Opts)),
    ok.

negative_parse_range_test_parallel() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    {ok, Tip} =
        hb_ao:resolve(
            <<?ARWEAVE_DEVICE/binary, "/current/height">>,
            Opts
        ),
    {ok, {false, NegativeFrom, UndefinedTo}} =
        parse_range(#{ <<"from">> => <<"-3">> }, Opts),
    ?assertEqual(hb_util:int(Tip) - 3, NegativeFrom),
    ?assertEqual(undefined, UndefinedTo),
    {ok, {false, PositiveFrom, NegativeTo}} =
        parse_range(#{ <<"from">> => <<"10">>, <<"to">> => <<"-3">> }, Opts),
    ?assertEqual(10, PositiveFrom),
    ?assertEqual(hb_util:int(Tip) - 3, NegativeTo),
    {ok, {true, DefaultPendingFrom, DefaultNegativeTo}} =
        parse_range(#{ <<"to">> => <<"-3">> }, Opts),
    ?assertEqual(hb_util:int(Tip), DefaultPendingFrom),
    ?assertEqual(hb_util:int(Tip) - 3, DefaultNegativeTo),
    {ok, {true, PendingFrom, PendingTo}} =
        parse_range(#{ <<"from">> => <<"pending">>, <<"to">> => <<"pending">> }, Opts),
    ?assertEqual(hb_util:int(Tip), PendingFrom),
    ?assertEqual(hb_util:int(Tip) + 1, PendingTo),
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
                <<"~copycat@1.0/arweave&mode=shallow">>, Opts)
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
                "mode=shallow"
            >>,
            Opts
        ),
    {ok, StopBlock} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", NegativeFrom/binary, "&"
                "mode=shallow"
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

pending_range_indexes_bundle_children_test() ->
    {_TestStore, StoreOpts, DefaultOpts} = setup_index_opts(),
    Wallet = ar_wallet:new(),
    Child = ar_bundles:sign_item(
        #tx{
            data = <<"pending-child">>,
            tags = [{<<"content-type">>, <<"text/plain">>}]
        },
        Wallet
    ),
    {undefined, BundleData} =
        ar_bundles:serialize_bundle(list, [Child], false),
    RootTX =
        ar_tx:sign(
            ar_tx:generate_chunk_tree(
                #tx{
                    data = BundleData,
                    data_size = byte_size(BundleData),
                    format = 2,
                    tags = [
                        {<<"bundle-format">>, <<"binary">>},
                        {<<"bundle-version">>, <<"2.0.0">>}
                    ]
                }
            ),
            Wallet
        ),
    TXID = hb_util:encode(RootTX#tx.id),
    ChildID = hb_util:encode(ar_bundles:id(Child, signed)),
    DataPath =
        ar_merkle:generate_path(RootTX#tx.data_root, 0, RootTX#tx.data_tree),
    HeaderJSON = ar_tx:tx_to_json_struct(RootTX#tx{ data = <<>> }),
    ChunkBody =
        hb_json:encode(
            #{
                <<"chunk">> => hb_util:encode(BundleData),
                <<"data_path">> => hb_util:encode(DataPath)
            }
        ),
    {ok, MockNode, MockHandle} = hb_mock_server:start([
        {"/block/current", block_current, {200, <<"{\"height\": 10}">>}},
        {"/tx/pending", pending, {200, hb_json:encode([TXID])}},
        {"/unconfirmed_tx/:id", pending_tx, {200, hb_json:encode(HeaderJSON)}},
        {"/unconfirmed_chunk/:id/:offset", pending_chunk, {200, ChunkBody}}
    ]),
    Routes = [
        #{
            <<"template">> => <<"^/arweave">>,
            <<"nodes">> => [
                #{
                    <<"match">> => <<"^/arweave">>,
                    <<"with">> => MockNode,
                    <<"opts">> => #{ <<"http-client">> => httpc }
                }
            ],
            <<"stop-after">> => true
        }
    ],
    ReadStore = StoreOpts#{ <<"routes">> => Routes },
    Opts =
        DefaultOpts#{
            <<"routes">> => Routes,
            <<"arweave-index-blocks">> => false,
            <<"arweave-index-store">> => ReadStore
        },
    try
        {ok, #{ items_count := 1, total_txs := 1 }} =
            hb_ao:resolve(
                <<"~copycat@1.0/arweave&from=pending&to=pending">>, Opts),
        {ok, #{ items_count := 1, total_txs := 1 }} =
            hb_ao:resolve(
                <<"~copycat@1.0/arweave&mode=full&from=pending&to=pending">>,
                Opts),
        ?assertMatch(
            {ok, #{ <<"start-offset">> := relative }},
            hb_store_arweave:read_offset(ReadStore, TXID, Opts)
        ),
        ?assertMatch(
            {ok, #{ <<"start-offset">> := #{ <<"relative">> := TXID } }},
            hb_store_arweave:read_offset(ReadStore, ChildID, Opts)
        ),
        {ok, ChildMsg} =
            hb_store_arweave:read(ReadStore, #{ <<"read">> => ChildID }, Opts),
        ?assertMatch(
            {ok, _},
            hb_cache:read(ChildID, hb_store:scope(Opts, local))
        ),
        ?assertEqual(ChildID, hb_message:id(ChildMsg, signed, Opts))
    after
        hb_mock_server:stop(MockHandle)
    end.

assert_indexed_range(From, To, _Opts) when From < To ->
    ok;
assert_indexed_range(From, To, Opts) ->
    ?assert(has_any_indexed_tx(From, Opts)),
    assert_indexed_range(From - 1, To, Opts).

small_block_full_mode_test() ->
    {_TestStore, _StoreOpts, Opts} = setup_index_opts(),
    Block = 1889322,
    {ok, Block} =
        hb_ao:resolve(
            <<
                "~copycat@1.0/arweave&"
                "from=", (hb_util:bin(Block))/binary, "&"
                "to=", (hb_util:bin(Block))/binary, "&"
                "mode=full"
            >>,
            Opts
        ),
    L3ID = <<"npAzk_BomjWBQQr_xnmlhdxjyl97EJnNv_MAaXffs1s">>,
    assert_item_read(L3ID, Opts),
    LocalOpts = hb_store:scope(Opts, local),
    {ok, L3Header} = hb_cache:read(L3ID, LocalOpts),
    L3Data =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, L3Header}, <<"data">>},
                {{as, <<"message@1.0">>, L3Header}, <<"body">>}
            ],
            <<>>,
            Opts
        ),
    ?assert(byte_size(L3Data) > 0),
    L3AppName = hb_maps:get(
        <<"app-name">>, hb_message:uncommitted(L3Header, Opts), undefined, Opts),
    ?assertNotEqual(undefined, L3AppName),
    {ok, MessageIDs} =
        hb_cache:match(#{<<"app-name">> => L3AppName}, LocalOpts),
    ?assert(lists:member(L3ID, MessageIDs), {missing_match_index, L3ID}),
    ?assert(hb_message:verify(L3Header, all, Opts), {verify_failed, L3ID}),
    ?assertEqual(L3ID, hb_message:id(L3Header, signed, Opts)),
    ok.
