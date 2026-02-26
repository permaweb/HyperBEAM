%%% @doc A device that provides access to Arweave network information, relayed
%%% from a designated node.
%%%
%%% The node(s) that are used to query data may be configured by altering the
%%% `/arweave` route in the node's configuration message.
-module(dev_arweave).
-export([tx/3, raw/3, chunk/3, block/3, current/3, status/3, price/3, tx_anchor/3]).
-export([post_tx/3, post_tx/4, post_binary_ans104/2, post_json_chunk/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(IS_BLOCK_ID(X), (is_binary(X) andalso byte_size(X) == 64)).

%% @doc Proxy the `/info' endpoint from the Arweave node.
status(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/info">>, Opts).

%% @doc Returns the given transaction as an AO-Core message. By default, this
%% embeds the `/raw` payload. Set `exclude-data` to true to return just the
%% header.
tx(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"POST">> -> post_tx(Base, Request, Opts);
        <<"GET">> -> get_tx(Base, Request, Opts)
    end.

%% @doc Upload either an ans104 or an L1 transaction to Arweave.
%% Ensures that uploaded transactions are stored in the local cache after a
%% successful response has been received.
%% 
%% Note: When uploading ans104 transactions, this function will use the
%% node's default bundler. If instead you want to use this node as a bundler
%% you should use the ~bundler@1.0 device.
post_tx(Base, RawRequest, Opts) ->
    {ok, Request} = extract_target(Base, RawRequest, Opts),
    case hb_message:commitment_devices(Request, Opts) of
        [Device] -> post_tx(Base, Request, Opts, Device);
        [] -> 
            ?event(warning,
                {no_commitment_devices,
                    {request, Request},
                    {base, Base}
                }
            ),
            {error, <<"No commitment found on `POST tx` request.">>};
        Devices ->
            ?event(error, {too_many_commitment_devices, Devices}),
            {error, too_many_commitment_devices}
    end.

%% @doc Extract the target from the request or base message.
extract_target(Base, Request, Opts) ->
    case hb_maps:get(<<"target">>, Request, <<"request">>, Opts) of
        <<"request">> ->
            {ok, Request};
        <<"base">> ->
            {ok, Base};
        <<"base:", BaseTarget/binary>> ->
            hb_maps:find(BaseTarget, Base, Opts);
        <<"request:", RequestTarget/binary>> ->
            hb_maps:find(RequestTarget, Request, Opts);
        _ ->
            not_found
    end.

post_tx(_Base, Request, Opts, <<"tx@1.0">>) ->
    TX = hb_message:convert(Request, <<"tx@1.0">>, Opts),
    JSON = ar_tx:tx_to_json_struct(TX#tx{ data = <<>> }),
    Serialized = hb_json:encode(JSON),
    LogExtra = [
        {codec, <<"tx@1.0">>},
        {id, {explicit, hb_util:human_id(TX#tx.id)}}
    ],
    Res = request(
        <<"POST">>,
        <<"/tx">>,
        #{ <<"body">> => Serialized },
        LogExtra,
        Opts
    ),
    case Res of
        {ok, _} ->
            CacheRes = hb_cache:write(Request, Opts),
            case CacheRes of
                {ok, _} ->
                    ?event(arweave_debug, {tx_cached, {msg, Request}, {status, ok}});
                _ ->
                    ?event(error, {tx_failed_to_cache, {msg, Request}, CacheRes})
            end;
        _ ->
            ok
    end,
    Res;

post_tx(_Base, Request, Opts, <<"ans104@1.0">>) ->
    TX = hb_message:convert(Request, <<"ans104@1.0">>, Opts),
    Serialized = ar_bundles:serialize(TX),
    LogExtra = [
        {codec, <<"ans104@1.0">>},
        {id, {explicit, hb_util:human_id(TX#tx.id)}}
    ],
    post_binary_ans104(Serialized, LogExtra, Opts).

post_binary_ans104(SerializedTX, Opts) ->
    LogExtra = [
        {codec, <<"ans104@1.0">>},
        {id, unknown}
    ],
    post_binary_ans104(SerializedTX, LogExtra, Opts).

post_binary_ans104(SerializedTX, LogExtra, Opts) ->
    Res = hb_http:post(
        hb_opts:get(bundler_ans104, not_found, Opts),
        #{
            <<"path">> => <<"/tx">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => SerializedTX
        },
        Opts#{
            http_client =>
                hb_opts:get(bundler_ans104_http_client, httpc, Opts)
        }
    ),
    to_message(<<"/tx">>, <<"POST">>, Res, LogExtra, Opts).

%% @doc Get a transaction from the Arweave node, as indicated by the
%% `tx` key in the request or base message. By default, this embeds the data
%% payload. Set `exclude_data` to true to return just the header.
get_tx(Base, Request, Opts) ->
    case find_txid(Base, Request, Opts) of
        not_found -> {error, not_found};
        TXID ->
            request(
                <<"GET">>,
                <<"/tx/", TXID/binary>>,
                Opts#{ exclude_data => exclude_data(Base, Request, Opts) }
            )
    end.

%% @doc Get raw transaction *data* and `content-type` of an Arweave message.
%% Does not deserialize the message, nor return signature information. Included
%% only for compatibility with the legacy Arweave gateway `/raw` endpoint.
raw(Base, Request, Opts) ->
    case find_key(<<"raw">>, Base, Request, not_found, Opts) of
        not_found -> {error, not_found};
        TXID ->
            ?event(
                debug_raw,
                {found_txid, {id, TXID}}
            ),
            % Read the data from the local cache.
            IndexStore = hb_opts:get(arweave_index_store, no_store, Opts),
            case hb_store_arweave:read_offset(IndexStore, TXID) of
                {ok, Index = #{ <<"codec-device">> := <<"ans104@1.0">> }} ->
                    % Indexed messages of codec `ans104@1.0' are stored with
                    % the `offset` referencing the start of the *header*.
                    % Subsequently, we read the chunks and then deserialize
                    % only the wrapper, yielding a #tx record that may
                    % contain a bundle.
                    ?event(
                        debug_raw,
                        {found_offset, {id, TXID}, {index, Index}}
                    ),
                    case get_chunk_range(Index, Opts) of
                        {ok, Data} ->
                            TX = ar_bundles:deserialize_item_wrapper(Data),
                            ?event(
                                debug_raw,
                                {deserialized_raw, {id, TXID}, {tx, TX}}
                            ),
                            ContentType =
                                list_find(
                                    <<"content-type">>,
                                    TX#tx.tags,
                                    <<"application/octet-stream">>
                                ),
                            {ok, #{
                                <<"content-type">> => ContentType,
                                <<"data">> => Data
                            }};
                        Error -> Error
                    end;
                {ok, Index = #{ <<"codec-device">> := <<"tx@1.0">> }} ->
                    % Indexed messages of codec `tx@1.0' are stored with
                    % the `offset` referencing the start of the data.
                    % Subsequently, we read the chunks and return them
                    % as-is.
                    ?event(
                        debug_raw,
                        {found_offset, {id, TXID}, {index, Index}}
                    ),
                    case get_chunk_range(Index, Opts) of
                        {ok, Data} ->
                            ?event(
                                debug_raw,
                                {fetched_chunks, {id, TXID}, {data, Data}}
                            ),
                            {ok, StructuredTXHeader} =
                                get_tx(
                                    #{ <<"tx">> => TXID },
                                    #{ <<"exclude-data">> => true },
                                    Opts
                                ),
                            ContentType =
                                hb_ao:get(
                                    <<"content-type">>,
                                    StructuredTXHeader,
                                    <<"application/octet-stream">>,
                                    Opts#{
                                        cache_control =>
                                            [<<"no-cache">>, <<"no-store">>]
                                    }
                                ),
                            ?event(
                                debug_raw,
                                {content_type_and_data,
                                    {id, TXID},
                                    {content_type, ContentType},
                                    {data, Data}
                                }
                            ),
                            {
                                ok,
                                #{
                                    <<"content-type">> => ContentType,
                                    <<"data">> => Data
                                }
                            };
                        Error ->
                            ?event(
                                debug_raw,
                                {error, {id, TXID}, {returned, Error}}
                            ),
                            Error
                    end;
                Error ->
                    ?event(
                        debug_raw,
                        {error, {id, TXID}, {returned, Error}}
                    ),
                    Error
            end
    end.

%% @doc Case-insensitively find a key in a list and return its value.
list_find(_Key, [], Default) -> Default;
list_find(Key, [{XKey, Value} | Rest], Default) ->
    NormalizedKey = hb_util:to_lower(hb_ao:normalize_key(XKey)),
    if NormalizedKey =:= Key -> Value;
    true -> list_find(Key, Rest, Default)
    end.

%% @doc Case-insensitively find a key in a list and return its value.
list_find(_Key, [], Default) -> Default;
list_find(Key, [{XKey, Value} | Rest], Default) ->
    NormalizedKey = hb_util:to_lower(hb_ao:normalize_key(XKey)),
    if NormalizedKey =:= Key -> Value;
    true -> list_find(Key, Rest, Default)
    end.

%% @doc Retrieve the data of an Arweave message that has been indexed.
data(TXID, Opts) ->
    request(<<"GET">>, <<"/raw/", TXID/binary>>, Opts).

chunk(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"POST">> -> post_chunk(Base, Request, Opts);
        <<"GET">> -> get_chunk_range(Base, Request, Opts)
    end.

post_chunk(_Base, Request, Opts) ->
    Serialized = hb_json:encode(Request),
    post_json_chunk(Serialized, Opts).

post_json_chunk(JSON, Opts) ->
    hb_http:post(
        hb_opts:get(gateway, not_found, Opts),
        #{
            <<"path">> => <<"/chunk">>,
            <<"body">> => JSON
        },
        Opts
    ).

get_chunk_range(_Base, Request, Opts) ->
    Offset = hb_util:int(hb_ao:get(<<"offset">>, Request, Opts)),
    Length = hb_util:int(hb_ao:get(<<"length">>, Request, 1, Opts)),
    case fetch_chunk_range(Offset, Length, Opts) of
        {ok, Chunks} ->
            Data = iolist_to_binary(Chunks),
            case hb_maps:is_key(<<"length">>, Request, Opts) of
                true ->
                    {ok, binary:part(Data, 0, Length)};
                false ->
                    {ok, Data}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Fetch a range of chunks in parallel. Dispatches to pre-threshold or
%% post-threshold algorithm depending on the offset. A single TX/data-item
%% cannot span the strict data split threshold, so mixed ranges are rejected.
fetch_chunk_range(Offset, Length, Opts) ->
    EndOffset = Offset + Length - 1,
    ?event(arweave_debug, {fetch_chunk_range,
        {length, Length}, {start_offset, Offset}, {end_offset, EndOffset}}),
    case {Offset >= ?STRICT_DATA_SPLIT_THRESHOLD,
          EndOffset >= ?STRICT_DATA_SPLIT_THRESHOLD} of
        {true, true} ->
            fetch_post_threshold(Offset, EndOffset, Opts);
        {false, false} ->
            fetch_pre_threshold(Offset, EndOffset, Opts);
        {false, true} ->
            {error, chunk_range_spans_strict_data_split_threshold}
    end.

%% @doc Post-threshold: chunks occupy fixed 256KiB buckets. A single pass at
%% DATA_CHUNK_SIZE increments covers all chunks.
fetch_post_threshold(Offset, EndOffset, Opts) ->
    Offsets = generate_offsets(Offset, EndOffset, ?DATA_CHUNK_SIZE),
    case fetch_and_collect(Offsets, Opts) of
        {ok, ChunkInfos} -> assemble_chunks(ChunkInfos, Offset);
        Error -> Error
    end.

%% @doc Pre-threshold: chunks can be any size <= 256KiB. First pass at
%% DATA_CHUNK_SIZE increments, then iteratively fill gaps until contiguous.
fetch_pre_threshold(Offset, EndOffset, Opts) ->
    Offsets = generate_offsets(Offset, EndOffset, ?DATA_CHUNK_SIZE),
    case fetch_and_collect(Offsets, Opts) of
        {ok, ChunkInfos} ->
            fill_gaps(ChunkInfos, Offset, EndOffset, Opts);
        Error -> Error
    end.

%% @doc Iteratively detect gaps in coverage and fetch the chunk at the start
%% of each gap until the entire range [Offset, EndOffset] is covered.
fill_gaps(ChunkInfos, Offset, EndOffset, Opts) ->
    Sorted = sort_chunks(ChunkInfos),
    case find_gaps(Sorted, Offset, EndOffset) of
        [] ->
            assemble_chunks(Sorted, Offset);
        Gaps ->
            % WARNING: the find_gaps logic is untested in production and may not
            % be needed. We have yet to find an L1 TX that is chunked in such
            % a way as to create gaps when using our naive 256KiB chunking.
            GapOffsets = [Start || {Start, _End} <- Gaps],
            ?event(warning,
                {fetch_chunk_gap_handling_untested,
                    {gap_offsets, GapOffsets}}),
            case fetch_and_collect(GapOffsets, Opts) of
                {ok, NewInfos} ->
                    fill_gaps(
                        Sorted ++ NewInfos,
                        Offset, EndOffset, Opts
                    );
                Error -> Error
            end
    end.

%% @doc Fetch chunks at the given offsets in parallel and parse the responses
%% into {AbsoluteStartOffset, AbsoluteEndOffset, ChunkBinary} tuples.
fetch_and_collect(Offsets, Opts) ->
    Concurrency = hb_opts:get(chunk_fetch_concurrency, 10, Opts),
    Results = hb_pmap:parallel_map(
        Offsets,
        fun(O) -> get_chunk(O, Opts) end,
        Concurrency
    ),
    collect_chunks(Results).

%% @doc Generate a list of offsets from Start to End (inclusive) stepping by
%% Step bytes. Used to produce candidate query offsets at 256KiB increments.
generate_offsets(Start, End, Step) ->
    generate_offsets(Start, End, Step, []).

generate_offsets(Current, End, _Step, Acc) when Current > End ->
    Offsets = lists:reverse(Acc),
    ?event(arweave_debug, {fetch_chunk_offsets, {offsets, Offsets}}),
    Offsets;
generate_offsets(Current, End, Step, Acc) ->
    generate_offsets(Current + Step, End, Step, [Current | Acc]).

%% @doc Parse a list of chunk fetch results into chunk info tuples.
%% Fails fast on the first error.
collect_chunks(Results) ->
    collect_chunks(Results, []).

collect_chunks([], Acc) ->
    {ok, lists:reverse(Acc)};
collect_chunks([{ok, JSON} | Rest], Acc) ->
    Chunk = hb_util:decode(maps:get(<<"chunk">>, JSON)),
    AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, JSON)),
    AbsStart = AbsEnd - byte_size(Chunk) + 1,
    ?event(arweave_debug, 
        {collect_chunks,
            {abs_start, AbsStart}, 
            {abs_end, AbsEnd},
            {size, byte_size(Chunk)}}),
    collect_chunks(Rest, [{AbsStart, AbsEnd, Chunk} | Acc]);
collect_chunks([{error, Reason} | _], _Acc) ->
    {error, Reason}.

%% @doc Sort chunk infos by start offset. If duplicate starts appear, log a
%% warning since this should not happen.
sort_chunks(ChunkInfos) ->
    Sorted = lists:sort(
        fun({StartA, EndA, _}, {StartB, EndB, _}) ->
            case StartA =:= StartB of
                true ->
                    % This should never happen. Logging rather than ignoring
                    % "just in case".
                    ?event(
                        warning,
                        {duplicate_chunk_start_offset,
                            {start, StartA},
                            {left_end, EndA},
                            {right_end, EndB}
                        }
                    );
                false ->
                    ok
            end,
            StartA =< StartB
        end,
        ChunkInfos
    ),
    Sorted.

%% @doc Find byte ranges within [RangeStart, RangeEnd] not covered by any
%% chunk. Returns a list of {GapStart, GapEnd} tuples.
%% WARNING: the find_gaps logic is untested in production and may not be 
%%          needed. We have yet to find an L1 TX that is chunked in such
%%          a way as to create gaps when using our naive 256KiB chunking.
find_gaps(SortedChunks, RangeStart, RangeEnd) ->
    find_gaps(SortedChunks, RangeStart, RangeEnd, []).

find_gaps([], Pos, RangeEnd, Gaps) when Pos =< RangeEnd ->
    lists:reverse([{Pos, RangeEnd} | Gaps]);
find_gaps([], _Pos, _RangeEnd, Gaps) ->
    lists:reverse(Gaps);
find_gaps([{ChunkStart, ChunkEnd, _} | Rest], Pos, RangeEnd, Gaps) ->
    NewGaps = case ChunkStart > Pos of
        true -> [{Pos, ChunkStart - 1} | Gaps];
        false -> Gaps
    end,
    find_gaps(Rest, max(Pos, ChunkEnd + 1), RangeEnd, NewGaps).

%% @doc Assemble chunk infos into a list of contiguous binaries suitable for
%% iolist_to_binary. The first chunk is sliced if it starts before Offset.
assemble_chunks(ChunkInfos, Offset) ->
    Sorted = sort_chunks(ChunkInfos),
    Binaries = lists:map(
        fun({ChunkStart, _ChunkEnd, Data}) ->
            case ChunkStart < Offset of
                true ->
                    % The first chunk may start before the requested offset;
                    % trim the leading bytes to start exactly at Offset.
                    Skip = Offset - ChunkStart,
                    binary:part(Data, Skip, byte_size(Data) - Skip);
                false ->
                    Data
            end
        end,
        Sorted
    ),
    {ok, Binaries}.

get_chunk(Offset, Opts) ->
    % Note: it's possible that we will need to add the x-bucket-based-offset
    % header to *some* queries. When querying L1 TX chunks from after the
    % strict data split threshold, in theory that header is needed. But I
    % haven't found a TX which requires it. However, including the header
    % when querying some *dataitems* does cause an error. So for now we will
    % leaeve the header out and continue to search for a case where it is
    % needed.
    Path = <<"/chunk/", (hb_util:bin(Offset))/binary>>,
    request(<<"GET">>, Path, #{
        <<"route-by">> => Offset
    }, Opts).

%% @doc Retrieve (and cache) block information from Arweave. If the `block' key
%% is present, it is used to look up the associated block. If it is of Arweave
%% block hash length (43 characters), it is used as an ID. If it is parsable as
%% an integer, it is used as a block height. If it is not present, the current
%% block is used.
block(Base, Request, Opts) ->
    Block =
        hb_ao:get_first(
            [
                {Request, <<"block">>},
                {Base, <<"block">>}
            ],
            not_found,
            Opts
        ),
    case Block of
        <<"current">> -> current(Base, Request, Opts);
        not_found -> current(Base, Request, Opts);
        ID when ?IS_BLOCK_ID(ID) -> block({id, ID}, Opts);
        MaybeHeight ->
            try hb_util:int(MaybeHeight) of
                Int -> block({height, Int}, Opts)
            catch
                _:_ ->
                    {
                        error,
                        <<"Invalid block reference `", MaybeHeight/binary, "`">>
                    }
            end
    end.
block({id, ID}, Opts) ->
    case hb_cache:read(ID, Opts) of
        {ok, Block} ->
            ?event(arweave_short, {read_block_from_cache,
                {id, {explicit, ID}}
            }),
            {ok, Block};
        not_found ->
            request(<<"GET">>, <<"/block/hash/", ID/binary>>, Opts)
    end;
block({height, Height}, Opts) ->
    case dev_arweave_block_cache:read(Height, Opts) of
        {ok, Block} ->
            ?event(arweave_short, {read_block_from_cache,
                {height, Height}
            }),
            {ok, Block};
        not_found ->
            request(
                <<"GET">>,
                <<"/block/height/", (hb_util:bin(Height))/binary>>,
                #{ <<"route-by">> => Height },
                Opts
            )
    end.

%% @doc Retrieve the current block information from Arweave.
current(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/block/current">>, Opts).

price(Base, Request, Opts) ->
    Size =
        hb_ao:get_first(
            [
                {Request, <<"size">>},
                {Base, <<"size">>}
            ],
            not_found,
            Opts
        ),
    case Size of
        not_found ->
            {error, not_found};
        _ ->
            request(<<"GET">>, <<"/price/", (hb_util:bin(Size))/binary>>, Opts)
    end.

tx_anchor(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/tx_anchor">>, Opts).

%%% Internal Functions

%% @doc Find the transaction ID to retrieve from Arweave based on the request or
%% base message.
find_txid(Base, Request, Opts) ->
    hb_ao:get_first(
        [
            {Request, <<"tx">>},
            {Base, <<"tx">>}
        ],
        not_found,
        Opts
    ).

exclude_data(Base, Request, Opts) ->
    RawValue =
        hb_ao:get_first(
            [
                {Request, <<"exclude-data">>},
                {Base, <<"exclude-data">>}
            ],
            false,
            Opts
        ),
    hb_util:bool(RawValue).

%% @doc Make a request to the Arweave node and parse the response into an
%% AO-Core message. Most Arweave API responses are in JSON format, but without
%% a `content-type' header. Subsequently, we parse the response manually and
%% pass it back as a message.
request(Method, Path, Opts) ->
    request(Method, Path, #{}, [], Opts).
request(Method, Path, Extra, Opts) ->
    request(Method, Path, Extra, [], Opts).
request(Method, Path, Extra, LogExtra, Opts) ->
    ?event(arweave_debug, {request,
        {method, Method}, {path, Path}, {log_extra, LogExtra}}),
    Res =
        hb_http:request(
            Extra#{
                <<"path">> => <<"/arweave", Path/binary>>,
                <<"method">> => Method
            },
            Opts#{
                cache_control => [<<"no-cache">>, <<"no-store">>]
            }
        ),
    to_message(Path, Method, best_response(Res), LogExtra, Opts).

%% @doc Select the best response from a list of responses by sorting them
%% ascending by HTTP status code. Returns the first (best) response tuple.
best_response({error, {no_viable_responses, Responses}}) ->
    best_response(Responses);
best_response([]) ->
    {error, no_viable_responses};
best_response(Responses) when is_list(Responses) ->
    Sorted = lists:sort(
        fun({_, ResponseA}, {_, ResponseB}) ->
            StatusA = response_status(ResponseA),
            StatusB = response_status(ResponseB),
            StatusA =< StatusB
        end,
        Responses
    ),
    hd(Sorted);
best_response(Response) ->
    Response.

response_status(Response) when is_map(Response) ->
    maps:get(<<"status">>, Response, 999);
response_status(_Response) ->
    999.

%% @doc Transform a response from the Arweave node into an AO-Core message.
to_message(Path, Method, {error, #{ <<"status">> := 404 }}, LogExtra, _Opts) ->
    event_request(Path, Method, 404, LogExtra),
    {error, not_found};
to_message(Path, Method, {error, Response}, LogExtra, _Opts) when is_map(Response) ->
    Status = maps:get(<<"status">>, Response, client_error),
    event_request(Path, Method, Status, LogExtra),
    {error, Response};
to_message(Path, Method, {error, Response}, LogExtra, _Opts) ->
    event_request(Path, Method, client_error, LogExtra),
    {error, Response};
to_message(Path, Method, {failure, Response}, LogExtra, _Opts) when is_map(Response) ->
    Status = maps:get(<<"status">>, Response, server_error),
    event_request(Path, Method, Status, LogExtra),
    {error, server_error};
to_message(Path, Method, {failure, _Response}, LogExtra, _Opts) ->
    event_request(Path, Method, server_error, LogExtra),
    {error, server_error};
to_message(Path = <<"/tx">>, <<"POST">>, {ok, Response}, LogExtra, _Opts) ->
    Status = maps:get(<<"status">>, Response, 200),
    event_request(Path, <<"POST">>, Status, LogExtra),
    {ok, Response};
to_message(Path = <<"/tx/", TXID/binary>>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    TXHeader = ar_tx:json_struct_to_tx(hb_json:decode(Body)),
    ?event(arweave_debug,
        {arweave_tx_response,
            {path, {explicit, Path}},
            {raw_body, {explicit, Body}},
            {body, {explicit, hb_json:decode(Body)}},
            {tx, TXHeader}
        }
    ),
    case hb_opts:get(exclude_data, false, Opts) of
        true ->
            {ok, hb_message:convert(TXHeader, <<"structured@1.0">>, <<"tx@1.0">>, Opts)};
        false ->
            case data(TXID, Opts) of
                {ok, RawData} ->
                    TX = TXHeader#tx{ data = RawData },
                    {ok, hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts)};
                {error, not_found} ->
                    {ok, hb_message:convert(TXHeader, <<"structured@1.0">>, <<"tx@1.0">>, Opts)};
                Error ->
                    Error
            end
    end;
to_message(Path = <<"/raw/", _/binary>>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, _Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    {ok, Body};
to_message(Path = <<"/block/", _/binary>>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    {ok, Block} =
        dev_codec_json:from(
            Body,
            #{ <<"accept-codec">> => <<"structured@1.0">> },
            Opts
        ),
    CacheRes =
        case hb_opts:get(arweave_index_blocks, true, Opts) of
            true -> dev_arweave_block_cache:write(Block, Opts);
            false -> skipped
        end,
    ?event(
        debug_arweave_index,
        {
            if CacheRes == skipped -> skipped_caching_arweave_block;
            true -> cached_arweave_block
            end,
            {path, Path},
            {result, CacheRes}
        }
    ),
    {ok, Block};
to_message(Path = <<"/price/", _/binary>>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, _Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    Price = hb_util:int(Body),
    {ok, Price};
to_message(Path = <<"/tx_anchor">>, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, _Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    Anchor = hb_util:decode(Body),
    {ok, Anchor};
to_message(Path, <<"GET">>, {ok, #{ <<"body">> := Body }}, LogExtra, Opts) ->
    event_request(Path, <<"GET">>, 200, LogExtra),
    % All other responses that are `OK' status are converted from JSON to an
    % AO-Core message.
    ?event(
        {arweave_json_response,
            {path, Path},
            {body_size, byte_size(Body)}
        }
    ),
    {
        ok,
        hb_message:convert(
            Body,
            <<"structured@1.0">>,
            <<"json@1.0">>,
            Opts
        )
    }.

event_request(Path, Method, Status, Extra) ->
    BaseList = [{request, {explicit, Path}}, {method, Method}, {status, Status}],
    MergedTuple = erlang:list_to_tuple(BaseList ++ Extra),
    ?event(arweave_short, MergedTuple).

%%% Tests

post_ans104_message_test() ->
    ServerOpts = #{ store => [hb_test_utils:test_store()] },
    Server = hb_http_server:start_node(ServerOpts),
    ClientOpts =
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => hb:wallet()
        },
    Msg =
        hb_message:commit(
            #{
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Process">>,
                <<"data">> => <<"test-data">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    {ok, PostRes} =
        hb_http:post(
            Server,
            Msg#{
                <<"path">> => <<"/~arweave@2.9/tx">>
            },
            ClientOpts
        ),
    ?assertMatch(#{ <<"status">> := 200 }, PostRes),
    ?event(debug_test, {post_res, PostRes}),
    SignedID = hb_message:id(Msg, signed, ClientOpts),
    {ok, GetRes} =
        hb_http:get(
            Server, <<"/", SignedID/binary>>,
            ClientOpts
        ),
    ?assertMatch(
        #{
            <<"status">> := 200,
            <<"variant">> := <<"ao.N.1">>,
            <<"type">> := <<"Process">>,
            <<"data">> := <<"test-data">>
        },
        GetRes
    ),
    ok.

post_ans104_binary_test() ->
    ServerOpts = #{ store => [hb_test_utils:test_store()] },
    Server = hb_http_server:start_node(ServerOpts),
    ClientOpts =
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => hb:wallet()
        },
    Msg =
        hb_message:commit(
            #{
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Process">>,
                <<"data">> => <<"test-data">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    DataItem = hb_message:convert(Msg, <<"ans104@1.0">>, <<"structured@1.0">>, ClientOpts),
    ?event(debug_test, {data_item, DataItem}),
    Serialized = ar_bundles:serialize(DataItem),
    {ok, PostRes} =
        hb_http:post(
            Server,
            #{
                <<"device">> => <<"arweave@2.9">>,
                <<"path">> => <<"/tx?codec-device=ans104@1.0">>,
                <<"content-type">> => <<"application/octet-stream">>,
                <<"body">> => Serialized
            },
            ClientOpts
        ),
    ?assertMatch(#{ <<"status">> := 200 }, PostRes),
    ?event(debug_test, {post_res, PostRes}),
    SignedID = hb_message:id(Msg, signed, ClientOpts),
    {ok, GetRes} =
        hb_http:get(
            Server, <<"/", SignedID/binary>>,
            ClientOpts
        ),
    ?assertMatch(
        #{
            <<"status">> := 200,
            <<"variant">> := <<"ao.N.1">>,
            <<"type">> := <<"Process">>,
            <<"data">> := <<"test-data">>
        },
        GetRes
    ),
    ok.

post_tx_message_test() ->
    ServerOpts = #{ store => [hb_test_utils:test_store()] },
    Server = hb_http_server:start_node(ServerOpts),
    ClientOpts =
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => hb:wallet()
        },
    Msg =
        hb_message:commit(
            #{
                <<"tag">> => <<"value">>,
                <<"data">> => <<"test-data">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    ?event(debug_test, {msg, Msg}),
    Response =
        hb_http:post(
            Server,
            Msg#{
                <<"device">> => <<"arweave@2.9">>,
                <<"path">> => <<"/tx">>
            },
            ClientOpts
        ),
    ?event(debug_test, {post_response, Response}),
    % The transaction is invalid because it has insufficient balance, only
    % way we'll know that is if the HB node successfully posted the tx to
    % an arweave node.
    ?assertMatch({error, #{ <<"status">> := 400 }}, Response),
    {error, #{ <<"body">> := Body }} = Response,
    ?assertEqual(<<"Transaction verification failed.">>, Body),
    ok.

post_tx_json_failure_test() ->
    ServerOpts = #{ store => [hb_test_utils:test_store()] },
    Server = hb_http_server:start_node(ServerOpts),
    ClientOpts = post_tx_json_client_opts(),
    Response = post_tx_json_request(Server, ClientOpts),
    % The transaction is invalid because it has insufficient balance, only
    % way we'll know that is if the HB node successfully posted the tx to
    % an arweave node.
    ?assertMatch({error, #{ <<"status">> := 400 }}, Response),
    {error, #{ <<"body">> := Body }} = Response,
    ?assertEqual(<<"Transaction verification failed.">>, Body),
    ok.

post_tx_json_success_test() ->
    {Response, Node1Posts, Node2Posts} =
        post_tx_json_two_node_test({200, <<"OK-1">>}, {200, <<"OK-2">>}),
    ?assertMatch({ok, #{ <<"status">> := 200 }}, Response),
    ?assertEqual(1, length(Node1Posts)),
    ?assertEqual(1, length(Node2Posts)),
    ok.

post_tx_json_mixed_status_prefers_success_test() ->
    {Response, Node1Posts, Node2Posts} =
        post_tx_json_two_node_test(
            {400, <<"Transaction verification failed.">>},
            {200, <<"OK-2">>}
        ),
    ?assertMatch({ok, #{ <<"status">> := 200 }}, Response),
    ?assertEqual(1, length(Node1Posts)),
    ?assertEqual(1, length(Node2Posts)),
    ok.

best_response_handles_failed_connect_entries_test() ->
    FailedConnect =
        {failed_connect,
            [
                {to_address, {"tip-4.arweave.xyz", 1984}},
                {inet, [inet], etimedout}
            ]
        },
    Responses = [
        {error, FailedConnect},
        {ok, #{ <<"status">> => 200, <<"body">> => <<"OK-2">> }}
    ],
    ?assertEqual(
        {ok, #{ <<"status">> => 200, <<"body">> => <<"OK-2">> }},
        best_response(Responses)
    ).

best_response_non_map_error_round_trips_test() ->
    FailedConnect =
        {failed_connect,
            [
                {to_address, {"tip-4.arweave.xyz", 1984}},
                {inet, [inet], etimedout}
            ]
        },
    ?assertEqual(
        {error, FailedConnect},
        to_message(<<"/tx">>, <<"GET">>, {error, FailedConnect}, [], #{})
    ).

post_tx_json_two_node_test(Node1TxResponse, Node2TxResponse) ->
    {ok, MockNode1, MockHandle1} = hb_mock_server:start([
        {"/tx", tx, Node1TxResponse}
    ]),
    {ok, MockNode2, MockHandle2} = hb_mock_server:start([
        {"/tx", tx, Node2TxResponse}
    ]),
    Server = hb_http_server:start_node(
        post_tx_json_two_node_server_opts(MockNode1, MockNode2)
    ),
    ClientOpts = post_tx_json_client_opts(),
    try
        Response = post_tx_json_request(Server, ClientOpts),
        Node1Posts = hb_mock_server:get_requests(tx, 1, MockHandle1),
        Node2Posts = hb_mock_server:get_requests(tx, 1, MockHandle2),
        {Response, Node1Posts, Node2Posts}
    after
        hb_mock_server:stop(MockHandle1),
        hb_mock_server:stop(MockHandle2)
    end.

post_tx_json_two_node_server_opts(MockNode1, MockNode2) ->
    #{
        store => [hb_test_utils:test_store()],
        routes => [
            #{
                <<"template">> =>
                    #{
                        <<"path">> => <<"^/arweave/tx">>,
                        <<"method">> => <<"POST">>
                    },
                <<"nodes">> =>
                    [
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"with">> => MockNode1,
                            <<"opts">> => #{ http_client => httpc }
                        },
                        #{
                            <<"match">> => <<"^/arweave">>,
                            <<"with">> => MockNode2,
                            <<"opts">> => #{ http_client => httpc }
                        }
                    ],
                <<"parallel">> => true,
                <<"responses">> => 2,
                <<"stop-after">> => false,
                <<"admissible-status">> => 200
            }
        ]
    }.

post_tx_json_client_opts() ->
    #{
        store => [hb_test_utils:test_store()],
        priv_wallet => hb:wallet()
    }.

post_tx_json_payload(ClientOpts) ->
    Msg =
        hb_message:commit(
            #{
                <<"tag">> => <<"value">>,
                <<"data">> => <<"test-data">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"tx@1.0">> }
        ),
    TX = hb_message:convert(Msg, <<"tx@1.0">>, <<"structured@1.0">>, ClientOpts),
    JSON = ar_tx:tx_to_json_struct(TX#tx{ data = <<>> }),
    hb_json:encode(JSON).

post_tx_json_request(Server, ClientOpts) ->
    Serialized = post_tx_json_payload(ClientOpts),
    hb_http:post(
        Server,
        #{
            <<"device">> => <<"arweave@2.9">>,
            <<"path">> => <<"/tx?codec-device=tx@1.0">>,
            <<"content-type">> => <<"application/json">>,
            <<"body">> => Serialized
        },
        ClientOpts
    ).

get_tx_basic_data_test() ->
    {ok, Structured} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
            <<"exclude-data">> => false
        },
        #{}
    ),
    ?event(debug_test, {structured_tx, Structured}),
    ?assert(hb_message:verify(Structured, all, #{})),
    % Hash the data to make it easier to match
    StructuredWithHash = Structured#{
        <<"data">> => hb_util:encode(
            crypto:hash(sha256, (maps:get(<<"data">>, Structured)))
        )
    },
    ExpectedMsg = #{
        <<"data">> => <<"PEShWA1ER2jq7CatAPpOZ30TeLrjOSpaf_Po7_hKPo4">>,
        <<"reward">> => <<"482143296">>,
        <<"anchor">> => <<"XTzaU2_m_hRYDLiXkcleOC4zf5MVTXIeFWBOsJSRrtEZ8kM6Oz7EKLhZY7fTAvKq">>,
        <<"content-type">> => <<"application/json">>
    },
    ?assert(hb_message:match(ExpectedMsg, StructuredWithHash, only_present)),
    ok.

%% @doc The data for this transaction ends with two smaller chunks.
get_tx_split_chunk_test() ->
    {ok, Structured} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
            <<"exclude-data">> => false
        },
        #{}
    ),
    ?assert(hb_message:verify(Structured, all, #{})),
    ?assertEqual(
        <<"T2pluNnaavL7-S2GkO_m3pASLUqMH_XQ9IiIhZKfySs">>,
        hb_message:id(Structured, signed)),
    ExpectedMsg = #{
        <<"reward">> => <<"6035386935">>,
        <<"anchor">> => <<"PX16-598IrIMvLxFkvfNTWLVKXqXSmArOdW3o7X8jWMCH1fiNOjBZ2XjQlw0FOme">>,
        <<"Contract">> => <<"KTzTXT_ANmF84fWEKHzWURD1LWd9QaFR9yfYUwH2Lxw">>
    },
    ?assert(hb_message:match(ExpectedMsg, Structured, only_present)),

    Child = hb_ao:get(<<"1/2">>, Structured),
    ?assert(hb_message:verify(Child, all, #{})),
    ?event(debug_test, {child, {explicit, hb_message:id(Child, signed)}}),
    ?assertEqual(
        <<"8aJrRWtHcJvJ61qsH6agGkemzrtLw3W22xFrpCGAnTM">>,
        hb_message:id(Child, signed)),
    ok.

get_tx_basic_data_exclude_data_test() ->
    {ok, Structured} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>,
            <<"exclude-data">> => true
        },
        #{}
    ),
    ?event(debug_test, {structured_tx, Structured}),
    ?assert(hb_message:verify(Structured, all, #{})),
    ?assertEqual(false, maps:is_key(<<"data">>, Structured)),
    ExpectedMsg = #{
        <<"reward">> => <<"482143296">>,
        <<"anchor">> => <<"XTzaU2_m_hRYDLiXkcleOC4zf5MVTXIeFWBOsJSRrtEZ8kM6Oz7EKLhZY7fTAvKq">>,
        <<"content-type">> => <<"application/json">>
    },
    ?assert(hb_message:match(ExpectedMsg, Structured, only_present)),
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"raw">>,
            <<"tx">> => <<"ptBC0UwDmrUTBQX3MqZ1lB57ex20ygwzkjjCrQjIx3o">>
        },
        #{}
    ),
    StructuredWithData = Structured#{ <<"data">> => Data },
    ?assert(hb_message:verify(StructuredWithData, all, #{})),
    DataHash = hb_util:encode(crypto:hash(sha256, Data)),
    ?assertEqual(<<"PEShWA1ER2jq7CatAPpOZ30TeLrjOSpaf_Po7_hKPo4">>, DataHash),
    ok.

get_tx_data_tag_exclude_data_test() ->
    {ok, Structured} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => <<"jI0A4BASHaUdCCsdv249BxDX6IlE0Ko391TuI6REATw">>,
            <<"exclude-data">> => true
        },
        #{}
    ),
    ?event(debug_test, {structured_tx, Structured}),
    ?assert(hb_message:verify(Structured, all, #{})),
    ?assertEqual(false, maps:is_key(<<"data">>, Structured)),
    ExpectedMsg = #{
        <<"reward">> => <<"630923958">>,
        <<"anchor">> => <<"CWJKkpdXEQO9sCWLFg8Cqby0d7wY0Gez5H95YG15g8pAYaXVatF9Ms1QBUpvZ-Ll">>,
        <<"content-type">> => <<"application/json">>
    },
    ?assert(hb_message:match(ExpectedMsg, Structured, only_present)),
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"raw">>,
            <<"tx">> => <<"jI0A4BASHaUdCCsdv249BxDX6IlE0Ko391TuI6REATw">>
        },
        #{}
    ),
    StructuredWithData = Structured#{ <<"data">> => Data },
    ?assert(hb_message:verify(StructuredWithData, all, #{})),
    DataHash = hb_util:encode(crypto:hash(sha256, Data)),
    ?assertEqual(<<"IHyJ9BlQaHLWVwwklMwV1XEYXGjwx2B6HXNJZ4yJXeQ">>, DataHash),
    ok.

get_tx_rsa_nested_bundle_test() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9/tx=bndIwac23-s0K11TLC1N7z472sLGAkiOdhds87ZywoE">>,
    {ok, Root} = hb_http:get(Node, Path, #{}),
    ?event(debug_test, {root, Root}),
    ?assert(hb_message:verify(Root, all, #{})),
    ChildPath = <<Path/binary, "/1/2">>,
    {ok, Child} = hb_http:get(Node, ChildPath, #{}),
    ?event(debug_test, {child, Child}),
    ?assert(hb_message:verify(Child, all, #{})),
    {ok, ExpectedChild} =
        hb_ao:resolve(
            Root,
            <<"1/2">>,
            #{}
        ),
    ?assert(hb_message:match(ExpectedChild, Child, only_present)),
    ManualChild = #{
        <<"data">> => <<"{\"totalTickedRewardsDistributed\":0,\"distributedEpochIndexes\":[],\"newDemandFactors\":[],\"newEpochIndexes\":[],\"tickedRewardDistributions\":[],\"newPruneGatewaysResults\":[{\"delegateStakeReturned\":0,\"stakeSlashed\":0,\"gatewayStakeReturned\":0,\"delegateStakeWithdrawing\":0,\"prunedGateways\":[],\"slashedGateways\":[],\"gatewayStakeWithdrawing\":0}]}">>,
        <<"data-protocol">> => <<"ao">>,
        <<"from-module">> => <<"cbn0KKrBZH7hdNkNokuXLtGryrWM--PjSTBqIzw9Kkk">>,
        <<"from-process">> => <<"agYcCFJtrMG6cqMuZfskIkFTGvUPddICmtQSBIoPdiA">>,
        <<"anchor">> => <<"MDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAyODAxODg">>,
        <<"reference">> => <<"280188">>,
        <<"target">> => <<"1R5QEtX53Z_RRQJwzFWf40oXiPW2FibErT_h02pu8MU">>,
        <<"type">> => <<"Message">>,
        <<"variant">> => <<"ao.TN.1">>
    },
    ?assert(hb_message:match(ManualChild, Child, only_present)),
    ok.

%% @TODO: This test is disabled because it takes too long to run. Re-enable
%% once some performance optimizations are implemented.
get_tx_rsa_large_bundle_test_disabled() ->
    {timeout, 300, fun() ->
        Node = hb_http_server:start_node(),
        Path = <<"/~arweave@2.9/tx=VifINXnMxLwJXOjHG5uM0JssiylR8qvajjj7HlzQvZA">>,
        {ok, Root} = hb_http:get(Node, Path, #{}),
        ?event(debug_test, {root, Root}),
        ?assert(hb_message:verify(Root, all, #{})),
        ok
    end}.

get_bad_tx_test() ->
    Node = hb_http_server:start_node(),
    Path = <<"/~arweave@2.9/tx=INVALID-ID">>,
    Res = hb_http:get(Node, Path, #{}),
    ?assertEqual({error, not_found}, Res).

%% @doc: helper test to generate and write a dataitem to disk so that we
%% can validate it using 3rd-party js libraries and gateways.
serialize_data_item_test_disabled() ->
    DataItem = ar_bundles:sign_item(
        #tx{
            data = <<"Hello from HyperBEAM test!">>,
            tags = [
                {<<"content-type">>, <<"text/plain">>},
                {<<"test-tag">>, <<"test-value">>},
                {<<"app-name">>, <<"HyperBEAM">>}
            ]
        },
        hb:wallet()
    ),
    SerializedItem = ar_bundles:serialize(DataItem),
    % Write to disk in the test directory
    OutputPath = filename:join([
        "test",
        "arbundles.js",
        "hyperbeam-test-item.bin"
    ]),
    ok = filelib:ensure_dir(OutputPath),
    ok = file:write_file(OutputPath, SerializedItem),
    ?event({wrote_data_item, {path, OutputPath}, {size, byte_size(SerializedItem)}}),
    ?assert(filelib:is_file(OutputPath)),
    % Read it back and verify it deserializes correctly
    {ok, ReadData} = file:read_file(OutputPath),
    VerifiedItem = ar_bundles:deserialize(ReadData),
    ?assertEqual(DataItem#tx.data, VerifiedItem#tx.data),
    ?assertEqual(length(DataItem#tx.tags), length(VerifiedItem#tx.tags)),
    ?assert(ar_bundles:verify_item(VerifiedItem)),
    ok.

get_partial_chunk_post_split_test() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    %% 
    Offset = 378092137521399,
    ExpectedLength = 1000,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"G62E7qonT1RBmkC6e3pNJz_thpS9xkVD3qTJAk6o3Uc">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_full_chunk_post_split_test() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    %% 
    Offset = 378092137521399,
    ExpectedLength = ?DATA_CHUNK_SIZE,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"LyTBdUe0rNmpqt8C-p7HksdiredXaa0wCBAPt3504W0">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_multi_chunk_post_split_test() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    %% 
    Offset = 378092137521399,
    ExpectedLength = ?DATA_CHUNK_SIZE * 3,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"4Cb_N0z0tMDwCiWrUbuzktfn-H6NLHT1btXGDo3CByI">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.


%% @doc Query a chunk range that starts and ends in the middle of a chunk.
get_mid_chunk_post_split_test() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    %% 
    Offset = 378092137521399 + 200_000,
    ExpectedLength = ?DATA_CHUNK_SIZE + 300_000,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"xkEZpGqDiCVuVZfGVyscmfYNZqYmgBLjOrMD2P_SfWs">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_partial_chunk_pre_split_test() ->
    %% https://arweave.net/tx/v4ophPvV-cNp5gkpkjMuUZ-lf-fBfm1Wk-pB4vJb00E/offset
    %% 
    Offset = 30575701172109,
    ExpectedLength = 1000,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"yU5tZyDCTZ4MFcT6lng74tvx1oIbPkpCw1VAJsSqeuo">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_full_chunk_pre_split_test() ->
    %% https://arweave.net/tx/v4ophPvV-cNp5gkpkjMuUZ-lf-fBfm1Wk-pB4vJb00E/offset
    %% 
    Offset = 30575701172109,
    ExpectedLength = ?DATA_CHUNK_SIZE,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"nVCvjEq9T5nxIR6jvglNbX1_CYCg0WifxfQoXhS4gik">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_multi_chunk_pre_split_test() ->
    %% https://arweave.net/tx/v4ophPvV-cNp5gkpkjMuUZ-lf-fBfm1Wk-pB4vJb00E/offset
    %% 
    Offset = 30575701172109,
    ExpectedLength = ?DATA_CHUNK_SIZE * 3,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"DfS3jtLXqG3zO_IFA3P-r55SUBoeJmeIh4Eim2Rldeo">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_mid_chunk_pre_split_test() ->
    %% https://arweave.net/tx/v4ophPvV-cNp5gkpkjMuUZ-lf-fBfm1Wk-pB4vJb00E/offset
    %% 
    Offset = 30575701172109 + 200_000,
    ExpectedLength = ?DATA_CHUNK_SIZE + 300_000,
    Opts = #{},
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    ?assertEqual(
        <<"mgSfqsNapn_BXpbnIHtdeu3rQyvrjBaS0c7rEbUbtBU">>,
        hb_util:encode(crypto:hash(sha256, Data))
    ),
    ok.

get_pre_split_small_chunks_test() ->
    assert_chunk_range(
        <<"4FnBmvgWmqXWEEprjVqBsV5aRpAgF6_yJX_GTGsSZjY">>,
        11_741_031_646_397,
        810774,
        <<"LJbiKv5gT2Y5XKFFPF6WqYAdOtaZAvHmtCkfCTbP43g">>
    ).

get_post_split_small_chunks_test() ->
    assert_chunk_range(
        <<"YR9m4c3CrlljCRYEWBLeoKekbAyYZRMo2Kpz61IeNp8">>,
        146_563_435_390_439,
        541937,
        <<"cR2HRQRfZP_MiC1egrdc8y8j4SAF9-ppvaIaXDq5i7s">>
    ).

%% @doc this test fails if the chunks are queried with
%% the `x-bucket-based-offset' header set. I believe it is because
%% bucket-based offset should only be used when querying an L1 TX
bucket_based_offset_test() ->
    Offset = 376836461101675,
    Length = 116247,
    ExpectedID = <<"z-oKJfhMq5qoVFrljEfiBKgumaJmCWVxNJaavR5aPE8">>,
    {ok, SerializedItem} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => Offset + 1,
            <<"length">> => Length
        },
        #{}
    ),
    Item = ar_bundles:deserialize(SerializedItem),
    ?assertEqual(ExpectedID, hb_util:encode(Item#tx.id)),
    ?assert(ar_bundles:verify_item(Item)),
    ok.

% large_tx_test() ->
%     assert_chunk_range(
%         <<"GX2bvdo736wJPR1GmIkyW9GRk3JdXQ_aAd1ozX1d450">>,
%         378161418083672,
%         42040418,
%         <<"wmDVKM6nYRvqre2DdxmX_mhJ6u8unwmTD4YdmzERcZs">>
%     ).

assert_chunk_range(TXID, EndOffset, ExpectedLength, ExpectedHash) ->
    StartOffset = EndOffset - ExpectedLength,
    Opts = #{},
    T1 = erlang:monotonic_time(millisecond),
    {ok, Data} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"chunk">>,
            <<"offset">> => StartOffset+1,
            <<"length">> => ExpectedLength
        },
        Opts
    ),
    T2 = erlang:monotonic_time(millisecond),
    ?event(debug_performance, {chunk_range_resolve,
        {elapsed_ms, T2 - T1},
        {tx, TXID},
        {offset, StartOffset + 1},
        {length, ExpectedLength}
    }),
    {ok, RawData} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"raw">>,
            <<"tx">> => TXID
        },
        Opts
    ),
    ?event(debug_test, {chunk_vs_raw_comparison,
        {tx, TXID},
        {start_offset, StartOffset},
        {end_offset, EndOffset},
        {expected_length, ExpectedLength},
        {chunk_size, byte_size(Data)},
        {raw_size, byte_size(RawData)},
        {match, Data =:= RawData},
        {hash, {explicit, hb_util:encode(crypto:hash(sha256, Data))}}
    }),
    ?assertEqual(RawData, Data),
    ?assertEqual(ExpectedHash, hb_util:encode(crypto:hash(sha256, Data))),
    {ok, TXHeader} = hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => TXID,
            <<"exclude-data">> => true
        },
        Opts
    ),
    ?assertEqual(false, maps:is_key(<<"data">>, TXHeader)),
    ?assert(hb_message:verify(TXHeader, all, Opts)),
    TXWithData = TXHeader#{ <<"data">> => Data },
    ?assert(hb_message:verify(TXWithData, all, Opts)),
    ok.
