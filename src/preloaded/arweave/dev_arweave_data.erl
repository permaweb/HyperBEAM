%%% @doc Read the data of Arweave transactions from Arweave nodes for the
%%% `~arweave@2.9' device: byte ranges of the weave or of a pending
%%% transaction, read chunk by chunk, each chunk checked against its Merkle
%%% proof, or a whole transaction checked against its data root. Weave chunks
%%% are kept in the node's `arweave-chunk-store' (`hb_store_arweave_chunk').
-module(dev_arweave_data).
-export([get_chunk/2, get_chunk/3]).
-export([get_chunk_range_relative/4, decode_relative_chunk/4]).
-export([get_pending_chunk/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Arweave nodes serve the data of a transaction whole up to 12 MiB
%% (`MAX_SERVED_TX_DATA_SIZE').
-define(MAX_SERVED_TX_DATA_SIZE, 12 * 1024 * 1024).

%% @doc Retrieve a chunk or slice of bytes by its offset, either relative to the
%% global Arweave data tree, or relative to the start of a specific pending
%% transaction. A request that also gives the `tx' whose data the range is and
%% that transaction's `data-root' is read whole when `tx_data/2' can read it.
get_chunk(Base, Request, Opts) ->
    case tx_data(Request, Opts) of
        {ok, Data} -> {ok, Data};
        Other ->
            ?event(debug_arweave, {tx_data_not_read, {reason, Other}}),
            get_range(Base, Request, Opts)
    end.

%% @doc Read the data of a transaction of at most 12 MiB in one request, as
%% `/tx/<id>/data.bin' gives it, and keep it if it hashes to the request's
%% `data-root', which proves every byte as the proofs of its chunks do.
tx_data(Request, Opts) ->
    maybe
        {ok, TXID} ?= hb_maps:find(<<"tx">>, Request, Opts),
        {ok, DataRoot} ?= hb_maps:find(<<"data-root">>, Request, Opts),
        Offset = hb_util:int(hb_maps:get(<<"offset">>, Request, 0, Opts)),
        Length = hb_util:int(hb_maps:get(<<"length">>, Request, 0, Opts)),
        true ?= Length =< ?MAX_SERVED_TX_DATA_SIZE orelse {error, 'tx-data-too-large'},
        {ok, Data} ?=
            dev_arweave:request(
                <<"GET">>,
                <<"/tx/", TXID/binary, "/data.bin">>,
                #{ <<"route-by">> => Offset },
                Opts
            ),
        true ?=
            (byte_size(Data) =:= Length
                andalso has_data_root(Data, hb_util:decode(DataRoot)))
            orelse {error, 'invalid-tx-data'},
        {ok, Data}
    end.

%% @doc Whether the data hashes to the data root when it is split into chunks
%% as arweave-js splits it or into fixed 256 KiB chunks, the layouts that
%% clients use. Arweave also accepts other splits of the last two chunks, and
%% from the 2.7 fork rebased subtrees, which these do not reproduce.
has_data_root(Data, DataRoot) ->
    lists:any(
        fun(Mode) -> ar_tx:data_root(Mode, Data) =:= DataRoot end,
        [arweavejs, legacy]
    ).

%% @doc Read the requested range chunk by chunk.
get_range(_Base, Request, Opts) ->
    Offset = hb_util:int(hb_maps:get(<<"offset">>, Request, 0, Opts)),
    Length = hb_util:int(hb_maps:get(<<"length">>, Request, 1, Opts)),
    MaybeRelativeTXID = hb_maps:get(<<"pending">>, Request, undefined, Opts),
    case fetch_chunk_range(Offset, Length, MaybeRelativeTXID, Opts) of
        {ok, Chunks} ->
            Data = iolist_to_binary(Chunks),
            case hb_maps:is_key(<<"length">>, Request, Opts) of
                true ->
                    {ok, binary:part(Data, 0, min(Length, byte_size(Data)))};
                false ->
                    {ok, Data}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

fetch_chunk_range(Offset, Length, undefined, Opts)
        when (Offset >= ?STRICT_DATA_SPLIT_THRESHOLD) andalso
        ((Offset + Length - 1) >= ?STRICT_DATA_SPLIT_THRESHOLD) ->
    get_chunk_range_fixed_size(Offset, (Offset + Length - 1), Opts);
fetch_chunk_range(Offset, Length, undefined, Opts)
        when (Offset < ?STRICT_DATA_SPLIT_THRESHOLD) andalso
        ((Offset + Length - 1) < ?STRICT_DATA_SPLIT_THRESHOLD) ->
    get_chunk_range_variable_size(Offset, (Offset + Length - 1), Opts);
fetch_chunk_range(_Offset, _Length, undefined, _Opts) ->
    {error, chunk_range_spans_strict_data_split_threshold};
fetch_chunk_range(Offset, Length, RelativeTXID, Opts)
        when is_binary(RelativeTXID) ->
    get_chunk_range_relative(Offset, Length, RelativeTXID, Opts).

%% @doc Post-threshold: chunks occupy fixed 256KiB buckets aligned to
%% absolute weave offsets, which need not coincide with Offset. Query at
%% DATA_CHUNK_SIZE increments and let fill_gaps iteratively cover any
%% remaining holes until the range is contiguous.
get_chunk_range_fixed_size(Offset, EndOffset, Opts) ->
    hb_prometheus:observe(
        EndOffset - Offset,
        arweave_chunk_load_requested_bytes,
        []),
    Offsets = generate_offsets(Offset, EndOffset, ?DATA_CHUNK_SIZE),
    case fetch_and_collect(Offsets, Opts) of
        {ok, ChunkInfos} -> fill_gaps(ChunkInfos, Offset, EndOffset, Opts);
        Error -> Error
    end.

%% @doc Pre-threshold: chunks can be any size <= 256KiB. First pass at
%% DATA_CHUNK_SIZE increments plus one extra candidate chunk, then
%% iteratively fill gaps until contiguous.
get_chunk_range_variable_size(Offset, EndOffset, Opts) ->
    hb_prometheus:observe(
        EndOffset - Offset,
        arweave_chunk_load_requested_bytes,
        []),
    Offsets = generate_offsets(Offset, EndOffset, ?DATA_CHUNK_SIZE),
    case fetch_and_collect(Offsets, Opts) of
        {ok, ChunkInfos} -> fill_gaps(ChunkInfos, Offset, EndOffset, Opts);
        Error -> Error
    end.

%% @doc Return a chunk or range of bytes relative to a specific, unconfirmed,
%% transaction's data root.
get_chunk_range_relative(Offset, Length, RelativeTXID, Opts) ->
    case dev_arweave:pending(
        #{},
        #{ <<"pending">> => RelativeTXID, <<"exclude-data">> => true },
        Opts
    ) of
        {ok, PendingTX} ->
            DataSize = hb_util:int(maps:get(<<"data-size">>, PendingTX)),
            DataRoot = hb_util:decode(maps:get(<<"data-root">>, PendingTX)),
            hb_prometheus:observe(
                Length,
                arweave_chunk_load_requested_bytes,
                []
            ),
            Offsets = pending_relative_chunk_offsets(Offset, Length, DataSize),
            GETFun =
                fun(XOffset) ->
                    decode_relative_chunk(
                        XOffset,
                        DataRoot,
                        DataSize,
                        get_pending_chunk(RelativeTXID, XOffset, Opts)
                    )
                end,
            % A peer answers 404 for an offset that is not the end of a chunk,
            % so a chunk that does not end at one of `Offsets' is missing here.
            Concurrency =
                hb_opts:get(arweave_chunk_fetch_concurrency, 10, Opts),
            Found =
                [
                    ChunkInfo
                ||
                    {ok, ChunkInfo} <-
                        hb_pmap:parallel_map(Offsets, GETFun, Concurrency)
                ],
            case fill_relative_gaps(
                Found,
                max(1, Offset + 1),
                min(Offset + Length, DataSize),
                DataSize,
                GETFun,
                Opts
            ) of
                {ok, ChunkInfos} ->
                    {ok, Binaries} = assemble_chunks(ChunkInfos, Offset + 1),
                    {ok, iolist_to_binary(Binaries)};
                Error -> Error
            end;
        Error ->
            Error
    end.

%% @doc Decode a pending chunk response into a {Start, End, Binary} tuple,
%% taking the start and end from the chunk's proof. The proof must verify
%% against the data root under either rule set by which a node admits a
%% pending chunk, and the chunk must contain the byte at the offset it was
%% fetched for.
decode_relative_chunk(Offset, DataRoot, DataSize, {ok, JSON}) ->
    Chunk = hb_util:decode(maps:get(<<"chunk">>, JSON)),
    ChunkID = ar_tx:generate_chunk_id(Chunk),
    DataPath = hb_util:decode(maps:get(<<"data_path">>, JSON)),
    Validate =
        fun(Ruleset) ->
            ar_merkle:validate_path(
                DataRoot,
                Offset - 1,
                DataSize,
                DataPath,
                Ruleset
            )
        end,
    % A proof with rebased subtrees verifies only under the rebase rule set.
    Bounds =
        case Validate(strict_borders_ruleset) of
            false -> Validate(offset_rebase_support_ruleset);
            StrictBounds -> StrictBounds
        end,
    case Bounds of
        {ChunkID, Start, End}
                when End - Start =:= byte_size(Chunk),
                    Start < Offset, Offset =< End ->
            {ok, {Start + 1, End, Chunk}};
        _ ->
            {error, 'invalid-chunk-proof'}
    end;
decode_relative_chunk(_Offset, _DataRoot, _DataSize, {error, _} = Err) ->
    Err.

%% @doc Fetch pending chunks until they cover bytes `RangeStart' to `RangeEnd'.
%% A peer serves a pending chunk only at its end offset, which is the start of
%% the chunk after it. So the chunk that contains the last byte of a gap is
%% asked for at the start of the first chunk after the gap, or at the data
%% size if no chunk after the gap is known.
fill_relative_gaps(ChunkInfos, RangeStart, RangeEnd, DataSize, GETFun, Opts) ->
    Sorted = sort_chunks(ChunkInfos),
    case find_gaps(Sorted, RangeStart, RangeEnd) of
        [] ->
            {ok, [Info || Info = {Start, _, _} <- Sorted, Start =< RangeEnd]};
        Gaps ->
            Offsets =
                [
                    hd(
                        [Start - 1 || {Start, _, _} <- Sorted, Start > GapEnd]
                            ++ [DataSize]
                    )
                ||
                    {_GapStart, GapEnd} <- Gaps
                ],
            case fetch_and_collect(Offsets, GETFun, Opts) of
                {ok, NewInfos} ->
                    fill_relative_gaps(
                        Sorted ++ NewInfos,
                        RangeStart,
                        RangeEnd,
                        DataSize,
                        GETFun,
                        Opts
                    );
                Error -> Error
            end
    end.

%% @doc Iteratively detect gaps in coverage and fetch the chunk at the start
%% of each gap until the entire range [Offset, EndOffset] is covered.
fill_gaps(ChunkInfos, Offset, EndOffset, Opts) ->
    Sorted = sort_chunks(ChunkInfos),
    case find_gaps(Sorted, Offset, EndOffset) of
        [] ->
            assemble_chunks(Sorted, Offset);
        Gaps ->
            GapOffsets = [Start || {Start, _End} <- Gaps],
            ?event(debug_arweave,
                {fill_gaps, 
                    {offset, Offset},
                    {end_offset, EndOffset},
                    {chunks,
                        [
                            {Start, End, byte_size(Chunk)}
                        ||
                            {Start, End, Chunk} <- Sorted
                        ]
                    },
                    {gap_offsets, GapOffsets}
                }
            ),
            case fetch_and_collect(GapOffsets, Opts) of
                {ok, NewInfos} ->
                    ?event(debug_arweave, {fill_gaps, NewInfos}),
                    fill_gaps(
                        Sorted ++ NewInfos,
                        Offset, EndOffset, Opts
                    );
                Error -> Error
            end
    end.

%% @doc Read chunks at the given offsets in parallel, as
%% {AbsoluteStartOffset, AbsoluteEndOffset, ChunkBinary} tuples.
fetch_and_collect(Offsets, Opts) ->
    fetch_and_collect(
        Offsets,
        fun(Offset) -> read_chunk(Offset, Opts) end,
        Opts
    ).
fetch_and_collect(Offsets, GETFun, Opts) ->
    Concurrency = hb_opts:get(arweave_chunk_fetch_concurrency, 10, Opts),
    collect_chunks(hb_pmap:parallel_map(Offsets, GETFun, Concurrency)).

%% @doc Read the chunk that holds the byte at the offset from the node's
%% `arweave-chunk-store', or fetch it from an Arweave node and write it there.
read_chunk(Offset, Opts) ->
    case hb_store_arweave_chunk:read(Offset, Opts) of
        {error, not_found} ->
            maybe
                {ok, {Start, End, Chunk}} ?=
                    decode_chunk(Offset, get_chunk(Offset, Opts)),
                hb_store_arweave_chunk:write(End, Chunk, Opts),
                {ok, {Start, End, Chunk}}
            end;
        Read -> Read
    end.

%% @doc Generate a list of offsets from Start to End (inclusive) stepping by
%% Step bytes. Used to produce candidate query offsets at 256KiB increments.
generate_offsets(Start, End, Step) ->
    generate_offsets(Start, End, Step, []).
generate_offsets(Current, End, _Step, Acc) when Current > End ->
    Offsets = lists:reverse(Acc),
    ?event(debug_arweave, {fetch_chunk_offsets, {offsets, Offsets}}),
    Offsets;
generate_offsets(Current, End, Step, Acc) ->
    generate_offsets(Current + Step, End, Step, [Current | Acc]).

pending_relative_chunk_offsets(Offset, Length, DataSize) ->
    RangeStart = max(1, Offset + 1),
    RangeEnd = min(Offset + Length, DataSize),
    case RangeStart > RangeEnd of
        true ->
            [];
        false ->
            FirstChunk = ((RangeStart - 1) div ?DATA_CHUNK_SIZE) + 1,
            LastChunk = ((RangeEnd - 1) div ?DATA_CHUNK_SIZE) + 1,
            [min(Chunk * ?DATA_CHUNK_SIZE, DataSize) ||
                Chunk <- lists:seq(FirstChunk, LastChunk)]
    end.

%% @doc Decode a chunk response into a {Start, End, Binary} tuple, if the chunk
%% contains the byte at the offset it was fetched for.
%% Runs inside the pmap worker so raw JSON is GC'd per-worker.
decode_chunk(Offset, {ok, JSON}) ->
    AbsEnd = hb_util:int(maps:get(<<"absolute_end_offset">>, JSON)),
    {AbsStart, _AbsEnd, Chunk} = ChunkTuple = decode_chunk_tuple(JSON, AbsEnd),
    ?event(debug_arweave,
        {decode_chunk,
            {abs_start, AbsStart},
            {abs_end, AbsEnd},
            {size, byte_size(Chunk)}}),
    case AbsStart =< Offset andalso Offset =< AbsEnd of
        true -> {ok, ChunkTuple};
        false -> {error, 'chunk-not-at-offset'}
    end;
decode_chunk(_Offset, {error, _} = Err) ->
    Err.

decode_chunk_tuple(JSON, ChunkEnd) ->
    Chunk = hb_util:decode(maps:get(<<"chunk">>, JSON)),
    {ChunkEnd - byte_size(Chunk) + 1, ChunkEnd, Chunk}.

%% @doc Collect decoded chunk results. Fails fast on the first error.
collect_chunks(Results) ->
    collect_chunks(Results, []).

collect_chunks([], Acc) ->
    {ok, lists:reverse(Acc)};
collect_chunks([{ok, ChunkInfo} | Rest], Acc) ->
    collect_chunks(Rest, [ChunkInfo | Acc]);
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
                    ?event(debug_arweave, {assemble_chunks,
                        {skip, Skip},
                        {chunk_start, ChunkStart},
                        {offset, Offset},
                        {byte_size, byte_size(Data)},
                        {length, byte_size(Data) - Skip}
                    }),
                    binary:part(Data, Skip, byte_size(Data) - Skip);
                false ->
                    ?event(debug_arweave, {assemble_chunks,
                        {chunk_start, ChunkStart},
                        {offset, Offset},
                        {byte_size, byte_size(Data)}
                    }),
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
    maybe
        {ok, JSON} ?=
            dev_arweave:request(<<"GET">>, Path, #{ <<"route-by">> => Offset }, Opts),
        TXPath = hb_util:decode(maps:get(<<"tx_path">>, JSON)),
        TXEnd = ar_merkle:extract_note(TXPath),
        % The transaction proof supplies the data root and transaction bounds.
        % Its block root is not available on this read path.
        {ok, TXRoot} ?= ar_merkle:extract_root(TXPath),
        {DataRoot, TXStart, TXEnd} =
            ar_merkle:validate_path(TXRoot, TXEnd - 1, TXEnd, TXPath),
        validate_chunk(JSON, DataRoot, TXEnd - TXStart)
    end.

%% @doc Check the data path, chunk hash and size against the data root.
validate_chunk(JSON, DataRoot, DataSize) ->
    Chunk = hb_util:decode(maps:get(<<"chunk">>, JSON)),
    ChunkID = ar_tx:generate_chunk_id(Chunk),
    DataPath = hb_util:decode(maps:get(<<"data_path">>, JSON)),
    {ChunkEnd, Ruleset} = chunk_position(DataPath, 0, 0, basic_ruleset),
    case ar_merkle:validate_path(
        DataRoot,
        ChunkEnd - 1,
        DataSize,
        DataPath,
        Ruleset
    ) of
        {ChunkID, Start, End} when End - Start =:= byte_size(Chunk) ->
            {ok, JSON};
        _ -> {error, 'invalid-chunk-proof'}
    end.

%% @doc Recover the transaction-relative end offset, including subtree rebases.
chunk_position(<<_:32/binary, End:256>>, _Left, Shift, Ruleset) ->
    {Shift + End, Ruleset};
chunk_position(Path, Left, Shift, Ruleset) ->
    {Rebased, Branch} =
        case Path of
            <<0:256, Rest/binary>> -> {true, Rest};
            _ -> {false, Path}
        end,
    <<L:32/binary, _:32/binary, Note:256, Child/binary>> = Branch,
    {ok, ChildRoot} =
        ar_merkle:extract_root(
            case Child of
                <<0:256, ChildBranch/binary>> -> ChildBranch;
                _ -> Child
            end
        ),
    NextLeft =
        case ChildRoot of
            L -> Left;
            _ -> max(Left, Note)
        end,
    case Rebased of
        true ->
            chunk_position(
                Child,
                0,
                Shift + NextLeft,
                offset_rebase_support_ruleset
            );
        false -> chunk_position(Child, NextLeft, Shift, Ruleset)
    end.

%% @doc Download an unconfirmed chunk by its offset.
get_pending_chunk(TXID, Offset, Opts) ->
    dev_arweave:request(
        <<"GET">>,
        <<
            "/unconfirmed_chunk/",
            TXID/binary,
            "/",
            (hb_util:bin(Offset))/binary
        >>,
        Opts
    ).

%%% Tests

pending_relative_chunk_helpers_test_parallel() ->
    DataSize = 315127,
    ?assertEqual([?DATA_CHUNK_SIZE], pending_relative_chunk_offsets(0, 1, DataSize)),
    ?assertEqual([DataSize], pending_relative_chunk_offsets(?DATA_CHUNK_SIZE, 1, DataSize)),
    ?assertEqual([?DATA_CHUNK_SIZE, DataSize], pending_relative_chunk_offsets(0, DataSize, DataSize)).

%% @doc A range that is one whole transaction, read with its `tx' and
%% `data-root', gives the bytes that a read chunk by chunk gives, as does a
%% read with a data root that those bytes do not hash to.
tx_data_test_parallel() ->
    %% https://arweave.net/tx/QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4/offset
    TXID = <<"QL7_EnmrFtx-0wVgPr2IwaGWQT8vmPcF3R20CKMO3D4">>,
    DataRoot = <<"aHoisEr0IJ5xV2i0NjnE-vbtZOzZqI01ecyn15tojbE">>,
    Read =
        fun(Extra) ->
            hb_ao:resolve(
                #{ <<"device">> => <<"arweave@2.9">> },
                Extra#{
                    <<"path">> => <<"chunk">>,
                    <<"offset">> => 378092137521399,
                    <<"length">> => 1264430
                },
                #{}
            )
        end,
    {ok, Chunks} = Read(#{}),
    ?assertEqual(DataRoot, hb_util:encode(ar_tx:data_root(Chunks))),
    ?assertEqual(
        {ok, Chunks},
        Read(#{ <<"tx">> => TXID, <<"data-root">> => DataRoot })
    ),
    ?assertEqual(
        {ok, Chunks},
        Read(
            #{
                <<"tx">> => TXID,
                <<"data-root">> => hb_util:encode(crypto:strong_rand_bytes(32))
            }
        )
    ).
