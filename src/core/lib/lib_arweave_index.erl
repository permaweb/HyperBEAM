%%% @doc The Arweave data indexer: a parallel sequential scan of unpacked
%%% storage modules that emits the published-index rows.
%%%
%%% `run/1' walks one storage module's manifest of L1 transaction boundaries
%%% (`lib_arweave_index_manifest'), splits it into byte-balanced spans, and
%%% scans the spans concurrently -- each worker streaming the module's chunk
%%% files (`lib_arweave_index_read'), parsing the ANS-104 headers in them
%%% (`lib_arweave_index_scan'), and spilling sorted runs of the fixed-width
%%% index items (`lib_arweave_index_runs'). `merge/1' folds the runs into
%%% one ascending item file per index kind, which is the shape the published
%%% containers are built from by appends.
%%%
%%% Configuration, through the same node options the storage layer reads:
%%%
%%% <ul>
%%%   <li>`arweave-data-dir': the Arweave data directory.</li>
%%%   <li>`arweave-index-module': the storage module directory name to scan;
%%%       defaults to the first unpacked module the data directory holds.</li>
%%%   <li>`arweave-index-manifest': the boundary manifest's path.</li>
%%%   <li>`arweave-index-output': where runs, merged item files and reports
%%%       land. Default `arweave-index-out'.</li>
%%%   <li>`arweave-index-workers': concurrent scan spans. Default 4: right
%%%       for NVMe; spinning disks want 1 or 2.</li>
%%%   <li>`arweave-index-from', `arweave-index-to': absolute weave offsets
%%%       clipping the scan; default the module's own range.</li>
%%%   <li>`arweave-index-read-size', `arweave-index-run-rows': the reader's
%%%       batch and the spill threshold.</li>
%%% </ul>
%%%
%%% For example, from a stock checkout:
%%%
%%% ```
%%% rebar3 shell --eval '
%%%     {ok, Report} = lib_arweave_index:run(#{
%%%         <<"arweave-data-dir">> => <<"/mnt/arweave-weave/data">>,
%%%         <<"arweave-index-manifest">> => <<"manifest.aimf">>,
%%%         <<"arweave-index-workers">> => 8
%%%     }).'
%%% '''
-module(lib_arweave_index).
-export([run/1, merge/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Scan a module and spill sorted runs, returning a report of bytes
%% moved, rows emitted, and the scan's counters.
run(Opts) ->
    maybe
        {ok, Module} ?= module(Opts),
        {RangeStart, RangeEnd} = lib_arweave_storage:range(Module),
        From =
            hb_util:int(
                hb_opts:get(<<"arweave-index-from">>, RangeStart, Opts)),
        To = hb_util:int(hb_opts:get(<<"arweave-index-to">>, RangeEnd, Opts)),
        {ok, Txs} ?= manifest(From, To, Opts),
        Workers = workers(Opts),
        Spans = spans(Txs, Workers),
        ?event(arweave_index,
            {scan_starting,
                {module, hb_util:bin(lib_arweave_storage:id(Module))},
                {txs, length(Txs)},
                {spans, length(Spans)},
                {from, From},
                {to, To}
            }
        ),
        Started = erlang:monotonic_time(millisecond),
        Results =
            hb_pmap:parallel_map(
                lists:enumerate(Spans),
                fun({N, Span}) -> span(Module, N, Span, Opts) end,
                Workers
            ),
        Wall = erlang:monotonic_time(millisecond) - Started,
        {ok, report(Results, From, To, Wall)}
    end.

%% @doc Merge every spilled run in the output directory into one ascending
%% item file per index kind.
merge(Opts) ->
    Started = erlang:monotonic_time(millisecond),
    Merged =
        [
            merged_kind(Kind, Opts)
        ||
            Kind <- [<<"offset">>, <<"match">>]
        ],
    Wall = erlang:monotonic_time(millisecond) - Started,
    Report =
        #{
            <<"merged">> =>
                maps:from_list(
                    [{Kind, Items} || {Kind, Items, _Path} <- Merged]),
            <<"paths">> =>
                [hb_util:bin(Path) || {_Kind, _Items, Path} <- Merged],
            <<"wall-ms">> => Wall
        },
    ?event(arweave_index, {merge_complete, Report}),
    {ok, Report}.

%%% Internal functions.

%% @doc Merge one kind's runs into its item file.
merged_kind(Kind, Opts) ->
    Dir = runs_dir(Opts),
    Runs =
        [
            filename:join(Dir, Name)
        ||
            Name <-
                lists:sort(
                    filelib:wildcard(
                        hb_util:list(<< "*-", Kind/binary, "-*.run" >>),
                        hb_util:list(Dir)
                    )
                )
        ],
    Out = filename:join(out_dir(Opts), << Kind/binary, ".items" >>),
    {Kind, lib_arweave_index_runs:merge(Kind, Runs, Out), Out}.

%% @doc The storage module to scan: the one named by `arweave-index-module',
%% or the first unpacked module the data directory holds.
module(Opts) ->
    case hb_opts:get(<<"arweave-index-module">>, not_found, Opts) of
        not_found ->
            unpacked(lib_arweave_storage:discovered(Opts));
        StoreID ->
            case lib_arweave_storage:parse_id(StoreID) of
                not_found -> {error, <<"module-id-invalid">>};
                Module -> {ok, Module}
            end
    end.

%% @doc The first unpacked module of a discovered set.
unpacked(Modules) ->
    case [M || M <- Modules, lib_arweave_storage:packing(M) == unpacked] of
        [] -> {error, <<"no-unpacked-module">>};
        [Module | _Rest] -> {ok, Module}
    end.

%% @doc Load the boundary manifest for the scanned range.
manifest(From, To, Opts) ->
    case hb_opts:get(<<"arweave-index-manifest">>, not_found, Opts) of
        not_found -> {error, <<"manifest-required">>};
        Path -> lib_arweave_index_manifest:load(Path, From, To)
    end.

%% @doc How many spans scan concurrently.
workers(Opts) ->
    hb_util:int(hb_opts:get(<<"arweave-index-workers">>, 4, Opts)).

%% @doc Split the transactions into contiguous spans of roughly equal data
%% size, one per worker. Contiguity keeps each worker's reads sequential.
spans([], _Workers) ->
    [];
spans(Txs, Workers) ->
    Total = lists:sum([maps:get(<<"size">>, Tx) || Tx <- Txs]),
    Quota = max(1, Total div Workers),
    split(Txs, Quota, 0, [], []).

split([], _Quota, _Fill, Span, Spans) ->
    lists:reverse(closed(Span, Spans));
split([Tx | Txs], Quota, Fill, Span, Spans) ->
    Size = maps:get(<<"size">>, Tx),
    case Fill + Size >= Quota andalso Span /= [] of
        true -> split(Txs, Quota, Size, [Tx], closed(Span, Spans));
        false -> split(Txs, Quota, Fill + Size, [Tx | Span], Spans)
    end.

closed([], Spans) -> Spans;
closed(Span, Spans) -> [lists:reverse(Span) | Spans].

%% @doc Scan one span of transactions: one reader, one sink, one pass.
span(Module, N, Txs, Opts) ->
    Reader = lib_arweave_index_read:open(Module, Opts),
    Sink =
        lib_arweave_index_runs:open(
            runs_dir(Opts),
            hb_util:bin(io_lib:format("w~3..0B", [N])),
            Opts
        ),
    Started = erlang:monotonic_time(millisecond),
    State =
        lists:foldl(
            fun(Tx, Acc) ->
                case lib_arweave_index_scan:tx(Tx, Acc) of
                    {ok, Acc2} -> Acc2;
                    {error, Reason} -> erlang:error({'span-failed', N, Reason})
                end
            end,
            lib_arweave_index_scan:open(
                Reader, fun lib_arweave_index_runs:add/3, Sink),
            Txs
        ),
    Wall = erlang:monotonic_time(millisecond) - Started,
    {SinkState, Reader2, Counts} = lib_arweave_index_scan:finish(State),
    ReaderStats = lib_arweave_index_read:stats(Reader2),
    ok = lib_arweave_index_read:close(Reader2),
    Runs = lib_arweave_index_runs:close(SinkState),
    #{
        <<"span">> => N,
        <<"wall-ms">> => Wall,
        <<"reader">> => ReaderStats,
        <<"counts">> => Counts,
        <<"runs">> => Runs
    }.

%% @doc Fold the span results into the run's report.
report(Results, From, To, Wall) ->
    BytesRead =
        lists:sum(
            [maps:get(<<"bytes-read">>, maps:get(<<"reader">>, R)) || R <- Results]),
    Counts =
        lists:foldl(
            fun(R, Acc) ->
                maps:merge_with(
                    fun(_Key, A, B) -> A + B end, Acc, maps:get(<<"counts">>, R))
            end,
            #{},
            Results
        ),
    Rows =
        lists:foldl(
            fun(R, Acc) ->
                maps:merge_with(
                    fun(_Key, A, B) -> A + B end,
                    Acc,
                    maps:get(<<"rows">>, maps:get(<<"runs">>, R))
                )
            end,
            #{},
            Results
        ),
    Report =
        #{
            <<"from">> => From,
            <<"to">> => To,
            <<"weave-bytes">> => To - From,
            <<"bytes-read">> => BytesRead,
            <<"wall-ms">> => Wall,
            <<"read-gbps">> => gbps(BytesRead, Wall),
            <<"weave-gbps">> => gbps(To - From, Wall),
            <<"counts">> => Counts,
            <<"rows">> => Rows,
            <<"spans">> => Results
        },
    ?event(arweave_index, {scan_complete, maps:without([<<"spans">>], Report)}),
    Report.

%% @doc Gigabytes per second, to two decimals, from bytes and milliseconds.
gbps(_Bytes, 0) -> 0.0;
gbps(Bytes, Millis) ->
    round(Bytes / (Millis / 1000) / 10000000) / 100.

%% @doc Where runs are spilled.
runs_dir(Opts) ->
    filename:join(out_dir(Opts), "runs").

%% @doc Where merged item files land.
out_dir(Opts) ->
    hb_util:list(
        hb_opts:get(<<"arweave-index-output">>, <<"arweave-index-out">>, Opts)).
