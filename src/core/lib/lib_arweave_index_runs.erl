%%% @doc Sorted runs of fixed-width index items, and their merge.
%%%
%%% A scan worker accumulates the rows it emits in memory, sorts each batch,
%%% and spills it as a run: a flat file of fixed-width items in ascending
%%% order, `<prefix>-<kind>-<seq>.run'. Runs are the shape the published
%%% containers want -- items in `memcmp' order -- held in the cheapest form
%%% that has it, so runs from any number of workers, modules or machines
%%% concatenate under one k-way merge with no reprocessing.
%%%
%%% `merge/3' streams any number of runs into one ascending item file,
%%% dropping exact duplicates. The result feeds `container/3', which appends
%%% the items into the published LMDB `DUPSORT|DUPFIXED' container through
%%% `elmdb''s sorted-append path when the linked `elmdb' provides it, and
%%% says so when it does not: the flat item file is complete in itself, and
%%% rebuilding a container from it is one further pass.
-module(lib_arweave_index_runs).
-export([open/3, add/3, close/1, merge/3, container/3]).
-export([item_size/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% How many rows of one kind a worker holds before spilling a run.
-define(RUN_ROWS, 4194304).

%%% The byte widths of the two item kinds.
-define(OFFSET_ITEM, 21).
-define(MATCH_ITEM, 17).

%%% How many bytes of each run a merge holds in memory at a time.
-define(MERGE_BLOCK, 4194304).

%% @doc A sink over a directory: rows accumulate per kind and spill as
%% sorted runs. The spill threshold is read from `arweave-index-run-rows'.
open(Dir, Prefix, Opts) ->
    ok = filelib:ensure_path(Dir),
    #{
        <<"dir">> => Dir,
        <<"prefix">> => Prefix,
        <<"max-rows">> =>
            hb_util:int(
                hb_opts:get(<<"arweave-index-run-rows">>, ?RUN_ROWS, Opts)),
        <<"offset">> => [],
        <<"offset-count">> => 0,
        <<"match">> => [],
        <<"match-count">> => 0,
        <<"seq">> => 0,
        <<"files">> => [],
        <<"rows">> => #{}
    }.

%% @doc Take one item's rows: its offset item -- or `excluded' -- and its
%% match items. The shape `lib_arweave_index_scan' calls a sink with.
add(OffsetItem, MatchItems, Sink) ->
    Sink2 = add_offset(OffsetItem, Sink),
    add_match(MatchItems, Sink2).

%% @doc Spill what remains and report: the run files written per kind, and
%% the row counters.
close(Sink) ->
    Spilled = spill(<<"match">>, spill(<<"offset">>, Sink)),
    #{ <<"files">> := Files, <<"rows">> := Rows } = Spilled,
    #{
        <<"offset-runs">> =>
            [File || {<<"offset">>, File} <- lists:reverse(Files)],
        <<"match-runs">> =>
            [File || {<<"match">>, File} <- lists:reverse(Files)],
        <<"rows">> => Rows
    }.

%% @doc Merge sorted runs into one ascending file of unique items, returning
%% how many items it holds. Runs must all be of the given kind, whose width
%% delimits items.
merge(Kind, RunFiles, OutPath) ->
    Width = item_size(Kind),
    Sources =
        [
            Source
        ||
            File <- RunFiles,
            (Source = source(File, Width)) /= empty
        ],
    ok = filelib:ensure_path(filename:dirname(OutPath)),
    {ok, Out} = file:open(OutPath, [write, raw, binary, {delayed_write, 1048576, 2000}]),
    Heads =
        gb_sets:from_list(
            [{Head, N} || {N, {Head, _Rest}} <- lists:enumerate(Sources)]),
    Tails =
        maps:from_list(
            [{N, Rest} || {N, {_Head, Rest}} <- lists:enumerate(Sources)]),
    Written = merged(Heads, Tails, Width, undefined, Out, 0),
    ok = file:close(Out),
    Written.

%% @doc Append a merged item file into the published LMDB container, when the
%% linked `elmdb' carries the sorted-append API the container needs. Until it
%% does, the item file itself is the artifact.
container(Kind, ItemsPath, DBPath) ->
    case erlang:function_exported(elmdb, put_batch_append, 2) of
        false ->
            {error, <<"elmdb-append-unavailable">>};
        true ->
            appended(Kind, ItemsPath, DBPath)
    end.

%% @doc The byte width of one item of a kind.
item_size(<<"offset">>) -> ?OFFSET_ITEM;
item_size(<<"match">>) -> ?MATCH_ITEM.

%%% Internal functions.

%% @doc Accumulate an offset item, spilling at the threshold.
add_offset(excluded, Sink) ->
    row_counted(<<"offset-excluded">>, 1, Sink);
add_offset(Item, Sink = #{ <<"offset">> := Rows, <<"offset-count">> := N,
        <<"max-rows">> := Max }) ->
    Sink2 =
        row_counted(
            <<"offset">>,
            1,
            Sink#{ <<"offset">> => [Item | Rows], <<"offset-count">> => N + 1 }
        ),
    case N + 1 >= Max of
        true -> spill(<<"offset">>, Sink2);
        false -> Sink2
    end.

%% @doc Accumulate a batch of match items, spilling at the threshold.
add_match(Items, Sink = #{ <<"match">> := Rows, <<"match-count">> := N,
        <<"max-rows">> := Max }) ->
    Count = length(Items),
    Sink2 =
        row_counted(
            <<"match">>,
            Count,
            Sink#{
                <<"match">> => Items ++ Rows,
                <<"match-count">> => N + Count
            }
        ),
    case N + Count >= Max of
        true -> spill(<<"match">>, Sink2);
        false -> Sink2
    end.

%% @doc Write one kind's accumulated rows as a sorted run file.
spill(Kind, Sink) ->
    spill(Kind, maps:get(Kind, Sink), Sink).

spill(_Kind, [], Sink) ->
    Sink;
spill(Kind, Rows, Sink) ->
    #{
        <<"dir">> := Dir,
        <<"prefix">> := Prefix,
        <<"seq">> := Seq,
        <<"files">> := Files
    } = Sink,
    Name =
        hb_util:bin(
            io_lib:format("~s-~s-~4..0B.run", [Prefix, Kind, Seq])),
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, lists:sort(Rows)),
    Sink#{
        Kind => [],
        << Kind/binary, "-count" >> => 0,
        <<"seq">> => Seq + 1,
        <<"files">> => [{Kind, Path} | Files]
    }.

%% @doc Step a row counter by a batch.
row_counted(_Key, 0, Sink) ->
    Sink;
row_counted(Key, N, Sink = #{ <<"rows">> := Rows }) ->
    Sink#{
        <<"rows">> =>
            maps:update_with(Key, fun(Count) -> Count + N end, N, Rows)
    }.

%% @doc Open one run as a merge source: its first item and its tail state.
source(File, Width) ->
    {ok, IO} = file:open(File, [read, raw, binary]),
    refilled(IO, <<>>, Width).

%% @doc A source's next item, as `{Head, Rest}' or `empty' at its end. A run
%% ending mid-item was not written by the spiller and is refused, not read
%% around.
refilled(IO, Buffer, Width) when byte_size(Buffer) < Width ->
    case file:read(IO, ?MERGE_BLOCK) of
        {ok, Block} ->
            refilled(IO, << Buffer/binary, Block/binary >>, Width);
        eof when Buffer == <<>> ->
            ok = file:close(IO),
            empty;
        eof ->
            erlang:error({'run-truncated', byte_size(Buffer)})
    end;
refilled(IO, Buffer, Width) ->
    << Head:Width/binary, Rest/binary >> = Buffer,
    {Head, {IO, Rest, Width}}.

%% @doc Take the least head across sources, write it unless it repeats the
%% item before it, and advance its source.
merged(Heads, Tails, Width, Last, Out, Written) ->
    case gb_sets:is_empty(Heads) of
        true ->
            Written;
        false ->
            {{Head, N}, Heads2} = gb_sets:take_smallest(Heads),
            Written2 =
                case Head of
                    Last -> Written;
                    _ -> ok = file:write(Out, Head), Written + 1
                end,
            {IO, Buffer, _} = maps:get(N, Tails),
            case refilled(IO, Buffer, Width) of
                empty ->
                    merged(Heads2, Tails, Width, Head, Out, Written2);
                {Next, Rest} ->
                    merged(
                        gb_sets:insert({Next, N}, Heads2),
                        Tails#{ N => Rest },
                        Width,
                        Head,
                        Out,
                        Written2
                    )
            end
    end.

%% @doc Build the published container by sorted appends: an LMDB 1.0
%% environment of 64 KiB pages whose main database is `DUPSORT|DUPFIXED'
%% holding every item as a duplicate of the single key `<<0>>'.
appended(Kind, ItemsPath, DBPath) ->
    Width = item_size(Kind),
    MapSize = filelib:file_size(ItemsPath) * 4 + 1073741824,
    {ok, Env} =
        elmdb:env_open(
            DBPath,
            [{page_size, 65536}, {map_size, MapSize}, no_subdir]
        ),
    {ok, DB} = elmdb:db_open(Env, [create, dupsort, dupfixed]),
    {ok, In} = file:open(ItemsPath, [read, raw, binary]),
    Result = append_blocks(In, DB, Width),
    ok = file:close(In),
    ok = elmdb:env_close(Env),
    Result.

%% @doc Feed the item file to the append path block by block.
append_blocks(In, DB, Width) ->
    case file:read(In, ?MERGE_BLOCK) of
        {ok, Block} ->
            Pairs =
                [{<<0>>, Item} || << Item:Width/binary >> <= Block],
            ok = elmdb:put_batch_append(DB, Pairs),
            append_blocks(In, DB, Width);
        eof ->
            ok
    end.

%%% Tests.

%% @doc Rows spill as sorted runs and merge into one ascending, duplicate-free
%% file, whatever the arrival order. Expectations are computed with plain
%% list operations over the raw binaries.
spill_and_merge_test() ->
    Dir =
        filename:join(
            os:getenv("TMPDIR", "/tmp"),
            <<
                "hb-index-runs-",
                (hb_util:encode(crypto:strong_rand_bytes(6)))/binary
            >>
        ),
    % Three-row spills force multiple runs from ten adds.
    Opts = #{ <<"arweave-index-run-rows">> => 3 },
    Rows =
        [
            crypto:hash(sha256, << N:32 >>)
        ||
            N <- lists:seq(1, 10)
        ],
    OffsetItems = [binary:part(Row, 0, 21) || Row <- Rows],
    MatchItems = [binary:part(Row, 0, 17) || Row <- Rows],
    Sink =
        lists:foldl(
            fun({Offset, Match}, Acc) ->
                add(Offset, [Match, Match], Acc)
            end,
            open(Dir, <<"t">>, Opts),
            lists:zip(OffsetItems, MatchItems)
        ),
    Closed = add(excluded, [], Sink),
    Result = close(Closed),
    #{
        <<"offset-runs">> := OffsetRuns,
        <<"match-runs">> := MatchRuns,
        <<"rows">> := Counts
    } = Result,
    ?assert(length(OffsetRuns) >= 2),
    ?assertEqual(
        #{
            <<"offset">> => 10,
            <<"match">> => 20,
            <<"offset-excluded">> => 1
        },
        Counts
    ),
    % Every run is internally ascending.
    lists:foreach(
        fun(Run) ->
            {ok, Bin} = file:read_file(Run),
            Items = [Item || << Item:21/binary >> <= Bin],
            ?assertEqual(lists:sort(Items), Items)
        end,
        OffsetRuns
    ),
    % The merges reproduce the full row sets, ascending and deduplicated.
    OffsetOut = filename:join(Dir, <<"offset.items">>),
    MatchOut = filename:join(Dir, <<"match.items">>),
    ?assertEqual(10, merge(<<"offset">>, OffsetRuns, OffsetOut)),
    ?assertEqual(10, merge(<<"match">>, MatchRuns, MatchOut)),
    {ok, OffsetBin} = file:read_file(OffsetOut),
    ?assertEqual(
        lists:sort(OffsetItems),
        [Item || << Item:21/binary >> <= OffsetBin]
    ),
    {ok, MatchBin} = file:read_file(MatchOut),
    ?assertEqual(
        lists:sort(MatchItems),
        [Item || << Item:17/binary >> <= MatchBin]
    ).
