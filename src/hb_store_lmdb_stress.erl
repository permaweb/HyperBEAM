%% @doc LMDB write stress tool.
%%
%% Writes as fast as possible for a configurable duration, reporting RSS and
%% LMDB file size every second. After a warmup period the tool checks whether
%% RSS has stabilised using a sliding window; a continuously growing RSS
%% indicates a memory leak rather than normal mmap pressure.
%%
%% Usage from the Erlang shell:
%%
%%   hb_store_lmdb_stress:run().
%%   hb_store_lmdb_stress:run(#{ <<"duration">> => 60, <<"batch-size">> => 100 }).
%%
%% Configurable keys (all optional, merged over the defaults):
%%
%%   <<"name">>              - LMDB directory  (default: /tmp/lmdb-stress-tool)
%%   <<"capacity">>          - map_size bytes   (default: 100 GiB)
%%   <<"batch-size">>        - elmdb batch_size (default: 10)
%%   <<"sync">>              - msync on commit  (default: true)
%%   <<"duration">>          - seconds to run   (default: 300)
%%   <<"warmup">>            - seconds before RSS check begins (default: 60)
%%   <<"stable-window">>     - sliding window size in seconds (default: 30)
%%   <<"max-rss-growth-mib">>- max RSS growth MiB over window (default: 500)
%%   <<"value-size">>        - bytes per record (default: 4096)
%%   <<"throttle-every">>    - sleep 1 ms every N writes (default: 10)
%%   <<"max-overlay">>       - pause writing when overlay exceeds this count, 0 = disabled (default: 0)
%%                            safe on any size database — does not trigger fsync

-module(hb_store_lmdb_stress).
-export([run/0, run/1]).

-define(DEFAULTS, #{
    <<"name">>               => <<"/tmp/lmdb-stress-tool">>,
    <<"capacity">>           => 100 * 1024 * 1024 * 1024,
    <<"batch-size">>         => 10000,
    <<"sync">>               => true,
    <<"duration">>           => 400,
    <<"warmup">>             => 60,
    <<"stable-window">>      => 30,
    <<"max-rss-growth-mib">> => 500,
    <<"value-size">>         => 10,
    <<"throttle-every">>     => 10000,
    <<"max-overlay">>        => 10000
}).

run() -> run(#{}).

run(UserOpts) ->
    Opts = maps:merge(?DEFAULTS, UserOpts),
    StoreOpts = #{
        <<"store-module">> => hb_store_lmdb,
        <<"name">>         => maps:get(<<"name">>, Opts),
        <<"capacity">>     => maps:get(<<"capacity">>, Opts),
        <<"batch-size">>   => maps:get(<<"batch-size">>, Opts),
        <<"sync">>         => maps:get(<<"sync">>, Opts)
    },
    Duration      = maps:get(<<"duration">>, Opts),
    Warmup        = maps:get(<<"warmup">>, Opts),
    StableWindow  = maps:get(<<"stable-window">>, Opts),
    MaxGrowthKiB  = maps:get(<<"max-rss-growth-mib">>, Opts) * 1024,
    ValueSize     = maps:get(<<"value-size">>, Opts),
    ThrottleEvery = maps:get(<<"throttle-every">>, Opts),
    MaxOverlay    = maps:get(<<"max-overlay">>, Opts),

    hb_store_lmdb:reset(StoreOpts),
    DataDir  = binary_to_list(maps:get(<<"name">>, StoreOpts)),
    DataFile = filename:join(DataDir, "data.mdb"),

    io:format(
        "~n[stress] === LMDB Write Stress Tool ===~n"
        "[stress] Duration: ~Bs | Warmup: ~Bs | Stable window: ~Bs~n"
        "[stress] Max RSS growth over window: ~B MiB | Value: ~B B"
        " | Batch: ~B | Sync: ~p~n"
        "[stress] LMDB: ~s~n",
        [Duration, Warmup, StableWindow,
         maps:get(<<"max-rss-growth-mib">>, Opts), ValueSize,
         maps:get(<<"batch-size">>, StoreOpts),
         maps:get(<<"sync">>, StoreOpts), DataFile]
    ),

    Value     = binary:copy(<<0>>, ValueSize),
    StartTime = erlang:monotonic_time(second),
    KeyTab    = ets:new(stress_keys, [public, set]),
    ets:insert(KeyTab, {written, 0}),
    WriterPid = spawn(fun() -> write_loop(StoreOpts, Value, ThrottleEvery, MaxOverlay, KeyTab, 0) end),
    Result    = monitor_loop(StoreOpts, DataFile, StartTime, Duration, Warmup,
                             StableWindow, MaxGrowthKiB, KeyTab, 0, []),
    WriterPid ! stop,

    FinalSize = filelib:file_size(DataFile),
    TotalKeys = ets:lookup_element(KeyTab, written, 2),
    ets:delete(KeyTab),
    io:format(
        "[stress] === Final: LMDB ~s bytes | Keys written: ~s | Result: ~p ===~n",
        [fmt(FinalSize), fmt(TotalKeys), Result]
    ),
    case Result of
        {rss_leak, GrowthKiB} ->
            io:format(
                "[stress] FAIL: RSS grew ~s KiB over ~Bs window (limit ~s KiB)~n",
                [fmt(GrowthKiB), StableWindow, fmt(MaxGrowthKiB)]
            ),
            hb_store_lmdb:stop(StoreOpts),
            {error, {rss_leak, GrowthKiB}};
        ok when FinalSize =:= 0 ->
            io:format("[stress] FAIL: LMDB file is empty~n"),
            hb_store_lmdb:stop(StoreOpts),
            {error, lmdb_file_empty};
        ok ->
            io:format("[stress] PASS~n"),
            hb_store_lmdb:stop(StoreOpts),
            ok
    end.

write_loop(StoreOpts, Value, ThrottleEvery, MaxOverlay, KeyTab, N) ->
    Key = <<"stress/", (integer_to_binary(N))/binary>>,
    hb_store_lmdb:write(StoreOpts, Key, Value),
    ets:update_counter(KeyTab, written, 1),
    case ThrottleEvery > 0 andalso N rem ThrottleEvery =:= 0 of
        true -> timer:sleep(1);
        false -> ok
    end,
    case MaxOverlay > 0 of
        true  -> wait_overlay(StoreOpts, MaxOverlay);
        false -> ok
    end,
    receive stop -> ok
    after 0 -> write_loop(StoreOpts, Value, ThrottleEvery, MaxOverlay, KeyTab, N + 1)
    end.

%% Spin until the elmdb write overlay drains below MaxOverlay entries.
%% Uses overlay_count which is purely in-memory — no fsync, safe on any db size.
wait_overlay(StoreOpts, MaxOverlay) ->
    case hb_store_lmdb:overlay_count(StoreOpts) > MaxOverlay of
        true  -> timer:sleep(1), wait_overlay(StoreOpts, MaxOverlay);
        false -> ok
    end.

%% RssWindow holds the last StableWindow RSS samples, newest first.
monitor_loop(StoreOpts, DataFile, StartTime, Duration, Warmup,
             StableWindow, MaxGrowthKiB, KeyTab, PrevSize, RssWindow) ->
    timer:sleep(1000),
    Elapsed  = erlang:monotonic_time(second) - StartTime,
    FileSize = filelib:file_size(DataFile),
    RssKB    = rss_kb(),
    Keys     = ets:lookup_element(KeyTab, written, 2),
    Overlay  = hb_store_lmdb:overlay_count(StoreOpts),
    NewWindow = lists:sublist([RssKB | RssWindow], StableWindow),
    Phase = case Elapsed > Warmup of
        false -> warmup;
        true  -> stable
    end,
    io:format(
        "[stress] t=~3Bs (~s) | RSS ~s KiB | LMDB ~s bytes (+~s) | keys ~s | overlay ~s~n",
        [Elapsed, Phase,
         fmt(RssKB), fmt(FileSize), fmt(FileSize - PrevSize),
         fmt(Keys), fmt(Overlay)]
    ),
    LeakCheck =
        case {Phase, length(NewWindow) >= StableWindow} of
            {stable, true} ->
                Growth = RssKB - lists:last(NewWindow),
                case Growth > MaxGrowthKiB of
                    true  -> {rss_leak, Growth};
                    false -> continue
                end;
            _ ->
                continue
        end,
    case LeakCheck of
        {rss_leak, _} = Leak ->
            Leak;
        continue when Elapsed >= Duration ->
            ok;
        continue ->
            monitor_loop(StoreOpts, DataFile, StartTime, Duration, Warmup,
                         StableWindow, MaxGrowthKiB, KeyTab, FileSize, NewWindow)
    end.

rss_kb() ->
    Output = os:cmd("ps -o rss= -p " ++ os:getpid()),
    try list_to_integer(string:trim(Output))
    catch _:_ -> 0
    end.

fmt(N) ->
    lists:reverse(insert_commas(lists:reverse(integer_to_list(N)), 0)).

insert_commas([], _) -> [];
insert_commas(Digits, 3) -> [$, | insert_commas(Digits, 0)];
insert_commas([D | Rest], Count) -> [D | insert_commas(Rest, Count + 1)].
