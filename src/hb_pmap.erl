%% @doc Concurrency-limited parallel map that preserves input order.
%% Spawns up to MaxWorkers workers and refills the pool as workers complete.
-module(hb_pmap).

-export([parallel_map/3, parallel_reduce/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

parallel_map(Items, Fun, MaxWorkers) when is_list(Items), is_function(Fun, 1) ->
    Workers = max(1, MaxWorkers),
    Parent = self(),
    ItemsWithRefs = [{Item, make_ref()} || Item <- Items],
    {ToSpawn, Remaining} =
        lists:split(min(length(ItemsWithRefs), Workers), ItemsWithRefs),
    ActiveRefs = [spawn_worker(IWR, Fun, Parent) || IWR <- ToSpawn],
    ResultsMap = collect(ActiveRefs, Remaining, Fun, Parent, #{}),
    [maps:get(Ref, ResultsMap) || {_Item, Ref} <- ItemsWithRefs].

%% @doc Concurrency-limited parallel reduce that preserves input order at the
%% reducer boundary without materializing the full result list in the caller.
parallel_reduce(Items, Fun, ReduceFun, Acc, MaxWorkers)
        when is_list(Items),
             is_function(Fun, 1),
             is_function(ReduceFun, 2) ->
    Parent = self(),
    CoordinatorRef = make_ref(),
    MonitorRef =
        erlang:monitor(
            process,
            spawn(
                fun() ->
                    parallel_reduce_coordinator(
                        Parent,
                        CoordinatorRef,
                        Fun,
                        ReduceFun,
                        Acc,
                        max(1, MaxWorkers),
                        lists:zip(lists:seq(1, length(Items)), Items)
                    )
                end
            )
        ),
    receive
        {hb_pmap_done, CoordinatorRef, Result} ->
            erlang:demonitor(MonitorRef, [flush]),
            case Result of
                {ok, FinalAcc} ->
                    FinalAcc;
                {error, Error} ->
                    throw(Error)
            end;
        {'DOWN', MonitorRef, process, _Pid, Reason} ->
            throw({pmap_coordinator_crashed, Reason})
    end.

spawn_worker({Item, Ref}, Fun, Parent) ->
    spawn(
        fun() ->
            try
                Parent ! {hb_pmap_result, Ref, Fun(Item)}
            catch
                Class:Reason:Stacktrace ->
                    ?event(pmap_error, {pmap_worker_crashed,
                        {class, Class},
                        {reason, Reason}, 
                        {stacktrace, {trace, Stacktrace}}}),
                    Parent ! {
                        hb_pmap_worker_crash,
                        Ref,
                        Class,
                        Reason,
                        Stacktrace
                    }
            end
        end
    ),
    Ref.

collect([], [], _Fun, _Parent, Results) ->
    Results;
collect(Active, Remaining, Fun, Parent, Results) ->
    receive
        {hb_pmap_result, Ref, Result} ->
            NewResults = Results#{Ref => Result},
            NewActive = lists:delete(Ref, Active),
            case Remaining of
                [] ->
                    collect(NewActive, [], Fun, Parent, NewResults);
                [Next | Rest] ->
                    NextRef = spawn_worker(Next, Fun, Parent),
                    collect(
                        [NextRef | NewActive],
                        Rest,
                        Fun,
                        Parent,
                        NewResults
                    )
            end;
        {hb_pmap_worker_crash, _Ref, Class, Reason, Stacktrace} ->
            throw({pmap_worker_crashed, Class, Reason, Stacktrace})
    end.

%% @doc Coordinate the parallel reduce in a dedicated process so worker results
%% never accumulate in the caller mailbox on failure.
parallel_reduce_coordinator(
        Parent,
        CoordinatorRef,
        Fun,
        ReduceFun,
        Acc,
        MaxWorkers,
        Items
    ) ->
    try
        {ToSpawn, Remaining} = lists:split(min(length(Items), MaxWorkers), Items),
        Active = [spawn_indexed_worker(I, Fun, self()) || I <- ToSpawn],
        FinalAcc =
            collect_reduce(Active, Remaining, Fun, ReduceFun, 1, #{}, Acc),
        Parent ! {hb_pmap_done, CoordinatorRef, {ok, FinalAcc}}
    catch
        throw:Reason ->
            Parent ! {hb_pmap_done, CoordinatorRef, {error, Reason}};
        Class:Reason:Stacktrace ->
            Parent ! {
                hb_pmap_done,
                CoordinatorRef,
                {error, {pmap_reduce_failed, Class, Reason, Stacktrace}}
            }
    end.

spawn_indexed_worker({Index, Item}, Fun, Parent) ->
    Pid =
        spawn(
            fun() ->
                try
                    Parent ! {hb_pmap_result, Index, Fun(Item)}
                catch
                    Class:Reason:Stacktrace ->
                        ?event(pmap_error, {pmap_worker_crashed,
                            {class, Class},
                            {reason, Reason},
                            {stacktrace, {trace, Stacktrace}}}),
                        Parent ! {
                            hb_pmap_worker_crash,
                            Index,
                            Class,
                            Reason,
                            Stacktrace
                        }
                end
            end
        ),
    {Index, Pid}.

collect_reduce([], [], _Fun, ReduceFun, NextIndex, Pending, Acc) ->
    {FinalNextIndex, FinalPending, FinalAcc} =
        reduce_ready(NextIndex, Pending, ReduceFun, Acc),
    case {FinalNextIndex, map_size(FinalPending)} of
        {_Index, 0} ->
            FinalAcc
    end;
collect_reduce(Active, Remaining, Fun, ReduceFun, NextIndex, Pending, Acc) ->
    receive
        {hb_pmap_result, Index, Result} ->
            NewPending = Pending#{Index => Result},
            {ReadyIndex, ReadyPending, ReadyAcc} =
                reduce_ready(NextIndex, NewPending, ReduceFun, Acc),
            NewActive = lists:keydelete(Index, 1, Active),
            case Remaining of
                [] ->
                    collect_reduce(
                        NewActive,
                        [],
                        Fun,
                        ReduceFun,
                        ReadyIndex,
                        ReadyPending,
                        ReadyAcc
                    );
                [Next | Rest] ->
                    NextWorker = spawn_indexed_worker(Next, Fun, self()),
                    collect_reduce(
                        [NextWorker | NewActive],
                        Rest,
                        Fun,
                        ReduceFun,
                        ReadyIndex,
                        ReadyPending,
                        ReadyAcc
                    )
            end;
        {hb_pmap_worker_crash, _Index, Class, Reason, Stacktrace} ->
            kill_workers(Active),
            throw({pmap_worker_crashed, Class, Reason, Stacktrace})
    end.

reduce_ready(Index, Pending, ReduceFun, Acc) ->
    case maps:take(Index, Pending) of
        error ->
            {Index, Pending, Acc};
        {Result, Rest} ->
            reduce_ready(Index + 1, Rest, ReduceFun, ReduceFun(Result, Acc))
    end.

kill_workers(Active) ->
    lists:foreach(
        fun({_Index, Pid}) ->
            exit(Pid, kill)
        end,
        Active
    ).

%%% Tests

%% @doc Verifies empty input returns an empty result list.
empty_input_test() ->
    ?assertEqual([], parallel_map([], fun(X) -> X end, 4)).

%% @doc Covers normal-path behavior across worker configs:
%% output order, per-item single execution, and max in-flight worker bounds.
instrumented_normal_path_test() ->
    Items = [1, 2, 3, 4, 5, 6],
    ExpectedResults = [Item * 10 || Item <- Items],
    lists:foreach(
        fun(MaxWorkers) ->
            #{
                results := Results,
                started := Started,
                completed := Completed,
                in_flight := InFlight,
                peak := Peak
            } =
                run_instrumented_case(Items, MaxWorkers),
            EffectiveWorkers = min(max(1, MaxWorkers), length(Items)),
            ?assertEqual(ExpectedResults, Results),
            ?assertEqual(length(Items), Started),
            ?assertEqual(length(Items), Completed),
            ?assertEqual(0, InFlight),
            ?assert(Peak =< EffectiveWorkers),
            ?assert(Peak >= 1)
        end,
        [0, 3, 10]
    ).

%% @doc Verifies worker exceptions fail fast instead of hanging.
worker_crash_fails_fast_test() ->
    ?assertMatch(
        {pmap_worker_crashed, error, boom, _},
        catch parallel_map(
            [1, 2, 3],
            fun
                (2) -> erlang:error(boom);
                (Item) -> Item
            end,
            2
        )
    ).

%% @doc Covers streaming reduction across worker configs while preserving input
%% order and max in-flight bounds.
streaming_reduce_normal_path_test() ->
    Items = [1, 2, 3, 4, 5, 6],
    ExpectedResults = [Item * 10 || Item <- Items],
    lists:foreach(
        fun(MaxWorkers) ->
            #{
                results := Results,
                started := Started,
                completed := Completed,
                in_flight := InFlight,
                peak := Peak
            } =
                run_streaming_case(Items, MaxWorkers),
            EffectiveWorkers = min(max(1, MaxWorkers), length(Items)),
            ?assertEqual(ExpectedResults, Results),
            ?assertEqual(length(Items), Started),
            ?assertEqual(length(Items), Completed),
            ?assertEqual(0, InFlight),
            ?assert(Peak =< EffectiveWorkers),
            ?assert(Peak >= 1)
        end,
        [0, 3, 10]
    ).

%% @doc Verifies the streaming coordinator keeps worker messages out of the
%% caller mailbox after a fail-fast crash.
streaming_reduce_failure_isolates_caller_mailbox_test() ->
    ?assertMatch(
        {pmap_worker_crashed, error, boom, _},
        catch parallel_reduce(
            [1, 2],
            fun
                (1) -> erlang:error(boom);
                (2) ->
                    timer:sleep(25),
                    <<"late-result">>
            end,
            fun(Result, Acc) -> [Result | Acc] end,
            [],
            2
        )
    ),
    timer:sleep(50),
    {messages, Messages} = process_info(self(), messages),
    ?assertEqual(
        false,
        lists:any(
            fun
                ({hb_pmap_result, _, _}) -> true;
                ({hb_pmap_worker_crash, _, _, _, _}) -> true;
                (_) -> false
            end,
            Messages
        )
    ).

%% @doc Runs a single instrumented parallel_map/3 case and returns
%% aggregated execution stats and final ordered results.
run_instrumented_case(Items, MaxWorkers) ->
    Counters = atomics:new(4, []),
    Results =
        parallel_map(
            Items,
            fun(Item) ->
                mark_worker_started(Counters),
                % Reverse completion order to stress order preservation.
                timer:sleep((length(Items) - Item) * 5),
                mark_worker_completed(Counters),
                Item * 10
            end,
            MaxWorkers
        ),
    #{
        results => Results,
        started => atomics:get(Counters, 1),
        completed => atomics:get(Counters, 2),
        in_flight => atomics:get(Counters, 3),
        peak => atomics:get(Counters, 4)
    }.

%% @doc Runs a single instrumented parallel_reduce/5 case and returns
%% aggregated execution stats and final ordered results.
run_streaming_case(Items, MaxWorkers) ->
    Counters = atomics:new(4, []),
    Results =
        lists:reverse(
            parallel_reduce(
                Items,
                fun(Item) ->
                    mark_worker_started(Counters),
                    timer:sleep((length(Items) - Item) * 5),
                    mark_worker_completed(Counters),
                    Item * 10
                end,
                fun(Result, Acc) ->
                    [Result | Acc]
                end,
                [],
                MaxWorkers
            )
        ),
    #{
        results => Results,
        started => atomics:get(Counters, 1),
        completed => atomics:get(Counters, 2),
        in_flight => atomics:get(Counters, 3),
        peak => atomics:get(Counters, 4)
    }.

mark_worker_started(Counters) ->
    _ = atomics:add_get(Counters, 1, 1),
    InFlight = atomics:add_get(Counters, 3, 1),
    update_peak(Counters, InFlight).

mark_worker_completed(Counters) ->
    _ = atomics:add_get(Counters, 2, 1),
    _ = atomics:add_get(Counters, 3, -1),
    ok.

update_peak(Counters, InFlight) ->
    Peak = atomics:get(Counters, 4),
    case InFlight =< Peak of
        true ->
            ok;
        false ->
            case atomics:compare_exchange(Counters, 4, Peak, InFlight) of
                Peak -> ok;
                _ -> update_peak(Counters, InFlight)
            end
    end.
