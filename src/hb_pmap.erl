%% @doc Concurrency-limited parallel map that preserves input order.
%% Spawns up to MaxWorkers workers and refills the pool as workers complete.
-module(hb_pmap).

-export([parallel_map/3]).
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

spawn_worker({Item, Ref}, Fun, Parent) ->
    spawn(
        fun() ->
            try
                Parent ! {hb_pmap_result, Ref, Fun(Item)}
            catch
                Class:Reason:Stacktrace ->
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
