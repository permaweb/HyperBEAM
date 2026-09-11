-module(hb_metrics_collector).

-export(
    [
        deregister_cleanup/1,
        collect_mf/2,
        collect_metrics/2
    ]
).
-behaviour(prometheus_collector).
-include("include/hb.hrl").
-include("include/hb_http_client.hrl").
%%====================================================================
%% Collector API
%%====================================================================
deregister_cleanup(_) -> ok.

collect_mf(_Registry, Callback) ->
    {Uptime, _} = erlang:statistics(wall_clock),
    Callback(
        create_gauge(
            process_uptime_seconds,
            "The number of seconds the Erlang process has been up.",
            Uptime
        )
    ),

    SystemLoad = safe_avg5(),

    Callback(
        create_gauge(
            system_load,
            "The load values are proportional to how long"
            " time a runnable Unix process has to spend in the run queue"
            " before it is scheduled. Accordingly, higher values mean"
            " more system load",
            SystemLoad
        )
    ),

    {InUse, Free, Queue} = hackney_pool_stats(),
    Callback(
        create_gauge(
            hackney_pool_in_use,
            "Hackney connections currently in use",
            InUse
        )
    ),
    Callback(
        create_gauge(
            hackney_pool_free,
            "Idle hackney connections available in the pool",
            Free
        )
    ),
    Callback(
        create_gauge(
            hackney_pool_queue,
            "Requests waiting for a hackney connection",
            Queue
        )
    ),

    {GunUp, GunInflight, GunQueued} = gun_pool_stats(),
    Callback(
        create_gauge(
            hb_gun_pool_workers_up,
            "Gun pool workers currently in the up state across all pools",
            GunUp
        )
    ),
    Callback(
        create_gauge(
            hb_gun_pool_inflight,
            "Total in-flight gun requests across all pools",
            GunInflight
        )
    ),
    Callback(
        create_gauge(
            hb_gun_pool_queued,
            "Total queued requests across all gun pools",
            GunQueued
        )
    ),

    ok.
collect_metrics(system_load, SystemLoad) ->
    %% Return the gauge metric with no labels
    prometheus_model_helpers:gauge_metrics(
        [
            {[], SystemLoad}
        ]
    );
collect_metrics(process_uptime_seconds, Uptime) ->
    UptimeSeconds = Uptime / 1000,
    prometheus_model_helpers:gauge_metrics([{[], UptimeSeconds}]);
collect_metrics(hackney_pool_in_use, Value) ->
    prometheus_model_helpers:gauge_metrics([{[], Value}]);
collect_metrics(hackney_pool_free, Value) ->
    prometheus_model_helpers:gauge_metrics([{[], Value}]);
collect_metrics(hackney_pool_queue, Value) ->
    prometheus_model_helpers:gauge_metrics([{[], Value}]);
collect_metrics(hb_gun_pool_workers_up, Value) ->
    prometheus_model_helpers:gauge_metrics([{[], Value}]);
collect_metrics(hb_gun_pool_inflight, Value) ->
    prometheus_model_helpers:gauge_metrics([{[], Value}]);
collect_metrics(hb_gun_pool_queued, Value) ->
    prometheus_model_helpers:gauge_metrics([{[], Value}]).

%%====================================================================
%% Private Functions
%%====================================================================

%% @doc Wrapper around cpu_sup:avg5/0 with a 2-second timeout.
%% cpu_sup:avg5/0 uses an infinity timeout to os_mon internally;
%% if the port program stalls, it blocks the Prometheus scrape indefinitely.
%% On timeout, the worker is killed to avoid leaking blocked processes.
safe_avg5() ->
    Ref = make_ref(),
    Self = self(),
    {Pid, MonRef} = spawn_monitor(fun() -> Self ! {Ref, catch cpu_sup:avg5()} end),
    receive
        {Ref, Load} when is_integer(Load) ->
            erlang:demonitor(MonRef, [flush]),
            Load;
        {Ref, _} ->
            erlang:demonitor(MonRef, [flush]),
            0;
        {'DOWN', MonRef, process, Pid, _} ->
            0
    after 2000 ->
        exit(Pid, kill),
        erlang:demonitor(MonRef, [flush]),
        receive {Ref, _} -> ok after 0 -> ok end,
        0
    end.

%% @doc Read hackney pool stats at scrape time.
hackney_pool_stats() ->
    try hackney_pool:get_stats(?HACKNEY_POOL) of
        Stats ->
            {proplists:get_value(in_use_count, Stats, 0),
             proplists:get_value(free_count, Stats, 0),
             proplists:get_value(queue_count, Stats, 0)}
    catch _:_ -> {0, 0, 0}
    end.

%% @doc Sum workers_up, inflight, and queued across all gun pools at scrape time.
gun_pool_stats() ->
    %% Dead managers (stale ETS row during pool shutdown) or slow managers
    %% (busy draining a queue) must contribute zero rather than crash the
    %% scrape. Empirically observed classes: exit:{normal,_} on exited pid,
    %% exit:{noproc,_} on never-registered name, exit:{timeout,_} on busy
    %% mailbox, error:badarg on missing ETS table.
    try ets:tab2list(hb_gun_pool_registry) of
        Entries ->
            lists:foldl(
                fun({_Key, MgrPid}, {Up0, In0, Qd0}) when is_pid(MgrPid) ->
                    try gen_server:call(MgrPid, pool_info, 1000) of
                        #{workers_up := U, inflight := I, queued := Q} ->
                            {Up0 + U, In0 + I, Qd0 + Q}
                    catch
                        exit:{normal,  _} -> {Up0, In0, Qd0};
                        exit:{noproc,  _} -> {Up0, In0, Qd0};
                        exit:{timeout, _} ->
                            ?event(debug_http_client,
                                   {pool_info_scrape_timeout, {mgr, MgrPid}}),
                            {Up0, In0, Qd0}
                    end;
                   (_PendingEntry, Acc) ->
                    %% {Key, {pending, Claimer}} appears transiently during
                    %% pool cold-start; it is not a manager yet.
                    Acc
                end,
                {0, 0, 0},
                Entries)
    catch error:badarg -> {0, 0, 0}
    end.

create_gauge(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).
