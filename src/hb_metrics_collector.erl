-module(hb_metrics_collector).

-export(
    [
        deregister_cleanup/1,
        collect_mf/2,
        collect_metrics/2
    ]
).
-behaviour(prometheus_collector).
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

    ok.
collect_metrics(system_load, SystemLoad) ->
    %% Return the gauge metric with no labels
    prometheus_model_helpers:gauge_metrics(
        [
            {[], SystemLoad}
        ]
    );
collect_metrics(process_uptime_seconds, Uptime) ->
    %% Convert the uptime from milliseconds to seconds
    UptimeSeconds = Uptime / 1000,

    %% Return the gauge metric with no labels
    prometheus_model_helpers:gauge_metrics(
        [
            {[], UptimeSeconds}
        ]
    ).

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

create_gauge(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).