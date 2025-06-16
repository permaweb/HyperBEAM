-module(hb_metrics_collector).

-export([deregister_cleanup/1,
         collect_mf/2,
         collect_metrics/2]).
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
        Uptime)),

    SystemLoad = cpu_sup:avg5(),

    Callback(
      create_gauge(
        system_load,
        "The load values are proportional to how long"
        " time a runnable Unix process has to spend in the run queue"
        " before it is scheduled. Accordingly, higher values mean"
        " more system load",
        SystemLoad)),

    ok.


collect_metrics(system_load, SystemLoad) ->
    %% Return the gauge metric with no labels
    prometheus_model_helpers:gauge_metrics(
      [{[], SystemLoad}]);
collect_metrics(process_uptime_seconds, Uptime) ->
    %% Convert the uptime from milliseconds to seconds
    UptimeSeconds = Uptime / 1000,

    %% Return the gauge metric with no labels
    prometheus_model_helpers:gauge_metrics(
      [{[], UptimeSeconds}]).


%%====================================================================
%% Private Functions
%%====================================================================
create_gauge(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).
