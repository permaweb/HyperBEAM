%%% @doc Handle prometheus metrics
-module(hb_prometheus).

-export([measure_and_report/2, measure_and_report/3]).

%% @doc Measure function duration and report metric
measure_and_report(Fun, Metric) -> 
    measure_and_report(Fun, Metric, []).
measure_and_report(Fun, Metric, Labels) ->
    Start = erlang:monotonic_time(),
    try
        Result = Fun(),
        Result
    after
        DurationNative = erlang:monotonic_time() - Start,
        prometheus_histogram:observe(Metric, Labels, DurationNative)
    end.
