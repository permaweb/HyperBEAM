%%% @doc HyperBEAM wrapper for Prometheus metrics.
-module(hb_prometheus).
-export([ensure_started/0, declare/2, measure_and_report/2, measure_and_report/3]).

-define(STARTUP_RETRY_INTERVAL, 60). % seconds

%% @doc Ensure the Prometheus application has been started. Caches startup
%% failure with a timestamp to avoid repeated blocking ensure_all_started
%% calls on hot paths, but retries after a cooldown period.
ensure_started() ->
    case application:get_application(prometheus) of
        undefined ->
            case persistent_term:get(hb_prometheus_start_failed, undefined) of
                FailedAt when is_integer(FailedAt) ->
                    case erlang:monotonic_time(second) - FailedAt >= ?STARTUP_RETRY_INTERVAL of
                        true -> attempt_start();
                        false -> {error, not_started}
                    end;
                undefined ->
                    attempt_start()
            end;
        _ -> ok
    end.

attempt_start() ->
    case application:ensure_all_started(
        [prometheus, prometheus_cowboy, prometheus_ranch]
    ) of
        {ok, _} ->
            persistent_term:erase(hb_prometheus_start_failed),
            ok;
        {error, _} = Err ->
            persistent_term:put(
                hb_prometheus_start_failed,
                erlang:monotonic_time(second)
            ),
            Err
    end.

%% @doc Declare a new Prometheus metric in a replay-safe manner.
declare(Type, Metric) ->
    case ensure_started() of
        ok ->
            try do_declare(Type, Metric)
            catch error:mfa_already_exists -> ok
            end;
        _ -> ok
    end.

do_declare(histogram, Metric) -> prometheus_histogram:declare(Metric);
do_declare(counter, Metric) -> prometheus_counter:declare(Metric);
do_declare(gauge, Metric) -> prometheus_gauge:declare(Metric);
do_declare(Type, _Metric) -> throw({unsupported_metric_type, Type}).

%% @doc Measure function duration and report metric, ensuring that the Prometheus
%% application has been started first. If Prometheus is unavailable, the function
%% is executed without measurement.
measure_and_report(Fun, Metric) ->
    measure_and_report(Fun, Metric, []).
measure_and_report(Fun, Metric, Labels) ->
    case ensure_started() of
        ok ->
            Start = erlang:monotonic_time(),
            try Fun()
            after
                DurationNative = erlang:monotonic_time() - Start,
                try prometheus_histogram:observe(Metric, Labels, DurationNative)
                catch _:_ -> ok
                end
            end;
        _ -> Fun()
    end.
