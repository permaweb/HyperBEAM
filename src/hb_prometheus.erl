%%% @doc HyperBEAM wrapper for Prometheus metrics.
-module(hb_prometheus).
-export([ensure_started/0, declare/2, measure_and_report/2, measure_and_report/3]).
-export([observe/2, observe/3, inc/2, inc/3, inc/4, dec/2, dec/3, dec/4]).

%% @doc Ensure the Prometheus application has been started. Caches startup
%% failure with a timestamp to avoid repeated blocking ensure_all_started
%% calls on hot paths, but retries after a cooldown period.
ensure_started() ->
    case is_started() of
        true -> ok;
        false ->
            application:ensure_all_started(
                [prometheus, prometheus_cowboy, prometheus_ranch]
            ),
            wait_for_prometheus_started()
    end.

%% @doc Lazy wait for prometheus to come up, after we have started the application.
wait_for_prometheus_started() ->
    case is_started() of
        true -> ok;
        false ->
            timer:sleep(1),
            wait_for_prometheus_started()
    end.

%% @doc Check if prometheus has been started.
%% The application itself may return `ok` to Erlang before it is actually ready
%% for use, so we wait for the `ets` table to be created instead.
is_started() ->
    Info = ets:info(prometheus_registry_table),
    Info =/= undefined.

%% @doc Declare a new Prometheus metric in a replay-safe manner.
declare(Type, Metric) ->
    case ensure_started() of
        ok ->
            try do_declare(Type, Metric)
            catch
                error:mfa_already_exists ->
                    ok;
                error:{mf_already_exists, _, _} ->
                    ok
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
measure_and_report(Fun, Metric) when is_function(Fun) ->
    measure_and_report(Fun, Metric, []).
measure_and_report(Fun, Metric, Labels) when is_function(Fun) ->
    Start = erlang:monotonic_time(),
    try Fun()
    after
        DurationNative = erlang:monotonic_time() - Start,
        observe(DurationNative, Metric, Labels)
    end.

observe(Duration, Metric) when is_integer(Duration) ->
    observe(Duration, Metric, []).
observe(Duration, Metric, Labels) when is_integer(Duration) ->
    case ensure_started() of
        ok ->
            try prometheus_histogram:observe(Metric, Labels, Duration)
            catch _:_ -> ok
            end;
        _ ->
            ok
    end.

inc(Type, Metrics) ->
    inc(Type, Metrics, []).
inc(Type, Metrics, Labels) ->
    inc(Type, Metrics, Labels, 1).
inc(Type, Metrics, Labels, Value) ->
    case ensure_started() of
        ok ->
            try do_inc(Type, Metrics, Labels, Value)
            catch error:mfa_already_exists -> ok
            end;
        _ -> ok
    end.
do_inc(counter, Name, Labels, Value) ->
    prometheus_counter:inc(Name, Labels, Value);
do_inc(gauge, Name, Labels, Value) ->
    prometheus_gauge:inc(Name, Labels, Value).

dec(Type, Metrics) ->
    dec(Type, Metrics, []).
dec(Type, Metrics, Labels) ->
    dec(Type, Metrics, Labels, 1).
dec(Type, Metrics, Labels, Value) ->
    case ensure_started() of 
        ok ->
            try do_dec(Type, Metrics, Labels, Value)
            catch error:mfa_already_exists -> ok
            end;
        _ -> ok
    end.

do_dec(gauge, Name, Labels, Value) ->
    prometheus_gauge:dec(Name, Labels, Value).