%%% @doc HyperBEAM wrapper for Prometheus metrics.
-module(hb_prometheus).
-export([ensure_started/0, declare/2, measure_and_report/2, measure_and_report/3]).

%% @doc Ensure the Prometheus application has been started.
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
    ok = ensure_started(),
    try do_declare(Type, Metric)
    catch error:mfa_already_exists -> ok
    end.

do_declare(histogram, Metric) -> prometheus_histogram:declare(Metric);
do_declare(counter, Metric) -> prometheus_counter:declare(Metric);
do_declare(gauge, Metric) -> prometheus_gauge:declare(Metric);
do_declare(Type, _Metric) -> throw({unsupported_metric_type, Type}).

%% @doc Measure function duration and report metric, ensuring that the Prometheus
%% application has been started first.
measure_and_report(Fun, Metric) -> 
    measure_and_report(Fun, Metric, []).
measure_and_report(Fun, Metric, Labels) ->
    ok = ensure_started(),
    Start = erlang:monotonic_time(),
    try Fun()
    after
        DurationNative = erlang:monotonic_time() - Start,
        try prometheus_histogram:observe(Metric, Labels, DurationNative)
        catch _:_ -> ok
        end
    end.
