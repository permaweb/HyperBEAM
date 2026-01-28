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

    SystemLoad = cpu_sup:avg5(),

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

    collect_lru_metrics(Callback),

    ok.

collect_lru_metrics(Callback) ->
    try
        case hb_opts:get(store) of
            undefined -> ok;
            Stores when is_list(Stores) -> collect_lru_from_stores(Stores, Callback);
            Store when is_map(Store) -> collect_lru_from_stores([Store], Callback)
        end
    catch
        _:_ -> ok
    end.

collect_lru_from_stores([], _Callback) -> ok;
collect_lru_from_stores([Store | Rest], Callback) ->
    case hb_maps:get(<<"store-module">>, Store, undefined, #{}) of
        hb_store_lru ->
            try
                #{bytes := Bytes, elements := Elements} = hb_store_lru:stats(Store),
                Name = hb_maps:get(<<"name">>, Store, <<"default">>, #{}),
                Callback(create_labeled_gauge(
                    lru_cache_bytes,
                    "LRU cache size in bytes",
                    Name,
                    Bytes
                )),
                Callback(create_labeled_gauge(
                    lru_cache_elements,
                    "LRU cache element count",
                    Name,
                    Elements
                ))
            catch
                _:_ -> ok
            end;
        _ -> ok
    end,
    collect_lru_from_stores(Rest, Callback).
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
    );
collect_metrics(lru_cache_bytes, {Name, Value}) ->
    prometheus_model_helpers:gauge_metrics([{[{name, Name}], Value}]);
collect_metrics(lru_cache_elements, {Name, Value}) ->
    prometheus_model_helpers:gauge_metrics([{[{name, Name}], Value}]).

%%====================================================================
%% Private Functions
%%====================================================================
create_gauge(Name, Help, Data) ->
    prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, Data).

create_labeled_gauge(Name, Help, Label, Value) ->
    prometheus_model_helpers:create_mf(Name, Help, gauge, ?MODULE, {Label, Value}).