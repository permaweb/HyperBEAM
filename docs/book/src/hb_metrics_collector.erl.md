# hb_metrics_collector

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_metrics_collector.erl)

## Exported Functions


---

### deregister_cleanup

```erlang
deregister_cleanup(_) -> ok.
```

### collect_mf

```erlang
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
    ok.
```

### collect_metrics

```erlang
collect_metrics(system_load, SystemLoad) ->
    %% Return the gauge metric with no labels
    prometheus_model_helpers:gauge_metrics(
        [
            {[], SystemLoad}
        ]
    );
```

### collect_metrics

```erlang
collect_metrics(process_uptime_seconds, Uptime) ->
    %% Convert the uptime from milliseconds to seconds
    UptimeSeconds = Uptime / 1000,
    %% Return the gauge metric with no labels
    prometheus_model_helpers:gauge_metrics(
        [
            {[], UptimeSeconds}
        ]
    ).
```

### create_gauge

```erlang
create_gauge(Name, Help, Data) ->
```

---

*Generated from [hb_metrics_collector.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_metrics_collector.erl)*
