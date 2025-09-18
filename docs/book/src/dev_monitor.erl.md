# dev_monitor

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_monitor.erl)

A simple device that allows flexible monitoring of a process execution.
Adding a dev_monitor device to a process will cause the listed functions
to be called with the current process state during each pass. The monitor
functions must not mutate state.

---

## Exported Functions

- `add_monitor/2`
- `end_of_schedule/1`
- `execute/2`
- `init/3`
- `uses/0`

---

### init

```erlang
init(State, _, InitState) ->
    {ok, State#{ <<"monitors">> => InitState }}.
```

### execute

```erlang
execute(Message, State = #{ <<"pass">> := Pass, <<"passes">> := Passes }) when Pass == Passes ->
    signal(State, {message, Message});
```

### execute

```erlang
execute(_, S) -> {ok, S}.
```

### add_monitor

```erlang
add_monitor(Mon, State = #{ <<"monitors">> := Monitors }) ->
    {ok, State#{ <<"monitors">> => [Mon | Monitors] }}.
```

### end_of_schedule

```erlang
end_of_schedule(State) -> signal(State, end_of_schedule).
```

### signal

```erlang
signal(State = #{ <<"monitors">> := StartingMonitors }, Signal) ->
    RemainingMonitors =
        lists:filter(
            fun(Mon) ->
                case Mon(State, Signal) of
                    done -> false;
                    _ -> true
                end
            end,
            StartingMonitors
        ),
    ?event({remaining_monitors, length(RemainingMonitors)}),
    {ok, State#{ <<"monitors">> := RemainingMonitors }}.
```

---

*Generated from [dev_monitor.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_monitor.erl)*
