# hb_logger

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_logger.erl)

## Exported Functions

- `log/2`
- `register/1`
- `report/1`
- `start/0`
- `start/1`

---

### start

```erlang
start() -> start(undefined).
```

### start

```erlang
start(Client) ->
    spawn(fun() ->
        loop(#state{client = Client})
    end).
```

### log

```erlang
log(Monitor, Data) ->
    Monitor ! {log, Data}.
```

### register

```erlang
register(Monitor) ->
    ?event({self(), registering}),
    Monitor ! {register, self()}.
```

### report

```erlang
report(Monitor) ->
    Monitor ! {report, self()},
    receive
        {report, Activity} ->
            Activity
    end.
```

### loop

```erlang
loop(#state { processes = [], client = undefined }) -> done;
```

### loop

```erlang
loop(#state { processes = [], client = C, activity = A }) ->
    C ! {?MODULE, self(), done, A};
```

### loop

```erlang
loop(State) ->
    receive
        {log, Activity} ->
            console(State, Activity),
            loop(State#state{ activity = [Activity | State#state.activity] });
        {register, PID} ->
            ?event(registered),
            %erlang:monitor(process, PID),
            console(State, Act = {ok, registered, PID}),
            ?event({registered, PID}),
            loop(State#state{
                processes =
                    [PID | case State#state.processes of waiting -> []; L -> L end],
                activity = [Act | State#state.activity]
            });
        {'DOWN', _MonitorRef, process, PID, Reason} ->
            console(State, Act = {terminated, Reason, PID}),
            ?event({dead, PID}),
            loop(State#state{
                processes = State#state.processes -- [PID],
                activity = [Act | State#state.activity]
            });
        {report, PID} ->
            PID ! {report, State#state.activity},
            loop(State)
    end.
```

### console

```erlang
console(#state { console = false }, _) ->
    not_printing;
```

### console

```erlang
console(S, {Status, Type, Details}) when is_record(Details, tx) ->
    console(S, {Status, Type, hb_util:id(Details)});
```

### console

```erlang
console(_S, {Status, Type, Details}) ->
    io:format("### MU PUSH REPORT ~p ###~n~p: ~p~n~p~n~n",
        [self(), Status, Type, Details]);
```

### console

```erlang
console(_S, Act) ->
```

---

*Generated from [hb_logger.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_logger.erl)*
