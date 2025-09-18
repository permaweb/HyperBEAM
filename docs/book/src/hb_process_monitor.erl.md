# hb_process_monitor

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_process_monitor.erl)

## Exported Functions

- `start/1`
- `start/2`
- `start/3`
- `stop/1`

---

### start

```erlang
start(ProcID) ->
    start(ProcID, hb_opts:get(default_cron_rate)).
```

### start

```erlang
start(ProcID, Rate) ->
    start(ProcID, Rate, hb_client:cron_cursor(ProcID)).
```

### start

```erlang
start(ProcID, Rate, Cursor) ->
    Logger = hb_logger:start(),
    Monitor = spawn(
        fun() ->
            server(
                #state{
                    proc_id = ProcID,
                    cursor = Cursor,
                    logger = Logger
                }
            )
        end),
    Ticker = spawn(fun() -> ticker(Monitor, Rate) end),
    hb_logger:register(Monitor),
    hb_logger:log(Monitor, {ok, started_monitor, {ProcID, Rate, Cursor}}),
    hb_logger:register(Ticker),
    {Monitor, Logger}.
```

### stop

```erlang
stop(PID) ->
    PID ! stop.
```

### server

```erlang
server(State) ->
    receive
        stop -> ok;
        tick ->server(handle_crons(State))
    end.
```

### handle_crons

```erlang
handle_crons(State) ->
    case hb_client:cron(State#state.proc_id, State#state.cursor) of
        {ok, HasNextPage, Results, Cursor} ->
            lists:map(
                fun(Res) ->
                    % TODO: Validate this
                    dev_mu:push(#{ message => Res }, State)
                end,
                Results
            ),
            NS = State#state{cursor = Cursor},
            case HasNextPage of
                true -> NS;
                false -> handle_crons(NS)
            end;
        Error ->
            hb_logger:log(State#state.logger, Error),
            State
    end.
```

### ticker

```erlang
ticker(Monitor, Rate) ->
    case erlang:is_process_alive(Monitor) of
        true ->
            timer:sleep(Rate),
            Monitor ! tick,
            ticker(Monitor, Rate);
        false ->
            ok
```

---

*Generated from [hb_process_monitor.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_process_monitor.erl)*
