# hb_app

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_app.erl)

The main HyperBEAM application module.

---

## Exported Functions

- `start/2`
- `stop/1`

---

### start

The main HyperBEAM application module.

```erlang
start(_StartType, _StartArgs) ->
    hb:init(),
    hb_sup:start_link(),
    ok = dev_scheduler_registry:start(),
    _TimestampServer = ar_timestamp:start(),
    {ok, _} = hb_http_server:start().
```

### stop

```erlang
stop(_State) ->
```

---

*Generated from [hb_app.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_app.erl)*
