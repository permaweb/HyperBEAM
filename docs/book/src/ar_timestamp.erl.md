# ar_timestamp

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/ar_timestamp.erl)

A simple Erlang server that caches the current Arweave timestamp and
refreshes it periodically.

---

## Exported Functions

- `get/0`
- `start/0`

---

### start

Check if the server is already running, and if not, start it.

```erlang
start() ->
    ?event(starting_ar_timestamp_server),
    case whereis(?MODULE) of
        undefined -> spawn_server();
        PID ->
            case is_process_alive(PID) of
                true -> PID;
                false -> spawn_server()
            end
    end.
```

### spawn_server

Spawn a new server and its refresher.

```erlang
spawn_server() ->
    TSServer =
        spawn(fun() -> cache(hb_client:arweave_timestamp()) end),
    spawn(fun() -> refresher(TSServer) end),
    register(?MODULE, TSServer),
    TSServer.
```

### get

Get the current timestamp from the server, starting the server if it

```erlang
get() ->
    ?event(getting_ar_timestamp),
    PID = start(),
    ?event({got_ar_timestamp_pid, PID}),
    PID ! {get, self()},
    ?event(waiting_for_ar_timestamp),
    receive
        {timestamp, Timestamp} ->
            ?event({got_ar_timestamp, Timestamp}),
            Timestamp
    end.
```

### cache

Cache the current timestamp from Arweave.

```erlang
cache(Current) ->
    ?event(cache_waiting),
    receive
        {get, Pid} ->
            ?event({got_get_request, Pid}),
            Pid ! {timestamp, Current},
            ?event({sent_timestamp, Current}),
            cache(Current);
        {refresh, New} ->
            ?event({refreshed_ar_timestamp, New}),
            cache(New)
    end.
```

### refresher

Refresh the timestamp cache periodically.

```erlang
refresher(TSServer) ->
    timer:sleep(?TIMEOUT),
    TS = hb_client:arweave_timestamp(),
    TSServer ! {refresh, TS},
```

---

*Generated from [ar_timestamp.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/ar_timestamp.erl)*
