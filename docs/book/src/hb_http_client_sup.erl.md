# hb_http_client_sup

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_http_client_sup.erl)

The supervisor for the gun HTTP client wrapper.

---

## Exported Functions

- `init/1`
- `start_link/1`

---

### start_link

The supervisor for the gun HTTP client wrapper.

```erlang
start_link(Opts) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).
```

### init

```erlang
init(Opts) ->
	{ok, {{one_for_one, 5, 10}, [?CHILD(hb_http_client, worker, Opts)]}}.
```

---

*Generated from [hb_http_client_sup.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_http_client_sup.erl)*
