# hb_sup

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_sup.erl)

## Exported Functions

- `init/1`
- `start_link/0`
- `start_link/1`

---

### start_link

```erlang
start_link() ->
    start_link(#{}).
```

### start_link

```erlang
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).
```

### init

```erlang
init(Opts) ->
    SupFlags = #{strategy => one_for_all,
                intensity => 0,
                period => 1},
    StoreChildren = store_children(hb_opts:get(store, [], Opts)),
    GunChild =
        #{
            id => hb_http_client,
            start => {hb_http_client, start_link, [Opts]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [hb_http_client]
        },
    {ok, {SupFlags, [GunChild | StoreChildren]}}.
```

### store_children

Generate a child spec for stores in the given Opts.

```erlang
store_children(Store) when not is_list(Store) ->
    store_children([Store]);
```

### store_children

Generate a child spec for stores in the given Opts.

```erlang
store_children([]) -> [];
```

### store_children

Generate a child spec for stores in the given Opts.

```erlang
store_children([RocksDBOpts = #{ <<"store-module">> := hb_store_rocksdb } | Rest]) ->
    [
        #{
            id => hb_store_rocksdb,
            start => {hb_store_rocksdb, start_link, [RocksDBOpts]}
        }
    ] ++ store_children(Rest);
```

### store_children

Generate a child spec for stores in the given Opts.

```erlang
store_children([_ | Rest]) ->
    store_children(Rest).
```

---

*Generated from [hb_sup.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_sup.erl)*
