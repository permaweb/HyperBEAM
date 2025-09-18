# hb_features

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_features.erl)

A module that exports a list of feature flags that the node supports
using the `-ifdef` macro.
As a consequence, this module acts as a proxy of information between the
build system and the runtime execution environment.

---

## Exported Functions

- `all/0`
- `eflame/0`
- `enabled/1`
- `genesis_wasm/0`
- `http3/0`
- `rocksdb/0`
- `test/0`

---

### all

A module that exports a list of feature flags that the node supports
Returns a list of all feature flags that the node supports.

```erlang
all() ->
    Features =
        lists:filtermap(
            fun({Name, _}) ->
                case lists:member(Name, [all, enabled, module_info]) of
                    true -> false;
                    false -> {true, Name}
                end
            end,
            ?MODULE:module_info(exports)
        ),
    hb_maps:from_list(
        lists:map(
            fun(Name) ->
                {Name, ?MODULE:Name()}
            end,
            Features
        )
    ).
```

### enabled

Returns true if the feature flag is enabled.

```erlang
enabled(Feature) ->
    hb_maps:get(Feature, all(), false).
```

### http3

```erlang
http3() -> true.
-else.
```

### http3

```erlang
http3() -> false.
-endif.
```

### rocksdb

```erlang
rocksdb() -> true.
-else.
```

### rocksdb

```erlang
rocksdb() -> false.
-endif.
```

### genesis_wasm

```erlang
genesis_wasm() -> true.
-else.
```

### genesis_wasm

```erlang
genesis_wasm() -> false.
-endif.
```

### eflame

```erlang
eflame() -> true.
-else.
```

### eflame

```erlang
eflame() -> false.
-endif.
```

### test

```erlang
test() -> true.
-else.
```

### test

```erlang
test() -> false.
-endif.
```

---

*Generated from [hb_features.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_features.erl)*
