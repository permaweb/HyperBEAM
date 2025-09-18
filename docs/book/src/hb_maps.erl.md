# hb_maps

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_maps.erl)

An abstraction for working with maps in HyperBEAM, matching the
generic `maps` module, but additionally supporting the resolution of
links as they are encountered. These functions must be used extremely
carefully. In virtually all circumstances, the `hb_ao:resolve/3` or
`hb_ao:get/3` functions should be used instead, as they will execute the
full AO-Core protocol upon requests (normalizing keys, applying the
appropriate device's functions, as well as resolving links). By using this
module's functions, you are implicitly making the assumption that the message
in question is of the `~message@1.0` form, ignoring any other keys that its
actual device may present. This module is intended for the extremely rare
circumstances in which the additional overhead of the full AO-Core
execution cycle is not acceptable, and the data in question is known to
conform to the `~message@1.0` form.
If you do not understand any/all of the above, you are in the wrong place!
Utilise the `hb_ao` module and read the documentation therein, saving
yourself from the inevitable issues that will arise from using this
module without understanding the full implications. You have been warned.

---

## Exported Functions

- `filter/2`
- `filter/3`
- `filtermap/2`
- `filtermap/3`
- `find/2`
- `find/3`
- `fold/3`
- `fold/4`
- `from_list/1`
- `get/2`
- `get/3`
- `get/4`
- `is_key/2`
- `is_key/3`
- `keys/1`
- `keys/2`
- `map/2`
- `map/3`
- `merge/2`
- `merge/3`
- `put/3`
- `put/4`
- `remove/2`
- `remove/3`
- `size/1`
- `size/2`
- `take/2`
- `take/3`
- `to_list/1`
- `to_list/2`
- `update_with/3`
- `update_with/4`
- `values/1`
- `values/2`
- `with/2`
- `with/3`
- `without/2`
- `without/3`

---

### get

Get a value from a map, resolving links as they are encountered in both

```erlang
-spec get(
    Key :: term(),
    Map :: map(),
    Default :: term(),
    Opts :: map()
) -> term().
```

```erlang
get(Key, Map, Default, Opts) ->
    hb_cache:ensure_loaded(
        maps:get(
            Key,
            hb_cache:ensure_loaded(Map, Opts),
            Default
        ),
        Opts
    ).
```

### put

```erlang
-spec put(
	Key :: term(),
	Value :: term(),
	Map :: map(),
	Opts :: map()
) -> map().
```

```erlang
put(Key, Value, Map, Opts) ->
    maps:put(Key, Value, hb_cache:ensure_loaded(Map, Opts)).
```

### map

```erlang
-spec map(
    Fun :: fun((Key :: term(), Value :: term()) -> term()),
    Map :: map()
) -> map().
```

```erlang
map(Fun, Map) ->
    map(Fun, Map, #{}).
```

### map

```erlang
-spec map(
    Fun :: fun((Key :: term(), Value :: term()) -> term()),
    Map :: map(),
    Opts :: map()
) -> map().
```

```erlang
map(Fun, Map, Opts) ->
    maps:map(
        fun(K, V) -> Fun(K, hb_cache:ensure_loaded(V, Opts)) end,
        hb_cache:ensure_loaded(Map, Opts)
    ).
```

### filter

```erlang
-spec filter(
    Fun :: fun((Key :: term(), Value :: term()) -> boolean()),
    Map :: map()
) -> map().
```

```erlang
filter(Fun, Map) ->
    filter(Fun, Map, #{}).
```

### filter

```erlang
-spec filter(
    Fun :: fun((Key :: term(), Value :: term()) -> boolean()),
    Map :: map(),
    Opts :: map()
) -> map().
```

```erlang
filter(Fun, Map, Opts) ->
    maps:filtermap(
        fun(K, V) ->
            case Fun(K, Loaded = hb_cache:ensure_loaded(V, Opts)) of
                true -> {true, Loaded};
                false -> false
            end
        end,
        hb_cache:ensure_loaded(Map, Opts)
    ).
```

### filtermap

```erlang
-spec filtermap(
    Fun :: fun((Key :: term(), Value :: term()) -> {boolean(), term()}),
    Map :: map()
) -> map().
```

```erlang
filtermap(Fun, Map) ->
    filtermap(Fun, Map, #{}).
```

### filtermap

```erlang
-spec filtermap(
    Fun :: fun((Key :: term(), Value :: term()) -> {boolean(), term()}),
    Map :: map(),
    Opts :: map()
) -> map().
```

```erlang
filtermap(Fun, Map, Opts) ->
    maps:filtermap(
        fun(K, V) -> Fun(K, hb_cache:ensure_loaded(V, Opts)) end,
        hb_cache:ensure_loaded(Map, Opts)
    ).
```

### fold

```erlang
-spec fold(
    Fun :: fun((Key :: term(), Value :: term(), Acc :: term()) -> term()),
    Acc :: term(),
    Map :: map()
) -> term().
```

```erlang
fold(Fun, Acc, Map) ->
    fold(Fun, Acc, Map, #{}).
```

### fold

```erlang
-spec fold(
    Fun :: fun((Key :: term(), Value :: term(), Acc :: term()) -> term()),
    Acc :: term(),
    Map :: map(),
    Opts :: map()
) -> term().
```

```erlang
fold(Fun, Acc, Map, Opts) ->
    maps:fold(
        fun(K, V, CurrAcc) -> Fun(K, hb_cache:ensure_loaded(V, Opts), CurrAcc) end,
        Acc,
        hb_cache:ensure_loaded(Map, Opts)
    ).
```

### update_with

```erlang
-spec update_with(
    Key :: term(),
    Fun :: fun((Value :: term()) -> term()),
    Map :: map()
) -> map().
```

```erlang
update_with(Key, Fun, Map) ->
    update_with(Key, Fun, Map, #{}).
```

### update_with

```erlang
-spec update_with(
    Key :: term(),
    Fun :: fun((Value :: term()) -> term()),
    Map :: map(),
    Opts :: map()
) -> map().
```

```erlang
update_with(Key, Fun, Map, Opts) ->
    maps:update_with(Key, Fun, hb_cache:ensure_loaded(Map, Opts), Opts).
```

### get_with_link_test

```erlang
-spec to_list(Map :: map(), Opts :: map()) -> [{Key :: term(), Value :: term()}].
to_list(Map, Opts) ->
    maps:to_list(hb_cache:ensure_loaded(Map, Opts)).
```

```erlang
get_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(Bin, get(2, Map)).
```

### map_with_link_test

```erlang
map_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(#{1 => 1, 2 => Bin, 3 => 3}, map(fun(_K, V) -> V end, Map, #{})).
```

### get_with_typed_link_test

```erlang
get_with_typed_link_test() ->
    Bin = <<"123">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{ <<"type">> => integer }}, 3 => 3 },
    ?assertEqual(123, get(2, Map, undefined)).
```

### resolve_on_link_test

```erlang
resolve_on_link_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">> },
    Opts = #{},
    {ok, ID} = hb_cache:write(Msg, Opts),
    ?assertEqual(
        {ok, <<"test-value">>},
        hb_ao:resolve({link, ID, #{}}, <<"test-key">>, #{})
    ).
```

### filter_with_link_test

```erlang
filter_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(#{1 => 1, 3 => 3}, filter(fun(_, V) -> V =/= Bin end, Map)).
```

### filtermap_with_link_test

```erlang
filtermap_with_link_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(
        #{2 => <<"FOUND">>},
        filtermap(
            fun(_, <<"TEST DATA">>) -> {true, <<"FOUND">>};
               (_K, _V) -> false
            end,
            Map
        )
    ).
```

### fold_with_typed_link_test

```erlang
fold_with_typed_link_test() ->
    Bin = <<"123">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{ <<"type">> => integer }}, 3 => 3 },
    ?assertEqual(127, fold(fun(_, V, Acc) -> V + Acc end, 0, Map)).
```

### filter_passively_loads_test

```erlang
filter_passively_loads_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(
        #{1 => 1, 2 => <<"TEST DATA">>, 3 => 3},
        filter(fun(_, _) -> true end, Map)
    ).
```

### filtermap_passively_loads_test

```erlang
filtermap_passively_loads_test() ->
    Bin = <<"TEST DATA">>,
    Opts = #{},
    {ok, Location} = hb_cache:write(Bin, Opts),
    Map = #{ 1 => 1, 2 => {link, Location, #{}}, 3 => 3 },
    ?assertEqual(
        #{ 1 => 1, 2 => <<"TEST DATA">>, 3 => 3 },
        filtermap(fun(_, V) -> {true, V} end, Map)
```

---

*Generated from [hb_maps.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_maps.erl)*
