# hb_router

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_router.erl)

Locate a service in the AO network. This module uses
URLs to locate services, so it can be used to locate
nodes using IP addresses or domain names. This also 
allows us to use different protocols later, potentially.

---

## Exported Functions

- `find/2`
- `find/3`

---

### find

```erlang
find(Type, ID) ->
    find(Type, ID, '_').
```

### find

```erlang
find(Type, ID, Address) ->
	find(Type, ID, Address, #{}).
```

### find

```erlang
find(Type, _ID, Address, Opts) ->
    case hb_maps:get(Type, hb_opts:get(nodes), undefined, Opts) of
        #{ Address := Node } -> {ok, Node};
        undefined -> {error, service_type_not_found}
```

---

*Generated from [hb_router.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_router.erl)*
