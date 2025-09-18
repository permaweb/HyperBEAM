# hb_store_remote_node

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_remote_node.erl)

A store module that reads data from another AO node.
Notably, this store only provides the _read_ side of the store interface.
The write side could be added, returning an commitment that the data has
been written to the remote node. In that case, the node would probably want
to upload it to an Arweave bundler to ensure persistence, too.

---

## Exported Functions

- `make_link/3`
- `maybe_cache/2`
- `maybe_cache/3`
- `read/2`
- `resolve/2`
- `scope/1`
- `type/2`
- `write/3`

---

### scope

A store module that reads data from another AO node.
Return the scope of this store.

```erlang
scope(_StoreOpts) ->
    remote.
```

### resolve

Resolve a key path in the remote store.

```erlang
resolve(#{ <<"node">> := Node }, Key) ->
    ?event({remote_resolve, {node, Node}, {key, Key}}),
    Key.
```

### type

Determine the type of value at a given key.

```erlang
type(Opts = #{ <<"node">> := Node }, Key) ->
    ?event({remote_type, {node, Node}, {key, Key}}),
    case read(Opts, Key) of
        not_found -> not_found;
        _ -> simple
    end.
```

### read

Read a key from the remote node.

```erlang
read(Opts = #{ <<"node">> := Node }, Key) ->
    ?event(store_remote_node, {executing_read, {node, Node}, {key, Key}}),
    HTTPRes =
        hb_http:get(
            Node,
            #{ <<"path">> => <<"/~cache@1.0/read">>, <<"target">> => Key },
            Opts
        ),
    case HTTPRes of
        {ok, Res} ->
            % returning the whole response to get the test-key
            {ok, Msg} = hb_message:with_only_committed(Res, Opts),
            ?event(store_remote_node, {read_found, {result, Msg, response, Res}}),
            maybe_cache(Opts, Msg, [Key]),
            {ok, Msg};
        {error, _Err} ->
            ?event(store_remote_node, {read_not_found, {key, Key}}),
            not_found
    end.
```

### maybe_cache

Cache the data if the cache is enabled. The `local-store` option may

```erlang
maybe_cache(StoreOpts, Data) ->
    maybe_cache(StoreOpts, Data, []).
```

### maybe_cache

```erlang
maybe_cache(StoreOpts, Data, Links) ->
    ?event({maybe_cache, StoreOpts, Data}),
    % Check if the local store is in our store options.
```

### write

Write a key to the remote node.

```erlang
write(Opts = #{ <<"node">> := Node }, Key, Value) ->
    ?event({write, {node, Node}, {key, Key}, {value, Value}}),
    WriteMsg = #{
        <<"path">> => <<"/~cache@1.0/write">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Value
    },
    SignedMsg = hb_message:commit(WriteMsg, Opts),
    ?event({write, {signed, SignedMsg}}),
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} ->
            Status = hb_ao:get(<<"status">>, Response, 0, #{}),
            ?event(store_remote_node, {write_completed, {response, Response}}),
            case Status of
                200 -> ok;
                _ -> {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            ?event({write, {error, Err}}),
            {error, Err}
    end.
```

### make_link

Link a source to a destination in the remote node.

```erlang
make_link(Opts = #{ <<"node">> := Node }, Source, Destination) ->
    ?event({make_remote_link, {node, Node}, {source, Source},
                                  {destination, Destination}}),
    LinkMsg = #{
        <<"path">> => <<"/~cache@1.0/link">>,
        <<"method">> => <<"POST">>,
        <<"source">> => Source,
        <<"destination">> => Destination
    },
    SignedMsg = hb_message:commit(LinkMsg, Opts),
    ?event({make_remote_link, {signed, SignedMsg}}),
    case hb_http:post(Node, SignedMsg, Opts) of
        {ok, Response} ->
            Status = hb_ao:get(<<"status">>, Response, 0, #{}),
            ?event(store_remote_node, {make_link_completed, {response, Response}}),
            case Status of
                200 -> ok;
                _ -> {error, {unexpected_status, Status}}
            end;
        {error, Err} ->
            ?event(store_remote_node, {make_link_error, {error, Err}}),
            {error, Err}
    end.
```

### read_test

Test that we can create a store, write a random message to it, then

```erlang
read_test() ->
    rand:seed(default),
    LocalStore = #{ 
		<<"store-module">> => hb_store_fs,
		<<"name">> => <<"cache-mainnet">>
	},
    hb_store:reset(LocalStore),
    M = #{ <<"test-key">> => Rand = rand:uniform(1337) },
    ID = hb_message:id(M),
    {ok, ID} =
        hb_cache:write(
			M, 
			#{ store => LocalStore }
		),
    ?event({wrote, ID}),
    Node =
        hb_http_server:start_node(
            #{
                store => LocalStore
            }
        ),
    RemoteStore = [
		#{ <<"store-module">> => hb_store_remote_node, <<"node">> => Node }
	],
    {ok, RetrievedMsg} = hb_cache:read(ID, #{ store => RemoteStore }),
```

---

*Generated from [hb_store_remote_node.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_store_remote_node.erl)*
