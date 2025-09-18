# dev_lookup

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_lookup.erl)

A device that looks up an ID from a local store and returns it, honoring
the `accept` key to return the correct format.

---

## Exported Functions

- `read/3`

---

### read

A device that looks up an ID from a local store and returns it, honoring
Fetch a resource from the cache using "target" ID extracted from the message

```erlang
read(_M1, M2, Opts) ->
    ID = hb_ao:get(<<"target">>, M2, Opts),
    ?event({lookup, {id, ID}, {opts, Opts}}),
    case hb_cache:read(ID, Opts) of
        {ok, RawRes} ->
            % We are sending the result over the wire, so make sure it is
            % fully loaded, to save the recipient latency.
```

### binary_lookup_test

```erlang
binary_lookup_test() ->
    Bin = <<"Simple unsigned data item">>,
    {ok, ID} = hb_cache:write(Bin, #{}),
    {ok, RetrievedBin} = read(#{}, #{ <<"target">> => ID }, #{}),
    ?assertEqual(Bin, RetrievedBin).
```

### message_lookup_test

```erlang
message_lookup_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, #{}),
    {ok, RetrievedMsg} = read(#{}, #{ <<"target">> => ID }, #{}),
    ?assert(hb_message:match(Msg, RetrievedMsg)).
```

### aos2_message_lookup_test

```erlang
aos2_message_lookup_test() ->
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, #{}),
    {ok, RetrievedMsg} =
        read(
            #{},
            #{ <<"target">> => ID, <<"accept">> => <<"application/aos-2">> },
            #{}
        ),
    {ok, Decoded} = dev_json_iface:json_to_message(hb_ao:get(<<"body">>, RetrievedMsg, #{}), #{}),
    ?assertEqual(<<"test-data">>, hb_ao:get(<<"data">>, Decoded, #{})).
```

### http_lookup_test

```erlang
http_lookup_test() ->
    Store = #{
        <<"store-module">> => hb_store_fs,
        <<"name">> => <<"cache-mainnet">>
    },
    Opts = #{ store => [Store] },
    Msg = #{ <<"test-key">> => <<"test-value">>, <<"data">> => <<"test-data">> },
    {ok, ID} = hb_cache:write(Msg, Opts),
    Node = hb_http_server:start_node(Opts),
    Wallet = hb:wallet(),
    Req = hb_message:commit(#{
        <<"path">> => <<"/~lookup@1.0/read?target=", ID/binary>>,
        <<"device">> => <<"lookup@1.0">>,
        <<"accept">> => <<"application/aos-2">>
    }, Wallet),
    {ok, Res} = hb_http:post(Node, Req, Opts),
    {ok, Decoded} = dev_json_iface:json_to_message(hb_ao:get(<<"body">>, Res, Opts), Opts),
```

---

*Generated from [dev_lookup.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_lookup.erl)*
