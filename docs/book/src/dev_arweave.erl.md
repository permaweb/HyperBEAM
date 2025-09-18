# dev_arweave

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_arweave.erl)

A device that provides access to Arweave network information, relayed
from a designated node.
The node(s) that are used to query data may be configured by altering the
`/arweave` route in the node`s configuration message.

---

## Exported Functions

- `block/3`
- `current/3`
- `status/3`
- `tx/3`

---

### status

A device that provides access to Arweave network information, relayed
Proxy the `/info` endpoint from the Arweave node.
Returns the given transaction, if known to the client node(s), as an

```erlang
status(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/info">>, Opts).
```

### tx

A device that provides access to Arweave network information, relayed
Proxy the `/info` endpoint from the Arweave node.
Returns the given transaction, if known to the client node(s), as an

```erlang
tx(Base, Request, Opts) ->
    case hb_maps:get(<<"method">>, Request, <<"GET">>, Opts) of
        <<"POST">> -> post_tx(Base, Request, Opts);
        <<"GET">> -> get_tx(Base, Request, Opts)
    end.
```

### post_tx

Upload a transaction to Arweave, using the node's default bundler (see

```erlang
post_tx(_Base, Request, Opts) ->
    case hb_client:upload(Request, Opts) of
        Res = {ok, _} ->
            ?event(arweave, {uploaded, Request}),
            CacheRes = hb_cache:write(Request, Opts),
            ?event(arweave,
                {cache_uploaded_message,
                    {msg, Request},
                    {status,
                        case CacheRes of {ok, _} -> ok;
                        _ -> failed
                        end
                    }
                }
            ),
            Res;
        Res ->
            Res
    end.
```

### get_tx

Get a transaction ID from the Arweave node, as indicated by the `tx` key

```erlang
get_tx(Base, Request, Opts) ->
    case find_txid(Base, Request, Opts) of
        not_found -> {error, not_found};
        TXID ->
            case request(<<"GET">>, <<"/tx/", TXID/binary>>, Opts) of
                {ok, TXHeader} ->
                    ?event(arweave, {retrieved_tx_header, {tx, TXID}}),
                    maybe_add_data(TXID, TXHeader, Base, Request, Opts);
                Other -> Other
            end
    end.
```

### maybe_add_data

Handle the optional adding of data to the transaction header, depending

```erlang
maybe_add_data(TXID, Header, Base, Request, Opts) ->
    GetData =
        hb_util:atom(hb_ao:get_first(
            [
                {Request, <<"data">>},
                {Base, <<"data">>}
            ],
            true,
            Opts
        )),
    case hb_util:atom(GetData) of
        false ->
            {ok, Header};
        _ ->
            case data(Base, Request, Opts) of
                {ok, Data} ->
                    FullMessage = Header#{ <<"data">> => Data },
                    ?event(
                        arweave,
                        {retrieved_tx_with_data,
                            {id, TXID},
                            {data_size, byte_size(Data)},
                            {message, FullMessage}
                        }
                    ),
                    {ok, FullMessage};
                {error, Reason} ->
                    ?event(arweave,
                        {data_retrieval_failed_after_header,
                            {id, TXID},
                            {error, Reason}
                        }
                    ),
                    if GetData =/= always -> {ok, Header};
                    true -> {error, Reason}
                    end
            end
    end.
```

### data

Retrieve the data of a transaction from Arweave.

```erlang
data(Base, Request, Opts) ->
    case find_txid(Base, Request, Opts) of
        not_found -> {error, not_found};
        TXID ->
            ?event(arweave, {retrieving_tx_data, {tx, TXID}}),
            request(<<"GET">>, <<"/raw/", TXID/binary>>, Opts)
    end.
```

### block

Retrieve (and cache) block information from Arweave. If the `block` key

```erlang
block(Base, Request, Opts) ->
    Block =
        hb_ao:get_first(
            [
                {Request, <<"block">>},
                {Base, <<"block">>}
            ],
            not_found,
            Opts
        ),
    case Block of
        <<"current">> -> current(Base, Request, Opts);
        not_found -> current(Base, Request, Opts);
        ID when ?IS_ID(ID) -> block({id, ID}, Opts);
        MaybeHeight ->
            try hb_util:int(MaybeHeight) of
              Int -> block({height, Int}, Opts)
            catch
                _:_ ->
                    {
                        error,
                        <<"Invalid block reference `", MaybeHeight/binary, "`">>
                    }
            end
    end.
```

### block

```erlang
block({id, ID}, Opts) ->
    case hb_cache:read(ID, Opts) of
        {ok, Block} ->
            ?event(arweave, {retrieved_block_from_cache, {id, ID}}),
            {ok, Block};
        not_found ->
            request(<<"GET">>, <<"/block/hash/", ID/binary>>, Opts)
    end;
```

### block

```erlang
block({height, Height}, Opts) ->
    case dev_arweave_block_cache:read(Height, Opts) of
        {ok, Block} ->
            ?event(arweave, {retrieved_block_from_cache, {height, Height}}),
            {ok, Block};
        not_found ->
            request(
                <<"GET">>,
                <<"/block/height/", (hb_util:bin(Height))/binary>>,
                Opts
            )
    end.
```

### current

Retrieve the current block information from Arweave.
Find the transaction ID to retrieve from Arweave based on the request or

```erlang
current(_Base, _Request, Opts) ->
    request(<<"GET">>, <<"/block/current">>, Opts).
%%% Internal Functions
```

### find_txid

Retrieve the current block information from Arweave.
Find the transaction ID to retrieve from Arweave based on the request or

```erlang
find_txid(Base, Request, Opts) ->
    hb_ao:get_first(
        [
            {Request, <<"tx">>},
            {Base, <<"tx">>}
        ],
        not_found,
        Opts
    ).
```

### request

Make a request to the Arweave node and parse the response into an

```erlang
request(Method, Path, Opts) ->
    ?event(arweave, {arweave_request, {method, Method}, {path, Path}}),
    Res =
        hb_http:request(
            #{
                <<"path">> => <<"/arweave", Path/binary>>,
                <<"method">> => Method
            },
            Opts
        ),
    to_message(Path, Res, Opts).
```

### to_message

Transform a response from the Arweave node into an AO-Core message.

```erlang
to_message(Path = <<"/raw/", _/binary>>, {ok, #{ <<"body">> := Body }}, _Opts) ->
    ?event(arweave,
        {arweave_raw_response,
            {path, Path},
            {data_size, byte_size(Body)}
        }
    ),
    {ok, Body};
```

### to_message

Transform a response from the Arweave node into an AO-Core message.

```erlang
to_message(Path = <<"/block/", _/binary>>, {ok, #{ <<"body">> := Body }}, Opts) ->
    Block = hb_message:convert(Body, <<"structured@1.0">>, <<"json@1.0">>, Opts),
    ?event(arweave,
        {arweave_block_response,
            {path, Path},
            {block, Block}
        }
    ),
    CacheRes = dev_arweave_block_cache:write(Block, Opts),
    ?event(arweave,
        {cached_arweave_block,
            {path, Path},
            {result, CacheRes}
        }
    ),
    {ok, Block};
```

### to_message

Transform a response from the Arweave node into an AO-Core message.

```erlang
to_message(Path, {ok, #{ <<"body">> := Body }}, Opts) ->
    % All other responses that are `OK' status are converted from JSON to an
    % AO-Core message.
```

### post_ans104_tx_test

```erlang
post_ans104_tx_test() ->
    ServerOpts = #{ store => [hb_test_utils:test_store()] },
    Server = hb_http_server:start_node(ServerOpts),
    ClientOpts =
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => hb:wallet()
        },
    Msg =
        hb_message:commit(
            #{
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Process">>,
                <<"data">> => <<"test-data">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    {ok, PostRes} =
        hb_http:post(
            Server,
            Msg#{
                <<"path">> => <<"/~arweave@2.9-pre/tx">>,
                <<"codec-device">> => <<"ans104@1.0">>
            },
            ClientOpts
        ),
    ?assertMatch(#{ <<"status">> := 200 }, PostRes),
    SignedID = hb_message:id(Msg, signed, ClientOpts),
    {ok, GetRes} =
        hb_http:get(
            Server, <<"/", SignedID/binary>>,
            ClientOpts
        ),
    ?assertMatch(
        #{
            <<"status">> := 200,
            <<"variant">> := <<"ao.N.1">>,
            <<"type">> := <<"Process">>,
            <<"data">> := <<"test-data">>
        },
        GetRes
    ),
```

---

*Generated from [dev_arweave.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_arweave.erl)*
