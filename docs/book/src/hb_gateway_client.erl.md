# hb_gateway_client

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_gateway_client.erl)

Implementation of Arweave's GraphQL API to gain access to specific 
items of data stored on the network.
This module must be used to get full HyperBEAM `structured@1.0` form messages
from data items stored on the network, as Arweave gateways do not presently
expose all necessary fields to retrieve this information outside of the
GraphQL API. When gateways integrate serving in `httpsig@1.0` form, this
module will be deprecated.

---

## Exported Functions

- `data/2`
- `item_spec/0`
- `query/2`
- `query/3`
- `query/4`
- `query/5`
- `read/2`
- `result_to_message/2`
- `scheduler_location/2`

---

### read

Implementation of Arweave's GraphQL API to gain access to specific 
Get a data item (including data and tags) by its ID, using the node's

```erlang
read(ID, Opts) ->
    {Query, Variables} = case maps:is_key(<<"subindex">>, Opts) of
      true -> 
        Tags = subindex_to_tags(maps:get(<<"subindex">>, Opts)),
        {
            <<
                "query($transactionIds: [ID!]!) { ",
                    "transactions(ids: $transactionIds,",
                    "tags: ", (Tags)/binary , ",",
                    "first: 1){ ",
                        "edges { ", (item_spec())/binary , " } ",
                    "} ",
                "} "
            >>,
            #{
                <<"transactionIds">> => [hb_util:human_id(ID)]
            }
        };
      false -> 
        {
            <<
                "query($transactionIds: [ID!]!) { ",
                    "transactions(ids: $transactionIds, first: 1){ ",
                        "edges { ", (item_spec())/binary , " } ",
                    "} ",
                "} "
            >>,
            #{
                <<"transactionIds">> => [hb_util:human_id(ID)]
            }
        }
    end,
    case query(Query, Variables, Opts) of
        {error, Reason} -> {error, Reason};
        {ok, GqlMsg} ->
            case hb_ao:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found ->
                    ?event({read_not_found, {id, ID}, {gql_msg, GqlMsg}}),
                    {error, not_found};
                Item ->
                    ?event({read_found, {id, ID}, {item, Item}}),
                    result_to_message(ID, Item, Opts)
            end
    end.
```

### item_spec

Gives the fields of a transaction that are needed to construct an
Get the data associated with a transaction by its ID, using the node's

```erlang
item_spec() ->
    <<"""
        node {
            id
            anchor
            signature
            recipient
            owner { key }
            fee { winston }
            quantity { winston }
            tags { name value }
            data { size }
        }
        cursor
    """>>.
```

### data

Gives the fields of a transaction that are needed to construct an
Get the data associated with a transaction by its ID, using the node's

```erlang
data(ID, Opts) ->
    Req = #{
        <<"multirequest-accept-status">> => 200,
        <<"multirequest-responses">> => 1,
        <<"path">> => <<"/raw/", ID/binary>>,
        <<"method">> => <<"GET">>
    },
    case hb_http:request(Req, Opts) of
        {ok, Res} ->
            ?event(gateway,
                {data,
                    {id, ID},
                    {response, Res},
                    {body, hb_ao:get(<<"body">>, Res, <<>>, Opts)}
                }
            ),
            {ok, hb_ao:get(<<"body">>, Res, <<>>, Opts)};
        Res ->
            ?event(gateway, {request_error, {id, ID}, {response, Res}}),
            {error, no_viable_gateway}
    end.
```

### scheduler_location

Find the location of the scheduler based on its ID, through GraphQL.

```erlang
scheduler_location(Address, Opts) ->
    Query =
        <<"query($SchedulerAddrs: [String!]!) { ",
                "transactions(",
                "owners: $SchedulerAddrs, ",
                "tags: { name: \"Type\" values: [\"Scheduler-Location\"] }, ",
                "first: 1",
            "){ ",
                "edges { ",
                    (item_spec())/binary ,
                " } ",
            "} ",
        "}">>,
    Variables = #{ <<"SchedulerAddrs">> => [Address] },
    case query(Query, Variables, Opts) of
        {error, Reason} ->
            ?event({scheduler_location, {query, Query}, {error, Reason}}),
            {error, Reason};
        {ok, GqlMsg} ->
            ?event({scheduler_location_req, {query, Query}, {response, GqlMsg}}),
            case hb_ao:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found ->
                    ?event(scheduler_location,
                        {graphql_scheduler_location_not_found,
                            {address, Address}
                        }
                    ),
                    {error, not_found};
                Item = #{ <<"id">> := ID } ->
                    ?event(scheduler_location,
                        {found_via_graphql,
                            {address, Address},
                            {id, ID}
                        }
                    ),
                    result_to_message(ID, Item, Opts)
            end
    end.
```

### query

Run a GraphQL request encoded as a binary. The node message may contain 

```erlang
query(Query, Opts) ->
    query(Query, undefined, Opts).
```

### query

```erlang
query(Query, Variables, Opts) ->
    query(Query, Variables, undefined, Opts).
```

### query

```erlang
query(Query, Variables, Node, Opts) ->
    query(Query, Variables, Node, undefined, Opts).
```

### query

```erlang
query(Query, Variables, Node, Operation, Opts) ->
    % Either use the given node if provided, or use the local machine's routes
    % to find the GraphQL endpoint.
```

### result_to_message

Takes a GraphQL item node, matches it with the appropriate data from a

```erlang
result_to_message(Item, Opts) ->
    case hb_maps:get(<<"id">>, Item, not_found, Opts) of
        ExpectedID when is_binary(ExpectedID) ->
            result_to_message(ExpectedID, Item, Opts);
        _ ->
            result_to_message(undefined, Item, Opts)
    end.
```

### result_to_message

```erlang
result_to_message(ExpectedID, Item, Opts) ->
    GQLOpts =
        Opts#{
            hashpath => ignore,
            cache_control => [<<"no-cache">>, <<"no-store">>]
        },
    % We have the headers, so we can get the data.
```

### normalize_null

```erlang
normalize_null(null) -> <<>>;
```

### normalize_null

```erlang
normalize_null(not_found) -> <<>>;
```

### normalize_null

```erlang
normalize_null(Bin) when is_binary(Bin) -> Bin.
```

### decode_id_or_null

```erlang
decode_id_or_null(Bin) when byte_size(Bin) > 0 ->
    hb_util:human_id(Bin);
```

### decode_id_or_null

```erlang
decode_id_or_null(_) ->
    <<>>.
```

### decode_or_null

```erlang
decode_or_null(Bin) when is_binary(Bin) ->
    hb_util:decode(Bin);
```

### decode_or_null

```erlang
decode_or_null(_) ->
    <<>>.
```

### subindex_to_tags

Takes a list of messages with `name` and `value` fields, and formats

```erlang
subindex_to_tags(Subindex) ->
    Formatted =
        lists:map(
            fun(Spec) ->
                io_lib:format(
                    "{ name: \"~s\", values: [\"~s\"]}",
                    [
                        hb_ao:get(<<"name">>, Spec),
                        hb_ao:get(<<"value">>, Spec)
                    ]
                )
            end,
            hb_util:message_to_ordered_list(Subindex)
        ),
    ListInner =
        hb_util:bin(
            string:join([lists:flatten(E) || E <- Formatted], ", ")
        ),
    <<"[", ListInner/binary, "]">>.
%%% Tests
```

### ans104_no_data_item_test

Takes a list of messages with `name` and `value` fields, and formats

```erlang
ans104_no_data_item_test() ->
    % Start a random node so that all of the services come up.
```

### scheduler_location_test

Test that we can get the scheduler location.

```erlang
scheduler_location_test() ->
    % Start a random node so that all of the services come up.
```

### l1_transaction_test

Test l1 message from graphql
Test l2 message from graphql

```erlang
l1_transaction_test() ->
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"uJBApOt4ma3pTfY6Z4xmknz5vAasup4KcGX7FJ0Of8w">>, #{}),
    ?event(gateway, {l1_transaction, Res}),
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"Hello World">>, Data).
```

### l2_dataitem_test

Test l1 message from graphql
Test l2 message from graphql
Test optimistic index

```erlang
l2_dataitem_test() ->
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"oyo3_hCczcU7uYhfByFZ3h0ELfeMMzNacT-KpRoJK6g">>, #{}),
    ?event(gateway, {l2_dataitem, Res}),
    Data = maps:get(<<"data">>, Res),
    ?assertEqual(<<"Hello World">>, Data).
```

### ao_dataitem_test

Test l1 message from graphql
Test l2 message from graphql
Test optimistic index

```erlang
ao_dataitem_test() ->
    _Node = hb_http_server:start_node(#{}),
    {ok, Res} = read(<<"oyo3_hCczcU7uYhfByFZ3h0ELfeMMzNacT-KpRoJK6g">>, #{ }),
    ?event(gateway, {l2_dataitem, Res}),
    Data = maps:get(<<"data">>, Res),
```

---

*Generated from [hb_gateway_client.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/hb_gateway_client.erl)*
