# dev_copycat_graphql

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_copycat_graphql.erl)

A `~copycat@1.0` engine that fetches data from a GraphQL endpoint for
replication.

---

## Exported Functions

- `graphql/3`

---

### graphql

A `~copycat@1.0` engine that fetches data from a GraphQL endpoint for
Takes a GraphQL query, optionally with a node address, and curses through

```erlang
graphql(Base, Req, Opts) ->
    case parse_query(Base, Req, Opts) of
        {ok, Query} ->
            Node = maps:get(<<"node">>, Opts, undefined),
            OpName = hb_maps:get(<<"operationName">>, Req, undefined, Opts),
            Vars = hb_maps:get(<<"variables">>, Req, #{}, Opts),
            index_graphql(0, Query, Vars, Node, OpName, Opts);
        Other ->
            Other
    end.
```

### index_graphql

Index a GraphQL query into the node's caches.

```erlang
index_graphql(Total, Query, Vars, Node, OpName, Opts) ->
    maybe
        ?event(
            {graphql_run_called,
                {query, {string, Query}},
                {operation, OpName},
                {variables, Vars}
            }
        ),
        {ok, RawRes} ?= hb_gateway_client:query(Query, Vars, Node, OpName, Opts),
        Res = hb_util:deep_get(<<"data/transactions">>, RawRes, #{}, Opts),
        NodeStructs = hb_util:deep_get(<<"edges">>, Res, [], Opts),
        ?event({graphql_request_returned_items, length(NodeStructs)}),
        ?event(
            {graphql_indexing_responses,
                {query, {string, Query}},
                {variables, Vars},
                {result, Res}
            }
        ),
        ParsedMsgs =
            lists:filtermap(
                fun(NodeStruct) ->
                    Struct = hb_maps:get(<<"node">>, NodeStruct, not_found, Opts),
                    try
                        {ok, ParsedMsg} =
                            hb_gateway_client:result_to_message(
                                Struct,
                                Opts
                            ),
                        {true, ParsedMsg}
                    catch
                        error:Reason ->
                            ?event(
                                warning,
                                {indexer_graphql_parse_failed,
                                    {struct, NodeStruct},
                                    {reason, Reason}
                                }
                            ),
                            false
                    end
                end,
                NodeStructs
            ),
        ?event({graphql_parsed_msgs, length(ParsedMsgs)}),
        WrittenMsgs =
            lists:filter(
                fun(ParsedMsg) ->
                    try
                        {ok, _} = hb_cache:write(ParsedMsg, Opts),
                        true
                    catch
                        error:Reason ->
                            ?event(
                                warning,
                                {indexer_graphql_write_failed,
                                    {reason, Reason},
                                    {msg, ParsedMsg}
                                }
                            ),
                            false
                    end
                end,
                ParsedMsgs
            ),
        NewTotal = Total + length(WrittenMsgs),
        ?event(copycat_short,
            {indexer_graphql_wrote,
                {total, NewTotal},
                {batch, length(WrittenMsgs)},
                {batch_failures, length(ParsedMsgs) - length(WrittenMsgs)}
            }
        ),
        HasNextPage = hb_util:deep_get(<<"pageInfo/hasNextPage">>, Res, false, Opts),
        case HasNextPage of
            true ->
                % Get the last cursor from the node structures and recurse.
```

### parse_query

Find or create a GraphQL query from a given base and request. We expect

```erlang
parse_query(Base, Req, Opts) ->
    % Merge the keys of the base and request maps, and remove duplicates.
```

### default_query

Return a default query for a given filter type.

```erlang
default_query(<<"tags">>, RawMessage, Opts) ->
    Message = hb_cache:ensure_all_loaded(RawMessage, Opts),
    BinaryPairs =
        lists:map(
            fun({Key, Value}) -> {hb_util:bin(Key), hb_util:bin(Value)} end,
            hb_maps:to_list(Message, Opts)
        ),
    TagsQueryStr =
        hb_util:bin(
            [
                <<"{name: \"", Key/binary, "\", values: [\"", Value/binary, "\"]}">>
            ||
                {Key, Value} <- BinaryPairs
            ]
        ),
    ?event({tags_query,
        {message, Message},
        {binary_pairs, BinaryPairs},
        {tags_query_str, {string, TagsQueryStr}}
    }),
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, tags: [",
            TagsQueryStr/binary,
        "]) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
```

### default_query

Return a default query for a given filter type.

```erlang
default_query(<<"tag">>, {Key, Value}, _Opts) ->
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, tags: [",
            "{name: \"", Key/binary, "\", values: [\"", Value/binary, "\"]}",
        "]) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
```

### default_query

Return a default query for a given filter type.

```erlang
default_query(<<"recipient">>, Merged, Opts) ->
    Recipient = hb_maps:get(<<"recipient">>, Merged, <<>>, Opts),
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, recipients: [\"", Recipient/binary, "\"]) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
```

### default_query

Return a default query for a given filter type.

```erlang
default_query(<<"owner">>, Merged, Opts) ->
    Owner = hb_maps:get(<<"owner">>, Merged, <<>>, Opts),
    {ok, <<"query($after: String) { ",
        "transactions(after: $after, owner: \"", Owner/binary, "\") { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
    "} }">>};
```

### default_query

Return a default query for a given filter type.

```erlang
default_query(<<"all">>, _Merged, _Opts) ->
    {ok, <<"query($after: String) { ",
        "transactions(after: $after) { ",
        "edges { ", (hb_gateway_client:item_spec())/binary , " } ",
        "pageInfo { hasNextPage }",
```

---

*Generated from [dev_copycat_graphql.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_copycat_graphql.erl)*
