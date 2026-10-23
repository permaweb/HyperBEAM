%%% @doc A suite of test queries and responses for the `~query@1.0' device's
%%% GraphQL implementation.
-module(dev_query_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% An opaque cursor in a legacy GraphQL gateway's own (non-native) format;
%% HyperBEAM must reject it rather than misinterpret it (it is not native).
-define(LEGACY_GRAPHQL_CURSOR,
    <<
        "eyJzZWFyY2hfYWZ0ZXIiOlsxOTQyODI5LCItaVlWTHQtREt4ZEZkRU4xOHdB",
        "QWtkYUw4anlMSEdVd29uSEgzN3BLWmNvIl0sImluZGV4IjowfQ=="
    >>
).

%%% Test helpers.

write_test_message(Opts) ->
    hb_cache:write(
        Msg = hb_message:commit(
            #{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Message">>,
                <<"action">> => <<"Eval">>,
                <<"Émoji-Tag"/utf8>> => <<"v">>,
                <<"data">> => <<"test data">>
            },
            Opts,
            #{
                <<"commitment-device">> => <<"ans104@1.0">>
            }
        ),
        Opts
    ),
    {ok, Msg}.

%% @doc Populate the cache with three test blocks.
get_test_blocks(Node, Opts) ->
    InitialHeight = 1745749,
    FinalHeight = 1745750,
    lists:foreach(
        fun(Height) ->
            {ok, _} =
                hb_http:request(
                    <<"GET">>,
                    Node,
                    <<"/~arweave@2.9/block=", (hb_util:bin(Height))/binary>>,
                    Opts
                )
        end,
        lists:seq(InitialHeight, FinalHeight)
    ).

%% @doc Use the `~copycat@1.0' device to fetch and index blocks into a new testing
%% node with its own local and index stores.
test_env_with_blocks(InitialHeight, FinalHeight) ->
    ArweaveStore =
        #{
            <<"store-module">> => hb_store_arweave,
            <<"index-store">> => hb_test_utils:test_store(),
            <<"local-store">> => LocalStore = hb_test_utils:test_store()
        },
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [LocalStore, ArweaveStore],
            <<"arweave-index-blocks">> => true,
            <<"query-arweave-remote-block-ranges">> => true
        },
    Node = hb_http_server:start_node(Opts),
    hb_http:request(
        <<"GET">>,
        Node,
        <<
            "/~copycat@1.0/arweave?from=",
                (hb_util:bin(InitialHeight))/binary, "&to=",
                (hb_util:bin(FinalHeight))/binary
        >>,
        Opts
    ),
    {ok, Node, Opts}.

post_graphql(Node, Query, Variables, Req, Opts) ->
    Path =
        case hb_util:bool(hb_maps:get(<<"force-next-page">>, Req, false, Opts)) of
            true -> <<"~query@1.0/graphql?force-next-page=true">>;
            false -> <<"~query@1.0/graphql">>
        end,
    {ok, Res} =
        hb_http:post(
            Node,
            #{
                <<"path">> => Path,
                <<"content-type">> => <<"application/json">>,
                <<"body">> =>
                    hb_json:encode(
                        #{
                            <<"query">> => Query,
                            <<"variables">> => Variables
                        }
                    )
            },
            Opts
        ),
    hb_json:decode(hb_maps:get(<<"body">>, Res, <<>>, Opts)).

test_env_with_message() ->
    LocalStore = hb_test_utils:test_store(),
    ArweaveStore =
        #{
            <<"store-module">> => hb_store_arweave,
            <<"index-store">> => hb_test_utils:test_store(),
            <<"local-store">> => LocalStore
        },
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [LocalStore, ArweaveStore]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, Msg} = write_test_message(Opts),
    {ok, Node, Opts, hb_message:id(Msg, all, Opts)}.

transactions_cursor_query() ->
    <<"""
        query($ids: [ID!], $sort: SortOrder, $first: Int, $after: String) {
            transactions(
                ids: $ids,
                sort: $sort,
                first: $first,
                after: $after
            ) {
                count
                pageInfo {
                    hasNextPage
                }
                edges {
                    cursor
                    node {
                        id
                    }
                }
            }
        }
    """>>.

transaction_edges(Res, Opts) ->
    hb_maps:get(
        <<"edges">>,
        hb_util:deep_get(<<"data/transactions">>, Res, #{}, Opts),
        [],
        Opts
    ).

transaction_ids(Res, Opts) ->
    [
        ID
    ||
        #{ <<"node">> := #{ <<"id">> := ID }} <- transaction_edges(Res, Opts)
    ].

transaction_has_next_page(Res, Opts) ->
    hb_util:deep_get(
        <<"data/transactions/pageInfo/hasNextPage">>,
        Res,
        false,
        Opts
    ).

transaction_cursor(Res, Opts) ->
    [#{ <<"cursor">> := Cursor }] = transaction_edges(Res, Opts),
    Cursor.

%% @doc Write an ANS-104 item with a native recipient.
write_test_message_with_recipient(Recipient, Opts) ->
    TX = ar_bundles:sign_item(
        #tx{ target = hb_util:decode(Recipient), data = <<"test data">>,
            tags = [{<<"Data-Protocol">>, <<"ao">>}, {<<"Variant">>, <<"ao.N.1">>},
                {<<"Type">>, <<"Message">>}, {<<"Action">>, <<"Eval">>},
                {<<"Content-Type">>, <<"text/plain">>}] },
        hb_maps:get(<<"priv-wallet">>, Opts)
    ),
    Msg = hb_message:convert(TX, <<"structured@1.0">>, <<"ans104@1.0">>, Opts),
    hb_cache:write(Msg, Opts),
    {ok, Msg}.

%%% Tests

simple_blocks_query_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()],
            <<"arweave-index-blocks">> => true
        },
    Node = hb_http_server:start_node(Opts),
    get_test_blocks(Node, Opts),
    Query =
        <<"""
            query {
                networkInfo { height }
                blocks(
                    ids: ["V7yZNKPQLIQfUu8r8-lcEaz4o7idl6LTHn5AHlGIFF8TKfxIe7s_yFxjqan6OW45"]
                ) {
                    edges {
                        node {
                            id
                            previous
                            height
                            timestamp
                        }
                    }
                }
            }
        """>>,
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"networkInfo">> := #{ <<"height">> := Height },
                <<"blocks">> := #{
                    <<"edges">> := [
                        #{
                            <<"node">> := #{
                                <<"id">> := _,
                                <<"previous">> := _,
                                <<"height">> := 1745749,
                                <<"timestamp">> := 1756866695
                            }
                        }
                    ]
                }
            }
        } when is_integer(Height) andalso Height >= 1745749,
        dev_query_graphql:test_query(Node, Query, #{}, Opts)
    ).

block_by_height_query_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()],
            <<"arweave-block-store">> => hb_test_utils:test_store(),
            <<"arweave-index-blocks">> => true,
            <<"query-arweave-remote-block-ranges">> => false
        },
    Node = hb_http_server:start_node(Opts),
    get_test_blocks(Node, Opts),
    ?assertEqual([], hb_cache:list_numbered(<<"~arweave@2.9/block/height">>, Opts)),
    ?assertEqual([1745749, 1745750], lists:sort(hb_util:ok(
        hb_ao:resolve(<<"~arweave@2.9/block-heights">>, Opts)))),
    Query =
        <<"""
            query {
                blocks( height: {min: 1745749, max: 1745750}, sort: HEIGHT_ASC ) {
                    edges {
                        node {
                            id
                            previous
                            height
                            timestamp
                        }
                    }
                }
            }
        """>>,
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"blocks">> := #{
                    <<"edges">> := [
                        #{
                            <<"node">> := #{
                                <<"id">> := _,
                                <<"previous">> := _,
                                <<"height">> := 1745749,
                                <<"timestamp">> := 1756866695
                            }
                        },
                        #{
                            <<"node">> := #{
                                <<"id">> := _,
                                <<"previous">> := _,
                                <<"height">> := 1745750,
                                <<"timestamp">> := _
                            }
                        }
                    ]
                }
            }
        },
        dev_query_graphql:test_query(Node, Query, #{}, Opts)
    ),
    PageQuery =
        <<"""
            query($height:RangeFilter, $sort:SortOrder, $after:String,
                    $first:Int, $ids:[ID!]) {
                blocks(height:$height, sort:$sort, after:$after,
                        first:$first, ids:$ids) {
                    pageInfo { hasNextPage }
                    edges { cursor node { id height } }
                }
            }
        """>>,
    Bounds = #{ <<"min">> => 1745749, <<"max">> => 1745750 },
    Page =
        fun(Vars) ->
            Res =
                dev_query_graphql:test_query(Node, PageQuery,
                    maps:merge(#{ <<"height">> => Bounds, <<"first">> => 1 }, Vars),
                    Opts
                ),
            ?assertEqual([], maps:get(<<"errors">>, Res, [])),
            hb_util:deep_get(<<"data/blocks">>, Res, Opts)
        end,
    lists:foreach(
        fun({Sort, First, Last}) ->
            Vars = #{ <<"sort">> => Sort },
            #{ <<"edges">> := [#{ <<"cursor">> := Cursor,
                <<"node">> := #{ <<"height">> := First, <<"id">> := ID } }],
                <<"pageInfo">> := #{ <<"hasNextPage">> := true } } = Page(Vars),
            ?assertEqual(64, byte_size(ID)),
            #{ <<"edges">> := [#{ <<"cursor">> := Next,
                <<"node">> := #{ <<"height">> := Last, <<"id">> := LastID } }],
                <<"pageInfo">> := #{ <<"hasNextPage">> := false } } =
                    Page(Vars#{ <<"after">> => Cursor }),
            ?assertMatch(#{ <<"edges">> := [],
                <<"pageInfo">> := #{ <<"hasNextPage">> := false } },
                Page(Vars#{ <<"after">> => Next })),
            ?assertMatch(#{ <<"edges">> := [#{ <<"node">> := #{ <<"id">> := ID } }],
                <<"pageInfo">> := #{ <<"hasNextPage">> := false } },
                Page(Vars#{ <<"ids">> => [ID, ID] })),
            ?assertEqual(Page(Vars#{ <<"after">> => Cursor }),
                Page(Vars#{ <<"ids">> => [LastID, ID], <<"after">> => Cursor }))
        end,
        [
            {<<"HEIGHT_ASC">>, 1745749, 1745750},
            {<<"HEIGHT_DESC">>, 1745750, 1745749}
        ]
    ),
    ?assertMatch(#{ <<"edges">> := [#{ <<"node">> := #{ <<"height">> := 1745750 } }] },
        Page(#{})),
    ?assertMatch(#{ <<"edges">> := [],
        <<"pageInfo">> := #{ <<"hasNextPage">> := true } },
        Page(#{ <<"first">> => 0 })),
    lists:foreach(
        fun(Vars) ->
            ?assertMatch(#{ <<"edges">> := [],
                <<"pageInfo">> := #{ <<"hasNextPage">> := false } }, Page(Vars))
        end,
        [
            #{ <<"ids">> => [] }, #{ <<"after">> => <<"height=0">> },
            #{ <<"height">> => #{ <<"min">> => 1745750, <<"max">> => 1745749 } }
        ]
    ),
    ?assertMatch(#{ <<"edges">> := [#{ <<"node">> := #{ <<"height">> := 1745750 } }] },
        Page(#{ <<"height">> => Bounds#{ <<"min">> => null } })),
    lists:foreach(
        fun(Vars) ->
            ?assertMatch(#{ <<"errors">> := [_ | _] },
                dev_query_graphql:test_query(Node, PageQuery, Vars, Opts))
        end,
        [
            #{ <<"after">> => <<"height=invalid">> },
            #{ <<"after">> => <<"height=-1">> },
            #{ <<"sort">> => <<"INGESTED_AT_ASC">> }
        ]
    ).

simple_ans104_query_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => Wallet = ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($owners: [String!]) {
                transactions(
                    tags:
                        [
                            {name: "type" values: ["Message"]},
                            {name: "variant" values: ["ao.N.1"]}
                        ],
                        owners: $owners
                    ) {
                    edges {
                        node {
                            id,
                            bundledIn { id }
                            parent { id }
                            quantity { winston ar }
                            fee { winston ar }
                            tags {
                                name,
                                value
                            }
                        }
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"owners">> => [hb:address(Wallet)]
            },
            Opts
        ),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?event({expected_id, ExpectedID}),
    ?event({simple_ans104_query_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"edges">> :=
                        [#{
                            <<"node">> :=
                                #{
                                    <<"id">> := ExpectedID,
                                    <<"bundledIn">> := #{ <<"id">> := <<>> },
                                    <<"parent">> := #{ <<"id">> := <<>> },
                                    <<"quantity">> := #{
                                        <<"winston">> := <<"0">>,
                                        <<"ar">> := <<"0.000000000000">>
                                    },
                                    <<"fee">> := #{
                                        <<"winston">> := <<"0">>,
                                        <<"ar">> := <<"0.000000000000">>
                                    },
                                    <<"tags">> :=
                                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                                }
                        }]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

%% @doc Test transactions query with tags filter
transactions_query_tags_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($types: [String!]!) {
                transactions(
                    ids: null,
                    owners: null,
                    after: null,
                    tags: [
                        {name: "Type", values: $types},
                        {name: "VARIANT", values: ["ao.N.1"]}
                    ]
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
                                value
                            }
                        }
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{ <<"types">> => [<<"Message">>] },
            Opts
        ),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_tags_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"edges">> :=
                        [#{
                            <<"node">> :=
                                #{
                                    <<"id">> := ExpectedID,
                                    <<"tags">> :=
                                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                                }
                        }]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ),
    % A filter by a tag name with a non-ASCII capital finds the message.
    EmojiQuery =
        <<"query { transactions(tags: [{name: \"Émoji-Tag\", "/utf8,
            "values: [\"v\"]}]) { edges { node { id } } } }">>,
    ?assertEqual(
        [ExpectedID],
        transaction_ids(
            dev_query_graphql:test_query(Node, EmojiQuery, Opts),
            Opts
        )
    ).

%% @doc Test transactions query with owners filter
transactions_query_owners_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => Wallet = ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($owners: [String!]) {
                transactions(
                    owners: $owners
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
                                value
                            }
                        }
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"owners">> => [hb:address(Wallet)]
            },
            Opts
        ),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_owners_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"edges">> :=
                        [#{
                            <<"node">> :=
                                #{
                                    <<"id">> := ExpectedID,
                                    <<"tags">> :=
                                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                                }
                        }]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

%% @doc Test transactions query with recipients filter
transactions_query_recipients_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    Alice = ar_wallet:new(),
    ?event({alice, Alice, {explicit, hb:address(Alice)}}),
    AliceAddress = hb:address(Alice),
    {ok, WrittenMsg} = write_test_message_with_recipient(AliceAddress, Opts),
    TagOnly = ar_bundles:sign_item(#tx{ tags = [
        {<<"Target">>, AliceAddress}, {<<"field-target">>, AliceAddress}
    ] }, ar_wallet:new()),
    hb_cache:write(hb_message:convert(
        TagOnly, <<"structured@1.0">>, <<"ans104@1.0">>, Opts
    ), Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($recipients: [String!], $ids: [ID!], $sort: SortOrder) {
                transactions(
                    recipients: $recipients, ids: $ids, sort: $sort
                ) {
                    edges {
                        node {
                            id
                            recipient
                            tags {
                                name
                                value
                            }
                        }
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"recipients">> => [AliceAddress, hb:address(ar_wallet:new())]
            },
            Opts
        ),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_recipients_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"edges">> :=
                        [#{
                            <<"node">> :=
                                #{
                                    <<"id">> := ExpectedID,
                                    <<"recipient">> := AliceAddress,
                                    <<"tags">> :=
                                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                                }
                        }]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ),
    % Native recipient and Target tags must stay distinct on both read paths.
    lists:foreach(
        fun(Codec) ->
            Wallet = ar_wallet:new(),
            Native = hb:address(ar_wallet:new()),
            Unsigned = #tx{ format = 1, target = hb_util:decode(Native),
                tags = [{<<"Target">>, AliceAddress},
                    {<<"field-target">>, AliceAddress}] },
            TX = case Codec of
                <<"tx@1.0">> -> ar_tx:sign(Unsigned, Wallet);
                <<"ans104@1.0">> -> ar_bundles:sign_item(Unsigned, Wallet)
            end,
            Msg = hb_message:convert(TX, <<"structured@1.0">>, Codec, Opts),
            hb_cache:write(Msg, Opts),
            ID = hb_util:encode(TX#tx.id),
            lists:foreach(
                fun({Recipient, IDs, Expected, Direction}) ->
                    Actual = dev_query_graphql:test_query(Node, Query,
                        #{ <<"recipients">> => [Recipient], <<"ids">> => IDs,
                            <<"sort">> => Direction }, Opts),
                    ?assertEqual(Expected, transaction_ids(Actual, Opts))
                end,
                [{Recipient, IDs, Expected, Direction}
                    || {Recipient, IDs, Expected} <-
                        [{Native, null, [ID]}, {Native, [ID], [ID]},
                         {AliceAddress, null, [ExpectedID]}, {AliceAddress, [ID], []}],
                    Direction <- [<<"HEIGHT_ASC">>, <<"HEIGHT_DESC">>]]
            )
        end,
        [<<"tx@1.0">>, <<"ans104@1.0">>]
    ).

%% @doc Test transactions query with ids filter
transactions_query_ids_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($ids: [ID!]) {
                transactions(
                    ids: $ids
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
                                value
                            }
                        }
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"ids">> => [ExpectedID]
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_ids_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"edges">> :=
                        [#{
                            <<"node">> :=
                                #{
                                    <<"id">> := ExpectedID,
                                    <<"tags">> :=
                                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                                }
                        }]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

%% @doc Test transactions query with combined filters
transactions_query_combined_test_parallel() ->
    ArweaveStore = #{ <<"index-store">> => hb_test_utils:test_store() },
    Opts =
        #{
            <<"priv-wallet">> => Wallet = ar_wallet:new(),
            <<"arweave-index-store">> => ArweaveStore,
            <<"store">> => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    hb_cache:write(hb_private:set(WrittenMsg, <<"offset">>, 10, Opts), Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($owners: [String!], $ids: [ID!], $recipients: [String!],
                $tags: [TagFilter!] = [{name: "Type", values: ["Message", "Other"]}]) {
                transactions(
                    owners: $owners,
                    ids: $ids,
                    recipients: $recipients,
                    tags: $tags
                ) {
                    edges {
                        node {
                            id
                            tags {
                                name
                                value
                            }
                        }
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"owners">> => [hb:address(Wallet)],
                <<"ids">> => [ExpectedID]
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_combined_test, Res}),
    % Predicates work without an offset, and when the recorded offset changes.
    ok = hb_store_arweave:write_offset(
        ArweaveStore, ExpectedID, <<"ans104@1.0">>, 20, 0),
    lists:foreach(
        fun({TestNode, MatchID}) ->
            lists:foreach(
                fun({Values, Expected}) ->
                    Result = dev_query_graphql:test_query(TestNode, Query,
                        #{ <<"ids">> => [MatchID], <<"tags">> =>
                            [#{ <<"name">> => <<"TyPe">>, <<"values">> => V }
                            || V <- Values] }, Opts),
                    ?assertNot(maps:is_key(<<"errors">>, Result)),
                    ?assertEqual(Expected, transaction_ids(Result, Opts))
                end,
                [
                    {[[<<"Other">>, <<"Message">>], [<<"Message">>]], [MatchID]},
                    {[[<<"Other">>, <<"Absent">>]], []},
                    {[[<<"message">>]], []},
                    {[[<<"Message">>], [<<"Other">>]], []},
                    {[[]], []},
                    {[], [MatchID]}
                ]
            )
        end,
        [{Node, ExpectedID}, {Node, hb_message:id(WrittenMsg, none, Opts)},
            {hb_http_server:start_node(Opts#{
                <<"priv-wallet">> => ar_wallet:new(), <<"match-index">> => false
            }), hb_message:id(WrittenMsg, none, Opts)}]
    ),
    lists:foreach(
        fun({Filter, Values}) ->
            Empty = dev_query_graphql:test_query(Node, Query, #{ Filter => Values }, Opts),
            ?assertEqual([], hb_util:deep_get(<<"data/transactions/edges">>, Empty, Opts))
        end,
        [{Filter, []} || Filter <- [<<"ids">>, <<"owners">>, <<"recipients">>]] ++
            [{<<"ids">>, [hb_util:encode(crypto:hash(sha256, <<"missing">>))]}]
    ),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"edges">> :=
                        [#{
                            <<"node">> :=
                                #{
                                    <<"id">> := ExpectedID,
                                    <<"tags">> :=
                                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                                }
                        }]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

transactions_query_sort_by_block_test_parallel() ->
    {ok, Node, Opts} = test_env_with_blocks(1892159, 1892158),
    EarlierID = <<"xBpOR2KOjYEgv5HmddMlAgYa-yMvfEVl-0XzRIfm2uY">>,
    LaterID = <<"HVr7EpRhlPkbwdnoXKHf25p7BPa0qJOs6C7XueLthA0">>,
    VerifyFun =
        fun(Order, First, Second) ->
            Q = 
                <<"""
                    query($ids: [ID!], $sort: SortOrder) {
                        transactions(
                            ids: $ids,
                            sort: $sort
                        ) {
                            edges {
                                node {
                                    id
                                }
                            }
                        }
                    }
                """>>,
            ?assertMatch(
                #{
                    <<"data">> := #{
                        <<"transactions">> := #{
                            <<"edges">> := [
                                #{ <<"node">> := #{ <<"id">> := First } },
                                #{ <<"node">> := #{ <<"id">> := Second } }
                            ]
                        }
                    }
                },
                dev_query_graphql:test_query(
                    Node,
                    Q,
                    #{ <<"ids">> => [First, Second], <<"sort">> => Order },
                    Opts
                )
            )
        end,
    VerifyFun(<<"HEIGHT_ASC">>, EarlierID, LaterID),
    VerifyFun(<<"HEIGHT_DESC">>, LaterID, EarlierID).

transactions_query_filter_by_block_test_parallel() ->
    {ok, Node, Opts} = test_env_with_blocks(1892159, 1892158),
    EarlierID = <<"xBpOR2KOjYEgv5HmddMlAgYa-yMvfEVl-0XzRIfm2uY">>,
    LaterID = <<"HVr7EpRhlPkbwdnoXKHf25p7BPa0qJOs6C7XueLthA0">>,
    VerifyFun =
        fun(Start, End, Present, Absent) ->
            Q = 
                <<"""
                    query($ids: [ID!], $block: BlockFilter) {
                        transactions(
                            ids: $ids,
                            block: $block
                        ) {
                            edges {
                                node {
                                    id
                                }
                            }
                        }
                    }
                """>>,
            #{ <<"data">> := #{ <<"transactions">> := #{ <<"edges">> := Edges } } } =
                dev_query_graphql:test_query(
                    Node,
                    Q,
                    #{
                        <<"ids">> => Present ++ Absent,
                        <<"block">> => #{ <<"min">> => Start, <<"max">> => End }
                    },
                    Opts
                ),
            IDs = [ ID || #{ <<"node">> := #{ <<"id">> := ID } } <- Edges ],
            lists:foreach(
                fun(ID) -> ?assert(lists:member(ID, IDs)) end,
                Present
            ),
            lists:foreach(
                fun(ID) -> ?assertNot(lists:member(ID, IDs)) end,
                Absent
            )
        end,
    VerifyFun(1892158, 1892159, [EarlierID, LaterID], []),
    VerifyFun(1892156, 1892157, [], [EarlierID, LaterID]),
    VerifyFun(1892157, 1892158, [EarlierID], [LaterID]),
    VerifyFun(null, 1892158, [EarlierID], [LaterID]),
    VerifyFun(1892159, null, [LaterID], [EarlierID]),
    VerifyFun(1892159, 1892160, [LaterID], [EarlierID]).

transactions_query_filter_by_block_excludes_unknown_offsets_test_parallel() ->
    {ok, _Node, Opts} = test_env_with_blocks(1892159, 1892158),
    {ok, ID} =
        hb_cache:write(
            #{
                <<"type">> => <<"Message">>,
                <<"data">> => <<"local-only">>
            },
            Opts
        ),
    ?assertEqual(
        not_found,
        hb_store_arweave:read_offset(hb_store_arweave:store_from_opts(Opts), ID, Opts)
    ),
    ?assertMatch(
        {ok, #{
            <<"count">> := <<"0">>,
            <<"edges">> := []
        }},
        dev_query_arweave:query(
            #{},
            <<"transactions">>,
            #{
                <<"ids">> => [ID],
                <<"block">> => #{
                    <<"min">> => 1892158,
                    <<"max">> => 1892158
                }
            },
            Opts
        )
    ).

transactions_query_filter_by_block_can_ignore_ranges_test_parallel() ->
    {ok, _Node, BaseOpts} = test_env_with_blocks(1892159, 1892158),
    Opts = BaseOpts#{ <<"query-arweave-ignore-block-ranges">> => true },
    {ok, ID} =
        hb_cache:write(
            #{
                <<"type">> => <<"Message">>,
                <<"data">> => <<"local-only">>
            },
            Opts
        ),
    ?assertMatch(
        {ok, #{
            <<"count">> := <<"1">>,
            <<"edges">> := [
                #{
                    <<"id">> := ID,
                    <<"node">> := _
                }
            ]
        }},
        dev_query_arweave:query(
            #{},
            <<"transactions">>,
            #{
                <<"ids">> => [ID],
                <<"block">> => #{
                    <<"min">> => 1892158,
                    <<"max">> => 1892158
                }
            },
            Opts
        )
    ).

transactions_query_ids_preserve_arweave_tx_id_test_parallel() ->
    {ok, Node, Opts} = test_env_with_blocks(1892487, 1892487),
    ID = <<"mT7pIQx9ORnemXoIzWmKwymiZJxtOSvzxm3P44M9C1A">>,
    {ok, #{ <<"start">> := _, <<"length">> := Length }} =
        hb_store_arweave:read_offset(hb_store_arweave:store_from_opts(Opts), ID, Opts),
    DataSize = hb_util:bin(Length),
    ?assertMatch(
        #{ <<"data">> := #{ <<"transactions">> := #{
            <<"count">> := <<"1">>,
            <<"edges">> := [
                #{
                    <<"node">> := #{
                        <<"id">> := ID,
                        <<"data">> := #{ <<"size">> := DataSize },
                        <<"quantity">> := #{
                            <<"winston">> := <<"0">>,
                            <<"ar">> := <<"0.000000000000">>
                        },
                        <<"fee">> := #{
                            <<"winston">> := <<"8549817344">>,
                            <<"ar">> := <<"0.008549817344">>
                        }
                    }
                }
            ]
        } } },
        dev_query_graphql:test_query(
            Node,
            <<"""
                query($ids: [ID!]) {
                    transactions(ids: $ids, block: {min: 1892487, max: 1892487}) {
                        count
                        edges { node { id data { size }
                            quantity { winston ar } fee { winston ar } } }
                    }
                }
            """>>,
            #{ <<"ids">> => [ID] },
            Opts
        )
    ),
    % Whole-message reads retain unsigned commitments alongside the L1 one.
    Store = hb_test_utils:test_store(hb_store_volatile),
    AmountOpts = Opts#{ <<"store">> => [Store], <<"priv-wallet">> => ar_wallet:new() },
    AmountNode = hb_http_server:start_node(AmountOpts),
    lists:foreach(
        fun({Codec, Amount}) ->
            Unsigned = #tx{
                format = 2, quantity = Amount, reward = Amount,
                target = crypto:strong_rand_bytes(32),
                tags = [{<<"quantity">>, <<"1000">>}, {<<"reward">>, <<"2000">>},
                    {<<"fee">>, <<"3000">>}]
            },
            Wallet = hb_maps:get(<<"priv-wallet">>, AmountOpts),
            TX = case Codec of
                <<"tx@1.0">> -> ar_tx:sign(Unsigned, Wallet);
                <<"ans104@1.0">> -> ar_bundles:sign_item(Unsigned, Wallet)
            end,
            Msg = hb_message:normalize_commitments(
                hb_message:convert(TX, <<"structured@1.0">>, Codec, AmountOpts),
                AmountOpts),
            TXID = hb_util:encode(TX#tx.id),
            ok = hb_store:write(Store, #{ TXID => Msg }, AmountOpts),
            Winston = hb_util:bin(Amount),
            ?assertMatch(#{ <<"data">> := #{ <<"transaction">> := #{
                <<"id">> := TXID,
                <<"quantity">> := #{ <<"winston">> := Winston },
                <<"fee">> := #{ <<"winston">> := Winston }
            }}}, dev_query_graphql:test_query(AmountNode,
                <<"query($id: ID!) { transaction(id: $id) { id",
                    " quantity { winston } fee { winston } } }">>,
                #{ <<"id">> => TXID }, AmountOpts))
        end,
        [{<<"tx@1.0">>, 0}, {<<"tx@1.0">>, 5}, {<<"ans104@1.0">>, 0}]
    ).

%% @doc L1 metadata does not load or cache its bundle payload.
transactions_header_only_test_parallel() ->
    Local = hb_test_utils:test_store(hb_store_volatile),
    Store = #{ <<"store-module">> => hb_store_arweave,
        <<"index-store">> => hb_test_utils:test_store(hb_store_volatile),
        <<"local-store">> => [Local] },
    Opts = #{ <<"store">> => [Local, Store],
        <<"arweave-index-store">> => Store, <<"priv-wallet">> => ar_wallet:new() },
    ID = <<"VJhGCNOw2zB0c9apKBs_LKXpPx6rctrw2kSlSSyI_SQ">>,
    ok = hb_store_arweave:write_offset(Store, ID, <<"tx@1.0">>,
        386086672048374, 2283569),
    % A header read that the peers refuse (`429') fails the query.
    {ok, Peer, Handle} = hb_mock_server:start([{"/tx/:id", tx, {429, <<>>}}]),
    ?assertMatch(#{ <<"errors">> := [_ | _] }, dev_query_graphql:test_query(
        hb_http_server:start_node(Opts#{ <<"priv-wallet">> => ar_wallet:new(),
            <<"routes">> => [#{ <<"template">> => <<"^/arweave">>,
                <<"node">> => #{ <<"match">> => <<"^/arweave">>, <<"with">> => Peer,
                    <<"opts">> => #{ <<"http-client">> => httpc } } }] }),
        <<"query($id: ID!) { transaction(id: $id) { id } }">>,
        #{ <<"id">> => ID }, Opts)),
    hb_mock_server:stop(Handle),
    {ok, Header} = hb_ao:resolve(#{ <<"path">> => <<"~arweave@2.9/tx">>,
        <<"tx">> => ID, <<"exclude-data">> => true }, Opts),
    TX = hb_message:convert(Header, <<"tx@1.0">>, Opts),
    Node = hb_http_server:start_node(Opts),
    Result = dev_query_graphql:test_query(Node,
        <<"query($id: ID!) { transaction(id: $id) { id anchor recipient",
            " owner { address } tags { name value } data { size }",
            " bundledIn { id } parent { id }",
            " quantity { winston } fee { winston } } }">>,
        #{ <<"id">> => ID }, Opts),
    ?assertMatch(#{ <<"data">> := #{ <<"transaction">> := #{
        <<"id">> := ID, <<"data">> := #{ <<"size">> := <<"2283569">> }
    }}}, Result),
    ?assertEqual({error, not_found}, hb_cache:read(ID, hb_store:scope(Opts, local))),
    #{ <<"data">> := #{ <<"transaction">> := Projected }} = Result,
    ?assertMatch(#{ <<"bundledIn">> := null, <<"parent">> := null }, Projected),
    ?assertEqual(hb_util:encode(TX#tx.anchor), maps:get(<<"anchor">>, Projected)),
    ?assertEqual(hb_util:encode(TX#tx.target), maps:get(<<"recipient">>, Projected)),
    ?assertEqual(#{ <<"winston">> => hb_util:bin(TX#tx.quantity) },
        maps:get(<<"quantity">>, Projected)),
    ?assertEqual(#{ <<"winston">> => hb_util:bin(TX#tx.reward) },
        maps:get(<<"fee">>, Projected)),
    ?assertEqual(lists:sort([#{ <<"name">> => K, <<"value">> => V }
        || {K, V} <- TX#tx.tags]), lists:sort(maps:get(<<"tags">>, Projected))).

transactions_query_cursor_by_offset_test_parallel() ->
    {ok, Node, Opts} = test_env_with_blocks(1892159, 1892158),
    EarlierID = <<"xBpOR2KOjYEgv5HmddMlAgYa-yMvfEVl-0XzRIfm2uY">>,
    LaterID = <<"HVr7EpRhlPkbwdnoXKHf25p7BPa0qJOs6C7XueLthA0">>,
    StoreOpts = hb_store_arweave:store_from_opts(Opts),
    {ok, #{ <<"start">> := EarlierOffset }} =
        hb_store_arweave:read_offset(StoreOpts, EarlierID, Opts),
    {ok, #{ <<"start">> := LaterOffset }} =
        hb_store_arweave:read_offset(StoreOpts, LaterID, Opts),
    Query = transactions_cursor_query(),
    VerifyFun =
        fun(Order, FirstID, FirstOffset, SecondID, SecondOffset) ->
            FirstRes =
                dev_query_graphql:test_query(
                    Node,
                    Query,
                    #{
                        <<"ids">> => [EarlierID, LaterID],
                        <<"sort">> => Order,
                        <<"first">> => 1
                    },
                    Opts
                ),
            #{
                <<"data">> := #{
                    <<"transactions">> := #{
                        <<"count">> := <<"2">>,
                        <<"pageInfo">> := #{
                            <<"hasNextPage">> := true
                        },
                        <<"edges">> := [
                            #{
                                <<"cursor">> := FirstCursor,
                                <<"node">> := #{
                                    <<"id">> := FirstID
                                }
                            }
                        ]
                    }
                }
            } = FirstRes,
            ?assertEqual(
                <<"offset=", (hb_util:bin(FirstOffset))/binary>>,
                FirstCursor
            ),
            SecondRes =
                dev_query_graphql:test_query(
                    Node,
                    Query,
                    #{
                        <<"ids">> => [EarlierID, LaterID],
                        <<"sort">> => Order,
                        <<"first">> => 1,
                        <<"after">> => FirstCursor
                    },
                    Opts
                ),
            #{
                <<"data">> := #{
                    <<"transactions">> := #{
                        <<"count">> := <<"2">>,
                        <<"pageInfo">> := #{
                            <<"hasNextPage">> := false
                        },
                        <<"edges">> := [
                            #{
                                <<"cursor">> := SecondCursor,
                                <<"node">> := #{
                                    <<"id">> := SecondID
                                }
                            }
                        ]
                    }
                }
            } = SecondRes,
            ?assertEqual(
                <<"offset=", (hb_util:bin(SecondOffset))/binary>>,
                SecondCursor
            )
        end,
    VerifyFun(
        <<"HEIGHT_ASC">>,
        EarlierID,
        EarlierOffset,
        LaterID,
        LaterOffset
    ),
    VerifyFun(
        <<"HEIGHT_DESC">>,
        LaterID,
        LaterOffset,
        EarlierID,
        EarlierOffset
    ).

transactions_query_terminal_page_test_parallel() ->
    {ok, Node, Opts, ID} = test_env_with_message(),
    Res =
        dev_query_graphql:test_query(
            Node,
            transactions_cursor_query(),
            #{ <<"ids">> => [ID], <<"first">> => 2 },
            Opts
        ),
    ?assertEqual([ID], transaction_ids(Res, Opts)),
    ?assertEqual(false, transaction_has_next_page(Res, Opts)),
    ?assertEqual(nomatch, binary:match(transaction_cursor(Res, Opts), <<"&remaining=0">>)).

transactions_query_force_next_page_test_parallel() ->
    {ok, Node, Opts, ID} = test_env_with_message(),
    Res =
        post_graphql(
            Node,
            transactions_cursor_query(),
            #{ <<"ids">> => [ID], <<"first">> => 2 },
            #{ <<"force-next-page">> => true },
            Opts
        ),
    ?assertEqual([ID], transaction_ids(Res, Opts)),
    ?assertEqual(true, transaction_has_next_page(Res, Opts)).

transactions_query_force_cursor_test_parallel() ->
    {ok, Node, Opts, ID} = test_env_with_message(),
    Query = transactions_cursor_query(),
    Vars = #{ <<"ids">> => [ID], <<"first">> => 2 },
    Normal = dev_query_graphql:test_query(Node, Query, Vars, Opts),
    Forced = post_graphql(
        Node,
        Query,
        Vars,
        #{ <<"force-next-page">> => true },
        Opts
    ),
    ?assertEqual(
        << (transaction_cursor(Normal, Opts))/binary, "&remaining=0" >>,
        transaction_cursor(Forced, Opts)
    ).

transactions_query_remaining_cursor_test_parallel() ->
    {ok, Node, Opts, ID} = test_env_with_message(),
    Query = transactions_cursor_query(),
    Vars = #{ <<"ids">> => [ID], <<"first">> => 2 },
    Normal = dev_query_graphql:test_query(Node, Query, Vars, Opts),
    FastFail =
        post_graphql(
            Node,
            Query,
            Vars#{
                <<"after">> =>
                    << (transaction_cursor(Normal, Opts))/binary, "&remaining=0" >>
            },
            #{ <<"force-next-page">> => true },
            Opts
        ),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"count">> := <<"0">>,
                    <<"pageInfo">> := #{ <<"hasNextPage">> := true },
                    <<"edges">> := []
                }
            }
        },
        FastFail
    ).

transactions_query_legacy_cursor_test_parallel() ->
    {ok, Node, Opts, ID} = test_env_with_message(),
    Res =
        dev_query_graphql:test_query(
            Node,
            transactions_cursor_query(),
            #{
                <<"ids">> => [ID],
                <<"first">> => 2,
                <<"after">> => ?LEGACY_GRAPHQL_CURSOR
            },
            Opts
        ),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transactions">> := #{
                    <<"count">> := <<"0">>,
                    <<"pageInfo">> := #{ <<"hasNextPage">> := false },
                    <<"edges">> := []
                }
            }
        },
        Res
    ).

transactions_query_gateway_fallback_test_() ->
    {timeout, 60, fun transactions_query_gateway_fallback/0}.
transactions_query_gateway_fallback() ->
    {ok, LocalNode, _LocalOpts, _LocalID} = test_env_with_message(),
    EarlierID = <<"xBpOR2KOjYEgv5HmddMlAgYa-yMvfEVl-0XzRIfm2uY">>,
    LaterID = <<"HVr7EpRhlPkbwdnoXKHf25p7BPa0qJOs6C7XueLthA0">>,
    Opts = #{ <<"routes">> => [graphql_gateway_route(LocalNode)] },
    % The client only follows cursors; it has no knowledge of the route's
    % fallback chain. The local node has neither transaction, so the gateway
    % answers both pages, and page 2 follows the gateway's own cursor.
    Pages =
        collect_graphql_pages(
            transactions_cursor_query(),
            #{ <<"ids">> => [EarlierID, LaterID], <<"first">> => 1 },
            Opts
        ),
    ?assertEqual(
        [LaterID, EarlierID],
        lists:append([transaction_ids(P, Opts) || P <- Pages])
    ),
    ?assertEqual(
        [true, false],
        [transaction_has_next_page(P, Opts) || P <- Pages]
    ).

graphql_gateway_route(LocalNode) ->
    #{
        <<"template">> => <<"/graphql">>,
        <<"nodes">> =>
            [
                % The local node is the fallback front: the route tells it to
                % `force-next-page' so it always signals "there may be more",
                % letting the (router-agnostic) client page on to the gateway.
                % The flag is injected here, not by the client.
                #{
                    <<"uri">> =>
                        << LocalNode/binary,
                            "~query@1.0/graphql?force-next-page=true" >>
                },
                #{
                    <<"prefix">> => <<"https://arweave.net">>,
                    <<"opts">> =>
                        #{ <<"http-client">> => httpc, <<"protocol">> => http2 }
                }
            ],
        <<"parallel">> => 1,
        <<"responses">> => 1,
        <<"stop-after">> => true,
        <<"admissible-status">> => 200,
        <<"admissible">> =>
            #{
                <<"device">> => <<"query@1.0">>,
                <<"path">> => <<"has-results">>
            }
    }.

collect_graphql_pages(Query, Vars, Opts) ->
    collect_graphql_pages(Query, Vars, Opts, [], 4).

collect_graphql_pages(_Query, _Vars, _Opts, Acc, 0) ->
    lists:reverse(Acc);
collect_graphql_pages(Query, Vars, Opts, Acc, Remaining) ->
    {ok, Page} = hb_client_gateway:query(Query, Vars, Opts),
    case transaction_has_next_page(Page, Opts) of
        true ->
            Edges = transaction_edges(Page, Opts),
            ?assert(Edges =/= []),
            #{ <<"cursor">> := Cursor } = lists:last(Edges),
            collect_graphql_pages(
                Query,
                Vars#{ <<"after">> => Cursor },
                Opts,
                [Page | Acc],
                Remaining - 1
            );
        false ->
            lists:reverse([Page | Acc])
    end.

%% @doc Test single transaction query by ID
transaction_query_by_id_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($id: ID!) {
                transaction(id: $id) {
                    id
                    tags {
                        name
                        value
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"id">> => ExpectedID
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transaction_query_by_id_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transaction">> := #{
                    <<"id">> := ExpectedID,
                    <<"tags">> :=
                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

%% @doc Test single transaction query with more fields  
transaction_query_full_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => SenderKey = ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    Alice = ar_wallet:new(),
    ?event({alice, Alice, {explicit, hb:address(Alice)}}),
    AliceAddress = hb:address(Alice),
    SenderAddress = hb:address(SenderKey),
    SenderPubKey = hb_util:encode(ar_wallet:to_pubkey(SenderKey)),
    {ok, WrittenMsg} = write_test_message_with_recipient(AliceAddress, Opts),
    ExpectedID = hb_message:id(WrittenMsg, all, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($id: ID!) {
                transaction(id: $id) {
                    id
                    anchor
                    signature
                    recipient
                    owner {
                        address
                        key
                    }
                    tags {
                        name
                        value
                    }
                    data {
                        size
                        type
                    }
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"id">> => ExpectedID
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transaction_query_full_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transaction">> := #{
                    <<"id">> := ExpectedID,
                    <<"recipient">> := AliceAddress,
                    <<"anchor">> := <<"">>,
                    <<"owner">> := #{
                        <<"address">> := SenderAddress,
                        <<"key">> := SenderPubKey
                    },
                    <<"data">> := #{
                        <<"size">> := <<"9">>,
                        <<"type">> := <<"text/plain">>
                    },
                    <<"tags">> :=
                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                    % Note: other fields may be "Not implemented." for now
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

%% @doc Unsigned results keep a mixed page's non-null owner and signature fields.
transactions_unsigned_owner_test_parallel() ->
    Opts = #{
        <<"priv-wallet">> => Wallet = ar_wallet:new(),
        <<"store">> => [hb_test_utils:test_store(hb_store_volatile)]
    },
    Node = hb_http_server:start_node(Opts),
    Unsigned = #{
        <<"type">> => <<"Device-Specification">>,
        <<"body">> => <<"unsigned specification">>
    },
    Signed = hb_message:commit(Unsigned, Opts,
        #{ <<"commitment-device">> => <<"ans104@1.0">> }),
    IDs = [begin
        {ok, _} = hb_cache:write(Msg, Opts),
        hb_message:id(Msg, all, Opts)
    end || Msg <- [Unsigned, Signed]],
    Result = post_graphql(Node,
        <<"query($ids: [ID!]) { transactions(ids: $ids) {",
            " edges { node { id owner { address key } signature } } } }">>,
        #{ <<"ids">> => IDs }, #{}, Opts),
    ?assertNot(maps:is_key(<<"errors">>, Result)),
    #{ <<"data">> := #{ <<"transactions">> := #{ <<"edges">> := Edges }}} = Result,
    Nodes = maps:from_list([{maps:get(<<"id">>, N), N}
        || #{ <<"node">> := N } <- Edges]),
    [UnsignedID, SignedID] = IDs,
    ?assertEqual(lists:sort(IDs), lists:sort(maps:keys(Nodes))),
    ?assertMatch(#{
        <<"owner">> := #{ <<"address">> := <<>>, <<"key">> := <<>> },
        <<"signature">> := <<>>
    }, maps:get(UnsignedID, Nodes)),
    #{ <<"owner">> := Owner, <<"signature">> := Signature } =
        maps:get(SignedID, Nodes),
    ?assertEqual(#{
        <<"address">> => hb:address(Wallet),
        <<"key">> => hb_util:encode(ar_wallet:to_pubkey(Wallet))
    }, Owner),
    ?assertEqual(hb_maps:get(<<"signature">>,
        hb_maps:get(SignedID, hb_maps:get(<<"commitments">>, Signed))), Signature).

%% @doc Preserve original tags, or exclude native fields from normalized keys.
transaction_tags_exclude_fields_test_parallel() ->
    Wallet = ar_wallet:new(),
    Opts = #{
        <<"priv-wallet">> => Wallet,
        <<"store">> => [hb_test_utils:test_store()]
    },
    Node = hb_http_server:start_node(Opts),
    lists:foreach(
        fun({Codec, Amount, Tags, Expected}) ->
            Unsigned = #tx{
                format = 1,
                target = crypto:strong_rand_bytes(32),
                anchor = crypto:strong_rand_bytes(32),
                quantity = Amount,
                reward = Amount,
                data = <<"payload">>,
                tags = Tags
            },
            TX = case Codec of
                <<"tx@1.0">> -> ar_tx:sign(Unsigned, Wallet);
                <<"ans104@1.0">> -> ar_bundles:sign_item(Unsigned, Wallet)
            end,
            Msg = hb_message:convert(TX, <<"structured@1.0">>, Codec, Opts),
            ID = hb_util:encode(TX#tx.id),
            Commitment = hb_maps:get(ID, hb_maps:get(<<"commitments">>, Msg)),
            Original = hb_maps:is_key(<<"original-tags">>, Commitment, Opts),
            case Tags of
                [] -> ?assertNot(Original);
                [{<<"app">>, _} | _] -> ?assertNot(Original);
                [{<<"quantity">>, _} | _] ->
                    ?assertEqual(Codec =:= <<"tx@1.0">>, Original);
                _ -> ?assert(Original)
            end,
            {ok, _} = hb_cache:write(Msg, Opts),
            #{ <<"data">> := #{ <<"transaction">> := #{ <<"tags">> := Actual } }} =
                dev_query_graphql:test_query(Node,
                    <<"query($id: ID!) { transaction(id: $id) {",
                        " tags { name value } } }">>,
                    #{ <<"id">> => ID }, Opts),
            case Original of
                true ->
                    ?assertEqual(
                        [#{ <<"name">> => Name, <<"value">> => Value }
                            || {Name, Value} <- Tags],
                        Actual
                    );
                false ->
                    % The query gives the tags in the order they were signed.
                    ?assertEqual(
                        [#{ <<"name">> => Name, <<"value">> => Value }
                            || {Name, Value} <- Tags,
                                lists:member({Name, Value}, Expected)],
                        Actual
                    )
            end
        end,
        [{Codec, Amount, Tags, Expected}
            || Codec <- [<<"tx@1.0">>, <<"ans104@1.0">>], Amount <- [0, 5],
            {Tags, Expected} <- [
                {[], []},
                {[{<<"app">>, <<"literal">>}, {<<"body">>, <<"a tag">>}],
                    [{<<"app">>, <<"literal">>}]},
                {[{<<"App">>, <<"literal">>}, {<<"Body">>, <<"a tag">>}],
                    [{<<"app">>, <<"literal">>}]},
                {[{<<"quantity">>, <<"1000">>}, {<<"reward">>, <<"2000">>},
                    {<<"fee">>, <<"3000">>}, {<<"field-reward">>, <<"a tag">>}],
                    [{<<"fee">>, <<"3000">>}, {<<"field-reward">>, <<"a tag">>}] ++
                        case {Codec, Amount} of
                            {<<"tx@1.0">>, 5} -> [];
                            _ -> [{<<"quantity">>, <<"1000">>},
                                {<<"reward">>, <<"2000">>}]
                        end},
                {[{<<"Anchor">>, <<"tag anchor">>},
                    {<<"Target">>, <<"tag target">>},
                    {<<"App">>, <<"first">>}, {<<"app">>, <<"second">>}],
                    [{<<"app">>, <<"\"first\", \"second\"">>}]}
            ]]
    ).

%% @doc Test single transaction query with non-existent ID
transaction_query_not_found_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Res =
        dev_query_graphql:test_query(
            hb_http_server:start_node(Opts),
            <<"""
                query($id: ID!) {
                    transaction(id: $id) {
                        id
                        tags {
                            name
                            value
                        }
                    }
                }
            """>>,
            #{
                <<"id">> => hb_util:encode(crypto:strong_rand_bytes(32))
            },
            Opts
        ),
    % Should return null for non-existent transaction
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transaction">> := null
            }
        },
        Res
    ).

%% @doc Test parsing, storing, and querying a transaction with an anchor.
transaction_query_with_anchor_test_parallel() ->
    Opts =
        #{
            <<"priv-wallet">> => Wallet = ar_wallet:new(),
            <<"store">> => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, _UnsignedID} =
        hb_cache:write(
            Msg = hb_message:convert(
                ar_bundles:sign_item(
                    #tx {
                        anchor = AnchorID = crypto:strong_rand_bytes(32),
                        data = <<"test-data">>
                    },
                    Wallet
                ),
                <<"structured@1.0">>,
                <<"ans104@1.0">>,
                Opts
            ),
            Opts
        ),
    SignedID = hb_message:id(Msg, signed, Opts),
    EncodedAnchor = hb_util:encode(AnchorID),
    Query =
        <<"""
            query($id: ID!) {
                transaction(id: $id) {
                    data {
                        size
                        type
                    }
                    anchor
                }
            }
        """>>,
    Res =
        dev_query_graphql:test_query(
            Node,
            Query,
            #{
                <<"id">> => SignedID
            },
            Opts
        ),
    ?event({transaction_query_with_anchor_test, Res}),
    ?assertMatch(
        #{
            <<"data">> := #{
                <<"transaction">> := #{
                    <<"anchor">> := EncodedAnchor
                }
            }
        },
        Res
    ).
