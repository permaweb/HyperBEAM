%%% @doc A suite of test queries and responses for the `~query@1.0' device's
%%% GraphQL implementation.
-module(dev_query_test_vectors).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Test helpers.

write_test_message(Opts) ->
    hb_cache:write(
        Msg = hb_message:commit(
            #{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Message">>,
                <<"action">> => <<"Eval">>,
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
            priv_wallet => ar_wallet:new(),
            store => [LocalStore, ArweaveStore],
            arweave_index_blocks => true,
            query_arweave_remote_block_ranges => true
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

%% Helper function to write test message with Recipient
write_test_message_with_recipient(Recipient, Opts) ->
    hb_cache:write(
        Msg = hb_message:commit(
            #{
                <<"data-protocol">> => <<"ao">>,
                <<"variant">> => <<"ao.N.1">>,
                <<"type">> => <<"Message">>,
                <<"action">> => <<"Eval">>,
                <<"content-type">> => <<"text/plain">>,
                <<"data">> => <<"test data">>,
                <<"target">> => Recipient
            },
            Opts,
            #{
                <<"commitment-device">> => <<"ans104@1.0">>
            }
        ),
        Opts
    ),
    {ok, Msg}.

%%% Tests

simple_blocks_query_test() ->
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()],
            arweave_index_blocks => true
        },
    Node = hb_http_server:start_node(Opts),
    get_test_blocks(Node, Opts),
    Query =
        <<"""
            query {
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
        },
        dev_query_graphql:test_query(Node, Query, #{}, Opts)
    ).

block_by_height_query_test() ->
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()],
            arweave_index_blocks => true
        },
    Node = hb_http_server:start_node(Opts),
    get_test_blocks(Node, Opts),
    Query =
        <<"""
            query {
                blocks( height: {min: 1745749, max: 1745750} ) {
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
    ).

simple_ans104_query_test() ->
    Opts =
        #{
            priv_wallet => Wallet = ar_wallet:new(),
            store => [hb_test_utils:test_store()]
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
transactions_query_tags_test() ->
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, WrittenMsg} = write_test_message(Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query {
                transactions(
                    tags: [
                        {name: "type", values: ["Message"]},
                        {name: "variant", values: ["ao.N.1"]}
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
            #{},
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
    ).

%% @doc Test transactions query with owners filter
transactions_query_owners_test() ->
    Opts =
        #{
            priv_wallet => Wallet = ar_wallet:new(),
            store => [hb_test_utils:test_store()]
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
transactions_query_recipients_test() ->
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    Alice = ar_wallet:new(),
    ?event({alice, Alice, {explicit, hb_util:human_id(Alice)}}),
    AliceAddress = hb_util:human_id(Alice),
    {ok, WrittenMsg} = write_test_message_with_recipient(AliceAddress, Opts),
    ?assertMatch(
        {ok, [_]},
        hb_cache:match(#{<<"type">> => <<"Message">>}, Opts)
    ),
    Query =
        <<"""
            query($recipients: [String!]) {
                transactions(
                    recipients: $recipients
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
                <<"recipients">> => [AliceAddress]
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
                                    <<"tags">> :=
                                        [#{ <<"name">> := _, <<"value">> := _ }|_]
                                }
                        }]
                }
            }
        } when ?IS_ID(ExpectedID),
        Res
    ).

%% @doc Test transactions query with ids filter
transactions_query_ids_test() ->
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()]
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
transactions_query_combined_test() ->
    Opts =
        #{
            priv_wallet => Wallet = ar_wallet:new(),
            store => [hb_test_utils:test_store()]
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
            query($owners: [String!], $ids: [ID!]) {
                transactions(
                    owners: $owners,
                    ids: $ids,
                    tags: [
                        {name: "type", values: ["Message"]}
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
            #{
                <<"owners">> => [hb:address(Wallet)],
                <<"ids">> => [ExpectedID]
            },
            Opts
        ),
    ?event({expected_id, ExpectedID}),
    ?event({transactions_query_combined_test, Res}),
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

transactions_query_sort_by_block_test() ->
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

transactions_query_filter_by_block_test() ->
    {ok, Node, Opts} = test_env_with_blocks(1892159, 1892158),
    EarlierID = <<"xBpOR2KOjYEgv5HmddMlAgYa-yMvfEVl-0XzRIfm2uY">>,
    LaterID = <<"HVr7EpRhlPkbwdnoXKHf25p7BPa0qJOs6C7XueLthA0">>,
    VerifyFun =
        fun(Start, End, Present, Absent) ->
            Q = 
                <<"""
                    query($ids: [ID!], $min: Int, $max: Int) {
                        transactions(
                            ids: $ids,
                            block: {min: $min, max: $max}
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
                        <<"min">> => Start,
                        <<"max">> => End
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
    VerifyFun(1892159, 1892160, [LaterID], [EarlierID]).

transactions_query_filter_by_block_excludes_unknown_offsets_test() ->
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
        hb_store_arweave:read_offset(hb_store_arweave:store_from_opts(Opts), ID)
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

transactions_query_filter_by_block_can_ignore_ranges_test() ->
    {ok, _Node, BaseOpts} = test_env_with_blocks(1892159, 1892158),
    Opts = BaseOpts#{ query_arweave_ignore_block_ranges => true },
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

transactions_query_ids_preserve_arweave_tx_id_test() ->
    {ok, _Node, Opts} = test_env_with_blocks(1892487, 1892487),
    ID = <<"mT7pIQx9ORnemXoIzWmKwymiZJxtOSvzxm3P44M9C1A">>,
    ?assertMatch(
        {ok, #{ <<"start-offset">> := _ }},
        hb_store_arweave:read_offset(hb_store_arweave:store_from_opts(Opts), ID)
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
                    <<"min">> => 1892487,
                    <<"max">> => 1892487
                }
            },
            Opts
        )
    ).

transactions_query_cursor_by_offset_test() ->
    {ok, Node, Opts} = test_env_with_blocks(1892159, 1892158),
    EarlierID = <<"xBpOR2KOjYEgv5HmddMlAgYa-yMvfEVl-0XzRIfm2uY">>,
    LaterID = <<"HVr7EpRhlPkbwdnoXKHf25p7BPa0qJOs6C7XueLthA0">>,
    StoreOpts = hb_store_arweave:store_from_opts(Opts),
    {ok, #{ <<"start-offset">> := EarlierOffset }} =
        hb_store_arweave:read_offset(StoreOpts, EarlierID),
    {ok, #{ <<"start-offset">> := LaterOffset }} =
        hb_store_arweave:read_offset(StoreOpts, LaterID),
    Query =
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
        """>>,
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
            ?assertEqual(hb_util:bin(FirstOffset), FirstCursor),
            SecondRes =
                dev_query_graphql:test_query(
                    Node,
                    Query,
                    #{
                        <<"ids">> => [EarlierID, LaterID],
                        <<"sort">> => Order,
                        <<"first">> => 1,
                        <<"after">> => FirstID
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
            ?assertEqual(hb_util:bin(SecondOffset), SecondCursor)
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

%% @doc Test single transaction query by ID
transaction_query_by_id_test() ->
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()]
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
transaction_query_full_test() ->
    Opts =
        #{
            priv_wallet => SenderKey = ar_wallet:new(),
            store => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    Alice = ar_wallet:new(),
    ?event({alice, Alice, {explicit, hb_util:human_id(Alice)}}),
    AliceAddress = hb_util:human_id(Alice),
    SenderAddress = hb_util:human_id(SenderKey),
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

%% @doc Test single transaction query with non-existent ID
transaction_query_not_found_test() ->
    Opts =
        #{
            priv_wallet => ar_wallet:new(),
            store => [hb_test_utils:test_store()]
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
transaction_query_with_anchor_test() ->
    Opts =
        #{
            priv_wallet => Wallet = ar_wallet:new(),
            store => [hb_test_utils:test_store()]
        },
    Node = hb_http_server:start_node(Opts),
    {ok, ID} =
        hb_cache:write(
            hb_message:convert(
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
                <<"id">> => ID
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
