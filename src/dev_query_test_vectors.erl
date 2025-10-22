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
get_test_blocks(Node) ->
    InitialHeight = 1745749,
    FinalHeight = 1745750,
    lists:foreach(
        fun(Height) ->
            {ok, _} =
                hb_http:request(
                    <<"GET">>,
                    Node,
                    <<"/~arweave@2.9-pre/block=", (hb_util:bin(Height))/binary>>,
                    #{}
                )
        end,
        lists:seq(InitialHeight, FinalHeight)
    ).

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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    get_test_blocks(Node),
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
        },
    Node = hb_http_server:start_node(Opts),
    get_test_blocks(Node),
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
                <<"owners">> => [hb:address()]
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
                <<"owners">> => [hb:address()]
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
                <<"owners">> => [hb:address()],
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


%% @doc Test single transaction query by ID
transaction_query_by_id_test() ->
    Opts =
        #{
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
            priv_wallet => SenderKey = hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
            priv_wallet => hb:wallet(),
            store => [hb_test_utils:test_store(hb_store_lmdb)]
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
                    hb:wallet()
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