%%% @doc A device that offers a bundling service for HyperBEAM users and other
%%% devices/nodes.
%%%
%%% The role of a bundler in the Arweave ecosystem is to create a single nested
%%% transaction that contains multiple data items. Because an extremely large
%%% number of items can be written to the network using only one transaction
%%% (max 2^256 bytes of combined data and headers), they allow the network to
%%% scale to without practical limits.
%%%
%%% When users post to the `~bundler@1.0' device, their request is written to
%%% the node's internal cache, and added to a queue of requests to be bundled.
%%% Once the queue reaches the node-operator's desired size, it is automatically
%%% bundled into one transaction, signed and dispatched to the network. Writing
%%% the message to the cache before transmission ensures that the message is
%%% available for reading instantly (`optimistically'), even before the
%%% transaction is dispatched.
-module(dev_bundler).
-export([tx/3, item/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Default options.
-define(SERVER_NAME, bundler_server).
-define(DEFAULT_MAX_SIZE, 100_000_000). % 100 MB.
-define(DEFAULT_MAX_IDLE_TIME, 300_000). % 5 minutes.
-define(DEFAULT_MAX_ITEMS, 1000).

%%% Public interface.

%% @doc An alias for `item/3'.
tx(Base, Req, Opts) ->
    item(Base, Req, Opts).

%% @doc Implements an Arweave/`up.arweave.net'-compatible endpoint for
%% bundling messages. 
item(Base, Req, Opts) ->
    PID = ensure_server(Opts),
    PID ! {item, self(), Ref = make_ref(), Base, Req},
    receive
        {response, Ref, Res} -> Res
    end.

%%% Bundling server.

%% @doc Return the PID of the bundler server. If the server is not running,
%% it is started and registered with the name `?SERVER_NAME'.
ensure_server(Opts) ->
    case hb_name:lookup(?SERVER_NAME) of
        undefined ->
            PID = spawn(fun() -> init(Opts) end),
            hb_name:register(?SERVER_NAME, PID),
            hb_name:lookup(?SERVER_NAME);
        PID -> PID
    end.

stop_server() ->
    case hb_name:lookup(?SERVER_NAME) of
        undefined -> ok;
        PID ->
            PID ! stop,
            hb_name:unregister(?SERVER_NAME)
    end.

%% @doc Initialize the bundler server.
init(Opts) ->
    server(
        #{
            max_size => hb_opts:get(
                bundler_max_size, ?DEFAULT_MAX_SIZE, Opts),
            max_idle_time => hb_opts:get(
                bundler_max_idle_time, ?DEFAULT_MAX_IDLE_TIME, Opts),
            max_items => hb_opts:get(
                bundler_max_items, ?DEFAULT_MAX_ITEMS, Opts),
            queue => [],
            bytes => 0
        },
        Opts
    ).

%% @doc The main loop of the bundler server. Simply waits for messages to be
%% added to the queue, and then dispatches them when the queue is large enough.
server(State = #{ max_idle_time := MaxIdleTime }, Opts) ->
    receive
        {item, From, Ref, _Base, Req} ->
            From ! {response, Ref, {ok, <<"Message queued.">>}},
            server(maybe_dispatch(add_item(Req, State, Opts), Opts), Opts);
        {get_state, From, Ref} ->
            % Only used in tests.
            From ! {response, Ref, State},
            server(State, Opts);
        stop ->
            exit(normal)
    after MaxIdleTime ->
        server(dispatch(State, Opts), Opts)
    end.

%% @doc Add an item to the queue. Update the state with the new queue and
%% approximate total byte size of the queue.
add_item(Req, State = #{ queue := Queue, bytes := Bytes }, Opts) ->
    {ok, Item} = hb_message:with_only_committed(Req, Opts),
    ItemSize = erlang:external_size(Item),
    ?event({adding_item, {item_size, ItemSize}, {req, Req}, {item, Item}}),
    {ok, _} = hb_cache:write(Item, Opts),
    State#{
        queue => [Item | Queue],
        bytes => Bytes + ItemSize
    }.

%% @doc Dispatch the queue if it is ready.
maybe_dispatch(State, Opts) ->
    case dispatchable(State, Opts) of
        true -> dispatch(State, Opts);
        false -> State
    end.

%% @doc Returns whether the queue is dispatchable.
dispatchable(#{ queue := Q, max_items := MaxLen }, Opts)
        when length(Q) >= MaxLen ->
    true;
dispatchable(#{ bytes := Bytes, max_size := MaxSize }, Opts)
        when Bytes >= MaxSize ->
    true;
dispatchable(_State, _Opts) ->
    false.

%% @doc Dispatch the queue.
dispatch(State = #{ queue := [] }, Opts) ->
    ?event({skipping_empty_queue}),
    server(State, Opts);
dispatch(State = #{ queue := Q }, Opts) ->
    % Use dev_codec_tx:to directly to build a bundled L1 transaction from
    % a list of dataitems. We do this rather than hb_message:convert because
    % hb_message doesn't handle lists natively - and it's tricky to get it
    % to manage the data conversions idempotently.
    {ok, Bundle} = dev_codec_tx:to(lists:reverse(Q), #{}, Opts),
    case {get_price(Bundle#tx.data_size, Opts), get_anchor(Opts)} of
        {{ok, Price}, {ok, Anchor}} ->
            dispatch_bundle(
                Bundle#tx{ anchor = Anchor, reward = Price }, State, Opts);
        {PriceError, AnchorError} ->
            ?event({unable_to_dispatch,
                {price_error, PriceError}, {anchor_error, AnchorError}}),
            server(State, Opts)
    end.

dispatch_bundle(Bundle, State, Opts) ->
    ?event(debug_test, {dispatching_bundle, {bundle, Bundle}}),
    Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
    Signed = ar_tx:sign(Bundle, Wallet),
    % Now that we have the #tx record ready to go, convert back to a message
    Committed = hb_message:convert(
        Signed,
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
        Opts),
    ?event(debug_test, {posting_tx, Committed}),
    PostTXResponse =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9-pre">> },
            Committed#{ 
                <<"path">> => <<"/tx">>,
                <<"method">> => <<"POST">>
            },
            Opts
        ),
    case PostTXResponse of
        {ok, _} ->
            server(State#{ queue => [], bytes => 0 }, Opts);
        {_, Error} ->
            ?event({unable_to_dispatch, {tx_error, Error}}),
            server(State, Opts)
    end.

get_price(DataSize, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{ <<"path">> => <<"/price">>, <<"size">> => DataSize },
        Opts
    ).

get_anchor(Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9-pre">> },
        #{ <<"path">> => <<"/tx_anchor">> },
        Opts
    ).

%%%===================================================================
%%% Tests
%%%===================================================================

bundle_count_test() ->
    test_bundle(#{ bundler_max_items => 3 }).

bundle_size_test() ->
    test_bundle(#{ bundler_max_size => floor(3.6 * ?DATA_CHUNK_SIZE) }).

price_error_test() ->
    test_api_error(#{
        price => {500, <<"error">>},
        tx_anchor => {200, hb_util:encode(rand:bytes(32))}
    }).

anchor_error_test() ->
    test_api_error(#{
        price => {200, <<"12345">>},
        tx_anchor => {500, <<"error">>}
    }).

tx_error_test() ->
    {ServerHandle, NodeOpts} = start_gateway_mock_server(
        #{
            tx => {400, <<"Transaction verification failed.">>},
            price => {200, <<"12345">>},
            tx_anchor => {200, hb_util:encode(rand:bytes(32))}
        }
    ),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            priv_wallet => hb:wallet(),
            bundler_max_items => 1
        }),
        Data1 = rand:bytes(floor(2.5 * ?DATA_CHUNK_SIZE)),
        Wallet1 = hb:wallet(),
        Item1 = ar_bundles:sign_item(
            #tx{
                data = Data1,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
            Wallet1
        ),
        ?assertMatch({ok, _}, post_data_item(Node, Item1, ClientOpts)),
        % We attempted to post the tx, so it will show up in the mocked
        % requests. But since the verificaiton failed, the data item should
        % still be queued.
        TXs = get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        Chunks = get_requests(chunk, 1, ServerHandle),
        ?assertEqual([], Chunks),
        % The item should still be in the bundler queue.
        ItemStructured = hb_message:convert(
            Item1, <<"structured@1.0">>, <<"ans104@1.0">>, ClientOpts),
        #{ queue := Queue, bytes := Bytes } = get_state(ClientOpts),
        ?assertEqual([ItemStructured], Queue),
        ?assertEqual(657070, Bytes),
        ok
    after
        %% Always cleanup, even if test fails
        hb_mock_server:stop(ServerHandle),
        stop_server()
    end.

unsigned_dataitem_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    % NodeOpts redirects arweave gateway requests to the mock server.
    {ServerHandle, NodeOpts} = start_gateway_mock_server(
        #{
            price => {200, integer_to_binary(Price)},
            tx_anchor => {200, hb_util:encode(Anchor)}
        }
    ),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            priv_wallet => hb:wallet()
        }),
        Item = #tx{
                data = <<"testdata">>,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
        % This should probably be a 4XX error, but for now the hb_http_server
        % throws an exception when a message is not signed.
        Response = post_data_item(Node, Item, ClientOpts),
        ?assertMatch(
            {failure, #{ <<"status">> := 500 }},
            Response)
    after
        hb_mock_server:stop(ServerHandle),
        stop_server()
    end.

idle_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    % NodeOpts redirects arweave gateway requests to the mock server.
    {ServerHandle, NodeOpts} = start_gateway_mock_server(
        #{
            price => {200, integer_to_binary(Price)},
            tx_anchor => {200, hb_util:encode(Anchor)}
        }
    ),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            bundler_max_idle_time => 10000,
            priv_wallet => hb:wallet()
        }),
        %% Upload 1 data items across 2 chunks.
        Data1 = rand:bytes(floor(1.5 * ?DATA_CHUNK_SIZE)),
        Wallet1 = hb:wallet(),
        Item1 = ar_bundles:sign_item(
            #tx{
                data = Data1,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
            Wallet1
        ),
        ?assertMatch({ok, _}, post_data_item(Node, Item1, ClientOpts)),
        % Wait just to give the server a chance to post a transaction
        % (but it shouldn't)
        timer:sleep(2000),
        ?assertEqual(0, length(get_requests(tx, 0, ServerHandle))),
        ?assertEqual(0, length(get_requests(chunk, 0, ServerHandle))),
        % Wait gain to give the server a chance to trip the max idle time.
        % It should *now* post a transaction.
        timer:sleep(8000),
        TXs = get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        %% Wait for expected chunks
        Proofs = get_requests(chunk, 2, ServerHandle),
        ?assertEqual(2, length(Proofs)),
        %% Reconstitute the transaction with its data from the POSTed payloads.
        TXBinary = maps:get(<<"body">>, hd(TXs)),
        TXJSON = hb_json:decode(TXBinary),
        TXHeader = ar_tx:json_struct_to_tx(TXJSON),
        %% Decode all chunks and concatenate into one binary
        Chunks = lists:map(
            fun(ChunkRequest) ->
                ProofBinary = maps:get(<<"body">>, ChunkRequest),
                ProofJSON = hb_json:decode(ProofBinary),
                hb_util:decode(maps:get(<<"chunk">>, ProofJSON))
            end,
            Proofs
        ),
        DataBinary = iolist_to_binary(Chunks),
        TX = TXHeader#tx{ data = DataBinary },
        ?assert(ar_tx:verify(TX)),
        ?assertEqual(Anchor, TX#tx.anchor),
        ?assertEqual(Price, TX#tx.reward),
        ?event(debug_test, {tx,TX}),
        TXStructured = hb_message:convert(
            TX, <<"structured@1.0">>, <<"tx@1.0">>, ClientOpts),
        ?event(debug_test, {tx_structured, TXStructured}),
        ?assert(hb_message:verify(TXStructured, all, ClientOpts)),
        %% Verify individual data items in the bundle
        BundleDeserialized = ar_bundles:deserialize(TX),
        ?event(debug_test, {bundle_deserialized, BundleDeserialized}),
        ?assertEqual(1, maps:size(BundleDeserialized#tx.data)),
        #{<<"1">> := BundledItem1} = BundleDeserialized#tx.data,
        %% Verify each data item's signature
        ?assert(ar_bundles:verify_item(BundledItem1)),
        %% Verify that the data items match the original items
        ?assertEqual(Item1, BundledItem1),
        ?assertEqual(undefined, TX#tx.manifest),
        ?assertEqual(undefined, BundleDeserialized#tx.manifest),
        ok
    after
        %% Always cleanup, even if test fails
        hb_mock_server:stop(ServerHandle),
        stop_server()
    end.
    

test_bundle(Opts) ->
    Anchor = rand:bytes(32),
    Price = 12345,
    % NodeOpts redirects arweave gateway requests to the mock server.
    {ServerHandle, NodeOpts} = start_gateway_mock_server(
        #{
            price => {200, integer_to_binary(Price)},
            tx_anchor => {200, hb_util:encode(Anchor)}
        }
    ),
    try
        ClientOpts = #{},
        NodeOpts2 = maps:merge(NodeOpts, Opts),
        Node = hb_http_server:start_node(NodeOpts2#{
            priv_wallet => hb:wallet()
        }),
        %% Upload 3 data items across 4 chunks.
        Data1 = rand:bytes(floor(2.5 * ?DATA_CHUNK_SIZE)),
        Wallet1 = hb:wallet(),
        Item1 = ar_bundles:sign_item(
            #tx{
                data = Data1,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
            Wallet1
        ),
        ?assertMatch({ok, _}, post_data_item(Node, Item1, ClientOpts)),
        Data2 = rand:bytes(?DATA_CHUNK_SIZE),
        Wallet2 = hb:wallet(),
        Item2 = ar_bundles:sign_item(
            #tx{
                data = Data2,
                tags = [{<<"tag2">>, <<"value2">>}]
            },
            Wallet2
        ),
        ?assertMatch({ok, _}, post_data_item(Node, Item2, ClientOpts)),
        Data3 = rand:bytes(floor(0.25 * ?DATA_CHUNK_SIZE)),
        Wallet3 = hb:wallet(),
        Item3 = ar_bundles:sign_item(
            #tx{
                data = Data3,
                tags = [{<<"tag3">>, <<"value3">>}]
            },
            Wallet3
        ),
        ?assertMatch({ok, _}, post_data_item(Node, Item3, ClientOpts)),
        TXs = get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        %% Wait for expected chunks
        Proofs = get_requests(chunk, 4, ServerHandle),
        ?assertEqual(4, length(Proofs)),
        %% Reconstitute the transaction with its data from the POSTed payloads.
        TXBinary = maps:get(<<"body">>, hd(TXs)),
        TXJSON = hb_json:decode(TXBinary),
        TXHeader = ar_tx:json_struct_to_tx(TXJSON),
        %% Decode all chunks and concatenate into one binary
        Chunks = lists:map(
            fun(ChunkRequest) ->
                ProofBinary = maps:get(<<"body">>, ChunkRequest),
                ProofJSON = hb_json:decode(ProofBinary),
                hb_util:decode(maps:get(<<"chunk">>, ProofJSON))
            end,
            Proofs
        ),
        DataBinary = iolist_to_binary(Chunks),
        TX = TXHeader#tx{ data = DataBinary },
        ?assert(ar_tx:verify(TX)),
        ?assertEqual(Anchor, TX#tx.anchor),
        ?assertEqual(Price, TX#tx.reward),
        ?event(debug_test, {tx,TX}),
        TXStructured = hb_message:convert(
            TX, <<"structured@1.0">>, <<"tx@1.0">>, ClientOpts),
        ?event(debug_test, {tx_structured, TXStructured}),
        ?assert(hb_message:verify(TXStructured, all, ClientOpts)),
        %% Verify individual data items in the bundle
        BundleDeserialized = ar_bundles:deserialize(TX),
        ?event(debug_test, {bundle_deserialized, BundleDeserialized}),
        ?assertEqual(3, maps:size(BundleDeserialized#tx.data)),
        #{<<"1">> := BundledItem1, <<"2">> := BundledItem2, <<"3">> := BundledItem3} = 
            BundleDeserialized#tx.data,
        %% Verify each data item's signature
        ?assert(ar_bundles:verify_item(BundledItem1)),
        ?assert(ar_bundles:verify_item(BundledItem2)),
        ?assert(ar_bundles:verify_item(BundledItem3)),
        %% Verify that the data items match the original items
        ?assertEqual(Item1, BundledItem1),
        ?assertEqual(Item2, BundledItem2),
        ?assertEqual(Item3, BundledItem3),

        ?assertEqual(undefined, TX#tx.manifest),
        ?assertEqual(undefined, BundleDeserialized#tx.manifest),
        ok
    after
        %% Always cleanup, even if test fails
        hb_mock_server:stop(ServerHandle),
        stop_server()
    end.

test_api_error(Responses) ->
    {ServerHandle, NodeOpts} = start_gateway_mock_server(Responses),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            priv_wallet => hb:wallet(),
            bundler_max_items => 1
        }),
        Data1 = rand:bytes(floor(2.5 * ?DATA_CHUNK_SIZE)),
        Wallet1 = hb:wallet(),
        Item1 = ar_bundles:sign_item(
            #tx{
                data = Data1,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
            Wallet1
        ),
        ?assertMatch({ok, _}, post_data_item(Node, Item1, ClientOpts)),
        % Since thre was an error either before or while posting the tx,
        % no bundles should be posted and no chunks should be posted.
        TXs = get_requests(tx, 1, ServerHandle),
        ?assertEqual([], TXs),
        Chunks = get_requests(chunk, 1, ServerHandle),
        ?assertEqual([], Chunks),
        % The item should still be in the bundler queue.
        ItemStructured = hb_message:convert(
            Item1, <<"structured@1.0">>, <<"ans104@1.0">>, ClientOpts),
        #{ queue := Queue, bytes := Bytes } = get_state(ClientOpts),
        ?assertEqual([ItemStructured], Queue),
        ?assertEqual(657070, Bytes),
        ok
    after
        %% Always cleanup, even if test fails
        hb_mock_server:stop(ServerHandle),
        stop_server()
    end.

post_data_item(Node, Item, Opts) ->
    Serialized = ar_bundles:serialize(Item),
    hb_http:post(
        Node,
        #{
            <<"device">> => <<"bundler@1.0">>,
            <<"path">> => <<"/tx?codec-device=ans104@1.0">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"body">> => Serialized
        },
        Opts
    ).

get_requests(Type, Count, ServerHandle) ->
    %% Wait for expected transaction
    hb_util:wait_until(
        fun() ->
            Requests = hb_mock_server:get_requests(ServerHandle, Type),
            length(Requests) >= Count
        end,
        5000
    ),
    hb_mock_server:get_requests(ServerHandle, Type).

start_gateway_mock_server(Responses) ->
    DefaultResponse = {200, <<>>},
    Endpoints = [
        {"/chunk", chunk, maps:get(chunk, Responses, DefaultResponse)},
        {"/tx", tx, maps:get(tx, Responses, DefaultResponse)},
        {"/price/:size", price, maps:get(price, Responses, DefaultResponse)},
        {"/tx_anchor", tx_anchor, maps:get(tx_anchor, Responses, DefaultResponse)}
    ],
    {ok, MockServer, ServerHandle} = hb_mock_server:start(Endpoints),
    NodeOpts = #{
        gateway => MockServer,
        routes => [
            #{
                <<"template">> => <<"/arweave">>,
                <<"node">> => #{
                    <<"match">> => <<"^/arweave">>,
                    <<"with">> => MockServer,
                    <<"opts">> => #{ 
                        http_client => httpc, protocol => http2 }
                }
            }
        ]
    },
    {ServerHandle, NodeOpts}.

get_state(Opts) ->
    PID = ensure_server(Opts),
    PID ! {get_state, self(), Ref = make_ref()},
    receive
        {response, Ref, Res} -> Res
    end.