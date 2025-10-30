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
dispatch(State = #{ queue := Q }, Opts) ->
    % Lists aren't handled well, so to avoid an ao-types key being inserted,
    % we'll convert the list to a nummbered message explicitly.
    OrderedMap = hb_util:list_to_numbered_message(lists:reverse(Q)),
    % Convert to a #tx so we can get the data_size and use it to query the
    % upload price.
    Bundle = hb_message:convert(OrderedMap,
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true }, Opts),
    Price = hb_util:ok(get_price(Bundle#tx.data_size, Opts)),
    Anchor = hb_util:ok(get_anchor(Opts)),
    % Now that we have the #tx record ready to go, convert back to a message...
    Msg = hb_message:convert(
        Bundle#tx{ anchor = Anchor, reward = Price },
        #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
        Opts),
    % ...and commit it
    Committed = hb_message:commit(Msg, Opts,
        #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true }),
    ?event({posting_tx, Committed}),
    {ok, _} =
        hb_ao:resolve(
            #{ <<"device">> => <<"arweave@2.9-pre">> },
            Committed#{ 
                <<"path">> => <<"/tx">>,
                <<"method">> => <<"POST">>
            },
            Opts
        ),
    server(State#{ queue => [], bytes => 0 }, Opts).

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

test_bundle(Opts) ->
    Anchor = rand:bytes(32),
    Price = 12345,
    % NodeOpts redirects arweave gateway requests to the mock server.
    {ServerHandle, NodeOpts} = start_gateway_mock_server(Price, Anchor),
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
        %% Wait for expected transaction
        hb_util:wait_until(
            fun() ->
                Requests = hb_mock_server:get_requests(ServerHandle, tx),
                length(Requests) >= 1
            end,
            5000
        ),
        TXs = hb_mock_server:get_requests(ServerHandle, tx),
        ?assertEqual(1, length(TXs)),
        %% Wait for expected chunks
        hb_util:wait_until(
            fun() ->
                Requests = hb_mock_server:get_requests(ServerHandle, chunk),
                length(Requests) >= 4
            end,
            5000
        ),
        Proofs = hb_mock_server:get_requests(ServerHandle, chunk),
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

start_gateway_mock_server(Price, Anchor) ->
    Endpoints = [
        {"/chunk", chunk},
        {"/tx", tx},
        {"/price/:size", price, {200, integer_to_binary(Price)}},
        {"/tx_anchor", tx_anchor, {200, hb_util:encode(Anchor)}}
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