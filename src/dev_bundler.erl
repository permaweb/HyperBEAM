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

%% @doc Implements an `up.arweave.net'-compatible endpoint for
%% bundling messages. 
item(_Base, Req, Opts) ->
    ServerPID = ensure_server(Opts),
    case verify_item(Req, Opts) of
        {ok, Item} ->
            ItemID = hb_message:id(Item, signed, Opts),
            case cache_item(Item, Opts) of
                ok ->
                    % Queue the item for bundling
                    % (fire-and-forget, ignore errors)
                     ?event(bundler_short, {queueing_item, 
                        {id, {explicit, ItemID}}}),
                    ServerPID ! {item, Item},
                    {ok, #{
                        <<"id">> => ItemID,
                        <<"timestamp">> => erlang:system_time(millisecond)
                    }};
                {error, Reason} ->
                    ?event(bundler_short, {cache_write_failed,
                        {id, {explicit, ItemID}}, {reason, Reason}}),
                    {error, #{
                        <<"status">> => 500,
                        <<"error">> => <<"cache_write_failed">>,
                        <<"details">> => list_to_binary(io_lib:format("~p", [Reason]))
                    }}
            end;
        {error, Reason} ->
            {error, #{
                <<"status">> => 400,
                <<"error">> => <<"invalid_item">>,
                <<"details">> => list_to_binary(io_lib:format("~p", [Reason]))
            }}
    end.

%% @doc Verify an item by extracting committed fields and checking signatures.
%% Returns {ok, Item} or {error, Reason}.
verify_item(Req, Opts) ->
    case hb_message:with_only_committed(Req, Opts) of
        {ok, Item} ->
            case hb_message:verify(Item, all, Opts) of
                true -> {ok, Item};
                false ->
                    ?event(bundler_short, {verify_failed, 
                        {id, {explicit, hb_message:id(Item, signed, Opts)}},
                        {reason, signature_verification_failed}}),
                    {error, signature_verification_failed}
            end;
        {error, Reason} ->
            ?event(bundler_short, {verify_failed, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Cache an item.
%% Returns ok or {error, Reason}.
cache_item(Item, Opts) ->
    try
        dev_bundler_cache:write_item(Item, Opts)
    catch
        Type:ExceptionReason ->
            {error, {Type, ExceptionReason}}
    end.

%%% Bundling server.

%% @doc Return the PID of the bundler server. If the server is not running,
%% it is started and registered with the name `?SERVER_NAME'.
ensure_server(Opts) ->
    case hb_name:lookup(?SERVER_NAME) of
        undefined ->
            PID = spawn(fun() -> init(Opts) end),
            ?event(bundler_short, {starting_bundler_server, {pid, PID}}),
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
    % Start the dispatcher to recover any in-progress bundles
    dev_bundler_dispatch:ensure_dispatcher(Opts),
    % Recover any unbundled items from cache
    {UnbundledItems, RecoveredBytes} = recover_unbundled_items(Opts),
    InitialState = #{
        max_size => hb_opts:get(
            bundler_max_size, ?DEFAULT_MAX_SIZE, Opts),
        max_idle_time => hb_opts:get(
            bundler_max_idle_time, ?DEFAULT_MAX_IDLE_TIME, Opts),
        max_items => hb_opts:get(
            bundler_max_items, ?DEFAULT_MAX_ITEMS, Opts),
        queue => UnbundledItems,
        bytes => RecoveredBytes
    },
    % If recovered items are ready to dispatch, do so immediately
    State = maybe_dispatch(InitialState, Opts),
    server(State, Opts).

%% @doc Recover unbundled items from cache and calculate their total size.
%% Returns {Items, TotalBytes}.
recover_unbundled_items(Opts) ->
    UnbundledItems = dev_bundler_cache:load_unbundled_items(Opts),
    ?event(bundler_short, {recovered_unbundled_items, length(UnbundledItems)}),
    % Calculate total bytes for recovered items
    RecoveredBytes = lists:foldl(
        fun(Item, Acc) ->
            Acc + erlang:external_size(Item)
        end,
        0,
        UnbundledItems
    ),
    {UnbundledItems, RecoveredBytes}.

%% @doc The main loop of the bundler server. Simply waits for messages to be
%% added to the queue, and then dispatches them when the queue is large enough.
server(State = #{ max_idle_time := MaxIdleTime }, Opts) ->
    receive
        {item, Item} ->
            server(maybe_dispatch(add_to_queue(Item, State, Opts), Opts), Opts);
        stop ->
            exit(normal)
    after MaxIdleTime ->
        Q = maps:get(queue, State),
        dev_bundler_dispatch:dispatch(Q, Opts),
        server(State#{ queue => [] }, Opts)
    end.

%% @doc Add an item to the queue. Update the state with the new queue
%% and approximate total byte size of the queue.
%% Note: Item has already been verified and cached before reaching here.
add_to_queue(Item, State = #{ queue := Queue, bytes := Bytes }, Opts) ->
    ItemSize = erlang:external_size(Item),
    State#{
        queue => [Item | Queue],
        bytes => Bytes + ItemSize
    }.

%% @doc Dispatch the queue if it is ready.
%% Only dispatches up to max_items at a time to respect the limit.
maybe_dispatch(State = #{queue := Q, max_items := MaxItems}, Opts) ->
    case dispatchable(State, Opts) of
        true ->
            % Only dispatch up to max_items, keep the rest in queue
            {ToDispatch, Remaining} = split_queue(Q, MaxItems),
            dev_bundler_dispatch:dispatch(ToDispatch, Opts),
            % Recalculate bytes for remaining items
            RemainingBytes = lists:foldl(
                fun(Item, Acc) -> Acc + erlang:external_size(Item) end,
                0,
                Remaining
            ),
            NewState = State#{queue => Remaining, bytes => RemainingBytes},
            % Check if we should dispatch again (in case we have more than max_items)
            maybe_dispatch(NewState, Opts);
        false -> State
    end.

%% @doc Split a queue into items to dispatch (up to max) and remaining items.
split_queue(Queue, MaxItems) when length(Queue) =< MaxItems ->
    {Queue, []};
split_queue(Queue, MaxItems) ->
    {ToDispatch, Remaining} = lists:split(MaxItems, Queue),
    {ToDispatch, Remaining}.

%% @doc Returns whether the queue is dispatchable.
dispatchable(#{ queue := Q, max_items := MaxLen }, _Opts)
        when length(Q) >= MaxLen ->
    true;
dispatchable(#{ bytes := Bytes, max_size := MaxSize }, _Opts)
        when Bytes >= MaxSize ->
    true;
dispatchable(_State, _Opts) ->
    false.

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
    {ServerHandle, NodeOpts} = start_mock_gateway(
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
            store => hb_test_utils:test_store(),
            bundler_max_items => 1
        }),
        Item1 = new_data_item(1, floor(2.5 * ?DATA_CHUNK_SIZE)),
        ?assertMatch({ok, _}, post_data_item(Node, Item1, ClientOpts)),
        % After a tx request fails it should be retried indefinitely. We'll
        % wait for a few retries then continue.
        TXs = hb_mock_server:get_requests(tx, 2, ServerHandle),
        ?assert(length(TXs) >= 2),
        Chunks = hb_mock_server:get_requests(chunk, 1, ServerHandle, 500),
        ?assertEqual([], Chunks),
        ok
    after
        %% Always cleanup, even if test fails
        stop_test_servers(ServerHandle)
    end.

unsigned_dataitem_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    % NodeOpts redirects arweave gateway requests to the mock server.
    {ServerHandle, NodeOpts} = start_mock_gateway(
        #{
            price => {200, integer_to_binary(Price)},
            tx_anchor => {200, hb_util:encode(Anchor)}
        }
    ),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            debug_print => false
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
        %% Always cleanup, even if test fails
        stop_test_servers(ServerHandle)
    end.

idle_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    % NodeOpts redirects arweave gateway requests to the mock server.
    {ServerHandle, NodeOpts} = start_mock_gateway(
        #{
            price => {200, integer_to_binary(Price)},
            tx_anchor => {200, hb_util:encode(Anchor)}
        }
    ),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            bundler_max_idle_time => 400,
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store()
        }),
        %% Upload 1 data items across 2 chunks.
        Item1 = new_data_item(1, floor(1.5 * ?DATA_CHUNK_SIZE)),
        ?assertMatch({ok, _}, post_data_item(Node, Item1, ClientOpts)),
        % Wait just to give the server a chance to post a transaction
        % (but it shouldn't)
        timer:sleep(150),
        ?assertEqual(0, length(hb_mock_server:get_requests(tx, 0, ServerHandle))),
        ?assertEqual(0, length(hb_mock_server:get_requests(chunk, 0, ServerHandle))),
        % Wait gain to give the server a chance to trip the max idle time.
        % It should *now* post a transaction.
        timer:sleep(300),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        %% Wait for expected chunks
        Proofs = hb_mock_server:get_requests(chunk, 2, ServerHandle),
        ?assertEqual(2, length(Proofs)),
        assert_bundle(Node, [Item1], Anchor, Price, hd(TXs), Proofs, ClientOpts),
        ok
    after
        %% Always cleanup, even if test fails
        stop_test_servers(ServerHandle)
    end.

dispatch_blocking_test() ->
    BlockTime = 500,
    Anchor = rand:bytes(32),
    Price = 12345,
    % NodeOpts redirects arweave gateway requests to the mock server.
    {ServerHandle, NodeOpts} = start_mock_gateway(
        #{
            price => {200, integer_to_binary(Price)},
            tx_anchor => {200, hb_util:encode(Anchor)},
            tx => fun(_Req) ->
                timer:sleep(BlockTime),
                {200, <<"Transaction posted">>}
            end
        }
    ),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 3
        }),
        %% Upload 4 data items and time each post
        Item1 = new_data_item(1, 10),
        {Time1, {ok, _}} =
            timer:tc(fun() -> post_data_item(Node, Item1, ClientOpts) end),
        Item2 = new_data_item(2, 10),
        {Time2, {ok, _}} = 
            timer:tc(fun() -> post_data_item(Node, Item2, ClientOpts) end),
        Item3 = new_data_item(3, 10),
        {Time3, {ok, _}} =
            timer:tc(fun() -> post_data_item(Node, Item3, ClientOpts) end),
        Item4 = new_data_item(4, 10),
        {Time4, {ok, _}} =
            timer:tc(fun() -> post_data_item(Node, Item4, ClientOpts) end),
        %% Assert that the 4th item takes no longer than twice the slowest of
        %% the first 3. This verifies that we aren't blocking on the tx
        %% bundle dispatching.
        Slowest = lists:max([Time1, Time2, Time3]),
        ?event(debug_test, {post_times,
            {item1, Time1}, {item2, Time2}, {item3, Time3}, {item4, Time4},
            {slowest, Slowest}, {max_allowed, 2 * Slowest}
        }),
        ?assert(Time4 =< 2 * Slowest),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        %% Wait for expected chunks
        Proofs = hb_mock_server:get_requests(chunk, 1, ServerHandle),
        ?assertEqual(1, length(Proofs)),
        assert_bundle(
            Node,
            [Item1, Item2, Item3],
            Anchor, Price, hd(TXs), Proofs, ClientOpts),
        ok
    after
        %% Always cleanup, even if test fails
        stop_test_servers(ServerHandle)
    end.

recover_unbundled_items_test() ->
    Opts = #{store => hb_test_utils:test_store()},
    % Create and cache some items
    Item1 = hb_message:convert(new_data_item(1, 10), <<"structured@1.0">>, <<"ans104@1.0">>, Opts),
    Item2 = hb_message:convert(new_data_item(2, 10), <<"structured@1.0">>, <<"ans104@1.0">>, Opts),
    Item3 = hb_message:convert(new_data_item(3, 10), <<"structured@1.0">>, <<"ans104@1.0">>, Opts),
    ok = dev_bundler_cache:write_item(Item1, Opts),
    ok = dev_bundler_cache:write_item(Item2, Opts),
    ok = dev_bundler_cache:write_item(Item3, Opts),
    % Bundle Item2 with a fake TX
    FakeTX = ar_tx:sign(#tx{format = 2, tags = [{<<"test">>, <<"tx">>}]}, hb:wallet()),
    StructuredTX = hb_message:convert(FakeTX, <<"structured@1.0">>, <<"tx@1.0">>, Opts),
    ok = dev_bundler_cache:write_tx(StructuredTX, [Item2], Opts),
    % Now recover unbundled items
    {RecoveredItems, RecoveredBytes} = recover_unbundled_items(Opts),
    ?assertEqual(3924, RecoveredBytes),
    RecoveredItems2 = [
        hb_message:with_commitments(
            #{ <<"commitment-device">> => <<"ans104@1.0">> }, Item, Opts)
        || Item <- RecoveredItems],
    ?assertEqual(lists:sort([Item1, Item3]), lists:sort(RecoveredItems2)),
    ok.

%% @doc Test that items are recovered and posted while respecting the
%% max_items limit.
recover_respects_max_items_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        % Use max_items of 3, so 10 items should dispatch as 3+3+3+1
        MaxItems = 3,
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => MaxItems
        },
        % Create and cache 10 unbundled items
        NumItems = 10,
        lists:foreach(
            fun(I) ->
                Item = hb_message:convert(
                    new_data_item(I, 10),
                    <<"structured@1.0">>,
                    <<"ans104@1.0">>,
                    Opts
                ),
                ok = dev_bundler_cache:write_item(Item, Opts)
            end,
            lists:seq(1, NumItems)
        ),
        % Start the node and bundler server (which recovers unbundled items)
        hb_http_server:start_node(Opts),
        ensure_server(Opts),        
        % Should dispatch 3 bundles and leave one item in the queue
        TXs = hb_mock_server:get_requests(tx, 3, ServerHandle),
        ?assertEqual(3, length(TXs)),
        ok
    after
        stop_test_servers(ServerHandle)
    end.

invalid_item_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        ClientOpts = #{},
        TestOpts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store()
        },
        Node = hb_http_server:start_node(TestOpts#{
            debug_print => false
        }),
        % Create a valid signed item
        Item = ar_bundles:sign_item(
            #tx{
                data = <<"testdata">>,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
            hb:wallet()
        ),
        % Tamper with the data after signing (this invalidates the signature)
        TamperedItem = Item#tx{data = <<"tampereddata">>},
        % Posting via HTTP fails upstream during ANS104 decode/verify.
        PostResult = post_data_item(Node, TamperedItem, ClientOpts),
        ?assertMatch({failure, #{<<"status">> := 500}}, PostResult),
        % Calling dev_bundler directly should return the intended 400.
        StructuredItem = hb_message:convert(
            TamperedItem, <<"structured@1.0">>, <<"ans104@1.0">>, TestOpts),
        DirectResult = dev_bundler:item(#{}, StructuredItem, TestOpts),
        ?assertMatch({error, #{
            <<"status">> := 400,
            <<"error">> := <<"invalid_item">>,
            <<"details">> := <<"signature_verification_failed">>}}, DirectResult),
        ok
    after
        stop_test_servers(ServerHandle)
    end.

cache_write_failure_test() ->
    GoodOpts = #{store => hb_test_utils:test_store()},
    BadOpts = #{
        store => undefined,
        debug_print => false
    }, % Invalid store will cause cache write to fail
    try
        % Start bundler with a valid store so recovery/init paths succeed.
        ensure_server(GoodOpts),
        Item = ar_bundles:sign_item(
            #tx{
                data = <<"testdata">>,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
            hb:wallet()
        ),
        StructuredItem = hb_message:convert(
            Item, <<"structured@1.0">>, <<"ans104@1.0">>, GoodOpts),
        % Call item/3 directly without a store, should cause cache write
        % to fail.
        Result = dev_bundler:item(#{}, StructuredItem, BadOpts),
        ?assertMatch({error, #{
            <<"status">> := 500,
            <<"error">> := <<"cache_write_failed">>}}, Result),
        ok
    after
        stop_server(),
        dev_bundler_dispatch:stop_dispatcher()
    end.

stop_test_servers(ServerHandle) ->
    hb_mock_server:stop(ServerHandle),
    stop_server(),
    dev_bundler_dispatch:stop_dispatcher().

test_bundle(Opts) ->
    Anchor = rand:bytes(32),
    Price = 12345,
    % NodeOpts redirects arweave gateway requests to the mock server.
    {ServerHandle, NodeOpts} = start_mock_gateway(
        #{
            price => {200, integer_to_binary(Price)},
            tx_anchor => {200, hb_util:encode(Anchor)}
        }
    ),
    try
        ClientOpts = #{},
        NodeOpts2 = maps:merge(NodeOpts, Opts),
        Node = hb_http_server:start_node(NodeOpts2#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store()
        }),
        %% Upload 3 data items across 4 chunks.
        Item1 = new_data_item(1, floor(2.5 * ?DATA_CHUNK_SIZE)),
        ?assertMatch({ok, _}, post_data_item(Node, Item1, ClientOpts)),
        Item2 = new_data_item(2, ?DATA_CHUNK_SIZE),
        ?assertMatch({ok, _}, post_data_item(Node, Item2, ClientOpts)),
        Item3 = new_data_item(3, floor(0.25 * ?DATA_CHUNK_SIZE)),
        ?assertMatch({ok, _}, post_data_item(Node, Item3, ClientOpts)),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        %% Wait for expected chunks
        Proofs = hb_mock_server:get_requests(chunk, 4, ServerHandle),
        ?assertEqual(4, length(Proofs)),
        assert_bundle(
            Node,
            [Item1, Item2, Item3], Anchor, Price, hd(TXs), Proofs, ClientOpts),
        ok
    after
        %% Always cleanup, even if test fails
        stop_test_servers(ServerHandle)
    end.

test_api_error(Responses) ->
    {ServerHandle, NodeOpts} = start_mock_gateway(Responses),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1
        }),
        Item1 = new_data_item(1, floor(2.5 * ?DATA_CHUNK_SIZE)),
        ?assertMatch({ok, _}, post_data_item(Node, Item1, ClientOpts)),
        % Since there was an error either before or while posting the tx,
        % no bundles should be posted and no chunks should be posted.
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle, 200),
        ?assertEqual([], TXs),
        Chunks = hb_mock_server:get_requests(chunk, 1, ServerHandle, 200),
        ?assertEqual([], Chunks),
        % Now that we dispatch asynchronously, an error won't cause the
        % Item to remain in the queue. Instead we'll rely on the retry
        % logic to pick it up.
        ok
    after
        %% Always cleanup, even if test fails
        stop_test_servers(ServerHandle)
    end.

new_data_item(Index, Size) ->
    Data = rand:bytes(Size),
    Tag = <<"tag", (integer_to_binary(Index))/binary>>,
    Value = <<"value", (integer_to_binary(Index))/binary>>,
    ar_bundles:sign_item(
        #tx{
            data = Data,
            tags = [{Tag, Value}]
        },
        hb:wallet()
    ).

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

assert_bundle(Node, ExpectedItems, Anchor, Price, TXRequest, Proofs, ClientOpts) ->
    %% Reconstitute the transaction with its data from the POSTed payloads.
    TXBinary = maps:get(<<"body">>, TXRequest),
    TXJSON = hb_json:decode(TXBinary),
    TXHeader = ar_tx:json_struct_to_tx(TXJSON),
    %% Decode all chunks with their offsets, sort by offset, then concatenate
    ChunksWithOffsets = lists:map(
        fun(ChunkRequest) ->
            ProofBinary = maps:get(<<"body">>, ChunkRequest),
            ProofJSON = hb_json:decode(ProofBinary),
            Offset = binary_to_integer(maps:get(<<"offset">>, ProofJSON)),
            Chunk = hb_util:decode(maps:get(<<"chunk">>, ProofJSON)),
            DataRoot = hb_util:decode(maps:get(<<"data_root">>, ProofJSON)),
            DataSize = binary_to_integer(maps:get(<<"data_size">>, ProofJSON)),
            DataPath = hb_util:decode(maps:get(<<"data_path">>, ProofJSON)),
            Valid = ar_merkle:validate_path(DataRoot, Offset, DataSize, DataPath),
            ?assertNotEqual(false, Valid),
            {ChunkID, StartOffset, EndOffset} = Valid,
            ?assertEqual(ChunkID, ar_tx:generate_chunk_id(Chunk)),
            ?assertEqual(EndOffset - StartOffset, byte_size(Chunk)),
            {Offset, Chunk}
        end,
        Proofs
    ),
    SortedChunks = lists:sort(fun({O1, _}, {O2, _}) -> O1 =< O2 end, ChunksWithOffsets),
    Chunks = [Chunk || {_Offset, Chunk} <- SortedChunks],
    DataBinary = iolist_to_binary(Chunks),
    TX = TXHeader#tx{ data = DataBinary },
    ?event(debug_test, {tx, TX}),
    ?assert(ar_tx:verify(TX)),
    ?assertEqual(Anchor, TX#tx.anchor),
    ?assertEqual(Price, TX#tx.reward),
    TXStructured = hb_message:convert(
        TX, <<"structured@1.0">>, <<"tx@1.0">>, ClientOpts),
    ?event(debug_test, {tx_structured, TXStructured}),
    ?assert(hb_message:verify(TXStructured, all, ClientOpts)),
    %% Verify individual data items in the bundle
    BundleDeserialized = ar_bundles:deserialize(TX),
    ?event(debug_test, {bundle_deserialized, BundleDeserialized}),
    ?assertEqual(length(ExpectedItems), maps:size(BundleDeserialized#tx.data)),
    %% Verify each data item's signature and match with expected items
    lists:foreach(
        fun({Index, ExpectedItem}) ->
            Key = integer_to_binary(Index),
            BundledItem = maps:get(Key, BundleDeserialized#tx.data),
            ?assert(ar_bundles:verify_item(BundledItem)),
            ?assertEqual(ExpectedItem, BundledItem)
        end,
        lists:zip(lists:seq(1, length(ExpectedItems)), ExpectedItems)
    ),
    ?assertEqual(undefined, TX#tx.manifest),
    ?assertEqual(undefined, BundleDeserialized#tx.manifest),
    % Verify that the TX was cached
    SignedTXID = hb_message:id(TXStructured, signed, ClientOpts),
    CachedTXFromSignedID = dev_cache:read_from_cache(Node, SignedTXID),
    ?assert(hb_message:verify(CachedTXFromSignedID, all, ClientOpts)),
    UnsignedTXID = hb_message:id(TXStructured, unsigned, ClientOpts),
    CachedTXFromUnsignedID = dev_cache:read_from_cache(Node, UnsignedTXID),
    ?assert(hb_message:verify(CachedTXFromUnsignedID, all, ClientOpts)),
    % Verify that the items were cached
    lists:foreach(
        fun(Item) ->
            ItemStructured = hb_message:convert(
                Item, <<"structured@1.0">>, <<"ans104@1.0">>, ClientOpts),
            SignedItemID = hb_message:id(ItemStructured, signed, ClientOpts),
            CachedItemFromSignedID = dev_cache:read_from_cache(Node, SignedItemID),
            ?assert(hb_message:verify(CachedItemFromSignedID, all, ClientOpts)),
            UnsignedItemID = hb_message:id(ItemStructured, unsigned, ClientOpts),
            CachedItemFromUnsignedID = dev_cache:read_from_cache(Node, UnsignedItemID),
            ?assert(hb_message:verify(CachedItemFromUnsignedID, all, ClientOpts))
        end, ExpectedItems),
    ok.

start_mock_gateway(Responses) ->
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
                    <<"opts">> => #{http_client => httpc, protocol => http2}
                }
            }
        ]
    },
    {ServerHandle, NodeOpts}.
