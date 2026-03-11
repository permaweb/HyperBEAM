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
-export([tx/3, item/3, ensure_server/1, stop_server/0, get_state/0]).
-include("include/hb.hrl").
-include("include/dev_bundler.hrl").
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
                    ServerPID ! {enqueue_item, Item},
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
    hb_name:singleton(
        ?SERVER_NAME,
        fun() -> init(Opts) end
    ).

stop_server() ->
    case hb_name:lookup(?SERVER_NAME) of
        undefined -> ok;
        PID ->
            PID ! stop,
            hb_name:unregister(?SERVER_NAME)
    end.

%% @doc Return the current bundler server state for tests.
get_state() ->
    case hb_name:lookup(?SERVER_NAME) of
        undefined -> undefined;
        PID ->
            PID ! {get_state, self(), Ref = make_ref()},
            receive
                {state, Ref, State} -> State
            after 1000 -> timeout
            end
    end.

%% @doc Initialize the bundler server.
init(Opts) ->
    NumWorkers = hb_opts:get(bundler_workers, ?DEFAULT_NUM_WORKERS, Opts),
    Workers = lists:map(
        fun(_) ->
            WorkerPID = spawn_link(fun dev_bundler_task:worker_loop/0),
            {WorkerPID, idle}
        end,
        lists:seq(1, NumWorkers)
    ),
    InitialState = #state{
        max_size = hb_opts:get(bundler_max_size, ?DEFAULT_MAX_SIZE, Opts),
        max_idle_time = hb_opts:get(
            bundler_max_idle_time, ?DEFAULT_MAX_IDLE_TIME, Opts),
        max_items = hb_opts:get(bundler_max_items, ?DEFAULT_MAX_ITEMS, Opts),
        queue = [],
        bytes = 0,
        workers = maps:from_list(Workers),
        task_queue = queue:new(),
        bundles = #{},
        opts = Opts
    },
    dev_bundler_recovery:recover_unbundled_items(self(), Opts),
    dev_bundler_recovery:recover_bundles(self(), Opts),
    server(assign_tasks(InitialState), Opts).

%% @doc The main loop of the bundler server.
server(State = #state{max_idle_time = MaxIdleTime}, Opts) ->
    receive
        {enqueue_item, Item} ->
            State1 = add_to_queue(Item, State, Opts),
            server(assign_tasks(maybe_dispatch(State1)), Opts);
        {recover_bundle, CommittedTX, Items} ->
            State1 = recover_bundle(CommittedTX, Items, State),
            server(assign_tasks(State1), Opts);
        {task_complete, WorkerPID, Task, Result} ->
            State1 = handle_task_complete(WorkerPID, Task, Result, State),
            server(assign_tasks(State1), Opts);
        {task_failed, WorkerPID, Task, Reason} ->
            State1 = handle_task_failed(WorkerPID, Task, Reason, State),
            server(assign_tasks(State1), Opts);
        {retry_task, Task} ->
            State1 = enqueue_task(Task, State),
            server(assign_tasks(State1), Opts);
        {get_state, From, Ref} ->
            From ! {state, Ref, State},
            server(State, Opts);
        stop ->
            maps:foreach(
                fun(WorkerPID, _) -> WorkerPID ! stop end,
                State#state.workers
            ),
            exit(normal)
    after MaxIdleTime ->
        server(assign_tasks(dispatch_queue(State)), Opts)
    end.

%% @doc Add an enqueue_item to the queue. Update the state with the new queue
%% and approximate total byte size of the queue.
%% Note: Item has already been verified and cached before reaching here.
add_to_queue(Item, State = #state{queue = Queue, bytes = Bytes}, Opts) ->
    ItemSize = erlang:external_size(Item),
    NewQueue = [Item | Queue],
    NewBytes = Bytes + ItemSize,
    ?event(bundler_short, {queueing_item, 
        {id, {explicit, hb_message:id(Item, signed, Opts)}},
        {size, erlang:external_size(Item)},
        {queue_size, length(NewQueue)},
        {queue_bytes, NewBytes}
    }),
    State#state{queue = NewQueue, bytes = NewBytes}.

%% @doc Dispatch the queue if it is ready.
%% Only dispatches up to max_items at a time to respect the limit.
maybe_dispatch(State = #state{queue = Q, max_items = MaxItems}) ->
    case dispatchable(State) of
        true ->
            {ToDispatch, Remaining} = split_queue(Q, MaxItems),
            State1 = create_bundle(ToDispatch, State),
            NewState = State1#state{
                queue = Remaining,
                bytes = queue_bytes(Remaining)
            },
            maybe_dispatch(NewState);
        false -> State
    end.

%% @doc Split a queue into items to dispatch (up to max) and remaining items.
split_queue(Queue, MaxItems) when length(Queue) =< MaxItems ->
    {Queue, []};
split_queue(Queue, MaxItems) ->
    {ToDispatch, Remaining} = lists:split(MaxItems, Queue),
    {ToDispatch, Remaining}.

%% @doc Returns whether the queue is dispatchable.
dispatchable(#state{queue = Q, max_items = MaxLen}) when length(Q) >= MaxLen ->
    true;
dispatchable(#state{bytes = Bytes, max_size = MaxSize}) when Bytes >= MaxSize ->
    true;
dispatchable(_State) ->
    false.

%% @doc Return the total size of a queue of items.
queue_bytes(Items) ->
    lists:foldl(
        fun(Item, Acc) -> Acc + erlang:external_size(Item) end,
        0,
        Items
    ).

%% @doc Dispatch all currently queued items immediately.
dispatch_queue(State = #state{queue = []}) ->
    State;
dispatch_queue(State = #state{queue = Queue}) ->
    create_bundle(Queue, State#state{queue = [], bytes = 0}).

%% @doc Create a bundle and enqueue its initial post task.
create_bundle([], State) ->
    State;
create_bundle(Items, State = #state{bundles = Bundles, opts = Opts}) ->
    BundleID = make_ref(),
    Bundle = #bundle{
        id = BundleID,
        items = Items,
        status = initializing,
        tx = undefined,
        proofs = #{},
        start_time = erlang:timestamp()
    },
    State1 = State#state{
        bundles = maps:put(BundleID, Bundle, Bundles)
    },
    ?event(
        bundler_short,
        {dispatching_bundle,
            {timestamp, dev_bundler_task:format_timestamp()},
            {bundle_id, BundleID},
            {num_items, length(Items)}
        }
    ),
    Task = #task{
        bundle_id = BundleID,
        type = post_tx,
        data = Items,
        opts = Opts
    },
    enqueue_task(Task, State1).

%% @doc Enqueue a task for worker execution.
enqueue_task(Task, State = #state{task_queue = Queue}) ->
    State#state{task_queue = queue:in(Task, Queue)}.

%% @doc Assign pending tasks to all idle workers.
assign_tasks(State = #state{workers = Workers}) ->
    IdleWorkers = maps:filter(
        fun(_, Status) -> Status =:= idle end,
        Workers
    ),
    assign_tasks(maps:keys(IdleWorkers), State).

assign_tasks([], State) ->
    State;
assign_tasks([WorkerPID | Rest], State = #state{workers = Workers, task_queue = Queue}) ->
    case queue:out(Queue) of
        {{value, Task}, Queue1} ->
            WorkerPID ! {execute_task, self(), Task},
            State1 = State#state{
                task_queue = Queue1,
                workers = maps:put(WorkerPID, {busy, Task}, Workers)
            },
            assign_tasks(Rest, State1);
        {empty, _} ->
            State
    end.

%% @doc Handle successful task completion.
handle_task_complete(WorkerPID, Task, Result, State = #state{
        workers = Workers,
        bundles = Bundles
    }) ->
    #task{bundle_id = BundleID} = Task,
    ?event(bundler_debug, dev_bundler_task:log_task(task_complete, Task, [])),
    State1 = State#state{
        workers = maps:put(WorkerPID, idle, Workers)
    },
    case maps:get(BundleID, Bundles, undefined) of
        undefined ->
            ?event(bundler_short, {bundle_not_found, BundleID}),
            State1;
        Bundle ->
            task_completed(Task, Bundle, Result, State1)
    end.

%% @doc Handle task failure and schedule a retry.
handle_task_failed(WorkerPID, Task, Reason, State = #state{
        workers = Workers,
        opts = Opts
    }) ->
    RetryCount = Task#task.retry_count,
    BaseDelay = hb_opts:get(
        retry_base_delay_ms, ?DEFAULT_RETRY_BASE_DELAY_MS, Opts),
    MaxDelay = hb_opts:get(
        retry_max_delay_ms, ?DEFAULT_RETRY_MAX_DELAY_MS, Opts),
    Jitter = hb_opts:get(retry_jitter, ?DEFAULT_RETRY_JITTER, Opts),
    BaseDelayWithBackoff = min(BaseDelay * (1 bsl RetryCount), MaxDelay),
    JitterFactor = (rand:uniform() * 2 - 1) * Jitter,
    Delay = round(BaseDelayWithBackoff * (1 + JitterFactor)),
    ?event(
        bundler_short,
        dev_bundler_task:log_task(task_failed_retrying, Task, [
            {reason, {explicit, Reason}},
            {retry_count, RetryCount},
            {delay_ms, Delay}
        ])
    ),
    Task1 = Task#task{retry_count = RetryCount + 1},
    erlang:send_after(Delay, self(), {retry_task, Task1}),
    State#state{
        workers = maps:put(WorkerPID, idle, Workers)
    }.

%% @doc Apply task completion effects to server state.
task_completed(#task{bundle_id = BundleID, type = post_tx}, Bundle, CommittedTX, State) ->
    Bundles = State#state.bundles,
    Opts = State#state.opts,
    Bundle1 = Bundle#bundle{status = tx_posted, tx = CommittedTX},
    State1 = State#state{
        bundles = maps:put(BundleID, Bundle1, Bundles)
    },
    BuildProofsTask = #task{
        bundle_id = BundleID,
        type = build_proofs,
        data = CommittedTX,
        opts = Opts
    },
    enqueue_task(BuildProofsTask, State1);
task_completed(#task{bundle_id = BundleID, type = build_proofs}, Bundle, Proofs, State) ->
    Bundles = State#state.bundles,
    Opts = State#state.opts,
    case Proofs of
        [] ->
            bundle_complete(Bundle, State);
        _ ->
            ProofsMap = maps:from_list([
                {maps:get(offset, Proof), #proof{proof = Proof, status = pending}}
                || Proof <- Proofs
            ]),
            Bundle1 = Bundle#bundle{
                proofs = ProofsMap,
                status = proofs_built
            },
            State1 = State#state{
                bundles = maps:put(BundleID, Bundle1, Bundles)
            },
            lists:foldl(
                fun(ProofData, StateAcc) ->
                    ProofTask = #task{
                        bundle_id = BundleID,
                        type = post_proof,
                        data = ProofData,
                        opts = Opts
                    },
                    enqueue_task(ProofTask, StateAcc)
                end,
                State1,
                Proofs
            )
    end;
task_completed(
        #task{bundle_id = BundleID, type = post_proof, data = ProofData},
        Bundle,
        _Result,
        State
    ) ->
    Bundles = State#state.bundles,
    Offset = maps:get(offset, ProofData),
    Proofs = Bundle#bundle.proofs,
    Proofs1 = maps:update_with(
        Offset,
        fun(Proof) -> Proof#proof{status = seeded} end,
        Proofs
    ),
    Bundle1 = Bundle#bundle{proofs = Proofs1},
    State1 = State#state{
        bundles = maps:put(BundleID, Bundle1, Bundles)
    },
    AllSeeded = lists:all(
        fun(#proof{status = Status}) -> Status =:= seeded end,
        maps:values(Proofs1)
    ),
    case AllSeeded of
        true ->
            bundle_complete(Bundle1, State1);
        false ->
            State1
    end.

%% @doc Mark a bundle as complete and remove it from state.
bundle_complete(Bundle, State = #state{opts = Opts}) ->
    ok = dev_bundler_cache:complete_tx(Bundle#bundle.tx, Opts),
    ElapsedTime =
        timer:now_diff(erlang:timestamp(), Bundle#bundle.start_time) / 1000000,
    ?event(
        bundler_short,
        {bundle_complete,
            {bundle_id, Bundle#bundle.id},
            {timestamp, dev_bundler_task:format_timestamp()},
            {tx, {explicit, hb_message:id(Bundle#bundle.tx, signed, Opts)}},
            {elapsed_time_s, ElapsedTime}
        }
    ),
    State#state{bundles = maps:remove(Bundle#bundle.id, State#state.bundles)}.

%% @doc Recover a single bundle and enqueue any follow-up work.
recover_bundle(CommittedTX, Items, State = #state{opts = Opts}) ->
    BundleID = make_ref(),
    Bundle = #bundle{
        id = BundleID,
        items = Items,
        status = tx_posted,
        tx = CommittedTX,
        proofs = #{},
        start_time = erlang:timestamp()
    },
    Bundles = State#state.bundles,
    State1 = State#state{
        bundles = maps:put(BundleID, Bundle, Bundles)
    },
    Task = #task{
        bundle_id = BundleID,
        type = build_proofs,
        data = CommittedTX,
        opts = Opts
    },
    enqueue_task(Task, State1).

%%%===================================================================
%%% Tests
%%%===================================================================

bundle_count_test() ->
    test_bundle(#{ bundler_max_items => 3 }).

bundle_size_test() ->
    test_bundle(#{ bundler_max_size => floor(3.6 * ?DATA_CHUNK_SIZE) }).

nested_bundle_test() ->
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
        NodeOpts2 = maps:merge(NodeOpts, #{ bundler_max_items => 3 }),
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
        % Test posting each of the supported signature types
        RSAWallet = ar_wallet:new({rsa, 65537}),
        EdDSAWallet = ar_wallet:new({eddsa, ed25519}),
        EthereumWallet = ar_wallet:new(ethereum),
        ItemSize = floor(1.5 * ?DATA_CHUNK_SIZE),
        Item1 = new_data_item(1, ItemSize, RSAWallet),
        Item2 = new_data_item(2, ItemSize, EdDSAWallet),
        {ok, SolanaBin} =
            file:read_file(<<"test/arbundles.js/ans104-item-solana.bin">>),
        Item3 = ar_bundles:deserialize(SolanaBin),
        Item4 = new_data_item(4, ItemSize, EthereumWallet),
        Items = [Item1, Item2, Item3, Item4],
        lists:foreach(
            fun(Item) ->
                ?event(debug_test, {posting_item, Item}),
                ?assertMatch({ok, _}, post_data_item(Node, Item, ClientOpts))
            end,
            Items
        ),
        timer:sleep(150),
        ?assertEqual(0, length(hb_mock_server:get_requests(tx, 0, ServerHandle))),
        ?assertEqual(0, length(hb_mock_server:get_requests(chunk, 0, ServerHandle))),
        timer:sleep(300),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        %% 2x 1.5 chunk items + 1 small solana item + 1.5 Ethereum = 5 chunks
        ExpectedChunks = 5,
        Proofs = hb_mock_server:get_requests(
            chunk, ExpectedChunks, ServerHandle),
        ?assertEqual(ExpectedChunks, length(Proofs)),
        assert_bundle(
            Node, Items, Anchor, Price, hd(TXs), Proofs, ClientOpts),
        ok
    after
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

complete_task_sequence_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 2,
            retry_base_delay_ms => 100,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        Items = [
            new_structured_data_item(1, 10, Opts),
            new_structured_data_item(2, 10, Opts)
        ],
        submit_test_items(Items, Opts),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        Proofs = hb_mock_server:get_requests(chunk, 1, ServerHandle),
        ?assertEqual(1, length(Proofs)),
        State = get_state(),
        ?assertNotEqual(undefined, State),
        ?assertNotEqual(timeout, State),
        Workers = State#state.workers,
        IdleWorkers = [
            PID
            || {PID, Status} <- maps:to_list(Workers), Status =:= idle
        ],
        ?assertEqual(maps:size(Workers), length(IdleWorkers)),
        Queue = State#state.task_queue,
        ?assert(queue:is_empty(Queue)),
        Bundles = State#state.bundles,
        ?assertEqual(0, maps:size(Bundles)),
        ok
    after
        stop_test_servers(ServerHandle)
    end.

recover_bundles_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        chunk => fun(_Req) ->
            timer:sleep(250),
            {200, <<"OK">>}
        end,
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store()
        },
        hb_http_server:start_node(Opts),
        Item1 = new_structured_data_item(1, 10, Opts),
        Item2 = new_structured_data_item(2, 10, Opts),
        Item3 = new_structured_data_item(3, 10, Opts),
        ok = dev_bundler_cache:write_item(Item1, Opts),
        ok = dev_bundler_cache:write_item(Item2, Opts),
        ok = dev_bundler_cache:write_item(Item3, Opts),
        {ok, TX} = dev_codec_tx:to(
            lists:reverse([Item1, Item2, Item3]), #{}, #{}),
        CommittedTX = hb_message:convert(
            TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts),
        ok = dev_bundler_cache:write_tx(CommittedTX, [Item1, Item2, Item3], Opts),
        Item4 = new_structured_data_item(4, 10, Opts),
        ok = dev_bundler_cache:write_item(Item4, Opts),
        {ok, TX2} = dev_codec_tx:to(lists:reverse([Item4]), #{}, #{}),
        CommittedTX2 = hb_message:convert(
            TX2, <<"structured@1.0">>, <<"tx@1.0">>, Opts),
        ok = dev_bundler_cache:write_tx(CommittedTX2, [Item4], Opts),
        ok = dev_bundler_cache:complete_tx(CommittedTX2, Opts),
        ensure_server(Opts),
        State = get_state(),
        ?assertNotEqual(undefined, State),
        ?assertNotEqual(timeout, State),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle, 200),
        ?assertEqual([], TXs),
        ?assert(
            hb_util:wait_until(
                fun() ->
                    dev_bundler_cache:load_bundle_states(Opts) =:= []
                end,
                2000
            )
        ),
        FinalState = get_state(),
        ?assertEqual(0, maps:size(FinalState#state.bundles)),
        ok
    after
        stop_test_servers(ServerHandle)
    end.

post_tx_price_failure_retry_test() ->
    Anchor = rand:bytes(32),
    FailCount = 3,
    setup_test_counter(price_attempts_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => fun(_Req) ->
            Count = increment_test_counter(price_attempts_counter) - 1,
            case Count < FailCount of
                true -> {500, <<"error">>};
                false -> {200, <<"12345">>}
            end
        end,
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            retry_base_delay_ms => 50,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        Items = [new_structured_data_item(1, 10, Opts)],
        submit_test_items(Items, Opts),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        FinalCount = get_test_counter(price_attempts_counter),
        ?assertEqual(FailCount + 1, FinalCount),
        ok
    after
        cleanup_test_counter(price_attempts_counter),
        stop_test_servers(ServerHandle)
    end.

post_tx_anchor_failure_retry_test() ->
    Price = 12345,
    FailCount = 3,
    setup_test_counter(anchor_attempts_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => fun(_Req) ->
            Count = increment_test_counter(anchor_attempts_counter) - 1,
            case Count < FailCount of
                true -> {500, <<"error">>};
                false -> {200, hb_util:encode(rand:bytes(32))}
            end
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            retry_base_delay_ms => 50,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        Items = [new_structured_data_item(1, 10, Opts)],
        submit_test_items(Items, Opts),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        FinalCount = get_test_counter(anchor_attempts_counter),
        ?assertEqual(FailCount + 1, FinalCount),
        ok
    after
        cleanup_test_counter(anchor_attempts_counter),
        stop_test_servers(ServerHandle)
    end.

post_tx_post_failure_retry_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    FailCount = 4,
    setup_test_counter(tx_attempts_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            Count = increment_test_counter(tx_attempts_counter) - 1,
            case Count < FailCount of
                true -> {400, <<"Transaction verification failed">>};
                false -> {200, <<"OK">>}
            end
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            retry_base_delay_ms => 50,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        Items = [new_structured_data_item(1, 10, Opts)],
        submit_test_items(Items, Opts),
        TXs = hb_mock_server:get_requests(tx, FailCount + 1, ServerHandle),
        ?assertEqual(FailCount + 1, length(TXs)),
        FinalCount = get_test_counter(tx_attempts_counter),
        ?assertEqual(FailCount + 1, FinalCount),
        ok
    after
        cleanup_test_counter(tx_attempts_counter),
        stop_test_servers(ServerHandle)
    end.

post_proof_failure_retry_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    FailCount = 2,
    setup_test_counter(chunk_attempts_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        chunk => fun(_Req) ->
            Count = increment_test_counter(chunk_attempts_counter) - 1,
            case Count < FailCount of
                true -> {500, <<"error">>};
                false -> {200, <<"OK">>}
            end
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            retry_base_delay_ms => 50,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        Items = [new_structured_data_item(1, floor(4.5 * ?DATA_CHUNK_SIZE), Opts)],
        submit_test_items(Items, Opts),
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        Chunks = hb_mock_server:get_requests(chunk, FailCount + 5, ServerHandle),
        ?assertEqual(FailCount + 5, length(Chunks)),
        FinalCount = get_test_counter(chunk_attempts_counter),
        ?assertEqual(FailCount + 5, FinalCount),
        ok
    after
        cleanup_test_counter(chunk_attempts_counter),
        stop_test_servers(ServerHandle)
    end.

rapid_dispatch_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            timer:sleep(100),
            {200, <<"OK">>}
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            bundler_workers => 3
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        lists:foreach(
            fun(I) ->
                Items = [new_structured_data_item(I, 10, Opts)],
                submit_test_items(Items, Opts)
            end,
            lists:seq(1, 10)
        ),
        TXs = hb_mock_server:get_requests(tx, 10, ServerHandle),
        ?assertEqual(10, length(TXs)),
        ok
    after
        stop_test_servers(ServerHandle)
    end.

one_bundle_fails_others_continue_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    setup_test_counter(mixed_attempts_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            Count = increment_test_counter(mixed_attempts_counter) - 1,
            case Count of
                0 -> {200, <<"OK">>};
                _ -> {400, <<"fail">>}
            end
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            retry_base_delay_ms => 100,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        Items1 = [new_structured_data_item(1, 10, Opts)],
        submit_test_items(Items1, Opts),
        Items2 = [new_structured_data_item(2, 10, Opts)],
        submit_test_items(Items2, Opts),
        TXs = hb_mock_server:get_requests(tx, 5, ServerHandle),
        ?assert(length(TXs) >= 5, length(TXs)),
        ok
    after
        cleanup_test_counter(mixed_attempts_counter),
        stop_test_servers(ServerHandle)
    end.

parallel_task_execution_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    SleepTime = 120,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        chunk => fun(_Req) ->
            timer:sleep(SleepTime),
            {200, <<"OK">>}
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            bundler_workers => 5
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        lists:foreach(
            fun(I) ->
                Items = [new_structured_data_item(I, 10, Opts)],
                submit_test_items(Items, Opts)
            end,
            lists:seq(1, 10)
        ),
        StartTime = erlang:system_time(millisecond),
        Chunks = hb_mock_server:get_requests(chunk, 10, ServerHandle),
        ElapsedTime = erlang:system_time(millisecond) - StartTime,
        ?assertEqual(10, length(Chunks)),
        ?assert(ElapsedTime < 2000, "ElapsedTime: " ++ integer_to_list(ElapsedTime)),
        ok
    after
        stop_test_servers(ServerHandle)
    end.

exponential_backoff_timing_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    FailCount = 5,
    setup_test_counter(backoff_cap_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            Timestamp = erlang:system_time(millisecond),
            Attempt = increment_test_counter(backoff_cap_counter),
            Count = Attempt - 1,
            add_test_attempt_timestamp(backoff_cap_counter, Attempt, Timestamp),
            case Count < FailCount of
                true -> {400, <<"fail">>};
                false -> {200, <<"OK">>}
            end
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            retry_base_delay_ms => 100,
            retry_max_delay_ms => 500,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        Items = [new_structured_data_item(1, 10, Opts)],
        submit_test_items(Items, Opts),
        TXs = hb_mock_server:get_requests(tx, FailCount + 1, ServerHandle, 5000),
        ?assertEqual(FailCount + 1, length(TXs)),
        Timestamps = test_attempt_timestamps(backoff_cap_counter),
        ?assertEqual(6, length(Timestamps)),
        [T1, T2, T3, T4, T5, T6] = Timestamps,
        Delay1 = T2 - T1,
        Delay2 = T3 - T2,
        Delay3 = T4 - T3,
        Delay4 = T5 - T4,
        Delay5 = T6 - T5,
        ?assert(Delay1 >= 70 andalso Delay1 =< 200, Delay1),
        ?assert(Delay2 >= 150 andalso Delay2 =< 300, Delay2),
        ?assert(Delay3 >= 300 andalso Delay3 =< 500, Delay3),
        ?assert(Delay4 >= 400 andalso Delay4 =< 700, Delay4),
        ?assert(Delay5 >= 400 andalso Delay5 =< 700, Delay5),
        ok
    after
        cleanup_test_counter(backoff_cap_counter),
        stop_test_servers(ServerHandle)
    end.

independent_task_retry_counts_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    setup_test_counter(independent_retry_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            Count = increment_test_counter(independent_retry_counter) - 1,
            case Count < 2 of
                true -> {400, <<"fail">>};
                false -> {200, <<"OK">>}
            end
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            bundler_max_items => 1,
            retry_base_delay_ms => 100,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        ensure_server(Opts),
        Items1 = [new_structured_data_item(1, 10, Opts)],
        submit_test_items(Items1, Opts),
        hb_mock_server:get_requests(tx, 3, ServerHandle),
        Items2 = [new_structured_data_item(2, 10, Opts)],
        submit_test_items(Items2, Opts),
        TotalAttempts = 4,
        TXs = hb_mock_server:get_requests(tx, TotalAttempts, ServerHandle),
        ?assertEqual(TotalAttempts, length(TXs)),
        ok
    after
        cleanup_test_counter(independent_retry_counter),
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
        stop_server()
    end.

stop_test_servers(ServerHandle) ->
    hb_mock_server:stop(ServerHandle),
    stop_server().

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
    new_data_item(Index, Size, hb:wallet()).

new_structured_data_item(Index, Size, Opts) ->
    hb_message:convert(
        new_data_item(Index, Size),
        <<"structured@1.0">>,
        <<"ans104@1.0">>,
        Opts
    ).

submit_test_items([], _Opts) ->
    ok;
submit_test_items(Items, Opts) ->
    lists:foreach(
        fun(Item) ->
            ?assertMatch({ok, _}, item(#{}, Item, Opts))
        end,
        Items
    ).

new_data_item(Index, Size, Wallet) ->
    Data = rand:bytes(Size),
    Tag = <<"tag", (integer_to_binary(Index))/binary>>,
    Value = <<"value", (integer_to_binary(Index))/binary>>,
    ar_bundles:sign_item(
        #tx{
            data = Data,
            tags = [{Tag, Value}]
        },
        Wallet
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

setup_test_counter(Table) ->
    cleanup_test_counter(Table),
    ets:new(Table, [named_table, public, set]),
    ok.

cleanup_test_counter(Table) ->
    case ets:info(Table) of
        undefined -> ok;
        _ -> ets:delete(Table), ok
    end.

increment_test_counter(Table) ->
    ets:update_counter(Table, Table, {2, 1}, {Table, 0}).

get_test_counter(Table) ->
    case ets:lookup(Table, Table) of
        [{_, Value}] -> Value;
        [] -> 0
    end.

add_test_attempt_timestamp(Table, Attempt, Timestamp) ->
    ets:insert(Table, {{Table, Attempt}, Timestamp}).

test_attempt_timestamps(Table) ->
    TimestampEntries = [
        {Attempt, Timestamp}
        || {{Prefix1, Attempt}, Timestamp} <- ets:tab2list(Table),
            Prefix1 =:= Table
    ],
    [Timestamp || {_, Timestamp} <- lists:sort(TimestampEntries)].