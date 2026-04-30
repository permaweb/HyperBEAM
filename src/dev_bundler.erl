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
-export([tx/3, item/3, ensure_server/1, stop_server/0, stop_server/1]).
-export([get_state/0, get_state/1]).
%%% Test-only exports.
-export([start_mock_gateway/1]).

-include("include/hb.hrl").
-include("include/dev_bundler.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Default options.
-define(SERVER_NAME, bundler_server).
-define(DEFAULT_MAX_SIZE, 100_000_000). % 100 MB.
-define(DEFAULT_MAX_IDLE_TIME, 300_000). % 5 minutes.
-define(DEFAULT_BUNDLER_MAX_DISPATCH_TIMEOUT, 30_000). % 30 seconds.
-define(DEFAULT_MAX_ITEMS, 1000).

%%% Public interface.

%% @doc An alias for `item/3'.
tx(Base, Req, Opts) ->
    item(Base, Req, Opts).

%% @doc Implements an `up.arweave.net'-compatible endpoint for
%% bundling messages.
item(_Base, Req, Opts) ->
    ServerPID = ensure_server(Opts),
    ItemToProcess =
        case hb_maps:find(<<"bundler-subject">>, Req, Opts) of
            {ok, SubjectKey} -> hb_maps:get(SubjectKey, Req, Req, Opts);
            error -> Req
        end,
    case verify_message(ItemToProcess, Opts) of
        {ok, Item} ->
            ItemID = hb_message:id(Item, signed, Opts),
            case cache_item(Item, Opts) of
                ok ->
                    BundledSize = bundled_item_size(Item, Opts),
                    dev_metering:consume(
                        <<"arweave-bytes">>,
                        BundledSize,
                        Opts
                    ),
                    % Queue the item for bundling
                    % (fire-and-forget, ignore errors)
                    ServerPID ! {enqueue_item, Item, BundledSize},
                    {ok, #{
                        <<"id">> => ItemID,
                        <<"timestamp">> => erlang:system_time(millisecond)
                    }};
                {error, Reason} ->
                    ?event(
                        bundler_short,
                        {cache_write_failed,
                            {id, {explicit, ItemID}},
                            {reason, Reason}
                        }
                    ),
                    {error, #{
                        <<"status">> => 500,
                        <<"error">> => <<"cache-write-failed">>,
                        <<"details">> => error_to_bin(Reason)
                    }}
            end;
        {error, Reason} ->
            {error, #{
                <<"status">> => 400,
                <<"error">> => <<"invalid-item">>,
                <<"details">> => error_to_bin(Reason)
            }}
    end.

%% @doc Verify the subject by extracting committed fields and checking signatures.
%% Returns {ok, Item} or {error, Reason}.
verify_message(Req, Opts) ->
    case hb_message:with_only_committed(Req, Opts) of
        {ok, Item} ->
            case hb_message:signers(Item, Opts) of
                [] ->
                    ?event(
                        bundler_short,
                        {verify_failed, {reason, unsigned_item}}
                    ),
                    {error, unsigned_item};
                _ ->
                    case hb_message:verify(Item, all, Opts) of
                        true -> {ok, Item};
                        false ->
                            ?event(
                                bundler_short,
                                {verify_failed,
                                    {id,
                                        {string, hb_message:id(Item, signed, Opts)}
                                    },
                                    {reason, signature_verification_failed}
                                },
                                Opts
                            ),
                            {error, signature_verification_failed}
                    end
            end;
        {error, Reason} ->
            ?event(bundler_short, {verify_failed, {reason, Reason}}),
            {error, Reason}
    end.

%% @doc Format an error signifier for external responses.
error_to_bin({error, Reason}) -> error_to_bin(Reason);
error_to_bin(Reason) ->
    binary:replace(hb_util:bin(Reason), <<"_">>, <<"-">>, [global]).

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

%% @doc Look up the registration name for the bundler server. Derived from
%% the HTTP server's identity (which `hb_http_server:new_server/1' itself
%% computes from `priv-wallet') so there is exactly one bundler per HTTP
%% server on a BEAM node. Falls back to the legacy global `?SERVER_NAME'
%% atom when no wallet is available (kept for production callers that
%% stopped the server without a configured wallet).
server_name(Opts) ->
    case hb_opts:get(priv_wallet, undefined, Opts) of
        undefined -> ?SERVER_NAME;
        Wallet ->
            {bundler_server,
                hb_util:human_id(ar_wallet:to_address(Wallet))}
    end.

%% @doc Return the PID of the bundler server. If the server is not running,
%% it is started and registered with the name returned by `server_name/1'.
ensure_server(Opts) ->
    Name = server_name(Opts),
    hb_name:singleton(
        Name,
        fun() -> init(Opts) end
    ).

%% @doc Stop the default bundler server. Used by production-oriented
%% tests that rely on the global `?SERVER_NAME'.
stop_server() ->
    stop_server(#{}).
stop_server(Opts) ->
    Name = server_name(Opts),
    case hb_name:lookup(Name) of
        undefined -> ok;
        PID ->
            PID ! stop,
            hb_name:unregister(Name)
    end.

%% @doc Return the current bundler server state for tests.
get_state() ->
    get_state(#{}).
get_state(Opts) ->
    Name = server_name(Opts),
    case hb_name:lookup(Name) of
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
            State1 =
                add_to_queue(
                    Item,
                    bundled_item_size(Item, Opts),
                    State,
                    Opts
                ),
            server(assign_tasks(maybe_dispatch(State1)), Opts);
        {enqueue_item, Item, BundledSize} ->
            State1 = add_to_queue(Item, BundledSize, State, Opts),
            server(assign_tasks(maybe_dispatch(State1)), Opts);
        {dispatch_queue, Timestamp} ->
            ?event(bundler_short, {dispatched_queue_start, calendar:now_to_universal_time(Timestamp)}),
            server(assign_tasks(dispatch_queue(State)), Opts);
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

%% @doc Add an item to the queue. Update the state with the queue's total
%% bundled byte size.
%% Note: Item has already been verified and cached before reaching here.
add_to_queue(Item, BundledSize, State = #state{
        queue = Queue,
        bytes = Bytes,
        dispatch_ref = DispatchRef
    }, Opts) ->
    NewQueue = [{Item, BundledSize} | Queue],
    NewBytes = Bytes + BundledSize,
    ?event(bundler_short, {queueing_item, 
        {id, {explicit, hb_message:id(Item, signed, Opts)}},
        {size, BundledSize},
        {queue_size, length(NewQueue)},
        {queue_bytes, NewBytes}
    }),
    UpdatedDispatchRef = if Queue =:= [] ->
        MaxBundleDispatchTimeout =
            hb_opts:get(
                bundler_max_bundle_dispatch_delay,
                ?DEFAULT_BUNDLER_MAX_DISPATCH_TIMEOUT,
                Opts
            ),
        ?event(bundler_short, {scheduling_max_bundle_dispatch_timeout, {dispatch_timeout, MaxBundleDispatchTimeout}}, Opts),
        erlang:send_after(
            MaxBundleDispatchTimeout,
            self(),
            {dispatch_queue, erlang:timestamp()}
        );
    true -> DispatchRef
    end,
    State#state{queue = NewQueue, bytes = NewBytes, dispatch_ref = UpdatedDispatchRef}.

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
        fun({_Item, BundledSize}, Acc) -> Acc + BundledSize end,
        0,
        Items
    ).

%% @doc Dispatch all currently queued items immediately.
dispatch_queue(State = #state{queue = []}) ->
    State;
dispatch_queue(State = #state{queue = Queue, dispatch_ref = DispatchRef}) ->
    case is_reference(DispatchRef) of 
        true -> erlang:cancel_timer(DispatchRef);
        false -> no_op
    end,
    create_bundle(Queue, State#state{queue = [], bytes = 0, dispatch_ref = undefined}).

%% @doc Create a bundle and enqueue its initial post task.
create_bundle([], State) ->
    State;
create_bundle(QueuedItems, State = #state{bundles = Bundles, opts = Opts}) ->
    {Items, ItemSizes} = lists:unzip(QueuedItems),
    BundleID = make_ref(),
    Bundle = #bundle{
        id = BundleID,
        items = Items,
        item_sizes = ItemSizes,
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
    ?event(debug_bundler, dev_bundler_task:log_task(task_complete, Task, [])),
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
    run_completion_hooks(Bundle, Opts),
    State#state{bundles = maps:remove(Bundle#bundle.id, State#state.bundles)}.

%% @doc Execute hooks for each completed bundled item and the full bundle.
run_completion_hooks(Bundle, Opts) ->
    lists:foreach(
        fun({Item, Size}) ->
            dev_hook:on(
                <<"bundled-message-complete">>,
                #{
                    <<"body">> => Item,
                    <<"bundled-size">> => Size
                },
                Opts
            )
        end,
        lists:zip(
            lists:reverse(Bundle#bundle.items),
            lists:reverse(Bundle#bundle.item_sizes)
        )
    ),
    dev_hook:on(
        <<"bundle-complete">>,
        #{
            <<"body">> => Bundle#bundle.tx,
            <<"bundled-size">> => bundle_size(Bundle#bundle.tx, Opts)
        },
        Opts
    ).

%% @doc Calculate the exact byte size of an item inside its bundle.
bundled_item_size(Item, Opts) ->
    TX =
        hb_message:convert(
            Item,
            #{ <<"device">> => <<"ans104@1.0">>, <<"bundle">> => true },
            <<"structured@1.0">>,
            Opts
        ),
    byte_size(ar_bundles:serialize(TX)).

%% @doc Calculate the byte size of the completed bundle payload.
bundle_size(CommittedTX, Opts) ->
    TX =
        hb_message:convert(
            CommittedTX,
            <<"tx@1.0">>,
            <<"structured@1.0">>,
            Opts
        ),
    case TX#tx.data_size of
        Size when is_integer(Size) -> Size;
        _ -> byte_size(TX#tx.data)
    end.

%% @doc Recover a single bundle and enqueue any follow-up work.
recover_bundle(CommittedTX, Items, State = #state{opts = Opts}) ->
    BundleID = make_ref(),
    Bundle = #bundle{
        id = BundleID,
        items = Items,
        item_sizes = [bundled_item_size(Item, Opts) || Item <- Items],
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

%%% Four test cases below (`idle_test', `bundle_dispatch_delay_test',
%%% `dispatch_blocking_test', `exponential_backoff_timing_test') assert
%%% wall-clock timing against the bundler's internal timers. They use
%%% EUnit's plain `_test/0' convention rather than `_test_parallel/0'
%%% so they run sequentially, before the parse_transform-injected
%%% `all_parallel_test_/0' inparallel batch runs the other 20 cases.
%%% Keeping them out of the batch avoids a ~20% flake seen when
%%% `timer:sleep/1' returns late under same-module scheduler pressure.

bundle_count_test_parallel() ->
    test_bundle(#{ <<"bundler-max-items">> => 3 }).

bundle_size_test_parallel() ->
    test_bundle(#{ <<"bundler-max-size">> => floor(3.6 * ?DATA_CHUNK_SIZE) }).

bundle_dispatch_delay_test() ->
    test_bundle(#{ <<"bundler-max-bundle-dispatch-delay">> => 3000 }).

nested_bundle_test_parallel() ->
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
        NodeOpts2 = maps:merge(NodeOpts, #{ <<"bundler-max-items">> => 3 }),
        Node = hb_http_server:start_node(NodeOpts2#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store()
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

price_error_test_parallel() ->
    test_api_error(#{
        price => {500, <<"error">>},
        tx_anchor => {200, hb_util:encode(rand:bytes(32))}
    }).

anchor_error_test_parallel() ->
    test_api_error(#{
        price => {200, <<"12345">>},
        tx_anchor => {500, <<"error">>}
    }).

tx_error_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

unsigned_dataitem_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"debug-print">> => false
        }),
        Item = #tx{
                data = <<"testdata">>,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
        Response = post_data_item(Node, Item, ClientOpts),
        ?assertMatch(
            {error, #{
                <<"status">> := 400,
                <<"error">> := <<"invalid-item">>,
                <<"details">> := <<"unsigned-item">>
            }},
            Response)
    after
        %% Always cleanup, even if test fails
        stop_test_servers(ServerHandle, NodeOpts)
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
            <<"bundler-max-idle-time">> => 400,
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store()
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
        stop_test_servers(ServerHandle, NodeOpts)
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 3
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

%% @doc Test that items are recovered and posted while respecting the
%% max_items limit.
recover_respects_max_items_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => MaxItems
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

complete_task_sequence_test_parallel() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        Opts = NodeOpts#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 2,
            <<"retry-base-delay-ms">> => 100,
            <<"retry-jitter">> => 0
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
        State = get_state(Opts),
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

recover_bundles_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store()
        },
        hb_http_server:start_node(Opts),
        Item1 = new_structured_data_item(1, 10, Opts),
        Item2 = new_structured_data_item(2, 10, Opts),
        Item3 = new_structured_data_item(3, 10, Opts),
        ok = dev_bundler_cache:write_item(Item1, Opts),
        ok = dev_bundler_cache:write_item(Item2, Opts),
        ok = dev_bundler_cache:write_item(Item3, Opts),
        TX = dev_bundler_task:data_items_to_tx(
            lists:reverse([Item1, Item2, Item3]), Opts),
        CommittedTX = hb_message:convert(
            TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts),
        ok = dev_bundler_cache:write_tx(CommittedTX, [Item1, Item2, Item3], Opts),
        Item4 = new_structured_data_item(4, 10, Opts),
        ok = dev_bundler_cache:write_item(Item4, Opts),
        TX2 = dev_bundler_task:data_items_to_tx(
            lists:reverse([Item4]), Opts),
        CommittedTX2 = hb_message:convert(
            TX2, <<"structured@1.0">>, <<"tx@1.0">>, Opts),
        ok = dev_bundler_cache:write_tx(CommittedTX2, [Item4], Opts),
        ok = dev_bundler_cache:complete_tx(CommittedTX2, Opts),
        ensure_server(Opts),
        State = get_state(Opts),
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
        FinalState = get_state(Opts),
        ?assertEqual(0, maps:size(FinalState#state.bundles)),
        ok
    after
        stop_test_servers(ServerHandle, NodeOpts)
    end.

post_tx_price_failure_retry_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"retry-base-delay-ms">> => 50,
            <<"retry-jitter">> => 0
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

post_tx_anchor_failure_retry_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"retry-base-delay-ms">> => 50,
            <<"retry-jitter">> => 0
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

post_tx_post_failure_retry_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"retry-base-delay-ms">> => 50,
            <<"retry-jitter">> => 0
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

post_proof_failure_retry_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"retry-base-delay-ms">> => 50,
            <<"retry-jitter">> => 0
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

rapid_dispatch_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"bundler-workers">> => 3
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

one_bundle_fails_others_continue_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"retry-base-delay-ms">> => 100,
            <<"retry-jitter">> => 0
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

parallel_task_execution_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"bundler-workers">> => 5
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
        stop_test_servers(ServerHandle, NodeOpts)
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"retry-base-delay-ms">> => 100,
            <<"retry-max-delay-ms">> => 500,
            <<"retry-jitter">> => 0
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

independent_task_retry_counts_test_parallel() ->
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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1,
            <<"retry-base-delay-ms">> => 100,
            <<"retry-jitter">> => 0
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

invalid_item_test_parallel() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        ClientOpts = #{},
        TestOpts = NodeOpts#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store()
        },
        Node = hb_http_server:start_node(TestOpts#{
            <<"debug-print">> => false
        }),
        % Create a valid signed item
        Item = ar_bundles:sign_item(
            #tx{
                data = <<"testdata">>,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
            ar_wallet:new()
        ),
        % Tamper with the data after signing (this invalidates the signature)
        TamperedItem = Item#tx{data = <<"tampereddata">>},
        StructuredItem = hb_message:convert(
            TamperedItem, <<"structured@1.0">>, <<"ans104@1.0">>, TestOpts),
        PostResult = post_data_item(Node, TamperedItem, ClientOpts),
        ?assertMatch({error, #{
            <<"status">> := 400,
            <<"error">> := <<"invalid-item">>,
            <<"details">> := <<"signature-verification-failed">>}}, PostResult),
        DirectResult = dev_bundler:item(#{}, StructuredItem, TestOpts),
        ?assertMatch({error, #{
            <<"status">> := 400,
            <<"error">> := <<"invalid-item">>,
            <<"details">> := <<"signature-verification-failed">>}}, DirectResult),
        ok
    after
        stop_test_servers(ServerHandle, NodeOpts)
    end.

cache_write_failure_test_parallel() ->
    GoodOpts = #{<<"store">> => hb_test_utils:test_store()},
    BadOpts = #{
        <<"store">> => undefined,
        <<"debug-print">> => false
    }, % Invalid store will cause cache write to fail
    try
        % Start bundler with a valid store so recovery/init paths succeed.
        ensure_server(GoodOpts),
        Item = ar_bundles:sign_item(
            #tx{
                data = <<"testdata">>,
                tags = [{<<"tag1">>, <<"value1">>}]
            },
            ar_wallet:new()
        ),
        StructuredItem = hb_message:convert(
            Item, <<"structured@1.0">>, <<"ans104@1.0">>, GoodOpts),
        % Call item/3 directly without a store, should cause cache write
        % to fail.
        Result = dev_bundler:item(#{}, StructuredItem, BadOpts),
        ?assertMatch({error, #{
            <<"status">> := 500,
            <<"error">> := <<"cache-write-failed">>}}, Result),
        ok
    after
        stop_server()
    end.

stop_test_servers(ServerHandle) ->
    stop_test_servers(ServerHandle, #{}).
stop_test_servers(ServerHandle, Opts) ->
    hb_mock_server:stop(ServerHandle),
    stop_server(Opts).

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
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store()
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

test_api_error(Responses) ->
    {ServerHandle, NodeOpts} = start_mock_gateway(Responses),
    try
        ClientOpts = #{},
        Node = hb_http_server:start_node(NodeOpts#{
            <<"priv-wallet">> => ar_wallet:new(),
            <<"store">> => hb_test_utils:test_store(),
            <<"bundler-max-items">> => 1
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
        stop_test_servers(ServerHandle, NodeOpts)
    end.

new_data_item(Index, Size) ->
    new_data_item(Index, Size, ar_wallet:new()).

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
    StructuredItem = hb_message:convert(
        Item,
        <<"structured@1.0">>,
        <<"ans104@1.0">>,
        Opts
    ),
    hb_http:post(
        Node,
        #{
            <<"path">> => <<"/~bundler@1.0/tx">>,
            <<"bundler-subject">> => <<"body">>,
            <<"body">> => StructuredItem
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
        <<"gateway">> => MockServer,
        <<"routes">> => [
            #{
                <<"template">> => <<"/arweave">>,
                <<"node">> => #{
                    <<"match">> => <<"^/arweave">>,
                    <<"with">> => MockServer,
                    <<"opts">> => #{<<"http-client">> => httpc, <<"protocol">> => http2}
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
