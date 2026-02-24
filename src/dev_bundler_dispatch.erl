%%% @doc A dispatcher for the bundler device (dev_bundler). This module
%%% manages a worker pool to handle bundle building, TX posting, proof
%%% generation, and chunk seeding. Failed tasks are automatically re-queued
%%% for immediate retry until successful.
-module(dev_bundler_dispatch).
-export([dispatch/2, ensure_dispatcher/1, stop_dispatcher/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% State record for the dispatcher process.
-record(state, {
    workers,             % Map of WorkerPID => idle | {busy, Task}
    task_queue,          % Queue of pending tasks
    bundles,             % Map of BundleID => #bundle{}
    opts                 % Configuration options
}).

%%% Task record representing work to be done by a worker.
-record(task, {
    bundle_id,           % ID of the bundle this task belongs to
    type,                % Task type: post_tx | build_proofs | post_proof
    data,                % Task-specific data (map)
    opts,                % Configuration options
    retry_count = 0      % Number of times this task has been retried
}).

%%% Proof record to track individual proof seeding status.
-record(proof, {
    proof,               % The proof data (chunk, merkle path, etc)
    status               % pending | seeded
}).

%%% Bundle record to track bundle progress through the dispatch pipeline.
-record(bundle, {
    id,                  % Unique bundle identifier
    items,               % List of dataitems to bundle
    status,              % Current state (initializing, tx_built, tx_posted, proofs_built)
    tx,                  % The built/signed transaction
    proofs,              % Map of offset => #proof{} records
    start_time           % The time the bundle was started
}).

%%% Default options.
-define(DISPATCHER_NAME, bundler_dispatcher).
-define(DEFAULT_NUM_WORKERS, 5).
-define(DEFAULT_RETRY_BASE_DELAY_MS, 1000).
-define(DEFAULT_RETRY_MAX_DELAY_MS, 600000). % 10 minutes
-define(DEFAULT_RETRY_JITTER, 0.25). % ±25% jitter

%% @doc Dispatch the queue.
dispatch([], _Opts) ->
    ok;
dispatch(Items, Opts) ->
    PID = ensure_dispatcher(Opts),
    PID ! {dispatch, Items}.

%% @doc Return the PID of the dispatch server. If the server is not running,
%% it is started and registered with the name `?SERVER_NAME'.
ensure_dispatcher(Opts) ->
    case hb_name:lookup(?DISPATCHER_NAME) of
        undefined ->
            PID = spawn(fun() -> init(Opts) end),
            ?event(bundler_short, {starting_dispatcher, {pid, PID}}),
            hb_name:register(?DISPATCHER_NAME, PID),
            hb_name:lookup(?DISPATCHER_NAME);
        PID -> PID
    end.

stop_dispatcher() ->
    case hb_name:lookup(?DISPATCHER_NAME) of
        undefined -> ok;
        PID ->
            PID ! stop,
            hb_name:unregister(?DISPATCHER_NAME)
    end.

get_state() ->
    case hb_name:lookup(?DISPATCHER_NAME) of
        undefined -> undefined;
        PID ->
            PID ! {get_state, self(), Ref = make_ref()},
            receive
                {state, Ref, State} -> State
            after 1000 -> timeout
            end
    end.

%% @doc Initialize the dispatcher with worker pool.
init(Opts) ->
    NumWorkers = hb_opts:get(bundler_workers, ?DEFAULT_NUM_WORKERS, Opts),
    Workers = lists:map(
        fun(_) ->
            WorkerPID = spawn_link(fun() -> worker_loop() end),
            {WorkerPID, idle}
        end,
        lists:seq(1, NumWorkers)
    ),
    State = #state{
        workers = maps:from_list(Workers),
        task_queue = queue:new(),
        bundles = #{},
        opts = Opts
    },
    % Recover any in-progress bundles from cache
    State1 = recover_bundles(State),
    dispatcher(assign_tasks(State1)).

%% @doc The main loop of the dispatcher. Manages task queue and worker pool.
dispatcher(State) ->
    receive
        {dispatch, Items} ->
            % Create a new bundle and queue the post_tx task
            Opts = State#state.opts,
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
                bundles = maps:put(BundleID, Bundle, State#state.bundles)
            },
            ?event(bundler_short, {dispatching_bundle, {timestamp, format_timestamp()},
                {bundle_id, BundleID}, {num_items, length(Items)}}),
            Task = #task{bundle_id = BundleID, type = post_tx, data = Items, opts = Opts},
            State2 = enqueue_task(Task, State1),
            % Assign tasks to idle workers
            dispatcher(assign_tasks(State2));
        {task_complete, WorkerPID, Task, Result} ->
            State1 = handle_task_complete(WorkerPID, Task, Result, State),
            dispatcher(assign_tasks(State1));
        {task_failed, WorkerPID, Task, Reason} ->
            State1 = handle_task_failed(WorkerPID, Task, Reason, State),
            dispatcher(assign_tasks(State1));
        {retry_task, Task} ->
            % Re-enqueue the task after backoff delay
            State1 = enqueue_task(Task, State),
            dispatcher(assign_tasks(State1));
        {get_state, From, Ref} ->
            From ! {state, Ref, State},
            dispatcher(State);
        stop ->
            % Stop all workers
            maps:foreach(
                fun(WorkerPID, _) -> WorkerPID ! stop end,
                State#state.workers
            ),
            exit(normal)
    end.

%% @doc Enqueue a task to the task queue.
enqueue_task(Task, State) ->
    Queue = State#state.task_queue,
    State#state{task_queue = queue:in(Task, Queue)}.

%% @doc Format a task for logging.
format_task(#task{bundle_id = BundleID, type = post_tx, data = DataItems}) ->
    {post_tx, {timestamp, format_timestamp()}, {bundle, BundleID},
        {num_items, length(DataItems)}};
format_task(#task{bundle_id = BundleID, type = build_proofs, data = CommittedTX}) ->
    {build_proofs, {timestamp, format_timestamp()}, {bundle, BundleID},
        {tx, {explicit, hb_message:id(CommittedTX, signed, #{})}}};
format_task(#task{bundle_id = BundleID, type = post_proof, data = Proof}) ->
    Offset = maps:get(offset, Proof),
    {post_proof, {timestamp, format_timestamp()}, {bundle, BundleID},
        {offset, Offset}}.

%% @doc Format erlang:timestamp() as a user-friendly RFC3339 string with milliseconds.
format_timestamp() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    Millisecs = (MegaSecs * 1000000 + Secs) * 1000 + (MicroSecs div 1000),
    calendar:system_time_to_rfc3339(Millisecs, [{unit, millisecond}, {offset, "Z"}]).

%% @doc Assign tasks to all idle workers until no idle workers
%% or no tasks remain.
assign_tasks(State) ->
    IdleWorkers = maps:filter(
        fun(_, Status) -> Status =:= idle end,
        State#state.workers),
    assign_tasks(maps:keys(IdleWorkers), State).

assign_tasks([], State) ->
    % No more idle workers
    State;
assign_tasks([WorkerPID | Rest], State) ->
    Workers = State#state.workers,
    Queue = State#state.task_queue,
    case queue:out(Queue) of
        {{value, Task}, Queue1} ->
            % Assign task to this worker
            WorkerPID ! {execute_task, self(), Task},
            State1 = State#state{
                task_queue = Queue1,
                workers = maps:put(WorkerPID, {busy, Task}, Workers)
            },
            % Continue with remaining idle workers
            assign_tasks(Rest, State1);
        {empty, _} ->
            % No more tasks, stop
            State
    end.

handle_task_complete(WorkerPID, Task, Result, State) ->
    Workers = State#state.workers,
    Bundles = State#state.bundles,
    #task{bundle_id = BundleID} = Task,
    ?event(bundler_debug, {task_complete, format_task(Task)}),
    % Update worker to idle
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

handle_task_failed(WorkerPID, Task, Reason, State) ->
    Workers = State#state.workers,
    Opts = State#state.opts,
    RetryCount = Task#task.retry_count,
    % Calculate exponential backoff delay
    BaseDelay = hb_opts:get(retry_base_delay_ms, ?DEFAULT_RETRY_BASE_DELAY_MS, Opts),
    MaxDelay = hb_opts:get(retry_max_delay_ms, ?DEFAULT_RETRY_MAX_DELAY_MS, Opts),
    Jitter = hb_opts:get(retry_jitter, ?DEFAULT_RETRY_JITTER, Opts),
    % Compute base delay with exponential backoff: min(base * 2^retry_count, max_delay)
    BaseDelayWithBackoff = min(BaseDelay * (1 bsl RetryCount), MaxDelay),
    % Apply jitter: delay * (1 + random(-jitter, +jitter))
    % This distributes the delay across [delay * (1-jitter), delay * (1+jitter)]
    JitterFactor = (rand:uniform() * 2 - 1) * Jitter,  % Random value in [-jitter, +jitter]
    Delay = round(BaseDelayWithBackoff * (1 + JitterFactor)),
    ?event(bundler_short, {task_failed_retrying, format_task(Task),
            {reason, {explicit, Reason}}, 
            {retry_count, RetryCount}, {delay_ms, Delay}}),
    % Update worker to idle
    State1 = State#state{
        workers = maps:put(WorkerPID, idle, Workers)
    },
    % Increment retry count and schedule delayed retry
    Task1 = Task#task{retry_count = RetryCount + 1},
    erlang:send_after(Delay, self(), {retry_task, Task1}),
    State1.

task_completed(#task{bundle_id = BundleID, type = post_tx}, Bundle, CommittedTX, State) ->
    Bundles = State#state.bundles,
    Opts = State#state.opts,
    dev_bundler_cache:write_tx(CommittedTX, Bundle#bundle.items, Opts),
    Bundle1 = Bundle#bundle{status = tx_posted, tx = CommittedTX},
    State1 = State#state{
        bundles = maps:put(BundleID, Bundle1, Bundles)
    },
    BuildProofsTask = #task{
        bundle_id = BundleID, type = build_proofs,
        data = CommittedTX, opts = Opts},
    enqueue_task(BuildProofsTask, State1);

task_completed(#task{bundle_id = BundleID, type = build_proofs}, Bundle, Proofs, State) ->
    Bundles = State#state.bundles,
    Opts = State#state.opts,
    case Proofs of
        [] ->
            % No proofs, bundle complete
            bundle_complete(Bundle, State);
        _ ->
            % Proofs built, wrap each in a proof record with offset as key
            ProofsMap = maps:from_list([
                {maps:get(offset, P), #proof{proof = P, status = pending}} || P <- Proofs
            ]),
            Bundle1 = Bundle#bundle{
                proofs = ProofsMap,
                status = proofs_built
            },
            State1 = State#state{
                bundles = maps:put(BundleID, Bundle1, Bundles)
            },
            % Enqueue all post_proof tasks
            lists:foldl(
                fun(ProofData, S) ->
                    ProofTask = #task{
                        bundle_id = BundleID,
                        type = post_proof,
                        data = ProofData,
                        opts = Opts
                    },
                    enqueue_task(ProofTask, S)
                end,
                State1,
                Proofs
            )
    end;

task_completed(#task{bundle_id = BundleID, type = post_proof, data = ProofData}, Bundle, _Result, State) ->
    Bundles = State#state.bundles,
    Offset = maps:get(offset, ProofData),
    Proofs = Bundle#bundle.proofs,
    Proofs1 = maps:update_with(
        Offset,
        fun(P) -> P#proof{status = seeded} end,
        Proofs
    ),
    Bundle1 = Bundle#bundle{proofs = Proofs1},
    State1 = State#state{
        bundles = maps:put(BundleID, Bundle1, Bundles)
    },
    % Check if all proofs are seeded
    AllSeeded = lists:all(
        fun(#proof{status = Status}) -> Status =:= seeded end,
        maps:values(Proofs1)
    ),
    case AllSeeded of
        true ->
            bundle_complete(Bundle, State1);
        false ->
            State1
    end.

%% @doc Mark a bundle as complete and remove it from state.
bundle_complete(Bundle, State) ->
    Opts = State#state.opts,
    ok = dev_bundler_cache:complete_tx(Bundle#bundle.tx, Opts),
    ElapsedTime = 
        timer:now_diff(erlang:timestamp(), Bundle#bundle.start_time) / 1000000,
    ?event(bundler_short, {bundle_complete, {bundle_id, Bundle#bundle.id},
        {timestamp, format_timestamp()},
        {tx, {explicit, hb_message:id(Bundle#bundle.tx, signed, Opts)}},
        {elapsed_time_s, ElapsedTime}}),
    State#state{bundles = maps:remove(Bundle#bundle.id, State#state.bundles)}.

%%% Recovery

%% @doc Recover in-progress bundles from cache after a crash.
recover_bundles(State) ->
    Opts = State#state.opts,
    % Reconstruct bundles and enqueue appropriate tasks
    lists:foldl(
        fun({TXID, Status}, StateAcc) ->
            recover_bundle(TXID, Status, StateAcc)
        end,
        State,
        dev_bundler_cache:load_bundle_states(Opts)
    ).

%% @doc Recover a single bundle based on its cached state.
recover_bundle(TXID, Status, State) ->
    Opts = State#state.opts,
    ?event(bundler_short, {recovering_bundle,
        {tx_id, {explicit, TXID}},
        {status, Status}
    }),
    try
        % Load the TX and its items
        CommittedTX = dev_bundler_cache:load_tx(TXID, Opts),
        Items = dev_bundler_cache:load_bundled_items(TXID, Opts),
        % Create a new bundle record
        BundleID = make_ref(),
        Bundle = #bundle{
            id = BundleID,
            items = Items,
            status = tx_posted,
            tx = CommittedTX,
            proofs = #{},
            start_time = erlang:timestamp()
        },
        % Add bundle to state
        Bundles = State#state.bundles,
        State1 = State#state{
            bundles = maps:put(BundleID, Bundle, Bundles)
        },
        
        % Enqueue appropriate task based on status
        Task = #task{
            bundle_id = BundleID, type = build_proofs,
            data = CommittedTX, opts = Opts},
        enqueue_task(Task, State1)
    catch
        _:Error:Stack ->
            ?event(bundler_short, {failed_to_recover_bundle,
                {tx_id, {explicit, TXID}},
                {error, Error},
                {stack, Stack}
            }),
            % Skip this bundle and continue
            State
    end.

%%% Worker implementation

%% @doc Worker loop - executes tasks and reports back to dispatcher.
worker_loop() ->
    receive
        {execute_task, DispatcherPID, Task} ->
            Result = execute_task(Task),
            case Result of
                {ok, Value} ->
                    DispatcherPID ! {task_complete, self(), Task, Value};
                {error, Reason} ->
                    DispatcherPID ! {task_failed, self(), Task, Reason}
            end,
            worker_loop();
        stop ->
            exit(normal)
    end.

%% @doc Execute a specific task.
execute_task(#task{type = post_tx, data = Items, opts = Opts} = Task) ->
    try
        ?event(bundler_debug, {execute_task, format_task(Task)}),
        % Get price and anchor
        {ok, TX} = dev_codec_tx:to(lists:reverse(Items), #{}, #{}),
        DataSize = TX#tx.data_size,
        PriceResult = get_price(DataSize, Opts),
        AnchorResult = get_anchor(Opts),
        case {PriceResult, AnchorResult} of
            {{ok, Price}, {ok, Anchor}} ->
                % Sign the TX
                Wallet = hb_opts:get(priv_wallet, no_viable_wallet, Opts),
                SignedTX = ar_tx:sign(TX#tx{ anchor = Anchor, reward = Price }, Wallet),
                % Convert and post
                Committed = hb_message:convert(
                    SignedTX,
                    #{ <<"device">> => <<"structured@1.0">>, <<"bundle">> => true },
                    #{ <<"device">> => <<"tx@1.0">>, <<"bundle">> => true },
                    Opts),
                ?event(bundler_short, {posting_tx,
                    {tx, {explicit, hb_message:id(Committed, signed, Opts)}}}),
                PostTXResponse = hb_ao:resolve(
                    #{ <<"device">> => <<"arweave@2.9">> },
                    Committed#{
                        <<"path">> => <<"/tx">>,
                        <<"method">> => <<"POST">>
                    },
                    Opts
                ),
                case PostTXResponse of
                    {ok, _Result} -> {ok, Committed};
                    {_, ErrorReason} -> {error, ErrorReason}
                end;
            {PriceErr, AnchorErr} ->
                ?event(bundle_short, {post_tx_failed,
                    format_task(Task),
                    {price, PriceErr},
                    {anchor, AnchorErr}}),
                {error, {PriceErr, AnchorErr}}
        end
    catch
        _:Err:_Stack -> 
            ?event(bundle_short, {post_tx_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
    end;

execute_task(#task{type = build_proofs, data = CommittedTX, opts = Opts} = Task) ->
    try
        ?event(bundler_debug, {execute_task, format_task(Task)}),
        % Calculate chunks and proofs
        TX = hb_message:convert(
            CommittedTX, <<"tx@1.0">>, <<"structured@1.0">>, Opts),
        Data = TX#tx.data,
        DataRoot = TX#tx.data_root,
        DataSize = TX#tx.data_size,
        Mode = ar_tx:chunking_mode(TX#tx.format),
        Chunks = ar_tx:chunk_binary(Mode, ?DATA_CHUNK_SIZE, Data),
        ?event(bundler_short, {building_proofs,
            {bundle, Task#task.bundle_id},
            {data_size, DataSize},
            {num_chunks, length(Chunks)}}),
        SizeTaggedChunks = ar_tx:chunks_to_size_tagged_chunks(Chunks),
        SizeTaggedChunkIDs = ar_tx:sized_chunks_to_sized_chunk_ids(SizeTaggedChunks),
        {_Root, DataTree} = ar_merkle:generate_tree(SizeTaggedChunkIDs),
        % Build proof list
        Proofs = lists:filtermap(
            fun({Chunk, Offset}) ->
                case Chunk of
                    <<>> -> false;
                    _ ->
                        DataPath = ar_merkle:generate_path(
                            DataRoot, Offset - 1, DataTree),
                        Proof = #{
                            chunk => Chunk,
                            data_path => DataPath,
                            offset => Offset - 1,
                            data_size => DataSize,
                            data_root => DataRoot
                        },
                        {true, Proof}
                end
            end,
            SizeTaggedChunks
        ),
        {ok, Proofs}
    catch
        _:Err:_Stack ->
            ?event(bundler_short, {build_proofs_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
    end;

execute_task(#task{type = post_proof, data = Proof, opts = Opts} = Task) ->
    #{chunk := Chunk, data_path := DataPath, offset := Offset,
      data_size := DataSize, data_root := DataRoot} = Proof,
    ?event(bundler_debug, {execute_task, format_task(Task)}),
    Request = #{
        <<"chunk">> => hb_util:encode(Chunk),
        <<"data_path">> => hb_util:encode(DataPath),
        <<"offset">> => integer_to_binary(Offset),
        <<"data_size">> => integer_to_binary(DataSize),
        <<"data_root">> => hb_util:encode(DataRoot)
    },
    try
        Serialized = hb_json:encode(Request),
        Response = hb_http:post(
            hb_opts:get(gateway, not_found, Opts),
            #{
                <<"path">> => <<"/chunk">>,
                <<"body">> => Serialized
            },
            Opts
        ),
        case Response of
            {ok, _} -> {ok, proof_posted};
            {error, Reason} -> {error, Reason}
        end
    catch
        _:Err:_Stack ->
            ?event(bundler_short, {post_proof_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
    end.

get_price(DataSize, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{ <<"path">> => <<"/price">>, <<"size">> => DataSize },
        Opts
    ).

get_anchor(Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{ <<"path">> => <<"/tx_anchor">> },
        Opts
    ).

%%%===================================================================
%%% Tests
%%%===================================================================

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
            retry_base_delay_ms => 100,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10, Opts), new_data_item(2, 10, Opts)],
        dispatch(Items, Opts),
        % Wait for TX to be posted
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        % Wait for chunk to be posted
        Proofs = hb_mock_server:get_requests(chunk, 1, ServerHandle),
        ?assertEqual(1, length(Proofs)),
        % Verify dispatcher state
        State = get_state(),
        ?assertNotEqual(undefined, State),
        ?assertNotEqual(timeout, State),
        % All workers should be idle
        Workers = State#state.workers,
        IdleWorkers = [PID || {PID, Status} <- maps:to_list(Workers), Status =:= idle],
        ?assertEqual(maps:size(Workers), length(IdleWorkers)),
        % Task queue should be empty
        Queue = State#state.task_queue,
        ?assert(queue:is_empty(Queue)),
        % Bundle should be completed and removed
        Bundles = State#state.bundles,
        ?assertEqual(0, maps:size(Bundles)),
        ok
    after
        cleanup_dispatcher(ServerHandle)
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
            retry_base_delay_ms => 50,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10, Opts)],
        dispatch(Items, Opts),
        % Wait for TX to eventually be posted
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        % Verify it retried multiple times
        FinalCount = get_test_counter(price_attempts_counter),
        ?assertEqual(FailCount+1, FinalCount),
        ok
    after
        cleanup_test_counter(price_attempts_counter),
        cleanup_dispatcher(ServerHandle)
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
            retry_base_delay_ms => 50,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10, Opts)],
        dispatch(Items, Opts),
        % Wait for TX to eventually be posted
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        % Verify it retried multiple times
        FinalCount = get_test_counter(anchor_attempts_counter),
        ?assertEqual(FailCount+1, FinalCount),
        ok
    after
        cleanup_test_counter(anchor_attempts_counter),
        cleanup_dispatcher(ServerHandle)
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
        % Use short retry delays for testing.
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            retry_base_delay_ms => 50,
            retry_jitter => 0  % Disable jitter for deterministic tests
        },
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10, Opts)],
        dispatch(Items, Opts),
        % Wait for TX to eventually succeed
        TXs = hb_mock_server:get_requests(tx, FailCount+1, ServerHandle),
        ?assertEqual(FailCount+1, length(TXs)),
        % Verify final attempt succeeded
        FinalCount = get_test_counter(tx_attempts_counter),
        ?assertEqual(FailCount+1, FinalCount),
        ok
    after
        cleanup_test_counter(tx_attempts_counter),
        cleanup_dispatcher(ServerHandle)
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
            retry_base_delay_ms => 50,
            retry_jitter => 0
        },
        hb_http_server:start_node(Opts),
        % Large enough for multiple chunks
        Items = [new_data_item(1, floor(4.5 * ?DATA_CHUNK_SIZE), Opts)],
        dispatch(Items, Opts),
        % Wait for TX
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        % Wait for chunks to eventually succeed
        Chunks = hb_mock_server:get_requests(chunk, FailCount+5, ServerHandle),
        ?assertEqual( FailCount+5, length(Chunks)),
        % Verify retries happened
        FinalCount = get_test_counter(chunk_attempts_counter),
        ?assertEqual(FailCount+5, FinalCount),
        ok
    after
        cleanup_test_counter(chunk_attempts_counter),
        cleanup_dispatcher(ServerHandle)
    end.

empty_dispatch_test() ->
    Opts = #{},
    dispatch([], Opts),
    % Should not crash
    ok.

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
            bundler_workers => 3
        },
        hb_http_server:start_node(Opts),
        % Dispatch 10 bundles rapidly
        lists:foreach(
            fun(I) ->
                Items = [new_data_item(I, 10, Opts)],
                dispatch(Items, Opts)
            end,
            lists:seq(1, 10)
        ),
        
        % Wait for all 10 TXs
        TXs = hb_mock_server:get_requests(tx, 10, ServerHandle),
        ?assertEqual(10, length(TXs)),
        ok
    after
        cleanup_dispatcher(ServerHandle)
    end.

one_bundle_fails_others_continue_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    setup_test_counter(mixed_attempts_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            % First TX succeeds, all following attempts fail.
            Count = increment_test_counter(mixed_attempts_counter) - 1,
            case Count of
                0 -> {200, <<"OK">>}; 
                _ -> {400, <<"fail">>}
            end
        end
    }),
    try
        % Use short retry delays for testing (100ms base, with exponential backoff)
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            retry_base_delay_ms => 100,
            retry_jitter => 0  % Disable jitter for deterministic tests
        },
        hb_http_server:start_node(Opts),
        % Dispatch first bundle (will keep failing)
        Items1 = [new_data_item(1, 10, Opts)],
        dispatch(Items1, Opts),
        % Dispatch second bundle (will succeed)
        Items2 = [new_data_item(2, 10, Opts)],
        dispatch(Items2, Opts),
        % Wait for at least 5 TX attempts (1 success + multiple retries)
        TXs = hb_mock_server:get_requests(tx, 5, ServerHandle),
        ?assert(length(TXs) >= 5, length(TXs)),
        ok
    after
        cleanup_test_counter(mixed_attempts_counter),
        cleanup_dispatcher(ServerHandle)
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
            bundler_workers => 5
        },
        hb_http_server:start_node(Opts),
        % Dispatch 3 bundles, each with 2 chunks
        lists:foreach(
            fun(I) ->
                Items = [new_data_item(I, 10, Opts)],
                dispatch(Items, Opts)
            end,
            lists:seq(1, 10)
        ),
        % With 3 workers and 1s delay, 10 chunks should complete in ~2s not 9s
        StartTime = erlang:system_time(millisecond),
        Chunks = hb_mock_server:get_requests(chunk, 10, ServerHandle),
        ElapsedTime = erlang:system_time(millisecond) - StartTime,
        ?assertEqual(10, length(Chunks)),
        % Should take ~2-3 seconds with parallelism, not 9+
        ?assert(ElapsedTime < 2000, "ElapsedTime: " ++ integer_to_list(ElapsedTime)),
        ok
    after
        cleanup_dispatcher(ServerHandle)
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
            % Store timestamp by attempt number.
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
            retry_base_delay_ms => 100,
            retry_max_delay_ms => 500,  % Cap at 500ms
            retry_jitter => 0  % Disable jitter for deterministic tests
        },
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10, Opts)],
        dispatch(Items, Opts),
        % Wait for TX to eventually succeed
        TXs = hb_mock_server:get_requests(tx, FailCount+1, ServerHandle, 5000),
        ?assertEqual(FailCount+1, length(TXs)),
        % Verify backoff respects cap
        Timestamps = test_attempt_timestamps(backoff_cap_counter),
        ?assertEqual(6, length(Timestamps)),
        [T1, T2, T3, T4, T5, T6] = Timestamps,
        % Calculate actual delays
        Delay1 = T2 - T1,
        Delay2 = T3 - T2,
        Delay3 = T4 - T3,
        Delay4 = T5 - T4,
        Delay5 = T6 - T5,
        % Expected: ~100ms, ~200ms, ~400ms, ~500ms (capped), ~500ms (capped)
        ?assert(Delay1 >= 70 andalso Delay1 =< 200, Delay1),
        ?assert(Delay2 >= 150 andalso Delay2 =< 300, Delay2),
        ?assert(Delay3 >= 300 andalso Delay3 =< 500, Delay3),
        ?assert(Delay4 >= 400 andalso Delay4 =< 700, Delay4),
        ?assert(Delay5 >= 400 andalso Delay5 =< 700, Delay5),
        ok
    after
        cleanup_test_counter(backoff_cap_counter),
        cleanup_dispatcher(ServerHandle)
    end.

independent_task_retry_counts_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    setup_test_counter(independent_retry_counter),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            % Use request ordering to distinguish bundles
            % First 3 requests are bundle1 (fail, fail, succeed)
            % 4th request is bundle2 (succeed)
            Count = increment_test_counter(independent_retry_counter) - 1,
            case Count < 2 of
                true -> {400, <<"fail">>};  % First 2 attempts fail
                false -> {200, <<"OK">>}    % Rest succeed
            end
        end
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store(),
            retry_base_delay_ms => 100,
            retry_jitter => 0  % Disable jitter for deterministic tests
        },
        hb_http_server:start_node(Opts),
        % Dispatch first bundle (will fail twice and retry)
        Items1 = [new_data_item(1, 10, Opts)],
        dispatch(Items1, Opts),
        % Wait a bit for first bundle to start failing
        hb_mock_server:get_requests(tx, 3, ServerHandle),
        % Dispatch second bundle (will succeed on first try since we're past the 2 failures)
        Items2 = [new_data_item(2, 10, Opts)],
        dispatch(Items2, Opts),
        % Verify we got all TX requests logged
        TotalAttempts = 4,
        TXs = hb_mock_server:get_requests(tx, TotalAttempts, ServerHandle),
        ?assertEqual(TotalAttempts, length(TXs)),
        ok
    after
        cleanup_test_counter(independent_retry_counter),
        cleanup_dispatcher(ServerHandle)
    end.

recover_bundles_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            store => hb_test_utils:test_store()
        },
        hb_http_server:start_node(Opts),
        % Create some test items
        Item1 = new_data_item(1, 10, Opts),
        Item2 = new_data_item(2, 10, Opts),
        Item3 = new_data_item(3, 10, Opts),
        % Write items to cache as unbundled
        ok = dev_bundler_cache:write_item(Item1, Opts),
        ok = dev_bundler_cache:write_item(Item2, Opts),
        ok = dev_bundler_cache:write_item(Item3, Opts),
        % Create a bundle TX and cache it with posted status
        {ok, TX} = dev_codec_tx:to(lists:reverse([Item1, Item2, Item3]), #{}, #{}),
        CommittedTX = hb_message:convert(TX, <<"structured@1.0">>, <<"tx@1.0">>, Opts),
        ok = dev_bundler_cache:write_tx(CommittedTX, [Item1, Item2, Item3], Opts),
        % Create a second bundle that is already complete (should not be recovered)
        Item4 = new_data_item(4, 10, Opts),
        ok = dev_bundler_cache:write_item(Item4, Opts),
        {ok, TX2} = dev_codec_tx:to(lists:reverse([Item4]), #{}, #{}),
        CommittedTX2 = hb_message:convert(TX2, <<"structured@1.0">>, <<"tx@1.0">>, Opts),
        ok = dev_bundler_cache:write_tx(CommittedTX2, [Item4], Opts),
        ok = dev_bundler_cache:complete_tx(CommittedTX2, Opts),
        % Now initialize dispatcher which should recover only the posted bundle
        ensure_dispatcher(Opts),
        State = get_state(),
        % Get the recovered bundle (should only be 1, not the completed one)
        ?assertEqual(1, maps:size(State#state.bundles)),
        [Bundle] = maps:values(State#state.bundles),
        ?assertNotEqual(undefined, Bundle#bundle.start_time),
        ?assertEqual(#{}, Bundle#bundle.proofs),
        RecoveredItems = [
            hb_message:with_commitments(
                #{ <<"commitment-device">> => <<"ans104@1.0">> }, Item, Opts)
            || Item <- Bundle#bundle.items],
        ?assertEqual(
            lists:sort([Item1, Item2, Item3]),
            lists:sort(RecoveredItems)),
        ?assertEqual(tx_posted, Bundle#bundle.status),
        ?assert(hb_message:verify(Bundle#bundle.tx)),
        ?assertEqual(
            hb_message:id(CommittedTX, signed, Opts),
            hb_message:id(Bundle#bundle.tx, signed, Opts)),
        ok
    after
        cleanup_dispatcher(ServerHandle)
    end.

%%% Test Helper Functions

new_data_item(Index, Size, Opts) ->
    Data = rand:bytes(Size),
    Tag = <<"tag", (integer_to_binary(Index))/binary>>,
    Value = <<"value", (integer_to_binary(Index))/binary>>,
    Item = ar_bundles:sign_item(
        #tx{
            data = Data,
            tags = [{Tag, Value}]
        },
        hb:wallet()
    ),
    hb_message:convert(Item, <<"structured@1.0">>, <<"ans104@1.0">>, Opts).

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

cleanup_dispatcher(ServerHandle) ->
    stop_dispatcher(),
    timer:sleep(10), % Ensure dispatcher fully stops
    hb_mock_server:stop(ServerHandle).

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
