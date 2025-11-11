%%% @doc A dispatcher for the bundler device (dev_bundler). This module
%%% manages a worker pool to handle bundle building, TX posting, proof
%%% generation, and chunk seeding. Failed tasks are automatically re-queued
%%% for immediate retry until successful.
-module(dev_bundler_dispatch).
-export([dispatch/2, stop_dispatcher/0]).
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
    type,                % Task type: build_tx | post_tx | build_proofs | post_proof
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
    proofs               % Map of offset => #proof{} records
}).

%%% Default options.
-define(DISPATCHER_NAME, bundler_dispatcher).
-define(DEFAULT_NUM_WORKERS, 5).
-define(DEFAULT_RETRY_BASE_DELAY_MS, 1000).
-define(DEFAULT_RETRY_MAX_DELAY_MS, 600000). % 10 minutes
-define(DEFAULT_RETRY_JITTER, 0.25). % ±25% jitter

%% @doc Dispatch the queue.
dispatch([], _Opts) ->
    ?event({skipping_empty_queue});
dispatch(Items, Opts) ->
    PID = ensure_dispatcher(Opts),
    PID ! {dispatch, Items}.

%% @doc Return the PID of the dispatch server. If the server is not running,
%% it is started and registered with the name `?SERVER_NAME'.
ensure_dispatcher(Opts) ->
    case hb_name:lookup(?DISPATCHER_NAME) of
        undefined ->
            PID = spawn(fun() -> init_dispatcher(Opts) end),
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
init_dispatcher(Opts) ->
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
    dispatcher(State).

%% @doc The main loop of the dispatcher. Manages task queue and worker pool.
dispatcher(State) ->
    receive
        {dispatch, Items} ->
            % Create a new bundle and queue the build_tx task
            Opts = State#state.opts,
            BundleID = make_ref(),
            Bundle = #bundle{
                id = BundleID,
                items = Items,
                status = initializing,
                tx = undefined,
                proofs = #{}
            },
            State1 = State#state{
                bundles = maps:put(BundleID, Bundle, State#state.bundles)
            },
            % Enqueue the build_tx task
            Task = #task{bundle_id = BundleID, type = build_tx, data = Items, opts = Opts},
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
format_task(#task{bundle_id = BundleID, type = build_tx, data = Items}) ->
    {build_tx, {bundle, BundleID}, {items, length(Items)}};
format_task(#task{bundle_id = BundleID, type = post_tx, data = TX}) ->
    {post_tx, {bundle, BundleID}, {tx, TX}};
format_task(#task{bundle_id = BundleID, type = build_proofs, data = TX}) ->
    {build_proofs, {bundle, BundleID}, {tx, TX}};
format_task(#task{bundle_id = BundleID, type = post_proof, data = Proof}) ->
    Offset = maps:get(offset, Proof),
    {post_proof, {bundle, BundleID}, {offset, Offset}}.

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
    ?event({task_complete, format_task(Task)}),
    % Update worker to idle
    State1 = State#state{
        workers = maps:put(WorkerPID, idle, Workers)
    },
    case maps:get(BundleID, Bundles, undefined) of
        undefined ->
            ?event({bundle_not_found, BundleID}),
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
    ?event({task_failed_retrying, format_task(Task),
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

task_completed(#task{bundle_id = BundleID, type = build_tx}, Bundle, Result, State) ->
    Bundles = State#state.bundles,
    Opts = State#state.opts,
    Bundle1 = Bundle#bundle{
        tx = Result,
        status = tx_built
    },
    State1 = State#state{
        bundles = maps:put(BundleID, Bundle1, Bundles)
    },
    PostTXTask = #task{bundle_id = BundleID, type = post_tx, data = Result, opts = Opts},
    enqueue_task(PostTXTask, State1);

task_completed(#task{bundle_id = BundleID, type = post_tx}, Bundle, TX, State) ->
    Bundles = State#state.bundles,
    Opts = State#state.opts,
    Bundle1 = Bundle#bundle{status = tx_posted, tx = TX},
    State1 = State#state{
        bundles = maps:put(BundleID, Bundle1, Bundles)
    },
    BuildProofsTask = #task{bundle_id = BundleID, type = build_proofs, data = TX, opts = Opts},
    enqueue_task(BuildProofsTask, State1);

task_completed(#task{bundle_id = BundleID, type = build_proofs}, Bundle, Proofs, State) ->
    Bundles = State#state.bundles,
    case Proofs of
        [] ->
            % No proofs, bundle complete
            bundle_complete(BundleID, State);
        _ ->
            Opts = State#state.opts,
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
            bundle_complete(BundleID, State1);
        false ->
            State1
    end.

%% @doc Mark a bundle as complete and remove it from state.
bundle_complete(BundleID, State) ->
    ?event({bundle_complete, BundleID}),
    Bundles = State#state.bundles,
    State#state{
        bundles = maps:remove(BundleID, Bundles)
    }.

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
execute_task(#task{type = build_tx, data = Items}) ->
    try
        {ok, TX} = dev_codec_tx:to(lists:reverse(Items), #{}, #{}),
        {ok, TX}
    catch
        _:Err:_Stack -> {error, Err}
    end;

execute_task(#task{type = post_tx, data = TX, opts = Opts} = Task) ->
    try
        ?event({execute_task, format_task(Task)}),
        % Get price and anchor
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
                PostTXResponse = hb_ao:resolve(
                    #{ <<"device">> => <<"arweave@2.9-pre">> },
                    Committed#{
                        <<"path">> => <<"/tx">>,
                        <<"method">> => <<"POST">>
                    },
                    Opts
                ),
                case PostTXResponse of
                    {ok, _Result} -> {ok, SignedTX};
                    {_, ErrorReason} -> {error, ErrorReason}
                end;
            {PriceErr, AnchorErr} ->
                ?event({post_tx_failed,
                    format_task(Task),
                    {price, PriceErr},
                    {anchor, AnchorErr}}),
                {error, {PriceErr, AnchorErr}}
        end
    catch
        _:Err:_Stack -> 
            ?event({post_tx_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
    end;

execute_task(#task{type = build_proofs, data = TX, opts = _Opts} = Task) ->
    try
        ?event({execute_task, format_task(Task)}),
        % Calculate chunks and proofs
        Data = TX#tx.data,
        DataRoot = TX#tx.data_root,
        DataSize = TX#tx.data_size,
        Chunks = ar_tx:chunk_binary(?DATA_CHUNK_SIZE, Data),
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
            ?event({build_proofs_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
    end;

execute_task(#task{type = post_proof, data = Proof, opts = Opts} = Task) ->
    #{chunk := Chunk, data_path := DataPath, offset := Offset,
      data_size := DataSize, data_root := DataRoot} = Proof,
    ?event({execute_task, format_task(Task)}),
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
            ?event({post_proof_failed,
                format_task(Task),
                {error, Err}}),
            {error, Err}
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

complete_task_sequence_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        Opts = NodeOpts#{priv_wallet => hb:wallet()},
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10), new_data_item(2, 10)],
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
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => fun(_Req) ->
            Count = persistent_term:get(price_attempts, 0),
            persistent_term:put(price_attempts, Count + 1),
            case Count < FailCount of
                true -> {500, <<"error">>};
                false -> {200, <<"12345">>}
            end
        end,
        tx_anchor => {200, hb_util:encode(Anchor)}
    }),
    try
        persistent_term:put(price_attempts, 0),
        Opts = NodeOpts#{priv_wallet => hb:wallet()},
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10)],
        dispatch(Items, Opts),
        % Wait for TX to eventually be posted
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        % Verify it retried multiple times
        FinalCount = persistent_term:get(price_attempts, 0),
        ?assertEqual(FailCount+1, FinalCount),
        ok
    after
        persistent_term:erase(price_attempts),
        cleanup_dispatcher(ServerHandle)
    end.

post_tx_anchor_failure_retry_test() ->
    Price = 12345,
    FailCount = 3,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => fun(_Req) ->
            Count = persistent_term:get(anchor_attempts, 0),
            persistent_term:put(anchor_attempts, Count + 1),
            case Count < FailCount of
                true -> {500, <<"error">>};
                false -> {200, hb_util:encode(rand:bytes(32))}
            end
        end
    }),
    try
        persistent_term:put(anchor_attempts, 0),
        Opts = NodeOpts#{priv_wallet => hb:wallet()},
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10)],
        dispatch(Items, Opts),
        % Wait for TX to eventually be posted
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        % Verify it retried multiple times
        FinalCount = persistent_term:get(anchor_attempts, 0),
        ?assertEqual(FailCount+1, FinalCount),
        ok
    after
        persistent_term:erase(anchor_attempts),
        cleanup_dispatcher(ServerHandle)
    end.

post_tx_post_failure_retry_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    FailCount = 4,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            Count = persistent_term:get(tx_attempts, 0),
            persistent_term:put(tx_attempts, Count + 1),
            case Count < FailCount of
                true -> {400, <<"Transaction verification failed">>};
                false -> {200, <<"OK">>}
            end
        end
    }),
    try
        persistent_term:put(tx_attempts, 0),
        % Use short retry delays for testing (100ms base, with exponential backoff)
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            retry_base_delay_ms => 100,
            retry_jitter => 0  % Disable jitter for deterministic tests
        },
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10)],
        dispatch(Items, Opts),
        % Wait for TX to eventually succeed
        TXs = hb_mock_server:get_requests(tx, FailCount+1, ServerHandle),
        ?assertEqual(FailCount+1, length(TXs)),
        % Verify final attempt succeeded
        FinalCount = persistent_term:get(tx_attempts, 0),
        ?assertEqual(FailCount+1, FinalCount),
        ok
    after
        persistent_term:erase(tx_attempts),
        cleanup_dispatcher(ServerHandle)
    end.

post_proof_failure_retry_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    FailCount = 2,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        chunk => fun(_Req) ->
            Count = persistent_term:get(chunk_attempts, 0),
            persistent_term:put(chunk_attempts, Count + 1),
            case Count < FailCount of
                true -> {500, <<"error">>};
                false -> {200, <<"OK">>}
            end
        end
    }),
    try
        persistent_term:put(chunk_attempts, 0),
        Opts = NodeOpts#{priv_wallet => hb:wallet()},
        hb_http_server:start_node(Opts),
        % Large enough for multiple chunks
        Items = [new_data_item(1, floor(4.5 * ?DATA_CHUNK_SIZE))],
        dispatch(Items, Opts),
        % Wait for TX
        TXs = hb_mock_server:get_requests(tx, 1, ServerHandle),
        ?assertEqual(1, length(TXs)),
        % Wait for chunks to eventually succeed
        Chunks = hb_mock_server:get_requests(chunk, FailCount+5, ServerHandle),
        ?assertEqual( FailCount+5, length(Chunks)),
        % Verify retries happened
        FinalCount = persistent_term:get(chunk_attempts, 0),
        ?assertEqual(FailCount+5, FinalCount),
        ok
    after
        persistent_term:erase(chunk_attempts),
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
            timer:sleep(1000),
            {200, <<"OK">>}
        end
    }),
    try
        Opts = NodeOpts#{priv_wallet => hb:wallet(), bundler_workers => 3},
        hb_http_server:start_node(Opts),
        % Dispatch 10 bundles rapidly
        lists:foreach(
            fun(I) ->
                Items = [new_data_item(I, 10)],
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
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            % First TX fails, second succeeds
            Count = persistent_term:get(tx_mixed_attempts, 0),
            persistent_term:put(tx_mixed_attempts, Count + 1),
            case Count of
                0 -> {200, <<"OK">>}; 
                _ -> {400, <<"fail">>}
            end
        end
    }),
    try
        persistent_term:put(tx_mixed_attempts, 0),
        % Use short retry delays for testing (100ms base, with exponential backoff)
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            retry_base_delay_ms => 100,
            retry_jitter => 0  % Disable jitter for deterministic tests
        },
        hb_http_server:start_node(Opts),
        % Dispatch first bundle (will keep failing)
        Items1 = [new_data_item(1, 10)],
        dispatch(Items1, Opts),
        % Dispatch second bundle (will succeed)
        Items2 = [new_data_item(2, 10)],
        dispatch(Items2, Opts),
        % Wait for at least 5 TX attempts (1 success + multiple retries)
        TXs = hb_mock_server:get_requests(tx, 5, ServerHandle),
        ?assert(length(TXs) >= 5, length(TXs)),
        ok
    after
        persistent_term:erase(tx_mixed_attempts),
        cleanup_dispatcher(ServerHandle)
    end.

parallel_task_execution_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    SleepTime = 1000,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        chunk => fun(_Req) ->
            timer:sleep(SleepTime),
            {200, <<"OK">>}
        end
    }),
    try
        Opts = NodeOpts#{priv_wallet => hb:wallet(), bundler_workers => 5},
        hb_http_server:start_node(Opts),
        % Dispatch 3 bundles, each with 2 chunks
        lists:foreach(
            fun(I) ->
                Items = [new_data_item(I, 10)],
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
        ?assert(ElapsedTime < 5000, "ElapsedTime: " ++ integer_to_list(ElapsedTime)),
        ok
    after
        cleanup_dispatcher(ServerHandle)
    end.

exponential_backoff_timing_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    FailCount = 5,
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            Count = persistent_term:get(backoff_cap_attempts, 0),
            Timestamp = erlang:system_time(millisecond),
            persistent_term:put(backoff_cap_attempts, Count + 1),
            % Store timestamp of each attempt
            Timestamps = persistent_term:get(backoff_cap_timestamps, []),
            persistent_term:put(backoff_cap_timestamps, [Timestamp | Timestamps]),
            case Count < FailCount of
                true -> {400, <<"fail">>};
                false -> {200, <<"OK">>}
            end
        end
    }),
    try
        persistent_term:put(backoff_cap_attempts, 0),
        persistent_term:put(backoff_cap_timestamps, []),
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            retry_base_delay_ms => 1000,
            retry_max_delay_ms => 5000,  % Cap at 5000ms
            retry_jitter => 0  % Disable jitter for deterministic tests
        },
        hb_http_server:start_node(Opts),
        Items = [new_data_item(1, 10)],
        dispatch(Items, Opts),
        % Wait for TX to eventually succeed
        TXs = hb_mock_server:get_requests(tx, FailCount+1, ServerHandle, 30000),
        ?assertEqual(FailCount+1, length(TXs)),
        % Verify backoff respects cap
        Timestamps = lists:reverse(persistent_term:get(backoff_cap_timestamps, [])),
        ?assertEqual(6, length(Timestamps)),
        [T1, T2, T3, T4, T5, T6] = Timestamps,
        % Calculate actual delays
        Delay1 = T2 - T1,
        Delay2 = T3 - T2,
        Delay3 = T4 - T3,
        Delay4 = T5 - T4,
        Delay5 = T6 - T5,
        % Expected: ~1000ms, ~2000ms, ~4000ms, ~5000ms (capped), ~5000ms (capped)
        ?assert(Delay1 >= 800 andalso Delay1 =< 1500, Delay1),
        ?assert(Delay2 >= 1800 andalso Delay2 =< 2500, Delay2),
        ?assert(Delay3 >= 3800 andalso Delay3 =< 4500, Delay3),
        % These should be capped at 5000ms, not 8000ms and 16000ms
        ?assert(Delay4 >= 4800 andalso Delay4 =< 5500, Delay4),
        ?assert(Delay5 >= 4800 andalso Delay5 =< 5500, Delay5),
        ok
    after
        persistent_term:erase(backoff_cap_attempts),
        persistent_term:erase(backoff_cap_timestamps),
        cleanup_dispatcher(ServerHandle)
    end.

independent_task_retry_counts_test() ->
    Anchor = rand:bytes(32),
    Price = 12345,
    % Track which bundles we've seen
    persistent_term:put(independent_bundle_ids, []),
    {ServerHandle, NodeOpts} = start_mock_gateway(#{
        price => {200, integer_to_binary(Price)},
        tx_anchor => {200, hb_util:encode(Anchor)},
        tx => fun(_Req) ->
            % Use request ordering to distinguish bundles
            % First 3 requests are bundle1 (fail, fail, succeed)
            % 4th request is bundle2 (succeed)
            Count = persistent_term:get(independent_total_attempts, 0),
            persistent_term:put(independent_total_attempts, Count + 1),
            case Count < 2 of
                true -> {400, <<"fail">>};  % First 2 attempts fail
                false -> {200, <<"OK">>}    % Rest succeed
            end
        end
    }),
    try
        persistent_term:put(independent_total_attempts, 0),
        Opts = NodeOpts#{
            priv_wallet => hb:wallet(),
            retry_base_delay_ms => 1000,
            retry_jitter => 0  % Disable jitter for deterministic tests
        },
        hb_http_server:start_node(Opts),
        % Dispatch first bundle (will fail twice and retry)
        Items1 = [new_data_item(1, 10)],
        dispatch(Items1, Opts),
        % Wait a bit for first bundle to start failing
        hb_mock_server:get_requests(tx, 3, ServerHandle),
        % Dispatch second bundle (will succeed on first try since we're past the 2 failures)
        Items2 = [new_data_item(2, 10)],
        dispatch(Items2, Opts),
        % Verify we got all TX requests logged
        TotalAttempts = 4,
        TXs = hb_mock_server:get_requests(tx, TotalAttempts, ServerHandle),
        ?assertEqual(TotalAttempts, length(TXs)),
        ok
    after
        persistent_term:erase(independent_total_attempts),
        persistent_term:erase(independent_bundle_ids),
        cleanup_dispatcher(ServerHandle)
    end.

%%% Test Helper Functions

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
    timer:sleep(100), % Ensure dispatcher fully stops
    hb_mock_server:stop(ServerHandle).
