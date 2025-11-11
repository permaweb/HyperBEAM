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
    opts                 % Configuration options
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
    ?event({enqueue_task, format_task(Task)}),
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
    ?event({task_failed_retrying, format_task(Task), {reason, Reason}}),
    % Update worker to idle
    State1 = State#state{
        workers = maps:put(WorkerPID, idle, Workers)
    },
    % Immediately re-queue the failed task
    enqueue_task(Task, State1).

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
        TXSize = TX#tx.data_size,
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
                        DataPath = ar_merkle:generate_path(DataRoot, Offset - 1, DataTree),
                        Proof = #{
                            chunk => Chunk,
                            data_path => DataPath,
                            offset => Offset,
                            data_size => TXSize,
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
