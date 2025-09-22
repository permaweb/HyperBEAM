%% Add this to your module or create a new module: parallel_hb_compare.erl
-module(parallel_hb_compare).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, compare_processes/1, compare_processes/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_WORKERS, 10).

-record(state, {
    max_workers = ?DEFAULT_MAX_WORKERS :: integer(),
    active_workers = #{} :: #{pid() => string()}, % pid -> process_id mapping
    pending_processes = [] :: [string()],
    completed = 0 :: integer(),
    total = 0 :: integer(),
    caller = undefined :: pid() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the GenServer with default max workers
start_link() ->
    start_link(?DEFAULT_MAX_WORKERS).

%% @doc Start the GenServer with custom max workers
start_link(MaxWorkers) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxWorkers], []).

%% @doc Compare processes with default max workers (10)
compare_processes(ProcessList) ->
    compare_processes(ProcessList, ?DEFAULT_MAX_WORKERS).

%% @doc Compare processes with custom max workers
compare_processes(ProcessList, MaxWorkers) ->
    case whereis(?SERVER) of
        undefined ->
            {ok, _} = start_link(MaxWorkers);
        _Pid ->
            ok
    end,
    gen_server:call(?SERVER, {compare_processes, ProcessList}, infinity).

%% @doc Stop the GenServer
stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MaxWorkers]) ->
    process_flag(trap_exit, true),
    io:format("Starting parallel comparison server with ~p max workers~n", [MaxWorkers]),
    {ok, #state{max_workers = MaxWorkers}}.

handle_call({compare_processes, ProcessList}, From, State) ->
    io:format("Starting comparison of ~p processes~n", [length(ProcessList)]),
    NewState = State#state{
        pending_processes = ProcessList,
        completed = 0,
        total = length(ProcessList),
        caller = From,
        active_workers = #{}
    },
    %% Start initial batch of workers
    UpdatedState = spawn_workers(NewState),
    {noreply, UpdatedState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle worker completion/failure
handle_info({'EXIT', Pid, Reason}, State) ->
    ProcessId = maps:get(Pid, State#state.active_workers),
    case Reason of
        normal ->
            io:format("Worker completed successfully for process ~s (~p/~p)~n", 
                     [ProcessId, State#state.completed + 1, State#state.total]);
        _ ->
            io:format("Worker failed for process ~s with reason ~p (~p/~p)~n", 
                     [ProcessId, Reason, State#state.completed + 1, State#state.total])
    end,
    handle_worker_completion(Pid, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Kill all active workers
    WorkerPids = maps:keys(State#state.active_workers),
    [exit(Pid, kill) || Pid <- WorkerPids],
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Handle completion of a worker process
handle_worker_completion(CompletedPid, State) ->
    %% Remove completed worker from active workers
    NewActiveWorkers = maps:remove(CompletedPid, State#state.active_workers),
    NewCompleted = State#state.completed + 1,
    
    UpdatedState = State#state{
        active_workers = NewActiveWorkers,
        completed = NewCompleted
    },
    
    %% Check if all work is done
    case NewCompleted >= State#state.total of
        true ->
            %% All processes completed
            io:format("All ~p processes completed successfully~n", [State#state.total]),
            case State#state.caller of
                undefined -> ok;
                Caller -> gen_server:reply(Caller, ok)
            end,
            {noreply, UpdatedState#state{caller = undefined}};
        false ->
            %% More work to do, spawn next worker if available
            FinalState = spawn_workers(UpdatedState),
            {noreply, FinalState}
    end.

%% @doc Spawn worker processes up to the maximum limit
spawn_workers(State) ->
    ActiveCount = maps:size(State#state.active_workers),
    AvailableSlots = State#state.max_workers - ActiveCount,
    ProcessesToStart = lists:sublist(State#state.pending_processes, AvailableSlots),
    
    case ProcessesToStart of
        [] ->
            %% No processes to start
            State;
        _ ->
            %% Start new workers
            {NewActiveWorkers, RemainingProcesses} = 
                lists:foldl(fun(ProcessId, {WorkersAcc, [_|RestProcesses]}) ->
                    WorkerPid = spawn_link(fun() -> worker_function(ProcessId) end),
                    io:format("Started worker ~p for process ~s~n", [WorkerPid, ProcessId]),
                    {maps:put(WorkerPid, ProcessId, WorkersAcc), RestProcesses}
                end, {State#state.active_workers, State#state.pending_processes}, ProcessesToStart),
            
            State#state{
                active_workers = NewActiveWorkers,
                pending_processes = RemainingProcesses
            }
    end.

%% @doc Worker function that performs the actual comparison
worker_function(ProcessId) ->
    try
        legacy_hb_compare:compare_testnet(ProcessId)
    catch
        Class:Reason:Stacktrace ->
            io:format("Error in worker for process ~s: ~p:~p~n~p~n", 
                     [ProcessId, Class, Reason, Stacktrace]),
            %% Re-raise the exception to trigger EXIT message
            erlang:raise(Class, Reason, Stacktrace)
    end.