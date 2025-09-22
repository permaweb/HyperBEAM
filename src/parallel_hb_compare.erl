%% Add this to your module or create a new module: parallel_hb_compare.erl
-module(parallel_hb_compare).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, compare_processes/1, compare_processes/2]).

%% gen_server callbacks
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SUPERVISED_MODE, true).
-define(DEFAULT_MAX_WORKERS, 10).

-record(state, {
    max_workers = ?DEFAULT_MAX_WORKERS :: integer(),
    active_workers = #{} :: #{pid() => string()}, % pid -> process_id mapping
    pending_processes = [] :: [string()],
    completed = 0 :: integer(),
    total = 0 :: integer()
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
        undefined when ?SUPERVISED_MODE ->
            {ok, _} = start_supervised(MaxWorkers);
        undefined ->
            {ok, _} = start_link(MaxWorkers);
        _Pid ->
            ok
    end,
    gen_server:cast(?SERVER, {compare_processes, ProcessList}).

start_supervised(MaxWorkers) ->
    %% Then add the parallel module child dynamically
    ChildSpec = #{
        id => parallel_hb_compare,
        start => {parallel_hb_compare, start_link, [MaxWorkers]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [parallel_hb_compare]
    },
    supervisor:start_child(hb_sup, ChildSpec).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([MaxWorkers]) ->
    process_flag(trap_exit, true),
    io:format("Starting parallel comparison server with ~p max workers~n", [MaxWorkers]),
    {ok, #state{max_workers = MaxWorkers}}.

handle_cast({compare_processes, ProcessList}, State) ->
    io:format("Starting comparison of ~p processes~n", [length(ProcessList)]),
    NewState = State#state{
        pending_processes = State#state.pending_processes ++ ProcessList,
        total = State#state.total + length(ProcessList)
    },
    %% Start initial batch of workers
    UpdatedState = spawn_workers(NewState),
    {noreply, UpdatedState}.

handle_call(_Msg, _From, State) ->
    {reply, error, State}.

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
    CompletedState = State#state{
        active_workers = NewActiveWorkers,
        completed = NewCompleted
    },
    
    %% Check if all work is done
    case NewCompleted >= State#state.total of
        true ->
            %% All processes completed
            io:format("All ~p processes completed successfully~n", [State#state.total]),
            {noreply, CompletedState};
        false ->
            %% More work to do, spawn next worker if available
            FinalState = spawn_workers(CompletedState),
            {noreply, FinalState}
    end.

%% @doc Spawn worker processes up to the maximum limit
spawn_workers(#{active_workers := ActiveWorkers, max_workers := MaxWorkers} = State) when map_size(ActiveWorkers) =:= MaxWorkers ->
    State;

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
            {NewActiveWorkers, PendingProcesses} = 
                lists:foldl(fun(ProcessId, {WorkersAcc, [_ | PendingProcesses]}) ->
                    WorkerPid = spawn_link(fun() -> worker_function(ProcessId) end),
                    io:format("Started worker ~p for process ~s~n", [WorkerPid, ProcessId]),
                    {
                        maps:put(WorkerPid, ProcessId, WorkersAcc),
                        PendingProcesses
                    }
                end, {State#state.active_workers, State#state.pending_processes}, ProcessesToStart),
            io:format("~p processes pending left.~n", [length(PendingProcesses)]),
            State#state{
                active_workers = NewActiveWorkers,
                pending_processes = PendingProcesses
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