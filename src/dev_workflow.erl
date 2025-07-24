-module(dev_workflow).
-export([info/1, info/3, echo/3, create/3, prepare_start/3, prepare_next/3, run/3]).
% -define(STORE_OPTS, #{~"store-module" => hb_store_lmdb, ~"name" => ~"cache-mainnet/lmdb"}).
-define(STORE_OPTS, #{~"store-module" => hb_store_lmdb}).
-define(DEVICE_VSN, ~"workflow@1.0").

%% @doc workflow for storage where the transitions are only the vertices ids
%%   id is a unique internal identifier for the workflow
%%   name is user given name for the workflow
%%   transitions is a map #{from_step_id => [#{condition => target_step_id}]}
%%   AR wallet of the user that created the workflow
-record(workflow, {
    id,
    device = ?DEVICE_VSN,
    commit,
    name,
    transitions,
    wallet_address
}).

%% workflow step (graph vertex) is persisted separately for lazy loading
%% action_code is either a lua module or an wasm image
-record(workflow_step, {
    id,
    device = ?DEVICE_VSN,
    name,
    description,
    execution_device,
    action_code,
    workflow_id
}).

% provision of resources for workflow execution 
-record(workflow_schedule, {
    id,
    device = ?DEVICE_VSN,
    step_process,
    message,
    step_id,
    step_name,
    wallet_address
}).

% tracking of workflow steps execution 
-record(workflow_activity, {
    id,
    device = ?DEVICE_VSN,
    result,
    schedule_id,
    step_name,
    wallet_address
}).

info(_) ->
	#{ exports => [info, echo, create, prepare_start, prepare_next, run] }.

% @doc device info endpoint
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        ~"description" => ~"Workflow device to orchestrate a sequence of processes",
        ~"version" => ~"1.0",
        ~"paths" => #{
            ~"info" => ~"Get device info",
            ~"echo" => ~"Simple echo world test",
            ~"create" => ~"Commits and saves a workflow",
            ~"prepare" => ~"Schedules the process for a workflow step",
            ~"run" => ~"Runs the prepared workflow input"
        }
    },
    {ok, #{~"status" => 200, ~"body" => InfoBody}}.

%% @doc simple echo endpoint
%% curl -d "foozbarz" http://localhost:8734/~workflow@1.0/echo/~json@1.0/serialize | jq .
echo(_Msg1, Msg2, _Opts) ->
    {ok, #{~"status" => 200, ~"body" => Msg2}}.

%% @doc creates and commits a new workflow
create(_Msg1, Msg2, WalletOpts) ->
    Wallet = maps:get(~"wallet", WalletOpts),
    maybe
        ok ?= validate_workflow(Msg2),
        {ok, #workflow{id=WorkflowId}} ?= create_workflow(Msg2, Wallet),
        {ok, #{~"workflow-id" => WorkflowId}}
    end.

%% @doc creates and commits initial process for workflow execution
prepare_start(_Msg1, Msg2, WalletOpts) ->
    Wallet = maps:get(~"wallet", WalletOpts),
    maybe
        {ok, WorkflowId} ?= find_expected_field(~"workflow-id", Msg2),
        {ok, MsgBody} ?= find_expected_field(~"body", Msg2),
        {ok, Workflow} ?= load_workflow(WorkflowId, Wallet),
        {ok, {PreparedId, Process, Schedule}} ?= prepare_start_run(Workflow, MsgBody, Wallet),
        {ok, Slot} ?= hb_ao:resolve(Process, Schedule, #{ hashpath => ignore }),
        {ok, #{~"prepared-id" => PreparedId, ~"start-process" => Process, ~"schedule" => Schedule, ~"slot" => Slot}}
    end.

prepare_next(_Msg1, Msg2, WalletOpts) ->
    Wallet = maps:get(~"wallet", WalletOpts),
    maybe
        {ok, WorkflowId} ?= find_expected_field(~"workflow-id", Msg2),
        {ok, ActivityId} ?= find_expected_field(~"prev-activity-id", Msg2),
        {ok, Workflow} ?= load_workflow(WorkflowId, Wallet),
        {ok, PrevActivity} ?= load_activity(ActivityId),
        {ok, {PreparedId, Process, Schedule}} ?= prepare_next_run(Workflow, PrevActivity, Wallet),
        {ok, Slot} ?= hb_ao:resolve(Process, Schedule, #{ hashpath => ignore }),
        {ok, #{~"prepared-id" => PreparedId, ~"start-process" => Process, ~"schedule" => Schedule, ~"slot" => Slot}}
    end.

%% @doc resolves the process from the prepared state to run a workflow step
run(_Msg1, Msg2, WalletOpts) ->
    Wallet = maps:get(~"wallet", WalletOpts),
    maybe
        {ok, WorkflowId} ?= find_expected_field(~"workflow-id", Msg2),
        {ok, PreparedId} ?= find_expected_field(~"prepared-id", Msg2),
        {ok, _Workflow} ?= load_workflow(WorkflowId, Wallet),
        {ok, WorkflowSchedule} ?= load_workflow_schedule(PreparedId),
        {ok, #workflow_activity{id=ActivityId, result=Result}} ?= run_scheduled(WorkflowSchedule, Wallet),
        {ok, #{~"activity-id" => ActivityId, ~"result" => Result}}
    end.

%%
%% Internals
%%

%
% Commits to HyperBEAM
% 
commit_workflow(Wallet) ->
    Address= wallet_address(Wallet),
    Operator = wallet_address(hb:wallet()),
    hb_message:commit(#{
        ~"device" => ?DEVICE_VSN, 
        ~"type" => ~"Workflow", 
        ~"scheduler-device" => "scheduler@1.0", 
        ~"authority" => Address, 
        ~"scheduler-location" => Operator}, Wallet).

commit_process(#workflow_step{execution_device = ExecDevice, action_code = StartAction}, Wallet) ->
    Address= wallet_address(Wallet),
    Operator = wallet_address(hb:wallet()),
    hb_message:commit(#{ 
        ~"device" => ~"process@1.0", 
        ~"type" => ~"Process", 
        ~"scheduler-device" => "scheduler@1.0", 
        ~"execution-device" => ExecDevice, 
        ~"module" => StartAction, 
        ~"authority" => Address, 
        ~"scheduler-location" => Operator}, Wallet).

commit_message(MsgBody, Process, Wallet) ->
    hb_message:commit(#{ 
        ~"target" => hb_message:id(Process, all),
        ~"type" => ~"Message",
        ~"body" => MsgBody}, Wallet).

commit_schedule(MsgCommit, Wallet) ->
    hb_message:commit(#{ path => ~"schedule", method => ~"POST", body => MsgCommit}, Wallet).

%
% Create Workflow
%     
validate_workflow(Msg2) ->
    maybe 
        {ok, WorkflowMap} ?= find_expected_field(~"workflow", Msg2),
        {ok, _Name} ?= find_expected_field(~"name", WorkflowMap),
        {ok, Steps} ?= find_expected_field(~"steps", WorkflowMap),
        {ok, Transitions} ?= find_expected_field(~"transitions", WorkflowMap),
        validate_workflow(Steps, Transitions)
    end.

create_workflow(#{<<"workflow">> := #{<<"name">> := WorkflowName, <<"steps">> := Steps, <<"transitions">> := Transitions}}, Wallet) ->
    WalletAddress = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WorkflowId = <<WalletAddress/binary, ":", WorkflowName/binary>>,
    Workflow = #workflow{
        id = WorkflowId,
        name = WorkflowName,
        transitions = Transitions,
        commit = commit_workflow(Wallet),
        wallet_address = wallet_address(Wallet)
    },
    maybe
        ok ?= save(Workflow),
        ok ?= create_steps(WorkflowId, Steps),
        {ok, Workflow}
    end.

create_steps(WorkflowId, Steps) ->
    lists:foreach(fun({StepName, StepMap}) ->
        create_step(WorkflowId, StepName, StepMap)
    end, maps:to_list(Steps)).

create_step(WorkflowId, StepName, #{
        <<"description">> := Description,
        <<"execution-device">> := ExecDevice} = StepMap) ->
    ActionKey = case ExecDevice of
        <<"lua@5.3a">> -> <<"module">>;
        _ -> <<"image">>
    end,
    Action = maps:get(ActionKey, StepMap),
    Id = <<WorkflowId/binary, ":", StepName/binary>>,
    Step = #workflow_step{
        id = Id,
        name = StepName,
        description = Description,
        execution_device = ExecDevice,
        action_code = Action,
        workflow_id = WorkflowId
    },
    save(Step).

%% Validate workflow structure
validate_workflow(Steps, Transitions) ->
    case traverse_workflow(Steps, Transitions) of
        ok -> ok;
        {error, {cycle_detected, Step}} ->
            {error, <<"Cycle detected at step: ", Step/binary>>};
        {error, unreachable_steps} ->
            {error, ~"There are unreachable steps!"};
        {error, Reason} ->
            {error, Reason}
    end.

traverse_workflow(Steps, Transitions) ->
    maybe
        {ok, _StartStep} ?= maps:find(~"start", Steps), 
        {ok, _StartTransition} ?= maps:find(~"start", Transitions),
        {ok, VisitedList} ?= dfs_traverse(~"start", Transitions, sets:new(), []),
        compare_visited_steps(Steps, VisitedList)
    end.

compare_visited_steps(Steps, VisitedList) ->
    VisitedSet = sets:from_list(VisitedList),
    AllStepsSet = sets:from_list(maps:keys(Steps)),
    case {sets:is_subset(AllStepsSet, VisitedSet), sets:is_subset(VisitedSet, AllStepsSet)} of
        {true, true} -> ok;
        _ -> {error, unreachable_steps}
    end.

%
% Prepare Workflow/Step for run
% 
prepare_start_run(#workflow{id=WorkflowId}, MsgBody, Wallet) ->
    prepare_run(WorkflowId, ~"start", MsgBody, Wallet).

prepare_next_run(#workflow{id=WorkflowId, transitions=Transitions}, #workflow_activity{step_name=StepName, result=PrevResult}, Wallet) ->
    NextStepTransitions = maps:get(StepName, Transitions),
    NextStepName = maps:get(~"true", NextStepTransitions),
    prepare_run(WorkflowId, NextStepName, PrevResult, Wallet).

prepare_run(WorkflowId, StepName, MsgBody, Wallet) ->
    {ok, NextStep} = load_step(WorkflowId, StepName),
    {Process, ScheduleCommit} = commit_for_prepare(NextStep, MsgBody, Wallet),
    case schedule_run(NextStep, Process, ScheduleCommit, Wallet) of
        {ok, #workflow_schedule{id=PreparedId}} ->
            {ok, {PreparedId, Process, ScheduleCommit}};
        {error, Reason} ->
            {error, Reason}
    end.

commit_for_prepare(Step, MsgBody, Wallet) ->
    Process = commit_process(Step, Wallet),
    MsgCommit = commit_message(MsgBody, Process, Wallet),
    ScheduleCommit = commit_schedule(MsgCommit, Wallet),
    {Process, ScheduleCommit}.

schedule_run(#workflow_step{id=StepId, name=StepName}, Process, ScheduleCommit, Wallet) ->
    % TODO: Change to use random data from a commit
    WorkflowSchedule = #workflow_schedule{
        id = timestamp_id(StepId),
        step_process = Process,
        message = ScheduleCommit,
        step_id = StepId,
        step_name = StepName,
        wallet_address = wallet_address(Wallet)
    },
    maybe
        ok ?= save(WorkflowSchedule),
        {ok, WorkflowSchedule}
    end.

%
% Run Workflow
% 
run_scheduled(#workflow_schedule{step_process=Process} = Schedule, Wallet) ->
    maybe 
        {ok, Activity} ?= create_workflow_activity(Schedule, Wallet),
        {ok, Result} ?= hb_ao:resolve(Process, ~"now/result", #{}),
        update_workflow_activity(Activity, Result)
    end.

create_workflow_activity(#workflow_schedule{id=ScheduleId, step_name=StepName}, Wallet) ->
    % Create a new workflow activity
    Activity = #workflow_activity{
        id = timestamp_id(ScheduleId),
        schedule_id = ScheduleId,
        step_name = StepName,
        wallet_address = wallet_address(Wallet)
    },
    ok = save(Activity),
    {ok, Activity}.

update_workflow_activity(#workflow_activity{} = WorkflowActivity, Result) ->
    UpdatedActivity = WorkflowActivity#workflow_activity{
        result = Result
    },
    ok = save(UpdatedActivity),
    {ok, UpdatedActivity}.

%
% Store and load model records on KV store
%
save(#workflow{id=Id} = Workflow) -> save(~"w_", Id, Workflow);
save(#workflow_step{id=Id} = Step) -> save(~"ws_", Id, Step);
save(#workflow_schedule{id=Id} = Exec) -> save(~"wp_", Id, Exec);
save(#workflow_activity{id=Id} = Activity) -> save(~"wa_", Id, Activity).

save(Prefix, Id, Record) when is_binary(Id), is_tuple(Record) ->
    Key = <<Prefix/binary, Id/binary>>,
    Value = term_to_binary(Record),
    hb_store:write(?STORE_OPTS, Key, Value).

load_workflow(WorkflowId, Wallet) ->
    Address = wallet_address(Wallet),
    Key = <<"w_", WorkflowId/binary>>,
    maybe {ok, WorkflowBin} ?= hb_store:read(?STORE_OPTS, Key),
        case binary_to_term(WorkflowBin) of
            #workflow{wallet_address = PrevAddress} = Workflow when Address =:= PrevAddress ->
                {ok, Workflow};
            #workflow{} ->
                {error, <<"Wallet mismatch for workflow: ", WorkflowId/binary>>}
        end
    end.

load_workflow_schedule(PreparedId) ->
    Key = <<"wp_", PreparedId/binary>>,
    maybe {ok, ScheduleBin} ?= hb_store:read(?STORE_OPTS, Key),
        {ok, binary_to_term(ScheduleBin)}
    end.

load_step(WorkflowId, StepName) ->
    Key = <<"ws_", WorkflowId/binary, ":", StepName/binary>>,
    maybe {ok, StepBin} ?= hb_store:read(?STORE_OPTS, Key),
        {ok, binary_to_term(StepBin)}
    end.

load_activity(ActivityId) ->
    Key = <<"wa_", ActivityId/binary>>,
    maybe {ok, ActivityBin} ?= hb_store:read(?STORE_OPTS, Key),
        {ok, binary_to_term(ActivityBin)}
    end.

%
% Utils
%
wallet_address(Wallet) ->
    hb_util:human_id(ar_wallet:to_address(Wallet)).

find_expected_field(Field, Map) ->
    maybe 
        error ?= maps:find(Field, Map),
        {error, <<"Missing mandatory field: ", Field/binary>>}
    end.

timestamp_id(Id) ->
    Timestamp = integer_to_binary(os:system_time()),
    <<Id/binary, ":", Timestamp/binary>>.

%% Depth-first search with cycle detection
dfs_traverse(Step, Transitions, Visited, Path) ->
    case lists:member(Step, Path) of
        true ->
            {error, {cycle_detected, Step}};
        false ->
            case sets:is_element(Step, Visited) of
                true ->
                    {ok, sets:to_list(Visited)};
                false ->
                    NewVisited = sets:add_element(Step, Visited),
                    NewPath = [Step | Path],
                    
                    case maps:get(Step, Transitions, #{}) of
                        StepTransitions when map_size(StepTransitions) =:= 0 ->
                            % No outgoing transitions - end step
                            {ok, sets:to_list(NewVisited)};
                        StepTransitions ->
                            NextSteps = maps:values(StepTransitions),
                            dfs_all_steps(NextSteps, Transitions, NewVisited, NewPath)
                    end
            end
    end.

%% Visit all next steps in DFS manner
dfs_all_steps([], _Transitions, Visited, _Path) ->
    {ok, sets:to_list(Visited)};
dfs_all_steps([NextStep | RestSteps], Transitions, Visited, Path) ->
    case dfs_traverse(NextStep, Transitions, Visited, Path) of
        {ok, NewVisited} ->
            VisitedSet = sets:from_list(NewVisited),
            dfs_all_steps(RestSteps, Transitions, VisitedSet, Path);
        {error, Reason} ->
            {error, Reason}
    end.
