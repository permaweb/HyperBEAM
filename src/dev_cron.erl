%%% @doc A device that inserts new messages into the schedule to allow processes
%%% to passively 'call' themselves without user interaction.
-module(dev_cron).
-export([list/3, once/3, every/3, stop/3, info/1, info/3]).
-export([normalize/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Exported function for getting device info.
info(_) -> 
	#{ exports => [info, once, every, stop, add, list, normalize] }.

%% @doc Exported function for granting a description of the device.
info(_Msg1, _Msg2, _Opts) ->
	InfoBody = #{
		<<"description">> => <<"Cron device for scheduling messages">>,
		<<"version">> => <<"1.0">>,
		<<"paths">> => #{
			<<"info">> => <<"Get device info">>,
			<<"once">> => <<"Schedule a one-time message">>,
			<<"every">> => <<"Schedule a recurring message">>,
			<<"stop">> => <<"Stop a scheduled task {task}">>,
			<<"list">> => <<"List all registered cron tasks">>,
			<<"normalize">> => <<"Boostrap cron jobs from cache">>
		}
	},
	{ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.


%% @doc Modify a given node message to include the cron process.
cron_opts(Opts) ->
    {ok, Script} = file:read_file("scripts/cron.lua"),
    CronProcDef = #{
        <<"device">> => <<"process@1.0">>,
        <<"execution-device">> => <<"lua@5.3a">>,
        <<"function">> => <<"compute">>,
        <<"module">> => #{
            <<"content-type">> => <<"application/lua">>,
            <<"body">> => Script
        }
    },
    ?event({debug_cron_opts, {cron_process_def, CronProcDef}}),
    Opts#{
        node_processes =>
            (hb_opts:get(node_processes, #{}, Opts))#{
                <<"cron">> => CronProcDef
            }
    }.

%% @doc Normalize the cron jobs from cache by attempting to restart them.
normalize(Msg1, _Msg2, Opts) ->
	?event({normalize_cron_jobs_start}),
    CronOpts = cron_opts(Opts),
    case cache_list(CronOpts) of
		{ok, Crons} -> 
			?event({normalize_cron_jobs_loaded, {count, length(Crons)}}),
            {Successes, Errors} = process_cached_jobs(Crons, Msg1, CronOpts),
            NumSuccess = length(Successes),
            ErrorDetails = Errors,
            ?event({normalize_cron_jobs_finished, 
				{success_or_existing, NumSuccess}, 
				{errors, length(ErrorDetails)}, 
				{error_details, ErrorDetails}}),
			{ok, #{
				<<"restarted_or_existing">> => NumSuccess, 
				<<"errors">> => ErrorDetails
			}};
		{error, Reason} ->
			 ?event({normalize_cron_jobs_load_error, {error, Reason}}),
             {error, Reason}
	end.

%% Helper function to process the list of cached cron jobs
process_cached_jobs(Crons, Msg1, CronOpts) ->
    Results = lists:map(
        fun(Job) -> 
            process_single_job(Job, Msg1, CronOpts)
        end, Crons
    ),
    lists:partition(
		fun({ok, _}) -> true; 
		(_) -> false end, 
		Results).

%% Helper function to process a single cached job entry
process_single_job(Job, Msg1, CronOpts) ->
    case extract_job_details(Job) of
        {ok, DetailsMap} ->
            case reconstruct_original_msg(DetailsMap) of
                {ok, OriginalMsg} ->
                    attempt_job_restart(DetailsMap, OriginalMsg, Msg1, CronOpts);
                {error, Reason} ->
                    TaskId = maps:get(task_id, DetailsMap, unknown),
                    {error, {TaskId, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Helper function to extract and validate details from a raw cache job entry
extract_job_details(Job) ->
    Body = maps:get(<<"body">>, Job, #{}),
    TaskId = maps:get(<<"task_id">>, Body, undefined),
    AugmentedData = maps:get(<<"data">>, Body, #{}),
    Type = maps:get(<<"type">>, AugmentedData, undefined),
    Interval = maps:get(<<"interval">>, AugmentedData, undefined),
    WorkerMsg = maps:get(<<"worker_msg">>, AugmentedData, #{}),
    if TaskId == undefined orelse Type == undefined orelse not is_map(WorkerMsg) orelse WorkerMsg == #{} ->
        ?event({normalize_skipping_invalid_job, {reason, invalid_structure}, {raw_job, Job}}),
        {error, {unknown_task_id, invalid_job_structure}};
       true ->
            {ok, #{ task_id => TaskId, type => Type, interval => Interval, worker_msg => WorkerMsg }}
    end.

%% Helper function to reconstruct the original message for once/every from cached details
reconstruct_original_msg(DetailsMap) ->
    #{ task_id := TaskId, type := Type, interval := Interval, worker_msg := WorkerMsg } = DetailsMap,
    TargetPath = maps:get(<<"path">>, WorkerMsg, undefined),
    if TargetPath == undefined ->
        ?event({normalize_skipping_no_target_path, {task_id, TaskId}, {worker_msg, WorkerMsg}}),
        {error, no_target_path};
       true ->
            OtherParams = maps:remove(<<"path">>, WorkerMsg),
            OriginalMsg0 = maps:put(<<"cron-path">>, TargetPath, OtherParams),
            OriginalMsg1 = case Type of
                               <<"every">> when Interval =/= null andalso Interval =/= undefined -> 
                                   maps:put(<<"interval">>, Interval, OriginalMsg0);
                               _ -> OriginalMsg0
                           end,
            OriginalMsgPath = <<"/~cron@1.0/", Type/binary>>,
            OriginalMsg = maps:put(<<"path">>, OriginalMsgPath, OriginalMsg1),
            {ok, OriginalMsg}
    end.

%% Helper function to attempt the restart of a single job
attempt_job_restart(DetailsMap, OriginalMsg, Msg1, CronOpts) ->
    #{ task_id := TaskId, type := Type } = DetailsMap,
    %?event({normalize_attempting_restart, {task_id, TaskId}, {type, Type}, {original_msg_preview, OriginalMsg#{ 
    %     commitments => commitments
    % }}}),
    case Type of
        <<"once">> -> 
            case once(Msg1, OriginalMsg, CronOpts) of
                {ok, ReturnedId} -> 
                     % Success if original ID matches OR a new ID was returned (idempotency handled)
                    ?event({normalize_once_success, {task_id, TaskId}, {returned_id, ReturnedId}}),
                    {ok, TaskId};
                Error -> 
                    ?event({normalize_once_error, {task_id, TaskId}, {error, Error}}),
                    {error, {TaskId, Error}}
            end;
        <<"every">> -> 
            case every(Msg1, OriginalMsg, CronOpts) of
                 {ok, ReturnedId} -> 
                     % Success if original ID matches OR a new ID was returned (idempotency handled)
                    ?event({normalize_every_success, {task_id, TaskId}, {returned_id, ReturnedId}}),
                    {ok, TaskId};
                Error -> 
                    ?event({normalize_every_error, {task_id, TaskId}, {error, Error}}),
                    {error, {TaskId, Error}}
            end;
        _ ->
            ?event({normalize_unknown_type, {task_id, TaskId}, {type, Type}}),
            {error, {TaskId, unknown_type}}
    end.

%% @doc List all registered cron tasks.
list(_, _, Opts) ->
    CronOpts = cron_opts(Opts),
    ?event({list_cron_jobs_start, {cron_opts, CronOpts}}),
    case cache_list(CronOpts) of
        {ok, Crons} ->
            ?event({list_cron_jobs_success, {crons, Crons}}),
            {ok, #{<<"crons">> => Crons}};
        {error, Reason} ->
            ?event({list_cron_jobs_error, {error, Reason}}),
            {error, Reason}
    end.


%% @doc Exported function for scheduling a one-time message.
once(_Msg1, Msg2, Opts) ->
	case hb_ao:get(<<"cron-path">>, Msg2, Opts) of
		not_found ->
			{error, <<"No cron path found in message.">>};
		CronPath ->
			ReqMsgID = hb_message:id(Msg2, all),
			% make the path specific for the end device to be used
			ModifiedMsg2 =
                maps:remove(
                    <<"cron-path">>,
                    maps:put(<<"path">>, CronPath, Msg2)
                ),
			Name = {<<"cron@1.0">>, ReqMsgID},
			case hb_name:lookup(Name) of
				Pid when is_pid(Pid) ->
					% Process already registered, return existing TaskID
					?event({once_already_registered, {task_id, ReqMsgID}, {pid, Pid}}),
					{ok, ReqMsgID};
				undefined ->
					% Process not found, spawn and register
                    Pid = spawn(fun() -> once_worker(CronPath, ModifiedMsg2, Opts) end),
					hb_name:register(Name, Pid),
					% Define the data structure for the cache
					MinimalAugmentedData = #{ 
						<<"type">> => <<"once">>,
						<<"interval">> => null, % No interval for 'once'
						<<"worker_msg">> => ModifiedMsg2 
					},
					% Cache the augmented task data
					{ok, PutResult} = cache_put(ReqMsgID, MinimalAugmentedData, cron_opts(Opts)),
					?event({once_cache_put_result, {result, PutResult}}),
					% {ok, Crons} = cache_list(cron_opts(Opts)),
					% ?event({once_cron_cache_load_test_crons, {crons, Crons}}),
					{ok, ReqMsgID}
			end
	end.

%% @doc Internal function for scheduling a one-time message.
once_worker(Path, Req, Opts) ->
    % Directly call the meta device on the newly constructed 'singleton', just
    % as hb_http_server does.
    TracePID = hb_tracer:start_trace(),
    try
        dev_meta:handle(Opts#{ trace => TracePID }, Req#{ <<"path">> => Path})
    catch
        %% Cowboy / Ranch listener is gone (node stopped) – exit quietly
        error:badarg ->
            exit(normal);
        Class:Reason:Stacktrace ->
            ?event(
                {cron_once_worker_error,
                    {path, Path},
                    {error, Class, Reason, Stacktrace}
                }
            ),
            exit({Class, Reason, Stacktrace})
    end.


%% @doc Exported function for scheduling a recurring message.
every(_Msg1, Msg2, Opts) ->
	case {
		hb_ao:get(<<"cron-path">>, Msg2, Opts),
		hb_ao:get(<<"interval">>, Msg2, Opts)
	} of
		{not_found, _} -> 
			{error, <<"No cron path found in message.">>};
		{_, not_found} ->
			{error, <<"No interval found in message.">>};
		{CronPath, IntervalString} -> 
			try 
				IntervalMillis = parse_time(IntervalString),
				if IntervalMillis =< 0 ->
					throw({error, invalid_interval_value});
				true ->
					ok
				end,
				ReqMsgID = hb_message:id(Msg2, all),
				ModifiedMsg2 =
                    maps:remove(
                        <<"cron-path">>,
                        maps:remove(<<"interval">>, Msg2)
                    ),
				TracePID = hb_tracer:start_trace(),
				Name = {<<"cron@1.0">>, ReqMsgID},
				case hb_name:lookup(Name) of
					Pid when is_pid(Pid) ->
						% Process already registered, return existing TaskID
						?event({every_already_registered, {task_id, ReqMsgID}, {pid, Pid}}),
						{ok, ReqMsgID};
					undefined ->
						% Process not found, spawn and register
						Pid =
		                    spawn(
		                        fun() ->
		                            every_worker_loop(
		                                CronPath,
		                                ModifiedMsg2,
		                                Opts#{ trace => TracePID },
		                                IntervalMillis
		                            )
		                        end
		                    ),
						hb_name:register(Name, Pid),
						% Define the data structure for the cache
						MinimalAugmentedData = #{ 
							<<"type">> => <<"every">>,
							<<"interval">> => IntervalString,
							<<"worker_msg">> => ModifiedMsg2 
						},
						% Cache the augmented task data
						{ok, PutResult} = cache_put(ReqMsgID, MinimalAugmentedData, cron_opts(Opts)),
						?event({every_cache_put_result, {result, PutResult}}),
						{ok, ReqMsgID}
				end
			catch
				error:{invalid_time_unit, Unit} ->
                    {error, <<"Invalid time unit: ", Unit/binary>>};
				error:{invalid_interval_value} ->
                    {error, <<"Invalid interval value.">>};
				error:{Reason, _Stack} ->
					{error, {<<"Error parsing interval">>, Reason}}
			end
	end.
every_worker_loop(CronPath, Req, Opts, IntervalMillis) ->
	ReqSingleton = Req#{ <<"path">> => CronPath },
	?event(
        {cron_every_worker_executing,
            {path, CronPath},
            {req_id, hb_message:id(Req, all)}
        }
    ),
	try
		Result = dev_meta:handle(Opts, ReqSingleton),
		?event({cron_every_worker_executed, {path, CronPath}, {result, Result}})
	catch
        error:badarg ->
            exit(normal);
		Class:Reason:Stacktrace ->
			?event(
                {cron_every_worker_error,
                    {path, CronPath},
                    {error, Class, Reason, Stacktrace}
                }
            ),
			every_worker_loop(CronPath, Req, Opts, IntervalMillis)
	end,
	timer:sleep(IntervalMillis),
	every_worker_loop(CronPath, Req, Opts, IntervalMillis).


%% @doc Exported function for stopping a scheduled task.
stop(_Msg1, Msg2, Opts) ->
	case hb_ao:get(<<"task">>, Msg2, Opts) of
		not_found ->
			{error, <<"No task ID found in message.">>};
		TaskID ->
			Name = {<<"cron@1.0">>, TaskID},
			case hb_name:lookup(Name) of
				Pid when is_pid(Pid) ->
					?event({cron_stopping_task, {task_id, TaskID}, {pid, Pid}}),
					exit(Pid, kill),
					hb_name:unregister(Name),
					{ok, RemoveResult} = cache_remove(TaskID, cron_opts(Opts)),
					?event({stop_cache_remove_result, {result, RemoveResult}}),
					{ok, #{<<"status">> => 200, <<"body">> => #{
						<<"message">> => <<"Task stopped successfully">>,
						<<"task_id">> => TaskID
					}}};
				undefined ->
					{error, <<"Task not found.">>};
				Error ->
					?event({cron_stop_lookup_error, {task_id, TaskID}, {error, Error}}),
					{error, #{
                        <<"error">> =>
                            <<"Failed to lookup task or unexpected result">>,
                            <<"details">> => Error
                    }}
			end
	end.

%% @doc Parse a time string into milliseconds.
parse_time(BinString) ->
	[AmountStr, UnitStr] = binary:split(BinString, <<"-">>),
	Amount = binary_to_integer(AmountStr),
	Unit = string:lowercase(binary_to_list(UnitStr)),
	case Unit of
		"millisecond" ++ _ -> Amount;
		"second" ++ _ -> Amount * 1000;
		"minute" ++ _ -> Amount * 60 * 1000;
		"hour" ++ _ -> Amount * 60 * 60 * 1000;
		"day" ++ _ -> Amount * 24 * 60 * 60 * 1000;
		_ -> throw({error, invalid_time_unit, UnitStr})
	end.


%% Cache functions and helpers
%% @doc Helper function to send commands to the cron process
send_cron_command(Command, Body, Opts) ->
    SampleMsg = [
        #{ <<"device">> => <<"node-process@1.0">> },
        <<"cron">>,
        #{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    #{
                        <<"path">> => <<"compute">>,
                        <<"body">> => Body#{<<"path">> => Command}
                    },
                    Opts
                )
        }
    ],
    Result = hb_ao:resolve_many(SampleMsg, Opts),
    ?event({cron_command_result, {command, Command}, {result, Result}}),
    Result.

%% @doc Put a value in the cache with the specified task ID
%% Returns {ok, TaskId} on success
cache_put(TaskId, AugmentedData, Opts) ->
	?event({cache_put_start, {task_id, TaskId}, {augmented_data, AugmentedData}}),
    send_cron_command(
		<<"put">>,
		#{ 
			<<"task_id">> => TaskId,
        	<<"data">> => AugmentedData % Pass the augmented data map
    	},
		Opts
	),
	{ok, TaskId}.

%% @doc Get a value from the cache by task ID
%% Returns {ok, Value} if found, {error, not_found} otherwise
cache_get(TaskId, Opts) ->
    AllJobs = hb_ao:get(
        << "cron/now/crons" >>,
        #{ <<"device">> => <<"node-process@1.0">> },
        Opts
    ),
    find_job_by_task_id(AllJobs, TaskId).

%% @doc Remove a value from the cache by task ID
%% Returns {ok, removed} if successful
cache_remove(TaskId, Opts) ->
    send_cron_command(
		<<"remove">>, 
		#{
        	<<"task_id">> => TaskId
    	}, 
	Opts),
{ok, removed}.

cache_list(Opts) ->
	?event({cache_list_start}),
	SampleMsg = [
		#{ <<"device">> => <<"node-process@1.0">> },
		<<"cron">>,
		#{ 
			<<"path">> => <<"now/crons">>, 
			<<"method">> => <<"GET">> 
		}
	],
	{ok, Result} = hb_ao:resolve_many(SampleMsg, Opts),
	CronData = hb_ao:get(<<"crons">>, Result, #{}),
	?event({cache_list_cron_data, {cron_data, CronData}}),
    case is_list(CronData) of
        true -> {ok, CronData};
        false -> {error, CronData}
    end.

%% @doc Clear all values from the cache
%% Returns {ok, cleared} on success
cache_clear(Opts) ->
    send_cron_command(<<"clear">>, #{}, Opts),
    {ok, cleared}.

%% @doc Helper function to find a job by task ID in a list of jobs
find_job_by_task_id([], _) ->
    {error, not_found};

find_job_by_task_id([Job|Rest], TaskId) ->
	?event({find_job_by_task_id, {job, Job}, {task_id, TaskId}}),
    case hb_ao:get(<<"body/task_id">>, Job, #{}) of
        TaskId -> 
            JobData = hb_ao:get(<<"body/data">>, Job, #{}),
            ?event({found_job_by_task_id_job_data, {job_data, JobData}}),
            {ok, JobData};
        _ -> find_job_by_task_id(Rest, TaskId)
    end.


%%%
%%% Cron device tests
%%%
% Helper functions to generate test opts
generate_test_opts() ->
	{ok, Script} = file:read_file("scripts/cron.lua"),
	generate_test_opts(#{
		<<"cron">> => #{
			<<"device">> => <<"process@1.0">>,
			<<"execution-device">> => <<"lua@5.3a">>,
			<<"scheduler-device">> => <<"scheduler@1.0">>,
			<<"module">> => #{
				<<"content-type">> => <<"application/lua">>,
				<<"body">> => Script
			}
		}
	}).
generate_test_opts(Defs) ->
#{
	store =>
		[
			#{
				<<"store-module">> => hb_store_fs,
				<<"prefix">> =>
					<<
						"cache-TEST-",
						(integer_to_binary(os:system_time(millisecond)))/binary
					>>
			}
		],
	node_processes => Defs,
	priv_wallet => ar_wallet:new()
}.

%% @doc This test verifies that a one-time task can be stopped by
%% calling the stop function with the task ID.
stop_once_test() ->
	Opts = generate_test_opts(),
	% Start a new node
	Node = hb_http_server:start_node(Opts),
	% Set up a standard test worker (even though delay doesn't use its state)
	TestWorkerPid = spawn(fun test_worker/0),
	TestWorkerNameId = hb_util:human_id(crypto:strong_rand_bytes(32)),
	hb_name:register({<<"test">>, TestWorkerNameId}, TestWorkerPid),
	% Create a "once" task targeting the delay function
	OnceUrlPath = <<"/~cron@1.0/once?test-id=", TestWorkerNameId/binary,
				 "&cron-path=/~test-device@1.0/delay">>,
	{ok, OnceTaskID} = hb_http:get(Node, OnceUrlPath, #{}),
	?event({'cron:stop_once:test:created', {task_id, OnceTaskID}}),
	% Give a short delay to ensure the task has started and called handle,
    % entering the sleep
	timer:sleep(200),
	% Verify the once task worker process is registered and alive
	OncePid = hb_name:lookup({<<"cron@1.0">>, OnceTaskID}),
	?assert(is_pid(OncePid), "Lookup did not return a PID"),
	?assert(erlang:is_process_alive(OncePid), "OnceWorker process died prematurely"),
	% Call stop on the once task while it's sleeping
	OnceStopPath = <<"/~cron@1.0/stop?task=", OnceTaskID/binary>>,
	{ok, OnceStopResult} = hb_http:get(Node, OnceStopPath, #{}),
	?event({'cron:stop_once:test:stopped', {result, OnceStopResult}}),
	% Verify success response from stop
	?assertMatch(#{<<"status">> := 200}, OnceStopResult),
	% Verify name is unregistered
	?assertEqual(undefined, hb_name:lookup({<<"cron@1.0">>, OnceTaskID})),
	% Allow a moment for the kill signal to be processed
	timer:sleep(100),
	% Verify process termination
	?assertNot(erlang:is_process_alive(OncePid), "Process not killed by stop"),
	% Call stop again to verify 404 response
	{error, <<"Task not found.">>} = hb_http:get(Node, OnceStopPath, #{}).


%% @doc This test verifies that a recurring task can be stopped by
%% calling the stop function with the task ID.
stop_every_test() ->
	Opts = generate_test_opts(),
	% Start a new node
	Node = hb_http_server:start_node(Opts),
	% Set up a test worker process to hold state (counter)
	TestWorkerPid = spawn(fun test_worker/0),
	TestWorkerNameId = hb_util:human_id(crypto:strong_rand_bytes(32)),
	hb_name:register({<<"test">>, TestWorkerNameId}, TestWorkerPid),
	% Create an "every" task that calls the test worker
	EveryUrlPath = <<"/~cron@1.0/every?test-id=", TestWorkerNameId/binary, 
				   "&interval=500-milliseconds",
				   "&cron-path=/~test-device@1.0/increment_counter">>,
	{ok, CronTaskID} = hb_http:get(Node, EveryUrlPath, #{}),
	?event({'cron:stop_every:test:created', {task_id, CronTaskID}}),
	% Verify the cron worker process was registered and is alive
	CronWorkerPid = hb_name:lookup({<<"cron@1.0">>, CronTaskID}),
	?assert(is_pid(CronWorkerPid)),
	?assert(erlang:is_process_alive(CronWorkerPid)),
	% Wait a bit to ensure the cron worker has run a few times
	timer:sleep(1000),
	% Call stop on the cron task using its ID
	EveryStopPath = <<"/~cron@1.0/stop?task=", CronTaskID/binary>>,
	{ok, EveryStopResult} = hb_http:get(Node, EveryStopPath, #{}),
	?event({'cron:stop_every:test:stopped', {result, EveryStopResult}}),
	% Verify success response
	?assertMatch(#{<<"status">> := 200}, EveryStopResult),
	% Verify the cron task name is unregistered (lookup returns undefined)
	?assertEqual(undefined, hb_name:lookup({<<"cron@1.0">>, CronTaskID})),
	% Allow a moment for the process termination signal to be processed
	timer:sleep(100),
	% Verify the cron worker process is terminated
	?assertNot(erlang:is_process_alive(CronWorkerPid)),
	% Check the counter in the original test worker was incremented
	TestWorkerPid ! {get, self()},
	receive
		{state, State = #{count := Count}} ->
			?event({'cron:stop_every:test:counter_state', {state, State}}),
			?assert(Count > 0)
	after 1000 ->
		throw(no_response_from_worker)
	end,
	% Call stop again using the same CronTaskID to verify the error
	{error, <<"Task not found.">>} = hb_http:get(Node, EveryStopPath, #{}).


%% @doc This test verifies that a one-time task can be scheduled and executed.
once_executed_test() ->
	Opts = generate_test_opts(),
	% start a new node 
	Node = hb_http_server:start_node(Opts),
	% spawn a worker on the new node that calls test_worker/0 which inits
    % test_worker/1 with a state of undefined
	PID = spawn(fun test_worker/0),
	% generate a random id that we can then use later to lookup the worker
	ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
	% register the worker with the id
	hb_name:register({<<"test">>, ID}, PID),
	% Construct the URL path with the dynamic ID
	UrlPath = <<"/~cron@1.0/once?test-id=", ID/binary,
			"&cron-path=/~test-device@1.0/update_state">>,
	% this should call the worker via the test device
	% the test device should look up the worker via the id given 
	{ok, ReqMsgId1} = hb_http:get(Node, UrlPath, #{}),
	% Call again to test idempotency
	{ok, ReqMsgId2} = hb_http:get(Node, UrlPath, #{}),
	?assertEqual(ReqMsgId1, ReqMsgId2, "Second call should return the same Task ID"),
	% wait for the request to be processed
	timer:sleep(1000),
	% send a message to the worker to get the state
	PID ! {get, self()},
	% receive the state from the worker
	receive
		{state, State} ->
			?event({once_executed_test_received_state, {state, State}}),
			?assertMatch(#{ <<"test-id">> := ID }, State)
	after 1000 ->
		FinalLookup = hb_name:lookup({<<"test">>, ID}),
		?event({timeout_waiting_for_worker, {pid, PID}, {lookup_result, FinalLookup}}),
		throw(no_response_from_worker)
	end.

%% @doc This test verifies that a recurring task can be scheduled and executed.
every_worker_loop_test() ->
	Opts = generate_test_opts(),
	Node = hb_http_server:start_node(Opts),
	PID = spawn(fun test_worker/0),
	ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
	hb_name:register({<<"test">>, ID}, PID),
	UrlPath = <<"/~cron@1.0/every?test-id=", ID/binary, 
		"&interval=500-milliseconds",
		"&cron-path=/~test-device@1.0/increment_counter">>,
	?event({'cron:every:test:sendUrl', {url_path, UrlPath}}),
	{ok, ReqMsgId1} = hb_http:get(Node, UrlPath, #{}),
	?event({'cron:every:test:get_done', {req_id, ReqMsgId1}}),
	% Call again to test idempotency
	{ok, ReqMsgId2} = hb_http:get(Node, UrlPath, #{}),
	?assertEqual(ReqMsgId1, ReqMsgId2, "Second call should return the same Task ID"),
	?event({'cron:every:test:idempotency_check_done', {req_id, ReqMsgId2}}),
	timer:sleep(1500),
	PID ! {get, self()},
	% receive the state from the worker
	receive
		{state, State = #{count := C}} ->
			?event({'cron:every:test:received_state', {state, State}}),
			?assert(C >= 3)
	after 1000 ->
		FinalLookup = hb_name:lookup({<<"test">>, ID}),
		?event({'cron:every:test:timeout', {pid, PID}, {lookup_result, FinalLookup}}),
		throw({test_timeout_waiting_for_state, {id, ID}})
	end.

%% @doc Test that verifies a one-time job is added to 
%% the cron cache and can be loaded via the load endpoint
once_cron_cache_list_test() ->
    Opts = generate_test_opts(),
    % Start a new node with test options
    Node = hb_http_server:start_node(Opts),
    % Create a test worker
    PID = spawn(fun test_worker/0),
    ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    hb_name:register({<<"test">>, ID}, PID),
    % Create a "once" task
    UrlPath = <<"/~cron@1.0/once?test-id=", ID/binary,
              "&cron-path=/~test-device@1.0/update_state">>,
    {ok, ReqMsgId} = hb_http:get(Node, UrlPath, #{}),
    ?event({once_load_test_created, {task_id, ReqMsgId}}),
    % Wait for the task to be processed
    timer:sleep(1000),
	% Use the cache list function to retrieve cron jobs
	{ok, Crons} = cache_list(Opts),
	?event({once_cron_cache_load_test_crons, {crons, Crons}}),
    % % Use the load function to retrieve cron jobs
	{ok, LoadedCrons} = hb_ao:resolve_many(
        hb_singleton:from(#{
            <<"path">> => <<"/~cron@1.0/list">>
        }),
        Opts
    ),
    ?event({once_load_test_loaded, {loaded_crons, LoadedCrons}}),
    % Extract the list of cron jobs from the result map
    CronsList = hb_ao:get(<<"crons">>, LoadedCrons, #{}),
    ?event({once_load_test_extracted, {crons_list, CronsList}}),
    % Verify the task is in the loaded list
    Res = find_job_by_task_id(CronsList, ReqMsgId),
    ?event({once_load_test_job, {res, Res}}),
    ?assertMatch({ok, _}, Res),
    ?event({'once_load_test_done'}).

%% @doc Test that verifies a recurring job is added to
%% the cron cache and can be loaded via cache_list.
every_cron_cache_list_test() ->
    Opts = generate_test_opts(),
    % Start a new node with test options
    Node = hb_http_server:start_node(Opts),
    % Create a test worker
    PID = spawn(fun test_worker/0),
    ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    hb_name:register({<<"test">>, ID}, PID),
    % Create an "every" task
    UrlPath = <<"/~cron@1.0/every?test-id=", ID/binary,
              "&interval=500-milliseconds", % Specify interval
              "&cron-path=/~test-device@1.0/increment_counter">>, % Specify target path
    {ok, ReqMsgId} = hb_http:get(Node, UrlPath, #{}),
    ?event({every_load_test_created, {task_id, ReqMsgId}}),
    % Wait for the task to be processed and added to the cache
    timer:sleep(100), % Short sleep just to ensure cache_put completes
    % Use the cache list function to retrieve cron jobs
    {ok, Crons} = cache_list(Opts),
    ?event({every_cron_cache_list_test_crons, {crons, Crons}}),
    % Verify the task is in the loaded list
    ?assertMatch({ok, _}, find_job_by_task_id(Crons, ReqMsgId)),
    ?event({'every_load_test_done'}).

cron_device_load_test() ->
	% This test checks whether the cron load function returns 
	% the correct data
	Opts = generate_test_opts(),
	Node = hb_http_server:start_node(Opts),
	TaskId = <<"load-test-task-id">>,
	TestData = #{<<"key">> => <<"value">>, <<"timestamp">> => os:system_time(millisecond)},
	{ok, _PutResult} = cache_put(TaskId, TestData, Opts),
	TaskId2 = <<"load-test-task-id-2">>,
	TestData2 = #{<<"key">> => <<"value-2">>, <<"timestamp">> => os:system_time(millisecond)},
	{ok, _PutResult2} = cache_put(TaskId2, TestData2, Opts),
	UrlPath = <<"/~cron@1.0/list">>,
	{ok, LoadedCrons} = hb_http:get(Node, UrlPath, #{}),
	CronsList = hb_ao:get(<<"crons">>, LoadedCrons, #{}),
	{ok, RetrievedData} = find_job_by_task_id(CronsList, TaskId),
	{ok, RetrievedData2} = find_job_by_task_id(CronsList, TaskId2),
	CleanedData = maps:remove(<<"priv">>, RetrievedData),
	CleanedData2 = maps:remove(<<"priv">>, RetrievedData2),
	?assertEqual(TestData, CleanedData),
	?assertEqual(TestData2, CleanedData2),
	?event({'cache_load_test_done'}).

%% @doc This is a helper function that is used to test the cron device.
%% It is used to increment a counter and update the state of the worker.
test_worker() -> test_worker(#{count => 0}).
test_worker(State) ->
	receive
		{increment} ->
            % ensure that count is defined in the case that a full message
            % is already in the state.
			Current = case State of
                #{count := N} -> N;
                _             -> 0
            end,
            NewCount = Current + 1,
			?event({'test_worker:incremented', {new_count, NewCount}}),
			NewState = maps:put(count, NewCount, State),
			test_worker(NewState);
		{update, NewState} ->
			 ?event({'test_worker:updated', {new_state, NewState}}),
			 test_worker(NewState);
		{get, Pid} ->
			Pid ! {state, State},
			test_worker(State);
        _Other ->
            test_worker(State)
	end.


% Cache test framework for cron.lua via the node-process devices
cache_put_test() ->
	Opts = generate_test_opts(),
	TaskId = <<"test-task-id">>,
	TestData = #{<<"key">> => <<"value">>, <<"timestamp">> => os:system_time(millisecond)},
	% Put the data in the cache
	{ok, PutResult} = cache_put(TaskId, TestData, Opts),
	?event({cache_put_test_result, {task_id, TaskId}, {result, PutResult}}),
	?assertEqual(TaskId, PutResult),
	timer:sleep(100),
	% Verify the data was stored by retrieving the crons list
	StoredData = hb_ao:get(
		<<"cron/now/crons">>,
		#{ <<"device">> => <<"node-process@1.0">> },
		Opts
	),
	?event({cache_put_test_stored_data, {stored_data, StoredData}}),
	% % Verify we can find our task in the stored data
	{ok, RetrievedData} = find_job_by_task_id(StoredData, TaskId),
	CleanedData = maps:remove(<<"priv">>, RetrievedData),
	?event({cache_put_test_retrieved, {retrieved_data, CleanedData}}),
	?assertEqual(TestData, CleanedData),
	?event({'cache_put_test_done'}).

%% @doc Test the cache_remove function
cache_remove_test() ->
    Opts = generate_test_opts(),
    TaskId = <<"test-remove-task-id">>,
    TestData = #{<<"key">> => <<"value-to-remove">>, <<"timestamp">> => os:system_time(millisecond)},
    % First put the data in the cache
    {ok, PutResult} = cache_put(TaskId, TestData, Opts),
    ?event({cache_remove_test_put, {task_id, TaskId}, {result, PutResult}}),
    ?assertEqual(TaskId, PutResult),
    timer:sleep(100),
    % Verify the data was stored
    StoredData = hb_ao:get(
        <<"cron/now/crons">>,
        #{ <<"device">> => <<"node-process@1.0">> },
        Opts
    ),
    ?event({cache_remove_test_stored, {stored_data, StoredData}}),
    % Verify we can find our task
    {ok, RetrievedData} = find_job_by_task_id(StoredData, TaskId),
    ?event({cache_remove_test_retrieved, {retrieved_data, RetrievedData}}),
    CleanedData = maps:remove(<<"priv">>, RetrievedData),
    ?assertEqual(TestData, CleanedData),
    % Now remove the data
    {ok, RemoveResult} = cache_remove(TaskId, Opts),
    ?event({cache_remove_test_remove, {result, RemoveResult}}),
    ?assertEqual(removed, RemoveResult),
    timer:sleep(100),
    % Verify the data was removed
    UpdatedData = hb_ao:get(
        <<"cron/now/crons">>,
        #{ <<"device">> => <<"node-process@1.0">> },
        Opts
    ),
    ?event({cache_remove_test_after, {updated_data, UpdatedData}}),
    % Verify the task is no longer found
    NotFoundResult = find_job_by_task_id(UpdatedData, TaskId),
    ?assertEqual({error, not_found}, NotFoundResult),
    ?event({'cache_remove_test_done'}).

%% @doc Test the cache_list function
cache_list_test() ->
    Opts = generate_test_opts(),
    % Create a few task IDs and test data
    TaskId1 = <<"test-list-task-1">>,
    TaskId2 = <<"test-list-task-2">>,
    TestData1 = #{<<"key">> => <<"value-1">>, <<"timestamp">> => os:system_time(millisecond)},
    TestData2 = #{<<"key">> => <<"value-2">>, <<"timestamp">> => os:system_time(millisecond)},
    % Clear any existing data first (for test isolation)
    {ok, _} = cache_clear(Opts),
    timer:sleep(100),
    % Put the test data in the cache
    {ok, PutResult1} = cache_put(TaskId1, TestData1, Opts),
    {ok, PutResult2} = cache_put(TaskId2, TestData2, Opts),
    ?event({cache_list_test_put, {task_id1, TaskId1}, {task_id2, TaskId2}}),
    ?assertEqual(TaskId1, PutResult1),
    ?assertEqual(TaskId2, PutResult2),
    timer:sleep(100),
    % List all values in the cache
    {ok, AllJobs} = cache_list(Opts),
    ?event({cache_list_test_all_jobs, {all_jobs, AllJobs}}),
    % Verify we can find both tasks in the list
    {ok, RetrievedData1} = find_job_by_task_id(AllJobs, TaskId1),
    {ok, RetrievedData2} = find_job_by_task_id(AllJobs, TaskId2),
    % Clean the data for comparison
    CleanedData1 = maps:remove(<<"priv">>, RetrievedData1),
    CleanedData2 = maps:remove(<<"priv">>, RetrievedData2),
    % Verify the data matches
    ?assertEqual(TestData1, CleanedData1),
    ?assertEqual(TestData2, CleanedData2),
    % Verify the list length
    ?assertEqual(2, length(AllJobs)),
    ?event({'cache_list_test_done'}).

%% @doc Test the cache_clear function
cache_clear_test() ->
    Opts = generate_test_opts(),
    % Create test data
    TaskId1 = <<"test-clear-task-1">>,
    TaskId2 = <<"test-clear-task-2">>,
    TestData1 = #{<<"key">> => <<"value-to-clear-1">>, <<"timestamp">> => os:system_time(millisecond)},
    TestData2 = #{<<"key">> => <<"value-to-clear-2">>, <<"timestamp">> => os:system_time(millisecond)},
    % Add data to the cache
    {ok, PutResult1} = cache_put(TaskId1, TestData1, Opts),
    {ok, PutResult2} = cache_put(TaskId2, TestData2, Opts),
    ?event({cache_clear_test_put, {task_id1, TaskId1}, {task_id2, TaskId2}}),
    ?assertEqual(TaskId1, PutResult1),
    ?assertEqual(TaskId2, PutResult2),
    timer:sleep(100),
    % Verify the data was stored
    {ok, AllJobsBefore} = cache_list(Opts),
    ?event({cache_clear_test_before, {all_jobs, AllJobsBefore}}),
    ?assert(length(AllJobsBefore) >= 2),
    % Clear the cache
    {ok, ClearResult} = cache_clear(Opts),
    ?event({cache_clear_test_clear, {result, ClearResult}}),
    ?assertEqual(cleared, ClearResult),
    timer:sleep(100),
    % Verify the cache is empty
    {ok, AllJobsAfter} = cache_list(Opts),
    ?event({cache_clear_test_after, {all_jobs, AllJobsAfter}}),
    ?assertEqual(0, length(AllJobsAfter)),
    ?event({'cache_clear_test_done'}).

%% @doc Test the cache_get function
cache_get_test() ->
    Opts = generate_test_opts(),
    TaskId = <<"test-get-task-id">>,
    TestData = #{<<"key">> => <<"value-to-get">>, <<"timestamp">> => os:system_time(millisecond)},
    % First put the data in the cache
    {ok, PutResult} = cache_put(TaskId, TestData, Opts),
    ?event({cache_get_test_put, {task_id, TaskId}, {result, PutResult}}),
    ?assertEqual(TaskId, PutResult),
    timer:sleep(100),
    % Get the data from the cache
    {ok, RetrievedData} = cache_get(TaskId, Opts),
    ?event({cache_get_test_retrieved, {retrieved_data, RetrievedData}}),
    % Clean the data for comparison
    CleanedData = maps:remove(<<"priv">>, RetrievedData),
    % Verify the data matches
    ?assertEqual(TestData, CleanedData),
    % Test getting a non-existent task
    NonExistentResult = cache_get(<<"non-existent-task">>, Opts),
    ?assertEqual({error, not_found}, NonExistentResult),
    ?event({'cache_get_test_done'}).

normalize_test() ->
    Opts = generate_test_opts(),
    Node = hb_http_server:start_node(Opts),
    % Setup test worker
    PID = spawn(fun test_worker/0),
    ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    hb_name:register({<<"test">>, ID}, PID),
    % Create a once job
    OnceUrlPath = <<"/~cron@1.0/once?test-id=", ID/binary,
                   "&cron-path=/~test-device@1.0/update_state">>,
    {ok, _OnceTaskId} = hb_http:get(Node, OnceUrlPath, #{}),
    ?event({'normalize_test_once_created'}),
    % Create an every job
    EveryUrlPath = <<"/~cron@1.0/every?test-id=", ID/binary,
                    "&interval=1000-milliseconds", % Use a reasonable interval
                    "&cron-path=/~test-device@1.0/increment_counter">>,
    {ok, _EveryTaskId} = hb_http:get(Node, EveryUrlPath, #{}),
    ?event({'normalize_test_every_created'}),
    timer:sleep(100), 
    NormalizeUrlPath = <<"/~cron@1.0/normalize">>,
    ?event({'normalize_test_calling_normalize', {url, NormalizeUrlPath}}),
    {ok, NormalizeResult} = hb_http:get(Node, NormalizeUrlPath, #{}),
    ?event({'normalize_test_normalize_result', {result, NormalizeResult}}),
    ?assertMatch(#{ 
        <<"status">> := 200,
        <<"restarted_or_existing">> := _,
        <<"errors">> := _
    }, NormalizeResult),
    ?event({'normalize_test_done'}).

start_hook_normalize_test() ->
	Opts = generate_test_opts(),
	Node = hb_http_server:start_node(Opts),
	?event({start_hook_normalize_test}),
	% schedule a once job
	PID = spawn(fun test_worker/0),
    ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
    hb_name:register({<<"test">>, ID}, PID),
	OnceUrlPath = <<"/~cron@1.0/once?test-id=", ID/binary,
                   "&cron-path=/~test-device@1.0/update_state">>,
    {ok, _OnceTaskId} = hb_http:get(Node, OnceUrlPath, #{}),
	?event({start_hook_normalize_test, once_created}),
	% stop the node.
	Res = hb_http_server:stop_node(Opts),
	?event({start_hook_normalize_test, node_stopped, Res}),
	HookNodeOpts = maps:merge(Opts, #{
		on => #{
			<<"start">> => #{
				<<"device">> => <<"cron@1.0">>,
				<<"path">> => <<"normalize">>,
				<<"hook">> => #{ <<"result">> => <<"ignore">> }
			}
		}
	}),
	% ?event({start_hook_normalize_test, hook_node_opts, HookNodeOpts}),
	HookNode = hb_http_server:start_node(HookNodeOpts),
	% ?event({start_hook_normalize_test, hook_node_started, HookNode}),
	timer:sleep(200),
	% check the cron cache list
	{ok, Crons} = cache_list(HookNodeOpts),
	% verify we can find the once job in the crons list after node start
	?assertMatch({ok, _}, find_job_by_task_id(Crons, _OnceTaskId)),
	?event({start_hook_normalize_test, test_end}).