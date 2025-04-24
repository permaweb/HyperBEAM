%%% @doc A device that inserts new messages into the schedule to allow processes
%%% to passively 'call' themselves without user interaction.
-module(dev_cron).
-export([load/3, once/3, every/3, stop/3, info/1, info/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Exported function for getting device info.
info(_) -> 
	#{ exports => [info, once, every, stop, add, load] }.

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
			<<"load">> => <<"Load cron jobs">>
		}
	},
	{ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.

%% @doc Modify a given node message to include the cron process.
cron_opts(Opts) ->
    {ok, Script} = file:read_file("scripts/cron.lua"),
    Opts#{
        node_processes =>
            (hb_opts:get(node_processes, #{}, Opts))#{
                <<"cron">> => #{
                    <<"device">> => <<"process@1.0">>,
                    <<"execution-device">> => <<"lua@5.3a">>,
                    <<"function">> => <<"compute">>,
                    <<"script">> => Script
                }
            }
    }.

%% @doc Reload all registered cron tasks.
load(_, _, Opts) ->
    CronOpts = cron_opts(Opts),
    Msgs =
        hb_singleton:from(
            #{
                <<"path">> => <<"/cron~node-process@1.0/now/crons">>,
                <<"method">> => <<"GET">>
            }
        ),
	?event({load_cron_jobs_start, {msgs, Msgs}, {cron_opts, CronOpts}}),
    case hb_ao:resolve_many(Msgs, CronOpts) of
        {ok, Crons} ->
            % TODO: Restore the crons from the response
			?event({load_cron_jobs_success, {crons, Crons}}),
            {ok, Crons};
        {error, Reason} ->
			?event({load_cron_jobs_error, {error, Reason}}),
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
			Pid = spawn(fun() -> once_worker(CronPath, ModifiedMsg2, Opts) end),
			hb_name:register(Name, Pid),
            Res = hb_ao:resolve_many(
                hb_singleton:from(
                    #{
                        <<"path">> => <<"/cron~node-process@1.0/schedule">>,
                        <<"method">> => <<"POST">>,
                        <<"body">> => ModifiedMsg2
                    }
                ),
                cron_opts(Opts)
            ),
			?event({once_resolve_many_result, {result, Res}}),
			{ok, ReqMsgID}
	end.

%% @doc Internal function for scheduling a one-time message.
once_worker(Path, Req, Opts) ->
	% Directly call the meta device on the newly constructed 'singleton', just
    % as hb_http_server does.
    TracePID = hb_tracer:start_trace(),
	try
		dev_meta:handle(Opts#{ trace => TracePID }, Req#{ <<"path">> => Path})
	catch
		Class:Reason:Stacktrace ->
			?event(
                {cron_every_worker_error,
                    {path, Path},
                    {error, Class, Reason, Stacktrace}
                }
            ),
			throw({error, Class, Reason, Stacktrace})
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
				Name = {<<"cron@1.0">>, ReqMsgID},
				hb_name:register(Name, Pid),
				{ok, ReqMsgID}
			catch
				error:{invalid_time_unit, Unit} ->
                    {error, <<"Invalid time unit: ", Unit/binary>>};
				error:{invalid_interval_value} ->
                    {error, <<"Invalid interval value.">>};
				error:{Reason, _Stack} ->
					{error, {<<"Error parsing interval">>, Reason}}
			end
	end.

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
                    hb_ao:resolve_many(
                        hb_singleton:from(
                            #{
                                <<"path">> => <<"/cron~node-process@1.0/schedule">>,
                                <<"method">> => <<"POST">>,
                                <<"body">> =>
                                    hb_message:commit(
                                        #{
                                            <<"action">> => <<"Stop">>,
                                            <<"task">> => TaskID
                                        },
                                        Opts
                                    )
                            }
                        ),
                        cron_opts(Opts)
                    ),
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
cache_put(TaskId, Value, Opts) ->
    send_cron_command(<<"put">>, #{
        <<"task_id">> => TaskId,
        <<"data">> => Value
    }, Opts),
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
    send_cron_command(<<"remove">>, #{
        <<"task_id">> => TaskId
    }, Opts),
    {ok, removed}.

%% @doc List all values in the cache
%% Returns {ok, Values} with a list of all entries
cache_list(Opts) ->
    AllJobs = hb_ao:get(
        << "cron/now/crons" >>,
        #{ <<"device">> => <<"node-process@1.0">> },
        Opts
    ),
    {ok, AllJobs}.

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


%%% Cron device tests

stop_once_test() ->
	% Start a new node
	Node = hb_http_server:start_node(),
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
	% Start a new node
	Node = hb_http_server:start_node(),
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
	% start a new node 
	Node = hb_http_server:start_node(),
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
	{ok, _ReqMsgId} = hb_http:get(Node, UrlPath, #{}),
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
	Node = hb_http_server:start_node(),
	PID = spawn(fun test_worker/0),
	ID = hb_util:human_id(crypto:strong_rand_bytes(32)),
	hb_name:register({<<"test">>, ID}, PID),
	UrlPath = <<"/~cron@1.0/every?test-id=", ID/binary, 
		"&interval=500-milliseconds",
		"&cron-path=/~test-device@1.0/increment_counter">>,
	?event({'cron:every:test:sendUrl', {url_path, UrlPath}}),
	{ok, ReqMsgId} = hb_http:get(Node, UrlPath, #{}),
	?event({'cron:every:test:get_done', {req_id, ReqMsgId}}),
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
	
%% @doc This is a helper function that is used to test the cron device.
%% It is used to increment a counter and update the state of the worker.
test_worker() -> test_worker(#{count => 0}).
test_worker(State) ->
	receive
		{increment} ->
			NewCount = maps:get(count, State, 0) + 1,
			?event({'test_worker:incremented', {new_count, NewCount}}),
			test_worker(State#{count := NewCount});
		{update, NewState} ->
			 ?event({'test_worker:updated', {new_state, NewState}}),
			 test_worker(NewState);
		{get, Pid} ->
			Pid ! {state, State},
			test_worker(State)
	end.


% Cache test framework for cron.lua via the node-process device
generate_test_opts() ->
    {ok, Script} = file:read_file("scripts/cron.lua"),
    generate_test_opts(#{
        <<"cron">> => #{
            <<"device">> => <<"process@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"script">> => #{
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

%% Unit tests for cache functions
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





%%%%%%%%%%%%%%% Experimental code and tests, to be cleaned up
%% working test
add_test() ->
	Opts = generate_test_opts(),
	SampleMsg = [
		#{ <<"device">> => <<"node-process@1.0">> },
		<<"cron">>,
		#{
			<<"path">> => <<"schedule">>,
			<<"method">> => <<"POST">>,
			<<"body">> =>
				hb_message:commit(
					#{
						<<"path">> => <<"computeOld">>,
						<<"body">> => #{
							<<"path">> => <<"once">>
						}
					},
					Opts
				)
		}
	],
	Res = hb_ao:resolve_many(SampleMsg, Opts),
	?event({add2_test_result, {res, Res}}),
	Res2 = hb_ao:get(
		<< "cron/now/crons" >>,
		#{ <<"device">> => <<"node-process@1.0">> },
		Opts
	),
	?event({add2_test_result2, {res2, Res2}}),
	?event({'add2_test_done'}).

%% should be an exact counterpart of add_test() but isn't currently working
add_resolve_test() ->
	Opts = generate_test_opts(),
	Msg = hb_singleton:from(
		#{
			<<"path">> => <<"/cron~node-process@1.0/schedule">>,
			<<"method">> => <<"POST">>,
			<<"body">> =>
				%hb_message:commit(
					#{
						<<"path">> => <<"compute">>,
						<<"body">> => #{
							<<"path">> => <<"once">>
						}
					}
				%	Opts
				%)
		}
	),
	?event({add_resolve_msg_singleton, {msg, Msg}}),
	Res = hb_ao:resolve_many(Msg, Opts),
	?event({add_resolve_test_result, {result, Res}}),
    % Msgs =
    %     hb_singleton:from(
    %         #{
    %             <<"path">> => <<"/cron~node-process@1.0/now/crons">>,
    %             <<"method">> => <<"GET">>
    %         }
    %     ),
    % case hb_ao:resolve_many(Msgs, Opts) of
    %     {ok, Crons} ->
    %         % TODO: Restore the crons from the response
	% 		?event({load_cron_jobs_success, {crons, Crons}}),
    %         {ok, Crons};
    %     {error, Reason} ->
	% 		?event({load_cron_jobs_error, {error, Reason}}),
    %         {error, Reason}
    % end,
	%% ?event({add_resolve_test_result2, {result2, Res2}}),
	?event({'add_resolve_test_done'}).



% basic invocation test
invoke_test() ->
	{ok, Script} = file:read_file("scripts/cron.lua"),
	Base = #{
		<<"device">> => <<"lua@5.3a">>,
		<<"script">> => #{
			<<"content-type">> => <<"application/lua">>,
			<<"body">> => Script
		},
		<<"test-value">> => 42
	},
	{ok, Result1} = hb_ao:resolve(Base, <<"hello">>, #{}),
	?event({result1, Result1}),
	?assertEqual(42, hb_ao:get(<<"test-value">>, Result1, #{})),
	?assertEqual(<<"world">>, hb_ao:get(<<"hello">>, Result1, #{})),
	{ok, Result2} =
		hb_ao:resolve(
			Base,
			#{<<"path">> => <<"hello">>, <<"name">> => <<"Alice">>},
			#{}
		),
	?event({result2, Result2}),
	?assertEqual(<<"Alice">>, hb_ao:get(<<"hello">>, Result2, #{})).

% cache handle test via the lua device
% 
cache_handle_test() ->
	{ok, Script} = file:read_file("scripts/cron.lua"),
	Base = #{
		<<"device">> => <<"lua@5.3a">>,
		<<"script">> => #{
			<<"content-type">> => <<"application/lua">>,
			<<"body">> => Script
		},
		<<"somekey">> => <<"somevalue">>
	},
	% resolve the handle function with a base body path of once
	{ok, Result1} = hb_ao:resolve(
		Base, 
		#{
			<<"path">> => <<"handle">>,
			<<"body">> => #{<<"path">> => <<"every">>}
		},
		#{}
	),
	?event({result1, Result1}),
	CronsRes = hb_ao:get(<<"crons">>, Result1, #{}),
	?event({crons, CronsRes}),
	% TODO(viksit): add asserts here tbd
	% end of test
	?event({'cache_handle_test_done'}).
	
%%%%%%%%%%%%%%%
% add_test() ->
% 	{ok, Script} = file:read_file("scripts/cron.lua"),
% 	Node = hb_http_server:start_node(Opts = #{
%         priv_wallet => ar_wallet:new(),
%         node_processes => #{
%             <<"cron">> => #{
%                 <<"device">> => <<"process@1.0">>,
%                 <<"execution-device">> => <<"lua@5.3a">>,
%                 <<"scheduler-device">> => <<"scheduler@1.0">>,
%                 <<"script">> => #{
%                     <<"content-type">> => <<"application/lua">>,
%                     <<"module">> => <<"cron">>,
%                     <<"body">> => Script
%                 }
%             }
%         }
%     }),
% 	% ?event({add_test_node, {node, Node}}),
% 	% Res = hb_http:get(Node, <<"/meta@1.0/info">>, #{}),
% 	% ?event({add_test_node_info, {res, Res}}),
% 	ResPost = hb_http:post(
% 		Node, 
% 		#{
% 			<<"path">> => <<"/cron~node-process@1.0/schedule">>,
% 			<<"method">> => <<"POST">>,
% 			<<"body">> => hb_message:commit(#{
% 				<<"path">> => <<"once">>,
% 				% some meta data to send later
% 				<<"task">> => <<"foobar">>
% 			},
% 			Opts)
% 		},
% 		Opts
% 	),
% 	?event({'cron:add:test:post_done', {res, ResPost}}),
% 	ResGet = hb_http:get(Node, <<"/cron~node-process@1.0/now">>, #{}),
% 	?event({'cron:add:test:get_cron', {req_id, ResGet}}),
	
% 	% Res = hb_http:get(Node, <<"/cron~node-process@1.0/now/crons">>, #{}),
% 	% ?event({add_test_get_crons, {res, Res}}),

% 	?event({'cron:add:test:::::::::::get_done'}).

%%%%
%%% 
%%% 
%%% 
%%% 
%%% 	% Node = hb_http_server:start_node(),
	%%UrlPath = <<"/~cron@1.0/add?task=foobar">>,	
	%%{ok, _ReqMsgId} = hb_http:get(Node, UrlPath, #{}),
	% {ok, Res} = hb_ao:resolve_many(
	% 	hb_singleton:from(
	% 		#{
	% 			<<"path">> => <<"/cron~node-process@1.0/schedule">>,
	% 			<<"method">> => <<"POST">>,
	% 			<<"body">> => Req
	% 		}
	% 	),
	% 	cron_opts(Opts)
	% ),
	% ?event({viksit_cron_add_result, {result, Res}}),
	% hb_http:get(Node, <<"/cron~node-process@1.0/now">>, #{}),
	% ?event({'cron:add:test:get_done', {req_id, _ReqMsgId}}).
	% 
% 
% load_test() ->
% 	Node = hb_http_server:start_node(),
% 	UrlPath = <<"/~cron@1.0/load">>,
% 	{ok, _ReqMsgId} = hb_http:get(Node, UrlPath, #{}),
% 	?event({'cron:load:test:get_done', {req_id, _ReqMsgId}}).