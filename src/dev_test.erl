-module(dev_test).
-export([info/1, test_func/1, compute/3, init/3, restore/3, snapshot/3, mul/2]).
-export([update_state/3, increment_counter/3, delay/3]).
-export([postprocess/3]).
-export([info/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% A simple test device for AO-Core, so that we can test the functionality that
%%% depends on using Erlang's module system.
%%% 
%%% NOTE: This device is labelled `Test-Device/1.0' to avoid conflicts with
%%% other testing functionality -- care should equally be taken to avoid
%%% using the `test' key in other settings.


%% @doc Exports a default_handler function that can be used to test the
%% handler resolution mechanism.
info(_) ->
	#{
        <<"default">> => dev_message,
		handlers => #{
			<<"info">> => fun info/3,
			<<"update_state">> => fun update_state/3,
			<<"increment_counter">> => fun increment_counter/3
		}
	}.

%% @doc Exports a default_handler function that can be used to test the
%% handler resolution mechanism.
info(_Msg1, _Msg2, _Opts) ->
	InfoBody = #{
		<<"description">> => <<"Test device for testing the AO-Core framework">>,
		<<"version">> => <<"1.0">>,
		<<"paths">> => #{
			<<"info">> => <<"Get device info">>,
			<<"test_func">> => <<"Test function">>,
			<<"compute">> => <<"Compute function">>,
			<<"init">> => <<"Initialize function">>,
			<<"restore">> => <<"Restore function">>,
			<<"mul">> => <<"Multiply function">>,
			<<"snapshot">> => <<"Snapshot function">>,
			<<"postprocess">> => <<"Postprocess function">>,
			<<"update_state">> => <<"Update state function">>
		}
	},
	{ok, #{<<"status">> => 200, <<"body">> => InfoBody}}.


test_func(_) ->
	{ok, <<"GOOD_FUNCTION">>}.

%% @doc Example implementation of a `compute' handler. Makes a running list of
%% the slots that have been computed in the state message and places the new
%% slot number in the results key.
compute(Msg1, Msg2, Opts) ->
    AssignmentSlot = hb_ao:get(<<"slot">>, Msg2, Opts),
    Seen = hb_ao:get(<<"already-seen">>, Msg1, Opts),
    ?event({compute_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    {ok,
        hb_ao:set(
            Msg1,
            #{
                <<"random-key">> => <<"random-value">>,
                <<"results">> =>
                    #{ <<"assignment-slot">> => AssignmentSlot },
                <<"already-seen">> => [AssignmentSlot | Seen]
            },
            Opts
        )
    }.

%% @doc Example `init/3' handler. Sets the `Already-Seen' key to an empty list.
init(Msg, _Msg2, Opts) ->
    ?event({init_called_on_dev_test, Msg}),
    {ok, hb_ao:set(Msg, #{ <<"already-seen">> => [] }, Opts)}.

%% @doc Example `restore/3' handler. Sets the hidden key `Test/Started' to the
%% value of `Current-Slot' and checks whether the `Already-Seen' key is valid.
restore(Msg, _Msg2, Opts) ->
    ?event({restore_called_on_dev_test, Msg}),
    case hb_ao:get(<<"already-seen">>, Msg, Opts) of
        not_found ->
            ?event({restore_not_found, Msg}),
            {error, <<"No viable state to restore.">>};
        AlreadySeen ->
            ?event({restore_found, AlreadySeen}),
            {ok,
                hb_private:set(
                    Msg,
                    #{ <<"test-key/started-state">> => AlreadySeen },
                    Opts
                )
            }
    end.

%% @doc Example implementation of an `imported' function for a WASM
%% executor.
mul(Msg1, Msg2) ->
    ?event(mul_called),
    State = hb_ao:get(<<"state">>, Msg1, #{ hashpath => ignore }),
    [Arg1, Arg2] = hb_ao:get(<<"args">>, Msg2, #{ hashpath => ignore }),
    ?event({mul_called, {state, State}, {args, [Arg1, Arg2]}}),
    {ok, #{ <<"state">> => State, <<"results">> => [Arg1 * Arg2] }}.

%% @doc Do nothing when asked to snapshot.
snapshot(_Msg1, _Msg2, _Opts) ->
    {ok, #{}}.

%% @doc Set the `postprocessor-called' key to true in the HTTP server.
postprocess(_Msg, #{ <<"body">> := Msgs }, Opts) ->
    ?event({postprocess_called, Opts}),
    hb_http_server:set_opts(Opts#{ <<"postprocessor-called">> => true }),
    {ok, Msgs}.

%% @doc Find a test worker's PID and send it an update message.
update_state(_Msg, Msg2, _Opts) ->
    case hb_ao:get(<<"test-id">>, Msg2) of
        not_found ->
            {error, <<"No test ID found in message.">>};
        ID ->
            LookupResult = hb_name:lookup({<<"test">>, ID}),
            case LookupResult of
                undefined ->
                    {error, <<"No test worker found.">>};
                Pid ->
                    Pid ! {update, Msg2},
                    {ok, Pid}
            end
    end.

%% @doc Find a test worker's PID and send it an increment message.
increment_counter(_Msg1, Msg2, _Opts) ->
    case hb_ao:get(<<"test-id">>, Msg2) of
        not_found ->
            {error, <<"No test ID found in message.">>};
        ID ->
            LookupResult = hb_name:lookup({<<"test">>, ID}),
            case LookupResult of
                undefined ->
                    {error, <<"No test worker found for increment.">>};
                Pid when is_pid(Pid) ->
                    Pid ! {increment},
				    {ok, Pid};
                _ -> % Handle case where registered value isn't a PID
                    {error, <<"Invalid registration found for test worker.">>}
            end
    end.

%% @doc Does nothing, just sleeps `Req/duration or 750' ms and returns the 
%% appropriate form in order to be used as preprocessor.
delay(Msg1, Req, Opts) ->
    Duration =
        hb_ao:get_first(
            [
                {Msg1, <<"duration">>},
                {Req, <<"duration">>}
            ],
            750,
            Opts
        ),
    ?event(delay, {delay, {sleeping, Duration}}),
    timer:sleep(Duration),
    ?event({delay, waking}),
    Return =
        case hb_ao:get(<<"return">>, Msg1, Opts) of
            not_found ->
                hb_ao:get(<<"body">>, Req, #{ <<"result">> => <<"slept">> }, Opts);
            ReturnMsgs ->
                ReturnMsgs
        end,
    ?event(delay, {returning, Return}),
    {ok, Return}.

%%% Tests

%% @doc Tests the resolution of a default function.
device_with_function_key_module_test() ->
	Msg =
		#{
			<<"device">> => <<"Test-Device@1.0">>
		},
	?assertEqual(
		{ok, <<"GOOD_FUNCTION">>},
		hb_ao:resolve(Msg, test_func, #{})
	).

compute_test() ->
    Msg0 = #{ <<"device">> => <<"Test-Device@1.0">> },
    {ok, Msg1} = hb_ao:resolve(Msg0, init, #{}),
    Msg2 =
        hb_ao:set(
            #{ <<"path">> => <<"compute">> },
            #{
                <<"slot">> => 1,
                <<"body/number">> => 1337
            },
            #{}
        ),
    {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, #{}),
    ?assertEqual(1, hb_ao:get(<<"results/assignment-slot">>, Msg3, #{})),
    Msg4 =
        hb_ao:set(
            #{ <<"path">> => <<"compute">> },
            #{
                <<"slot">> => 2,
                <<"body/number">> => 9001
            },
            #{}
        ),
    {ok, Msg5} = hb_ao:resolve(Msg3, Msg4, #{}),
    ?assertEqual(2, hb_ao:get(<<"results/assignment-slot">>, Msg5, #{})),
    ?assertEqual([2, 1], hb_ao:get(<<"already-seen">>, Msg5, #{})).

restore_test() ->
    Msg1 = #{ <<"device">> => <<"Test-Device@1.0">>, <<"already-seen">> => [1] },
    {ok, Msg3} = hb_ao:resolve(Msg1, <<"restore">>, #{}),
    ?assertEqual([1], hb_private:get(<<"test-key/started-state">>, Msg3, #{})).



%% test code

todo_app_http_test_() -> % Ensure name matches EUnit convention for timed tests
    NodeWallet = ar_wallet:new(), % Create a wallet for the HTTP server node
    Node = hb_http_server:start_node(#{ priv_wallet => NodeWallet }), % Provide wallet to the node
    try
        % 2. Define the Lua Script (same as previous example)
        TodoScriptBinary = <<"""
        if TodoList == nil then
            TodoList = {}
        end

        function addTask(params)
            local task_description = params.task
            if task_description == nil then
                return { status = "error", message = "Task parameter is missing" }
            end
            table.insert(TodoList, task_description)
            return { status = "ok", message = "Task '" .. task_description .. "' added." }
        end

        function getTasks()
            return { status = "ok", tasks = TodoList }
        end

        function initializeList()
            TodoList = {}
            return { status = "ok", message = "Todo list initialized."}
        end
        """>>,

        % 3. Create the Lua process that will host our todo list
        Wallet = hb:wallet(),
        TodoProcess = hb_message:commit(#{
            <<"device">> => <<"process@1.0">>,
            <<"type">> => <<"Process">>,
            <<"scheduler-device">> => <<"scheduler@1.0">>,
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"script">> => #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => TodoScriptBinary
            },
            <<"authority">> => [hb:address()],
            <<"scheduler-location">> => hb_util:human_id(ar_wallet:to_address(Wallet))
        }, Wallet),
        ProcID = hb_message:id(TodoProcess, all),
        {ok, _} = hb_cache:write(TodoProcess, #{}), % Make the process discoverable

        % NEW STEP: Register/Schedule the process with the HTTP Node's scheduler
        {ok, ScheduleResponse} = hb_http:post(Node, <<"/schedule">>, TodoProcess, #{json_response => true}),
        ?event({debug_http_todo, {schedule_process_response, ScheduleResponse}}),
        case maps:get(<<"status">>, ScheduleResponse, undefined) of
            200 -> ok; % OK
            201 -> ok; % Created
            202 -> ok; % Accepted
            _Status -> 
                ?debugFmt("Unexpected schedule process response: ~p", [ScheduleResponse]),
                ?assert(false)
        end,

        % Base URL for our process
        BaseUrl = <<"/", ProcID/binary>>,

        % 4. Initialize the list via HTTP POST
        InitPath = << BaseUrl/binary, "/initializeList" >>,
        InitBody = #{ <<"parameters">> => #{} }, % Correctly structured for dev_lua
        {ok, InitResponse} = hb_http:post(Node, InitPath, InitBody, #{json_response => true}),
        ?event({debug_http_todo, {init_response, InitResponse}}),
        ?assertEqual(<<"ok">>, maps:get(<<"status">>, InitResponse)), % Corrected path

        % 5. Add the first task via HTTP POST
        Task1 = <<"Buy groceries via HTTP">>,
        AddPath = << BaseUrl/binary, "/addTask" >>,
        AddBody1 = #{ <<"parameters">> => #{ <<"task">> => Task1 } },
        {ok, AddResponse1} = hb_http:post(Node, AddPath, AddBody1, #{json_response => true}),
        ?event({debug_http_todo, {add_response_1, AddResponse1}}),
        ?assertEqual(<<"ok">>, maps:get(<<"status">>, AddResponse1)), % Corrected path

        % 6. Add a second task via HTTP POST
        Task2 = <<"Submit expenses via HTTP">>,
        AddBody2 = #{ <<"parameters">> => #{ <<"task">> => Task2 } },
        {ok, AddResponse2} = hb_http:post(Node, AddPath, AddBody2, #{json_response => true}),
        ?event({debug_http_todo, {add_response_2, AddResponse2}}),
        ?assertEqual(<<"ok">>, maps:get(<<"status">>, AddResponse2)), % Corrected path

        % 7. Get all tasks via HTTP GET and verify
        GetTasksPath = << BaseUrl/binary, "/getTasks" >>,
        {ok, GetTasksResponse} = hb_http:get(Node, GetTasksPath, #{json_response => true}),
        ?event({debug_http_todo, {get_tasks_response, GetTasksResponse}}),

        ExpectedTasks = [Task1, Task2],
        ActualTasks = maps:get(<<"tasks">>, GetTasksResponse), % Corrected path
        
        ?assertEqual(<<"ok">>, maps:get(<<"status">>, GetTasksResponse)), % Corrected path
        ?assertEqual(ExpectedTasks, ActualTasks),
        
        ok
    after
        % 8. Clean up: Stop the HTTP server node
        try gen_server:call(Node, stop) catch _:_ -> ok end
    end. % This closes the fun() and the test definition.



very_simple_lua_execution_test() ->
	SimpleScript = <<"function get_greeting() return 'Hello from simple Lua!' end">>,
	BaseMsg = #{
		<<"device">> => <<"lua@5.3a">>,
		<<"script">> => #{
			<<"content-type">> => <<"application/lua">>,
			<<"body">> => SimpleScript
		}
	},
	% When a Lua function returns a simple string, and dev_lua processes it,
	% the expected successful result from hb_ao:resolve is {ok, ReturnedString}.
	ExpectedResult = <<"Hello from simple Lua!">>,
	?assertEqual({ok, ExpectedResult}, hb_ao:resolve(BaseMsg, <<"get_greeting">>, #{})).

generate_lua_process(File, Opts) ->
	Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
	{ok, Script} = file:read_file(File),
	hb_message:commit(#{
		<<"device">> => <<"process@1.0">>,
		<<"type">> => <<"Process">>,
		<<"scheduler-device">> => <<"scheduler@1.0">>,
		<<"execution-device">> => <<"lua@5.3a">>,
		<<"script">> => #{
			<<"content-type">> => <<"application/lua">>,
			<<"body">> => Script
		},
		<<"authority">> => [ 
			hb:address(), 
			<<"E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ">>
		], 
		<<"scheduler-location">> =>
			hb_util:human_id(ar_wallet:to_address(Wallet)),
		<<"test-random-seed">> => rand:uniform(1337)
	}, Wallet).

generate_test_message(Process) ->
    ProcID = hb_message:id(Process, all),
    Wallet = hb:wallet(),
    Code = """ 
      Count = 0
      function add() 
        Count = Count + 1 
      end
      add()
      return Count
    """,
    hb_message:commit(#{
            <<"path">> => <<"schedule">>,
            <<"method">> => <<"POST">>,
            <<"body">> =>
                hb_message:commit(
                    #{
                        <<"target">> => ProcID,
                        <<"type">> => <<"Message">>,
                        <<"body">> => #{
                            <<"content-type">> => <<"application/lua">>,
                            <<"body">> => list_to_binary(Code) 
                        },
                        <<"random-seed">> => rand:uniform(1337),
                        <<"action">> => <<"Eval">>
                    },
                    Wallet
                )
        },
        Wallet
    ).

simple_lua_via_process_test() ->
	Process = generate_lua_process("test/hyper-aos.lua", #{}),
	{ok, _} = hb_cache:write(Process, #{}),
	Message = generate_test_message(Process),
    {ok, _} = hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
	{ok, Results} = hb_ao:resolve(Process, <<"now/results/output/data">>, #{}),
	?event({debug_http_todo, {results, Results}}),
    ?event({debug_http_todo, {end_of_test}}).




test_base_process() ->
	test_base_process(#{}).
test_base_process(Opts) ->
	Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
	Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
	hb_message:commit(#{
		<<"device">> => <<"process@1.0">>,
		<<"scheduler-device">> => <<"scheduler@1.0">>,
		<<"scheduler-location">> => Address,
		<<"type">> => <<"Process">>,
		<<"test-random-seed">> => rand:uniform(1337)
	}, Wallet).
	
test_wasm_process(WASMImage) ->
	test_wasm_process(WASMImage, #{}).
test_wasm_process(WASMImage, Opts) ->
	Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
	#{ <<"image">> := WASMImageID } = dev_wasm:cache_wasm_image(WASMImage, Opts),
	hb_message:commit(
		maps:merge(
			hb_message:uncommitted(test_base_process(Opts)),
			#{
				<<"execution-device">> => <<"stack@1.0">>,
				<<"device-stack">> => [<<"WASM-64@1.0">>],
				<<"image">> => WASMImageID
			}
		),
		Wallet
	).

simple_lua_via_process_http_test_() ->
		% 1. Setup: Start HTTP Server Node with its own wallet
		NodeWallet = ar_wallet:new(),
		Node = hb_http_server:start_node(Opts = #{
			port => 10000 + rand:uniform(10000),
			priv_wallet => NodeWallet,
			cache_control => <<"always">>,
			store => #{
				<<"store-module">> => hb_store_fs,
				<<"prefix">> => <<"cache-TEST">>
			}
		}),
		% 2. Define Ultra-Simple Lua Script
		{ok, Script} = file:read_file("test/test.lua"),
	
		% 3. Create the AO Process
		ClientWallet = hb:wallet(), 
		Process = hb_message:commit(#{
			<<"device">> => <<"process@1.0">>,
			<<"type">> => <<"Process">>,
			<<"scheduler-device">> => <<"scheduler@1.0">>,
			<<"execution-device">> => <<"lua@5.3a">>,
			<<"script">> => #{
				<<"content-type">> => <<"application/lua">>,
				<<"body">> => Script
			},
			<<"authority">> => [ 
				hb:address(), 
				<<"E3FJ53E6xtAzcftBpaw2E1H4ZM9h6qy6xz9NXh5lhEQ">>
			  ], 
			  <<"scheduler-location">> =>
				  hb_util:human_id(ar_wallet:to_address(ClientWallet)),
			  <<"test-random-seed">> => rand:uniform(1337)
		}, ClientWallet),
		hb_cache:write(Process, Opts),
		ProcID = hb_util:human_id(hb_message:id(Process, all)),
		Message = hb_message:commit(#{
			<<"path">> => <<"schedule">>,
			<<"method">> => <<"POST">>,
			<<"body">> =>
				hb_message:commit(
					#{
						<<"target">> => ProcID,
						<<"path">> => <<"hello_world">>,
						<<"type">> => <<"Message">>,
						<<"data">> => <<"1 + 1">>,
						<<"random-seed">> => rand:uniform(1337),
						<<"action">> => <<"Eval">>,
						<<"from-process">> => <<"1234">>
	
			}, ClientWallet)
		  }, ClientWallet
		),
	{ok, _} = hb_ao:resolve(Process, Message, #{ hashpath => ignore }),
	{ok, Results} = hb_ao:resolve(Process, <<"now/result">>, #{}),
	?event({debug_http_todo, {results, Results}}),
	?assertEqual(1, Results).

%%% Documentation
%%%  NodeWallet – the private key that the HTTP node uses to sign replies.
% Every Process that the node creates or owns should normally be
% committed with this wallet so the scheduler-location matches the node.
% • ClientWallet – the key you (the test client) use to sign messages that
% are later delivered to the scheduler of some Process.
% It can be the same key as NodeWallet, but it doesn’t have to be.
% In a real deployment it would more often be a different user wallet.
% • Wallet inside generate_lua_process/2 – the wallet used to sign the
% Process definition itself. When you pass Opts that already contains
% priv_wallet => NodeWallet the function ends up committing with the node’s
% own key, which satisfies the scheduler’s trust test.

%%% Process signed by  :  NodeWallet       (owner of the scheduler)
%%% Messages scheduled :  ClientWallet     ( arbitrary client  )
%%% HTTP server signs  :  NodeWallet



simple_http_resolve_test_() ->
	%% 1.  Start an HTTP node with its own wallet
	NodeWallet = ar_wallet:new(),
	NodeOpts = #{
		priv_wallet   => NodeWallet,
		cache_control => <<"always">>,          % read/write only the local store
		store         => #{
			<<"store-module">> => hb_store_fs,
			<<"prefix">>       => <<"cache-TEST">>
		}
	},
	Node = hb_http_server:start_node(NodeOpts),
	Process = generate_lua_process("test/test.lua", NodeOpts),
	ProcID = hb_util:human_id(hb_message:id(Process, all)),	
	hb_cache:write(Process, NodeOpts),
	{ok, _Reply} = hb_http:post(Node, <<"/schedule">>, Process, #{}),
	% ?event({debug_http_todo, {res, _Reply}}),

	%% the process has been schduled above. 
	%% now we can all the compute function directly to slot 0.
	% {ok, Compute0} = hb_http:get(Node, #{
	% 	<<"path">> => <<ProcID/binary, "/compute/results/output/body">>,
	% 	<<"slot">> => 0
	% }, #{json_response => true}),
	% ?event({debug_http_todo, {compute0, Compute0}}),
	% ?assertEqual(42, Compute0),
	% {ok, Compute1} = hb_http:get(Node, #{
	% 	<<"path">> => <<ProcID/binary, "/now/results/output/body">>
	% }, #{json_response => true}),
	% ?event({debug_http_todo, {compute1, Compute1}}),
	% ?assertEqual(42, Compute1),
	
	%% call a direct lua function (hello_world in test.lua)
	%% this will be scheduled to slot 0 if the above is comented out.
	ClientWallet = hb:wallet(), 
	HelloWorldMsg =
        hb_message:commit(#{
             <<"target">> => ProcID,
             <<"type">>   => <<"Message">>,
             <<"path">>   => <<"hello_world">>,  %% Lua function
             <<"action">> => <<"Eval">>,
             <<"random-seed">> => rand:uniform(1337)
         }, ClientWallet),
	HttpBody = #{ <<"body">> => HelloWorldMsg },
	{ok, _SchedRes} =
		 hb_http:post(Node,
					  <<"/", ProcID/binary, "/schedule">>,
					  HttpBody,
					  #{}),
	?event({debug_http_todo, {sched_res_hello_world, _SchedRes}}),
	{ok, ComputeHelloWorldRes} =
	hb_http:get(Node,
			#{
			<<"path">> => <<ProcID/binary, "/compute">>,
			<<"slot">> => 0
			},
			#{json_response => true}),
	?event({debug_http_todo, {compute_hello_world_res, ComputeHelloWorldRes}}),
	
	% {ok, #{<<"result">> := Val}} =
    % hb_http:get(Node,
    %             <<"/", ProcID/binary, "/hello_world">>,
    %             #{json_response => true}),
	% ?event({debug_http_todo, {hello_world_direct_res, Val}}),
	
	?event(debug_http_todo, {end_of_test}).