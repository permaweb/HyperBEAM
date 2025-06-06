%%% @doc A device that mimics an environment suitable for `legacynet' AO 
%%% processes, using HyperBEAM infrastructure. This allows existing `legacynet'
%%% AO process definitions to be used in HyperBEAM.
-module(dev_genesis_wasm).
-export([init/3, compute/3, normalize/3, snapshot/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%%% Timeout for legacy CU status check.
-define(STATUS_TIMEOUT, 100).

%% @doc Initialize the device.
init(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc Normalize the device.
normalize(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc Snapshot the device.
snapshot(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc All the `delegated-compute@1.0' device to execute the request. We then apply
%% the `patch@1.0' device, applying any state patches that the AO process may have
%% requested.
compute(Msg, Msg2, Opts) ->
    % Validate whether the genesis-wasm feature is enabled.
    case ensure_started(Opts) of
        true ->
            % Resolve the `delegated-compute@1.0' device.
            case hb_ao:resolve(Msg, {as, <<"delegated-compute@1.0">>, Msg2}, Opts) of
                {ok, Msg3} ->
                    % Resolve the `patch@1.0' device.
                    {ok, Msg4} =
                        hb_ao:resolve(
                            Msg3,
                            {
                                as,
                                <<"patch@1.0">>,
                                Msg2#{ <<"patch-from">> => <<"/results/outbox">> }
                            },
                            Opts
                        ),
                    % Return the patched message.
                    {ok, Msg4};
                {error, Error} ->
                    % Return the error.
                    {error, Error}
            end;
        false ->
            % Return an error if the genesis-wasm feature is disabled.
            {error, #{
                <<"status">> => 500,
                <<"message">> =>
                    <<"HyperBEAM was not compiled with genesis-wasm@1.0 on "
                        "this node.">>
            }}
    end.

%% @doc Ensure the local `genesis-wasm@1.0' is live. If it not, start it.
ensure_started(Opts) ->
    % Check if the `genesis-wasm@1.0' device is already running. The presence
    % of the registered name implies its availability.
    ?event({ensure_started, genesis_wasm, self()}),
    IsRunning = is_genesis_wasm_server_running(Opts),
    IsCompiled = hb_features:genesis_wasm(),
    GenWASMProc = is_pid(hb_name:lookup(<<"genesis-wasm@1.0">>)),
    case IsRunning orelse (IsCompiled andalso GenWASMProc) of
        true ->
            % If it is, do nothing.
            true;
        false ->
			% The device is not running, so we need to start it.
            PID =
                spawn(
                    fun() ->
                        ?event({genesis_wasm_booting, {pid, self()}}),
                        % Create genesis_wasm cache dir, if it does not exist.
                        NodeURL =
                            "http://localhost:" ++
                            integer_to_list(hb_opts:get(port, no_port, Opts)),
                        DBDir =
                            filename:absname(
                                hb_util:list(
                                    hb_opts:get(
                                        genesis_wasm_db_dir,
                                        "cache-mainnet/genesis-wasm",
                                        Opts
                                    )
                                )
                            ),
						CheckpointDir =
                            filename:absname(
                                hb_util:list(
                                    hb_opts:get(
                                        genesis_wasm_checkpoints_dir,
                                        "cache-mainnet/genesis-wasm/checkpoints",
                                        Opts
                                    )
                                )
                            ),
                        DatabaseUrl = filename:absname(DBDir ++ "/genesis-wasm-db"),
                        filelib:ensure_path(DBDir),
						filelib:ensure_path(CheckpointDir),
                        Port =
                            open_port(
                                {spawn_executable,
                                    "_build/genesis-wasm-server/launch-monitored.sh"
                                },
                                [
                                    binary, use_stdio, stderr_to_stdout,
                                    {args, [
                                        "npm",
                                        "--prefix",
                                        "_build/genesis-wasm-server",
                                        "run",
                                        "dev"
                                    ]},
                                    {env,
                                        [
                                            {"UNIT_MODE", "hbu"},
                                            {"HB_URL", NodeURL},
                                            {"PORT",
                                                integer_to_list(
                                                    hb_opts:get(
                                                        genesis_wasm_port,
                                                        6363,
                                                        Opts
                                                    )
                                                )
                                            },
                                            {"DB_URL", DatabaseUrl},
                                            {"NODE_CONFIG_ENV", "development"},
                                            {"DEFAULT_LOG_LEVEL",
                                                hb_util:list(
                                                    hb_opts:get(
                                                        genesis_wasm_log_level,
                                                        "error",
                                                        Opts
                                                    )
                                                )
                                            },
                                            {"WALLET_FILE",
                                                filename:absname(
                                                    hb_util:list(
                                                        hb_opts:get(
                                                            priv_key_location,
                                                            no_key,
                                                            Opts
                                                        )
                                                    )
                                                )
                                            },
											{"DISABLE_PROCESS_FILE_CHECKPOINT_CREATION", "false"},
											{"PROCESS_MEMORY_FILE_CHECKPOINTS_DIR", CheckpointDir}
                                        ]
                                    }
                                ]
                            ),
                        ?event({genesis_wasm_port_opened, {port, Port}}),
                        collect_events(Port)
                    end
                ),
            hb_name:register(<<"genesis-wasm@1.0">>, PID),
            ?event({genesis_wasm_starting, {pid, PID}}),
            % Wait for the device to start.
            hb_util:until(
                fun() ->
                    receive after 2000 -> ok end,
                    Status = is_genesis_wasm_server_running(Opts),
                    ?event({genesis_wasm_boot_wait, {received_status, Status}}),
                    Status
                end
            ),
            ?event({genesis_wasm_started, {pid, PID}}),
            true
    end.

%% @doc Check if the genesis-wasm server is running, using the cached process ID
%% if available.
is_genesis_wasm_server_running(Opts) ->
    case get(genesis_wasm_pid) of
        undefined ->
            ?event(genesis_wasm_pinging_server),
            Parent = self(),
            PID = spawn(
                fun() ->
                    ?event({genesis_wasm_get_info_endpoint, {worker, self()}}),
                    Parent ! {ok, self(), status(Opts)}
                end
            ),
            receive
                {ok, PID, Status} ->
                    put(genesis_wasm_pid, Status),
                    ?event({genesis_wasm_received_status, Status}),
                    Status
            after ?STATUS_TIMEOUT ->
                ?event({genesis_wasm_status_check, timeout}),
                erlang:exit(PID, kill),
                false
            end;
        _ -> true
    end.

%% @doc Check if the genesis-wasm server is running by requesting its status
%% endpoint.
status(Opts) ->
    ServerPort =
        integer_to_binary(
            hb_opts:get(
                genesis_wasm_port,
                6363,
                Opts
            )
        ),
    try hb_http:get(<<"http://localhost:", ServerPort/binary, "/status">>, Opts) of
        {ok, Res} ->
            ?event({genesis_wasm_status_check, {res, Res}}),
            true;
        Err ->
            ?event({genesis_wasm_status_check, {err, Err}}),
            false
    catch
        _:Err ->
            ?event({genesis_wasm_status_check, {error, Err}}),
            false
    end.

%% @doc Collect events from the port and log them.
collect_events(Port) ->
    collect_events(Port, <<>>).
collect_events(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_events(Port,
                log_server_events(<<Acc/binary, Data/binary>>)
            );
        stop ->
            port_close(Port),
            ?event(genesis_wasm_stopped, {pid, self()}),
            ok
    end.

%% @doc Log lines of output from the genesis-wasm server.
log_server_events(Bin) when is_binary(Bin) ->
    log_server_events(binary:split(Bin, <<"\n">>, [global]));
log_server_events([Remaining]) -> Remaining;
log_server_events([Line | Rest]) ->
    ?event(genesis_wasm_server, {server_logged, {string, Line}}),
    log_server_events(Rest).

%% Tests
-ifdef(ENABLE_GENESIS_WASM).

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

test_wasm_stack_process(Opts, Stack) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WASMProc = test_wasm_process(<<"test/aos-2-pure-xs.wasm">>, Opts),
    hb_message:commit(
            maps:merge(
                hb_message:uncommitted(WASMProc),
                #{
                    <<"device-stack">> => Stack,
                    <<"execution-device">> => <<"genesis-wasm@1.0">>,
                    <<"scheduler-device">> => <<"scheduler@1.0">>,
                    <<"patch-from">> => <<"/results/outbox">>,
                    <<"passes">> => 2,
                    <<"stack-keys">> =>
                        [
                            <<"init">>,
                            <<"compute">>,
                            <<"snapshot">>,
                            <<"normalize">>,
                            <<"compute">>
                        ],
                    <<"scheduler">> => Address,
                    <<"authority">> => Address,
                    <<"module">> => <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>,
                    <<"data-protocol">> => <<"ao">>,
                    <<"type">> => <<"Process">>
                }
            ),
        Wallet
    ).

test_genesis_wasm_process() ->
    Opts = #{
        genesis_wasm_db_dir => "cache-mainnet-test/genesis-wasm",
        genesis_wasm_checkpoints_dir => "cache-mainnet-test/genesis-wasm/checkpoints",
        genesis_wasm_log_level => "error",
        genesis_wasm_port => 6363,
        execution_device => <<"genesis-wasm@1.0">>,
        port => 8734
    },
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WASMProc = test_wasm_process(<<"test/aos-2-pure-xs.wasm">>, Opts),
    hb_message:commit(
        maps:merge(
            hb_message:uncommitted(WASMProc),
            #{
                <<"execution-device">> => <<"genesis-wasm@1.0">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"patch-from">> => <<"/results/outbox">>,
                <<"passes">> => 1,
                <<"scheduler">> => Address,
                <<"authority">> => Address,
                <<"module">> => <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>,
                <<"data-protocol">> => <<"ao">>,
                <<"type">> => <<"Process">>
            }),
        Wallet
    ).

schedule_test_message(Msg1, Text) ->
    schedule_test_message(Msg1, Text, #{}).
schedule_test_message(Msg1, Text, MsgBase) ->
    Wallet = hb:wallet(),
    UncommittedBase = hb_message:uncommitted(MsgBase),
    Msg2 =
        hb_message:commit(#{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        UncommittedBase#{
                            <<"type">> => <<"Message">>,
                            <<"test-label">> => Text
                        },
                        Wallet
                    )
            },
            Wallet
        ),
    {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, #{}),
    Msg3.

schedule_aos_call(Msg1, Code) ->
    schedule_aos_call(Msg1, Code, #{}).
schedule_aos_call(Msg1, Code, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ProcID = hb_message:id(Msg1, all),
    Msg2 =
        hb_message:commit(
            #{
                <<"action">> => <<"Eval">>,
                <<"data">> => Code,
                <<"target">> => ProcID
            },
            Wallet
        ),
    schedule_test_message(Msg1, <<"TEST MSG">>, Msg2).
   
spawn_and_execute_slot_test_() ->
    { timeout, 900, fun spawn_and_execute_slot/0 }.
spawn_and_execute_slot() ->
    application:ensure_all_started(hb),
    Msg1 = test_genesis_wasm_process(),
    hb_cache:write(Msg1, #{}),
    hb_ao:resolve(Msg1, Msg1#{<<"path">> => <<"schedule">>, <<"method">> => <<"POST">>}, #{}),

    schedule_aos_call(Msg1, <<"return 1+1">>),
    schedule_aos_call(Msg1, <<"return 2+2">>),
    
    {ok, SchedulerRes} =
        hb_ao:resolve(Msg1, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, #{}),

    % Verify computation messages
    ?assertMatch(
        <<"return 1+1">>,
        hb_ao:get(<<"assignments/1/body/data">>, SchedulerRes)
    ),
    ?assertMatch(
        <<"return 2+2">>,
        hb_ao:get(<<"assignments/2/body/data">>, SchedulerRes)
    ),

    % Verify process message is scheduled first
    ?assertMatch(
        <<"Process">>,
        hb_ao:get(<<"assignments/0/body/type">>, SchedulerRes)
    ),

    {ok, Result} = hb_ao:resolve(Msg1, #{
        <<"path">> => <<"now">>
    }, #{}),

    ?assertEqual(<<"4">>, hb_ao:get(<<"results/data">>, Result)).


compare_result_genesis_wasm_and_wasm_test_() ->
    { timeout, 900, fun compare_result_genesis_wasm_and_wasm/0 }.
compare_result_genesis_wasm_and_wasm() ->
    application:ensure_all_started(hb),

    % Test with genesis-wasm
    MsgGenesisWasm = test_genesis_wasm_process(),
    hb_cache:write(MsgGenesisWasm, #{}),
    hb_ao:resolve(MsgGenesisWasm, MsgGenesisWasm#{<<"path">> => <<"schedule">>, <<"method">> => <<"POST">>}, #{}),

    schedule_aos_call(MsgGenesisWasm, <<"return 1+1">>),
    schedule_aos_call(MsgGenesisWasm, <<"return 2+2">>),
    
    {ok, SchedulerResGenesisWasm} =
        hb_ao:resolve(MsgGenesisWasm, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, #{}),

    LoadedSchedulerResGenesisWasm = hb_cache:ensure_all_loaded(SchedulerResGenesisWasm, #{}),
    % Verify computation messages
    ?assertMatch(
        <<"return 1+1">>,
        hb_ao:get(<<"assignments/1/body/data">>, LoadedSchedulerResGenesisWasm)
    ),
    ?assertMatch(
        <<"return 2+2">>,
        hb_ao:get(<<"assignments/2/body/data">>, LoadedSchedulerResGenesisWasm)
    ),

    % Verify process message is scheduled first
    ?assertMatch(
        <<"Process">>,
        hb_ao:get(<<"assignments/0/body/type">>, LoadedSchedulerResGenesisWasm)
    ),

    {ok, ResultGenesisWasm} = hb_ao:resolve(MsgGenesisWasm, #{
        <<"path">> => <<"now">>
    }, #{}),


    % Test with wasm
    MsgWasm = test_wasm_stack_process(#{
        port => 8734
    }, [
        <<"WASI@1.0">>,
        <<"JSON-Iface@1.0">>,
        <<"WASM-64@1.0">>,
        <<"Multipass@1.0">>
    ]),
    hb_cache:write(MsgWasm, #{}),
    hb_ao:resolve(MsgWasm, MsgWasm#{<<"path">> => <<"schedule">>, <<"method">> => <<"POST">>}, #{}),

    schedule_aos_call(MsgWasm, <<"return 1+1">>),
    schedule_aos_call(MsgWasm, <<"return 2+2">>),
    
    {ok, SchedulerResWasm} =
        hb_ao:resolve(MsgWasm, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, #{}),

    LoadedSchedulerResWasm = hb_cache:ensure_all_loaded(SchedulerResWasm, #{}),
    % Verify computation messages
    ?assertMatch(
        <<"return 1+1">>,
        hb_ao:get(<<"assignments/1/body/data">>, SchedulerResWasm)
    ),

    ?assertMatch(
        <<"return 2+2">>,
        hb_ao:get(<<"assignments/2/body/data">>, LoadedSchedulerResWasm)
    ),

    {ok, ResultWasm} = hb_ao:resolve(MsgWasm, #{
        <<"path">> => <<"now">>
    }, #{}),

    ?assertEqual(<<"4">>, hb_ao:get(<<"results/data">>, ResultWasm)),

    ?assertEqual(hb_ao:get(<<"results/data">>, ResultGenesisWasm), hb_ao:get(<<"results/data">>, ResultWasm)).

compare_result_genesis_wasm_and_wasm_2_test_() ->
    { timeout, 900, fun compare_result_genesis_wasm_and_wasm_2/0 }.
compare_result_genesis_wasm_and_wasm_2() ->
    application:ensure_all_started(hb),

    % Test with genesis-wasm
    MsgGenesisWasm = test_genesis_wasm_process(),
    hb_cache:write(MsgGenesisWasm, #{}),
    hb_ao:resolve(MsgGenesisWasm, MsgGenesisWasm#{<<"path">> => <<"schedule">>, <<"method">> => <<"POST">>}, #{}),

    schedule_aos_call(MsgGenesisWasm, <<"Number = 0">>),
    schedule_aos_call(MsgGenesisWasm, <<"Number = Number + 5">>),
    schedule_aos_call(MsgGenesisWasm, <<"Number = Number * 10">>),
    schedule_aos_call(MsgGenesisWasm, <<"return Number">>),
    
    {ok, SchedulerResGenesisWasm} =
        hb_ao:resolve(MsgGenesisWasm, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, #{}),

    LoadedSchedulerResGenesisWasm = hb_cache:ensure_all_loaded(SchedulerResGenesisWasm, #{}),
    % Verify computation messages
    ?assertMatch(
        <<"Number = 0">>,
        hb_ao:get(<<"assignments/1/body/data">>, LoadedSchedulerResGenesisWasm)
    ),
    ?assertMatch(
        <<"Number = Number + 5">>,
        hb_ao:get(<<"assignments/2/body/data">>, LoadedSchedulerResGenesisWasm)
    ),
    ?assertMatch(
        <<"Number = Number * 10">>,
        hb_ao:get(<<"assignments/3/body/data">>, LoadedSchedulerResGenesisWasm)
    ),

    % Verify process message is scheduled first
    ?assertMatch(
        <<"Process">>,
        hb_ao:get(<<"assignments/0/body/type">>, LoadedSchedulerResGenesisWasm)
    ),

    {ok, ResultGenesisWasm} = hb_ao:resolve(MsgGenesisWasm, #{
        <<"path">> => <<"now">>
    }, #{}),


    % Test with wasm
    MsgWasm = test_wasm_stack_process(#{
        port => 8734
    }, [
        <<"WASI@1.0">>,
        <<"JSON-Iface@1.0">>,
        <<"WASM-64@1.0">>,
        <<"Multipass@1.0">>
    ]),
    hb_cache:write(MsgWasm, #{}),
    hb_ao:resolve(MsgWasm, MsgWasm#{<<"path">> => <<"schedule">>, <<"method">> => <<"POST">>}, #{}),

    schedule_aos_call(MsgWasm, <<"Number = 0">>),
    schedule_aos_call(MsgWasm, <<"Number = Number + 5">>),
    schedule_aos_call(MsgWasm, <<"Number = Number * 10">>),
    schedule_aos_call(MsgWasm, <<"return Number">>),
    
    {ok, SchedulerResWasm} =
        hb_ao:resolve(MsgWasm, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, #{}),

    LoadedSchedulerResWasm = hb_cache:ensure_all_loaded(SchedulerResWasm, #{}),
    % Verify computation messages
    ?assertMatch(
        <<"Number = 0">>,
        hb_ao:get(<<"assignments/1/body/data">>, LoadedSchedulerResGenesisWasm)
    ),
    ?assertMatch(
        <<"Number = Number + 5">>,
        hb_ao:get(<<"assignments/2/body/data">>, LoadedSchedulerResGenesisWasm)
    ),
    ?assertMatch(
        <<"Number = Number * 10">>,
        hb_ao:get(<<"assignments/3/body/data">>, LoadedSchedulerResGenesisWasm)
    ),
    ?assertMatch(
        <<"return Number">>,
        hb_ao:get(<<"assignments/4/body/data">>, LoadedSchedulerResGenesisWasm)
    ),


    {ok, ResultWasm} = hb_ao:resolve(MsgWasm, #{
        <<"path">> => <<"now">>
    }, #{}),

    ?assertEqual(<<"50">>, hb_ao:get(<<"results/data">>, ResultWasm)),

    ?assertEqual(hb_ao:get(<<"results/data">>, ResultGenesisWasm), hb_ao:get(<<"results/data">>, ResultWasm)).

send_message_to_genesis_wasm_process_test_() ->
    { timeout, 900, fun send_message_to_genesis_wasm_process/0 }.
send_message_to_genesis_wasm_process() ->
    application:ensure_all_started(hb),

    % SET UP HANDLER ON RECEIVER PROCESS
    MsgReceiver = test_genesis_wasm_process(),
    hb_cache:write(MsgReceiver, #{}),
    ProcId = dev_process:process_id(MsgReceiver, #{}, #{}),
    hb_ao:resolve(MsgReceiver, MsgReceiver#{<<"path">> => <<"schedule">>, <<"method">> => <<"POST">>}, #{}),

    schedule_aos_call(MsgReceiver, <<"Number = 10">>),
    schedule_aos_call(MsgReceiver, <<"Handlers.add('foo', function(msg) print(\"Number: \" .. Number * 2) return Number * 2 end)">>),
    schedule_aos_call(MsgReceiver, <<"return Number">>),
    ?event({ jack, { proc_id, ProcId } }),
    {ok, ResultReceiver} = hb_ao:resolve(MsgReceiver, #{
        <<"path">> => <<"now">>
    }, #{}),

    ?assertEqual(<<"10">>, hb_ao:get(<<"results/data">>, ResultReceiver)),

    % SEND MESSAGE TO RECEIVER PROCESS
    MsgSender = test_genesis_wasm_process(),
    hb_cache:write(MsgSender, #{}),
    hb_ao:resolve(MsgSender, MsgSender#{<<"path">> => <<"schedule">>, <<"method">> => <<"POST">>}, #{}),
    SendMsgToReceiver = schedule_aos_call(MsgSender,  iolist_to_binary([<<"Send({ Target = \"">>, ProcId, <<"\", Action = \"foo\" })">>])),
    {ok, ResultSender} = hb_ao:resolve(MsgSender, #{
        <<"path">> => <<"now">>
    }, #{}),
    {ok, Slot} = hb_ao:resolve(SendMsgToReceiver, #{ <<"path">> => <<"slot">> }, #{}),
    {ok, Res} = hb_ao:resolve(MsgSender, #{
        <<"path">> => <<"push">>,
        <<"slot">> => Slot
    }, #{}),

    % GET SCHEDULE FOR RECEIVER
    {ok, ScheduleReceiver} =
        hb_ao:resolve(MsgReceiver, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, #{}),
    ?assertEqual(<<"foo">>, hb_ao:get(<<"assignments/4/body/action">>, ScheduleReceiver)),
    {ok, NewResultReceiver} = hb_ao:resolve(MsgReceiver, #{
        <<"path">> => <<"now">>
    }, #{}),
    ?assertEqual(<<"Number: 20">>, hb_ao:get(<<"results/data">>, NewResultReceiver)).
-endif.
