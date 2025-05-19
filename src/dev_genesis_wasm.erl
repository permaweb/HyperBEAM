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

genisis_wasm_opts() ->
    #{
        genesis_wasm_db_dir => "cache-mainnet-test/genesis-wasm",
        genesis_wasm_checkpoints_dir => "cache-mainnet-test/genesis-wasm/checkpoints",
        genesis_wasm_log_level => "error",
        genesis_wasm_port => 6363
    }.

genesis_wasm_init() ->
    genesis_wasm_init(genisis_wasm_opts()).
genesis_wasm_init(Opts) ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Msg1 = hb_message:commit(#{
        <<"device">> => <<"process@1.0">>,
        <<"scheduler-device">> => <<"scheduler@1.0">>,
        <<"scheduler-location">> => Address,
        <<"type">> => <<"Process">>,
        <<"test-random-seed">> => rand:uniform(1337)
    }, Wallet),
    Status = ensure_started(Opts),
    ?assertEqual(true, Status),
    Msg1.


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

test_aos_process() ->
    test_aos_process(#{
        genesis_wasm_db_dir => "cache-mainnet-test/genesis-wasm",
        genesis_wasm_checkpoints_dir => "cache-mainnet-test/genesis-wasm/checkpoints",
        genesis_wasm_log_level => "error",
        genesis_wasm_port => 6363,
        port => 8734
    }).
test_aos_process(Opts) ->
    test_aos_process(Opts, [
        <<"WASI@1.0">>,
        <<"JSON-Iface@1.0">>,
        <<"WASM-64@1.0">>,
        <<"Multipass@1.0">>,
        <<"genesis-wasm@1.0">>
    ]).
test_aos_process(Opts, Stack) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WASMProc = test_wasm_process(<<"test/aos-2-pure-xs.wasm">>, Opts),
    hb_message:commit(
        maps:merge(
            hb_message:uncommitted(WASMProc),
            #{
                <<"device-stack">> => Stack,
                <<"execution-device">> => <<"stack@1.0">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"output-prefix">> => <<"wasm">>,
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
    Msg1 = test_aos_process(),
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

    
    % Msg2 = #{ <<"device">> => <<"genesis-wasm@1.0">>, <<"path">> => <<"compute">>, <<"slot">> => 0 },
    % {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, #{}),
    ?assertEqual(<<"4">>, hb_ao:get(<<"results/data">>, Result)).
-endif.
