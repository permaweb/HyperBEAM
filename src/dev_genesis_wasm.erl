%%% @doc A device that mimics an environment suitable for `legacynet' AO 
%%% processes, using HyperBEAM infrastructure. This allows existing `legacynet'
%%% AO process definitions to be used in HyperBEAM.
-module(dev_genesis_wasm).
-export([init/3, compute/3, normalize/3, snapshot/3, import/3]).
-export([latest_checkpoint/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%%% Timeout for legacy CU status check.
-define(STATUS_TIMEOUT, 100).

%% @doc Initialize the device.
init(Msg, _Msg2, _Opts) -> {ok, Msg}.

%% @doc Normalize the device.
normalize(Msg, Msg2, Opts) ->
    case ensure_started(Opts) of
        true ->
            dev_delegated_compute:normalize(Msg, Msg2, Opts);
        false ->
            {error, #{
                <<"status">> => 500,
                <<"message">> => <<"Genesis-wasm server not running.">>
            }}
    end.

%% @doc Genesis-wasm device compute handler.
%% Normal compute execution through external CU with state persistence
compute(Msg, Msg2, Opts) ->
    % Validate whether the genesis-wasm feature is enabled.
    case delegate_request(Msg, Msg2, Opts) of
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
    end.

%% @doc Snapshot the state of the process via the `delegated-compute@1.0' device.
snapshot(Msg, Msg2, Opts) ->
    delegate_request(Msg, Msg2, Opts).

%% @doc Proxy a request to the delegated-compute@1.0 device, ensuring that
%% the server is running.
delegate_request(Msg, Msg2, Opts) ->
    % Validate whether the genesis-wasm feature is enabled.
    case ensure_started(Opts) of
        true ->
            do_compute(Msg, Msg2, Opts);
        false ->
            % Return an error if the genesis-wasm feature is disabled.
            {error, #{
                <<"status">> => 500,
                <<"message">> =>
                    <<"HyperBEAM was not compiled with genesis-wasm@1.0 on "
                        "this node.">>
            }}
    end.


%% @doc Handle normal compute execution with state persistence (GET method).
do_compute(Msg, Msg2, Opts) ->
    % Resolve the `delegated-compute@1.0' device.
    case hb_ao:resolve(Msg, {as, <<"delegated-compute@1.0">>, Msg2}, Opts) of
        {ok, Msg3} ->
            PatchResult = 
                hb_ao:resolve(
                    Msg3,
                    {
                        as,
                        <<"patch@1.0">>,
                        Msg2#{ <<"patch-from">> => <<"/results/outbox">> }
                    },
                    Opts
                ),
            % Resolve the `patch@1.0' device.
            case PatchResult of 
                {ok, Msg4} ->
                    % Return the patched message.
                    {ok, Msg4};
                {error, Error} ->
                    % Return the error.
                    {error, Error}
            end;
        {error, Error} ->
            % Return the error.
            {error, Error}
    end.

%% @doc Ensure the local `genesis-wasm@1.0' is live. If it not, start it.
ensure_started(Opts) ->
    % Check if the `genesis-wasm@1.0' device is already running. The presence
    % of the registered name implies its availability.
    {ok, Cwd} = file:get_cwd(),
    ?event({ensure_started, cwd, Cwd}),
    % Determine path based on whether we're in a release or development
    GenesisWasmServerDir =
        case init:get_argument(mode) of
            {ok, [["embedded"]]} ->
                % We're in release mode - genesis-wasm-server is in the release root
                filename:join([Cwd, "genesis-wasm-server"]);
            _ ->
                % We're in development mode - look in the build directory
                DevPath =
                    filename:join(
                        [
                            Cwd,
                            "_build",
                            "genesis_wasm",
                            "genesis-wasm-server"
                        ]
                    ),
                case filelib:is_dir(DevPath) of
                    true -> DevPath;
                    false -> filename:join([Cwd, "genesis-wasm-server"]) % Fallback
                end
        end,
    ?event({ensure_started, genesis_wasm_server_dir, GenesisWasmServerDir}),
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
                        NodeURL =
                            "http://localhost:" ++
                            integer_to_list(hb_opts:get(port, no_port, Opts)),
                        RelativeDBDir =
                            hb_util:list(
                                hb_opts:get(
                                    genesis_wasm_db_dir,
                                    "cache-mainnet/genesis-wasm",
                                    Opts
                                )
                            ),
                        DBDir =
                            filename:absname(RelativeDBDir),
						CheckpointDir =
                            filename:absname(
                                hb_util:list(
                                    hb_opts:get(
                                        genesis_wasm_checkpoints_dir,
                                        RelativeDBDir ++ "/checkpoints",
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
                                    filename:join(
                                        [
                                            GenesisWasmServerDir,
                                            "launch-monitored.sh"
                                        ]
                                    )
                                },
                                [
                                    binary, use_stdio, stderr_to_stdout,
                                    {args, Args = [
                                        "npm",
                                        "--prefix",
                                        GenesisWasmServerDir,
                                        "run",
                                        "start"
                                    ]},
                                    {env,
                                        Env = [
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
                                            {"NODE_CONFIG_ENV", "production"},
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
                        ?event(
                            debug_genesis,
                            {started_genesis_wasm,
                                {args, Args},
                                {env, maps:from_list(Env)}
                            }
                        ),
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

%% @doc Find either a specific checkpoint by its ID, or find the most recent
%% checkpoint via GraphQL.
import(ProcMsg, Req, Opts) ->
    case hb_maps:find(<<"import">>, Req, Opts) of
        {ok, ImportID} ->
            case hb_cache:read(ImportID, Opts) of
                {ok, CheckpointMessage} ->
                    do_import(ProcMsg, CheckpointMessage, Opts);
                not_found -> {error, not_found}
            end;
        error ->
            ProcID = dev_process:process_id(ProcMsg, #{}, Opts),
            case latest_checkpoint(ProcID, Opts) of
                {ok, CheckpointMessage} ->
                    do_import(ProcMsg, CheckpointMessage, Opts);
                Err -> Err
            end
    end.

%% @doc Find the most recent legacy checkpoint for a process.
latest_checkpoint(ProcID, Opts) ->
    case hb_opts:get(genesis_wasm_import_authorities, [], Opts) of
        [] -> {error, no_import_authorities};
        TrustedSigners -> latest_checkpoint(ProcID, TrustedSigners, Opts)
    end.
latest_checkpoint(ProcID, TrustedSigners, Opts) ->
    Query =
        <<
            <<"""
            query($ProcID: String!, $TrustedSigners: [String!]) {
                transactions(
                    tags: [
                        { name: "Type" values: ["Checkpoint"] },
                        { name: "Process" values: [$ProcID] }
                    ],
                    owners: $TrustedSigners,
                    first: 1,
                    sort: HEIGHT_DESC
                ){
                edges {
            """>>/binary,
            (hb_gateway_client:item_spec())/binary,
            """
                }
            }}
        """>>,
    Variables =
        #{
            <<"ProcID">> => ProcID,
            <<"TrustedSigners">> => TrustedSigners
        },
    case hb_gateway_client:query(Query, Variables, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, GqlMsg} ->
            case hb_ao:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found -> {error, not_found};
                Item -> hb_gateway_client:result_to_message(Item, Opts)
            end
    end.

%% @doc Validate whether a checkpoint message is signed by a trusted snapshot
%% authority and is for a `ao.TN.1' process or has `execution-device' set to
%% `genesis-wasm@1.0', then normalize into a state snapshot.
%% Save the state snapshot into the store.
do_import(Proc, CheckpointMessage, Opts) ->
    maybe
        % Validate that the process is a valid target for importing a checkpoint.
        Variant = hb_maps:get(<<"variant">>, Proc, false, Opts),
        ExecutionDevice = hb_maps:get(<<"execution-device">>, Proc, false, Opts),
        true ?=
            (Variant == <<"ao.TN.1">>) orelse
            (ExecutionDevice == <<"genesis-wasm@1.0">>) orelse
            invalid_import_target,
        CheckpointSigners = hb_message:signers(CheckpointMessage, Opts),
        % Validate that the checkpoint message is signed by a trusted snapshot
        % authority, and targets this process.
        TrustedSigners = hb_opts:get(genesis_wasm_import_authorities, [], Opts),
        true ?=
            lists:any(
                fun(Signer) -> lists:member(Signer, TrustedSigners) end,
                CheckpointSigners
            ) orelse untrusted,
        true ?= hb_message:verify(CheckpointMessage, all, Opts) orelse unverified,
        CheckpointTargetProcID = hb_maps:get(<<"process">>, CheckpointMessage, Opts),
        ProcID = dev_process:process_id(Proc, #{}, Opts),
        true ?= CheckpointTargetProcID == ProcID orelse process_mismatch,
        % Normalize the checkpoint message into a process state message with 
        % a state snapshot.
        {ok, SlotBin} ?= hb_maps:find(<<"nonce">>, CheckpointMessage, Opts),
        Slot = hb_util:int(SlotBin),
        InitializedProc = dev_process:ensure_process_key(Proc, Opts),
        WithSnapshot =
            InitializedProc#{
                <<"at-slot">> => Slot,
                <<"snapshot">> => CheckpointMessage
            },
        % Save the state snapshot into the store.
        {ok, _} ?= dev_process_cache:write(ProcID, Slot, WithSnapshot, Opts),
        % Return the normalized process message.
        {ok, WithSnapshot}
    else
        invalid_import_target ->
            {error, #{
                <<"status">> => 400,
                <<"body">> =>
                    <<
                        "Process is not a valid target for importing a "
                        "`~genesis-wasm@1.0' checkpoint."
                    >>
            }};
        process_mismatch ->
            {error, #{
                <<"status">> => 400,
                <<"body">> =>
                    <<"Checkpoint message targets a different process.">>
            }};
        unverified ->
            {error, #{
                <<"status">> => 400,
                <<"body">> =>
                    <<"Checkpoint message is not verifiable.">>
            }};
        untrusted ->
            {error, #{
                <<"status">> => 400,
                <<"body">> =>
                    <<"Checkpoint message is not signed by a trusted snapshot "
                        "authority.">>
            }}
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

%%% Tests

import_legacy_checkpoint_test_() ->
    { timeout, 900, fun import_legacy_checkpoint/0 }.
import_legacy_checkpoint() ->
    application:ensure_all_started(hb),
    Opts = #{
        priv_wallet => hb:wallet(),
        genesis_wasm_import_authorities =>
            [<<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>]
    },
    ProcID = <<"DM3FoZUq_yebASPhgd8pEIRIzDW6muXEhxz5-JwbZwo">>,
    {ok, ProcWithCheckpoint} =
        hb_ao:resolve(
           << ProcID/binary, "~genesis-wasm@1.0/import">>,
           Opts
        ),
    ?assertMatch(
        Slot when Slot > 0,
        hb_maps:get(<<"at-slot">>, ProcWithCheckpoint)
    ),
    ?assertMatch(
        #{ <<"data">> := Data } when byte_size(Data) > 0,
        hb_maps:get(<<"snapshot">>, ProcWithCheckpoint)
    ),
    ?assertMatch(
        {ok, Slot, _} when Slot > 0,
        dev_process_cache:latest(ProcID, Opts)
    ),
    hb_ao:resolve(<<ProcID/binary, "~process@1.0/now">>, Opts).

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
    }, #{ priv_wallet => Wallet }).

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
        #{ priv_wallet => Wallet }
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
        #{ priv_wallet => Wallet }
    ).

test_genesis_wasm_process() ->
    Opts = #{
        genesis_wasm_db_dir => "cache-mainnet-test/genesis-wasm",
        genesis_wasm_checkpoints_dir => "cache-mainnet-test/genesis-wasm/checkpoints",
        genesis_wasm_log_level => "error",
        genesis_wasm_port => 6363,
        execution_device => <<"genesis-wasm@1.0">>
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
                <<"push-device">> => <<"push@1.0">>,
                <<"patch-from">> => <<"/results/outbox">>,
                <<"passes">> => 1,
                <<"scheduler">> => Address,
                <<"authority">> => Address,
                <<"module">> => <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>,
                <<"data-protocol">> => <<"ao">>,
                <<"type">> => <<"Process">>
            }),
        #{ priv_wallet => Wallet }
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
                        #{ priv_wallet => Wallet }
                    )
            },
            #{ priv_wallet => Wallet }
        ),
    hb_ao:resolve(Msg1, Msg2, #{}).

schedule_aos_call(Msg1, Code) ->
    schedule_aos_call(Msg1, Code, <<"Eval">>, #{}).
schedule_aos_call(Msg1, Code, Action) ->
    schedule_aos_call(Msg1, Code, Action, #{}).
schedule_aos_call(Msg1, Code, Action, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ProcID = hb_message:id(Msg1, all),
    Msg2 =
        hb_message:commit(
            #{
                <<"action">> => Action,
                <<"data">> => Code,
                <<"target">> => ProcID,
                <<"timestamp">> => os:system_time(millisecond)
            },
            #{ priv_wallet => Wallet }
        ),
    schedule_test_message(Msg1, <<"TEST MSG">>, Msg2).

spawn_and_execute_slot_test_() ->
    { timeout, 900, fun spawn_and_execute_slot/0 }.
spawn_and_execute_slot() ->
    application:ensure_all_started(hb),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store => hb_opts:get(store)
    },
    Msg1 = test_genesis_wasm_process(),
    hb_cache:write(Msg1, Opts),
    {ok, _SchedInit} = 
        hb_ao:resolve(
            Msg1,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Msg1
            },
            Opts
        ),
    {ok, _} = schedule_aos_call(Msg1, <<"return 1+1">>),
    {ok, _} = schedule_aos_call(Msg1, <<"return 2+2">>),
    {ok, SchedulerRes} =
        hb_ao:resolve(Msg1, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, Opts),
    % Verify process message is scheduled first
    ?assertMatch(
        <<"Process">>,
        hb_ao:get(<<"assignments/0/body/type">>, SchedulerRes)
    ),
    % Verify messages are scheduled
    ?assertMatch(
        <<"return 1+1">>,
        hb_ao:get(<<"assignments/1/body/data">>, SchedulerRes)
    ),
    ?assertMatch(
        <<"return 2+2">>,
        hb_ao:get(<<"assignments/2/body/data">>, SchedulerRes)
    ),
    {ok, Result} = hb_ao:resolve(Msg1, #{ <<"path">> => <<"now">> }, Opts),
    ?assertEqual(<<"4">>, hb_ao:get(<<"results/data">>, Result)).

compare_result_genesis_wasm_and_wasm_test_() ->
    { timeout, 900, fun compare_result_genesis_wasm_and_wasm/0 }.
compare_result_genesis_wasm_and_wasm() ->
    application:ensure_all_started(hb),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store => hb_opts:get(store)
    },
    % Test with genesis-wasm
    MsgGenesisWasm = test_genesis_wasm_process(),
    hb_cache:write(MsgGenesisWasm, Opts),
    {ok, _SchedInitGenesisWasm} =
        hb_ao:resolve(
            MsgGenesisWasm,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => MsgGenesisWasm
            },
            Opts
        ),
    % Test with wasm
    MsgWasm = test_wasm_stack_process(Opts, [
        <<"WASI@1.0">>,
        <<"JSON-Iface@1.0">>,
        <<"WASM-64@1.0">>,
        <<"Multipass@1.0">>
    ]),
    hb_cache:write(MsgWasm, Opts),
    {ok, _SchedInitWasm} =
        hb_ao:resolve(
            MsgWasm,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => MsgWasm
            },
            Opts
        ),
    % Schedule messages
    {ok, _} = schedule_aos_call(MsgGenesisWasm, <<"return 1+1">>),
    {ok, _} = schedule_aos_call(MsgGenesisWasm, <<"return 2+2">>),
    {ok, _} = schedule_aos_call(MsgWasm, <<"return 1+1">>),
    {ok, _} = schedule_aos_call(MsgWasm, <<"return 2+2">>),
    % Get results
    {ok, ResultGenesisWasm} = 
        hb_ao:resolve(
            MsgGenesisWasm,
            #{ <<"path">> => <<"now">> },
            Opts
        ),
    {ok, ResultWasm} = 
        hb_ao:resolve(
            MsgWasm,
            #{ <<"path">> => <<"now">> },
            Opts
        ),
    ?assertEqual(
        hb_ao:get(<<"results/data">>, ResultGenesisWasm),
        hb_ao:get(<<"results/data">>, ResultWasm)
    ).

send_message_between_genesis_wasm_processes_test_() ->
    { timeout, 900, fun send_message_between_genesis_wasm_processes/0 }.
send_message_between_genesis_wasm_processes() ->
    application:ensure_all_started(hb),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store => hb_opts:get(store)
    },
    % Create receiver process with handler
    MsgReceiver = test_genesis_wasm_process(),
    hb_cache:write(MsgReceiver, Opts),
    ProcId = dev_process:process_id(MsgReceiver, #{}, #{}),
    {ok, _SchedInitReceiver} =
        hb_ao:resolve(
            MsgReceiver,
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> => MsgReceiver
        },
        Opts
    ),
    schedule_aos_call(MsgReceiver, <<"Number = 10">>),
    schedule_aos_call(MsgReceiver, <<"
    Handlers.add('foo', function(msg)
        print(\"Number: \" .. Number * 2)
        return Number * 2 end)
    ">>),
    schedule_aos_call(MsgReceiver, <<"return Number">>),
    {ok, ResultReceiver} = hb_ao:resolve(MsgReceiver, <<"now">>, Opts),
    ?assertEqual(<<"10">>, hb_ao:get(<<"results/data">>, ResultReceiver)),
    % Create sender process to send message to receiver
    MsgSender = test_genesis_wasm_process(),
    hb_cache:write(MsgSender, Opts),
    {ok, _SchedInitSender} =
        hb_ao:resolve(
            MsgSender,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => MsgSender
            },
            Opts
        ),
    {ok, SendMsgToReceiver} =
        schedule_aos_call(
            MsgSender,
            <<"Send({ Target = \"", ProcId/binary, "\", Action = \"foo\" })">>
        ),
    {ok, ResultSender} = hb_ao:resolve(MsgSender, <<"now">>, Opts),
    {ok, Slot} = hb_ao:resolve(SendMsgToReceiver, <<"slot">>, Opts),
    {ok, Res} = 
        hb_ao:resolve(
            MsgSender,
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => Slot,
                <<"result-depth">> => 1
            },
            Opts
        ),
    % Get schedule for receiver
    {ok, ScheduleReceiver} =
        hb_ao:resolve(
            MsgReceiver,
            #{
                <<"method">> => <<"GET">>,
                <<"path">> => <<"schedule">>
            },
            Opts
        ),
    ?assertEqual(
        <<"foo">>,
        hb_ao:get(<<"assignments/4/body/action">>, ScheduleReceiver)
    ),
    {ok, NewResultReceiver} = hb_ao:resolve(MsgReceiver, <<"now">>, Opts),
    ?assertEqual(
        <<"Number: 20">>,
        hb_ao:get(<<"results/data">>, NewResultReceiver)
    ).

dryrun_genesis_wasm_test_() ->  
    { timeout, 900, fun dryrun_genesis_wasm/0 }.
dryrun_genesis_wasm() ->
    application:ensure_all_started(hb),
    Opts = #{
        priv_wallet => hb:wallet(),
        cache_control => <<"always">>,
        store => hb_opts:get(store)
    },
    % Set up process with increment handler to receive messages
    ProcReceiver = test_genesis_wasm_process(),
    hb_cache:write(ProcReceiver, #{}),
    {ok, _SchedInit1} = 
        hb_ao:resolve(
            ProcReceiver,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => ProcReceiver
            },
            Opts
        ),
    ProcReceiverId = dev_process:process_id(ProcReceiver, #{}, #{}),
    % Initialize increment handler
    {ok, _} = schedule_aos_call(ProcReceiver, <<"
    Number = Number or 5
    Handlers.add('Increment', function(msg) 
        Number = Number + 1
        ao.send({ Target = msg.From, Data = 'The current number is ' .. Number .. '!' })
        return 'The current number is ' .. Number .. '!'
    end)
    ">>),
    % Ensure Handlers were properly added
    schedule_aos_call(ProcReceiver, <<"return #Handlers.list">>),
    {ok, NumHandlers} =
        hb_ao:resolve(
            ProcReceiver,
            <<"now/results/data">>,
            Opts
        ),
    % _eval, _default, Increment
    ?assertEqual(<<"3">>, NumHandlers),

    schedule_aos_call(ProcReceiver, <<"return Number">>),
    {ok, InitialNumber} = 
        hb_ao:resolve(
            ProcReceiver, 
            <<"now/results/data">>,
            Opts
        ),
    % Number is initialized to 5
    ?assertEqual(<<"5">>, InitialNumber),
    % Set up sender process to send Action: Increment to receiver
    ProcSender = test_genesis_wasm_process(),
    hb_cache:write(ProcSender, #{}),
    {ok, _SchedInit2} = hb_ao:resolve(
        ProcSender,
        #{
            <<"method">> => <<"POST">>,
            <<"path">> => <<"schedule">>,
            <<"body">> => ProcSender
        },
        Opts
    ),
    % First increment + push
    {ok, ToPush}  = 
        schedule_aos_call(
            ProcSender,
            <<
                "Send({ Target = \"",
                (ProcReceiverId)/binary,
                "\", Action = \"Increment\" })"
            >>
        ),
    SlotToPush = hb_ao:get(<<"slot">>, ToPush, Opts),
    ?assertEqual(1, SlotToPush),
    {ok, PushRes1} = 
        hb_ao:resolve(
            ProcSender,
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => SlotToPush,
                <<"result-depth">> => 1
            },
            Opts
        ),
    % Check that number incremented normally
    schedule_aos_call(ProcReceiver, <<"return Number">>),
    {ok, AfterIncrementResult} =
        hb_ao:resolve(
            ProcReceiver, 
            <<"now/results/data">>, 
            Opts
        ),
    ?assertEqual(<<"6">>, AfterIncrementResult),

    % Send another increment and push it
    {ok, ToPush2}  = 
        schedule_aos_call(
            ProcSender,
            <<
                "Send({ Target = \"",
                (ProcReceiverId)/binary,
                "\", Action = \"Increment\" })"
            >>
        ),
    SlotToPush2 = hb_ao:get(<<"slot">>, ToPush2, Opts),
    ?assertEqual(3, SlotToPush2),
    {ok, PushRes2} = 
        hb_ao:resolve(
            ProcSender,
            #{
                <<"path">> => <<"push">>,
                <<"slot">> => SlotToPush2,
                <<"result-depth">> => 1
            },
            Opts
        ),
    % Check that number incremented normally
    schedule_aos_call(ProcReceiver, <<"return Number">>),
    {ok, AfterIncrementResult2} =
        hb_ao:resolve(
            ProcReceiver, 
            <<"now/results/data">>, 
            Opts
        ),
    ?assertEqual(<<"7">>, AfterIncrementResult2),
    % Test dryrun by calling compute with no assignment 
    % Should return result without changing state
    DryrunMsg =
        hb_message:commit(
            #{
                <<"path">> => <<"as/compute">>,
                <<"as-device">> => <<"execution">>,
                <<"action">> => <<"Increment">>,
                <<"target">> => ProcReceiverId
            },
            Opts
        ),
    {ok, DryrunResult} = hb_ao:resolve(ProcReceiver, DryrunMsg, Opts),
    {ok, DryrunData} = 
        hb_ao:resolve(DryrunResult, <<"results/outbox/1/Data">>, Opts),
    ?assertEqual(<<"The current number is 8!">>, DryrunData),
    % Ensure that number did not increment
    schedule_aos_call(ProcReceiver, <<"return Number">>),
    {ok, AfterDryrunResult} =
        hb_ao:resolve(
            ProcReceiver, 
            <<"now/results/data">>, 
            Opts
        ),
    ?assertEqual(<<"7">>, AfterDryrunResult).
-endif.