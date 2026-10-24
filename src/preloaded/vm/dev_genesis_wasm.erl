%%% @doc A device that mimics an environment suitable for `legacynet' AO 
%%% processes, using HyperBEAM infrastructure. This allows existing `legacynet'
%%% AO process definitions to be used in HyperBEAM.
-module(dev_genesis_wasm).
-specification("../../../specs/genesis-wasm@1.0.md").
-device_libraries([lib_process, lib_process_cache]).
-export([init/3, compute/3, normalize/3, snapshot/3, import/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%%% Timeout for legacy CU status check.
-define(STATUS_TIMEOUT, 100).
%%% Timeout for the legacy CU to start.
-define(START_TIMEOUT, 30000).

%% @doc Initialize the device.
-spec init(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) -> {ok, #{ _ => _ }}.
init(Msg, _Req, _Opts) -> {ok, Msg}.

%% @doc Normalize the device.
-spec normalize(
    #{ snapshot => #{ type => binary(), data => _, _ => _ }, _ => _ },
    #{ _ => _ },
    #{ _ => _ }
) ->
    {ok, #{ _ => _ }}
    | {error, #{ status := integer(), message := binary(), _ => _ }}.
normalize(Msg, Req, Opts) ->
    case ensure_started(Opts) of
        true ->
            hb_ao:resolve(
                Msg,
                {
                    as,
                    <<"delegated-compute@1.0">>,
                    Req#{ <<"path">> => <<"normalize">> }
                },
                delegated_opts(Opts)
            );
        false ->
            {error, #{
                <<"status">> => 500,
                <<"message">> => <<"Genesis-wasm server not running.">>
            }}
    end.

%% @doc Genesis-wasm device compute handler.
%% Normal compute execution through external CU with state persistence
-spec compute(
    #{ _ => _ },
    #{ slot => integer(), type => binary(), 'process-id' => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
compute(Msg, Req, Opts) ->
    % Give an assignment its `hash-chain', and the state the next one.
    {Msg2, Req2} = add_hash_chain(Msg, Req, Opts),
    % Validate whether the genesis-wasm feature is enabled.
    case delegate_request(Msg2, Req2, Opts) of
        {ok, Res} ->
            % Resolve the `patch@1.0' device.
            {ok, Msg4} =
                hb_ao:resolve(
                    Res,
                    {
                        as,
                        <<"patch@1.0">>,
                        Req#{ <<"patch-from">> => <<"/results/outbox">> }
                    },
                    Opts
                ),
            ?event({genesis_wasm_patched_message, Msg4}),
            % Return the patched message.
            {ok, Msg4};
        {skip, Res} ->
            ?event({genesis_wasm_skipping_duplicate, {req, Req}, {res, Res}, {msg, Msg}}),
            {ok, Msg};
        {error, Error} ->
            % Return the error.
            {error, Error};
        {failure, Failure} ->
            % The server did not compute the message, which is a failure of
            % this node.
            {failure, Failure}
    end.

%% @doc Give an assignment the `hash-chain' that the CU checks: its own, the
%% state's, or the process ID at slot 0. Set the state's `hash-chain' to the
%% chain of the next assignment, as the CU computes it: the SHA-256 of the
%% assignment's signed ID followed by its chain. After a skipped slot the CU
%% hashes its `original-assignment-id', the same signed ID. Other requests are
%% returned unchanged.
add_hash_chain(State, Req, Opts) ->
    case hb_ao:get(<<"type">>, Req, not_found, Opts) of
        <<"Assignment">> ->
            HashChain =
                case hb_maps:get_first(
                    [{Req, <<"hash-chain">>}, {State, <<"hash-chain">>}],
                    Opts
                ) of
                    not_found -> lib_process:process_id(State, #{}, Opts);
                    Found -> Found
                end,
            NextHashChain =
                hb_util:human_id(
                    hb_crypto:sha256_chain(
                        hb_util:native_id(hb_message:id(Req, signed, Opts)),
                        hb_util:native_id(HashChain)
                    )
                ),
            {
                hb_ao:set(State, #{ <<"hash-chain">> => NextHashChain }, Opts),
                Req#{ <<"hash-chain">> => HashChain }
            };
        _ ->
            {State, Req}
    end.

%% @doc Snapshot the state of the process via the `delegated-compute@1.0' device.
-spec snapshot(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {error, _}.
snapshot(Msg, Req, Opts) ->
    delegate_request(Msg, Req, Opts).

%% @doc Proxy a request to the delegated-compute@1.0 device, ensuring that
%% the server is running.
delegate_request(Msg, Req, Opts) ->
    % Validate whether the genesis-wasm feature is enabled.
    case ensure_started(Opts) of
        true ->
            do_compute(Msg, Req, Opts);
        false ->
            % This node cannot compute the message if the genesis-wasm
            % feature is disabled.
            {failure, #{
                <<"status">> => 500,
                <<"message">> =>
                    <<"HyperBEAM was not compiled with genesis-wasm@1.0 on "
                        "this node.">>
            }}
    end.


%% @doc Handle normal compute execution with state persistence (GET method).
do_compute(State, Req, Opts) ->
    maybe
        {ok, State2} ?=
            hb_ao:resolve(
                State,
                {as, <<"dedup@1.0">>, Req},
                Opts
            ),
        ?event(dedup_short,
            {continue,
                {path, hb_maps:get(<<"path">>, Req, no_path, Opts)},
                {assignment_slot, hb_maps:get(<<"slot">>, Req, no_slot, Opts)},
                {state_slot, hb_maps:get(<<"at-slot">>, State, no_slot, Opts)},
                {input, hb_ao:get(<<"body/data">>, Req, no_input, Opts)}
            }
        ),
        {ok, State3} ?=
            hb_ao:resolve(
                State2,
                {as, <<"delegated-compute@1.0">>, Req},
                delegated_opts(Opts)
            ),
        {ok, State4} ?=
            hb_ao:resolve(
                State3,
                {
                    as,
                    <<"patch@1.0">>,
                    Req#{ <<"patch-from">> => <<"/results/outbox">> }
                },
                Opts
            ),
        ?event(dedup_short,
            {result, hb_ao:get(<<"results/data">>, State4, no_data, Opts)}
        ),
        {ok, State4}
    else
        {error, Error} ->
            % Issue an event and return the error.
            ?event({genesis_wasm_compute_error, Error}),
            {error, Error};
        {failure, Failure} -> {failure, Failure};
        {skip, DoubleSkip = #{ <<"skip">> := true }} ->
            ?event(dedup_short,
                {dedup_error,
                    {cause, double_skip},
                    {skip_request, DoubleSkip}
                }
            ),
            {error, State};
        {skip, ExitState} ->
            % The CU needs a signed message at every slot. A skipped slot
            % sends its assignment as that message, the same on every node.
            % The CU drops the repeated message itself if it has `from-process'.
            ReqWithoutCommitments = hb_message:uncommitted_deep(Req, Opts),
            Req2 =
                ReqWithoutCommitments#{
                    <<"path">> =>
                        hb_maps:get(<<"path">>, Req, <<"compute">>, Opts),
                    <<"slot">> =>
                        hb_maps:get(<<"slot">>, Req, -1, Opts),
                    <<"skip">> => true,
                    <<"original-assignment-id">> =>
                        hb_message:id(Req, signed, Opts),
                    <<"body">> => Req
                },
            ?event(dedup_short,
                {skip,
                    {cause, dedup},
                    {action, run_no_op},
                    {path, hb_maps:get(<<"path">>, Req, no_path, Opts)},
                    {assignment_slot, hb_maps:get(<<"slot">>, Req, no_slot, Opts)},
                    {state_slot, hb_maps:get(<<"at-slot">>, State, no_slot, Opts)}
                }
            ),
            do_compute(ExitState, Req2, Opts)
    end.

%% @doc Bind delegated requests to this node's Genesis server.
delegated_opts(Opts) ->
    Port = integer_to_binary(hb_opts:get(genesis_wasm_port, 6363, Opts)),
    Opts#{
        <<"delegated-compute-peer">> => <<"http://localhost:", Port/binary>>
    }.

%% @doc Ensure the local `genesis-wasm@1.0' is live. If it is not, start it,
%% and return `false' if it does not answer within `?START_TIMEOUT' ms.
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
                                            {"WASM_BINARY_FILE_DIRECTORY", DBDir},
                                            {"PROCESS_MEMORY_CACHE_FILE_DIR",
                                                filename:join(DBDir, "memory-cache")
                                            },
                                            {"NODE_CONFIG_ENV", "production"},
                                            {"DEFAULT_LOG_LEVEL",
                                                hb_util:list(
                                                    hb_opts:get(
                                                        genesis_wasm_log_level,
                                                        "debug",
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
											{"PROCESS_MEMORY_FILE_CHECKPOINTS_DIR", CheckpointDir},
                                            {"PROCESS_MEMORY_CACHE_MAX_SIZE",
                                                hb_util:list(
                                                    hb_opts:get(
                                                        genesis_wasm_memory_cache_max_size,
                                                        "12_000_000_000",
                                                        Opts
                                                    )
                                                )
                                            },
                                            {"PROCESS_WASM_SUPPORTED_EXTENSIONS",
                                                hb_util:list(
                                                    hb_opts:get(
                                                        genesis_wasm_supported_extensions,
                                                        "WeaveDrive",
                                                        Opts
                                                    )
                                                )
                                            },
                                            {"PROCESS_WASM_MEMORY_MAX_LIMIT",
                                                hb_util:list(
                                                    hb_opts:get(
                                                        genesis_wasm_memory_max_limit,
                                                        "24_000_000_000",
                                                        Opts
                                                    )
                                                )
                                            }
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
            hb_util:wait_until(
                fun() ->
                    Status = is_genesis_wasm_server_running(Opts),
                    ?event({genesis_wasm_boot_wait, {received_status, Status}}),
                    Status
                end,
                ?START_TIMEOUT
            )
    end.

%% @doc Find either a specific checkpoint by its ID, or find the most recent
%% checkpoint via GraphQL.
-spec import(
    #{ _ => _ },
    #{ import => binary(), 'process-id' => binary(), _ => _ },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _}.
import(Base, Req, Opts) ->
    PassedProcID = hb_maps:find(<<"process-id">>, Req, Opts),
    ProcMsg =
        case PassedProcID of
            {ok, ProcessId} ->
                {ok, CacheProcMsg} = hb_cache:read(ProcessId, Opts),
                CacheProcMsg;
            error ->
                Base
        end,
    case hb_maps:find(<<"import">>, Req, Opts) of
        {ok, ImportID} ->
            case hb_cache:read(ImportID, Opts) of
                {ok, CheckpointMessage} ->
                    do_import(ProcMsg, CheckpointMessage, Opts);
                not_found -> {error, not_found}
            end;
        error ->
            ProcID = lib_process:process_id(ProcMsg, #{}, Opts),
            case latest_checkpoint(ProcID, Opts) of
                {ok, CheckpointMessage} ->
                    do_import(ProcMsg, CheckpointMessage, Opts);
                Err -> Err
            end
    end.

%% @doc Find the most recent legacy checkpoint for a process.
latest_checkpoint(ProcID, Opts) ->
    case import_authorities(Opts) of
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
            (hb_client_gateway:item_spec())/binary,
            """
                }
            }}
        """>>,
    Variables =
        #{
            <<"ProcID">> => ProcID,
            % GraphQL finds the items of the committer of an Arweave address
            % by the address.
            <<"TrustedSigners">> =>
                [
                    Address
                ||
                    SignerURI <- TrustedSigners,
                    {ok, Address} <- [hb_committer:address(SignerURI)]
                ]
        },
    case hb_client_gateway:query(Query, Variables, Opts) of
        {error, Reason} ->
            {error, Reason};
        {ok, GqlMsg} ->
            ?event(debug_proc_id, {gql_msg, GqlMsg}),
            case hb_ao:get(<<"data/transactions/edges/1/node">>, GqlMsg, Opts) of
                not_found -> {error, not_found};
                Item -> hb_client_gateway:result_to_message(Item, Opts)
            end
    end.

%% @doc Return the committers in the `genesis-wasm-import-authorities'
%% option.
import_authorities(Opts) ->
    [
        hb_committer:uri(Authority, Opts)
    ||
        Authority <- hb_opts:get(genesis_wasm_import_authorities, [], Opts)
    ].

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
        TrustedSigners = import_authorities(Opts),
        true ?=
            lists:any(
                fun(Signer) -> lists:member(Signer, TrustedSigners) end,
                CheckpointSigners
            ) orelse untrusted,
        true ?= hb_message:verify(CheckpointMessage, all, Opts) orelse unverified,
        CheckpointTargetProcID = hb_maps:get(<<"process">>, CheckpointMessage, Opts),
        ProcID = lib_process:process_id(Proc, #{}, Opts),
        true ?= CheckpointTargetProcID == ProcID orelse process_mismatch,
        % Normalize the checkpoint message into a process state message with 
        % a state snapshot.
        {ok, SlotBin} ?= hb_maps:find(<<"nonce">>, CheckpointMessage, Opts),
        Slot = hb_util:int(SlotBin),
        InitializedProc = lib_process:ensure_process_key(Proc, Opts),
        WithSnapshot =
            InitializedProc#{
                <<"at-slot">> => Slot,
                <<"snapshot">> => CheckpointMessage
            },
        % Save the state snapshot into the store.
        {ok, _} ?= lib_process_cache:write(ProcID, Slot, WithSnapshot, Opts),
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

%% @doc Check if the genesis-wasm server is running. A `true' status is kept
%% for the calling process, so later checks in it do not ask the server again.
is_genesis_wasm_server_running(Opts) ->
    case get(genesis_wasm_pid) of
        true -> true;
        _ ->
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
            end
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

%% @doc Signed imports are readable through the shared process cache library.
signed_checkpoint_import_test() ->
    Wallet = ar_wallet:new(),
    Opts = #{
        <<"store">> => [hb_test_utils:test_store()],
        <<"priv-wallet">> => Wallet,
        <<"genesis-wasm-import-authorities">> =>
            [hb_test_utils:committer(Wallet)]
    },
    Proc = hb_message:commit(#{
        <<"device">> => <<"process@1.0">>,
        <<"execution-device">> => <<"genesis-wasm@1.0">>,
        <<"type">> => <<"Process">>
    }, Opts),
    ProcID = hb_message:id(Proc, signed, Opts),
    {ok, _} = hb_cache:write(Proc, Opts),
    Checkpoint = hb_message:commit(#{
        <<"type">> => <<"Checkpoint">>,
        <<"process">> => ProcID,
        <<"nonce">> => <<"2">>,
        <<"data">> => zlib:gzip(<<"checkpoint memory">>)
    }, Opts),
    CheckpointID = hb_message:id(Checkpoint, signed, Opts),
    {ok, _} = hb_cache:write(Checkpoint, Opts),
    Req = {as, <<"genesis-wasm@1.0">>, #{
        <<"path">> => <<"import">>,
        <<"process-id">> => ProcID,
        <<"import">> => CheckpointID
    }},
    ?assertMatch({error, #{ <<"status">> := 400 }},
        hb_ao:resolve(Proc, Req,
            Opts#{ <<"genesis-wasm-import-authorities">> => [] })),
    ?assertEqual({error, not_found}, lib_process_cache:latest(ProcID, Opts)),
    {ok, Imported} = hb_ao:resolve(Proc, Req, Opts),
    ?assertEqual(2, hb_maps:get(<<"at-slot">>, Imported, Opts)),
    {ok, 2, Cached} = lib_process_cache:latest(ProcID, Opts),
    Snapshot = hb_maps:get(<<"snapshot">>, Cached, Opts),
    ?assert(hb_message:verify(Snapshot, all, Opts)),
    ?assertEqual(CheckpointID, hb_message:id(Snapshot, signed, Opts)),
    % `latest-checkpoint' is not a key of the device.
    ?assertEqual(
        {error, not_found},
        hb_ao:resolve(
            #{ <<"device">> => <<"genesis-wasm@1.0">> },
            #{
                <<"path">> => <<"latest-checkpoint">>,
                <<"genesis-wasm-import-authorities">> => []
            },
            Opts
        )
    ).

%% @doc Genesis requests stay on their configured server, independently of
%% the routing table used by standalone delegated computation.
instance_routing_test_() ->
    {timeout, 60, fun instance_routing/0}.

instance_routing() ->
    Nodes = [routing_test_node(self()), routing_test_node(self())],
    try
        [{First, _}, {Second, _}] = Nodes,
        lists:foreach(
            fun({Device, Peer, RoutedPeer}) ->
                #{port := Port} = uri_string:parse(Peer),
                Opts = #{
                    <<"store">> => [hb_test_utils:test_store()],
                    <<"priv-wallet">> => ar_wallet:new(),
                    <<"genesis-wasm-port">> => Port,
                    <<"cache-control">> => [<<"no-store">>, <<"no-cache">>],
                    <<"routes">> => [#{
                        <<"template">> => <<"/.*">>,
                        <<"node">> => #{ <<"prefix">> => RoutedPeer }
                    }]
                },
                Process = hb_message:commit(#{ <<"type">> => <<"Process">> }, Opts),
                ProcID = hb_message:id(Process, signed, Opts),
                Base = #{ <<"device">> => Device, <<"process">> => Process },
                % Warm the real HTTP endpoint before Genesis's bounded probe.
                {ok, _} = hb_http:get(Peer, <<"/status">>, Opts),
                erase(genesis_wasm_pid),
                lists:foreach(
                    fun({State, Req, Path}) ->
                        {ok, _} = hb_ao:resolve(State, Req, Opts),
                        Received = receive
                            {routing_request, ActualPeer, #{
                                <<"method">> := <<"POST">>
                            } = Incoming} -> {ActualPeer, Incoming}
                        after 5_000 -> error(genesis_request_timeout)
                        end,
                        {Peer, Request} = Received,
                        ?assertEqual(Path, hb_maps:get(<<"path">>, Request, Opts))
                    end,
                    [
                        {Base, #{ <<"path">> => <<"snapshot">> },
                            <<"/snapshot/", ProcID/binary>>},
                        {Base#{ <<"snapshot">> => #{
                            <<"type">> => <<"Checkpoint">>,
                            <<"content-type">> => <<"application/octet-stream">>,
                            <<"data">> => <<"checkpoint">>
                        }}, #{ <<"path">> => <<"normalize">> }, <<"/state">>},
                        {Base, #{ <<"path">> => <<"compute">> },
                            <<"/dry-run?process-id=", ProcID/binary>>},
                        {Base, #{
                            <<"path">> => <<"compute">>,
                            <<"type">> => <<"Assignment">>,
                            <<"slot">> => 0,
                            <<"body">> => hb_message:commit(
                                #{ <<"data">> => <<"test">> }, Opts
                            )
                        }, <<"/result/0?process-id=", ProcID/binary>>}
                    ]
                )
            end,
            [
                {<<"genesis-wasm@1.0">>, First, Second},
                {<<"genesis-wasm@1.0">>, Second, First},
                {<<"delegated-compute@1.0">>, First, First}
            ]
        )
    after
        erase(genesis_wasm_pid),
        lists:foreach(fun({_, ID}) -> cowboy:stop_listener(ID) end, Nodes)
    end.

%% @doc Use isolated HyperBEAM HTTP nodes to observe the outgoing requests.
routing_test_node(Parent) ->
    Wallet = ar_wallet:new(),
    ID = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Node = hb_http_server:start_node(#{
        <<"port">> => 0,
        <<"priv-wallet">> => Wallet,
        <<"store">> => [hb_test_utils:test_store()],
        <<"force-signed">> => false,
        <<"on">> => #{ <<"request">> => #{ <<"device">> => #{
            <<"request">> => fun(_, #{ <<"request">> := Req }, _) ->
                Port = integer_to_binary(ranch:get_port(ID)),
                Parent ! {routing_request,
                    <<"http://127.0.0.1:", Port/binary, "/">>, Req},
                {error, #{
                    <<"status">> => 200,
                    <<"content-type">> => <<"application/json">>,
                    <<"body">> => <<"{\"Output\":{\"data\":\"ok\"},\"Messages\":[]}">>
                }}
            end
        }}}
    }),
    {Node, ID}.

-ifdef(ENABLE_GENESIS_WASM).

profile_enabled_test() ->
    ?assert(hb_features:genesis_wasm()).

%% @doc Give each real-engine test its own node, wallet, ports and stores.
genesis_test(Fun) ->
    {timeout, 900, fun() ->
        erase(genesis_wasm_pid),
        Ignored = os:getenv("PROCESS_IGNORE_ARWEAVE_CHECKPOINTS"),
        Store = hb_test_utils:test_store(),
        Dir = hb_util:list(hb_maps:get(<<"name">>, Store)),
        Wallet = ar_wallet:new(),
        KeyFile = filename:join(Dir, "wallet.json"),
        ok = filelib:ensure_dir(KeyFile),
        ok = file:write_file(KeyFile, ar_wallet:to_json(Wallet)),
        ok = file:change_mode(KeyFile, 8#600),
        {ok, Socket} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
        {ok, GenesisPort} = inet:port(Socket),
        ok = gen_tcp:close(Socket),
        Opts0 = #{
            <<"port">> => 0,
            <<"mode">> => debug,
            <<"store">> => [Store],
            <<"priv-wallet">> => Wallet,
            <<"priv-key-location">> => KeyFile,
            <<"force-signed">> => false,
            <<"scheduling-mode">> => local_confirmation,
            <<"bundler-httpsig">> => not_found,
            <<"bundler-ans104">> => <<"http://127.0.0.1:1">>,
            <<"genesis-wasm-port">> => GenesisPort,
            <<"genesis-wasm-db-dir">> => Dir,
            <<"genesis-wasm-log-level">> => "error",
            <<"genesis-wasm-memory-cache-max-size">> => "1073741824",
            <<"genesis-wasm-memory-max-limit">> => "2147483648"
        },
        seed_test_module_cache(Dir),
        Node = hb_http_server:start_node(Opts0),
        #{port := Port} = uri_string:parse(Node),
        try Fun(Opts0#{ <<"port">> => Port })
        after
            case hb_name:lookup(<<"genesis-wasm@1.0">>) of
                PID when is_pid(PID) ->
                    Ref = monitor(process, PID),
                    PID ! stop,
                    receive {'DOWN', Ref, process, PID, _} -> ok
                    after 5000 -> error(genesis_cleanup_timeout)
                    end;
                _ -> ok
            end,
            erase(genesis_wasm_pid),
            case Ignored of
                false -> os:unsetenv("PROCESS_IGNORE_ARWEAVE_CHECKPOINTS");
                _ -> os:putenv("PROCESS_IGNORE_ARWEAVE_CHECKPOINTS", Ignored)
            end,
            cowboy:stop_listener(hb_util:human_id(ar_wallet:to_address(Wallet)))
        end
    end}.

%% @doc An optional module-only cache allows real WASM tests without egress.
seed_test_module_cache(Dir) ->
    case os:getenv("HB_GENESIS_TEST_CACHE") of
        false -> ok;
        CacheDir ->
            lists:foreach(
                fun(Path) ->
                    {ok, _} = file:copy(Path,
                        filename:join(Dir, filename:basename(Path)))
                end,
                filelib:wildcard(filename:join(CacheDir, "*.wasm.gz")) ++
                [filename:join(CacheDir, "genesis-wasm-db.sqlite")]
            )
    end.

%% @doc Signed checkpoints restore WASM memory and resume execution.
checkpoint_roundtrip_test_() ->
    genesis_test(fun checkpoint_roundtrip/1).
checkpoint_roundtrip(Opts) ->
    Base = test_genesis_wasm_process(Opts),
    ProcID = hb_message:id(Base, signed, Opts),
    {ok, _} = hb_cache:write(Base, Opts),
    {ok, _} = hb_ao:resolve(Base, #{
        <<"path">> => <<"schedule">>, <<"method">> => <<"POST">>,
        <<"body">> => Base
    }, Opts),
    {ok, _} = schedule_aos_call(Base, <<"Number = 41; return Number">>, Opts),
    {ok, State} = hb_ao:resolve(Base,
        #{ <<"path">> => <<"compute">>, <<"slot">> => 1 }, Opts),
    ?assertEqual(<<"41">>, hb_ao:get(<<"results/data">>, State, Opts)),
    {Meta, Memory} = test_snapshot(State, Opts),
    % The state gives slot 2 the chain that the CU computes from slot 1.
    ?assertEqual(
        hb_util:human_id(
            hb_crypto:sha256_chain(
                hb_util:native_id(hb_maps:get(<<"assignmentId">>, Meta)),
                hb_util:native_id(hb_maps:get(<<"hashChain">>, Meta))
            )
        ),
        hb_ao:get(<<"hash-chain">>, State, Opts)
    ),
    Checkpoint = hb_message:commit(#{
        <<"type">> => <<"Checkpoint">>,
        <<"process">> => ProcID,
        <<"module">> => hb_maps:get(<<"moduleId">>, Meta),
        <<"assignment">> => hb_maps:get(<<"assignmentId">>, Meta),
        <<"nonce">> => hb_util:bin(hb_maps:get(<<"nonce">>, Meta)),
        <<"epoch">> => hb_util:bin(hb_maps:get(<<"epoch">>, Meta)),
        <<"timestamp">> => hb_util:bin(hb_maps:get(<<"timestamp">>, Meta)),
        <<"block-height">> => hb_util:bin(hb_maps:get(<<"blockHeight">>, Meta)),
        <<"content-type">> => <<"application/octet-stream">>,
        <<"data">> => zlib:gzip(Memory)
    }, Opts),
    {ok, _} = hb_cache:write(Checkpoint, Opts),
    {ok, Imported} = hb_ao:resolve(Base,
        {as, <<"genesis-wasm@1.0">>, #{
            <<"path">> => <<"import">>,
            <<"process-id">> => ProcID,
            <<"import">> => hb_message:id(Checkpoint, signed, Opts)
        }},
        Opts#{ <<"genesis-wasm-import-authorities">> =>
            hb_message:signers(Checkpoint, Opts) }
    ),
    PID = hb_name:lookup(<<"genesis-wasm@1.0">>),
    Ref = monitor(process, PID),
    PID ! stop,
    receive {'DOWN', Ref, process, PID, _} -> ok
    after 5000 -> error(genesis_stop_timeout)
    end,
    erase(genesis_wasm_pid),
    {ok, Socket} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Dir = filename:join(hb_maps:get(<<"genesis-wasm-db-dir">>, Opts), "restored"),
    ok = filelib:ensure_dir(filename:join(Dir, "genesis-wasm-db.sqlite")),
    seed_test_module_cache(Dir),
    RestoreOpts = Opts#{
        <<"genesis-wasm-port">> => Port,
        <<"genesis-wasm-db-dir">> => Dir
    },
    Normalize = {as, <<"genesis-wasm@1.0">>, #{ <<"path">> => <<"normalize">> }},
    {ok, Restored} = hb_ao:resolve(Imported, Normalize, RestoreOpts),
    {_, RestoredMemory} = test_snapshot(Restored, RestoreOpts),
    ?assertEqual(byte_size(Memory), byte_size(RestoredMemory)),
    ?assertEqual(crypto:hash(sha256, Memory), crypto:hash(sha256, RestoredMemory)),
    ?assertMatch({error, _}, hb_ao:resolve(
        Restored#{ <<"snapshot">> => #{
            <<"type">> => <<"Checkpoint">>,
            <<"content-type">> => <<"application/octet-stream">>,
            <<"data">> => <<"not-gzip">>
        }}, Normalize, RestoreOpts)),
    {ok, _} = schedule_aos_call(Base, <<"Number = Number + 1; return Number">>, Opts),
    {ok, Continued} = hb_ao:resolve(Restored,
        {as, <<"process@1.0">>, #{ <<"path">> => <<"compute">>, <<"slot">> => 2 }},
        RestoreOpts),
    ?assertEqual(<<"42">>, hb_ao:get(<<"results/data">>, Continued, RestoreOpts)).

%% @doc Read the checkpoint produced by the actual Genesis server.
test_snapshot(State, Opts) ->
    {ok, Reply} = hb_ao:resolve(State,
        {as, <<"genesis-wasm@1.0">>, #{ <<"path">> => <<"snapshot">> }}, Opts),
    Details = hb_json:decode(hb_maps:get(<<"body">>, Reply, Opts)),
    ?assertEqual(true, hb_maps:get(<<"success">>, Details)),
    {ok, Memory} = file:read_file(filename:join([
        hb_maps:get(<<"genesis-wasm-db-dir">>, Opts),
        "checkpoints", hb_maps:get(<<"file">>, Details)
    ])),
    {hb_maps:get(<<"snapshot">>, Details), Memory}.

import_legacy_checkpoint_test_() ->
    { timeout, 900, fun import_legacy_checkpoint/0 }.
import_legacy_checkpoint() ->
    application:ensure_all_started(hb),
    Opts = #{
        <<"priv-wallet">> => hb:wallet(),
        <<"genesis-wasm-import-authorities">> =>
            [
                hb_committer:uri(<<"fcoN_xJeisVsPXA-trzVAuIiqO3ydLQxM-L4XbrQKzY">>),
                hb_committer:uri(<<"WjnS-s03HWsDSdMnyTdzB1eHZB2QheUWP_FVRVYxkXk">>)
            ]
    },
    % Process with 12 slots
    ProcID = <<"0Y6DdqejAqhmdlq6aJiFCOb3cIKYoPm49_Fzt08AvMs">>,
    % Checkpoint at slot 10
    CheckpointID = <<"p4GUwmzKf4RaD5xtGpTucGhdwukgAtIAclkhTk3Qv2Y">>,
    ExpectedSlot = 10,
    {ok, ProcWithCheckpoint} =
        hb_ao:resolve(
            <<
                "~genesis-wasm@1.0/import=",
                CheckpointID/binary,
                "&process-id=",
                ProcID/binary
            >>,
            Opts
        ),
    ?assertMatch(
        ExpectedSlot,
        hb_maps:get(<<"at-slot">>, ProcWithCheckpoint)
    ),
    Snapshot = hb_maps:get(<<"snapshot">>, ProcWithCheckpoint, not_found, Opts),
    SnapshotData = hb_maps:get(<<"data">>, Snapshot, not_found, Opts),
    ?assert(byte_size(SnapshotData) > 0),
    ?assertMatch(
        {ok, Slot, _} when Slot > 0,
        lib_process_cache:latest(ProcID, Opts)
    ),
    {ok, ActualSlot} =
        hb_ao:resolve(<<ProcID/binary, "~process@1.0/compute/at-slot">>, Opts),
    ?assertEqual(ExpectedSlot, ActualSlot),
    NextSlot = hb_util:bin(ActualSlot + 1),
    {ok, OutboxTarget} =
        hb_ao:resolve(
            <<
                ProcID/binary,
                "~process@1.0/compute&slot=",
                NextSlot/binary,
                "/results/outbox/1/Target"
            >>,
            Opts
        ),
    % The next slot (11) pushes a message targeting the below process.
    ?assertEqual(OutboxTarget, <<"_s_pwnSLoguEEst3QpZiTAoWhRc4iRawVxOnzU443IM">>),
    % Attempting to compute the previous slot should throw an error.
    PreviousSlot = hb_util:bin(ActualSlot - 1),
    ?assertThrow(
        _,
        hb_ao:resolve(
            <<ProcID/binary, "~process@1.0/compute&slot=", PreviousSlot/binary>>,
            Opts
        )
    ).

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
    }, #{ <<"priv-wallet">> => Wallet }).

test_wasm_process(WASMImage) ->
    test_wasm_process(WASMImage, #{}).
test_wasm_process(WASMImage, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    #{ <<"image">> := WASMImageID } =
        hb_wasm_test_utils:cache_image(WASMImage, Opts),
    hb_message:commit(
        maps:merge(
            hb_message:uncommitted(test_base_process(Opts)),
            #{
                <<"execution-device">> => <<"stack@1.0">>,
                <<"device-stack">> => [<<"wasm-64@1.0">>],
                <<"image">> => WASMImageID
            }
        ),
        #{ <<"priv-wallet">> => Wallet }
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
                    <<"execution-device">> => <<"stack@1.0">>,
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
        #{ <<"priv-wallet">> => Wallet }
    ).

test_genesis_wasm_process(Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WASMProc = test_wasm_process(<<"test/aos-2-pure-xs.wasm">>, Opts),
    Proc = hb_message:commit(
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
        #{ <<"priv-wallet">> => Wallet }
    ),
    ProcID = hb_message:id(Proc, signed, Opts),
    Ignored = os:getenv("PROCESS_IGNORE_ARWEAVE_CHECKPOINTS", ""),
    os:putenv("PROCESS_IGNORE_ARWEAVE_CHECKPOINTS",
        hb_util:list(ProcID) ++ "," ++ Ignored),
    Proc.

schedule_test_message(Base, Text, MsgBase, Opts) ->
    Wallet = hb_opts:get(priv_wallet, no_wallet, Opts),
    UncommittedBase = hb_message:uncommitted(MsgBase),
    Req =
        hb_message:commit(#{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        UncommittedBase#{
                            <<"type">> => <<"Message">>,
                            <<"test-label">> => Text
                        },
                        #{ <<"priv-wallet">> => Wallet }
                    )
            },
            #{ <<"priv-wallet">> => Wallet }
        ),
    hb_ao:resolve(Base, Req, Opts).

schedule_aos_call(Base, Code, Opts) ->
    schedule_aos_call(Base, Code, <<"Eval">>, Opts).
schedule_aos_call(Base, Code, Action, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ProcID = hb_message:id(Base, all),
    Req =
        hb_message:commit(
            #{
                <<"action">> => Action,
                <<"data">> => Code,
                <<"target">> => ProcID,
                <<"timestamp">> => os:system_time(millisecond)
            },
            #{ <<"priv-wallet">> => Wallet }
        ),
    schedule_test_message(Base, <<"TEST MSG">>, Req, Opts).

dedup_test_() ->
    genesis_test(fun dedup/1).
dedup(Opts) ->
    Base = test_genesis_wasm_process(Opts),
    hb_cache:write(Base, Opts),
    ProcID = hb_message:id(Base, all),
    {ok, _SchedInit} =
        hb_ao:resolve(
            Base,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Base
            },
            Opts
        ),
    schedule_aos_call(Base, <<"Number = 1">>, Opts),
    % Manually triple schedule the same message base
    MsgBase = 
        hb_message:commit(
            #{
                <<"action">> => <<"Eval">>,
                <<"data">> => <<"Number = Number + 1; return Number">>,
                <<"target">> => ProcID,
                <<"timestamp">> => os:system_time(millisecond)
            },
            Opts
        ),
    UncommittedBase = hb_message:uncommitted(MsgBase),
    Req =
        hb_message:commit(
            #{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        UncommittedBase#{
                            <<"type">> => <<"Message">>,
                            <<"test-label">> => <<"TEST MSG">>
                        },
                        Opts
                    )
            },
            Opts
        ),
    % Schedule the message thrice
    {ok, _} = hb_ao:resolve(Base, Req, Opts),
    {ok, _} = hb_ao:resolve(Base, Req, Opts),
    {ok, _} = hb_ao:resolve(Base, Req, Opts),
    % Ensure the message is scheduled twice
    {ok, SchedulerRes} =
        hb_ao:resolve(
            Base, 
            <<"schedule">>,
            Opts
        ),
    % Assert successful double schedule
    ?assertEqual(
        hb_private:reset(
            hb_ao:get(<<"assignments/2/body/commitments">>, SchedulerRes)
        ),
        hb_private:reset(
            hb_ao:get(<<"assignments/3/body/commitments">>, SchedulerRes)
        )
    ),
    ?assertEqual(
        hb_private:reset(
            hb_ao:get(<<"assignments/3/body/commitments">>, SchedulerRes)
        ),
        hb_private:reset(
            hb_ao:get(<<"assignments/4/body/commitments">>, SchedulerRes)
        )
    ),
    % Schedule twice to avoid nonce warning
    schedule_aos_call(Base, <<"return Number">>, Opts),
    schedule_aos_call(Base, <<"return Number">>, Opts),
    % Compute with dedup - initialize number to 1, then two increments,
    % but the second increment should be skipped for dedup - expected result is 2
    {ok, Result} = hb_ao:resolve(Base, <<"now">>, Opts),
    Data = hb_ao:get(<<"results/data">>, Result),
    ?assertEqual(<<"2">>, Data).
spawn_and_execute_slot_test_() ->
    genesis_test(fun spawn_and_execute_slot/1).
spawn_and_execute_slot(Opts) ->
    Base = test_genesis_wasm_process(Opts),
    hb_cache:write(Base, Opts),
    {ok, _SchedInit} = 
        hb_ao:resolve(
            Base,
            #{
                <<"method">> => <<"POST">>,
                <<"path">> => <<"schedule">>,
                <<"body">> => Base
            },
            Opts
        ),
    {ok, _} = schedule_aos_call(Base, <<"return 1+1">>, Opts),
    {ok, _} = schedule_aos_call(Base, <<"return 2+2">>, Opts),
    {ok, SchedulerRes} =
        hb_ao:resolve(Base, #{
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
    {ok, Result} = hb_ao:resolve(Base, #{ <<"path">> => <<"now">> }, Opts),
    ?assertEqual(<<"4">>, hb_ao:get(<<"results/data">>, Result)).

compare_result_genesis_wasm_and_wasm_test_() ->
    genesis_test(fun compare_result_genesis_wasm_and_wasm/1).
compare_result_genesis_wasm_and_wasm(Opts) ->
    % Test with genesis-wasm
    MsgGenesisWasm = test_genesis_wasm_process(Opts),
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
        <<"wasi@1.0">>,
        <<"json-iface@1.0">>,
        <<"wasm-64@1.0">>,
        <<"multipass@1.0">>
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
    {ok, _} = schedule_aos_call(MsgGenesisWasm, <<"return 1+1">>, Opts),
    {ok, _} = schedule_aos_call(MsgGenesisWasm, <<"return 2+2">>, Opts),
    {ok, _} = schedule_aos_call(MsgWasm, <<"return 1+1">>, Opts),
    {ok, _} = schedule_aos_call(MsgWasm, <<"return 2+2">>, Opts),
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
    genesis_test(fun send_message_between_genesis_wasm_processes/1).
send_message_between_genesis_wasm_processes(Opts) ->
    % Create receiver process with handler
    MsgReceiver = test_genesis_wasm_process(Opts),
    MsgSender = test_genesis_wasm_process(Opts),
    hb_cache:write(MsgReceiver, Opts),
    ProcId = lib_process:process_id(MsgReceiver, #{}, #{}),
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
    schedule_aos_call(MsgReceiver, <<"Number = 10">>, Opts),
    schedule_aos_call(MsgReceiver, <<"
    Handlers.add('foo', function(msg)
        print(\"Number: \" .. Number * 2)
        return Number * 2 end)
    ">>, Opts),
    schedule_aos_call(MsgReceiver, <<"return Number">>, Opts),
    {ok, ResultReceiver} = hb_ao:resolve(MsgReceiver, <<"now">>, Opts),
    ?assertEqual(<<"10">>, hb_ao:get(<<"results/data">>, ResultReceiver)),
    % Create sender process to send message to receiver
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
            <<"Send({ Target = \"", ProcId/binary, "\", Action = \"foo\" })">>,
            Opts
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
    genesis_test(fun dryrun_genesis_wasm/1).
dryrun_genesis_wasm(Opts) ->
    % Set up process with increment handler to receive messages
    ProcReceiver = test_genesis_wasm_process(Opts),
    ProcSender = test_genesis_wasm_process(Opts),
    hb_cache:write(ProcReceiver, Opts),
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
    ProcReceiverId = lib_process:process_id(ProcReceiver, #{}, #{}),
    % Initialize increment handler
    {ok, _} = schedule_aos_call(ProcReceiver, <<"
    Number = Number or 5
    Handlers.add('Increment', function(msg) 
        Number = Number + 1
        ao.send({ Target = msg.From, Data = 'The current number is ' .. Number .. '!' })
        return 'The current number is ' .. Number .. '!'
    end)
    ">>, Opts),
    % Ensure Handlers were properly added
    schedule_aos_call(ProcReceiver, <<"return #Handlers.list">>, Opts),
    {ok, NumHandlers} =
        hb_ao:resolve(
            ProcReceiver,
            <<"now/results/data">>,
            Opts
        ),
    % _eval, _default, Increment
    ?assertEqual(<<"3">>, NumHandlers),

    schedule_aos_call(ProcReceiver, <<"return Number">>, Opts),
    {ok, InitialNumber} = 
        hb_ao:resolve(
            ProcReceiver, 
            <<"now/results/data">>,
            Opts
        ),
    % Number is initialized to 5
    ?assertEqual(<<"5">>, InitialNumber),
    % Set up sender process to send Action: Increment to receiver
    hb_cache:write(ProcSender, Opts),
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
            >>,
            Opts
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
    schedule_aos_call(ProcReceiver, <<"return Number">>, Opts),
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
            >>,
            Opts
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
    schedule_aos_call(ProcReceiver, <<"return Number">>, Opts),
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
    schedule_aos_call(ProcReceiver, <<"return Number">>, Opts),
    {ok, AfterDryrunResult} =
        hb_ao:resolve(
            ProcReceiver, 
            <<"now/results/data">>, 
            Opts
        ),
    ?assertEqual(<<"7">>, AfterDryrunResult).
-endif.
