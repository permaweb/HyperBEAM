%%% @doc This module contains end-to-end tests for Hyperbeam, accessing through
%%% the HTTP interface. As well as testing the system, you can use these tests
%%% as examples of how to interact with HyperBEAM nodes.
-module(hb_examples).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% @doc Start a node running the simple pay meta device, and use it to relay
%% a message for a client. We must ensure:
%% 1. When the client has no balance, the relay fails.
%% 2. The operator is able to topup for the client.
%% 3. The client has the correct balance after the topup.
%% 4. The relay succeeds when the client has enough balance.
%% 5. The received message is signed by the host using http-sig and validates
%%    correctly.
relay_with_payments_test_() ->
    {timeout, 30, fun relay_with_payments/0}.
relay_with_payments() ->
    HostWallet = ar_wallet:new(),
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    % Start a node with the simple-pay device enabled.
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>
        },
    HostNode =
        hb_http_server:start_node(
            #{
                operator => ar_wallet:to_address(HostWallet),
                on => #{
                    <<"request">> => ProcessorMsg,
                    <<"response">> => ProcessorMsg
                }
            }
        ),
    % Create a message for the client to relay.
    ClientBase =
        hb_message:commit(
            #{<<"path">> => <<"/~relay@1.0/call?relay-path=https://www.google.com">>},
            #{ priv_wallet => ClientWallet }
        ),
    % Relay the message.
    Res = hb_http:get(HostNode, ClientBase, #{}),
    ?assertMatch({error, #{ <<"body">> := <<"Insufficient funds">> }}, Res),
    % Topup the client's balance.
    % Note: The fields must be in the headers, for now.
    TopupMessage =
        hb_message:commit(
            #{
                <<"path">> => <<"/~simple-pay@1.0/topup">>,
                <<"recipient">> => ClientAddress,
                <<"amount">> => 100
            },
            #{ priv_wallet => HostWallet }
        ),
    ?assertMatch({ok, _}, hb_http:get(HostNode, TopupMessage, #{})),
    % Relay the message again.
    Res2 = hb_http:get(HostNode, ClientBase, #{}),
    ?assertMatch({ok, #{ <<"body">> := Bin }} when byte_size(Bin) > 10_000, Res2),
    {ok, Resp} = Res2,
    ?assert(length(hb_message:signers(Resp, #{})) > 0),
    ?assert(hb_message:verify(Resp, all, #{})).

%% @doc Gain signed WASM responses from a node and verify them.
%% 1. Start the client with a small balance.
%% 2. Execute a simple WASM function on the host node.
%% 3. Verify the response is correct and signed by the host node.
%% 4. Get the balance of the client and verify it has been deducted.
paid_wasm_test_() ->
    {timeout, 30, fun paid_wasm/0}.
paid_wasm() ->
    HostWallet = ar_wallet:new(),
    ClientWallet = ar_wallet:new(),
    ClientAddress = hb_util:human_id(ar_wallet:to_address(ClientWallet)),
    ProcessorMsg =
        #{
            <<"device">> => <<"p4@1.0">>,
            <<"ledger-device">> => <<"simple-pay@1.0">>,
            <<"pricing-device">> => <<"simple-pay@1.0">>
        },
    HostNode =
        hb_http_server:start_node(
            Opts = #{
				store => [
					#{
						<<"store-module">> => hb_store_fs,
						<<"name">> => <<"cache-TEST">>
					}
				],
                simple_pay_ledger => #{ ClientAddress => 100 },
                simple_pay_price => 10,
                operator => ar_wallet:to_address(HostWallet),
                on => #{
                    <<"request">> => ProcessorMsg,
                    <<"response">> => ProcessorMsg
                }
            }
        ),
    % Read the WASM file from disk, post it to the host and execute it.
    {ok, WASMFile} = file:read_file(<<"test/test-64.wasm">>),
    ClientBase =
        hb_message:commit(
            #{
                <<"path">> =>
                    <<"/~wasm-64@1.0/init/compute/results?function=fac">>,
                <<"body">> => WASMFile,
                <<"parameters+list">> => <<"3.0">>
            },
            Opts#{ priv_wallet => ClientWallet }
        ),
    {ok, Res} = hb_http:post(HostNode, ClientBase, Opts),
    % Check that the message is signed by the host node.
    ?assert(length(hb_message:signers(Res, Opts)) > 0),
    ?assert(hb_message:verify(Res, all, Opts)),
    % Now we have the results, we can verify them.
    ?assertMatch(6.0, hb_ao:get(<<"output/1">>, Res, Opts)),
    % Check that the client's balance has been deducted.
    ClientRequest =
        hb_message:commit(
            #{<<"path">> => <<"/~p4@1.0/balance">>},
            #{ priv_wallet => ClientWallet }
        ),
    {ok, Res2} = hb_http:get(HostNode, ClientRequest, Opts),
    ?assertMatch(60, Res2).

%% @doc Simulate a full Mysticeti network with isolated stores and HTTP gossip.
%% Validates multi-node consensus over AO-Core HTTP, using scheduler-location
%% records to resolve peers.
mysticeti_network_test_() ->
    {timeout, 240, fun() -> run_mysticeti_network(5, lists:seq(0, 5), [0]) end}.

%% @doc Larger network run to stress peer gossip and quorum behavior.
mysticeti_network_many_nodes_test_() ->
    {timeout, 300, fun() -> run_mysticeti_network(7, lists:seq(0, 3), [0]) end}.

%% @doc End-to-end wasm execution over Mysticeti scheduling and HTTP.
mysticeti_wasm_process_http_test_() ->
    {timeout, 360, fun mysticeti_wasm_process_http/0}.

%% @doc End-to-end lua execution over Mysticeti scheduling and HTTP.
mysticeti_lua_process_http_test_() ->
    {timeout, 360, fun mysticeti_lua_process_http/0}.

%% @doc End-to-end Mysticeti-C network execution over HTTP.
%% Validates direct decision and total-order properties (Algorithms 2-3,
%% mysticeti-paper/algorithms/*.tex) under multi-node gossip.
run_mysticeti_network(NodeCount, Rounds, ExpectedRounds) ->
    {Nodes, Validators, PeerUrls} = setup_mysticeti_http_network(NodeCount),
    ProcBase = mysticeti_proc_base(Validators, PeerUrls),
    #{ opts := FirstOpts } = hd(Nodes),
    Proc = hb_message:commit(ProcBase, FirstOpts),
    ProcMsgId = hb_message:id(Proc, all, FirstOpts),
    ProcID = hb_util:human_id(ProcMsgId),
    ProcLoaded = hb_cache:ensure_all_loaded(Proc, FirstOpts),
    lists:foreach(
        fun(#{ opts := Opts }) ->
            {ok, _} = hb_cache:write(ProcLoaded, Opts)
        end,
        Nodes
    ),
    NodesWithProc = [Node#{ proc => ProcLoaded, proc_id => ProcID } || Node <- Nodes],
    lists:foreach(
        fun(#{ opts := Opts }) ->
            Pid = dev_mysticeti_registry:find(ProcID, ProcLoaded, Opts),
            Info = dev_mysticeti_server:info(Pid),
            Peers = hb_maps:get(peers, Info, [], Opts),
            ?assert(length(Peers) >= 1),
            ValidatorsInfo = hb_maps:get(validators, Info, [], Opts),
            ?assertEqual(NodeCount, length(ValidatorsInfo)),
            ?assert(lists:member(hb_maps:get(local_author, Info, undefined, Opts), ValidatorsInfo))
        end,
        NodesWithProc
    ),
    lists:foreach(
        fun(Round) ->
            lists:foreach(
                fun(#{ index := Index } = Node) ->
                    schedule_round_messages(Node, Index, [Round])
                end,
                NodesWithProc
            ),
            timer:sleep(200)
        end,
        Rounds
    ),
    ExpectedBodies =
        [
            << "r", (integer_to_binary(Round))/binary, "-",
               (integer_to_binary(Index))/binary >>
        || Round <- ExpectedRounds, Index <- lists:seq(1, NodeCount)],
    MaxSlots = length(Validators) * length(Rounds),
    lists:foreach(
        fun(Node) ->
            case wait_for_expected_bodies_http(
                Node,
                ProcID,
                ExpectedBodies,
                MaxSlots,
                120000
            ) of
                true ->
                    ok;
                false ->
                    #{ url := NodeUrl, opts := NodeOpts } = Node,
                    Assignments =
                        fetch_assignments_http(
                            NodeUrl,
                            ProcID,
                            0,
                            MaxSlots + 2,
                            NodeOpts
                        ),
                    Bodies = assignment_bodies(Assignments, NodeOpts),
                    Resp =
                        catch hb_http:get(
                            NodeUrl,
                            #{
                                <<"path">> => << ProcID/binary, "/schedule" >>,
                                <<"from">> => 0,
                                <<"to">> => MaxSlots + 2
                            },
                            NodeOpts
                        ),
                    Info =
                        case dev_mysticeti_registry:find(ProcID, ProcLoaded, NodeOpts) of
                            not_found -> not_found;
                            Pid -> dev_mysticeti_server:info(Pid)
                        end,
                    erlang:error(
                        {expected_bodies_missing,
                            {node, NodeUrl},
                            {expected, ExpectedBodies},
                            {found, Bodies},
                            {response, Resp},
                            {info, Info}}
                    )
            end
        end,
        NodesWithProc
    ),
    {RefSlots, _} =
        lists:foldl(
        fun(#{ url := Node, opts := Opts }, {Ref, Index}) ->
            Assignments =
                fetch_assignments_http(Node, ProcID, 0, MaxSlots + 2, Opts),
            Bodies = assignment_bodies(Assignments, Opts),
            ?assertEqual([], ExpectedBodies -- Bodies),
            Slots = assignment_body_slots(Assignments, ExpectedBodies, Opts),
                case Index of
                    0 ->
                        ?assert(
                            hb_message:verify(
                                hb_ao:get(
                                    <<"body">>,
                                    hd(hb_maps:values(Assignments, Opts)),
                                    Opts
                                ),
                                all,
                                Opts
                            )
                        ),
                        {Slots, 1};
                    _ ->
                        ?assertEqual(Ref, Slots),
                        {Ref, Index + 1}
                end
            end,
            {#{}, 0},
            NodesWithProc
        ),
    ?assert(hb_maps:size(RefSlots, FirstOpts) == length(ExpectedBodies)).

%% @doc Build a Mysticeti process base message for tests.
mysticeti_proc_base(Validators, PeerUrls) ->
    #{
        <<"device">> => <<"process@1.0">>,
        <<"scheduler-device">> => <<"mystislopi@1.0-pre">>,
        <<"scheduler-location">> => Validators,
        <<"mysticeti">> => #{
            <<"validators">> => Validators,
            <<"stakers">> => [#{ <<"id">> => V, <<"stake">> => 1 } || V <- Validators],
            <<"peers">> => PeerUrls,
            <<"wave-length">> => 3,
            <<"proposer-offset">> => 0,
            <<"num-proposers">> => length(Validators)
        },
        <<"type">> => <<"Process">>
    }.

mysticeti_wasm_process_http() ->
    mysticeti_exec_process_http(wasm).

mysticeti_lua_process_http() ->
    mysticeti_exec_process_http(lua).

%% @doc Execute a full Mysticeti-C HTTP workflow with a configured executor.
mysticeti_exec_process_http(Kind) ->
    NodeCount = 4,
    {Nodes, Validators, PeerUrls} = setup_mysticeti_http_network(NodeCount),
    {ProcExtra, MsgSpecs, MsgBuilder, ExpectedFun, OutputPath} = exec_spec(Kind, Nodes),
    ProcBase = mysticeti_proc_base(Validators, PeerUrls),
    #{ url := _FirstNode, opts := FirstOpts } = hd(Nodes),
    Proc = hb_message:commit(hb_maps:merge(ProcBase, ProcExtra, FirstOpts), FirstOpts),
    ProcMsgId = hb_message:id(Proc, all, FirstOpts),
    ProcID = hb_util:human_id(ProcMsgId),
    ProcLoaded = hb_cache:ensure_all_loaded(Proc, FirstOpts),
    lists:foreach(
        fun(#{ opts := Opts }) ->
            {ok, _} = hb_cache:write(ProcLoaded, Opts)
        end,
        Nodes
    ),
    NodesWithProc = [Node#{ proc => ProcLoaded, proc_id => ProcID } || Node <- Nodes],
    MsgInfos =
        lists:map(
            fun(Spec) ->
                NodeIndex = hb_maps:get(node_index, Spec, 1, #{}),
                Arg = hb_maps:get(arg, Spec, undefined, #{}),
                NodeInfo = lists:nth(NodeIndex, Nodes),
                NodeOpts = hb_maps:get(opts, NodeInfo, #{}, #{}),
                Msg = MsgBuilder(ProcID, Arg, NodeOpts),
                MsgId = hb_message:id(Msg, all, NodeOpts),
                Expected = ExpectedFun(Arg, Msg, NodeOpts),
                {NodeInfo, Msg, MsgId, Expected}
            end,
            MsgSpecs
        ),
    MsgByIndex =
        lists:foldl(
            fun({NodeInfo, Msg, MsgId, Expected}, Acc) ->
                Index = hb_maps:get(index, NodeInfo, 0, #{}),
                hb_maps:put(
                    Index,
                    #{ msg => Msg, msg_id => MsgId, expected => Expected },
                    Acc,
                    #{}
                )
            end,
            #{},
            MsgInfos
        ),
    ExecRound = 2,
    RoundPlan = lists:seq(0, 7),
    lists:foreach(
        fun(Round) ->
            lists:foreach(
                fun(NodeInfo) ->
                    Index = hb_maps:get(index, NodeInfo, 0, #{}),
                    NodeUrl = hb_maps:get(url, NodeInfo, undefined, #{}),
                    NodeOpts = hb_maps:get(opts, NodeInfo, #{}, #{}),
                    case {Round, Index} of
                        {ExecRound, _} ->
                            case hb_maps:is_key(Index, MsgByIndex, #{}) of
                                true ->
                                    Exec = hb_maps:get(Index, MsgByIndex, #{}, #{}),
                                    ExecMsg = hb_maps:get(msg, Exec, undefined, #{}),
                                    {ok, _} =
                                        dev_mysticeti_test_utils:post_process_schedule(
                                            NodeUrl,
                                            ProcID,
                                            ExecMsg,
                                            NodeOpts
                                        );
                                false ->
                                    schedule_drive_message(NodeInfo, Round)
                            end;
                        _ ->
                            schedule_drive_message(NodeInfo, Round)
                    end
                end,
                NodesWithProc
            ),
            timer:sleep(200)
        end,
        RoundPlan
    ),
    MaxSlots = length(RoundPlan) * length(NodesWithProc) + 10,
    MsgIds = [MsgId || {_NodeInfo, _Msg, MsgId, _Expected} <- MsgInfos],
    lists:foreach(
        fun(Node) ->
            case wait_for_message_ids_http(Node, ProcID, MsgIds, MaxSlots, 180000) of
                true -> ok;
                false -> erlang:error({messages_not_committed, Node})
            end
        end,
        Nodes
    ),
    RefNode = hd(Nodes),
    RefOpts = hb_maps:get(opts, RefNode, #{}, #{}),
    RefAssignments =
        fetch_assignments_http(
            hb_maps:get(url, RefNode, undefined, #{}),
            ProcID,
            0,
            MaxSlots,
            RefOpts
        ),
    RefSlots = message_slots_for_ids(RefAssignments, MsgIds, RefOpts),
    ?assertEqual(length(MsgIds), hb_maps:size(RefSlots, RefOpts)),
    lists:foreach(
        fun(Node) ->
            Assignments =
                fetch_assignments_http(
                    hb_maps:get(url, Node, undefined, #{}),
                    ProcID,
                    0,
                    MaxSlots,
                    hb_maps:get(opts, Node, #{}, #{})
                ),
            Slots = message_slots_for_ids(Assignments, MsgIds, hb_maps:get(opts, Node, #{}, #{})),
            ?assertEqual(RefSlots, Slots)
        end,
        Nodes
    ),
    ExpectedById =
        lists:foldl(
            fun({_NodeInfo, _Msg, MsgId, Expected}, Acc) ->
                hb_maps:put(MsgId, Expected, Acc, RefOpts)
            end,
            #{},
            MsgInfos
        ),
    lists:foreach(
        fun(Node) ->
            NodeOpts = hb_maps:get(opts, Node, #{}, #{}),
            NodeUrl = hb_maps:get(url, Node, undefined, #{}),
            lists:foreach(
                fun({MsgId, Slot}) ->
                    Req =
                        case OutputPath of
                            <<"results/output/body">> ->
                                #{
                                    <<"path">> => << ProcID/binary, "/compute" >>,
                                    <<"slot">> => Slot,
                                    <<"accept">> => <<"application/httpsig@1.0">>,
                                    <<"require-codec">> => <<"httpsig@1.0">>,
                                    <<"accept-bundle">> => false
                                };
                            _ ->
                                #{
                                    <<"path">> => << ProcID/binary, "/compute" >>,
                                    <<"slot">> => Slot
                                }
                        end,
                    {ok, Res} =
                        hb_http:get(
                            NodeUrl,
                            Req,
                            NodeOpts#{ cache_control => <<"always">> }
                        ),
                    Expected = hb_maps:get(MsgId, ExpectedById, undefined, NodeOpts),
                    ?assertEqual(Expected, hb_ao:get(OutputPath, Res, NodeOpts))
                end,
                hb_maps:to_list(RefSlots, RefOpts)
            )
        end,
        Nodes
    ).

exec_spec(wasm, Nodes) ->
    WasmImageId = cache_wasm_image_all_nodes(<<"test/test-64.wasm">>, Nodes),
    {
        #{
            <<"execution-device">> => <<"stack@1.0">>,
            <<"device-stack">> => [<<"wasm-64@1.0">>],
            <<"image">> => WasmImageId
        },
        [
            #{ node_index => 1, arg => 5.0 },
            #{ node_index => 2, arg => 6.0 }
        ],
        fun wasm_exec_message/3,
        fun(Arg, _Msg, _Opts) -> wasm_expected_output(Arg) end,
        <<"results/output">>
    };
exec_spec(lua, _Nodes) ->
    {ok, Module} = file:read_file("test/test.lua"),
    {
        #{
            <<"execution-device">> => <<"lua@5.3a">>,
            <<"module">> => #{
                <<"content-type">> => <<"application/lua">>,
                <<"body">> => Module
            }
        },
        [
            #{ node_index => 1, arg => <<"lua-1">> },
            #{ node_index => 2, arg => <<"lua-2">> }
        ],
        fun lua_exec_message/3,
        fun(_Arg, _Msg, _Opts) -> lua_expected_output() end,
        <<"results/output/body">>
    }.

setup_mysticeti_http_network(NodeCount) ->
    {Nodes, Validators} = dev_mysticeti_test_utils:start_mysticeti_nodes(NodeCount),
    case dev_mysticeti_test_utils:wait_for_nodes_ready(Nodes, 10000) of
        true -> ok;
        {error, MissingNodes} -> erlang:error({nodes_not_ready, MissingNodes})
    end,
    Locations =
        lists:map(
            fun(#{ url := Node, opts := Opts }) ->
                {ok, Location} =
                    dev_mysticeti_test_utils:register_scheduler_location(Node, Opts),
                #{ location => Location, opts => Opts }
            end,
            Nodes
        ),
    PeerUrls =
        [
            Url
        || #{ location := Location, opts := Opts } <- Locations,
           (Url = hb_ao:get(<<"url">>, Location, not_found, Opts)) =/= not_found
        ],
    lists:foreach(
        fun(#{ location := Location, opts := SenderOpts }) ->
            lists:foreach(
                fun(#{ url := Node }) ->
                    case dev_mysticeti_test_utils:post_scheduler_location(
                        Node, Location, SenderOpts, 10000
                    ) of
                        {ok, _} -> ok;
                        {error, Reason} ->
                            erlang:error({scheduler_location_post_failed, Node, Reason})
                    end
                end,
                Nodes
            )
        end,
        Locations
    ),
    timer:sleep(200),
    case dev_mysticeti_test_utils:wait_for_scheduler_locations(
        Nodes, Validators, 20000
    ) of
        true -> ok;
        {error, MissingLocations} ->
            erlang:error({scheduler_locations_missing, MissingLocations})
    end,
    {Nodes, Validators, PeerUrls}.

cache_wasm_image_all_nodes(Image, Nodes) ->
    [First | Rest] = Nodes,
    FirstOpts = hb_maps:get(opts, First, #{}, #{}),
    #{ <<"image">> := ImageId } = dev_wasm:cache_wasm_image(Image, FirstOpts),
    lists:foreach(
        fun(Node) ->
            NodeOpts = hb_maps:get(opts, Node, #{}, #{}),
            #{ <<"image">> := Other } = dev_wasm:cache_wasm_image(Image, NodeOpts),
            ?assertEqual(ImageId, Other)
        end,
        Rest
    ),
    ImageId.

wait_for_message_ids_http(#{ url := Node, opts := Opts }, ProcID, MsgIds, MaxSlots, Timeout) ->
    hb_util:wait_until(
        fun() ->
            Assignments = fetch_assignments_http(Node, ProcID, 0, MaxSlots, Opts),
            Slots = message_slots_for_ids(Assignments, MsgIds, Opts),
            hb_maps:size(Slots, Opts) >= length(MsgIds)
        end,
        Timeout
    ).

message_slots_for_ids(Assignments, MsgIds, Opts) ->
    lists:foldl(
        fun({Slot, Assignment}, Acc) ->
            case assignment_message_id(Assignment, Opts) of
                {ok, MsgId} ->
                    case lists:member(MsgId, MsgIds) andalso
                        not hb_maps:is_key(MsgId, Acc, Opts) of
                        true -> hb_maps:put(MsgId, Slot, Acc, Opts);
                        false -> Acc
                    end;
                _ ->
                    Acc
            end
        end,
        #{},
        hb_util:to_sorted_list(Assignments, Opts)
    ).

assignment_message_id(Assignment, Opts) ->
    dev_mysticeti_test_utils:assignment_message_id(Assignment, Opts).

wasm_exec_message(ProcID, Param, Opts) ->
    hb_message:commit(
        #{
            <<"target">> => ProcID,
            <<"type">> => <<"Message">>,
            <<"function">> => <<"fac">>,
            <<"parameters">> => [Param]
        },
        Opts
    ).

lua_exec_message(ProcID, Label, Opts) ->
    hb_message:commit(
        #{
            <<"target">> => ProcID,
            <<"type">> => <<"Message">>,
            <<"function">> => <<"compute">>,
            <<"label">> => Label
        },
        Opts
    ).

wasm_expected_output(Param) ->
    [factorial(Param)].

lua_expected_output() ->
    42.

factorial(Value) when is_float(Value) ->
    factorial(trunc(Value), 1.0);
factorial(Value) when is_integer(Value) ->
    factorial(Value, 1.0);
factorial(Value) ->
    factorial(trunc(Value), 1.0).

factorial(N, Acc) when N =< 1 -> Acc;
factorial(N, Acc) -> factorial(N - 1, Acc * N).

schedule_round_messages(
    #{ url := Node, opts := Opts, proc_id := ProcID },
    Index,
    Rounds
) ->
    lists:foreach(
        fun(Round) ->
            Body =
                << "r", (integer_to_binary(Round))/binary, "-",
                   (integer_to_binary(Index))/binary >>,
            Msg =
                hb_message:commit(
                    #{
                        <<"target">> => ProcID,
                        <<"body">> => Body,
                        <<"type">> => <<"Message">>
                    },
                    Opts
                ),
            {ok, Res} =
                dev_mysticeti_test_utils:post_process_schedule(Node, ProcID, Msg, Opts),
            case hb_maps:get(<<"status">>, Res, 200, Opts) of
                Status when Status >= 400 ->
                    erlang:error({schedule_failed, Status, Res});
                _ ->
                    ok
            end,
            ok
        end,
        Rounds
    ).

schedule_drive_message(
    #{ url := Node, opts := Opts, proc_id := ProcID, index := Index },
    Round
) ->
    Body =
        << "r", (integer_to_binary(Round))/binary, "-",
           (integer_to_binary(Index))/binary >>,
    Msg =
        hb_message:commit(
            #{
                <<"target">> => ProcID,
                <<"body">> => Body,
                <<"type">> => <<"Message">>
            },
            Opts
        ),
    {ok, Res} =
        dev_mysticeti_test_utils:post_process_schedule(Node, ProcID, Msg, Opts),
    case hb_maps:get(<<"status">>, Res, 200, Opts) of
        Status when Status >= 400 ->
            erlang:error({schedule_failed, Status, Res});
        _ ->
            ok
    end.

wait_for_expected_bodies_http(
    #{ url := Node, opts := Opts },
    ProcID,
    ExpectedBodies,
    MaxSlots,
    Timeout
) ->
    hb_util:wait_until(
        fun() ->
            Assignments =
                dev_mysticeti_test_utils:fetch_assignments_http(
                    Node,
                    ProcID,
                    0,
                    MaxSlots + 2,
                    Opts
                ),
            Bodies = assignment_bodies(Assignments, Opts),
            ExpectedBodies -- Bodies =:= []
        end,
        Timeout
    ).

fetch_assignments_http(Node, ProcID, From, To, Opts) ->
    dev_mysticeti_test_utils:fetch_assignments_http(Node, ProcID, From, To, Opts).

assignment_bodies(Assignments, Opts) ->
    lists:map(
        fun({_Slot, Assignment}) ->
            dev_mysticeti_test_utils:assignment_body(Assignment, Opts)
        end,
        hb_util:to_sorted_list(Assignments, Opts)
    ).

assignment_body_slots(Assignments, ExpectedBodies, Opts) ->
    lists:foldl(
        fun({Slot, Assignment}, Acc) ->
            Body = dev_mysticeti_test_utils:assignment_body(Assignment, Opts),
            case lists:member(Body, ExpectedBodies) of
                true ->
                    case hb_maps:is_key(Body, Acc, Opts) of
                        true -> Acc;
                        false -> hb_maps:put(Body, Slot, Acc, Opts)
                    end;
                false -> Acc
            end
        end,
        #{},
        hb_util:to_sorted_list(Assignments, Opts)
    ).

%% @doc Test that we can schedule an ANS-104 data item on a relayed node. The
%% input to the relaying server comes in the form of a serialized ANS-104
%% data item, which should then be correctly deserialized and sent to the
%% scheduler node.
relay_schedule_ans104_test() ->
    SchedulerWallet = ar_wallet:new(),
    ComputeWallet = ar_wallet:new(),
    RelayWallet = ar_wallet:new(),
    ?event(debug_test,
        {wallets,
            {scheduler, hb_util:human_id(SchedulerWallet)},
            {compute, hb_util:human_id(ComputeWallet)},
            {relay, hb_util:human_id(RelayWallet)}
        }
    ),
    Scheduler =
        hb_http_server:start_node(
            #{
                on => #{
                    <<"start">> => #{
                        <<"device">> => <<"location@1.0">>,
                        <<"path">> => <<"node">>,
                        <<"method">> => <<"POST">>,
                        <<"target">> => <<"self">>,
                        <<"require-codec">> => <<"ans104@1.0">>,
                        <<"hook">> => #{
                            <<"result">> => <<"ignore">>,
                            <<"commit-request">> => true
                        }
                    }
                },
                store => [hb_test_utils:test_store()],
                priv_wallet => SchedulerWallet
            }
        ),
    ?event(debug_test, {scheduler, Scheduler}),
    Compute =
        hb_http_server:start_node(
            #{
                priv_wallet => ComputeWallet,
                store =>
                    [
                        ComputeStore = hb_test_utils:test_store(),
                        #{
                            <<"store-module">> => hb_store_remote_node,
                            <<"name">> => <<"cache-TEST/remote-node">>,
                            <<"node">> => Scheduler
                        }
                    ]
            }
        ),
    % Get the scheduler location of the scheduling node and write it to the
    % compute node's store.
    {ok, SchedulerLocation} =
        hb_http:get(
            Scheduler,
            <<"/~location@1.0/node">>,
            #{}
        ),
    ?event({scheduler_location, SchedulerLocation}),
    dev_location_cache:write(
        SchedulerLocation,
        #{ store => [ComputeStore] }
    ),
    % Create the relaying server.
    Relay =
        hb_http_server:start_node(#{
            priv_wallet => RelayWallet,
            relay_allow_commit_request => true,
            store => [hb_test_utils:test_store()],
            routes =>
                [
                    #{
                        <<"template">> => <<"^/push">>,
                        <<"strategy">> => <<"Nearest">>,
                        <<"nodes">> => [
                            #{
                                <<"wallet">> => hb_util:human_id(SchedulerWallet),
                                <<"prefix">> => Scheduler
                            }
                        ]
                    },
                    #{
                        <<"template">> => <<"^/.*">>,
                        <<"strategy">> => <<"Nearest">>,
                        <<"nodes">> => [
                            #{
                                <<"wallet">> => hb_util:human_id(ComputeWallet),
                                <<"prefix">> => Compute
                            }
                        ]
                    }
                ],
            on => #{
                <<"request">> =>
                    #{
                        <<"device">> => <<"router@1.0">>,
                        <<"path">> => <<"preprocess">>,
                        <<"commit-request">> => true
                    }
            }
        }),
    ?event(debug_test,
        {nodes,
            {scheduler, {url, Scheduler}, {wallet, hb_util:human_id(SchedulerWallet)}},
            {compute, {url, Compute}, {wallet, hb_util:human_id(ComputeWallet)}},
            {relay, {url, Relay}, {wallet, hb_util:human_id(RelayWallet)}}
        }
    ),
    ClientOpts =
        #{
            store => [hb_test_utils:test_store()],
            priv_wallet => ar_wallet:new()
        },
    % Create process to schedule, then send it to the relaying server as
    % a serialized ANS-104 data item.
    Process =
        hb_message:commit(
            #{
                <<"device">> => <<"process@1.0">>,
                <<"execution-device">> => <<"test-device@1.0">>,
                <<"push-device">> => <<"push@1.0">>,
                <<"scheduler">> => hb_util:human_id(SchedulerWallet),
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"module">> => <<"URgYpPQzvxxfYQtjrIQ116bl3YBfcImo3JEnNo8Hlrk">>
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    % Push the initial message via the scheduler node.
    ScheduleRes =
        hb_http:post(
            Relay,
            Process#{
                <<"path">> => <<"push">>,
                <<"codec-device">> => <<"ans104@1.0">>
            },
            ClientOpts
        ),
    ?event(debug_test, {post_result, ScheduleRes}),
    ?assertMatch({ok, #{ <<"status">> := 200, <<"slot">> := 0 }}, ScheduleRes),
    % Push another message via the compute node.
    ProcID = hb_message:id(Process, all, ClientOpts),
    ToPush =
        hb_message:commit(
            #{
                <<"test-key">> => <<"value">>,
                <<"rand-key">> => hb_util:encode(crypto:strong_rand_bytes(32))
            },
            ClientOpts,
            #{ <<"commitment-device">> => <<"ans104@1.0">> }
        ),
    PushRes =
        hb_http:post(
            Relay,
            ToPush#{
                <<"path">> => <<ProcID/binary, "/push">>,
                <<"codec-device">> => <<"ans104@1.0">>
            },
            ClientOpts
        ),
    ?event(debug_test, {post_result, PushRes}),
    ?assertMatch({ok, #{ <<"status">> := 200, <<"slot">> := 1 }}, PushRes).
