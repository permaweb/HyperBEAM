%%% @doc Invariant-based network tests for Mysticeti-C scheduling.
%%%
%%% This suite spins up multiple isolated nodes (distinct stores), discovers
%%% peers via scheduler-location, and drives consensus through `process@1.0`
%%% HTTP. It enforces network-wide committer properties:
%%% - assignments are contiguous and unique per node,
%%% - assignments are a subset of scheduled messages,
%%% - all nodes share a common assignment prefix (no conflicting order).
%%%
%%% Paper references:
%%% - mysticeti-paper/algorithms/consensus_utils.tex (Alg. 1 predicates),
%%% - mysticeti-paper/algorithms/universal_committer.tex (Alg. 3 committer),
%%% - mysticeti-paper/sections/overview.tex (block correctness narrative).
-module(dev_mysticeti_test_networks).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Run invariant checks over a multi-node Mysticeti network.
simulate_test_() ->
    {timeout, 240, fun simulate/0}.

%% @doc Run the invariant state machine over a Mysticeti network.
simulate() ->
    ok = hb_invariant:state_machine(
        #{
            states => [fun init_state/1],
            requests => [fun schedule_request/2],
            properties => [
                fun verify_contiguous_slots/4,
                fun verify_assignment_subset/4,
                fun verify_assignment_unique/4,
                fun verify_prefix_consistency/4
            ],
            next => fun next_state/4,
            runs => 2,
            length => 40,
            opts => #{}
        }
    ).

%% @doc Build the initial invariant state with nodes and a process.
init_state(_Opts) ->
    NodeCount = 4,
    {Nodes0, Validators} = start_mysticeti_nodes(NodeCount),
    case wait_for_nodes_ready(Nodes0, 10000) of
        true -> ok;
        {error, MissingNodes} -> erlang:error({nodes_not_ready, MissingNodes})
    end,
    Locations =
        lists:map(
            fun(#{ url := Node, opts := Opts }) ->
                {ok, Location} = register_scheduler_location(Node, Opts),
                #{ location => Location, opts => Opts }
            end,
            Nodes0
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
                    case post_scheduler_location(Node, Location, SenderOpts, 10000) of
                        {ok, _} -> ok;
                        {error, Reason} ->
                            erlang:error({scheduler_location_post_failed, Node, Reason})
                    end
                end,
                Nodes0
            )
        end,
        Locations
    ),
    timer:sleep(200),
    case wait_for_scheduler_locations(Nodes0, Validators, 20000) of
        true -> ok;
        {error, MissingLocations} ->
            erlang:error({scheduler_locations_missing, MissingLocations})
    end,
    ProcBase =
        #{
            <<"device">> => <<"process@1.0">>,
            <<"scheduler-device">> => <<"mysticeti@1.0">>,
            <<"scheduler-location">> => Validators,
            <<"mysticeti">> => #{
                <<"validators">> => Validators,
                <<"stakers">> =>
                    [#{ <<"id">> => V, <<"stake">> => 1 } || V <- Validators],
                <<"peers">> => PeerUrls,
                <<"wave-length">> => 3,
                <<"proposer-offset">> => 0,
                <<"num-proposers">> => length(Validators)
            },
            <<"type">> => <<"Process">>
        },
    #{ opts := FirstOpts } = hd(Nodes0),
    Proc = hb_message:commit(ProcBase, FirstOpts),
    ProcID = hb_util:human_id(hb_message:id(Proc, all, FirstOpts)),
    ProcLoaded = hb_cache:ensure_all_loaded(Proc, FirstOpts),
    lists:foreach(
        fun(#{ opts := Opts }) ->
            {ok, _} = hb_cache:write(ProcLoaded, Opts)
        end,
        Nodes0
    ),
    Nodes = [Node#{ proc => ProcLoaded, proc_id => ProcID } || Node <- Nodes0],
    #{
        nodes => Nodes,
        proc => ProcLoaded,
        proc_id => ProcID,
        scheduled => #{},
        assignments => collect_assignments(Nodes, ProcID, 0, 120),
        max_slots => 120
    }.

%% @doc Submit a randomized schedule request and record it in state.
schedule_request(State, _Opts) ->
    Node = hb_invariant:pick(hb_maps:get(nodes, State, [], #{})),
    Body = hb_util:bin(hb_invariant:string(8)),
    fun(S, _SOpts) -> schedule_and_update(S, Node, Body) end.

%% @doc Schedule a message on a specific node and update the state.
schedule_and_update(State, NodeInfo, Body) ->
    Opts = hb_maps:get(opts, NodeInfo, #{}, #{}),
    Node = hb_maps:get(url, NodeInfo, undefined, Opts),
    ProcID = hb_maps:get(proc_id, NodeInfo, hb_maps:get(proc_id, State, undefined, Opts), Opts),
    Msg =
        hb_message:commit(
            #{
                <<"target">> => ProcID,
                <<"body">> => Body,
                <<"type">> => <<"Message">>
            },
            Opts
        ),
    {ok, Res} = dev_mysticeti_test_utils:post_process_schedule(Node, ProcID, Msg, Opts),
    case hb_maps:get(<<"status">>, Res, 200, Opts) of
        Status when Status >= 400 ->
            erlang:error({schedule_failed, Status, Res});
        _ -> ok
    end,
    MsgId = hb_message:id(Msg, all, Opts),
    Scheduled0 = hb_maps:get(scheduled, State, #{}, Opts),
    Scheduled = Scheduled0#{ MsgId => true },
    timer:sleep(50),
    Assignments =
        collect_assignments(
            hb_maps:get(nodes, State, [], Opts),
            ProcID,
            0,
            hb_maps:get(max_slots, State, 0, Opts)
        ),
    {ok, State#{ scheduled := Scheduled, assignments := Assignments }}.

%% @doc Ensure each node's assignments have contiguous slot indices.
verify_contiguous_slots(_Old, _Req, #{ nodes := Nodes, assignments := AssignmentsByNode }, _Opts) ->
    lists:foldl(
        fun(NodeInfo, Acc) ->
            case Acc of
                true ->
                    Opts = hb_maps:get(opts, NodeInfo, #{}, #{}),
                    Node = hb_maps:get(url, NodeInfo, undefined, Opts),
                    Assignments = hb_maps:get(Node, AssignmentsByNode, #{}, Opts),
                    case slots_to_ints(Assignments, Opts) of
                        {ok, Slots} ->
                            Expected =
                                case Slots of
                                    [] -> [];
                                    _ -> lists:seq(0, length(Slots) - 1)
                                end,
                            (Slots == Expected) orelse
                                {non_contiguous_slots, {node, Node}, {slots, Slots}};
                        {error, Reason} ->
                            {invalid_slots, {node, Node}, Reason}
                    end;
                Error ->
                    Error
            end
        end,
        true,
        Nodes
    ).

%% @doc Ensure assigned message ids are a subset of scheduled ids.
verify_assignment_subset(
    _Old,
    _Req,
    #{ nodes := Nodes, assignments := AssignmentsByNode, scheduled := Scheduled },
    _Opts
) ->
    lists:foldl(
        fun(NodeInfo, Acc) ->
            case Acc of
                true ->
                    Opts = hb_maps:get(opts, NodeInfo, #{}, #{}),
                    Node = hb_maps:get(url, NodeInfo, undefined, Opts),
                    Assignments = hb_maps:get(Node, AssignmentsByNode, #{}, Opts),
                    case assignment_ids(Assignments, Opts) of
                        {ok, MsgIds} ->
                            (lists:all(
                                fun(Id) -> hb_maps:is_key(Id, Scheduled, Opts) end,
                                MsgIds
                            )) orelse
                                {assignment_not_scheduled, {node, Node}, {msg_ids, MsgIds}};
                        {error, Reason} ->
                            {assignment_id_error, {node, Node}, Reason}
                    end;
                Error ->
                    Error
            end
        end,
        true,
        Nodes
    ).

%% @doc Ensure each node assigns any message id at most once.
verify_assignment_unique(_Old, _Req, #{ nodes := Nodes, assignments := AssignmentsByNode }, _Opts) ->
    lists:foldl(
        fun(NodeInfo, Acc) ->
            case Acc of
                true ->
                    Opts = hb_maps:get(opts, NodeInfo, #{}, #{}),
                    Node = hb_maps:get(url, NodeInfo, undefined, Opts),
                    Assignments = hb_maps:get(Node, AssignmentsByNode, #{}, Opts),
                    case assignment_ids(Assignments, Opts) of
                        {ok, MsgIds} ->
                            (length(lists:usort(MsgIds)) =:= length(MsgIds)) orelse
                                {duplicate_assignments, {node, Node}, {msg_ids, MsgIds}};
                        {error, Reason} ->
                            {assignment_id_error, {node, Node}, Reason}
                    end;
                Error ->
                    Error
            end
        end,
        true,
        Nodes
    ).

%% @doc Ensure all nodes share a common assignment prefix.
verify_prefix_consistency(_Old, _Req, #{ nodes := Nodes, assignments := AssignmentsByNode }, _Opts) ->
    case Nodes of
        [] -> true;
        [RefNodeInfo | Rest] ->
            RefOpts = hb_maps:get(opts, RefNodeInfo, #{}, #{}),
            RefNode = hb_maps:get(url, RefNodeInfo, undefined, RefOpts),
            RefAssignments = hb_maps:get(RefNode, AssignmentsByNode, #{}, RefOpts),
            case assignment_ids(RefAssignments, RefOpts) of
                {error, Reason} ->
                    {assignment_id_error, {node, RefNode}, Reason};
                {ok, RefSeq} ->
                    lists:foldl(
                        fun(NodeInfo, Acc) ->
                            case Acc of
                                true ->
                                    Opts = hb_maps:get(opts, NodeInfo, #{}, #{}),
                                    Node = hb_maps:get(url, NodeInfo, undefined, Opts),
                                    Assignments = hb_maps:get(Node, AssignmentsByNode, #{}, Opts),
                                    case assignment_ids(Assignments, Opts) of
                                        {error, Reason} ->
                                            {assignment_id_error, {node, Node}, Reason};
                                        {ok, Seq} ->
                                            Common = min(length(RefSeq), length(Seq)),
                                            (lists:sublist(RefSeq, Common) ==
                                                lists:sublist(Seq, Common)) orelse
                                                {inconsistent_prefix, {node, Node}, {ref, RefNode}}
                                    end;
                                Error ->
                                    Error
                            end
                        end,
                        true,
                        Rest
                    )
            end
    end.

%% @doc Advance the invariant state machine to the next state.
next_state(_Old, _Req, New, _Opts) ->
    New.

%% @doc Extract message ids from assignments in slot order.
assignment_ids(Assignments, Opts) ->
    case ordered_assignments(Assignments, Opts) of
        {ok, Ordered} ->
            case lists:foldl(
                fun({_Slot, Assignment}, {ok, Acc}) ->
                    case dev_mysticeti_test_utils:assignment_message_id(Assignment, Opts) of
                        {ok, Id} -> {ok, [Id | Acc]};
                        {error, Reason} -> {error, {invalid_assignment_body, Reason}}
                    end;
                (_Item, {error, _} = Err) ->
                    Err
                end,
                {ok, []},
                Ordered
            ) of
                {ok, MsgIds} -> {ok, lists:reverse(MsgIds)};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Convert assignment slot keys to integers.
slots_to_ints(Assignments, Opts) ->
    case ordered_assignments(Assignments, Opts) of
        {ok, Ordered} -> {ok, [Slot || {Slot, _} <- Ordered]};
        {error, Reason} -> {error, Reason}
    end.

ordered_assignments(Assignments, Opts) ->
    case lists:foldl(
        fun({Slot, Assignment}, {ok, Acc}) ->
            case hb_util:safe_int(Slot) of
                {ok, Int} -> {ok, [{Int, Assignment} | Acc]};
                {error, _} -> {error, {invalid_slot, Slot}}
            end;
        (_Item, {error, _} = Err) ->
            Err
        end,
        {ok, []},
        hb_maps:to_list(Assignments, Opts)
    ) of
        {ok, Pairs} ->
            {ok, lists:sort(fun({A, _}, {B, _}) -> A < B end, Pairs)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Collect assignments from all nodes over a slot range.
collect_assignments(Nodes, ProcID, From, To) ->
    lists:foldl(
        fun(NodeInfo, Acc) ->
            Opts = hb_maps:get(opts, NodeInfo, #{}, #{}),
            Node = hb_maps:get(url, NodeInfo, undefined, Opts),
            Assignments =
                dev_mysticeti_test_utils:fetch_assignments_http(Node, ProcID, From, To, Opts),
            Acc#{ Node => Assignments }
        end,
        #{},
        Nodes
    ).

%% @doc Start N isolated Mysticeti nodes with separate stores.
start_mysticeti_nodes(NodeCount) ->
    Wallets = [ar_wallet:new() || _ <- lists:seq(1, NodeCount)],
    Validators = [hb_util:human_id(ar_wallet:to_address(W)) || W <- Wallets],
    BasePort = 25000 + rand:uniform(2000),
    Ports = lists:seq(BasePort, BasePort + NodeCount - 1),
    Triples =
        lists:zip3(lists:zip(Wallets, Validators), Ports, lists:seq(1, NodeCount)),
    Nodes =
        lists:map(
            fun({{Wallet, Author}, Port, Index}) ->
                LocalStore = hb_test_utils:test_store(),
                ok = hb_store:reset(LocalStore),
                ok = hb_store:start(LocalStore),
                Opts =
                    #{
                        store => [LocalStore],
                        priv_wallet => Wallet,
                        mysticeti_author => Author,
                        mysticeti_registry_namespace => Author,
                        port => Port,
                        host => <<"localhost">>,
                        gateway => <<"http://localhost:1">>,
                        http_connect_timeout => 2000,
                        http_request_send_timeout => 2000
                    },
                StartedUrl = hb_http_server:start_node(Opts),
                Url = trim_trailing_slash(StartedUrl),
                #{ url => Url, addr => Author, opts => Opts, index => Index }
            end,
            Triples
        ),
    {Nodes, Validators}.

%% @doc Trim a trailing slash from a URL.
trim_trailing_slash(<<>>) -> <<>>;
trim_trailing_slash(Url) when is_binary(Url) ->
    case binary:last(Url) of
        $/ -> binary:part(Url, 0, byte_size(Url) - 1);
        _ -> Url
    end.

%% @doc Wait until all nodes respond to scheduler status.
wait_for_nodes_ready(Nodes, Timeout) ->
    _Ready =
        hb_util:wait_until(
            fun() ->
                missing_ready_nodes(Nodes) == []
            end,
            Timeout
        ),
    case missing_ready_nodes(Nodes) of
        [] -> true;
        Missing -> {error, Missing}
    end.

%% @doc Return any nodes that do not report ready status.
missing_ready_nodes(Nodes) ->
    lists:foldl(
        fun(#{ url := Node, opts := Opts }, Acc) ->
            ReqOpts = http_opts(Opts),
            case catch hb_http:get(Node, <<"/~scheduler@1.0/status">>, ReqOpts) of
                {ok, Res} ->
                    Status = hb_maps:get(<<"status">>, Res, 200, ReqOpts),
                    case Status < 400 of
                        true -> Acc;
                        false -> [{Node, {status, Status}} | Acc]
                    end;
                Error ->
                    [{Node, {request_error, Error}} | Acc]
            end
        end,
        [],
        Nodes
    ).

%% @doc Register the local scheduler location via HTTP.
register_scheduler_location(Node, Opts) ->
    Req0 = #{
        <<"path">> => <<"/~scheduler@1.0/location">>,
        <<"method">> => <<"POST">>
    },
    Req = hb_message:commit(Req0, Opts),
    ReqOpts = http_opts(Opts),
    case hb_http:post(Node, Req, ReqOpts) of
        {ok, Response} ->
            case scheduler_location_from_response(Response, Opts) of
                {ok, Location} ->
                    {ok, hb_cache:ensure_all_loaded(Location, Opts)};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Post a scheduler-location record to a peer node.
post_scheduler_location(Node, Location, SenderOpts, Timeout) ->
    Deadline = erlang:system_time(millisecond) + Timeout,
    post_scheduler_location(Node, Location, SenderOpts, Deadline, none).

%% @doc Retry posting a scheduler-location until success or timeout.
post_scheduler_location(Node, Location, SenderOpts, Deadline, LastError) ->
    ReqOpts = http_opts(SenderOpts),
    Attempt =
        case catch hb_http:post(
            Node,
            <<"/~scheduler@1.0/location">>,
            Location,
            ReqOpts
        ) of
            {ok, Res} ->
                Status = hb_maps:get(<<"status">>, Res, 200, ReqOpts),
                case Status < 400 of
                    true -> {ok, Res};
                    false -> {error, {status, Status, Res}}
                end;
            Error ->
                {error, {request_error, Error}}
        end,
    case Attempt of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            Now = erlang:system_time(millisecond),
            case Now >= Deadline of
                true -> {error, {timeout, Reason, LastError}};
                false ->
                    timer:sleep(100),
                    post_scheduler_location(
                        Node,
                        Location,
                        SenderOpts,
                        Deadline,
                        Reason
                    )
            end
    end.

%% @doc Wait until all nodes have locations for all addresses.
wait_for_scheduler_locations(Nodes, Addresses, Timeout) ->
    _Ready =
        hb_util:wait_until(
            fun() ->
                missing_scheduler_locations(Nodes, Addresses) == []
            end,
            Timeout
        ),
    case missing_scheduler_locations(Nodes, Addresses) of
        [] -> true;
        Missing -> {error, Missing}
    end.

%% @doc Return any missing scheduler locations across nodes.
missing_scheduler_locations(Nodes, Addresses) ->
    lists:foldl(
        fun(#{ url := Node, opts := Opts }, Acc0) ->
            lists:foldl(
                fun(Address, Acc1) ->
                    ReqOpts =
                        http_opts(Opts),
                    case catch hb_http:get(
                        Node,
                        <<"/~scheduler@1.0/location?address=", Address/binary>>,
                        ReqOpts
                    ) of
                        {ok, Response} ->
                            Status = hb_maps:get(<<"status">>, Response, 200, ReqOpts),
                            case Status >= 400 of
                                true ->
                                    [{Node, Address, {status, Status}} | Acc1];
                                false ->
                                    case scheduler_location_from_response(Response, ReqOpts) of
                                        {ok, _} -> Acc1;
                                        {error, Reason} ->
                                            [{Node, Address, {invalid, Reason}} | Acc1]
                                    end
                            end;
                        Error ->
                            [{Node, Address, {request_error, Error}} | Acc1]
                    end
                end,
                Acc0,
                Addresses
            )
        end,
        [],
        Nodes
    ).

%% @doc Extract a scheduler-location from a response message.
scheduler_location_from_response(Response, Opts) ->
    case scheduler_location_candidate(Response, Opts) of
        {ok, Location0} ->
            case hb_message:with_only_committed(Location0, Opts) of
                {ok, Location} -> {ok, Location};
                {error, _} -> {ok, Location0}
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Find a scheduler-location candidate in a response.
scheduler_location_candidate(Response, Opts) ->
    case direct_scheduler_location(Response, Opts) of
        {ok, _} = Ok -> Ok;
        {error, _} ->
            decode_scheduler_location(Response, Opts)
    end.

%% @doc Decode a structured scheduler-location response if needed.
decode_scheduler_location(Response, Opts) ->
    try hb_message:convert(Response, <<"structured@1.0">>, <<"httpsig@1.0">>, Opts) of
        Decoded ->
            case direct_scheduler_location(Decoded, Opts) of
                {ok, _} = Ok -> Ok;
                {error, _} -> {error, {invalid_scheduler_location_response, Response}}
            end
    catch
        _:_ ->
            {error, {invalid_scheduler_location_response, Response}}
    end.

%% @doc Extract a scheduler-location when it is already embedded.
direct_scheduler_location(Response, Opts) ->
    case hb_maps:get(<<"type">>, Response, undefined, Opts) of
        <<"scheduler-location">> ->
            {ok, Response};
        _ ->
            case hb_maps:get(<<"body">>, Response, not_found, Opts) of
                Body when is_map(Body) ->
                    case hb_maps:get(<<"type">>, Body, undefined, Opts) of
                        <<"scheduler-location">> -> {ok, Body};
                        _ -> {error, not_found}
                    end;
                _ ->
                    {error, not_found}
            end
    end.

http_opts(Opts) ->
    Opts#{
        http_connect_timeout => 2000,
        http_request_send_timeout => 10000
    }.
