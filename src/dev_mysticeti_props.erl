%%% @doc Invariant-based tests for Mysticeti-C scheduling.
%%%
%%% These tests drive `GET/POST /ID/schedule` and check core safety properties:
%%% - slots are contiguous,
%%% - assignments are unique,
%%% - every assigned message was previously scheduled.
%%%
%%% The consensus logic under test is described in:
%%% - mysticeti-paper/algorithms/consensus_utils.tex (Alg. 1 predicates),
%%% - mysticeti-paper/algorithms/universal_committer.tex (Alg. 3 committer).
-module(dev_mysticeti_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Run invariant checks over randomized schedule sequences.
mysticeti_invariant_test_() ->
    {timeout, 180, fun mysticeti_invariant/0}.

%% @doc Run the invariant state machine for a single-node Mysticeti instance.
mysticeti_invariant() ->
    ok = hb_invariant:state_machine(
        #{
            states => [fun init_state/1],
            requests => [fun schedule_request/2],
            properties => [
                fun verify_assignment_slots/4,
                fun verify_assignment_subset/4,
                fun verify_assignment_unique/4
            ],
            next => fun next_state/4,
            runs => 2,
            length => 60,
            opts => #{}
        }
    ).

%% @doc Build the initial invariant state with a single node and process.
init_state(_Opts) ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Store = hb_test_utils:test_store(),
    ok = hb_store:reset(Store),
    ok = hb_store:start(Store),
    Port = 22000 + rand:uniform(2000),
    Opts =
        #{
            store => [Store],
            priv_wallet => Wallet,
            mysticeti_author => Address,
            mysticeti_registry_namespace => Address,
            port => Port,
            host => <<"localhost">>
        },
    _Node = hb_http_server:start_node(Opts),
    ProcBase =
        #{
            <<"device">> => <<"process@1.0">>,
            <<"scheduler-device">> => <<"mysticeti@1.0">>,
            <<"scheduler-location">> => [Address],
            <<"mysticeti">> => #{
                <<"validators">> => [Address],
                <<"stakers">> => [#{ <<"id">> => Address, <<"stake">> => 1 }],
                <<"peers">> => [],
                <<"wave-length">> => 3,
                <<"proposer-offset">> => 0,
                <<"num-proposers">> => 1
            },
            <<"type">> => <<"Process">>
        },
    Proc = hb_message:commit(ProcBase, Opts),
    ProcID = hb_util:human_id(hb_message:id(Proc, all, Opts)),
    ProcLoaded = hb_cache:ensure_all_loaded(Proc, Opts),
    {ok, _} = hb_cache:write(ProcLoaded, Opts),
    #{
        node => node_url(Port),
        opts => Opts,
        proc => ProcLoaded,
        proc_id => ProcID,
        scheduled => #{},
        assignments => #{},
        max_slots => 120
    }.

%% @doc Generate a randomized schedule request for invariants.
schedule_request(_State, _Opts) ->
    Body = hb_util:bin(hb_invariant:string(8)),
    fun(S, _SOpts) -> schedule_and_update(S, Body) end.

%% @doc Schedule a message and update assignments in state.
schedule_and_update(State, Body) ->
    Opts = hb_maps:get(opts, State, #{}, #{}),
    ProcID = hb_maps:get(proc_id, State, undefined, Opts),
    Node = hb_maps:get(node, State, undefined, Opts),
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
    timer:sleep(50),
    Assignments =
        dev_mysticeti_test_utils:fetch_assignments_http(
            Node,
            ProcID,
            0,
            hb_maps:get(max_slots, State, 0, Opts),
            Opts
        ),
    Scheduled0 = hb_maps:get(scheduled, State, #{}, Opts),
    MsgId = hb_message:id(Msg, all, Opts),
    Scheduled = Scheduled0#{ MsgId => true },
    {ok, State#{
        scheduled := Scheduled,
        assignments := Assignments
    }}.

%% @doc Ensure assignment slots are contiguous starting at 0.
verify_assignment_slots(_Old, _Req, #{ assignments := Assignments }, Opts) ->
    Slots =
        lists:sort(
            [hb_util:int(Slot) || Slot <- hb_maps:keys(Assignments, Opts)]
        ),
    Expected =
        case Slots of
            [] -> [];
            _ -> lists:seq(0, length(Slots) - 1)
        end,
    (Slots == Expected) orelse {non_contiguous_slots, {slots, Slots}}.

%% @doc Ensure each assignment was previously scheduled.
verify_assignment_subset(_Old, _Req, #{ assignments := Assignments, scheduled := Scheduled }, Opts) ->
    MsgIds = assignment_ids(Assignments, Opts),
    (lists:all(fun(Id) -> hb_maps:is_key(Id, Scheduled, Opts) end, MsgIds)) orelse
        {assignment_not_scheduled, {assigned, MsgIds}}.

%% @doc Ensure no message id is assigned more than once.
verify_assignment_unique(_Old, _Req, #{ assignments := Assignments }, Opts) ->
    MsgIds = assignment_ids(Assignments, Opts),
    (length(lists:usort(MsgIds)) =:= length(MsgIds)) orelse
        {duplicate_assignments, {msg_ids, MsgIds}}.

%% @doc Advance the invariant state machine to the next state.
next_state(_Old, _Req, New, _Opts) ->
    New.

%% @doc Extract message ids from assignments in slot order.
assignment_ids(Assignments, Opts) ->
    lists:map(
        fun({_Slot, Assignment}) ->
            case dev_mysticeti_test_utils:assignment_message_id(Assignment, Opts) of
                {ok, Id} -> Id;
                {error, Reason} -> erlang:error({invalid_assignment, Reason})
            end
        end,
        hb_util:to_sorted_list(Assignments, Opts)
    ).

%% @doc Build a local node URL for a port.
node_url(Port) ->
    <<"http://localhost:", (integer_to_binary(Port))/binary>>.
