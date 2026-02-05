%%% @doc Invariant-based tests for Mysticeti consensus scheduling.
%%% These tests exercise the AO-Core HTTP interface and validate that
%%% assignments are monotonic, unique, and correspond to scheduled messages.
-module(dev_mysticeti_props).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Run invariant checks over randomized schedule sequences.
mysticeti_invariant_test_() ->
    {timeout, 180, fun mysticeti_invariant/0}.

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
            runs => 3,
            length => 120,
            opts => #{}
        }
    ).

init_state(_Opts) ->
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Store = hb_test_utils:test_store(),
    ok = hb_store:reset(Store),
    ok = hb_store:start(Store),
    Port = 22000 + rand:uniform(2000),
    _Node =
        hb_http_server:start_node(
            #{
                store => [Store],
                priv_wallet => Wallet,
                port => Port,
                host => <<"localhost">>
            }
        ),
    Proc =
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
    ProcID = hb_message:id(Proc, all, #{}),
    BaseOpts = #{ priv_wallet => Wallet, store => [Store], mysticeti_author => Address },
    Pid = dev_mysticeti_registry:find(ProcID, Proc, BaseOpts),
    #{
        pid => Pid,
        opts => BaseOpts,
        proc => Proc,
        proc_id => ProcID,
        scheduled => #{},
        assignments => #{}
    }.

schedule_request(_State, _Opts) ->
    Body = hb_util:bin(hb_invariant:string(8)),
    fun(S, _SOpts) -> schedule_and_update(S, Body) end.

schedule_and_update(State, Body) ->
    Opts = maps:get(opts, State),
    ProcID = maps:get(proc_id, State),
    Pid = maps:get(pid, State),
    Msg =
        hb_message:commit(
            #{
                <<"target">> => ProcID,
                <<"body">> => Body,
                <<"type">> => <<"Message">>
            },
            Opts
        ),
    {ok, _} = hb_cache:write(Msg, Opts),
    _ = dev_mysticeti_server:schedule(Pid, Msg),
    Assignments = fetch_assignments_cache(ProcID, Opts),
    Scheduled0 = maps:get(scheduled, State),
    MsgId = hb_message:id(Msg, all, Opts),
    Scheduled = Scheduled0#{ MsgId => true },
    {ok, State#{
        scheduled := Scheduled,
        assignments := normalize_assignments(Assignments)
    }}.

normalize_assignments(Assignments) ->
    lists:foldl(
        fun({Slot, Assignment}, Acc) ->
            Acc#{ Slot => Assignment }
        end,
        #{},
        Assignments
    ).

fetch_assignments_cache(ProcID, Opts) ->
    case dev_scheduler_cache:list(ProcID, Opts) of
        [] -> [];
        Slots ->
            lists:foldl(
                fun(Slot, Acc) ->
                    case dev_scheduler_cache:read(ProcID, Slot, Opts) of
                        {ok, Assignment} -> [{hb_util:int(Slot), Assignment} | Acc];
                        _ -> Acc
                    end
                end,
                [],
                Slots
            )
    end.

verify_assignment_slots(_Old, _Req, #{ assignments := Assignments }, _Opts) ->
    Slots = lists:sort(maps:keys(Assignments)),
    Expected = lists:seq(0, length(Slots) - 1),
    (Slots == Expected) orelse {non_contiguous_slots, {slots, Slots}}.

verify_assignment_subset(_Old, _Req, #{ assignments := Assignments, scheduled := Scheduled }, Opts) ->
    AssignedMsgIds =
        lists:map(
            fun(Assignment) ->
                Msg = hb_ao:get(<<"body">>, Assignment, Opts),
                hb_message:id(Msg, all, Opts)
            end,
            maps:values(Assignments)
        ),
    (lists:all(fun(Id) -> maps:is_key(Id, Scheduled) end, AssignedMsgIds)) orelse
        {assignment_not_scheduled, {assigned, AssignedMsgIds}}.

verify_assignment_unique(_Old, _Req, #{ assignments := Assignments }, Opts) ->
    MsgIds =
        lists:map(
            fun(Assignment) ->
                hb_message:id(hb_ao:get(<<"body">>, Assignment, Opts), all, Opts)
            end,
            maps:values(Assignments)
        ),
    (length(lists:usort(MsgIds)) =:= length(MsgIds)) orelse
        {duplicate_assignments, {msg_ids, MsgIds}}.

next_state(_Old, _Req, New, _Opts) ->
    New.
