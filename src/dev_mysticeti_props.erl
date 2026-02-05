%%% @doc Invariant-based tests for Mysticeti-C scheduling.
%%%
%%% These tests drive the AO-Core HTTP surface (`/schedule`) and check the
%%% minimal safety properties implied by the consensus design:
%%% - slots are contiguous and monotonic,
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
    ProcID = hb_message:id(Proc, all, Opts),
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

schedule_request(_State, _Opts) ->
    Body = hb_util:bin(hb_invariant:string(8)),
    fun(S, _SOpts) -> schedule_and_update(S, Body) end.

schedule_and_update(State, Body) ->
    Opts = hb_maps:get(opts, State, #{}, #{}),
    ProcID = hb_maps:get(proc_id, State, undefined, Opts),
    Proc = hb_maps:get(proc, State, undefined, Opts),
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
    Req = #{
        <<"path">> => <<"/~mysticeti@1.0/schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> => Msg,
        <<"process">> => Proc
    },
    {ok, Res} = hb_http:post(Node, Req, Opts),
    case hb_ao:get(<<"status">>, Res, 200, Opts) of
        Status when Status >= 400 ->
            erlang:error({schedule_failed, Status, Res});
        _ -> ok
    end,
    timer:sleep(50),
    Assignments =
        fetch_assignments_http(
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

fetch_assignments_http(Node, ProcID, From, To, Opts) ->
    ReqOpts = Opts#{ http_only_result => false },
    case catch hb_http:get(
        Node,
        <<"/~mysticeti@1.0/schedule&target=", ProcID/binary,
          "&from=", (integer_to_binary(From))/binary,
          "&to=", (integer_to_binary(To))/binary>>,
        ReqOpts
    ) of
        {ok, Response} ->
            Assignments0 = extract_assignments(Response, ReqOpts),
            Assignments =
                case hb_maps:size(Assignments0, ReqOpts) of
                    0 ->
                        Schedule = hb_maps:get(<<"body">>, Response, Response, ReqOpts),
                        extract_assignments(Schedule, ReqOpts);
                    _ ->
                        Assignments0
                end,
            hb_private:reset(Assignments);
        _ ->
            #{}
    end.

extract_assignments(Schedule, Opts) ->
    case hb_maps:get(<<"assignments">>, Schedule, not_found, Opts) of
        not_found ->
            case hb_ao:get(<<"assignments">>, Schedule, not_found, Opts) of
                not_found ->
                    case hb_maps:get(<<"slot">>, Schedule, not_found, Opts) of
                        not_found ->
                            case hb_maps:get(<<"body">>, Schedule, not_found, Opts) of
                                Body when is_map(Body) ->
                                    extract_assignments(Body, Opts);
                                _ ->
                                    #{}
                            end;
                        _Slot ->
                            normalize_assignments(Schedule, Opts)
                    end;
                Assignments ->
                    normalize_assignments(Assignments, Opts)
            end;
        Assignments ->
            normalize_assignments(Assignments, Opts)
    end.

normalize_assignments(Map, Opts) when is_map(Map) ->
    case hb_maps:get(<<"slot">>, Map, not_found, Opts) of
        not_found ->
            Numeric = numeric_assignment_map(Map, Opts),
            case hb_maps:size(Numeric, Opts) of
                0 -> #{};
                _ -> Numeric
            end;
        Slot ->
            #{ Slot => Map }
    end;
normalize_assignments(List, Opts) when is_list(List) ->
    lists:foldl(
        fun(Item, Acc) ->
            case hb_maps:get(<<"slot">>, Item, not_found, Opts) of
                not_found -> Acc;
                Slot -> hb_maps:put(Slot, Item, Acc, Opts)
            end
        end,
        #{},
        List
    );
normalize_assignments(_, _Opts) ->
    #{}.

numeric_assignment_map(Map, Opts) ->
    lists:foldl(
        fun(Key, Acc) ->
            case hb_util:safe_int(Key) of
                {ok, IntKey} ->
                    Value = hb_maps:get(Key, Map, undefined, Opts),
                    hb_maps:put(IntKey, Value, Acc, Opts);
                {error, _} ->
                    Acc
            end
        end,
        #{},
        hb_maps:keys(Map, Opts)
    ).

verify_assignment_slots(_Old, _Req, #{ assignments := Assignments }, Opts) ->
    Slots =
        [hb_util:int(Slot)
         || {Slot, _} <- hb_util:to_sorted_list(Assignments, Opts)],
    Expected =
        case Slots of
            [] -> [];
            _ -> lists:seq(0, length(Slots) - 1)
        end,
    (Slots == Expected) orelse {non_contiguous_slots, {slots, Slots}}.

verify_assignment_subset(_Old, _Req, #{ assignments := Assignments, scheduled := Scheduled }, Opts) ->
    MsgIds = assignment_ids(Assignments, Opts),
    (lists:all(fun(Id) -> hb_maps:is_key(Id, Scheduled, Opts) end, MsgIds)) orelse
        {assignment_not_scheduled, {assigned, MsgIds}}.

verify_assignment_unique(_Old, _Req, #{ assignments := Assignments }, Opts) ->
    MsgIds = assignment_ids(Assignments, Opts),
    (length(lists:usort(MsgIds)) =:= length(MsgIds)) orelse
        {duplicate_assignments, {msg_ids, MsgIds}}.

next_state(_Old, _Req, New, _Opts) ->
    New.

assignment_ids(Assignments, Opts) ->
    lists:map(
        fun({_Slot, Assignment}) ->
            Msg = hb_ao:get(<<"body">>, Assignment, Opts),
            hb_message:id(Msg, all, Opts)
        end,
        hb_util:to_sorted_list(Assignments, Opts)
    ).

node_url(Port) ->
    <<"http://localhost:", (integer_to_binary(Port))/binary>>.
