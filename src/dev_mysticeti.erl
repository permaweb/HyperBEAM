%%% @doc Mysticeti consensus scheduler device.
%%%
%%% This device exposes an AO-Core scheduler interface backed by the
%%% Mysticeti-style consensus server:
%%% - `POST /~mysticeti@1.0/schedule` schedules a message (creates a proposer
%%%   block and returns pending status until it is committed).
%%% - `GET /~mysticeti@1.0/schedule` returns committed assignments.
%%% - `GET /~mysticeti@1.0/slot` returns the latest committed slot.
%%% - `POST /~mysticeti@1.0/block` ingests a consensus block from peers.
%%%
%%% The consensus logic itself lives in `dev_mysticeti_server`. This device is
%%% a thin routing and formatting layer that maintains compatibility with the
%%% `process@1.0` scheduler expectations.
%%%
%%% Reference: "Mysticeti: Reaching the Limits of Latency with Uncertified DAGs"
%%% (Babel et al., arXiv:2310.14821).
-module(dev_mysticeti).
-export([info/0]).
-export([start/0, router/4]).
-export([schedule/3, slot/3, next/3, status/3, block/3, checkpoint/1]).
-include("include/hb.hrl").

%%% The maximum number of assignments to return in a schedule query.
-define(MAX_ASSIGNMENT_QUERY_LEN, 1000).

%% @doc Helper to ensure the environment is started.
start() ->
    application:ensure_all_started(hb),
    <<I1:32/unsigned-integer, I2:32/unsigned-integer, I3:32/unsigned-integer>>
        = crypto:strong_rand_bytes(12),
    rand:seed(exsplus, {I1, I2, I3}),
    ok.

%% @doc Device info declaration.
info() ->
    #{
        exports =>
            [
                <<"status">>,
                <<"next">>,
                <<"schedule">>,
                <<"slot">>,
                <<"block">>,
                <<"checkpoint">>
            ],
        excludes => [set, keys],
        default => fun router/4
    }.

%% @doc Default router: treat unknown paths as schedule.
router(_, Base, Req, Opts) ->
    schedule(Base, Req, Opts).

%% @doc Scheduler interface.
schedule(Base, Req, Opts) ->
    case hb_util:key_to_atom(hb_ao:get(<<"method">>, Req, <<"GET">>, Opts)) of
        post -> post_schedule(Base, Req, Opts);
        get -> get_schedule(Base, Req, Opts)
    end.

%% @doc POST /schedule. Adds a message to the consensus DAG.
post_schedule(Base, Req, Opts) ->
    RawToSched = find_message_to_schedule(Base, Req, Opts),
    try hb_cache:ensure_all_loaded(RawToSched, Opts) of
        ToSched ->
            ProcID = find_target_id(Base, Req, ToSched, Opts),
            ProcMsg = find_process_message(Base, ToSched, Opts),
            case dev_mysticeti_registry:find(ProcID, ProcMsg, Opts) of
                not_found ->
                    {error, #{ <<"status">> => 404, <<"body">> => <<"No local scheduler">> }};
                PID ->
                    case hb_message:with_only_committed(ToSched, Opts) of
                        {ok, OnlyCommitted} ->
                            case hb_ao:get(<<"type">>, OnlyCommitted, Opts) of
                                <<"Process">> ->
                                    ok = hb_cache:write(OnlyCommitted, Opts);
                                _ -> ok
                            end,
                            Res = dev_mysticeti_server:schedule(PID, OnlyCommitted),
                            format_post_schedule_result(Res);
                        {error, Err} ->
                            {error, #{
                                <<"status">> => 400,
                                <<"body">> => <<"Message invalid: commitments invalid.">>,
                                <<"reason">> => Err
                            }}
                    end
            end
    catch
        error:{necessary_message_not_found, _, _} ->
            {error, #{
                <<"status">> => 404,
                <<"body">> => <<"Cannot fully load message to schedule.">>
            }}
    end.

format_post_schedule_result({committed, Assignment}) ->
    {ok, Assignment};
format_post_schedule_result({pending, Info}) ->
    {ok, #{
        <<"status">> => 202,
        <<"body">> => <<"pending">>,
        <<"pending">> => Info
    }};
format_post_schedule_result(Other) ->
    {ok, #{ <<"status">> => 202, <<"body">> => <<"pending">>, <<"pending">> => Other }}.

%% @doc GET /schedule. Returns committed assignments.
get_schedule(Base, Req, Opts) ->
    ProcID = hb_util:human_id(find_target_id(Base, Req, Opts)),
    From =
        case hb_ao:get(<<"from">>, Req, not_found, Opts) of
            not_found -> 0;
            X when X < 0 -> 0;
            FromRes -> hb_util:int(FromRes)
        end,
    To =
        case hb_ao:get(<<"to">>, Req, not_found, Opts) of
            not_found -> undefined;
            ToRes -> hb_util:int(ToRes)
        end,
    Format = hb_ao:get(<<"accept">>, Req, <<"application/http">>, Opts),
    generate_local_schedule(Format, ProcID, From, To, Opts).

%% @doc Return the next committed assignment for a process.
next(Base, Req, Opts) ->
    LastProcessed =
        hb_util:int(
            hb_ao:get(
                <<"at-slot">>,
                Base,
                -1,
                Opts#{ hashpath => ignore }
            )
        ),
    ProcID = dev_process_lib:process_id(Base, Req, Opts),
    NextSlot = LastProcessed + 1,
    case dev_scheduler_cache:read(ProcID, NextSlot, Opts) of
        {ok, Assignment} ->
            {ok, #{ <<"body">> => Assignment, <<"state">> => Base }};
        not_found ->
            {error, #{
                <<"status">> => 404,
                <<"reason">> =>
                    <<"Requested slot not yet available in schedule.">>
            }}
    end.

%% @doc Return the current committed slot for a process.
slot(Base, Req, Opts) ->
    ProcID = find_target_id(Base, Req, Opts),
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    CurrentSlot =
        case dev_mysticeti_registry:find(ProcID, false, Opts) of
            not_found ->
                case dev_scheduler_cache:latest(ProcID, Opts) of
                    not_found -> -1;
                    {Slot, _} -> Slot
                end;
            PID ->
                Info = dev_mysticeti_server:info(PID),
                maps:get(current, Info, -1)
        end,
    {ok, #{
        <<"process">> => ProcID,
        <<"current">> => CurrentSlot,
        <<"timestamp">> => Timestamp,
        <<"block-height">> => Height,
        <<"block-hash">> => Hash,
        <<"cache-control">> => <<"no-store">>
    }}.

%% @doc Status summary.
status(_Base, _Req, _Opts) ->
    {ok, #{
        <<"device">> => <<"mysticeti@1.0">>,
        <<"address">> => hb:address(),
        <<"processes">> => dev_mysticeti_registry:get_processes()
    }}.

%% @doc Ingest a block from peers.
block(Base, Req, Opts) ->
    BlockMsg =
        hb_ao:get(
            <<"body">>,
            Req,
            Req,
            Opts#{ hashpath => ignore }
        ),
    % Avoid AO-Core resolve here (block messages are not necessarily verified).
    ProcID =
        case hb_maps:find(<<"process">>, BlockMsg, Opts) of
            error -> find_target_id(Base, Req, Opts);
            {ok, P} -> P
        end,
    ProcMsg = find_process_message(Base, BlockMsg, Opts),
    case dev_mysticeti_registry:find(ProcID, ProcMsg, Opts) of
        not_found ->
            {error, #{ <<"status">> => 404, <<"body">> => <<"No local scheduler">> }};
        PID ->
            dev_mysticeti_server:ingest_block(PID, BlockMsg),
            {ok, #{ <<"status">> => 202, <<"body">> => <<"accepted">> }}
    end.

%% @doc Returns the current state of the scheduler.
checkpoint(State) -> {ok, State}.

%%% Helper functions

%% @doc Find the target process ID from a request.
find_target_id(Base, Req, ToSched, Opts) ->
    case hb_ao:get(<<"type">>, ToSched, not_found, Opts) of
        <<"Process">> ->
            dev_process_lib:process_id(ToSched, #{}, Opts);
        _ ->
            case hb_ao:get(<<"target">>, ToSched, not_found, Opts) of
                not_found -> find_target_id(Base, Req, Opts);
                Target -> hb_util:human_id(Target)
            end
    end.
find_target_id(Base, Req, Opts) ->
    TempOpts = Opts#{ hashpath => ignore },
    case hb_ao:resolve(Req, <<"target">>, TempOpts) of
        {ok, Target} ->
            Target;
        _ ->
            case hb_ao:resolve(Req, <<"type">>, TempOpts) of
                {ok, <<"Process">>} ->
                    dev_process_lib:process_id(Req, #{}, Opts);
                _ ->
                    case hb_ao:resolve(Base, <<"process">>, TempOpts) of
                        {ok, _Process} ->
                            dev_process_lib:process_id(Base, #{}, Opts);
                        _ ->
                            case hb_ao:get(<<"type">>, Base, TempOpts) of
                                <<"Process">> ->
                                    dev_process_lib:process_id(Base, #{}, Opts);
                                _ ->
                                    dev_process_lib:process_id(Req, #{}, Opts)
                            end
                    end
            end
    end.

%% @doc Find the message to schedule.
find_message_to_schedule(_Base, Req, Opts) ->
    Subject =
        hb_ao:get(
            <<"subject">>,
            Req,
            not_found,
            Opts#{ hashpath => ignore }
        ),
    case Subject of
        <<"self">> -> Req;
        not_found ->
            hb_ao:get(<<"body">>, Req, Req, Opts#{ hashpath => ignore });
        _ ->
            hb_ao:get(Subject, Req, Opts#{ hashpath => ignore })
    end.

%% @doc Locate a process message from Base or ToSched.
find_process_message(Base, ToSched, Opts) ->
    case hb_ao:get(<<"type">>, ToSched, not_found, Opts#{ hashpath => ignore }) of
        <<"Process">> -> ToSched;
        _ ->
            case hb_ao:get(<<"type">>, Base, not_found, Opts#{ hashpath => ignore }) of
                <<"Process">> -> Base;
                _ ->
                    hb_ao:get(<<"process">>, Base, Base, Opts#{ hashpath => ignore })
            end
    end.

%% @doc Generate a local schedule response.
generate_local_schedule(Format, ProcID, From, To, Opts) ->
    {Assignments, More} = get_local_assignments(ProcID, From, To, Opts),
    FormatterFun =
        case uri_string:percent_decode(Format) of
            <<"application/aos-2">> ->
                fun dev_scheduler_formats:assignments_to_aos2/4;
            _ ->
                fun dev_scheduler_formats:assignments_to_bundle/4
        end,
    FormatterFun(ProcID, Assignments, More, Opts).

%% @doc Get assignments from the local cache.
get_local_assignments(ProcID, From, undefined, Opts) ->
    case dev_scheduler_cache:latest(ProcID, Opts) of
        not_found -> {[], false};
        {Slot, _} -> get_local_assignments(ProcID, From, Slot, Opts)
    end;
get_local_assignments(ProcID, From, RequestedTo, Opts) ->
    ComputedTo =
        case (RequestedTo - From) > ?MAX_ASSIGNMENT_QUERY_LEN of
            true -> From + ?MAX_ASSIGNMENT_QUERY_LEN;
            false -> RequestedTo
        end,
    {
        read_local_assignments(ProcID, From, ComputedTo, Opts),
        ComputedTo < RequestedTo
    }.

read_local_assignments(_ProcID, From, To, _Opts) when From > To ->
    [];
read_local_assignments(ProcID, CurrentSlot, To, Opts) ->
    case dev_scheduler_cache:read(ProcID, CurrentSlot, Opts) of
        not_found -> [];
        {ok, Assignment} ->
            [
                Assignment
                | read_local_assignments(ProcID, CurrentSlot + 1, To, Opts)
            ]
    end.
