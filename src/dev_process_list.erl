%%% @doc A device that provides listing and tracking of computed AO processes
%%% on the current node. Similar to dev_cron's list functionality but for processes.
%%% This device allows users to:
%%% - List all computed processes on the node
%%% - Get statistics about process computation
%%% - Get detailed information about specific processes
%%% - Track which processes have active workers
-module(dev_process_list).
-export([info/1, info/3, list/3, stats/3, details/3, json/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Device info for path discovery
info(_) -> 
    #{ exports => [info, list, stats, details, json] }.

%% @doc Detailed device information
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"Process listing device for tracking computed processes">>,
        <<"version">> => <<"1.0">>,
        <<"paths">> => #{
            <<"info">> => <<"Get device info">>,
            <<"list">> => <<"List all computed processes on this node">>,
            <<"stats">> => <<"Get statistics about computed processes">>,
            <<"details">> => <<"Get details for a specific process {process}">>,
            <<"json">> => <<"Get process list as raw JSON array">>
        }
    },
    {ok, InfoBody}.

%% @doc List all computed processes on this node
list(_Msg1, _Msg2, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ComputedPath = hb_store:path(Store, [<<"computed">>]),
    AllProcessIDs = hb_cache:list(ComputedPath, Opts),
    
    ?event({process_list_found_processes, {count, length(AllProcessIDs)}}),
    
    ProcessList = lists:filtermap(
        fun(RawProcID) ->
            ProcID = hb_util:human_id(RawProcID),
            case dev_process_cache:latest(RawProcID, [], Opts) of
                {ok, Slot, StateMsg} ->
                    %% Check for active worker using the process group name
                    WorkerPid = hb_name:lookup(ProcID),
                    
                    %% Get process metadata if available
                    ProcessType = get_process_type(StateMsg, Opts),
                    
                    Info = #{
                        <<"process_id">> => ProcID,
                        <<"latest_slot">> => Slot,
                        <<"has_worker">> => is_pid(WorkerPid),
                        <<"worker_pid">> => format_pid(WorkerPid),
                        <<"type">> => ProcessType,
                        <<"status">> => <<"computed">>
                    },
                    {true, Info};
                _ ->
                    %% Process exists but no slots computed yet
                    Info = #{
                        <<"process_id">> => ProcID,
                        <<"latest_slot">> => 0,
                        <<"has_worker">> => false,
                        <<"worker_pid">> => null,
                        <<"status">> => <<"uncomputed">>
                    },
                    {true, Info}
            end
        end,
        AllProcessIDs
    ),
    
    ?event({process_list_returning, {process_count, length(ProcessList)}}),
    %% Return as a simple list - JSON serializer will handle it
    {ok, ProcessList}.

%% @doc Get statistics about computed processes
stats(_Msg1, _Msg2, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ComputedPath = hb_store:path(Store, [<<"computed">>]),
    AllProcessIDs = hb_cache:list(ComputedPath, Opts),
    
    %% Count active workers
    ActiveWorkers = length(lists:filter(
        fun(RawProcID) ->
            ProcID = hb_util:human_id(RawProcID),
            is_pid(hb_name:lookup(ProcID))
        end,
        AllProcessIDs
    )),
    
    %% Get slot statistics
    {TotalSlots, MaxSlot, ProcessesWithSlots} = lists:foldl(
        fun(RawProcID, {TotalAcc, MaxAcc, CountAcc}) ->
            case dev_process_cache:latest(RawProcID, [], Opts) of
                {ok, Slot, _} -> 
                    {TotalAcc + Slot, max(MaxAcc, Slot), CountAcc + 1};
                _ -> 
                    {TotalAcc, MaxAcc, CountAcc}
            end
        end,
        {0, 0, 0},
        AllProcessIDs
    ),
    
    Stats = #{
        <<"total_processes">> => length(AllProcessIDs),
        <<"processes_with_slots">> => ProcessesWithSlots,
        <<"active_workers">> => ActiveWorkers,
        <<"total_computed_slots">> => TotalSlots,
        <<"max_slot">> => MaxSlot,
        <<"average_slots_per_process">> => 
            case ProcessesWithSlots of
                0 -> 0;
                N -> TotalSlots div N
            end
    },
    
    {ok, Stats}.

%% @doc Get detailed info for a specific process
details(_Msg1, Msg2, Opts) ->
    case hb_ao:get(<<"{process}">>, Msg2, Opts) of
        not_found ->
            case hb_ao:get(<<"process">>, Msg2, Opts) of
                not_found ->
                    {error, <<"No process ID provided. Use ?process=PROCESS_ID">>};
                ProcIDParam ->
                    get_process_details(ProcIDParam, Opts)
            end;
        ProcIDParam ->
            get_process_details(ProcIDParam, Opts)
    end.

%% @doc Internal function to get process details
get_process_details(RawProcID, Opts) ->
    ProcID = hb_util:human_id(RawProcID),
    
    %% Get all slots for this process
    Store = hb_opts:get(store, no_viable_store, Opts),
    SlotPath = hb_store:path(Store, [<<"computed">>, ProcID, <<"slot">>]),
    AllSlots = hb_cache:list_numbered(SlotPath, Opts),
    
    case dev_process_cache:latest(RawProcID, [], Opts) of
        {ok, LatestSlot, StateMsg} ->
            %% Check for active worker
            WorkerPid = hb_name:lookup(ProcID),
            
            %% Get process info from state
            Process = hb_ao:get(<<"process">>, StateMsg, #{}, Opts),
            
            Details = #{
                <<"process_id">> => ProcID,
                <<"latest_slot">> => LatestSlot,
                <<"total_slots">> => length(AllSlots),
                <<"all_slots">> => lists:sort(AllSlots),
                <<"has_worker">> => is_pid(WorkerPid),
                <<"worker_info">> => get_worker_info(WorkerPid),
                <<"process_metadata">> => #{
                    <<"type">> => hb_maps:get(<<"type">>, Process, <<"unknown">>, Opts),
                    <<"variant">> => hb_maps:get(<<"variant">>, Process, <<"unknown">>, Opts),
                    <<"scheduler">> => hb_maps:get(<<"scheduler">>, Process, <<"unknown">>, Opts),
                    <<"execution_device">> => hb_maps:get(<<"execution-device">>, Process, <<"unknown">>, Opts)
                }
            },
            
            {ok, Details};
        _ ->
            %% Check if process exists at all
            case hb_cache:list(hb_store:path(Store, [<<"computed">>, ProcID]), Opts) of
                [] ->
                    {error, <<"Process not found or not computed">>};
                _ ->
                    %% Process exists but no latest state
                    Details = #{
                        <<"process_id">> => ProcID,
                        <<"latest_slot">> => 0,
                        <<"total_slots">> => 0,
                        <<"all_slots">> => [],
                        <<"has_worker">> => false,
                        <<"status">> => <<"uncomputed">>
                    },
                    {ok, Details}
            end
    end.

%% @doc Return list as raw JSON array
json(_Msg1, _Msg2, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ComputedPath = hb_store:path(Store, [<<"computed">>]),
    AllProcessIDs = hb_cache:list(ComputedPath, Opts),
    
    ProcessList = lists:filtermap(
        fun(RawProcID) ->
            ProcID = hb_util:human_id(RawProcID),
            case dev_process_cache:latest(RawProcID, [], Opts) of
                {ok, Slot, StateMsg} ->
                    WorkerPid = hb_name:lookup(ProcID),
                    ProcessType = get_process_type(StateMsg, Opts),
                    
                    Info = #{
                        <<"process_id">> => ProcID,
                        <<"latest_slot">> => Slot,
                        <<"has_worker">> => is_pid(WorkerPid),
                        <<"worker_pid">> => format_pid(WorkerPid),
                        <<"type">> => ProcessType,
                        <<"status">> => <<"computed">>
                    },
                    {true, Info};
                _ ->
                    Info = #{
                        <<"process_id">> => ProcID,
                        <<"latest_slot">> => 0,
                        <<"has_worker">> => false,
                        <<"worker_pid">> => null,
                        <<"status">> => <<"uncomputed">>
                    },
                    {true, Info}
            end
        end,
        AllProcessIDs
    ),
    
    %% Return as binary JSON string directly
    JsonBinary = hb_json:encode(ProcessList),
    {ok, #{
        <<"content-type">> => <<"application/json">>,
        <<"body">> => JsonBinary
    }}.

%%% Helper functions

%% @doc Extract process type from state message
get_process_type(StateMsg, Opts) ->
    case hb_ao:get(<<"process">>, StateMsg, not_found, Opts) of
        not_found -> <<"unknown">>;
        Process ->
            case hb_maps:get(<<"type">>, Process, not_found, Opts) of
                not_found -> <<"Process">>;
                Type -> Type
            end
    end.

%% @doc Format PID for JSON output
format_pid(undefined) -> null;
format_pid(Pid) when is_pid(Pid) -> 
    list_to_binary(pid_to_list(Pid)).

%% @doc Get detailed worker information
get_worker_info(undefined) -> null;
get_worker_info(Pid) when is_pid(Pid) ->
    #{
        <<"pid">> => format_pid(Pid),
        <<"message_queue_len">> => 
            case erlang:process_info(Pid, message_queue_len) of
                {message_queue_len, Len} -> Len;
                _ -> 0
            end,
        <<"memory">> =>
            case erlang:process_info(Pid, memory) of
                {memory, Mem} -> Mem;
                _ -> 0
            end,
        <<"reductions">> =>
            case erlang:process_info(Pid, reductions) of
                {reductions, Red} -> Red;
                _ -> 0
            end,
        <<"status">> =>
            case erlang:process_info(Pid, status) of
                {status, Status} -> atom_to_binary(Status, utf8);
                _ -> <<"unknown">>
            end
    }.

%%% Tests

%% @doc Test listing processes
list_processes_test() ->
    %% Initialize
    application:ensure_all_started(hb),
    
    %% Create and compute a test process
    Proc = dev_process:test_aos_process(),
    dev_process:schedule_aos_call(Proc, <<"return 1+1">>),
    
    ProcID = hb_message:id(Proc, all),
    ProcIDStr = hb_util:human_id(ProcID),
    
    %% Compute to slot 0
    {ok, _} = hb_ao:resolve(
        Proc,
        #{<<"path">> => <<"compute">>, <<"slot">> => 0},
        #{}
    ),
    
    %% List processes
    {ok, ProcessList} = list(#{}, #{}, #{}),
    
    %% Verify the process is listed
    ?assert(is_list(ProcessList)),
    ProcessFound = lists:any(
        fun(#{<<"process_id">> := ID}) -> 
            ID =:= ProcIDStr
        end,
        ProcessList
    ),
    ?assert(ProcessFound, "Process should be in the list").

%% @doc Test getting process details
details_test() ->
    %% Initialize
    application:ensure_all_started(hb),
    
    %% Create and compute a test process
    Proc = dev_process:test_aos_process(),
    dev_process:schedule_aos_call(Proc, <<"return 1+1">>),
    dev_process:schedule_aos_call(Proc, <<"return 2+2">>),
    
    ProcID = hb_message:id(Proc, all),
    ProcIDStr = hb_util:human_id(ProcID),
    
    %% Compute to slot 1
    {ok, _} = hb_ao:resolve(
        Proc,
        #{<<"path">> => <<"compute">>, <<"slot">> => 1},
        #{}
    ),
    
    %% Get details
    {ok, Details} = details(#{}, #{<<"process">> => ProcIDStr}, #{}),
    
    %% Verify details
    ?assertEqual(ProcIDStr, maps:get(<<"process_id">>, Details)),
    ?assertEqual(1, maps:get(<<"latest_slot">>, Details)),
    ?assert(lists:member(0, maps:get(<<"all_slots">>, Details))),
    ?assert(lists:member(1, maps:get(<<"all_slots">>, Details))).

%% @doc Test statistics
stats_test() ->
    %% Initialize
    application:ensure_all_started(hb),
    
    %% Get initial stats
    {ok, Stats} = stats(#{}, #{}, #{}),
    
    %% Verify stats structure
    ?assert(maps:is_key(<<"total_processes">>, Stats)),
    ?assert(maps:is_key(<<"active_workers">>, Stats)),
    ?assert(maps:is_key(<<"total_computed_slots">>, Stats)),
    ?assert(is_integer(maps:get(<<"total_processes">>, Stats))).