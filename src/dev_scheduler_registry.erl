-module(dev_scheduler_registry).
-export([start/0, find/1, find/2, find/3, get_wallet/0, get_processes/0]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% A simple registry for local services in AO, using pg. Currently,
%%% only SU processes are supported.

start() ->
    hb_name:start(),
    ok.

get_wallet() ->
    % TODO: We might want to use a different wallet per SU later.
    hb:wallet().

%%% @doc Find a process associated with the processor ID in the local registry
%%% If the process is not found, it will not create a new one 
find(ProcID) -> find(ProcID, false).

%%% @doc Find a process associated with the processor ID in the local registry
%%% If the process is not found and `GenIfNotHosted' is true, it attemps to create a new one 
find(ProcID, GenIfNotHosted) ->
    find(ProcID, GenIfNotHosted, #{ priv_wallet => hb:wallet() }).

%%% @doc Same as `find/2' but with additional options passed when spawning a new process (if needed)
find(ProcID, GenIfNotHosted, Opts) ->
    case hb_name:lookup({dev_scheduler, ProcID}) of
        undefined -> maybe_new_proc(ProcID, GenIfNotHosted, Opts);
        Pid -> Pid
    end.

%%% @doc Return a list of all currently registered ProcID.
get_processes() ->
    ?event({getting_processes, hb_name:all()}),
    [ ProcID || {{dev_scheduler, ProcID}, _} <- hb_name:all() ].

maybe_new_proc(_ProcID, false, _Opts) -> not_found;
maybe_new_proc(ProcID, _GenIfNotHosted, Opts) -> 
    dev_scheduler_server:start(ProcID, Opts).

%%% Tests

-define(TEST_PROC_ID1, <<0:256>>).
-define(TEST_PROC_ID2, <<1:256>>).

find_non_existent_process_test() ->
    start(),
    ?assertEqual(not_found, ?MODULE:find(?TEST_PROC_ID1)).

create_and_find_process_test() ->
    start(),
    Pid1 = ?MODULE:find(?TEST_PROC_ID1, true),
    ?assert(is_pid(Pid1)),
    ?assertEqual(Pid1, ?MODULE:find(?TEST_PROC_ID1)).

create_multiple_processes_test() ->
    start(),
    Pid1 = ?MODULE:find(?TEST_PROC_ID1, true),
    Pid2 = ?MODULE:find(?TEST_PROC_ID2, true),
    ?assert(is_pid(Pid1)),
    ?assert(is_pid(Pid2)),
    ?assertNotEqual(Pid1, Pid2),
    ?assertEqual(Pid1, ?MODULE:find(?TEST_PROC_ID1)),
    ?assertEqual(Pid2, ?MODULE:find(?TEST_PROC_ID2)).

get_all_processes_test() ->
    start(),
    ?MODULE:find(?TEST_PROC_ID1, true),
    ?MODULE:find(?TEST_PROC_ID2, true),
    Processes = ?MODULE:get_processes(),
    ?assert(length(Processes) >= 2),
    ?event({processes, Processes}),
    ?assert(lists:member(?TEST_PROC_ID1, Processes)),
    ?assert(lists:member(?TEST_PROC_ID2, Processes)).