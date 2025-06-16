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


%% @doc Find a process associated with the processor ID in the local registry
%% If the process is not found, it will not create a new one
find(ProcID) -> find(ProcID, false).


%% @doc Find a process associated with the processor ID in the local registry
%% If the process is not found and `GenIfNotHosted' is true, it attemps to
%% create a new one
find(ProcID, ProcMsgOrFalse) ->
    find(ProcID, ProcMsgOrFalse, #{priv_wallet => hb:wallet()}).


%% @doc Same as `find/2' but with additional options passed when spawning a
%% new process (if needed)
find(ProcID, ProcMsgOrFalse, Opts) ->
    case hb_name:lookup({<<"scheduler@1.0">>, ProcID}) of
        undefined -> maybe_new_proc(ProcID, ProcMsgOrFalse, Opts);
        Pid -> Pid
    end.


%% @doc Return a list of all currently registered ProcID.
get_processes() ->
    ?event({getting_processes, hb_name:all()}),
    [ ProcID || {{<<"scheduler@1.0">>, ProcID}, _} <- hb_name:all() ].


maybe_new_proc(_ProcID, false, _Opts) -> not_found;
maybe_new_proc(ProcID, ProcMsg, Opts) ->
    dev_scheduler_server:start(ProcID, ProcMsg, Opts).


%%% Tests


generate_test_procs() ->
    [hb_message:commit(
       #{
         <<"type">> => <<"Process">>,
         <<"image">> => <<0:(1024 * 32)>>
        },
       #{priv_wallet => hb:wallet()}),
     hb_message:commit(
       #{
         <<"type">> => <<"Process">>,
         <<"image">> => <<0:(1024 * 32)>>
        },
       #{priv_wallet => hb:wallet()})].


find_non_existent_process_test() ->
    [Proc1, _Proc2] = generate_test_procs(),
    start(),
    ?assertEqual(not_found, ?MODULE:find(hb_message:id(Proc1, all))).


create_and_find_process_test() ->
    [Proc1, _Proc2] = generate_test_procs(),
    ID = hb_message:id(Proc1, all),
    start(),
    Pid1 = ?MODULE:find(ID, Proc1),
    ?assert(is_pid(Pid1)),
    ?assertEqual(Pid1, ?MODULE:find(ID, Proc1)).


create_multiple_processes_test() ->
    [Proc1, Proc2] = generate_test_procs(),
    start(),
    ID1 = hb_message:id(Proc1, all),
    ID2 = hb_message:id(Proc2, all),
    Pid1 = ?MODULE:find(ID1, Proc1),
    Pid2 = ?MODULE:find(ID2, Proc2),
    ?assert(is_pid(Pid1)),
    ?assert(is_pid(Pid2)),
    ?assertNotEqual(Pid1, Pid2),
    ?assertEqual(Pid1, ?MODULE:find(ID1, Proc1)),
    ?assertEqual(Pid2, ?MODULE:find(ID2, Proc2)).


get_all_processes_test() ->
    [Proc1, Proc2] = generate_test_procs(),
    start(),
    ID1 = hb_message:id(Proc1, all),
    ID2 = hb_message:id(Proc2, all),
    ?MODULE:find(ID1, Proc1),
    ?MODULE:find(ID2, Proc2),
    Processes = ?MODULE:get_processes(),
    ?assert(length(Processes) >= 2),
    ?event({processes, Processes}),
    ?assert(lists:member(ID1, Processes)),
    ?assert(lists:member(ID2, Processes)).
