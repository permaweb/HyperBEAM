%% @doc Registry for Mysticeti consensus scheduling servers.
%%
%% Keeps a local mapping from `{<<"mysticeti@1.0">>, ProcID}` to a scheduler
%% server PID. Used by `dev_mysticeti` and tests to locate or spawn the
%% per-process consensus server.
%%
%% Reference: "Mysticeti: Reaching the Limits of Latency with Uncertified DAGs"
%% (Babel et al., arXiv:2310.14821).
-module(dev_mysticeti_registry).
-export([start/0, find/1, find/2, find/3, get_processes/0]).
-include("include/hb.hrl").

%% @doc Ensure the global name registry is started.
start() ->
    hb_name:start(),
    ok.

%% @doc Find a process associated with the given process ID in the local registry.
find(ProcID) -> find(ProcID, false).

%% @doc Find a process associated with the given process ID in the local registry.
%% If not found and ProcMsgOrFalse is provided, spawn a new server.
find(ProcID, ProcMsgOrFalse) ->
    find(ProcID, ProcMsgOrFalse, #{ priv_wallet => hb:wallet() }).

%% @doc Same as find/2 but with additional options passed when spawning a new
%% server (if needed).
find(ProcID, ProcMsgOrFalse, Opts) ->
    case hb_name:lookup({<<"mysticeti@1.0">>, ProcID}) of
        undefined -> maybe_new_proc(ProcID, ProcMsgOrFalse, Opts);
        Pid -> Pid
    end.

%% @doc Return a list of all currently registered ProcIDs.
get_processes() ->
    [ ProcID || {{<<"mysticeti@1.0">>, ProcID}, _} <- hb_name:all() ].

maybe_new_proc(_ProcID, false, _Opts) -> not_found;
maybe_new_proc(ProcID, ProcMsg, Opts) ->
    dev_mysticeti_server:start(ProcID, ProcMsg, Opts).
