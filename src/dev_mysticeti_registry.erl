%% @doc Registry for Mysticeti-C per-process servers.
%%
%% This module maintains a local mapping from a process ID to the Erlang PID
%% running the consensus server. It ensures there is at most one server per
%% process on a node, and provides the spawn-on-demand entry point used by the
%% HTTP device layer.
%%
%% Paper context: the consensus algorithm itself is implemented in
%% `dev_mysticeti_server` and follows mysticeti-paper/algorithms/consensus_utils.tex
%% (Alg. 1) and mysticeti-paper/algorithms/universal_committer.tex (Alg. 3).
-module(dev_mysticeti_registry).
-export([start/0, find/1, find/2, find/3, get_processes/0, registry_key/2]).
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
    case hb_name:lookup(registry_key(ProcID, Opts)) of
        undefined -> maybe_new_proc(ProcID, ProcMsgOrFalse, Opts);
        Pid -> Pid
    end.

%% @doc Return a list of all currently registered ProcIDs.
get_processes() ->
    [
        ProcID
    ||
        {Key, _} <- hb_name:all(),
        {true, ProcID} <-
            [
                case Key of
                    {<<"mysticeti@1.0">>, Pid} -> {true, Pid};
                    {<<"mysticeti@1.0">>, _Namespace, Pid} -> {true, Pid};
                    _ -> false
                end
            ]
    ].

registry_key(ProcID, Opts) ->
    case hb_opts:get(mysticeti_registry_namespace, undefined, Opts) of
        undefined -> {<<"mysticeti@1.0">>, ProcID};
        Namespace -> {<<"mysticeti@1.0">>, Namespace, ProcID}
    end.

maybe_new_proc(_ProcID, false, _Opts) -> not_found;
maybe_new_proc(ProcID, ProcMsg, Opts) when is_binary(ProcMsg) ->
    case hb_cache:read(ProcMsg, Opts) of
        {ok, Loaded} -> dev_mysticeti_server:start(ProcID, Loaded, Opts);
        _ -> not_found
    end;
maybe_new_proc(ProcID, ProcMsg, Opts) ->
    dev_mysticeti_server:start(ProcID, ProcMsg, Opts).
