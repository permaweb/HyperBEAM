%%% @doc A cache for the `~arweave-scheduler@1.0' device: stores the
%%% synthetic assignments derived from Arweave L1 transactions, as well as
%%% the per-process synchronization state of the indexer.
-module(dev_arweave_scheduler_cache).
-export([write/2, read/3, list_processes/1]).
-export([read_state/2, write_state/3]).
-export([assignments_to_bundle/4]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% The pseudo-path prefix which the arweave-scheduler cache should use.
-define(CACHE_PREFIX, <<"~arweave-scheduler@1.0">>).

%% @doc Merge the scheduler store with the main store. Used before writing
%% to the cache.
opts(Opts) -> lib_scheduler:cache_opts(Opts).

%% @doc Write an assignment message into the cache. Assignments are
%% deterministic derivations of chain data, so writes are idempotent:
%% concurrent synchronizations of the same process converge on identical
%% messages at identical paths.
write(Assignment, Opts) ->
    lib_scheduler:write_assignment(?CACHE_PREFIX, Assignment, Opts).

%% @doc Get an assignment message from the cache.
read(ProcID, Slot, Opts) ->
    lib_scheduler:read_assignment(?CACHE_PREFIX, ProcID, Slot, Opts).

%% @doc List the processes that the device holds synchronization state for.
list_processes(RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    case hb_store:list(Store, <<?CACHE_PREFIX/binary, "/state">>, Opts) of
        {ok, Names} -> Names;
        _ -> []
    end.

%% @doc Read the persisted synchronization state for a process. Returns
%% `{ok, State}' with the `next-slot' to be assigned, the process's
%% `spawn-height', `synced-to' (the highest block whose messages have been
%% contiguously indexed and materialized), and the `mode' the process is
%% sequenced in, or propagates the store's `not_found'. The mode is pinned here
%% at first contact: it is read from the process message, which cannot change,
%% and a schedule half-derived in each mode would be undetectable.
read_state(ProcID, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    maybe
        {ok, NextSlot} ?=
            hb_store:read(Store, state_path(ProcID, <<"next-slot">>), Opts),
        {ok, SpawnHeight} ?=
            hb_store:read(Store, state_path(ProcID, <<"spawn-height">>), Opts),
        {ok, SyncedTo} ?=
            hb_store:read(Store, state_path(ProcID, <<"synced-to">>), Opts),
        {ok, Mode} ?=
            hb_store:read(Store, state_path(ProcID, <<"mode">>), Opts),
        {ok,
            #{
                <<"next-slot">> => hb_util:int(NextSlot),
                <<"spawn-height">> => hb_util:int(SpawnHeight),
                <<"synced-to">> => hb_util:int(SyncedTo),
                <<"mode">> => Mode
            }
        }
    end.

%% @doc Persist the synchronization state for a process.
write_state(ProcID, State, RawOpts) ->
    Opts = opts(RawOpts),
    Store = hb_opts:get(store, no_viable_store, Opts),
    #{
        <<"next-slot">> := NextSlot,
        <<"spawn-height">> := SpawnHeight,
        <<"synced-to">> := SyncedTo,
        <<"mode">> := Mode
    } = State,
    hb_store:write(
        Store,
        #{
            state_path(ProcID, <<"next-slot">>) => hb_util:bin(NextSlot),
            state_path(ProcID, <<"spawn-height">>) => hb_util:bin(SpawnHeight),
            state_path(ProcID, <<"synced-to">>) => hb_util:bin(SyncedTo),
            state_path(ProcID, <<"mode">>) => Mode
        },
        Opts
    ).

state_path(ProcID, Key) ->
    hb_path:to_binary(
        [
            ?CACHE_PREFIX,
            <<"state">>,
            hb_util:human_id(ProcID),
            Key
        ]
    ).

%% @doc Generate a `GET /schedule' response for a process. Mirrors
%% `dev_scheduler_formats:assignments_to_bundle/4' (which cannot be called
%% across device namespaces), less the bundle-level `timestamp',
%% `block-height' and `block-hash'. Those describe the current weave tip
%% rather than the blocks that sequenced these assignments, so they would be
%% misleading on a schedule that is a deterministic read of historical chain
%% data. Each assignment carries the position that sequences its mode.
assignments_to_bundle(ProcID, Assignments, More, RawOpts) ->
    Opts = lib_scheduler:format_opts(RawOpts),
    {ok, #{
        <<"type">> => <<"schedule">>,
        <<"process">> => hb_util:human_id(ProcID),
        <<"continues">> => hb_util:atom(More),
        <<"assignments">> =>
            hb_message:normalize_commitments(
                hb_maps:from_list(
                    [ {hb_ao:get(<<"slot">>, A, Opts), A} || A <- Assignments ]
                ),
                Opts
            )
    }}.
