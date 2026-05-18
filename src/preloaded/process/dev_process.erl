%%% @doc This module contains the device implementation of AO processes
%%% in AO-Core. The core functionality of the module is in 'routing' requests
%%% for different functionality (scheduling, computing, and pushing messages)
%%% to the appropriate device. This is achieved by swapping out the device 
%%% of the process message with the necessary component in order to run the 
%%% execution, then swapping it back before returning. Computation is supported
%%% as a stack of devices, customizable by the user, while the scheduling
%%% device is (by default) a single device.
%%% 
%%% This allows the devices to share state as needed. Additionally, after each
%%% computation step the device caches the result as AO-Core result edges, such
%%% that the process message's ID can act as an immutable reference to the
%%% process's growing list of interactions.
%%% 
%%% The external API of the device is as follows:
%%% <pre>
%%% GET /ID/Schedule:                Returns the messages in the schedule
%%% POST /ID/Schedule:               Adds a message to the schedule
%%% 
%%% GET /ID/Compute/[IDorSlotNum]:   Returns the state of the process after 
%%%                                  applying a message
%%% GET /ID/Now:                     Returns the `/Results' key of the latest 
%%%                                  computed message
%%% </pre>
%%% 
%%% An example process definition will look like this:
%%% <pre>
%%%     Device: Process/1.0
%%%     Scheduler-Device: Scheduler/1.0
%%%     Execution-Device: Stack/1.0
%%%     Execution-Stack: "Scheduler/1.0", "Cron/1.0", "WASM/1.0"
%%%     Cron-Frequency: 10-Minutes
%%%     WASM-Image: WASMImageID
%%% </pre>
%%%
%%% Runtime options:
%%%     Process-Snapshot-Slots: The number of slots between full restorable
%%%                             state snapshots.
%%%     Process-Snapshot-Time:  The number of seconds between full restorable
%%%                             state snapshots.
-module(dev_process).
-device_libraries([lib_process]).
%%% Public API
-export([info/1, as/3, compute/3, schedule/3, slot/3, latest/3, now/3, push/3, snapshot/3]).
-export([target_slot/2]).
-export([default_device/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% The frequency at which the process state should be cached. Can be overridden
%% with the `process_snapshot_slots' or `process_snapshot_time' options.
-if(TEST == true).
-define(DEFAULT_SNAPSHOT_SLOTS, 1).
-define(DEFAULT_SNAPSHOT_TIME, undefined).
-else.
-define(DEFAULT_SNAPSHOT_SLOTS, undefined).
-define(DEFAULT_SNAPSHOT_TIME, 60).
-endif.

%% @doc When the info key is called, we should return the process exports.
info(_Base) ->
    #{
        worker => fun dev_process_worker:server/3,
        grouper => fun dev_process_worker:group/3,
        await => fun dev_process_worker:await/5,
        exports =>
            [
                <<"info">>,
                <<"as">>,
                <<"compute">>,
                <<"latest">>,
                <<"now">>,
                <<"schedule">>,
                <<"slot">>,
                <<"snapshot">>,
                <<"push">>
            ]
    }.

%% @doc Return the process state with the device swapped out for the device
%% of the given key.
-spec as(#{ 'input-prefix' => binary(), _ => _ },
    #{ as => binary(), 'as-device' => binary(), _ => _ },
    #{ _ => _ }) -> {ok, #{ device := binary(), _ => _ }}.
as(RawBase, Req, Opts) ->
    {ok, Base} = ensure_loaded(RawBase, Req, Opts),
    Key = maps:get(<<"as">>, Req, maps:get(<<"as-device">>, Req, <<"execution">>)),
    {ok,
        hb_util:deep_merge(
            lib_process:ensure_process_key(Base, Opts),
            #{
                <<"device">> =>
                    hb_maps:get(
                        << Key/binary, "-device">>,
                        Base,
                        default_device(Base, Key, Opts),
                        Opts
                    ),
                % Configure input prefix for proper message routing within the
                % device
                <<"input-prefix">> =>
                    case maps:get(<<"input-prefix">>, Base, not_found) of
                        not_found -> <<"process">>;
                        Prefix -> Prefix
                    end,
                % Configure output prefixes for result organization
                <<"output-prefixes">> =>
                    hb_maps:get(
                        <<Key/binary, "-output-prefixes">>,
                        Base,
                        undefined, % Undefined in set will be ignored.
                        Opts
                    )
            },
            Opts
        )
    }.

%% @doc Returns the default device for a given piece of functionality. Expects
%% the `process/variant' key to be set in the message. The `execution-device'
%% _must_ be set in all processes aside those marked with `ao.TN.1' variant.
%% This is in order to ensure that post-mainnet processes do not default to
%% using infrastructure that should not be present on nodes in the future.
-spec default_device(#{ 'process/variant' => binary(), _ => _ }, binary(), #{ _ => _ }) ->
    binary().
default_device(Base, Key, Opts) ->
    lib_process:default_device(Base, Key, Opts).

%% @doc Wraps functions in the Scheduler device.
-spec schedule(#{ scheduler => _, _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, _} | {error, _}.
schedule(Base, Req, Opts) ->
    lib_process:run_as(<<"scheduler">>, Base, Req, Opts).

-spec slot(#{ scheduler => _, _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, _} | {error, _}.
slot(Base, Req, Opts) ->
    ?event({slot_called, {base, Base}, {req, Req}}),
    lib_process:run_as(<<"scheduler">>, Base, Req, Opts).

next(Base, _Req, Opts) ->
    lib_process:run_as(<<"scheduler">>, Base, next, Opts).

-spec snapshot(#{ execution => _, _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }}.
snapshot(RawBase, _Req, Opts) ->
    Base = lib_process:ensure_process_key(RawBase, Opts),
    {ok, SnapshotMsg} =
        lib_process:run_as(
            <<"execution">>,
            Base,
            #{ <<"path">> => <<"snapshot">>, <<"mode">> => <<"Map">> },
            Opts#{
                <<"cache-control">> => [<<"no-cache">>, <<"no-store">>]
            }
        ),
    {ok, SnapshotMsg}.

%% @doc Before computation begins, a boot phase is required. This phase
%% allows devices on the execution stack to initialize themselves. We set the
%% `Initialized' key to `True' to indicate that the process has been
%% initialized.
init(Base, Req, Opts) ->
    ?event({init_called, {base, Base}, {req, Req}}),
    {ok, Initialized} =
        lib_process:run_as(
            <<"execution">>,
            Base,
            #{ <<"path">> => <<"init">> },
            Opts
        ),
    {
        ok,
        hb_ao:set(
            Initialized,
            #{
                <<"initialized">> => <<"true">>,
                <<"at-slot">> => -1
            },
            Opts
        )
    }.

%% @doc Compute the result of an assignment applied to the process state.
%% This function serves as the main entry point for compute operations and routes
%% between two distinct execution paths:
%% 
%% - GET method: Normal compute execution that applies messages to process state
%%   and advances the state permanently. Used for regular process execution.
%% 
%% - POST method: Dryrun compute execution that simulates message processing
%%   without permanently modifying process state. Used for testing message 
%%   handlers and previewing results. The POST method is the key entry point
%%   for the dryrun functionality that allows external clients to test
%%   message processing without side effects.
-spec compute(
    #{ initialized => binary(), 'at-slot' => integer(), _ => _ },
    #{
        compute => integer(),
        slot => integer(),
        init => binary(),
        push => _,
        'result-depth' => _,
        async => _,
        'max-depth' => _
    },
    #{ _ => _ }
) -> {ok, #{ _ => _ }} | {error, _} | {failure, _}.
compute(Base, Req, Opts) ->
    ProcBase = lib_process:ensure_process_key(Base, Opts),
    ProcID = lib_process:process_id(ProcBase, #{}, Opts),
    TargetSlot = target_slot(Req, Opts),
    case TargetSlot of
        not_found ->
            % The slot is not set, so we need to serve the latest known state
            % unless the `init' key is set to a value aside from `now'.
            % We do this by setting the `process_now_from_cache' option to `true'.
            case maps:get(<<"init">>, Req, <<"now">>) of
                <<"now">> ->
                    now(Base, Req, Opts#{ process_now_from_cache => true });
                _ ->
                    {error, not_found}
            end;
        Slot ->
            case read_process_slot(ProcID, Slot, Opts) of
                {ok, Result} ->
                    % The result is already cached, so we can return it.
                    ?event(
                        {compute_result_cached,
                            {proc_id, ProcID},
                            {slot, Slot},
                            {result, Result}
                        }
                    ),
                    {ok, without_snapshot(Result, Opts)};
                {error, not_found} ->
                    {ok, Loaded} = ensure_loaded(ProcBase, Req, Opts),
                    ?event(compute,
                        {computing, {process_id, ProcID},
                        {to_slot, Slot}},
                        Opts
                    ),
                    compute_to_slot(
                        ProcID,
                        Loaded,
                        Req,
                        Slot,
                        Opts
                    )
            end
    end.

%% @doc Return the slot requested by a `compute' request, or `not_found'.
target_slot(Req, _Opts) ->
    case maps:get(<<"compute">>, Req, not_found) of
        not_found -> maps:get(<<"slot">>, Req, not_found);
        ComputeSlot -> ComputeSlot
    end.

%% @doc Continually get and apply the next assignment from the scheduler until
%% we reach the target slot that the user has requested.
compute_to_slot(ProcID, Base, Req, TargetSlot, Opts) ->
    case hb_ao:get(<<"at-slot">>, Base, Opts#{ <<"hashpath">> => ignore }) of
        CurrentSlot when CurrentSlot == TargetSlot ->
            % We reached the target height so we force a snapshot and return.
            ?event(compute_short,
                {reached_target_slot_returning_state,
                    {proc_id, ProcID},
                    {slot, TargetSlot}
                },
                Opts
            ),
            store_result(true, ProcID, TargetSlot, Base, Req, Opts),
            {ok, without_snapshot(lib_process:as_process(Base, Opts), Opts)};
        CurrentSlot when CurrentSlot < TargetSlot ->
            % Compute the next state transition.
            NextSlot = CurrentSlot + 1,
            % Get the next input message from the scheduler device.
            case next(Base, Req, Opts) of
                {error, Res} ->
                    % If the scheduler device cannot provide a next message,
                    % we return its error details, along with the current slot.
                    ?event(compute_short,
                        {error_getting_assignment,
                            {proc_id, ProcID},
                            {attempted_slot, NextSlot},
                            {target_slot, TargetSlot},
                            {error, Res}
                        }
                    ),
                    {error,
                        Res#{
                            <<"phase">> => <<"get-schedule">>,
                            <<"attempted-slot">> => NextSlot,
                            <<"process-id">> => ProcID
                        }
                    };
                {ok, #{ <<"body">> := SlotMsg, <<"state">> := State }} ->
                    % Compute the next single state transition.
                    case compute_slot(ProcID, State, SlotMsg, Req, TargetSlot, Opts) of
                        {ok, NewState} ->
                            % Continue computing to the target slot.
                            compute_to_slot(
                                ProcID,
                                NewState,
                                Req,
                                TargetSlot,
                                Opts
                            );
                        {error, Error} ->
                            % Forward error details back to the caller.
                            {error, Error}
                    end
            end;
        CurrentSlot when CurrentSlot > TargetSlot ->
            % The cache should already have the result, so we should never end up
            % here. Depending on the type of process, 'rewinding' may require
            % re-computing from a significantly earlier checkpoint, so for now
            % we throw an error.
            ?event(
                compute,
                {error_already_calculated_slot,
                    {target, TargetSlot},
                    {current, CurrentSlot}
                },
                Opts
            ),
            throw(
                {error,
                    {already_calculated_slot,
                        {target, TargetSlot},
                        {current, CurrentSlot}
                    }
                }
            )
    end.

%% @doc Compute a single slot for a process, given an initialized state.
compute_slot(ProcID, State, RawInputMsg, InitReq, TargetSlot, Opts) ->
    {PrepTimeMicroSecs, {ok, Slot, PreparedState, Req}} =
        timer:tc(
            fun() ->
                prepare_next_slot(ProcID, State, RawInputMsg, Opts)
            end
        ),
    ?event(
        compute,
        {prepared_slot,
            {proc_id, ProcID},
            {slot, Slot},
            {prep_time_microsecs, PrepTimeMicroSecs}
        },
        Opts
    ),
    {RuntimeMicroSecs, Res} =
        timer:tc(
            fun() ->
                lib_process:run_as(<<"execution">>, PreparedState, Req, Opts)
            end
        ),
    ?event(
        compute,
        {computed_slot,
            {proc_id, ProcID},
            {slot, Slot},
            {runtime_microsecs, RuntimeMicroSecs}
        },
        Opts
    ),
    case Res of
        {ok, NewProcStateMsg} ->
            % We have now transformed slot n -> n + 1. Increment the current slot.
            NewProcStateMsgWithSlot =
                hb_ao:set(
                    NewProcStateMsg,
                    #{ <<"device">> => <<"process@1.0">>, <<"at-slot">> => Slot },
                    Opts
                ),
            {StoreTimeMicroSecs, ProcStateWithSnapshot} =
                timer:tc(
                    fun() ->
                        store_result(
                            false,
                            ProcID,
                            Slot,
                            NewProcStateMsgWithSlot,
                            InitReq,
                            Opts
                        )
                    end
                ),
            ?event(compute_short,
                {computed_slot,
                    {proc_id, ProcID},
                    {slot, Slot},
                    {target_slot, TargetSlot},
                    {prep_ms, PrepTimeMicroSecs div 1000},
                    {execution_ms, RuntimeMicroSecs div 1000},
                    {store_ms, StoreTimeMicroSecs div 1000},
                    {computed_slot_size, erlang:external_size(NewProcStateMsgWithSlot)},
                    {action,
                        maps:get(
                            <<"action">>,
                            maps:get(<<"body">>, Req, #{}),
                            no_action_set
                        )
                    }
                }
            ),
            % Notify waiters only after the slot is readable from the process
            % cache. Waiters may immediately re-enter via `/compute' or `/push',
            % and those paths treat the cache as the completion boundary.
            dev_process_worker:notify_compute(
                ProcID,
                Slot,
                {ok, ProcStateWithSnapshot},
                Opts
            ),
            % Optionally fire an async `/push' for the slot we just cached.
            % Only fresh computes reach this branch; cache hits in `compute/3'
            % short-circuit before we get here, so each slot is push-triggered
            % at most once per node lifetime regardless of how many times the
            % caller polls `/now' or `/compute'.
            maybe_trigger_push(State, Slot, InitReq, Opts),
            {ok, ProcStateWithSnapshot};
        {error, Error} ->
            % An error occurred while computing the slot. Return the details.
            ErrMsg =
                if is_map(Error) -> Error;
                true -> #{ <<"error">> => Error }
                end,
            ?event(compute_short,
                {error_computing_slot,
                    {proc_id, ProcID},
                    {attempted_slot, Slot},
                    {target_slot, TargetSlot},
                    {prep_ms, PrepTimeMicroSecs div 1000},
                    {execution_ms, RuntimeMicroSecs div 1000},
                    {error, ErrMsg}
                }
            ),
            {error,
                ErrMsg#{
                    <<"phase">> => <<"compute">>,
                    <<"attempted-slot">> => Slot
                }
            }
    end.

%% @doc Prepare the process state message for computing the next slot.
prepare_next_slot(ProcID, State, RawReq, Opts) ->
    Slot = hb_util:int(maps:get(<<"slot">>, RawReq)),
    ?event(compute, {next_slot, Slot}),
    % If the input message does not have a path, set it to `compute'.
    Req =
        case hb_path:from_message(request, RawReq, Opts) of
            undefined -> RawReq#{ <<"path">> => <<"compute">> };
            _ -> RawReq
        end,
    ?event(compute, {input_msg, Req}),
    ?event(compute, {executing, {proc_id, ProcID}, {slot, Slot}}, Opts),
    % Unset the previous results.
    PreparedState = hb_ao:set(State, #{ <<"results">> => unset }, Opts),
    {ok, Slot, PreparedState, Req}.

%% @doc Fire a `~push@1.0/push' for the slot we just computed, iff the
%% originating request carries a truthy `push' key. The push is invoked
%% from a freshly-spawned process so a slow downstream chain cannot stall
%% the compute path that produced this slot.
%%
%% `push' values:
%%   `true' / `<<"true">>'  - push, no `max-depth' set (unbounded recursion).
%%   non-negative integer N - push with `max-depth = N', so the fan-out
%%                            unwinds at most N levels deep. See `dev_push'
%%                            for `max-depth = 0' semantics: each outbox
%%                            entry is still scheduled on its target, but
%%                            the recursive `/push' is skipped.
%%   anything else (or absent) - silent no-op.
maybe_trigger_push(Process, Slot, Req, Opts) ->
    case hb_maps:get(<<"push">>, Req, undefined, Opts) of
        true        -> dispatch_push(Process, Slot, undefined, Req, Opts);
        <<"true">>  -> dispatch_push(Process, Slot, undefined, Req, Opts);
        N when is_integer(N), N >= 0 ->
            dispatch_push(Process, Slot, N, Req, Opts);
        Bin when is_binary(Bin) ->
            try hb_util:int(Bin) of
                N when is_integer(N), N >= 0 ->
                    dispatch_push(Process, Slot, N, Req, Opts);
                _ -> ok
            catch _:_ -> ok
            end;
        _ -> ok
    end.

%% @doc Build the inner `~push@1.0/push' request for `Slot' and invoke it from
%% a freshly-spawned process. Inherits the
%% originating request's payload keys (e.g. `result-depth', `async')
%% so the caller's preference flows through, replaces `path'/`slot',
%% and -- when bounded -- sets `max-depth'. The default sync mode
%% propagates back-pressure to the compute path: a slow downstream
%% chain throttles further hook fires rather than queueing unbounded
%% spawns under load.
dispatch_push(Process, Slot, MaxDepth, Req, Opts) ->
    BaseReq =
        (hb_maps:without([<<"push">>, <<"path">>, <<"slot">>], Req, Opts))#{
            <<"path">> => <<"push">>,
            <<"slot">> => Slot
        },
    PushReq =
        case MaxDepth of
            undefined -> BaseReq;
            N -> BaseReq#{ <<"max-depth">> => N }
        end,
    % Extract the canonical process spec from the live state so push ID
    % computation lands on the same cache key that `store_result' just
    %% wrote under -- passing the live state directly hashes to a different
    %% key and sends the downstream read into a re-compute loop.
    Spec = hb_maps:get(<<"process">>, Process, Process, Opts),
    ?event(push,
        {triggered_by_compute,
            {slot, Slot},
            {max_depth, MaxDepth}
        },
        Opts
    ),
    spawn(fun() -> hb_ao:raw(<<"push@1.0">>, Spec, PushReq, Opts) end),
    ok.

%% @doc Store the resulting state in the cache, potentially with the snapshot
%% key. The write is synchronous: callers may notify waiters or run push hooks
%% as soon as this returns, so the slot must already be cache-visible.
store_result(ForceSnapshot, ProcID, Slot, Res, Req, Opts) ->
    % Cache the `Snapshot' key as frequently as the node is configured to.
    ResMaybeWithSnapshot =
        case ForceSnapshot orelse should_snapshot(Slot, Res, Opts) of
            false -> Res;
            true ->
                ?event(
                    debug_compute,
                    {snapshotting, {proc_id, ProcID}, {slot, Slot}},
                    Opts
                ),
                {ok, Snapshot} = snapshot(Res, Req, Opts),
				?event(snapshot,
					{got_snapshot,
						{storing_as_slot, Slot},
						{snapshot, Snapshot}
					}
				),
                ?event(snapshot,
                    {snapshot_generated,
                        {proc_id, ProcID},
                        {slot, Slot},
                        {snapshot, Snapshot}
                    },
                    Opts
                ),
                WithSnapshot =
                    hb_ao:set(
                        Res,
                        <<"snapshot">>,
                        Snapshot,
                        Opts
                    ),
				WithLastSnapshot =
                    hb_private:set(
                        WithSnapshot,
                        <<"last-snapshot">>,
                        os:system_time(second),
                        Opts
                    ),
                ?event(debug_interval,
                    {snapshot_with_last_snapshot,
                        {proc_id, ProcID},
                        {slot, Slot},
                        {snapshot, WithLastSnapshot}
                    }
                ),
                WithLastSnapshot
        end,
    PublicResult = without_snapshot(ResMaybeWithSnapshot, Opts),
    ?event(compute, {caching_result, {proc_id, ProcID}, {slot, Slot}}, Opts),
    CacheEdges = [{ProcID, slot_req(Slot)}, {ProcID, latest_req()}],
    PublicCacheResult = hb_private:reset(PublicResult),
    {ok, _} = hb_cache:write_result(CacheEdges, PublicCacheResult, Opts),
    {ok, _} =
        write_restore_edges(
            ProcID,
            Slot,
            ResMaybeWithSnapshot,
            maps:is_key(<<"snapshot">>, ResMaybeWithSnapshot),
            Opts
        ),
    ?event(compute, {caching_completed, {proc_id, ProcID}, {slot, Slot}}, Opts),
    PublicResult.

read_process_slot(ProcID, Slot, Opts) ->
    read_process_edge(ProcID, slot_req(Slot), Opts).

read_process_edge(ProcID, Req, Opts) ->
    case hb_cache:read_resolved(ProcID, Req, Opts) of
        {hit, {ok, Msg}} -> {ok, Msg};
        {hit, Other} -> Other;
        miss -> {error, not_found}
    end.

slot_req(Slot) ->
    #{ <<"path">> => <<"compute">>, <<"slot">> => hb_util:int(Slot) }.

latest_req() ->
    #{ <<"path">> => <<"latest">> }.

restore_req() ->
    #{ <<"path">> => <<"restore">> }.

restore_req(Slot) ->
    #{ <<"path">> => <<"restore">>, <<"slot">> => hb_util:int(Slot) }.

write_restore_edges(ProcID, Slot, Checkpoint, true, Opts) ->
    hb_cache:write_result(
        [{ProcID, restore_req(Slot)}, {ProcID, restore_req()}],
        hb_private:reset(Checkpoint),
        Opts
    );
write_restore_edges(_ProcID, _Slot, _Res, false, _Opts) ->
    {ok, skipped}.

read_restore_checkpoint(ProcID, undefined, Opts) ->
    read_restore_checkpoint(ProcID, restore_req(), Opts);
read_restore_checkpoint(ProcID, Req, Opts) when is_map(Req) ->
    case read_process_edge(ProcID, Req, hb_store:scope(Opts, local)) of
        {ok, Msg = #{ <<"at-slot">> := Slot }} -> {ok, hb_util:int(Slot), Msg};
        {ok, _} -> {error, not_found};
        Other -> Other
    end;
read_restore_checkpoint(ProcID, TargetSlot, Opts) ->
    TargetSlotInt = hb_util:int(TargetSlot),
    case read_restore_checkpoint(ProcID, restore_req(TargetSlotInt), Opts) of
        {error, not_found} ->
            case read_restore_checkpoint(ProcID, restore_req(), Opts) of
                {ok, Slot, Msg} when Slot =< TargetSlotInt -> {ok, Slot, Msg};
                _ -> {error, not_found}
            end;
        Other ->
            Other
    end.

%% @doc Should we snapshot a new full state result? First, we check if the 
%% `process_snapshot_time' option is set. If it is, we check if the elapsed time
%% since the last snapshot is greater than the value. We also check the
%% `process_snapshot_slots' option. If it is set, we check if the slot is
%% a multiple of the interval. If either are true, we must snapshot.
should_snapshot(Slot, Res, Opts) ->
    should_snapshot_slots(Slot, Opts) orelse should_snapshot_time(Res, Opts).

%% @doc Calculate if we should snapshot based on the number of slots.
should_snapshot_slots(Slot, Opts) ->
    case hb_opts:get(process_snapshot_slots, ?DEFAULT_SNAPSHOT_SLOTS, Opts) of
        Undef when (Undef == undefined) or (Undef == <<"false">>) ->
            false;
        RawSnapshotSlots ->
            SnapshotSlots = hb_util:int(RawSnapshotSlots),
            Slot rem SnapshotSlots == 0
    end.

%% @doc Calculate if we should snapshot based on the elapsed time since the last
%% snapshot.
should_snapshot_time(Res, Opts) ->
    case hb_opts:get(process_snapshot_time, ?DEFAULT_SNAPSHOT_TIME, Opts) of
        Undef when (Undef == undefined) or (Undef == <<"false">>) ->
            false;
        RawSecs ->
            Secs = hb_util:int(RawSecs),
            case hb_private:get(<<"last-snapshot">>, Res, undefined, Opts) of
                undefined ->
                    ?event(
                        debug_interval,
                        {no_last_snapshot,
                            {interval, Secs},
                            {msg, Res}
                        }
                    ),
                    true;
                OldTimestamp ->
                    ?event(
                        debug_interval,
                        {calculating,
                            {secs, Secs},
                            {timestamp, OldTimestamp},
                            {now, os:system_time(second)}
                        }
                    ),
                    os:system_time(second) > OldTimestamp + hb_util:int(Secs)
            end
    end.

%% @doc Returns the known state of the process at either the current slot, or
%% the latest slot in the cache depending on the `process_now_from_cache' option.
-spec latest(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {failure, _} | {error, _}.
latest(Base, Req, Opts) ->
    now(Base, Req, Opts#{ process_now_from_cache => always }).

-spec now(#{ _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ _ => _ }} | {failure, _} | {error, _}.
now(RawBase, Req, Opts) ->
    Base = lib_process:ensure_process_key(RawBase, Opts),
    ProcessID = lib_process:process_id(Base, #{}, Opts),
    case hb_opts:get(process_now_from_cache, false, Opts) of
        false ->
            {ok, CurrentSlot} =
                hb_ao:resolve(
                    Base,
                    #{ <<"path">> => <<"slot/current">> },
                    Opts
                ),
            ?event({now_called, {process, ProcessID}, {slot, CurrentSlot}}),
            hb_ao:resolve(
                Base,
                #{ <<"path">> => <<"compute">>, <<"slot">> => CurrentSlot },
                Opts
            );
        CacheParam ->
            % We are serving the latest known state from the cache, rather
            % than computing it.
            LatestKnown = read_process_edge(ProcessID, latest_req(), Opts),
            case LatestKnown of
                {ok, RawLatestMsg = #{ <<"at-slot">> := LatestSlot }} ->
                    LatestMsg = compute_response(RawLatestMsg, Req, Opts),
                    ?event(compute_cache,
                        {serving_latest_cached_state,
                            {proc_id, ProcessID},
                            {slot, LatestSlot}
                        },
                        Opts
                    ),
                    dev_process_worker:notify_compute(
                        ProcessID,
                        LatestSlot,
                        {ok, LatestMsg},
                        Opts
                    ),
                    {ok, LatestMsg};
                _ ->
                    if CacheParam =/= always ->
                        % The node is configured to use the cache if possible,
                        % but forcing computation is also admissible. Subsequently,
                        % as no other option is available, we compute the state.
                        now(Base, Req, Opts#{ process_now_from_cache => false });
                    true ->
                        % The node is configured to only serve the latest known
                        % state from the cache, so we return the latest slot.
                        {failure, <<"No cached state available.">>}
                    end
            end
    end.

%% @doc Recursively push messages to the scheduler until we find a message
%% that does not lead to any further messages being scheduled.
-spec push(#{ push => _, _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, _} | {error, _}.
push(Base, Req, Opts) ->
    lib_process:run_as(
        <<"push">>,
        lib_process:ensure_process_key(Base, Opts),
        Req,
        Opts
    ).

%% @doc Ensure that the process message we have in memory is live and
%% up-to-date.
ensure_loaded(Base, Req, Opts) ->
    % Get the nonce we are currently on and the inbound nonce.
    TargetSlot = hb_ao:get(<<"slot">>, Req, undefined, Opts),
    ProcID = lib_process:process_id(Base, #{}, Opts),
    ?event({ensure_loaded, {base, Base}, {req, Req}}),
    case maps:get(<<"initialized">>, Base, undefined) of
        <<"true">> ->
            ?event(already_initialized),
            {ok, Base};
        _ ->
            ?event(not_initialized),
            % Try to load the latest complete state from disk.
            LoadRes = read_restore_checkpoint(ProcID, TargetSlot, Opts),
            ?event(compute,
                {snapshot_load_res,
                    {proc_id, ProcID},
                    {res, LoadRes},
                    {target, TargetSlot}
                },
                Opts
            ),
            case LoadRes of
                {ok, MaybeLoadedSlot, SnapshotMsg} ->
                    % Restore the devices in the executor stack with the
                    % loaded state. This allows the devices to load any
                    % necessary 'shadow' state (state not represented in
                    % the public component of a message) into memory.
                    % Do not update the hashpath while we do this, and remove
                    % the snapshot key after we have normalized the message.
                    Process = 
                        hb_maps:get(
                            <<"process">>,
                            SnapshotMsg,
                            undefined,
                            Opts
                        ),
                    #{ <<"commitments">> := HmacCommits} =
                        hb_message:with_commitments(
                            #{ <<"type">> => <<"hmac-sha256">>},
                            Process,
                            Opts
                        ),
                    #{ <<"commitments">> := SignCommits } =
                        hb_message:with_commitments(ProcID, Process, Opts),
                    UpdateProcess =
                        Process#{
                            <<"commitments">> =>
                                maps:merge(HmacCommits, SignCommits)
                        },
                    SnapshotReq =
                        SnapshotMsg#{
                            <<"process">> => UpdateProcess,
                            <<"initialized">> => <<"true">>
                        },
                    ?event(compute,
                        {found_state_checkpoint,
                            {proc_id, ProcID},
                            {slot, MaybeLoadedSlot}
                        },
                        Opts
                    ),
                    {ok, Normalized} =
                        lib_process:run_as(
                            <<"execution">>,
                            SnapshotReq,
                            normalize,
                            Opts#{ <<"hashpath">> => ignore }
                        ),
                    NormalizedWithoutSnapshot =
                        without_snapshot(Normalized, Opts),
                    ?event(snapshot,
                        {loaded_state_checkpoint_result,
                            {proc_id, ProcID},
                            {slot, MaybeLoadedSlot},
                            {after_normalization, NormalizedWithoutSnapshot}
                        }
                    ),
                    {ok, NormalizedWithoutSnapshot};
                {error, not_found} ->
                    % If we do not have a checkpoint, initialize the
                    % process from scratch.
                    ?event(
                        {no_checkpoint_found,
                            {process, ProcID},
                            {slot, TargetSlot}
                        }
                    ),
                    init(Base, Req, Opts)
            end
    end.

%% @doc Remove the `snapshot' key from a message and return it.
without_snapshot(Msg, Opts) ->
    hb_ao:set(Msg, <<"snapshot">>, unset, Opts).
