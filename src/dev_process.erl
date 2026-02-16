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
%%% computation step the device caches the result at a path relative to the
%%% process definition itself, such that the process message's ID can act as an
%%% immutable reference to the process's growing list of interactions. See 
%%% `dev_process_cache' for details.
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
%%%     Execution-Stack: "Scheduler/1.0", "Cron/1.0", "WASM/1.0", "PoDA/1.0"
%%%     Cron-Frequency: 10-Minutes
%%%     WASM-Image: WASMImageID
%%%     PoDA:
%%%         Device: PoDA/1.0
%%%         Authority: A
%%%         Authority: B
%%%         Authority: C
%%%         Quorum: 2
%%% </pre>
%%%
%%% Runtime options:
%%%     Cache-Frequency: The number of assignments that will be computed 
%%%                      before the full (restorable) state should be cached.
%%%     Cache-Keys:      A list of the keys that should be cached for all 
%%%                      assignments, in addition to `/Results'.
-module(dev_process).
%%% Public API
-export([info/1, as/3, compute/3, schedule/3, slot/3, now/3, push/3, snapshot/3]).
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
                <<"now">>,
                <<"schedule">>,
                <<"slot">>,
                <<"snapshot">>,
                <<"push">>
            ]
    }.

%% @doc Return the process state with the device swapped out for the device
%% of the given key.
as(RawBase, Req, Opts) ->
    {ok, Base} = ensure_loaded(RawBase, Req, Opts),
    Key = 
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Req}, <<"as">>},
                {{as, <<"message@1.0">>, Req}, <<"as-device">>}
            ],
            <<"execution">>,
            Opts
        ),
    {ok,
        hb_util:deep_merge(
            dev_process_lib:ensure_process_key(Base, Opts),
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
                    case hb_maps:get(<<"input-prefix">>, Base, not_found, Opts) of
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
default_device(Base, Key, Opts) ->
    NormKey = hb_ao:normalize_key(Key),
    case {NormKey, hb_util:deep_get(<<"process/variant">>, Base, Opts)} of
        {<<"execution">>, <<"ao.TN.1">>} -> <<"genesis-wasm@1.0">>;
        _ -> default_device_index(NormKey)
    end.
default_device_index(<<"scheduler">>) -> <<"scheduler@1.0">>;
default_device_index(<<"execution">>) -> <<"genesis-wasm@1.0">>;
default_device_index(<<"push">>) -> <<"push@1.0">>.

%% @doc Wraps functions in the Scheduler device.
schedule(Base, Req, Opts) ->
    dev_process_lib:run_as(<<"scheduler">>, Base, Req, Opts).

slot(Base, Req, Opts) ->
    ?event({slot_called, {base, Base}, {req, Req}}),
    dev_process_lib:run_as(<<"scheduler">>, Base, Req, Opts).

next(Base, _Req, Opts) ->
    dev_process_lib:run_as(<<"scheduler">>, Base, next, Opts).

snapshot(RawBase, _Req, Opts) ->
    Base = dev_process_lib:ensure_process_key(RawBase, Opts),
    {ok, SnapshotMsg} =
        dev_process_lib:run_as(
            <<"execution">>,
            Base,
            #{ <<"path">> => <<"snapshot">>, <<"mode">> => <<"Map">> },
            Opts#{
                cache_control => [<<"no-cache">>, <<"no-store">>]
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
        dev_process_lib:run_as(
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
compute(Base, Req, Opts) ->
    ProcBase = dev_process_lib:ensure_process_key(Base, Opts),
    ProcID = dev_process_lib:process_id(ProcBase, #{}, Opts),
    TargetSlot =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Req}, <<"compute">>},
                {{as, <<"message@1.0">>, Req}, <<"slot">>}
            ],
            Opts
        ),
    case TargetSlot of
        not_found ->
            % The slot is not set, so we need to serve the latest known state
            % unless the `init' key is set to a value aside from `now'.
            % We do this by setting the `process_now_from_cache' option to `true'.
            case hb_maps:get(<<"init">>, Req, <<"now">>, Opts) of
                <<"now">> ->
                    now(Base, Req, Opts#{ process_now_from_cache => true });
                _ ->
                    {error, not_found}
            end;
        RawSlot ->
            Slot = hb_util:int(RawSlot),
            case dev_process_cache:read(ProcID, Slot, Opts) of
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
                not_found ->
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

%% @doc Continually get and apply the next assignment from the scheduler until
%% we reach the target slot that the user has requested.
compute_to_slot(ProcID, Base, Req, TargetSlot, Opts) ->
    case hb_ao:get(<<"at-slot">>, Base, Opts#{ hashpath => ignore }) of
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
            {ok, without_snapshot(dev_process_lib:as_process(Base, Opts), Opts)};
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
                dev_process_lib:run_as(<<"execution">>, PreparedState, Req, Opts)
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
            % Notify any waiters that the result for a slot is now available.
            dev_process_worker:notify_compute(
                ProcID,
                Slot,
                {ok, NewProcStateMsgWithSlot},
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
                        hb_ao:get(
                            <<"body/action">>,
                            Req,
                            no_action_set,
                            Opts#{ hashpath => ignore }
                        )
                    }
                }
            ),
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
    Slot = hb_util:int(hb_ao:get(<<"slot">>, RawReq, Opts)),
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

%% @doc Store the resulting state in the cache, potentially with the snapshot
%% key.
store_result(ForceSnapshot, ProcID, Slot, Res, Req, Opts) ->
    % Cache the `Snapshot' key as frequently as the node is configured to.
    ResMaybeWithSnapshot =
        case ForceSnapshot orelse should_snapshot(Slot, Res, Opts) of
            false -> Res;
            true ->
                ?event(
                    compute_debug,
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
    ?event(compute, {caching_result, {proc_id, ProcID}, {slot, Slot}}, Opts),
    Writer = 
        fun() ->
            dev_process_cache:write(ProcID, Slot, ResMaybeWithSnapshot, Opts)
        end,
    case hb_opts:get(process_async_cache, true, Opts) of
        true ->
            spawn(Writer),
            ?event(compute, {caching_delegated, {proc_id, ProcID}, {slot, Slot}}, Opts);
        false ->
            Writer(),
            ?event(compute, {caching_completed, {proc_id, ProcID}, {slot, Slot}}, Opts)
    end,
    hb_maps:without([<<"snapshot">>], ResMaybeWithSnapshot, Opts).

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
now(RawBase, Req, Opts) ->
    Base = dev_process_lib:ensure_process_key(RawBase, Opts),
    ProcessID = dev_process_lib:process_id(Base, #{}, Opts),
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
            LatestKnown = dev_process_cache:latest(ProcessID, [], Opts),
            case LatestKnown of
                {ok, LatestSlot, RawLatestMsg} ->
                    LatestMsg = without_snapshot(RawLatestMsg, Opts),
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
push(Base, Req, Opts) ->
    dev_process_lib:run_as(
        <<"push">>,
        dev_process_lib:ensure_process_key(Base, Opts),
        Req,
        Opts
    ).

%% @doc Ensure that the process message we have in memory is live and
%% up-to-date.
ensure_loaded(Base, Req, Opts) ->
    % Get the nonce we are currently on and the inbound nonce.
    TargetSlot = hb_ao:get(<<"slot">>, Req, undefined, Opts),
    ProcID = dev_process_lib:process_id(Base, #{}, Opts),
    ?event({ensure_loaded, {base, Base}, {req, Req}}),
    case hb_ao:get(<<"initialized">>, Base, Opts) of
        <<"true">> ->
            ?event(already_initialized),
            {ok, Base};
        _ ->
            ?event(not_initialized),
            % Try to load the latest complete state from disk.
            LoadRes =
                dev_process_cache:latest(
                    ProcID,
                    [<<"snapshot+link">>],
                    TargetSlot,
                    Opts
                ),
            ?event(compute,
                {snapshot_load_res,
                    {proc_id, ProcID},
                    {res, LoadRes},
                    {target, TargetSlot}
                },
                Opts
            ),
            case LoadRes of
                {ok, MaybeLoadedSlot, LoadedSnapshotMsg} ->
                    % Restore the devices in the executor stack with the
                    % loaded state. This allows the devices to load any
                    % necessary 'shadow' state (state not represented in
                    % the public component of a message) into memory.
                    % Do not update the hashpath while we do this, and remove
                    % the snapshot key after we have normalized the message.
                    Process = 
                        hb_maps:get(
                            <<"process">>,
                            LoadedSnapshotMsg,
                            undefined,
                            Opts
                        ),
                    #{ <<"commitments">> := HmacCommits} =
                        hb_message:with_commitments(
                            #{ <<"type">> => <<"hmac-sha256">>},
                            Process,
                            Opts),
                    #{ <<"commitments">> := SignCommits } =
                        hb_message:with_commitments(ProcID, Process, Opts),
                    UpdateProcess = hb_maps:put(
                        <<"commitments">>,
                        hb_maps:merge(HmacCommits, SignCommits),
                        Process,
                        Opts
                    ),
                    LoadedSnapshotReq =
                        LoadedSnapshotMsg#{
                            <<"process">> => UpdateProcess,
                            <<"initialized">> => <<"true">>
                        },
                    LoadedSlot = hb_cache:ensure_all_loaded(MaybeLoadedSlot, Opts),
                    ?event(compute,
                        {found_state_checkpoint,
                            {proc_id, ProcID},
                            {slot, LoadedSlot}
                        },
                        Opts
                    ),
                    {ok, Normalized} =
                        dev_process_lib:run_as(
                            <<"execution">>,
                            LoadedSnapshotReq,
                            normalize,
                            Opts#{ hashpath => ignore }
                        ),
                    NormalizedWithoutSnapshot = without_snapshot(Normalized, Opts),
                    ?event(snapshot,
                        {loaded_state_checkpoint_result,
                            {proc_id, ProcID},
                            {slot, LoadedSlot},
                            {after_normalization, NormalizedWithoutSnapshot}
                        }
                    ),
                    {ok, NormalizedWithoutSnapshot};
                not_found ->
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
