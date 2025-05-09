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
-export([info/1, compute/3, schedule/3, slot/3, now/3, push/3, snapshot/3]).
-export([ensure_process_key/2]).
%%% Public utilities
-export([as_process/2, process_id/3]).
%%% Test helpers
-export([test_aos_process/0, test_aos_process/1, dev_test_process/0, test_wasm_process/1]).
-export([schedule_aos_call/2, schedule_aos_call/3, init/0]).
%%% Tests
-export([do_test_restore/0]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% The frequency at which the process state should be cached. Can be overridden
%% with the `cache_frequency' option.
-define(DEFAULT_CACHE_FREQ, 1).

%% @doc When the info key is called, we should return the process exports.
info(_Msg1) ->
    #{
        worker => fun dev_process_worker:server/3,
        grouper => fun dev_process_worker:group/3,
        await => fun dev_process_worker:await/5,
        excludes => [
            <<"test">>,
            <<"init">>,
            <<"ping_ping_script">>,
            <<"schedule_aos_call">>,
            <<"test_aos_process">>,
            <<"dev_test_process">>,
            <<"test_wasm_process">>
        ]
    }.

%% @doc Returns the default device for a given piece of functionality. Expects
%% the `process/variant' key to be set in the message. The `execution-device'
%% _must_ be set in all processes aside those marked with `ao.TN.1' variant.
%% This is in order to ensure that post-mainnet processes do not default to
%% using infrastructure that should not be present on nodes in the future.
default_device(Msg1, Key, Opts) ->
    NormKey = hb_ao:normalize_key(Key),
    case {NormKey, hb_ao:get(<<"process/variant">>, {as, dev_message, Msg1}, Opts)} of
        {<<"execution">>, <<"ao.TN.1">>} -> <<"genesis-wasm@1.0">>;
        _ -> default_device_index(NormKey)
    end.
default_device_index(<<"scheduler">>) -> <<"scheduler@1.0">>;
default_device_index(<<"execution">>) -> <<"genesis-wasm@1.0">>;
default_device_index(<<"push">>) -> <<"push@1.0">>.

%% @doc Wraps functions in the Scheduler device.
schedule(Msg1, Msg2, Opts) ->
    run_as(<<"scheduler">>, Msg1, Msg2, Opts).

slot(Msg1, Msg2, Opts) ->
    ?event({slot_called, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    run_as(<<"scheduler">>, Msg1, Msg2, Opts).

next(Msg1, _Msg2, Opts) ->
    run_as(<<"scheduler">>, Msg1, next, Opts).

snapshot(RawMsg1, _Msg2, Opts) ->
    Msg1 = ensure_process_key(RawMsg1, Opts),
    {ok, SnapshotMsg} = run_as(
        <<"Execution">>,
        Msg1,
        #{ <<"path">> => <<"snapshot">>, <<"mode">> => <<"Map">> },
        Opts#{
            cache_control => [<<"no-cache">>, <<"no-store">>],
            hashpath => ignore
        }
    ),
    ProcID = hb_message:id(Msg1, all),
    Slot = hb_ao:get(<<"at-slot">>, Msg1, Opts),
    {ok,
        hb_private:set(
            hb_ao:set(
                SnapshotMsg,
                #{ <<"cache-control">> => [<<"store">>] },
                Opts
            ),
            #{ <<"priv/additional-hashpaths">> =>
                    [
                        hb_path:to_binary([ProcID, <<"snapshot">>, Slot])
                    ]
            },
            Opts
        )
    }.

%% @doc Returns the process ID of the current process.
process_id(Msg1, Msg2, Opts) ->
    case hb_ao:get(<<"process">>, Msg1, Opts#{ hashpath => ignore }) of
        not_found ->
            process_id(ensure_process_key(Msg1, Opts), Msg2, Opts);
        Process ->
            hb_message:id(Process, all)
    end.

%% @doc Before computation begins, a boot phase is required. This phase
%% allows devices on the execution stack to initialize themselves. We set the
%% `Initialized' key to `True' to indicate that the process has been
%% initialized.
init(Msg1, _Msg2, Opts) ->
    ?event({init_called, {msg1, Msg1}, {opts, Opts}}),
    {ok, Initialized} =
        run_as(<<"execution">>, Msg1, #{ <<"path">> => init }, Opts),
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

%% @doc Compute the result of an assignment applied to the process state, if it 
%% is the next message.
compute(Msg1, Msg2, Opts) ->
    % If we do not have a live state, restore or initialize one.
    ProcBase = ensure_process_key(Msg1, Opts),
    ProcID = process_id(ProcBase, #{}, Opts),
    case hb_ao:get(<<"slot">>, {as, <<"message@1.0">>, Msg2}, Opts) of
        not_found ->
            % The slot is not set, so we need to serve the latest known state.
            % We do this by setting the `process_now_from_cache' option to `true'.
            now(Msg1, Msg2, Opts#{ process_now_from_cache => true });
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
                    {ok, Result};
                not_found ->
                    {ok, Loaded} = ensure_loaded(ProcBase, Msg2, Opts),
                    ?event(compute,
                        {computing, {process_id, ProcID},
                        {to_slot, Slot}},
                        Opts
                    ),
                    compute_to_slot(
                        ProcID,
                        Loaded,
                        Msg2,
                        Slot,
                        Opts
                    )
            end
    end.

%% @doc Continually get and apply the next assignment from the scheduler until
%% we reach the target slot that the user has requested.
compute_to_slot(ProcID, Msg1, Msg2, TargetSlot, Opts) ->
    CurrentSlot = hb_ao:get(<<"at-slot">>, Msg1, Opts#{ hashpath => ignore }),
    ?event(compute, {starting_compute, {current, CurrentSlot}, {target, TargetSlot}}),
    case CurrentSlot of
        CurrentSlot when CurrentSlot > TargetSlot ->
            % The cache should already have the result, so we should never end up
            % here. Depending on the type of process, 'rewinding' may require
            % re-computing from a significantly earlier checkpoint, so for now
            % we throw an error.
            throw(
                {error,
                    {already_calculated_slot,
                        {target, TargetSlot},
                        {current, CurrentSlot}
                    }
                }
            );
        CurrentSlot when CurrentSlot == TargetSlot ->
            % We reached the target height so we return.
            ?event(compute, {reached_target_slot_returning_state, TargetSlot}),
            {ok, as_process(Msg1, Opts)};
        CurrentSlot ->
            % Compute the next state transition.
            NextSlot = CurrentSlot + 1,
            % Get the next input message from the scheduler device.
            case next(Msg1, Msg2, Opts) of
                {error, Res} ->
                    % If the scheduler device cannot provide a next message,
                    % we return its error details, along with the current slot.
                    {error, Res#{
                        <<"phase">> => <<"get-schedule">>,
                        <<"attempted-slot">> => NextSlot
                    }};
                {ok, #{ <<"body">> := SlotMsg, <<"state">> := State }} ->
                    % Compute the next single state transition.
                    case compute_slot(ProcID, State, SlotMsg, Msg2, Opts) of
                        {ok, NewState} ->
                            % Continue computing to the target slot.
                            compute_to_slot(
                                ProcID,
                                NewState,
                                Msg2,
                                TargetSlot,
                                Opts
                            );
                        {error, Error} ->
                            % If the compute_slot function returns an error,
                            % we return the error details, along with the current
                            % slot.
                            ErrMsg =
                                if is_map(Error) ->
                                    Error;
                                true -> #{ <<"error">> => Error }
                                end,
                            {error,
                                ErrMsg#{
                                    <<"phase">> => <<"compute">>,
                                    <<"attempted-slot">> => NextSlot
                                }
                            }
                    end
            end
    end.

%% @doc Compute a single slot for a process, given an initialized state.
compute_slot(ProcID, State, RawInputMsg, ReqMsg, Opts) ->
    % Ensure that the next slot is the slot that we are expecting, just
    % in case there is a scheduler device error.
    NextSlot = hb_util:int(hb_ao:get(<<"slot">>, RawInputMsg, Opts)),
    % If the input message does not have a path, set it to `compute'.
    InputMsg =
        case hb_path:from_message(request, RawInputMsg) of
            undefined -> RawInputMsg#{ <<"path">> => <<"compute">> };
            _ -> RawInputMsg
        end,
    ?event({input_msg, InputMsg}),
    ?event(compute, {executing, {proc_id, ProcID}, {slot, NextSlot}}, Opts),
    % Unset the previous results.
    UnsetResults = hb_ao:set(State, #{ <<"results">> => unset }, Opts),
    Res = run_as(<<"execution">>, UnsetResults, InputMsg, Opts),
    case Res of
        {ok, Msg3} ->
            ?event(compute_short, {executed, {slot, NextSlot}, {proc_id, ProcID}}, Opts),
            % We have now transformed slot n -> n + 1. Increment the current slot.
            Msg3SlotAfter = hb_ao:set(Msg3, #{ <<"at-slot">> => NextSlot }, Opts),
            % Notify any waiters that the result for a slot is now available.
            dev_process_worker:notify_compute(
                ProcID,
                NextSlot,
                {ok, Msg3SlotAfter},
                Opts
            ),
            store_result(ProcID, NextSlot, Msg3SlotAfter, ReqMsg, Opts),
            {ok, Msg3SlotAfter};
        {error, Error} ->
            {error, Error}
    end.

%% @doc Store the resulting state in the cache, potentially with the snapshot
%% key.
store_result(ProcID, Slot, Msg3, Msg2, Opts) ->
    % Cache the `Memory' key every `Cache-Frequency' slots.
    Freq = hb_opts:get(process_cache_frequency, ?DEFAULT_CACHE_FREQ, Opts),
    Msg3MaybeWithSnapshot =
        case Slot rem Freq of
            0 ->
                ?event(compute_debug,
                    {snapshotting, {proc_id, ProcID}, {slot, Slot}}, Opts),
                {ok, Snapshot} = snapshot(Msg3, Msg2, Opts),
				?event(snapshot,
					{got_snapshot,
						{storing_as_slot, Slot},
						{snapshot, Snapshot}
					}
				),
                ?event(compute_debug,
                    {snapshot_generated, {proc_id, ProcID}, {slot, Slot}}, Opts),
				Msg3#{ <<"snapshot">> => Snapshot };
            _ -> 
                Msg3
        end,
    ?event(compute, {caching_result, {proc_id, ProcID}, {slot, Slot}}, Opts),
    Writer = 
        fun() ->
            dev_process_cache:write(ProcID, Slot, Msg3MaybeWithSnapshot, Opts)
        end,
    case hb_opts:get(process_async_cache, true, Opts) of
        true ->
            spawn(Writer),
            ?event(compute, {caching_delegated, {proc_id, ProcID}, {slot, Slot}}, Opts);
        false ->
            Writer(),
            ?event(compute, {caching_completed, {proc_id, ProcID}, {slot, Slot}}, Opts)
    end.

%% @doc Returns the known state of the process at either the current slot, or
%% the latest slot in the cache depending on the `process_now_from_cache' option.
now(RawMsg1, Msg2, Opts) ->
    Msg1 = ensure_process_key(RawMsg1, Opts),
    ProcessID = process_id(Msg1, #{}, Opts),
    case hb_opts:get(process_now_from_cache, false, Opts) of
        false ->
            {ok, CurrentSlot} =
                hb_ao:resolve(
                    Msg1,
                    #{ <<"path">> => <<"slot/current">> },
                    Opts
                ),
            ?event({now_called, {process, ProcessID}, {slot, CurrentSlot}}),
            hb_ao:resolve(
                Msg1,
                #{ <<"path">> => <<"compute">>, <<"slot">> => CurrentSlot },
                Opts
            );
        CacheParam ->
            % We are serving the latest known state from the cache, rather
            % than computing it.
            LatestKnown = dev_process_cache:latest(ProcessID, [], Opts),
            case LatestKnown of
                {ok, LatestSlot, LatestMsg} ->
                    ?event(compute_short,
                        {serving_latest_cached_state,
                            {proc_id, ProcessID},
                            {slot, LatestSlot}
                        },
                        Opts
                    ),
                    ?event(
                        {serving_from_cache,
                            {proc_id, ProcessID},
                            {slot, LatestSlot},
                            {msg, LatestMsg}
                    }),
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
                        now(Msg1, Msg2, Opts#{ process_now_from_cache => false });
                    true ->
                        % The node is configured to only serve the latest known
                        % state from the cache, so we return the latest slot.
                        {failure, <<"No cached state available.">>}
                    end
            end
    end.

%% @doc Recursively push messages to the scheduler until we find a message
%% that does not lead to any further messages being scheduled.
push(Msg1, Msg2, Opts) ->
    ProcBase = ensure_process_key(Msg1, Opts),
    run_as(<<"push">>, ProcBase, Msg2, Opts).

%% @doc Ensure that the process message we have in memory is live and
%% up-to-date.
ensure_loaded(Msg1, Msg2, Opts) ->
    % Get the nonce we are currently on and the inbound nonce.
    TargetSlot = hb_ao:get(<<"slot">>, Msg2, undefined, Opts),
    ProcID = process_id(Msg1, Msg2, Opts),
    ?event({ensure_loaded, {msg1, Msg1}, {msg2, Msg2}, {opts, Opts}}),
    case hb_ao:get(<<"initialized">>, Msg1, Opts) of
        <<"true">> ->
            ?event(already_initialized),
            {ok, Msg1};
        _ ->
            ?event(not_initialized),
            % Try to load the latest complete state from disk.
            LoadRes =
                dev_process_cache:latest(
                    ProcID,
                    [<<"snapshot">>],
                    TargetSlot,
                    Opts
                ),
            ?event(compute,
                {snapshot_load_res,
                    {proc_id, ProcID},
                    {res, LoadRes},
                    {target, TargetSlot}
                }
            ),
            case LoadRes of
                {ok, LoadedSlot, SnapshotMsg} ->
                    % Restore the devices in the executor stack with the
                    % loaded state. This allows the devices to load any
                    % necessary 'shadow' state (state not represented in
                    % the public component of a message) into memory.
                    % Do not update the hashpath while we do this, and remove
                    % the snapshot key after we have normalized the message.
                    ?event(compute, {loaded_state_checkpoint, ProcID, LoadedSlot}),
                    {ok, Normalized} =
                        run_as(
                            <<"execution">>,
                            SnapshotMsg,
                            normalize,
                            Opts#{ hashpath => ignore }
                        ),
                    NormalizedWithoutSnapshot = maps:remove(<<"snapshot">>, Normalized),
                    ?event({loaded_state_checkpoint_result,
                        {proc_id, ProcID},
                        {slot, LoadedSlot},
                        {after_normalization, NormalizedWithoutSnapshot}
                    }),
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
                    init(Msg1, Msg2, Opts)
            end
    end.

%% @doc Run a message against Msg1, with the device being swapped out for
%% the device found at `Key'. After execution, the device is swapped back
%% to the original device if the device is the same as we left it.
run_as(Key, Msg1, Msg2, Opts) ->
    BaseDevice = hb_ao:get(<<"device">>, {as, dev_message, Msg1}, Opts),
    ?event({running_as, {key, {explicit, Key}}, {req, Msg2}}),
    {ok, PreparedMsg} =
        dev_message:set(
            ensure_process_key(Msg1, Opts),
            #{
                <<"device">> =>
                    DeviceSet = hb_ao:get(
                        << Key/binary, "-device">>,
                        {as, dev_message, Msg1},
                        default_device(Msg1, Key, Opts),
                        Opts
                    ),
                <<"input-prefix">> =>
                    case hb_ao:get(<<"input-prefix">>, Msg1, Opts) of
                        not_found -> <<"process">>;
                        Prefix -> Prefix
                    end,
                <<"output-prefixes">> =>
                    hb_ao:get(
                        <<Key/binary, "-output-prefixes">>,
                        {as, dev_message, Msg1},
                        undefined, % Undefined in set will be ignored.
                        Opts
                    )
            },
            Opts
        ),
    {Status, BaseResult} =
        hb_ao:resolve(
            PreparedMsg,
            Msg2,
            Opts
        ),
    case {Status, BaseResult} of
        {ok, #{ <<"device">> := DeviceSet }} ->
            {ok, hb_ao:set(BaseResult, #{ <<"device">> => BaseDevice })};
        _ ->
            ?event({returning_base_result, BaseResult}),
            {Status, BaseResult}
    end.

%% @doc Change the message to for that has the device set as this module.
%% In situations where the key that is `run_as' returns a message with a 
%% transformed device, this is useful.
as_process(Msg1, Opts) ->
    {ok, Proc} = dev_message:set(Msg1, #{ <<"device">> => <<"process@1.0">> }, Opts),
    Proc.

%% @doc Helper function to store a copy of the `process' key in the message.
ensure_process_key(Msg1, Opts) ->
    case hb_ao:get(<<"process">>, Msg1, Opts#{ hashpath => ignore }) of
        not_found ->
            % If the message has lost its signers, we need to re-read it from
            % the cache. This can happen if the message was 'cast' to a different
            % device, leading the signers to be unset.
            ProcessMsg =
                case hb_message:signers(Msg1) of
                    [] ->
                        ?event({process_key_not_found_no_signers, {msg1, Msg1}}),
                        case hb_cache:read(hb_message:id(Msg1, all), Opts) of
                            {ok, Proc} -> Proc;
                            not_found ->
                                % Fallback to the original message if we cannot
                                % read it from the cache.
                                Msg1
                        end;
                    Signers ->
                        ?event(
                            {process_key_not_found_but_signers_present,
                                {signers, Signers},
                                {msg1, Msg1}
                            }
                        ),
                        Msg1
                end,
            {ok, Committed} = hb_message:with_only_committed(ProcessMsg, Opts),
            Res = hb_ao:set(
                Msg1,
                #{ <<"process">> => Committed },
                Opts#{ hashpath => ignore }
            ),
            ?event({set_process_key_res, {msg1, Msg1}, {process_msg, ProcessMsg}, {res, Res}}),
            Res;
        _ -> Msg1
    end.

%%% Tests

init() ->
    application:ensure_all_started(hb),
    ok.

%% @doc Generate a process message with a random number, and no 
%% executor.
test_base_process() ->
    test_base_process(#{}).
test_base_process(Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    hb_message:commit(#{
        <<"device">> => <<"process@1.0">>,
        <<"scheduler-device">> => <<"scheduler@1.0">>,
        <<"scheduler-location">> => Address,
        <<"type">> => <<"Process">>,
        <<"test-random-seed">> => rand:uniform(1337)
    }, Wallet).

test_wasm_process(WASMImage) ->
    test_wasm_process(WASMImage, #{}).
test_wasm_process(WASMImage, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    #{ <<"image">> := WASMImageID } = dev_wasm:cache_wasm_image(WASMImage, Opts),
    hb_message:commit(
        maps:merge(
            hb_message:uncommitted(test_base_process(Opts)),
            #{
                <<"execution-device">> => <<"stack@1.0">>,
                <<"device-stack">> => [<<"WASM-64@1.0">>],
                <<"image">> => WASMImageID
            }
        ),
        Wallet
    ).

%% @doc Generate a process message with a random number, and the 
%% `dev_wasm' device for execution.
test_aos_process() ->
    test_aos_process(#{}).
test_aos_process(Opts) ->
    test_aos_process(Opts, [
        <<"WASI@1.0">>,
        <<"JSON-Iface@1.0">>,
        <<"WASM-64@1.0">>,
        <<"Multipass@1.0">>
    ]).
test_aos_process(Opts, Stack) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WASMProc = test_wasm_process(<<"test/aos-2-pure-xs.wasm">>, Opts),
    hb_message:commit(
        maps:merge(
            hb_message:uncommitted(WASMProc),
            #{
                <<"device-stack">> => Stack,
                <<"execution-device">> => <<"stack@1.0">>,
                <<"scheduler-device">> => <<"scheduler@1.0">>,
                <<"output-prefix">> => <<"wasm">>,
                <<"patch-from">> => <<"/results/outbox">>,
                <<"passes">> => 2,
                <<"stack-keys">> =>
                    [
                        <<"init">>,
                        <<"compute">>,
                        <<"snapshot">>,
                        <<"normalize">>
                    ],
                <<"scheduler">> => Address,
                <<"authority">> => Address
            }),
        Wallet
    ).

%% @doc Generate a device that has a stack of two `dev_test's for 
%% execution. This should generate a message state has doubled 
%% `Already-Seen' elements for each assigned slot.
dev_test_process() ->
    Wallet = hb:wallet(),
    hb_message:commit(
        maps:merge(test_base_process(), #{
            <<"execution-device">> => <<"stack@1.0">>,
            <<"device-stack">> => [<<"test-device@1.0">>, <<"test-device@1.0">>]
        }),
        Wallet
    ).

schedule_test_message(Msg1, Text) ->
    schedule_test_message(Msg1, Text, #{}).
schedule_test_message(Msg1, Text, MsgBase) ->
    Wallet = hb:wallet(),
    UncommittedBase = hb_message:uncommitted(MsgBase),
    Msg2 =
        hb_message:commit(#{
                <<"path">> => <<"schedule">>,
                <<"method">> => <<"POST">>,
                <<"body">> =>
                    hb_message:commit(
                        UncommittedBase#{
                            <<"type">> => <<"Message">>,
                            <<"test-label">> => Text
                        },
                        Wallet
                    )
            },
            Wallet
        ),
    {ok, _} = hb_ao:resolve(Msg1, Msg2, #{}).

schedule_aos_call(Msg1, Code) ->
    schedule_aos_call(Msg1, Code, #{}).
schedule_aos_call(Msg1, Code, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    ProcID = hb_message:id(Msg1, all),
    Msg2 =
        hb_message:commit(
            #{
                <<"action">> => <<"Eval">>,
                <<"data">> => Code,
                <<"target">> => ProcID
            },
            Wallet
        ),
    schedule_test_message(Msg1, <<"TEST MSG">>, Msg2).

schedule_wasm_call(Msg1, FuncName, Params) ->
    schedule_wasm_call(Msg1, FuncName, Params, #{}).
schedule_wasm_call(Msg1, FuncName, Params, Opts) ->
    Wallet = hb:wallet(),
    Msg2 = hb_message:commit(#{
        <<"path">> => <<"schedule">>,
        <<"method">> => <<"POST">>,
        <<"body">> =>
            hb_message:commit(
                #{
                    <<"type">> => <<"Message">>,
                    <<"function">> => FuncName,
                    <<"parameters">> => Params
                },
                Wallet
            )
    }, Wallet),
    ?assertMatch({ok, _}, hb_ao:resolve(Msg1, Msg2, Opts)).

schedule_on_process_test() ->
    init(),
    Msg1 = test_aos_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    ?event(messages_scheduled),
    {ok, SchedulerRes} =
        hb_ao:resolve(Msg1, #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"schedule">>
        }, #{}),
    ?assertMatch(
        <<"TEST TEXT 1">>,
        hb_ao:get(<<"assignments/0/body/Test-Label">>, SchedulerRes)
    ),
    ?assertMatch(
        <<"TEST TEXT 2">>,
        hb_ao:get(<<"assignments/1/body/Test-Label">>, SchedulerRes)
    ).

get_scheduler_slot_test() ->
    init(),
    Msg1 = test_base_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    Msg2 = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>
    },
    ?assertMatch(
        {ok, #{ <<"current">> := CurrentSlot }} when CurrentSlot > 0,
        hb_ao:resolve(Msg1, Msg2, #{})
    ).

recursive_path_resolution_test() ->
    init(),
    Msg1 = test_base_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    CurrentSlot =
        hb_ao:resolve(
            Msg1,
            #{ <<"path">> => <<"slot/current">> },
            #{ <<"hashpath">> => ignore }
        ),
    ?event({resolved_current_slot, CurrentSlot}),
    ?assertMatch(
        CurrentSlot when CurrentSlot > 0,
        CurrentSlot
    ),
    ok.

test_device_compute_test() ->
    init(),
    Msg1 = dev_test_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>),
    ?assertMatch(
        {ok, <<"TEST TEXT 2">>},
        hb_ao:resolve(
            Msg1,
            <<"schedule/assignments/1/body/test-label">>,
            #{ <<"hashpath">> => ignore }
        )
    ),
    Msg2 = #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
    {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, #{}),
    ?event({computed_message, {msg3, Msg3}}),
    ?assertEqual(1, hb_ao:get(<<"results/assignment-slot">>, Msg3, #{})),
    ?assertEqual([1,1,0,0], hb_ao:get(<<"already-seen">>, Msg3, #{})).

wasm_compute_test() ->
    init(),
    Msg1 = test_wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Msg1, <<"fac">>, [5.0]),
    schedule_wasm_call(Msg1, <<"fac">>, [6.0]),
    {ok, Msg3} = 
        hb_ao:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
            #{ <<"hashpath">> => ignore }
        ),
    ?event({computed_message, {msg3, Msg3}}),
    ?assertEqual([120.0], hb_ao:get(<<"results/output">>, Msg3, #{})),
    {ok, Msg4} = 
       hb_ao:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            #{ <<"hashpath">> => ignore }
        ),
    ?event({computed_message, {msg4, Msg4}}),
    ?assertEqual([720.0], hb_ao:get(<<"results/output">>, Msg4, #{})).

wasm_compute_from_id_test() ->
    init(),
    Opts = #{ cache_control => <<"always">> },
    Msg1 = test_wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Msg1, <<"fac">>, [5.0], Opts),
    Msg1ID = hb_message:id(Msg1, all),
    Msg2 = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
    {ok, Msg3} = hb_ao:resolve(Msg1ID, Msg2, Opts),
    ?event(process_compute, {computed_message, {msg3, Msg3}}),
    ?assertEqual([120.0], hb_ao:get(<<"results/output">>, Msg3, Opts)).

http_wasm_process_by_id_test() ->
    rand:seed(default),
    SchedWallet = ar_wallet:new(),
    Node = hb_http_server:start_node(Opts = #{
        port => 10000 + rand:uniform(10000),
        priv_wallet => SchedWallet,
        cache_control => <<"always">>,
        store => #{
            <<"store-module">> => hb_store_fs,
            <<"prefix">> => <<"cache-mainnet">>
        }
    }),
    Wallet = ar_wallet:new(),
    Proc = test_wasm_process(<<"test/test-64.wasm">>, Opts),
    hb_cache:write(Proc, Opts),
    ProcID = hb_util:human_id(hb_message:id(Proc, all)),
    InitRes =
        hb_http:post(
            Node,
            << "/schedule" >>,
            Proc,
            #{}
        ),
    ?event({schedule_proc_res, InitRes}),
    ExecMsg =
        hb_message:commit(#{
            <<"target">> => ProcID,
            <<"type">> => <<"Message">>,
            <<"function">> => <<"fac">>,
            <<"parameters">> => [5.0]
        },
        Wallet
    ),
    {ok, Msg3} = hb_http:post(Node, << ProcID/binary, "/schedule">>, ExecMsg, #{}),
    ?event({schedule_msg_res, {msg3, Msg3}}),
    {ok, Msg4} =
        hb_http:get(
            Node,
            #{
                <<"path">> => << ProcID/binary, "/compute">>,
                <<"slot">> => 1
            },
            #{}
        ),
    ?event({compute_msg_res, {msg4, Msg4}}),
    ?assertEqual([120.0], hb_ao:get(<<"results/output">>, Msg4, #{})).

aos_compute_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"return 1+1">>),
        schedule_aos_call(Msg1, <<"return 2+2">>),
        Msg2 = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
        {ok, Msg3} = hb_ao:resolve(Msg1, Msg2, #{}),
        {ok, Res} = hb_ao:resolve(Msg3, <<"results">>, #{}),
        ?event({computed_message, {msg3, Res}}),
        {ok, Data} = hb_ao:resolve(Res, <<"data">>, #{}),
        ?event({computed_data, Data}),
        ?assertEqual(<<"2">>, Data),
        Msg4 = #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
        {ok, Msg5} = hb_ao:resolve(Msg1, Msg4, #{}),
        ?assertEqual(<<"4">>, hb_ao:get(<<"results/data">>, Msg5, #{})),
        {ok, Msg5}
    end}.

aos_browsable_state_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1,
            <<"table.insert(ao.outbox.Messages, { Target = ao.id, ",
                "Action = \"State\", ",
                "Data = { Deep = 4, Bool = true } })">>
        ),
        Msg2 = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
        {ok, Msg3} =
            hb_ao:resolve_many(
                [Msg1, Msg2, <<"results">>, <<"outbox">>, 1, <<"data">>, <<"Deep">>],
                #{ cache_control => <<"always">> }
            ),
        ID = hb_message:id(Msg1),
        ?event({computed_message, {id, {explicit, ID}}}),
        ?assertEqual(4, Msg3)
    end}.

aos_state_access_via_http_test_() ->
    {timeout, 60, fun() ->
        rand:seed(default),
        Wallet = ar_wallet:new(),
        Node = hb_http_server:start_node(Opts = #{
            port => 10000 + rand:uniform(10000),
            priv_wallet => Wallet,
            cache_control => <<"always">>,
            store => #{
                <<"store-module">> => hb_store_fs,
                <<"prefix">> => <<"cache-mainnet">>
            },
            force_signed_requests => true
        }),
        Proc = test_aos_process(Opts),
        ProcID = hb_util:human_id(hb_message:id(Proc, all)),
        {ok, _InitRes} = hb_http:post(Node, <<"/schedule">>, Proc, Opts),
        Msg2 = hb_message:commit(#{
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"type">> => <<"Message">>,
            <<"action">> => <<"Eval">>,
            <<"data">> =>
                <<"table.insert(ao.outbox.Messages, { Target = ao.id,",
                    " Action = \"State\", Data = { ",
                        "[\"content-type\"] = \"text/html\", ",
                        "[\"body\"] = \"<h1>Hello, world!</h1>\"",
                    "}})">>,
            <<"target">> => ProcID
        }, Wallet),
        {ok, Msg3} = hb_http:post(Node, << ProcID/binary, "/schedule">>, Msg2, Opts),
        ?event({schedule_msg_res, {msg3, Msg3}}),
        {ok, Msg4} =
            hb_http:get(
                Node,
                #{
                    <<"path">> => << ProcID/binary, "/compute/results/outbox/1/data" >>,
                    <<"slot">> => 1
                },
                Opts
            ),
        ?event({compute_msg_res, {msg4, Msg4}}),
        ?event(
            {try_yourself,
                {explicit,
                    <<
                        Node/binary,
                        "/",
                        ProcID/binary,
                        "/compute&slot=1/results/outbox/1/data"
                    >>
                }
            }
        ),
        ?assertMatch(#{ <<"body">> := <<"<h1>Hello, world!</h1>">> }, Msg4),
        ok
    end}.

aos_state_patch_test_() ->
    {timeout, 30, fun() ->
        Wallet = hb:wallet(),
        init(),
        Msg1Raw = test_aos_process(#{}, [
            <<"WASI@1.0">>,
            <<"JSON-Iface@1.0">>,
            <<"WASM-64@1.0">>,
            <<"patch@1.0">>,
            <<"Multipass@1.0">>
        ]),
        {ok, Msg1} = hb_message:with_only_committed(Msg1Raw),
        ProcID = hb_message:id(Msg1, all),
        Msg2 = (hb_message:commit(#{
            <<"data-protocol">> => <<"ao">>,
            <<"variant">> => <<"ao.N.1">>,
            <<"target">> => ProcID,
            <<"type">> => <<"Message">>,
            <<"action">> => <<"Eval">>,
            <<"data">> =>
                <<
                    "table.insert(ao.outbox.Messages, "
                        "{ method = \"PATCH\", x = \"banana\" })"
                >>
        }, Wallet))#{ <<"path">> => <<"schedule">>, <<"method">> => <<"POST">> },
        {ok, _} = hb_ao:resolve(Msg1, Msg2, #{}),
        Msg3 = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
        {ok, Msg4} = hb_ao:resolve(Msg1, Msg3, #{}),
        ?event({computed_message, {msg3, Msg4}}),
        {ok, Data} = hb_ao:resolve(Msg4, <<"x">>, #{}),
        ?event({computed_data, Data}),
        ?assertEqual(<<"banana">>, Data)
    end}.

%% @doc Manually test state restoration without using the cache.
restore_test_() -> {timeout, 30, fun do_test_restore/0}.

do_test_restore() ->
    % Init the process and schedule 3 messages:
    % 1. Set variables in Lua.
    % 2. Return the variable.
    % Execute the first computation, then the second as a disconnected process.
    Opts = #{ process_cache_frequency => 1 },
    init(),
    Store = hb_opts:get(store, no_viable_store, Opts),
    ResetRes = hb_store:reset(Store),
    ?event({reset_store, {result, ResetRes}, {store, Store}}),
    Msg1 = test_aos_process(),
    schedule_aos_call(Msg1, <<"X = 42">>),
    schedule_aos_call(Msg1, <<"X = 1337">>),
    schedule_aos_call(Msg1, <<"return X">>),
    % Compute the first message.
    {ok, _} =
        hb_ao:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            Opts
        ),
    {ok, ResultB} =
        hb_ao:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 2 },
            Opts
        ),
    ?event({result_b, ResultB}),
    ?assertEqual(<<"1337">>, hb_ao:get(<<"results/data">>, ResultB, #{})).

now_results_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"return 1+1">>),
        schedule_aos_call(Msg1, <<"return 2+2">>),
        ?assertEqual({ok, <<"4">>}, hb_ao:resolve(Msg1, <<"now/results/data">>, #{}))
    end}.

prior_results_accessible_test_() ->
	{timeout, 30, fun() ->
		init(),
		Msg1 = test_aos_process(),
		schedule_aos_call(Msg1, <<"return 1+1">>),
		schedule_aos_call(Msg1, <<"return 2+2">>),
		?assertEqual({ok, <<"4">>}, hb_ao:resolve(Msg1, <<"now/results/data">>, #{})),
		?assertMatch({ok, #{ <<"results">> := #{ <<"data">> := <<"4">> } }},
			hb_ao:resolve(
				Msg1,
				#{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
				#{}
			)
		)
	end}.

persistent_process_test() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"X=1">>),
        schedule_aos_call(Msg1, <<"return 2">>),
        schedule_aos_call(Msg1, <<"return X">>),
        T0 = hb:now(),
        FirstSlotMsg2 = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 0
        },
        ?assertMatch(
            {ok, _},
            hb_ao:resolve(Msg1, FirstSlotMsg2, #{ spawn_worker => true })
        ),
        T1 = hb:now(),
        ThirdSlotMsg2 = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 2
        },
        Res = hb_ao:resolve(Msg1, ThirdSlotMsg2, #{}),
        ?event({computed_message, {msg3, Res}}),
        ?assertMatch(
            {ok, _},
            Res
        ),
        T2 = hb:now(),
        ?event(benchmark, {runtimes, {first_run, T1 - T0}, {second_run, T2 - T1}}),
        % The second resolve should be much faster than the first resolve, as the
        % process is already running.
        ?assert(T2 - T1 < ((T1 - T0)/2))
    end}.

simple_wasm_persistent_worker_benchmark_test() ->
    init(),
    BenchTime = 1,
    Msg1 = test_wasm_process(<<"test/test-64.wasm">>),
    schedule_wasm_call(Msg1, <<"fac">>, [5.0]),
    schedule_wasm_call(Msg1, <<"fac">>, [6.0]),
    {ok, Initialized} = 
        hb_ao:resolve(
            Msg1,
            #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
            #{ spawn_worker => true, process_workers => true }
        ),
    Iterations = hb:benchmark(
        fun(Iteration) ->
            schedule_wasm_call(
                Initialized,
                <<"fac">>,
                [5.0]
            ),
            ?assertMatch(
                {ok, _},
                hb_ao:resolve(
                    Initialized,
                    #{ <<"path">> => <<"compute">>, <<"slot">> => Iteration + 1 },
                    #{}
                )
            )
        end,
        BenchTime
    ),
    ?event(benchmark, {scheduled, Iterations}),
    hb_util:eunit_print(
        "Scheduled and evaluated ~p simple wasm process messages in ~p s (~.2f msg/s)",
        [Iterations, BenchTime, Iterations / BenchTime]
    ),
    ?assert(Iterations > 2),
    ok.

aos_persistent_worker_benchmark_test_() ->
    {timeout, 30, fun() ->
        BenchTime = 5,
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"X=1337">>),
        FirstSlotMsg2 = #{
            <<"path">> => <<"compute">>,
            <<"slot">> => 0
        },
        ?assertMatch(
            {ok, _},
            hb_ao:resolve(Msg1, FirstSlotMsg2, #{ spawn_worker => true })
        ),
        Iterations = hb:benchmark(
            fun(Iteration) ->
                schedule_aos_call(
                    Msg1,
                    <<"return X + ", (integer_to_binary(Iteration))/binary>>
                ),
                ?assertMatch(
                    {ok, _},
                    hb_ao:resolve(
                        Msg1,
                        #{ <<"path">> => <<"compute">>, <<"slot">> => Iteration },
                        #{}
                    )
                )
            end,
            BenchTime
        ),
        ?event(benchmark, {scheduled, Iterations}),
        hb_util:eunit_print(
            "Scheduled and evaluated ~p AOS process messages in ~p s (~.2f msg/s)",
            [Iterations, BenchTime, Iterations / BenchTime]
        ),
        ?assert(Iterations >= 2),
        ok
    end}.