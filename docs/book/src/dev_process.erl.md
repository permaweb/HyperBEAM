# dev_process

[View source on GitHub](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_process.erl)

This module contains the device implementation of AO processes
in AO-Core. The core functionality of the module is in 'routing' requests
for different functionality (scheduling, computing, and pushing messages)
to the appropriate device. This is achieved by swapping out the device 
of the process message with the necessary component in order to run the 
execution, then swapping it back before returning. Computation is supported
as a stack of devices, customizable by the user, while the scheduling
device is (by default) a single device.
This allows the devices to share state as needed. Additionally, after each
computation step the device caches the result at a path relative to the
process definition itself, such that the process message's ID can act as an
immutable reference to the process's growing list of interactions. See 
`dev_process_cache` for details.
The external API of the device is as follows:
<pre>
GET /ID/Schedule:                Returns the messages in the schedule
POST /ID/Schedule:               Adds a message to the schedule
GET /ID/Compute/[IDorSlotNum]:   Returns the state of the process after 
                                 applying a message
GET /ID/Now:                     Returns the `/Results` key of the latest 
                                 computed message
</pre>
An example process definition will look like this:
<pre>
    Device: Process/1.0
    Scheduler-Device: Scheduler/1.0
    Execution-Device: Stack/1.0
    Execution-Stack: "Scheduler/1.0", "Cron/1.0", "WASM/1.0", "PoDA/1.0"
    Cron-Frequency: 10-Minutes
    WASM-Image: WASMImageID
    PoDA:
        Device: PoDA/1.0
        Authority: A
        Authority: B
        Authority: C
        Quorum: 2
</pre>
Runtime options:
    Cache-Frequency: The number of assignments that will be computed 
                     before the full (restorable) state should be cached.
    Cache-Keys:      A list of the keys that should be cached for all 
                     assignments, in addition to `/Results`.

---

## Exported Functions

- `as_process/2`
- `as/3`
- `compute/3`
- `dev_test_process/0`
- `do_test_restore/0`
- `ensure_process_key/2`
- `info/1`
- `init/0`
- `now/3`
- `process_id/3`
- `push/3`
- `schedule_aos_call/2`
- `schedule_aos_call/3`
- `schedule/3`
- `slot/3`
- `snapshot/3`
- `test_aos_process/0`
- `test_aos_process/1`
- `test_wasm_process/1`

---

### info

This module contains the device implementation of AO processes
When the info key is called, we should return the process exports.

```erlang
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
```

### as

Return the process state with the device swapped out for the device

```erlang
as(RawMsg1, Msg2, Opts) ->
    {ok, Msg1} = ensure_loaded(RawMsg1, Msg2, Opts),
    Key = 
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Msg2}, <<"as">>},
                {{as, <<"message@1.0">>, Msg2}, <<"as-device">>}
            ],
            <<"execution">>,
            Opts
        ),
    {ok,
        hb_util:deep_merge(
            ensure_process_key(Msg1, Opts),
            #{
                <<"device">> =>
                    hb_maps:get(
                        << Key/binary, "-device">>,
                        Msg1,
                        default_device(Msg1, Key, Opts),
                        Opts
                    ),
                % Configure input prefix for proper message routing within the
                % device
                <<"input-prefix">> =>
                    case hb_maps:get(<<"input-prefix">>, Msg1, not_found, Opts) of
                        not_found -> <<"process">>;
                        Prefix -> Prefix
                    end,
                % Configure output prefixes for result organization
                <<"output-prefixes">> =>
                    hb_maps:get(
                        <<Key/binary, "-output-prefixes">>,
                        Msg1,
                        undefined, % Undefined in set will be ignored.
```

### default_device

Returns the default device for a given piece of functionality. Expects

```erlang
default_device(Msg1, Key, Opts) ->
    NormKey = hb_ao:normalize_key(Key),
    case {NormKey, hb_util:deep_get(<<"process/variant">>, Msg1, Opts)} of
        {<<"execution">>, <<"ao.TN.1">>} -> <<"genesis-wasm@1.0">>;
        _ -> default_device_index(NormKey)
    end.
```

### default_device_index

```erlang
default_device_index(<<"scheduler">>) -> <<"scheduler@1.0">>;
```

### default_device_index

```erlang
default_device_index(<<"execution">>) -> <<"genesis-wasm@1.0">>;
```

### default_device_index

Wraps functions in the Scheduler device.

```erlang
default_device_index(<<"push">>) -> <<"push@1.0">>.
```

### schedule

Wraps functions in the Scheduler device.

```erlang
schedule(Msg1, Msg2, Opts) ->
    run_as(<<"scheduler">>, Msg1, Msg2, Opts).
```

### slot

Wraps functions in the Scheduler device.

```erlang
slot(Msg1, Msg2, Opts) ->
    ?event({slot_called, {msg1, Msg1}, {msg2, Msg2}}),
    run_as(<<"scheduler">>, Msg1, Msg2, Opts).
```

### next

Wraps functions in the Scheduler device.

```erlang
next(Msg1, _Msg2, Opts) ->
    run_as(<<"scheduler">>, Msg1, next, Opts).
```

### snapshot

Wraps functions in the Scheduler device.

```erlang
snapshot(RawMsg1, _Msg2, Opts) ->
    Msg1 = ensure_process_key(RawMsg1, Opts),
    {ok, SnapshotMsg} = run_as(
        <<"execution">>,
        Msg1,
        #{ <<"path">> => <<"snapshot">>, <<"mode">> => <<"Map">> },
        Opts#{
            cache_control => [<<"no-cache">>, <<"no-store">>],
            hashpath => ignore
        }
    ),
    ProcID = hb_message:id(Msg1, all, Opts),
    Slot = hb_ao:get(<<"at-slot">>, {as, <<"message@1.0">>, Msg1}, Opts),
    {ok,
        hb_private:set(
            SnapshotMsg#{ <<"cache-control">> => [<<"store">>] },
            #{ <<"priv/additional-hashpaths">> =>
                    [
                        hb_path:to_binary([ProcID, <<"snapshot">>, Slot])
                    ]
            },
            Opts
        )
    }.
```

### process_id

Returns the process ID of the current process.

```erlang
process_id(Msg1, Msg2, Opts) ->
    case hb_ao:get(<<"process">>, Msg1, Opts#{ hashpath => ignore }) of
        not_found ->
            process_id(ensure_process_key(Msg1, Opts), Msg2, Opts);
        Process ->
            hb_message:id(
                Process,
                hb_util:atom(maps:get(<<"commitments">>, Msg2, <<"all">>)),
                Opts
            )
    end.
```

### init

Before computation begins, a boot phase is required. This phase

```erlang
init(Msg1, Msg2, Opts) ->
    ?event({init_called, {msg1, Msg1}, {msg2, Msg2}}),
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
```

### compute

Compute the result of an assignment applied to the process state.

```erlang
compute(Msg1, Msg2, Opts) ->
    ProcBase = ensure_process_key(Msg1, Opts),
    ProcID = process_id(ProcBase, #{}, Opts),
    TargetSlot =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Msg2}, <<"compute">>},
                {{as, <<"message@1.0">>, Msg2}, <<"slot">>}
            ],
            Opts
        ),
    case TargetSlot of
        not_found ->
            % The slot is not set, so we need to serve the latest known state.
```

### compute_to_slot

Continually get and apply the next assignment from the scheduler until

```erlang
compute_to_slot(ProcID, Msg1, Msg2, TargetSlot, Opts) ->
    CurrentSlot = hb_ao:get(<<"at-slot">>, Msg1, Opts#{ hashpath => ignore }),
    ?event(compute_short,
        {starting_compute,
            {proc_id, ProcID},
            {current, CurrentSlot},
            {target, TargetSlot}
        }
    ),
    case CurrentSlot of
        CurrentSlot when CurrentSlot > TargetSlot ->
            % The cache should already have the result, so we should never end up
            % here. Depending on the type of process, 'rewinding' may require
            % re-computing from a significantly earlier checkpoint, so for now
            % we throw an error.
```

### compute_slot

Compute a single slot for a process, given an initialized state.

```erlang
compute_slot(ProcID, State, RawInputMsg, ReqMsg, Opts) ->
    % Ensure that the next slot is the slot that we are expecting, just
    % in case there is a scheduler device error.
```

### store_result

Store the resulting state in the cache, potentially with the snapshot

```erlang
store_result(ForceSnapshot, ProcID, Slot, Msg3, Msg2, Opts) ->
    % Cache the `Snapshot' key as frequently as the node is configured to.
```

### should_snapshot

Should we snapshot a new full state result? First, we check if the 

```erlang
should_snapshot(Slot, Msg3, Opts) ->
    should_snapshot_slots(Slot, Opts)
        orelse should_snapshot_time(Msg3, Opts).
```

### should_snapshot_slots

Calculate if we should snapshot based on the number of slots.

```erlang
should_snapshot_slots(Slot, Opts) ->
    case hb_opts:get(process_snapshot_slots, ?DEFAULT_SNAPSHOT_SLOTS, Opts) of
        Undef when (Undef == undefined) or (Undef == <<"false">>) ->
            false;
        RawSnapshotSlots ->
            SnapshotSlots = hb_util:int(RawSnapshotSlots),
            Slot rem SnapshotSlots == 0
    end.
```

### should_snapshot_time

Calculate if we should snapshot based on the elapsed time since the last

```erlang
should_snapshot_time(Msg3, Opts) ->
    case hb_opts:get(process_snapshot_time, ?DEFAULT_SNAPSHOT_TIME, Opts) of
        Undef when (Undef == undefined) or (Undef == <<"false">>) ->
            false;
        RawSecs ->
            Secs = hb_util:int(RawSecs),
            case hb_private:get(<<"last-snapshot">>, Msg3, undefined, Opts) of
                undefined ->
                    ?event(
                        debug_interval,
                        {no_last_snapshot,
                            {interval, Secs},
                            {msg, Msg3}
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
```

### now

Returns the known state of the process at either the current slot, or

```erlang
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
```

### push

Recursively push messages to the scheduler until we find a message
Ensure that the process message we have in memory is live and

```erlang
push(Msg1, Msg2, Opts) ->
    ProcBase = ensure_process_key(Msg1, Opts),
    run_as(<<"push">>, ProcBase, Msg2, Opts).
```

### ensure_loaded

Recursively push messages to the scheduler until we find a message
Ensure that the process message we have in memory is live and

```erlang
ensure_loaded(Msg1, Msg2, Opts) ->
    % Get the nonce we are currently on and the inbound nonce.
```

### without_snapshot

Remove the `snapshot` key from a message and return it.
Run a message against Msg1, with the device being swapped out for

```erlang
without_snapshot(Msg, Opts) ->
    hb_maps:remove(<<"snapshot">>, Msg, Opts).
```

### run_as

Remove the `snapshot` key from a message and return it.
Run a message against Msg1, with the device being swapped out for

```erlang
run_as(Key, Msg1, Path, Opts) when not is_map(Path) ->
    run_as(Key, Msg1, #{ <<"path">> => Path }, Opts);
```

### run_as

Remove the `snapshot` key from a message and return it.
Run a message against Msg1, with the device being swapped out for

```erlang
run_as(Key, Msg1, Msg2, Opts) ->
    % Store the original device so we can restore it after execution
    BaseDevice = hb_maps:get(<<"device">>, Msg1, not_found, Opts),
    ?event({running_as, {key, {explicit, Key}}, {req, Msg2}}),
    % Prepare the message with the specialized device configuration.
```

### as_process

Change the message to for that has the device set as this module.

```erlang
as_process(Msg1, Opts) ->
    {ok, Proc} = dev_message:set(Msg1, #{ <<"device">> => <<"process@1.0">> }, Opts),
    Proc.
```

### ensure_process_key

Helper function to store a copy of the `process` key in the message.

```erlang
ensure_process_key(Msg1, Opts) ->
    case hb_maps:get(<<"process">>, Msg1, not_found, Opts) of
        not_found ->
            % If the message has lost its signers, we need to re-read it from
            % the cache. This can happen if the message was 'cast' to a different
            % device, leading the signers to be unset.
```

### init

```erlang
init() ->
    application:ensure_all_started(hb),
    ok.
```

### test_base_process

Generate a process message with a random number, and no 

```erlang
test_base_process() ->
    test_base_process(#{}).
```

### test_base_process

```erlang
test_base_process(Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    hb_message:commit(#{
        <<"device">> => <<"process@1.0">>,
        <<"scheduler-device">> => <<"scheduler@1.0">>,
        <<"scheduler-location">> => hb_opts:get(scheduler, Address, Opts),
        <<"type">> => <<"Process">>,
        <<"test-random-seed">> => rand:uniform(1337)
    }, Wallet).
```

### test_wasm_process

```erlang
test_wasm_process(WASMImage) ->
    test_wasm_process(WASMImage, #{}).
```

### test_wasm_process

```erlang
test_wasm_process(WASMImage, Opts) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    #{ <<"image">> := WASMImageID } = dev_wasm:cache_wasm_image(WASMImage, Opts),
    hb_message:commit(
        hb_maps:merge(
            hb_message:uncommitted(test_base_process(Opts), Opts),
            #{
                <<"execution-device">> => <<"stack@1.0">>,
                <<"device-stack">> => [<<"wasm-64@1.0">>],
                <<"image">> => WASMImageID
            },
			Opts
        ),
        Opts#{ priv_wallet => Wallet}
    ).
```

### test_aos_process

Generate a process message with a random number, and the 

```erlang
test_aos_process() ->
    test_aos_process(#{}).
```

### test_aos_process

```erlang
test_aos_process(Opts) ->
    test_aos_process(Opts, [
        <<"wasi@1.0">>,
        <<"json-iface@1.0">>,
        <<"wasm-64@1.0">>,
        <<"multipass@1.0">>
    ]).
```

### test_aos_process

```erlang
test_aos_process(Opts, Stack) ->
    Wallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    WASMProc = test_wasm_process(<<"test/aos-2-pure-xs.wasm">>, Opts),
    hb_message:commit(
        hb_maps:merge(
            hb_message:uncommitted(WASMProc, Opts),
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
                <<"scheduler">> =>
                    hb_opts:get(scheduler, Address, Opts),
                <<"authority">> =>
                    hb_opts:get(authority, Address, Opts)
            }, Opts),
        Opts#{ priv_wallet => Wallet}
    ).
```

### dev_test_process

Generate a device that has a stack of two `dev_test`s for 

```erlang
dev_test_process() ->
    Wallet = hb:wallet(),
    hb_message:commit(
        hb_maps:merge(test_base_process(), #{
            <<"execution-device">> => <<"stack@1.0">>,
            <<"device-stack">> => [<<"test-device@1.0">>, <<"test-device@1.0">>]
        }, #{}),
        Wallet
    ).
```

### schedule_test_message

```erlang
schedule_test_message(Msg1, Text, Opts) ->
    schedule_test_message(Msg1, Text, #{}, Opts).
```

### schedule_test_message

```erlang
schedule_test_message(Msg1, Text, MsgBase, Opts) ->
    Wallet = hb:wallet(),
    UncommittedBase = hb_message:uncommitted(MsgBase, Opts),
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
                        Opts#{ priv_wallet => Wallet}
                    )
            },
			Opts#{ priv_wallet => Wallet}
        ),
    {ok, _} = hb_ao:resolve(Msg1, Msg2, Opts).
```

### schedule_aos_call

```erlang
schedule_aos_call(Msg1, Code) ->
    schedule_aos_call(Msg1, Code, #{}).
```

### schedule_aos_call

```erlang
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
            Opts#{priv_wallet => Wallet}
        ),
    schedule_test_message(Msg1, <<"TEST MSG">>, Msg2, Opts).
```

### schedule_wasm_call

```erlang
schedule_wasm_call(Msg1, FuncName, Params) ->
    schedule_wasm_call(Msg1, FuncName, Params, #{}).
```

### schedule_wasm_call

```erlang
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
                Opts#{ priv_wallet => Wallet}
            )
    }, Opts#{ priv_wallet => Wallet}),
    ?assertMatch({ok, _}, hb_ao:resolve(Msg1, Msg2, Opts)).
```

### schedule_on_process_test_

```erlang
schedule_on_process_test_() ->
	{timeout, 30, fun()->
		init(),
		Msg1 = test_aos_process(),
		schedule_test_message(Msg1, <<"TEST TEXT 1">>, #{}),
		schedule_test_message(Msg1, <<"TEST TEXT 2">>, #{}),
		?event(messages_scheduled),
		{ok, SchedulerRes} =
			hb_ao:resolve(Msg1, #{
				<<"method">> => <<"GET">>,
				<<"path">> => <<"schedule">>
			}, #{}),
		?assertMatch(
			<<"TEST TEXT 1">>,
			hb_ao:get(<<"assignments/0/body/test-label">>, SchedulerRes)
		),
		?assertMatch(
			<<"TEST TEXT 2">>,
			hb_ao:get(<<"assignments/1/body/test-label">>, SchedulerRes)
		)
	end}.
```

### get_scheduler_slot_test

```erlang
get_scheduler_slot_test() ->
    init(),
    Msg1 = test_base_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>, #{}),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>, #{}),
    Msg2 = #{
        <<"path">> => <<"slot">>,
        <<"method">> => <<"GET">>
    },
    ?assertMatch(
        {ok, #{ <<"current">> := CurrentSlot }} when CurrentSlot > 0,
        hb_ao:resolve(Msg1, Msg2, #{})
    ).
```

### recursive_path_resolution_test

```erlang
recursive_path_resolution_test() ->
    init(),
    Msg1 = test_base_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>, #{}),
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
```

### test_device_compute_test

```erlang
test_device_compute_test() ->
    init(),
    Msg1 = dev_test_process(),
    schedule_test_message(Msg1, <<"TEST TEXT 1">>, #{}),
    schedule_test_message(Msg1, <<"TEST TEXT 2">>, #{}),
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
```

### wasm_compute_test

```erlang
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
```

### wasm_compute_from_id_test

```erlang
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
```

### http_wasm_process_by_id_test

```erlang
http_wasm_process_by_id_test() ->
    rand:seed(default),
    SchedWallet = ar_wallet:new(),
    Node = hb_http_server:start_node(Opts = #{
        port => 10000 + rand:uniform(10000),
        priv_wallet => SchedWallet,
        cache_control => <<"always">>,
        process_async_cache => false,
        store => #{
            <<"store-module">> => hb_store_fs,
            <<"name">> => <<"cache-mainnet">>
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
```

### aos_compute_test_

```erlang
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
```

### aos_browsable_state_test_

```erlang
aos_browsable_state_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1,
            <<"table.insert(ao.outbox.Messages, { target = ao.id, ",
                "action = \"State\", ",
                "data = { deep = 4, bool = true } })">>
        ),
        Msg2 = #{ <<"path">> => <<"compute">>, <<"slot">> => 0 },
        {ok, Msg3} =
            hb_ao:resolve_many(
                [Msg1, Msg2, <<"results">>, <<"outbox">>, 1, <<"data">>, <<"deep">>],
                #{ cache_control => <<"always">> }
            ),
        ID = hb_message:id(Msg1),
        ?event({computed_message, {id, {explicit, ID}}}),
        ?assertEqual(4, Msg3)
    end}.
```

### aos_state_access_via_http_test_

```erlang
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
                <<"name">> => <<"cache-mainnet">>
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
                <<"table.insert(ao.outbox.Messages, { target = ao.id,",
                    " action = \"State\", data = { ",
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
```

### aos_state_patch_test_

```erlang
aos_state_patch_test_() ->
    {timeout, 30, fun() ->
        Wallet = hb:wallet(),
        init(),
        Msg1Raw = test_aos_process(#{}, [
            <<"wasi@1.0">>,
            <<"json-iface@1.0">>,
            <<"wasm-64@1.0">>,
            <<"patch@1.0">>,
            <<"multipass@1.0">>
        ]),
        {ok, Msg1} = hb_message:with_only_committed(Msg1Raw, #{}),
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
```

### restore_test_

Manually test state restoration without using the cache.

```erlang
restore_test_() -> {timeout, 30, fun do_test_restore/0}.
```

### do_test_restore

Manually test state restoration without using the cache.

```erlang
do_test_restore() ->
    % Init the process and schedule 3 messages:
    % 1. Set variables in Lua.
```

### now_results_test_

```erlang
now_results_test_() ->
    {timeout, 30, fun() ->
        init(),
        Msg1 = test_aos_process(),
        schedule_aos_call(Msg1, <<"return 1+1">>),
        schedule_aos_call(Msg1, <<"return 2+2">>),
        ?assertEqual({ok, <<"4">>}, hb_ao:resolve(Msg1, <<"now/results/data">>, #{}))
    end}.
```

### prior_results_accessible_test_

```erlang
prior_results_accessible_test_() ->
	{timeout, 30, fun() ->
		init(),
        Opts = #{
            process_async_cache => false
        },
		Msg1 = test_aos_process(),
		schedule_aos_call(Msg1, <<"return 1+1">>),
		schedule_aos_call(Msg1, <<"return 2+2">>),
		?assertEqual(
            {ok, <<"4">>},
            hb_ao:resolve(Msg1, <<"now/results/data">>, Opts)
        ),
        {ok, Results} = 
            hb_ao:resolve(
                Msg1,
                #{ <<"path">> => <<"compute">>, <<"slot">> => 1 },
                Opts
            ),
		?assertMatch(
            #{ <<"results">> := #{ <<"data">> := <<"4">> } },
            hb_cache:ensure_all_loaded(Results, Opts)
		)
	end}.
```

### persistent_process_test

```erlang
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
```

### simple_wasm_persistent_worker_benchmark_test

```erlang
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
    Iterations = hb_test_utils:benchmark(
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
    hb_format:eunit_print(
        "Scheduled and evaluated ~p simple wasm process messages in ~p s (~s msg/s)",
        [Iterations, BenchTime, hb_util:human_int(Iterations / BenchTime)]
    ),
    ?assert(Iterations >= 2),
    ok.
```

### aos_persistent_worker_benchmark_test_

```erlang
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
        Iterations = hb_test_utils:benchmark(
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
        hb_format:eunit_print(
            "Scheduled and evaluated ~p AOS process messages in ~p s (~s msg/s)",
            [Iterations, BenchTime, hb_util:human_int(Iterations / BenchTime)]
        ),
        ?assert(Iterations >= 2),
        ok
```

---

*Generated from [dev_process.erl](https://github.com/permaweb/HyperBEAM/blob/edge/src/dev_process.erl)*
