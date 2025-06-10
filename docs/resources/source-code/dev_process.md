# [Module dev_process.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_process.erl)




This module contains the device implementation of AO processes
in AO-Core.

<a name="description"></a>

## Description ##

The core functionality of the module is in 'routing' requests
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

```

   GET /ID/Schedule:                Returns the messages in the schedule
   POST /ID/Schedule:               Adds a message to the schedule
   GET /ID/Compute/[IDorSlotNum]:   Returns the state of the process after
                                    applying a message
   GET /ID/Now:                     Returns the <code>/Results</code> key of the latest
                                    computed message
```

An example process definition will look like this:

```

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
```

Runtime options:
Cache-Frequency: The number of assignments that will be computed
before the full (restorable) state should be cached.
Cache-Keys:      A list of the keys that should be cached for all
assignments, in addition to `/Results`.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aos_browsable_state_test_-0">aos_browsable_state_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#aos_compute_test_-0">aos_compute_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#aos_persistent_worker_benchmark_test_-0">aos_persistent_worker_benchmark_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#aos_state_access_via_http_test_-0">aos_state_access_via_http_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#aos_state_patch_test_-0">aos_state_patch_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#as_process-2">as_process/2</a></td><td>Change the message to for that has the device set as this module.</td></tr><tr><td valign="top"><a href="#compute-3">compute/3</a></td><td>Compute the result of an assignment applied to the process state, if it
is the next message.</td></tr><tr><td valign="top"><a href="#compute_slot-5">compute_slot/5*</a></td><td>Compute a single slot for a process, given an initialized state.</td></tr><tr><td valign="top"><a href="#compute_to_slot-5">compute_to_slot/5*</a></td><td>Continually get and apply the next assignment from the scheduler until
we reach the target slot that the user has requested.</td></tr><tr><td valign="top"><a href="#default_device-3">default_device/3*</a></td><td>Returns the default device for a given piece of functionality.</td></tr><tr><td valign="top"><a href="#default_device_index-1">default_device_index/1*</a></td><td></td></tr><tr><td valign="top"><a href="#dev_test_process-0">dev_test_process/0</a></td><td>Generate a device that has a stack of two <code>dev_test</code>s for
execution.</td></tr><tr><td valign="top"><a href="#do_test_restore-0">do_test_restore/0</a></td><td></td></tr><tr><td valign="top"><a href="#ensure_loaded-3">ensure_loaded/3*</a></td><td>Ensure that the process message we have in memory is live and
up-to-date.</td></tr><tr><td valign="top"><a href="#ensure_process_key-2">ensure_process_key/2</a></td><td>Helper function to store a copy of the <code>process</code> key in the message.</td></tr><tr><td valign="top"><a href="#get_scheduler_slot_test-0">get_scheduler_slot_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_wasm_process_by_id_test-0">http_wasm_process_by_id_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>When the info key is called, we should return the process exports.</td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3*</a></td><td>Before computation begins, a boot phase is required.</td></tr><tr><td valign="top"><a href="#next-3">next/3*</a></td><td></td></tr><tr><td valign="top"><a href="#now-3">now/3</a></td><td>Returns the known state of the process at either the current slot, or
the latest slot in the cache depending on the <code>process_now_from_cache</code> option.</td></tr><tr><td valign="top"><a href="#now_results_test_-0">now_results_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#persistent_process_test-0">persistent_process_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#prior_results_accessible_test_-0">prior_results_accessible_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#process_id-3">process_id/3</a></td><td>Returns the process ID of the current process.</td></tr><tr><td valign="top"><a href="#push-3">push/3</a></td><td>Recursively push messages to the scheduler until we find a message
that does not lead to any further messages being scheduled.</td></tr><tr><td valign="top"><a href="#recursive_path_resolution_test-0">recursive_path_resolution_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#restore_test_-0">restore_test_/0*</a></td><td>Manually test state restoration without using the cache.</td></tr><tr><td valign="top"><a href="#run_as-4">run_as/4*</a></td><td>Run a message against Msg1, with the device being swapped out for
the device found at <code>Key</code>.</td></tr><tr><td valign="top"><a href="#schedule-3">schedule/3</a></td><td>Wraps functions in the Scheduler device.</td></tr><tr><td valign="top"><a href="#schedule_aos_call-2">schedule_aos_call/2</a></td><td></td></tr><tr><td valign="top"><a href="#schedule_aos_call-3">schedule_aos_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#schedule_on_process_test-0">schedule_on_process_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#schedule_test_message-2">schedule_test_message/2*</a></td><td></td></tr><tr><td valign="top"><a href="#schedule_test_message-3">schedule_test_message/3*</a></td><td></td></tr><tr><td valign="top"><a href="#schedule_wasm_call-3">schedule_wasm_call/3*</a></td><td></td></tr><tr><td valign="top"><a href="#schedule_wasm_call-4">schedule_wasm_call/4*</a></td><td></td></tr><tr><td valign="top"><a href="#simple_wasm_persistent_worker_benchmark_test-0">simple_wasm_persistent_worker_benchmark_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#slot-3">slot/3</a></td><td></td></tr><tr><td valign="top"><a href="#snapshot-3">snapshot/3</a></td><td></td></tr><tr><td valign="top"><a href="#store_result-5">store_result/5*</a></td><td>Store the resulting state in the cache, potentially with the snapshot
key.</td></tr><tr><td valign="top"><a href="#test_aos_process-0">test_aos_process/0</a></td><td>Generate a process message with a random number, and the
<code>dev_wasm</code> device for execution.</td></tr><tr><td valign="top"><a href="#test_aos_process-1">test_aos_process/1</a></td><td></td></tr><tr><td valign="top"><a href="#test_aos_process-2">test_aos_process/2*</a></td><td></td></tr><tr><td valign="top"><a href="#test_base_process-0">test_base_process/0*</a></td><td>Generate a process message with a random number, and no
executor.</td></tr><tr><td valign="top"><a href="#test_base_process-1">test_base_process/1*</a></td><td></td></tr><tr><td valign="top"><a href="#test_device_compute_test-0">test_device_compute_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_wasm_process-1">test_wasm_process/1</a></td><td></td></tr><tr><td valign="top"><a href="#test_wasm_process-2">test_wasm_process/2*</a></td><td></td></tr><tr><td valign="top"><a href="#wasm_compute_from_id_test-0">wasm_compute_from_id_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#wasm_compute_test-0">wasm_compute_test/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="aos_browsable_state_test_-0"></a>

### aos_browsable_state_test_/0 * ###

`aos_browsable_state_test_() -> any()`

<a name="aos_compute_test_-0"></a>

### aos_compute_test_/0 * ###

`aos_compute_test_() -> any()`

<a name="aos_persistent_worker_benchmark_test_-0"></a>

### aos_persistent_worker_benchmark_test_/0 * ###

`aos_persistent_worker_benchmark_test_() -> any()`

<a name="aos_state_access_via_http_test_-0"></a>

### aos_state_access_via_http_test_/0 * ###

`aos_state_access_via_http_test_() -> any()`

<a name="aos_state_patch_test_-0"></a>

### aos_state_patch_test_/0 * ###

`aos_state_patch_test_() -> any()`

<a name="as_process-2"></a>

### as_process/2 ###

`as_process(Msg1, Opts) -> any()`

Change the message to for that has the device set as this module.
In situations where the key that is `run_as` returns a message with a
transformed device, this is useful.

<a name="compute-3"></a>

### compute/3 ###

`compute(Msg1, Msg2, Opts) -> any()`

Compute the result of an assignment applied to the process state, if it
is the next message.

<a name="compute_slot-5"></a>

### compute_slot/5 * ###

`compute_slot(ProcID, State, RawInputMsg, ReqMsg, Opts) -> any()`

Compute a single slot for a process, given an initialized state.

<a name="compute_to_slot-5"></a>

### compute_to_slot/5 * ###

`compute_to_slot(ProcID, Msg1, Msg2, TargetSlot, Opts) -> any()`

Continually get and apply the next assignment from the scheduler until
we reach the target slot that the user has requested.

<a name="default_device-3"></a>

### default_device/3 * ###

`default_device(Msg1, Key, Opts) -> any()`

Returns the default device for a given piece of functionality. Expects
the `process/variant` key to be set in the message. The `execution-device`
_must_ be set in all processes aside those marked with `ao.TN.1` variant.
This is in order to ensure that post-mainnet processes do not default to
using infrastructure that should not be present on nodes in the future.

<a name="default_device_index-1"></a>

### default_device_index/1 * ###

`default_device_index(X1) -> any()`

<a name="dev_test_process-0"></a>

### dev_test_process/0 ###

`dev_test_process() -> any()`

Generate a device that has a stack of two `dev_test`s for
execution. This should generate a message state has doubled
`Already-Seen` elements for each assigned slot.

<a name="do_test_restore-0"></a>

### do_test_restore/0 ###

`do_test_restore() -> any()`

<a name="ensure_loaded-3"></a>

### ensure_loaded/3 * ###

`ensure_loaded(Msg1, Msg2, Opts) -> any()`

Ensure that the process message we have in memory is live and
up-to-date.

<a name="ensure_process_key-2"></a>

### ensure_process_key/2 ###

`ensure_process_key(Msg1, Opts) -> any()`

Helper function to store a copy of the `process` key in the message.

<a name="get_scheduler_slot_test-0"></a>

### get_scheduler_slot_test/0 * ###

`get_scheduler_slot_test() -> any()`

<a name="http_wasm_process_by_id_test-0"></a>

### http_wasm_process_by_id_test/0 * ###

`http_wasm_process_by_id_test() -> any()`

<a name="info-1"></a>

### info/1 ###

`info(Msg1) -> any()`

When the info key is called, we should return the process exports.

<a name="init-0"></a>

### init/0 ###

`init() -> any()`

<a name="init-3"></a>

### init/3 * ###

`init(Msg1, Msg2, Opts) -> any()`

Before computation begins, a boot phase is required. This phase
allows devices on the execution stack to initialize themselves. We set the
`Initialized` key to `True` to indicate that the process has been
initialized.

<a name="next-3"></a>

### next/3 * ###

`next(Msg1, Msg2, Opts) -> any()`

<a name="now-3"></a>

### now/3 ###

`now(RawMsg1, Msg2, Opts) -> any()`

Returns the known state of the process at either the current slot, or
the latest slot in the cache depending on the `process_now_from_cache` option.

<a name="now_results_test_-0"></a>

### now_results_test_/0 * ###

`now_results_test_() -> any()`

<a name="persistent_process_test-0"></a>

### persistent_process_test/0 * ###

`persistent_process_test() -> any()`

<a name="prior_results_accessible_test_-0"></a>

### prior_results_accessible_test_/0 * ###

`prior_results_accessible_test_() -> any()`

<a name="process_id-3"></a>

### process_id/3 ###

`process_id(Msg1, Msg2, Opts) -> any()`

Returns the process ID of the current process.

<a name="push-3"></a>

### push/3 ###

`push(Msg1, Msg2, Opts) -> any()`

Recursively push messages to the scheduler until we find a message
that does not lead to any further messages being scheduled.

<a name="recursive_path_resolution_test-0"></a>

### recursive_path_resolution_test/0 * ###

`recursive_path_resolution_test() -> any()`

<a name="restore_test_-0"></a>

### restore_test_/0 * ###

`restore_test_() -> any()`

Manually test state restoration without using the cache.

<a name="run_as-4"></a>

### run_as/4 * ###

`run_as(Key, Msg1, Msg2, Opts) -> any()`

Run a message against Msg1, with the device being swapped out for
the device found at `Key`. After execution, the device is swapped back
to the original device if the device is the same as we left it.

<a name="schedule-3"></a>

### schedule/3 ###

`schedule(Msg1, Msg2, Opts) -> any()`

Wraps functions in the Scheduler device.

<a name="schedule_aos_call-2"></a>

### schedule_aos_call/2 ###

`schedule_aos_call(Msg1, Code) -> any()`

<a name="schedule_aos_call-3"></a>

### schedule_aos_call/3 ###

`schedule_aos_call(Msg1, Code, Opts) -> any()`

<a name="schedule_on_process_test-0"></a>

### schedule_on_process_test/0 * ###

`schedule_on_process_test() -> any()`

<a name="schedule_test_message-2"></a>

### schedule_test_message/2 * ###

`schedule_test_message(Msg1, Text) -> any()`

<a name="schedule_test_message-3"></a>

### schedule_test_message/3 * ###

`schedule_test_message(Msg1, Text, MsgBase) -> any()`

<a name="schedule_wasm_call-3"></a>

### schedule_wasm_call/3 * ###

`schedule_wasm_call(Msg1, FuncName, Params) -> any()`

<a name="schedule_wasm_call-4"></a>

### schedule_wasm_call/4 * ###

`schedule_wasm_call(Msg1, FuncName, Params, Opts) -> any()`

<a name="simple_wasm_persistent_worker_benchmark_test-0"></a>

### simple_wasm_persistent_worker_benchmark_test/0 * ###

`simple_wasm_persistent_worker_benchmark_test() -> any()`

<a name="slot-3"></a>

### slot/3 ###

`slot(Msg1, Msg2, Opts) -> any()`

<a name="snapshot-3"></a>

### snapshot/3 ###

`snapshot(RawMsg1, Msg2, Opts) -> any()`

<a name="store_result-5"></a>

### store_result/5 * ###

`store_result(ProcID, Slot, Msg3, Msg2, Opts) -> any()`

Store the resulting state in the cache, potentially with the snapshot
key.

<a name="test_aos_process-0"></a>

### test_aos_process/0 ###

`test_aos_process() -> any()`

Generate a process message with a random number, and the
`dev_wasm` device for execution.

<a name="test_aos_process-1"></a>

### test_aos_process/1 ###

`test_aos_process(Opts) -> any()`

<a name="test_aos_process-2"></a>

### test_aos_process/2 * ###

`test_aos_process(Opts, Stack) -> any()`

<a name="test_base_process-0"></a>

### test_base_process/0 * ###

`test_base_process() -> any()`

Generate a process message with a random number, and no
executor.

<a name="test_base_process-1"></a>

### test_base_process/1 * ###

`test_base_process(Opts) -> any()`

<a name="test_device_compute_test-0"></a>

### test_device_compute_test/0 * ###

`test_device_compute_test() -> any()`

<a name="test_wasm_process-1"></a>

### test_wasm_process/1 ###

`test_wasm_process(WASMImage) -> any()`

<a name="test_wasm_process-2"></a>

### test_wasm_process/2 * ###

`test_wasm_process(WASMImage, Opts) -> any()`

<a name="wasm_compute_from_id_test-0"></a>

### wasm_compute_from_id_test/0 * ###

`wasm_compute_from_id_test() -> any()`

<a name="wasm_compute_test-0"></a>

### wasm_compute_test/0 * ###

`wasm_compute_test() -> any()`

