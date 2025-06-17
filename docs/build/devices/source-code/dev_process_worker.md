# [Module dev_process_worker.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_process_worker.erl)




A long-lived process worker that keeps state in memory between
calls.

<a name="description"></a>

## Description ##
Implements the interface of `hb_ao` to receive and respond
to computation requests regarding a process as a singleton.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#await-5">await/5</a></td><td>Await a resolution from a worker executing the <code>process@1.0</code> device.</td></tr><tr><td valign="top"><a href="#group-3">group/3</a></td><td>Returns a group name for a request.</td></tr><tr><td valign="top"><a href="#grouper_test-0">grouper_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#info_test-0">info_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#notify_compute-4">notify_compute/4</a></td><td>Notify any waiters for a specific slot of the computed results.</td></tr><tr><td valign="top"><a href="#notify_compute-5">notify_compute/5*</a></td><td></td></tr><tr><td valign="top"><a href="#process_to_group_name-2">process_to_group_name/2*</a></td><td></td></tr><tr><td valign="top"><a href="#send_notification-4">send_notification/4*</a></td><td></td></tr><tr><td valign="top"><a href="#server-3">server/3</a></td><td>Spawn a new worker process.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop a worker process.</td></tr><tr><td valign="top"><a href="#test_init-0">test_init/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="await-5"></a>

### await/5 ###

`await(Worker, GroupName, Msg1, Msg2, Opts) -> any()`

Await a resolution from a worker executing the `process@1.0` device.

<a name="group-3"></a>

### group/3 ###

`group(Msg1, Msg2, Opts) -> any()`

Returns a group name for a request. The worker is responsible for all
computation work on the same process on a single node, so we use the
process ID as the group name.

<a name="grouper_test-0"></a>

### grouper_test/0 * ###

`grouper_test() -> any()`

<a name="info_test-0"></a>

### info_test/0 * ###

`info_test() -> any()`

<a name="notify_compute-4"></a>

### notify_compute/4 ###

`notify_compute(GroupName, SlotToNotify, Msg3, Opts) -> any()`

Notify any waiters for a specific slot of the computed results.

<a name="notify_compute-5"></a>

### notify_compute/5 * ###

`notify_compute(GroupName, SlotToNotify, Msg3, Opts, Count) -> any()`

<a name="process_to_group_name-2"></a>

### process_to_group_name/2 * ###

`process_to_group_name(Msg1, Opts) -> any()`

<a name="send_notification-4"></a>

### send_notification/4 * ###

`send_notification(Listener, GroupName, SlotToNotify, Msg3) -> any()`

<a name="server-3"></a>

### server/3 ###

`server(GroupName, Msg1, Opts) -> any()`

Spawn a new worker process. This is called after the end of the first
execution of `hb_ao:resolve/3`, so the state we are given is the
already current.

<a name="stop-1"></a>

### stop/1 ###

`stop(Worker) -> any()`

Stop a worker process.

<a name="test_init-0"></a>

### test_init/0 * ###

`test_init() -> any()`

