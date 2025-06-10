# [Module hb_persistent.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_persistent.erl)




Creates and manages long-lived AO-Core resolution processes.

<a name="description"></a>

## Description ##

These can be useful for situations where a message is large and expensive
to serialize and deserialize, or when executions should be deliberately
serialized to avoid parallel executions of the same computation. This
module is called during the core `hb_ao` execution process, so care
must be taken to avoid recursive spawns/loops.

Built using the `pg` module, which is a distributed Erlang process group
manager.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#await-4">await/4</a></td><td>If there was already an Erlang process handling this execution,
we should register with them and wait for them to notify us of
completion.</td></tr><tr><td valign="top"><a href="#deduplicated_execution_test-0">deduplicated_execution_test/0*</a></td><td>Test merging and returning a value with a persistent worker.</td></tr><tr><td valign="top"><a href="#default_await-5">default_await/5</a></td><td>Default await function that waits for a resolution from a worker.</td></tr><tr><td valign="top"><a href="#default_grouper-3">default_grouper/3</a></td><td>Create a group name from a Msg1 and Msg2 pair as a tuple.</td></tr><tr><td valign="top"><a href="#default_worker-3">default_worker/3</a></td><td>A server function for handling persistent executions.</td></tr><tr><td valign="top"><a href="#do_monitor-1">do_monitor/1*</a></td><td></td></tr><tr><td valign="top"><a href="#do_monitor-2">do_monitor/2*</a></td><td></td></tr><tr><td valign="top"><a href="#find_execution-2">find_execution/2*</a></td><td>Find a group with the given name.</td></tr><tr><td valign="top"><a href="#find_or_register-3">find_or_register/3</a></td><td>Register the process to lead an execution if none is found, otherwise
signal that we should await resolution.</td></tr><tr><td valign="top"><a href="#find_or_register-4">find_or_register/4*</a></td><td></td></tr><tr><td valign="top"><a href="#forward_work-2">forward_work/2</a></td><td>Forward requests to a newly delegated execution process.</td></tr><tr><td valign="top"><a href="#group-3">group/3</a></td><td>Calculate the group name for a Msg1 and Msg2 pair.</td></tr><tr><td valign="top"><a href="#notify-4">notify/4</a></td><td>Check our inbox for processes that are waiting for the resolution
of this execution.</td></tr><tr><td valign="top"><a href="#persistent_worker_test-0">persistent_worker_test/0*</a></td><td>Test spawning a default persistent worker.</td></tr><tr><td valign="top"><a href="#register_groupname-2">register_groupname/2*</a></td><td>Register for performing an AO-Core resolution.</td></tr><tr><td valign="top"><a href="#send_response-4">send_response/4*</a></td><td>Helper function that wraps responding with a new Msg3.</td></tr><tr><td valign="top"><a href="#spawn_after_execution_test-0">spawn_after_execution_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#spawn_test_client-2">spawn_test_client/2*</a></td><td></td></tr><tr><td valign="top"><a href="#spawn_test_client-3">spawn_test_client/3*</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0*</a></td><td>Ensure that the <code>pg</code> module is started.</td></tr><tr><td valign="top"><a href="#start_monitor-0">start_monitor/0</a></td><td>Start a monitor that prints the current members of the group every
n seconds.</td></tr><tr><td valign="top"><a href="#start_monitor-1">start_monitor/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_worker-2">start_worker/2</a></td><td>Start a worker process that will hold a message in memory for
future executions.</td></tr><tr><td valign="top"><a href="#start_worker-3">start_worker/3</a></td><td></td></tr><tr><td valign="top"><a href="#stop_monitor-1">stop_monitor/1</a></td><td></td></tr><tr><td valign="top"><a href="#test_device-0">test_device/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_device-1">test_device/1*</a></td><td></td></tr><tr><td valign="top"><a href="#unregister-3">unregister/3*</a></td><td>Unregister for being the leader on an AO-Core resolution.</td></tr><tr><td valign="top"><a href="#unregister_groupname-2">unregister_groupname/2*</a></td><td></td></tr><tr><td valign="top"><a href="#unregister_notify-4">unregister_notify/4</a></td><td>Unregister as the leader for an execution and notify waiting processes.</td></tr><tr><td valign="top"><a href="#wait_for_test_result-1">wait_for_test_result/1*</a></td><td></td></tr><tr><td valign="top"><a href="#worker_event-5">worker_event/5*</a></td><td>Log an event with the worker process.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="await-4"></a>

### await/4 ###

`await(Worker, Msg1, Msg2, Opts) -> any()`

If there was already an Erlang process handling this execution,
we should register with them and wait for them to notify us of
completion.

<a name="deduplicated_execution_test-0"></a>

### deduplicated_execution_test/0 * ###

`deduplicated_execution_test() -> any()`

Test merging and returning a value with a persistent worker.

<a name="default_await-5"></a>

### default_await/5 ###

`default_await(Worker, GroupName, Msg1, Msg2, Opts) -> any()`

Default await function that waits for a resolution from a worker.

<a name="default_grouper-3"></a>

### default_grouper/3 ###

`default_grouper(Msg1, Msg2, Opts) -> any()`

Create a group name from a Msg1 and Msg2 pair as a tuple.

<a name="default_worker-3"></a>

### default_worker/3 ###

`default_worker(GroupName, Msg1, Opts) -> any()`

A server function for handling persistent executions.

<a name="do_monitor-1"></a>

### do_monitor/1 * ###

`do_monitor(Group) -> any()`

<a name="do_monitor-2"></a>

### do_monitor/2 * ###

`do_monitor(Group, Last) -> any()`

<a name="find_execution-2"></a>

### find_execution/2 * ###

`find_execution(Groupname, Opts) -> any()`

Find a group with the given name.

<a name="find_or_register-3"></a>

### find_or_register/3 ###

`find_or_register(Msg1, Msg2, Opts) -> any()`

Register the process to lead an execution if none is found, otherwise
signal that we should await resolution.

<a name="find_or_register-4"></a>

### find_or_register/4 * ###

`find_or_register(GroupName, Msg1, Msg2, Opts) -> any()`

<a name="forward_work-2"></a>

### forward_work/2 ###

`forward_work(NewPID, Opts) -> any()`

Forward requests to a newly delegated execution process.

<a name="group-3"></a>

### group/3 ###

`group(Msg1, Msg2, Opts) -> any()`

Calculate the group name for a Msg1 and Msg2 pair. Uses the Msg1's
`group` function if it is found in the `info`, otherwise uses the default.

<a name="notify-4"></a>

### notify/4 ###

`notify(GroupName, Msg2, Msg3, Opts) -> any()`

Check our inbox for processes that are waiting for the resolution
of this execution. Comes in two forms:
1. Notify on group name alone.
2. Notify on group name and Msg2.

<a name="persistent_worker_test-0"></a>

### persistent_worker_test/0 * ###

`persistent_worker_test() -> any()`

Test spawning a default persistent worker.

<a name="register_groupname-2"></a>

### register_groupname/2 * ###

`register_groupname(Groupname, Opts) -> any()`

Register for performing an AO-Core resolution.

<a name="send_response-4"></a>

### send_response/4 * ###

`send_response(Listener, GroupName, Msg2, Msg3) -> any()`

Helper function that wraps responding with a new Msg3.

<a name="spawn_after_execution_test-0"></a>

### spawn_after_execution_test/0 * ###

`spawn_after_execution_test() -> any()`

<a name="spawn_test_client-2"></a>

### spawn_test_client/2 * ###

`spawn_test_client(Msg1, Msg2) -> any()`

<a name="spawn_test_client-3"></a>

### spawn_test_client/3 * ###

`spawn_test_client(Msg1, Msg2, Opts) -> any()`

<a name="start-0"></a>

### start/0 * ###

`start() -> any()`

Ensure that the `pg` module is started.

<a name="start_monitor-0"></a>

### start_monitor/0 ###

`start_monitor() -> any()`

Start a monitor that prints the current members of the group every
n seconds.

<a name="start_monitor-1"></a>

### start_monitor/1 ###

`start_monitor(Group) -> any()`

<a name="start_worker-2"></a>

### start_worker/2 ###

`start_worker(Msg, Opts) -> any()`

Start a worker process that will hold a message in memory for
future executions.

<a name="start_worker-3"></a>

### start_worker/3 ###

`start_worker(GroupName, NotMsg, Opts) -> any()`

<a name="stop_monitor-1"></a>

### stop_monitor/1 ###

`stop_monitor(PID) -> any()`

<a name="test_device-0"></a>

### test_device/0 * ###

`test_device() -> any()`

<a name="test_device-1"></a>

### test_device/1 * ###

`test_device(Base) -> any()`

<a name="unregister-3"></a>

### unregister/3 * ###

`unregister(Msg1, Msg2, Opts) -> any()`

Unregister for being the leader on an AO-Core resolution.

<a name="unregister_groupname-2"></a>

### unregister_groupname/2 * ###

`unregister_groupname(Groupname, Opts) -> any()`

<a name="unregister_notify-4"></a>

### unregister_notify/4 ###

`unregister_notify(GroupName, Msg2, Msg3, Opts) -> any()`

Unregister as the leader for an execution and notify waiting processes.

<a name="wait_for_test_result-1"></a>

### wait_for_test_result/1 * ###

`wait_for_test_result(Ref) -> any()`

<a name="worker_event-5"></a>

### worker_event/5 * ###

`worker_event(Group, Data, Msg1, Msg2, Opts) -> any()`

Log an event with the worker process. If we used the default grouper
function, we should also include the Msg1 and Msg2 in the event. If we did not,
we assume that the group name expresses enough information to identify the
request.

