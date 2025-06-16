# [Module dev_cron.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_cron.erl)




A device that inserts new messages into the schedule to allow processes
to passively 'call' themselves without user interaction.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#every-3">every/3</a></td><td>Exported function for scheduling a recurring message.</td></tr><tr><td valign="top"><a href="#every_worker_loop-4">every_worker_loop/4*</a></td><td></td></tr><tr><td valign="top"><a href="#every_worker_loop_test-0">every_worker_loop_test/0*</a></td><td>This test verifies that a recurring task can be scheduled and executed.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Exported function for getting device info.</td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td></td></tr><tr><td valign="top"><a href="#once-3">once/3</a></td><td>Exported function for scheduling a one-time message.</td></tr><tr><td valign="top"><a href="#once_executed_test-0">once_executed_test/0*</a></td><td>This test verifies that a one-time task can be scheduled and executed.</td></tr><tr><td valign="top"><a href="#once_worker-3">once_worker/3*</a></td><td>Internal function for scheduling a one-time message.</td></tr><tr><td valign="top"><a href="#parse_time-1">parse_time/1*</a></td><td>Parse a time string into milliseconds.</td></tr><tr><td valign="top"><a href="#stop-3">stop/3</a></td><td>Exported function for stopping a scheduled task.</td></tr><tr><td valign="top"><a href="#stop_every_test-0">stop_every_test/0*</a></td><td>This test verifies that a recurring task can be stopped by
calling the stop function with the task ID.</td></tr><tr><td valign="top"><a href="#stop_once_test-0">stop_once_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_worker-0">test_worker/0*</a></td><td>This is a helper function that is used to test the cron device.</td></tr><tr><td valign="top"><a href="#test_worker-1">test_worker/1*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="every-3"></a>

### every/3 ###

`every(Msg1, Msg2, Opts) -> any()`

Exported function for scheduling a recurring message.

<a name="every_worker_loop-4"></a>

### every_worker_loop/4 * ###

`every_worker_loop(CronPath, Req, Opts, IntervalMillis) -> any()`

<a name="every_worker_loop_test-0"></a>

### every_worker_loop_test/0 * ###

`every_worker_loop_test() -> any()`

This test verifies that a recurring task can be scheduled and executed.

<a name="info-1"></a>

### info/1 ###

`info(X1) -> any()`

Exported function for getting device info.

<a name="info-3"></a>

### info/3 ###

`info(Msg1, Msg2, Opts) -> any()`

<a name="once-3"></a>

### once/3 ###

`once(Msg1, Msg2, Opts) -> any()`

Exported function for scheduling a one-time message.

<a name="once_executed_test-0"></a>

### once_executed_test/0 * ###

`once_executed_test() -> any()`

This test verifies that a one-time task can be scheduled and executed.

<a name="once_worker-3"></a>

### once_worker/3 * ###

`once_worker(Path, Req, Opts) -> any()`

Internal function for scheduling a one-time message.

<a name="parse_time-1"></a>

### parse_time/1 * ###

`parse_time(BinString) -> any()`

Parse a time string into milliseconds.

<a name="stop-3"></a>

### stop/3 ###

`stop(Msg1, Msg2, Opts) -> any()`

Exported function for stopping a scheduled task.

<a name="stop_every_test-0"></a>

### stop_every_test/0 * ###

`stop_every_test() -> any()`

This test verifies that a recurring task can be stopped by
calling the stop function with the task ID.

<a name="stop_once_test-0"></a>

### stop_once_test/0 * ###

`stop_once_test() -> any()`

<a name="test_worker-0"></a>

### test_worker/0 * ###

`test_worker() -> any()`

This is a helper function that is used to test the cron device.
It is used to increment a counter and update the state of the worker.

<a name="test_worker-1"></a>

### test_worker/1 * ###

`test_worker(State) -> any()`

