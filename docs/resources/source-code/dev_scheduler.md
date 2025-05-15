# [Module dev_scheduler.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler.erl)




A simple scheduler scheme for AO.

<a name="description"></a>

## Description ##
This device expects a message of the form:
Process: `#{ id, Scheduler: #{ Authority } }`

```

   It exposes the following keys for scheduling:<code>#{ method: GET, path: <<"/info">> }</code> ->
           Returns information about the scheduler.<code>#{ method: GET, path: <<"/slot">> }</code> -> <code>slot(Msg1, Msg2, Opts)</code>
           Returns the current slot for a process.<code>#{ method: GET, path: <<"/schedule">> }</code> -> <code>get_schedule(Msg1, Msg2, Opts)</code>
           Returns the schedule for a process in a cursor-traversable format.<code>#{ method: POST, path: <<"/schedule">> }</code> -> <code>post_schedule(Msg1, Msg2, Opts)</code>
           Schedules a new message for a process, or starts a new scheduler
           for the given message.
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#benchmark_suite-2">benchmark_suite/2*</a></td><td></td></tr><tr><td valign="top"><a href="#benchmark_suite_test_-0">benchmark_suite_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#cache_remote_schedule-2">cache_remote_schedule/2*</a></td><td>Cache a schedule received from a remote scheduler.</td></tr><tr><td valign="top"><a href="#check_lookahead_and_local_cache-4">check_lookahead_and_local_cache/4*</a></td><td>Check if we have a result from a lookahead worker or from our local
cache.</td></tr><tr><td valign="top"><a href="#checkpoint-1">checkpoint/1</a></td><td>Returns the current state of the scheduler.</td></tr><tr><td valign="top"><a href="#do_get_remote_schedule-6">do_get_remote_schedule/6*</a></td><td>Get a schedule from a remote scheduler, unless we already have already
read all of the assignments from the local cache.</td></tr><tr><td valign="top"><a href="#do_post_schedule-4">do_post_schedule/4*</a></td><td>Post schedule the message.</td></tr><tr><td valign="top"><a href="#filter_json_assignments-3">filter_json_assignments/3*</a></td><td>Filter JSON assignment results from a remote legacy scheduler.</td></tr><tr><td valign="top"><a href="#find_message_to_schedule-3">find_message_to_schedule/3*</a></td><td>Search the given base and request message pair to find the message to
schedule.</td></tr><tr><td valign="top"><a href="#find_remote_scheduler-3">find_remote_scheduler/3*</a></td><td>Use the SchedulerLocation to the remote path and return a redirect.</td></tr><tr><td valign="top"><a href="#find_server-3">find_server/3*</a></td><td>Locate the correct scheduling server for a given process.</td></tr><tr><td valign="top"><a href="#find_server-4">find_server/4*</a></td><td></td></tr><tr><td valign="top"><a href="#find_target_id-3">find_target_id/3*</a></td><td>Find the schedule ID from a given request.</td></tr><tr><td valign="top"><a href="#generate_local_schedule-5">generate_local_schedule/5*</a></td><td>Generate a <code>GET /schedule</code> response for a process.</td></tr><tr><td valign="top"><a href="#generate_redirect-3">generate_redirect/3*</a></td><td>Generate a redirect message to a scheduler.</td></tr><tr><td valign="top"><a href="#get_hint-2">get_hint/2*</a></td><td>If a hint is present in the string, return it.</td></tr><tr><td valign="top"><a href="#get_local_assignments-4">get_local_assignments/4*</a></td><td>Get the assignments for a process, and whether the request was truncated.</td></tr><tr><td valign="top"><a href="#get_local_schedule_test-0">get_local_schedule_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#get_location-3">get_location/3*</a></td><td>Search for the location of the scheduler in the scheduler-location
cache.</td></tr><tr><td valign="top"><a href="#get_remote_schedule-5">get_remote_schedule/5*</a></td><td>Get a schedule from a remote scheduler, but first read all of the
assignments from the local cache that we already know about.</td></tr><tr><td valign="top"><a href="#get_schedule-3">get_schedule/3*</a></td><td>Generate and return a schedule for a process, optionally between
two slots -- labelled as <code>from</code> and <code>to</code>.</td></tr><tr><td valign="top"><a href="#http_get_json_schedule_test_-0">http_get_json_schedule_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_legacy_schedule_as_aos2_test_-0">http_get_legacy_schedule_as_aos2_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_legacy_schedule_slot_range_test_-0">http_get_legacy_schedule_slot_range_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_legacy_schedule_test_-0">http_get_legacy_schedule_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_legacy_slot_test_-0">http_get_legacy_slot_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_schedule-4">http_get_schedule/4*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_schedule-5">http_get_schedule/5*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_schedule_redirect_test-0">http_get_schedule_redirect_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_schedule_test_-0">http_get_schedule_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_get_slot-2">http_get_slot/2*</a></td><td></td></tr><tr><td valign="top"><a href="#http_init-0">http_init/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_init-1">http_init/1*</a></td><td></td></tr><tr><td valign="top"><a href="#http_post_legacy_schedule_test_-0">http_post_legacy_schedule_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#http_post_schedule_sign-4">http_post_schedule_sign/4*</a></td><td></td></tr><tr><td valign="top"><a href="#http_post_schedule_test-0">http_post_schedule_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#info-0">info/0</a></td><td>This device uses a default_handler to route requests to the correct
function.</td></tr><tr><td valign="top"><a href="#location-3">location/3</a></td><td>Router for <code>record</code> requests.</td></tr><tr><td valign="top"><a href="#many_clients-1">many_clients/1*</a></td><td></td></tr><tr><td valign="top"><a href="#message_cached_assignments-2">message_cached_assignments/2*</a></td><td>Non-device exported helper to get the cached assignments held in a
process.</td></tr><tr><td valign="top"><a href="#next-3">next/3</a></td><td>Load the schedule for a process into the cache, then return the next
assignment.</td></tr><tr><td valign="top"><a href="#node_from_redirect-2">node_from_redirect/2*</a></td><td>Get the node URL from a redirect.</td></tr><tr><td valign="top"><a href="#post_legacy_schedule-4">post_legacy_schedule/4*</a></td><td></td></tr><tr><td valign="top"><a href="#post_location-3">post_location/3*</a></td><td>Generate a new scheduler location record and register it.</td></tr><tr><td valign="top"><a href="#post_remote_schedule-4">post_remote_schedule/4*</a></td><td></td></tr><tr><td valign="top"><a href="#post_schedule-3">post_schedule/3*</a></td><td>Schedules a new message on the SU.</td></tr><tr><td valign="top"><a href="#read_local_assignments-4">read_local_assignments/4*</a></td><td>Get the assignments for a process.</td></tr><tr><td valign="top"><a href="#redirect_from_graphql_test-0">redirect_from_graphql_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#redirect_to_hint_test-0">redirect_to_hint_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#register_location_on_boot_test-0">register_location_on_boot_test/0*</a></td><td>Test that a scheduler location is registered on boot.</td></tr><tr><td valign="top"><a href="#register_new_process_test-0">register_new_process_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#register_scheduler_test-0">register_scheduler_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#remote_slot-3">remote_slot/3*</a></td><td>Get the current slot from a remote scheduler.</td></tr><tr><td valign="top"><a href="#remote_slot-4">remote_slot/4*</a></td><td>Get the current slot from a remote scheduler, based on the variant of
the process's scheduler.</td></tr><tr><td valign="top"><a href="#router-4">router/4</a></td><td>The default handler for the scheduler device.</td></tr><tr><td valign="top"><a href="#schedule-3">schedule/3</a></td><td>A router for choosing between getting the existing schedule, or
scheduling a new message.</td></tr><tr><td valign="top"><a href="#schedule_message_and_get_slot_test-0">schedule_message_and_get_slot_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#single_resolution-1">single_resolution/1*</a></td><td></td></tr><tr><td valign="top"><a href="#slot-3">slot/3</a></td><td>Returns information about the current slot for a process.</td></tr><tr><td valign="top"><a href="#spawn_lookahead_worker-3">spawn_lookahead_worker/3*</a></td><td>Spawn a new Erlang process to fetch the next assignments from the local
cache, if we have them available.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Helper to ensure that the environment is started.</td></tr><tr><td valign="top"><a href="#status-3">status/3</a></td><td>Returns information about the entire scheduler.</td></tr><tr><td valign="top"><a href="#status_test-0">status_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#test_process-0">test_process/0</a></td><td>Generate a _transformed_ process message, not as they are generated
by users.</td></tr><tr><td valign="top"><a href="#test_process-1">test_process/1*</a></td><td></td></tr><tr><td valign="top"><a href="#without_hint-1">without_hint/1*</a></td><td>Take a process ID or target with a potential hint and return just the
process ID.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="benchmark_suite-2"></a>

### benchmark_suite/2 * ###

`benchmark_suite(Port, Base) -> any()`

<a name="benchmark_suite_test_-0"></a>

### benchmark_suite_test_/0 * ###

`benchmark_suite_test_() -> any()`

<a name="cache_remote_schedule-2"></a>

### cache_remote_schedule/2 * ###

`cache_remote_schedule(Schedule, Opts) -> any()`

Cache a schedule received from a remote scheduler.

<a name="check_lookahead_and_local_cache-4"></a>

### check_lookahead_and_local_cache/4 * ###

`check_lookahead_and_local_cache(Msg1, ProcID, TargetSlot, Opts) -> any()`

Check if we have a result from a lookahead worker or from our local
cache. If we have a result in the local cache, we may also start a new
lookahead worker to fetch the next assignments if we have them locally,
ahead of time. This can be enabled/disabled with the `scheduler_lookahead`
option.

<a name="checkpoint-1"></a>

### checkpoint/1 ###

`checkpoint(State) -> any()`

Returns the current state of the scheduler.

<a name="do_get_remote_schedule-6"></a>

### do_get_remote_schedule/6 * ###

`do_get_remote_schedule(ProcID, LocalAssignments, From, To, Redirect, Opts) -> any()`

Get a schedule from a remote scheduler, unless we already have already
read all of the assignments from the local cache.

<a name="do_post_schedule-4"></a>

### do_post_schedule/4 * ###

`do_post_schedule(ProcID, PID, Msg2, Opts) -> any()`

Post schedule the message. `Msg2` by this point has been refined to only
committed keys, and to only include the `target` message that is to be
scheduled.

<a name="filter_json_assignments-3"></a>

### filter_json_assignments/3 * ###

`filter_json_assignments(JSONRes, To, From) -> any()`

Filter JSON assignment results from a remote legacy scheduler.

<a name="find_message_to_schedule-3"></a>

### find_message_to_schedule/3 * ###

`find_message_to_schedule(Msg1, Msg2, Opts) -> any()`

Search the given base and request message pair to find the message to
schedule. The precidence order for search is as follows:
1. `Msg2/body`
2. `Msg2`

<a name="find_remote_scheduler-3"></a>

### find_remote_scheduler/3 * ###

`find_remote_scheduler(ProcID, Scheduler, Opts) -> any()`

Use the SchedulerLocation to the remote path and return a redirect.

<a name="find_server-3"></a>

### find_server/3 * ###

`find_server(ProcID, Msg1, Opts) -> any()`

Locate the correct scheduling server for a given process.

<a name="find_server-4"></a>

### find_server/4 * ###

`find_server(ProcID, Msg1, ToSched, Opts) -> any()`

<a name="find_target_id-3"></a>

### find_target_id/3 * ###

`find_target_id(Msg1, Msg2, Opts) -> any()`

Find the schedule ID from a given request. The precidence order for
search is as follows:
[1. `ToSched/id` -- in the case of `POST schedule`, handled locally]
2. `Msg2/target`
3. `Msg2/id` when `Msg2` has `type: Process`
4. `Msg1/process/id`
5. `Msg1/id` when `Msg1` has `type: Process`
6. `Msg2/id`

<a name="generate_local_schedule-5"></a>

### generate_local_schedule/5 * ###

`generate_local_schedule(Format, ProcID, From, To, Opts) -> any()`

Generate a `GET /schedule` response for a process.

<a name="generate_redirect-3"></a>

### generate_redirect/3 * ###

`generate_redirect(ProcID, SchedulerLocation, Opts) -> any()`

Generate a redirect message to a scheduler.

<a name="get_hint-2"></a>

### get_hint/2 * ###

`get_hint(Str, Opts) -> any()`

If a hint is present in the string, return it. Else, return not_found.

<a name="get_local_assignments-4"></a>

### get_local_assignments/4 * ###

`get_local_assignments(ProcID, From, RequestedTo, Opts) -> any()`

Get the assignments for a process, and whether the request was truncated.

<a name="get_local_schedule_test-0"></a>

### get_local_schedule_test/0 * ###

`get_local_schedule_test() -> any()`

<a name="get_location-3"></a>

### get_location/3 * ###

`get_location(Msg1, Req, Opts) -> any()`

Search for the location of the scheduler in the scheduler-location
cache. If an address is provided, we search for the location of that
specific scheduler. Otherwise, we return the location record for the current
node's scheduler, if it has been established.

<a name="get_remote_schedule-5"></a>

### get_remote_schedule/5 * ###

`get_remote_schedule(RawProcID, From, To, Redirect, Opts) -> any()`

Get a schedule from a remote scheduler, but first read all of the
assignments from the local cache that we already know about.

<a name="get_schedule-3"></a>

### get_schedule/3 * ###

`get_schedule(Msg1, Msg2, Opts) -> any()`

Generate and return a schedule for a process, optionally between
two slots -- labelled as `from` and `to`. If the schedule is not local,
we redirect to the remote scheduler or proxy based on the node opts.

<a name="http_get_json_schedule_test_-0"></a>

### http_get_json_schedule_test_/0 * ###

`http_get_json_schedule_test_() -> any()`

<a name="http_get_legacy_schedule_as_aos2_test_-0"></a>

### http_get_legacy_schedule_as_aos2_test_/0 * ###

`http_get_legacy_schedule_as_aos2_test_() -> any()`

<a name="http_get_legacy_schedule_slot_range_test_-0"></a>

### http_get_legacy_schedule_slot_range_test_/0 * ###

`http_get_legacy_schedule_slot_range_test_() -> any()`

<a name="http_get_legacy_schedule_test_-0"></a>

### http_get_legacy_schedule_test_/0 * ###

`http_get_legacy_schedule_test_() -> any()`

<a name="http_get_legacy_slot_test_-0"></a>

### http_get_legacy_slot_test_/0 * ###

`http_get_legacy_slot_test_() -> any()`

<a name="http_get_schedule-4"></a>

### http_get_schedule/4 * ###

`http_get_schedule(N, PMsg, From, To) -> any()`

<a name="http_get_schedule-5"></a>

### http_get_schedule/5 * ###

`http_get_schedule(N, PMsg, From, To, Format) -> any()`

<a name="http_get_schedule_redirect_test-0"></a>

### http_get_schedule_redirect_test/0 * ###

`http_get_schedule_redirect_test() -> any()`

<a name="http_get_schedule_test_-0"></a>

### http_get_schedule_test_/0 * ###

`http_get_schedule_test_() -> any()`

<a name="http_get_slot-2"></a>

### http_get_slot/2 * ###

`http_get_slot(N, PMsg) -> any()`

<a name="http_init-0"></a>

### http_init/0 * ###

`http_init() -> any()`

<a name="http_init-1"></a>

### http_init/1 * ###

`http_init(Opts) -> any()`

<a name="http_post_legacy_schedule_test_-0"></a>

### http_post_legacy_schedule_test_/0 * ###

`http_post_legacy_schedule_test_() -> any()`

<a name="http_post_schedule_sign-4"></a>

### http_post_schedule_sign/4 * ###

`http_post_schedule_sign(Node, Msg, ProcessMsg, Wallet) -> any()`

<a name="http_post_schedule_test-0"></a>

### http_post_schedule_test/0 * ###

`http_post_schedule_test() -> any()`

<a name="info-0"></a>

### info/0 ###

`info() -> any()`

This device uses a default_handler to route requests to the correct
function.

<a name="location-3"></a>

### location/3 ###

`location(Msg1, Msg2, Opts) -> any()`

Router for `record` requests. Expects either a `POST` or `GET` request.

<a name="many_clients-1"></a>

### many_clients/1 * ###

`many_clients(Opts) -> any()`

<a name="message_cached_assignments-2"></a>

### message_cached_assignments/2 * ###

`message_cached_assignments(Msg, Opts) -> any()`

Non-device exported helper to get the cached assignments held in a
process.

<a name="next-3"></a>

### next/3 ###

`next(Msg1, Msg2, Opts) -> any()`

Load the schedule for a process into the cache, then return the next
assignment. Assumes that Msg1 is a `dev_process` or similar message, having
a `Current-Slot` key. It stores a local cache of the schedule in the
`priv/To-Process` key.

<a name="node_from_redirect-2"></a>

### node_from_redirect/2 * ###

`node_from_redirect(Redirect, Opts) -> any()`

Get the node URL from a redirect.

<a name="post_legacy_schedule-4"></a>

### post_legacy_schedule/4 * ###

`post_legacy_schedule(ProcID, OnlyCommitted, Node, Opts) -> any()`

<a name="post_location-3"></a>

### post_location/3 * ###

`post_location(Msg1, RawReq, Opts) -> any()`

Generate a new scheduler location record and register it. We both send
the new scheduler-location to the given registry, and return it to the caller.

<a name="post_remote_schedule-4"></a>

### post_remote_schedule/4 * ###

`post_remote_schedule(RawProcID, Redirect, OnlyCommitted, Opts) -> any()`

<a name="post_schedule-3"></a>

### post_schedule/3 * ###

`post_schedule(Msg1, Msg2, Opts) -> any()`

Schedules a new message on the SU. Searches Msg1 for the appropriate ID,
then uses the wallet address of the scheduler to determine if the message is
for this scheduler. If so, it schedules the message and returns the assignment.

<a name="read_local_assignments-4"></a>

### read_local_assignments/4 * ###

`read_local_assignments(ProcID, From, To, Opts) -> any()`

Get the assignments for a process.

<a name="redirect_from_graphql_test-0"></a>

### redirect_from_graphql_test/0 * ###

`redirect_from_graphql_test() -> any()`

<a name="redirect_to_hint_test-0"></a>

### redirect_to_hint_test/0 * ###

`redirect_to_hint_test() -> any()`

<a name="register_location_on_boot_test-0"></a>

### register_location_on_boot_test/0 * ###

`register_location_on_boot_test() -> any()`

Test that a scheduler location is registered on boot.

<a name="register_new_process_test-0"></a>

### register_new_process_test/0 * ###

`register_new_process_test() -> any()`

<a name="register_scheduler_test-0"></a>

### register_scheduler_test/0 * ###

`register_scheduler_test() -> any()`

<a name="remote_slot-3"></a>

### remote_slot/3 * ###

`remote_slot(ProcID, Redirect, Opts) -> any()`

Get the current slot from a remote scheduler.

<a name="remote_slot-4"></a>

### remote_slot/4 * ###

`remote_slot(X1, ProcID, Node, Opts) -> any()`

Get the current slot from a remote scheduler, based on the variant of
the process's scheduler.

<a name="router-4"></a>

### router/4 ###

`router(X1, Msg1, Msg2, Opts) -> any()`

The default handler for the scheduler device.

<a name="schedule-3"></a>

### schedule/3 ###

`schedule(Msg1, Msg2, Opts) -> any()`

A router for choosing between getting the existing schedule, or
scheduling a new message.

<a name="schedule_message_and_get_slot_test-0"></a>

### schedule_message_and_get_slot_test/0 * ###

`schedule_message_and_get_slot_test() -> any()`

<a name="single_resolution-1"></a>

### single_resolution/1 * ###

`single_resolution(Opts) -> any()`

<a name="slot-3"></a>

### slot/3 ###

`slot(M1, M2, Opts) -> any()`

Returns information about the current slot for a process.

<a name="spawn_lookahead_worker-3"></a>

### spawn_lookahead_worker/3 * ###

`spawn_lookahead_worker(ProcID, Slot, Opts) -> any()`

Spawn a new Erlang process to fetch the next assignments from the local
cache, if we have them available.

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Helper to ensure that the environment is started.

<a name="status-3"></a>

### status/3 ###

`status(M1, M2, Opts) -> any()`

Returns information about the entire scheduler.

<a name="status_test-0"></a>

### status_test/0 * ###

`status_test() -> any()`

<a name="test_process-0"></a>

### test_process/0 ###

`test_process() -> any()`

Generate a _transformed_ process message, not as they are generated
by users. See `dev_process` for examples of AO process messages.

<a name="test_process-1"></a>

### test_process/1 * ###

`test_process(Wallet) -> any()`

<a name="without_hint-1"></a>

### without_hint/1 * ###

`without_hint(Target) -> any()`

Take a process ID or target with a potential hint and return just the
process ID.

