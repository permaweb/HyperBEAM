# [Module dev_scheduler_server.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_scheduler_server.erl)




A long-lived server that schedules messages for a process.

<a name="description"></a>

## Description ##
It acts as a deliberate 'bottleneck' to prevent the server accidentally
assigning multiple messages to the same slot.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#assign-3">assign/3*</a></td><td>Assign a message to the next slot.</td></tr><tr><td valign="top"><a href="#commit_assignment-2">commit_assignment/2*</a></td><td>Commit to the assignment using all of our appropriate wallets.</td></tr><tr><td valign="top"><a href="#commitment_wallets-2">commitment_wallets/2*</a></td><td>Determine the appropriate list of keys to use to commit assignments for
a process.</td></tr><tr><td valign="top"><a href="#do_assign-3">do_assign/3*</a></td><td>Generate and store the actual assignment message.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Get the current slot from the scheduling server.</td></tr><tr><td valign="top"><a href="#maybe_inform_recipient-5">maybe_inform_recipient/5*</a></td><td>Potentially inform the caller that the assignment has been scheduled.</td></tr><tr><td valign="top"><a href="#new_proc_test-0">new_proc_test/0*</a></td><td>Test the basic functionality of the server.</td></tr><tr><td valign="top"><a href="#next_hashchain-3">next_hashchain/3*</a></td><td>Create the next element in a chain of hashes that links this and prior
assignments.</td></tr><tr><td valign="top"><a href="#schedule-2">schedule/2</a></td><td>Call the appropriate scheduling server to assign a message.</td></tr><tr><td valign="top"><a href="#server-1">server/1*</a></td><td>The main loop of the server.</td></tr><tr><td valign="top"><a href="#start-3">start/3</a></td><td>Start a scheduling server for a given computation.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="assign-3"></a>

### assign/3 * ###

`assign(State, Message, ReplyPID) -> any()`

Assign a message to the next slot.

<a name="commit_assignment-2"></a>

### commit_assignment/2 * ###

`commit_assignment(BaseAssignment, State) -> any()`

Commit to the assignment using all of our appropriate wallets.

<a name="commitment_wallets-2"></a>

### commitment_wallets/2 * ###

`commitment_wallets(ProcMsg, Opts) -> any()`

Determine the appropriate list of keys to use to commit assignments for
a process.

<a name="do_assign-3"></a>

### do_assign/3 * ###

`do_assign(State, Message, ReplyPID) -> any()`

Generate and store the actual assignment message.

<a name="info-1"></a>

### info/1 ###

`info(ProcID) -> any()`

Get the current slot from the scheduling server.

<a name="maybe_inform_recipient-5"></a>

### maybe_inform_recipient/5 * ###

`maybe_inform_recipient(Mode, ReplyPID, Message, Assignment, State) -> any()`

Potentially inform the caller that the assignment has been scheduled.
The main assignment loop calls this function repeatedly at different stages
of the assignment process. The scheduling mode determines which stages
trigger an update.

<a name="new_proc_test-0"></a>

### new_proc_test/0 * ###

`new_proc_test() -> any()`

Test the basic functionality of the server.

<a name="next_hashchain-3"></a>

### next_hashchain/3 * ###

`next_hashchain(HashChain, Message, Opts) -> any()`

Create the next element in a chain of hashes that links this and prior
assignments.

<a name="schedule-2"></a>

### schedule/2 ###

`schedule(AOProcID, Message) -> any()`

Call the appropriate scheduling server to assign a message.

<a name="server-1"></a>

### server/1 * ###

`server(State) -> any()`

The main loop of the server. Simply waits for messages to assign and
returns the current slot.

<a name="start-3"></a>

### start/3 ###

`start(ProcID, Proc, Opts) -> any()`

Start a scheduling server for a given computation.

<a name="stop-1"></a>

### stop/1 ###

`stop(ProcID) -> any()`

