# [Module dev_node_process.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_node_process.erl)




A device that implements the singleton pattern for processes specific
to an individual node.

<a name="description"></a>

## Description ##

This device uses the `local-name@1.0` device to
register processes with names locally, persistenting them across reboots.

Definitions of singleton processes are expected to be found with their
names in the `node_processes` section of the node message.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#augment_definition-2">augment_definition/2*</a></td><td>Augment the given process definition with the node's address.</td></tr><tr><td valign="top"><a href="#generate_test_opts-0">generate_test_opts/0*</a></td><td>Helper function to generate a test environment and its options.</td></tr><tr><td valign="top"><a href="#generate_test_opts-1">generate_test_opts/1*</a></td><td></td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Register a default handler for the device.</td></tr><tr><td valign="top"><a href="#lookup-4">lookup/4*</a></td><td>Lookup a process by name.</td></tr><tr><td valign="top"><a href="#lookup_execute_test-0">lookup_execute_test/0*</a></td><td>Test that a process can be spawned, executed upon, and its result retrieved.</td></tr><tr><td valign="top"><a href="#lookup_no_spawn_test-0">lookup_no_spawn_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_spawn_test-0">lookup_spawn_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#spawn_register-2">spawn_register/2*</a></td><td>Spawn a new process according to the process definition found in the
node message, and register it with the given name.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="augment_definition-2"></a>

### augment_definition/2 * ###

`augment_definition(BaseDef, Opts) -> any()`

Augment the given process definition with the node's address.

<a name="generate_test_opts-0"></a>

### generate_test_opts/0 * ###

`generate_test_opts() -> any()`

Helper function to generate a test environment and its options.

<a name="generate_test_opts-1"></a>

### generate_test_opts/1 * ###

`generate_test_opts(Defs) -> any()`

<a name="info-1"></a>

### info/1 ###

`info(Opts) -> any()`

Register a default handler for the device. Inherits `keys` and `set`
from the default device.

<a name="lookup-4"></a>

### lookup/4 * ###

`lookup(Name, Base, Req, Opts) -> any()`

Lookup a process by name.

<a name="lookup_execute_test-0"></a>

### lookup_execute_test/0 * ###

`lookup_execute_test() -> any()`

Test that a process can be spawned, executed upon, and its result retrieved.

<a name="lookup_no_spawn_test-0"></a>

### lookup_no_spawn_test/0 * ###

`lookup_no_spawn_test() -> any()`

<a name="lookup_spawn_test-0"></a>

### lookup_spawn_test/0 * ###

`lookup_spawn_test() -> any()`

<a name="spawn_register-2"></a>

### spawn_register/2 * ###

`spawn_register(Name, Opts) -> any()`

Spawn a new process according to the process definition found in the
node message, and register it with the given name.

