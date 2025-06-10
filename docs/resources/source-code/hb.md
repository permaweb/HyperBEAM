# [Module hb.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb.erl)




Hyperbeam is a decentralized node implementing the AO-Core protocol
on top of Arweave.

<a name="description"></a>

## Description ##

This protocol offers a computation layer for executing arbitrary logic on
top of the network's data.

Arweave is built to offer a robust, permanent storage layer for static data
over time. It can be seen as a globally distributed key-value store that
allows users to lookup IDs to retrieve data at any point in time:

`Arweave(ID) => Message`

Hyperbeam adds another layer of functionality on top of Arweave's protocol:
Allowing users to store and retrieve not only arbitrary bytes, but also to
perform execution of computation upon that data:

`Hyperbeam(Message1, Message2) => Message3`

When Hyperbeam executes a message, it will return a new message containing
the result of that execution, as well as signed commitments of its
correctness. If the computation that is executed is deterministic, recipients
of the new message are able to verify that the computation was performed
correctly. The new message may be stored back to Arweave if desired,
forming a permanent, verifiable, and decentralized log of computation.

The mechanisms described above form the basis of a decentralized and
verifiable compute engine without any relevant protocol-enforced
scalability limits. It is an implementation of a global, shared
supercomputer.

Hyperbeam can be used for an extremely large variety of applications, from
serving static Arweave data with signed commitments of correctness, to
executing smart contracts that have _built-in_ HTTP APIs. The Hyperbeam
node implementation implements AO, an Actor-Oriented process-based
environment for orchestrating computation over Arweave messages in order to
facilitate the execution of more traditional, consensus-based smart
contracts.

The core abstractions of the Hyperbeam node are broadly as follows:

1. The `hb` and `hb_opts` modules manage the node's configuration,
environment variables, and debugging tools.

2. The `hb_http` and `hb_http_server` modules manage all HTTP-related
functionality. `hb_http_server` handles turning received HTTP requests
into messages and applying those messages with the appropriate devices.
`hb_http` handles making requests and responding with messages. `cowboy`
is used to implement the underlying HTTP server.

3. `hb_ao` implements the computation logic of the node: A mechanism
for resolving messages to other messages, via the application of logic
implemented in `devices`. `hb_ao` also manages the loading of Erlang
modules for each device into the node's environment. There are many
different default devices implemented in the hyperbeam node, using the
namespace `dev_*`. Some of the critical components are:

- `dev_message`: The default handler for all messages that do not
specify their own device. The message device is also used to resolve
keys that are not implemented by the device specified in a message,
unless otherwise signalled.

- `dev_stack`: The device responsible for creating and executing stacks
of other devices on messages that request it. There are many uses for
this device, one of which is the resolution of AO processes.

- `dev_p4`: The device responsible for managing payments for the services
provided by the node.

4. `hb_store`, `hb_cache` and the store implementations forms a layered
system for managing the node's access to persistent storage. `hb_cache`
is used as a resolution mechanism for reading and writing messages, while
`hb_store` provides an abstraction over the underlying persistent key-value
byte storage mechanisms. Example `hb_store` mechanisms can be found in
`hb_store_fs` and `hb_store_remote_node`.

5. `ar_*` modules implement functionality related to the base-layer Arweave
protocol and are largely unchanged from their counterparts in the Arweave
node codebase presently maintained by the Digital History Association
(@dha-team/Arweave).

You can find documentation of a similar form to this note in each of the core
modules of the hyperbeam node.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#address-0">address/0</a></td><td>Get the address of a wallet.</td></tr><tr><td valign="top"><a href="#address-1">address/1*</a></td><td></td></tr><tr><td valign="top"><a href="#benchmark-2">benchmark/2</a></td><td>Run a function as many times as possible in a given amount of time.</td></tr><tr><td valign="top"><a href="#benchmark-3">benchmark/3</a></td><td>Run multiple instances of a function in parallel for a given amount of time.</td></tr><tr><td valign="top"><a href="#build-0">build/0</a></td><td>Utility function to hot-recompile and load the hyperbeam environment.</td></tr><tr><td valign="top"><a href="#debug_wait-4">debug_wait/4</a></td><td>Utility function to wait for a given amount of time, printing a debug
message to the console first.</td></tr><tr><td valign="top"><a href="#do_start_simple_pay-1">do_start_simple_pay/1*</a></td><td></td></tr><tr><td valign="top"><a href="#init-0">init/0</a></td><td>Initialize system-wide settings for the hyperbeam node.</td></tr><tr><td valign="top"><a href="#no_prod-3">no_prod/3</a></td><td>Utility function to throw an error if the current mode is prod and
non-prod ready code is being executed.</td></tr><tr><td valign="top"><a href="#now-0">now/0</a></td><td>Utility function to get the current time in milliseconds.</td></tr><tr><td valign="top"><a href="#profile-1">profile/1</a></td><td>Utility function to start a profiling session and run a function,
then analyze the results.</td></tr><tr><td valign="top"><a href="#read-1">read/1</a></td><td>Debugging function to read a message from the cache.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_mainnet-0">start_mainnet/0</a></td><td>Start a mainnet server without payments.</td></tr><tr><td valign="top"><a href="#start_mainnet-1">start_mainnet/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_simple_pay-0">start_simple_pay/0</a></td><td>Start a server with a <code>simple-pay@1.0</code> pre-processor.</td></tr><tr><td valign="top"><a href="#start_simple_pay-1">start_simple_pay/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_simple_pay-2">start_simple_pay/2</a></td><td></td></tr><tr><td valign="top"><a href="#topup-3">topup/3</a></td><td>Helper for topping up a user's balance on a simple-pay node.</td></tr><tr><td valign="top"><a href="#topup-4">topup/4</a></td><td></td></tr><tr><td valign="top"><a href="#wallet-0">wallet/0</a></td><td></td></tr><tr><td valign="top"><a href="#wallet-1">wallet/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="address-0"></a>

### address/0 ###

`address() -> any()`

Get the address of a wallet. Defaults to the address of the wallet
specified by the `priv_key_location` configuration key. It can also take a
wallet tuple as an argument.

<a name="address-1"></a>

### address/1 * ###

`address(Wallet) -> any()`

<a name="benchmark-2"></a>

### benchmark/2 ###

`benchmark(Fun, TLen) -> any()`

Run a function as many times as possible in a given amount of time.

<a name="benchmark-3"></a>

### benchmark/3 ###

`benchmark(Fun, TLen, Procs) -> any()`

Run multiple instances of a function in parallel for a given amount of time.

<a name="build-0"></a>

### build/0 ###

`build() -> any()`

Utility function to hot-recompile and load the hyperbeam environment.

<a name="debug_wait-4"></a>

### debug_wait/4 ###

`debug_wait(T, Mod, Func, Line) -> any()`

Utility function to wait for a given amount of time, printing a debug
message to the console first.

<a name="do_start_simple_pay-1"></a>

### do_start_simple_pay/1 * ###

`do_start_simple_pay(Opts) -> any()`

<a name="init-0"></a>

### init/0 ###

`init() -> any()`

Initialize system-wide settings for the hyperbeam node.

<a name="no_prod-3"></a>

### no_prod/3 ###

`no_prod(X, Mod, Line) -> any()`

Utility function to throw an error if the current mode is prod and
non-prod ready code is being executed. You can find these in the codebase
by looking for ?NO_PROD calls.

<a name="now-0"></a>

### now/0 ###

`now() -> any()`

Utility function to get the current time in milliseconds.

<a name="profile-1"></a>

### profile/1 ###

`profile(Fun) -> any()`

Utility function to start a profiling session and run a function,
then analyze the results. Obviously -- do not use in production.

<a name="read-1"></a>

### read/1 ###

`read(ID) -> any()`

Debugging function to read a message from the cache.
Specify either a scope atom (local or remote) or a store tuple
as the second argument.

<a name="read-2"></a>

### read/2 ###

`read(ID, ScopeAtom) -> any()`

<a name="start_mainnet-0"></a>

### start_mainnet/0 ###

`start_mainnet() -> any()`

Start a mainnet server without payments.

<a name="start_mainnet-1"></a>

### start_mainnet/1 ###

`start_mainnet(Port) -> any()`

<a name="start_simple_pay-0"></a>

### start_simple_pay/0 ###

`start_simple_pay() -> any()`

Start a server with a `simple-pay@1.0` pre-processor.

<a name="start_simple_pay-1"></a>

### start_simple_pay/1 ###

`start_simple_pay(Addr) -> any()`

<a name="start_simple_pay-2"></a>

### start_simple_pay/2 ###

`start_simple_pay(Addr, Port) -> any()`

<a name="topup-3"></a>

### topup/3 ###

`topup(Node, Amount, Recipient) -> any()`

Helper for topping up a user's balance on a simple-pay node.

<a name="topup-4"></a>

### topup/4 ###

`topup(Node, Amount, Recipient, Wallet) -> any()`

<a name="wallet-0"></a>

### wallet/0 ###

`wallet() -> any()`

<a name="wallet-1"></a>

### wallet/1 ###

`wallet(Location) -> any()`

