<img src="https://arweave.net/dOpRkKrNNQ4HHebxZlPCo0BWfrjwJ-CEBQs2EPgrwbg" />

This repository contains a reference implementation of AO-Core, along with an
Erlang-based (BEAM) client implementing a number of devices for the protocol.

AO-Core is a protocol built to enable decentralized computations, offering a
series of universal primitives to achieve this end. Instead of enforcing a single,
monolithic architecture, AO-Core provides a framework into which any number of
different computational models, encapsulated as primitive `devices`, can be attached.

AO-Core's protocol offers a framework for decentralized computations, built upon the
following fundamental primitives:

1. Hashpaths: A mechanism for referencing locations in a program's state-space
prior to execution. These state-space `links` are represented as Merklized lists of
programs inputs and initial states.
2. A unified data structure for representing program states as HTTP documents,
as described in the [HTTP Semantics RFC](https://www.rfc-editor.org/rfc/rfc9110.html).
3. A unified protocol for expressing `attestations` of the `states` found at
particular `hashpaths`. These attestations allow nodes to participate in varied
economic and cryptographic mechanisms to prove and challenge each-other's
representations regarding the programs that operate inside the AO-Core protocol.
4. A meta-VM that allows any number of different virtual machines and computational
models (`devices`) to be executed inside the AO-Core protocol, while enabling their
states and inputs to be calculated and attested to in a unified format.

## What is HyperBeam?

HyperBeam is a client implementation of the AO-Core protocol, written in Erlang.
It can be seen as the 'node' software for the decentralized operating system that
AO enables; abstracting hardware provisioning and details from the execution of
individual programs.

HyperBEAM node operators can offer the services of their machine to others inside
the network by electing to execute any number of different `devices`, charging 
users for their computation as necessary.

Each HyperBEAM node is configured using the `~meta@1.0` device, which provides
an interface for specifying the node's hardware, supported devices, metering and
payments information, amongst other configuration options.

## Getting Started

To begin using HyperBeam, you will need to install:

- The Erlang runtime (OTP 27)
- Rebar3
- Git
- Docker (optional, for containerized deployment)

You will also need:
- A wallet and it's keyfile(generate a new wallet and keyfile with https://www.wander.app)

Then you can clone the HyperBEAM source and build it:

```bash
git clone https://github.com/permaweb/HyperBEAM.git
cd HyperBEAM
rebar3 compile
rebar3 shell # launches the erlang shell with the HyperBEAM module preloaded
```

If you would prefer to execute HyperBEAM in a containerized environment, you
can use the provided Dockerfile to build a container image.

Add your wallet to the `wallets` directory and modify the `config.flat` file to use the correct port.

```bash
docker build --platform=linux/amd64 --target=builder -t hyperbeam .
```

To target a lean image to run just the node, you can use the following command:

```bash
docker build --platform=linux/amd64 --target=runner -t hyperbeam .
```

Once you have the image built, you can run the container with the following command:

```bash
docker run --memory="8g" --memory-swap="8g" \
-it \
-v $(pwd)/wallets/<wallet-file>:/app/hyperbeam-key.json \
-v $(pwd)/config.flat:/app/config.flat \
-p 10000:10000 \
hyperbeam \
rebar3 shell
```

If you intend to offer TEE-based computation of AO-Core devices, please see the
[`HyperBEAM OS`](https://github.com/permaweb/hb-os) repo for details on configuration and deployment.

## Configuration

HyperBeam can be configured using a `~meta@1.0` device. This device is initialized
via the command line arguments provided when the node is started.

```bash
rebar3 shell --eval "hb:start_mainnet(#{ [OPTS] })."
```

For example, in order to start a node using a custom port and Arweave wallet,
you could execute the following command:

```bash
rebar3 shell --eval "hb:start_mainnet(#{ port => 9001, priv_key_location => 'path/to/my/wallet.key' })."
```

Node operators can also configure the environment using a `flat@1.0` encoded settings file. An 
example configuration is found in the `config.flat` file of this repository. The format simply specifies 
configuration options using HTTP header styling. For example, to set the port for the node and to specify
whether it should use caching hueristics or always consult its local data store, the `config.flat` would
be as follows:

```
port: 1337
cache-lookup-hueristics: true
```

Additionally, if you would like to modify a running node's configuration, you can
do so by sending a HTTP Signed Message using any RFC-9421 compatible client in 
the following form:

```
POST /~meta@1.0/info
Your-Config-Tag: Your-Config-Tag
```

The individual headers provided in the message will each be interpreted as additional
configuration options for the node.

## Messages

HyperBEAM describes every piece of data as a `message`, which can be interpreted as
a binary term or as collection of named functions aka. a `Map` of functions.

Every message _may_ specify a `device` which is interpreted by the AO-Core compatible
system in order to operate upon the message's contents, which to say read it, or
execute it. Executing a named function within a message, providing a map of arguments,
results in another `message`.

In this way, `messages` in AO-Core always _beget_ further `messages`, giving rise 
to a vast computational space, leveraging function application and composition at its core.
For those familiar with the concept, this programming model is similar to that 
described by traditional `combinator` systems.

> Notably, this computation does not require the computor of a message
> to know the values of all the keys contained therin. In other words, keys
> may be _lazily_ evaluated, and only by computors that are interested
> in their outputs, or even _sharded_ across arbitrary sets of nodes, as necessary

If a `message` does not explicitly specify a `device`, its implied `device` is a
 `message@1.0`, which simply returns the binary or `message` at a given named function.

## Devices

HyperBeam supports a number of different devices, each of which enable different
services to be offered by the node. There are presently 25 different devices
included in the `preloaded_devices` of a HyperBEAM node, although it is possible
to add and remove devices as necessary.

### Preloaded Devices

The following devices are included in the `preloaded_devices` of a HyperBEAM node:

- `~meta@1.0`: The `~meta@1.0` device is used to configure the node's hardware,
supported devices, metering and payments information, amongst other configuration options.
Additionally, this device allows external clients to find and validate the configuration
of nodes in the network.

- `~relay@1.0`: The `~relay@1.0` device is used to relay messages between nodes
and the wider HTTP network. It offers an interface for sending and receiving messages
to and from nodes in the network, using a variety of execution strategies.

- `~wasm64@1.0`: The `~wasm64@1.0` device is used to execute WebAssembly code, using
the [Web Assembly Micro-Runtime (WAMR)](https://github.com/bytecodealliance/wasm-micro-runtime)
under-the-hood. WASM modules can be called from any other device, and can also be
used to execute `devices` written in languages such as Rust, C, and C++.

- `~json-iface@1.0`: The `~json-iface@1.0` device offers a translation layer between
the JSON-encoded message format used by AOS 2.0 and prior versions, to HyperBEAM's
native HTTP message format.

- `~compute-lite@1.0`: The `~compute-lite@1.0` device is a lightweight device wrapping
a local WASM executor, used for executing legacynet AO processes inside HyperBEAM.
See the [HyperBEAM OS](https://github.com/permaweb/hb-os) repository for an 
example setup with co-executing HyperBEAM and legacy-CU nodes.

- `~snp@1.0`: The `~snp@1.0` device is used to generate and validate proofs that 
the local node, or another node in the network, is executing inside a [Trusted Execution
Environment (TEE)](https://en.wikipedia.org/wiki/Trusted_execution_environment).
Nodes executing inside these environments use an ephemeral key pair, provably
only existing inside the TEE, and can be sign attestations of AO-Core executions
in a trust-minimized way.

- `p4@1.0`: The `p4@1.0` device runs as a `pre-processor` and `post-processor` in
the framework provided by `~meta@1.0`, enabling a framework for node operators to
sell usage of their machine's hardware to execute AO-Core devices. The `p4@1.0`
framework offers two additional hooks, allowing node operators flexibility in how
their hardware is offered: A `pricing` device, and a `ledger` device.

- `~simple-pay@1.0`: Implements a simple, flexible pricing device that can be used
in conjunction with `p4@1.0` to offer flat-fees for the execution of AO-Core messages.

- `~faff@1.0`: A simple pricing (and ledger) device for `p4@1.0`, allowing nodes
to offer access to their services only to a specific set of users. This device is
useful if you intend to operate your node onmly for personal use, or for a specific
subset of users (servicing an individual app, for example).

- `scheduler@1.0`: The `scheduler@1.0` device is used to assign a linear hashpath
to an execution, such that all users may access it with a deterministic ordering.
When used in conjunction with other AO-Core devices, this allows for the creation
of executions that mirror the behaviour of traditional smart contracting networks.

- `stack@1.0`: The `stack@1.0` device is used to execute an ordered set of devices,
over the same inputs. This device allows its users to create complex combinations of
other devices and apply them as a single unit, with a single hashpath.

- `~process@1.0`: Processes enable users to create persistent, shared executions
that can be accessed by any number of users, each of whom may add additional inputs
to its hashpath. The `~process@1.0` allows users to customize the `execution` and
`scheduler` devices that they choose for their process, such that a variety of different
execution patterns can be created. In addition, the `~process@1.0` device offers a
`push` key, which moves messages from a process's execution `outbox` into the
schedule of another execution.

Details on other devices found in the pre-loaded set can be located in their 
respective documentation.

## Contributing

HyperBEAM is developed as an open source implementation of the AO-Core protocol 
by [Forward Research](https://fwd.arweave.net). Pull Requests are always welcome!

To get started building on HyperBEAM, check out the [hacking on HyperBEAM](./docs/hacking-on-hyperbeam.md)
guide.
