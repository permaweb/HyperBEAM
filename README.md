![hyperbeam_logo-thin-3](https://github.com/user-attachments/assets/fcca891c-137e-4022-beff-360eb2a0d05e)

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
3. A unified protocol for expressing `commitments` of the `states` found at
particular `hashpaths`. These commitments allow nodes to participate in varied
economic and cryptographic mechanisms to prove and challenge each-other's
representations regarding the programs that operate inside the AO-Core protocol.
4. A meta-VM that allows any number of different virtual machines and computational
models (`devices`) to be executed inside the AO-Core protocol, while enabling their
states and inputs to be calculated and committed to in a unified format.

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
- A wallet and it's keyfile *(generate a new wallet and keyfile with https://www.wander.app)*

Then you can clone the HyperBEAM source and build it:

```bash
git clone https://github.com/permaweb/HyperBEAM.git
cd HyperBEAM
rebar3 compile
```

If you would prefer to execute HyperBEAM in a containerized environment, you
can use the provided Dockerfile to build a container image.

```bash
docker build -t hyperbeam .
```

If you intend to offer TEE-based computation of AO-Core devices, please see the
[`HyperBEAM OS`](https://github.com/permaweb/hb-os) repo for details on configuration and deployment.

## Running HyperBEAM

Once the code is compiled, you can start HyperBEAM with:

```bash
# Start with default configuration
rebar3 shell
```

The default configuration uses settings from `hb_opts.erl`, which preloads 
all devices and sets up default stores on port 10000.

### Optional Build Profiles

HyperBEAM supports several optional build profiles that enable additional features:

- `genesis_wasm`: Enables Genesis WebAssembly support
- `rocksdb`: Enables RocksDB storage backend (adds RocksDB v1.8.0 dependency)
- `http3`: Enables HTTP/3 support via QUIC protocol


Using these profiles allows you to optimize HyperBEAM for your specific use case without adding unnecessary dependencies to the base installation.

To start a shell with profiles:

```bash
# Single profile
rebar3 as rocksdb shell

# Multiple profiles
rebar3 as rocksdb, genesis_wasm shell
```

To create a release with profiles:

```bash
# Create release with profiles
rebar3 as rocksdb,genesis_wasm release
```

Note: Profiles modify compile-time options that get baked into the release. Choose the profiles you need before starting HyperBEAM.

### Verify Installation

To verify that your HyperBEAM node is running correctly, check:

```bash
curl http://localhost:10000/~meta@1.0/info
```

If you receive a response with node information, your HyperBEAM
installation is working properly.

## Configuration

HyperBEAM can be configured using a `~meta@1.0` device, which is initialized
 using either command line arguments or a configuration file.

### Configuration with `config.flat`

The simplest way to configure HyperBEAM is using the `config.flat` file:

1. A file named `config.flat` is already included in the project directory
2. Update to include your configuration values:

```
port: 10000
priv_key_location: /path/to/wallet.json
```

3. Start HyperBEAM with `rebar3 shell`

HyperBEAM will automatically load your configuration and display the active
settings in the startup log.

### Creating a Release

For production environments, you can create a standalone release:

```bash
rebar3 release
```

This creates a release in `_build/default/rel/hb` that can be deployed independently.

### Runtime Configuration Changes

Additionally, if you would like to modify a running node's configuration, you can
 do so by sending a HTTP Signed Message using any RFC-9421 compatible client
  in the following form:

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
only existing inside the TEE, and can be signed commitments of AO-Core executions
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

## Documentation

HyperBEAM uses [MkDocs](https://www.mkdocs.org/) with the [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/) theme to build its documentation site. All documentation source files are located in the `docs/` directory.

To build and view the documentation locally:

```bash
# Create and activate a virtual environment (optional but recommended)
python3 -m venv venv
source venv/bin/activate  # (macOS/Linux) On Windows use `venv\Scripts\activate`

# Install required packages
pip3 install mkdocs mkdocs-material mkdocs-git-revision-date-localized-plugin

# Build the docs
./docs/build-all.sh

# Serve the docs
cd mkdocs-site
python3 -m http.server 8000 
# Then open http://127.0.0.1:8000/ in your browser
```

For more details on the documentation structure, how to contribute, and other information, please see the [full documentation README](./docs/README.md).

## Contributing

HyperBEAM is developed as an open source implementation of the AO-Core protocol 
by [Forward Research](https://fwd.arweave.net). Pull Requests are always welcome!

To get started building on HyperBEAM, check out the [hacking on HyperBEAM](./docs/misc/hacking-on-hyperbeam.md)
guide.

