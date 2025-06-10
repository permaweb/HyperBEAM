# HyperBEAM Overview

HyperBEAM is a client implementation of the AO-Core protocol, written in Erlang. It serves as the 'node' software for the decentralized operating system that AO enables, abstracting hardware provisioning and details from the execution of individual programs.

HyperBEAM node operators can offer the services of their machine to others inside the network by electing to execute any number of different devices, charging users for their computation as necessary.

## Key Features

- **Decentralized Execution**: Run AO processes in a decentralized manner
- **Message Passing**: Communicate between processes via asynchronous message passing
- **Scalable Architecture**: Built on Erlang's powerful concurrency model
- **Extensible Design**: Easy to add new devices and capabilities

## Messages in HyperBEAM

HyperBEAM describes every piece of data as a message, which can be interpreted as a binary term or as a collection of named functions (a Map of functions).

Key properties of messages:
- Every message may specify a device which is interpreted by the AO-Core compatible system
- Executing a named function within a message results in another message
- Messages in AO-Core always beget further messages, giving rise to a vast computational space
- Keys may be lazily evaluated, allowing for efficient computation
- If a message does not explicitly specify a device, its implied device is a `message@1.0`

## Devices

HyperBEAM supports numerous devices, each enabling different services. There are approximately 25 different devices included in the preloaded_devices of a HyperBEAM node.

### Key Preloaded Devices

- **~meta@1.0**: Used to configure the node's hardware, supported devices, metering and payments information
- **~relay@1.0**: Used to relay messages between nodes and the wider HTTP network
- **~wasm64@1.0**: Used to execute WebAssembly code via WAMR
- **~json-iface@1.0**: Provides translation between JSON-encoded and HTTP message formats
- **~compute-lite@1.0**: A lightweight WASM executor wrapper for legacy AO processes
- **~snp@1.0**: Used for Trusted Execution Environment (TEE) operations
- **~p4@1.0**: Framework for node operators to sell usage of their hardware
- **~simple-pay@1.0**: Simple pricing device for flat-fee execution
- **~process@1.0**: Enables persistent, shared executions accessible by multiple users
- **~scheduler@1.0**: Assigns linear hashpaths to executions for deterministic ordering
- **~stack@1.0**: Executes an ordered set of devices over the same inputs

## Components

HyperBEAM consists of several core components:

1. **Core Runtime**: The base system that manages process execution
2. **Device Registry**: System for registering and managing devices
3. **Message Router**: Handles message passing between processes and devices
4. **API Layer**: HTTP interfaces for interacting with the system

## System Architecture

HyperBEAM works in conjunction with the Compute Unit (CU), which handles the actual WASM execution. Together, they form a complete execution environment for AO processes.

Each HyperBEAM node is configured using the `~meta@1.0` device, which provides an interface for specifying the node's supported devices, metering and payments information, amongst other configuration options.

## Next Steps

- [Setup HyperBEAM](setup.md): Instructions for installing and running HyperBEAM
- [Configuration](configuration.md): How to configure your HyperBEAM installation
- [Testing](testing.md): Run tests to verify your installation 