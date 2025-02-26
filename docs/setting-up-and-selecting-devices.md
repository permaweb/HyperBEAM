# Setting Up and Selecting Devices for HyperBEAM Nodes

HyperBEAM is a client implementation of the AO-Core protocol that enables decentralized computations. As a node operator, you'll need to understand the various devices available and how to configure them for your specific use case. This guide will help you make informed decisions when setting up your HyperBEAM node.

## What are Devices in HyperBEAM?

In HyperBEAM, devices are modular components that provide specific functionalities to your node. They encapsulate different computational models that can be attached to the AO-Core protocol framework. Each device serves a specific purpose, from configuring your node to executing WebAssembly code, managing payments, or facilitating message relays.

## Core Configuration: ~meta@1.0 Device

The foundation of every HyperBEAM node is the `~meta@1.0` device, which is used to configure your node's hardware, supported devices, metering, and payment information. This is the first device you'll need to set up.

### Setting Up the ~meta@1.0 Device

You can initialize the ~meta@1.0 device via command line arguments when starting your node:

```bash

rebar3 shell --eval "hb:start_mainnet(#{ port => 9001, key_location => 'path/to/my/wallet.key' })."

```

This command starts your node with a custom port (9001) and specifies the location of your Arweave wallet key file. You can also modify a running node's configuration by sending an HTTP Signed Message.

## Choosing the Right Devices for Your Use Case

HyperBEAM includes 25+ preloaded devices. Here's how to select the most appropriate ones based on your needs:

### Basic Node Operation

These devices form the foundation of a functional HyperBEAM node:

- **~meta@1.0**: Core configuration device (required for all nodes)
- **~relay@1.0**: Handles message relaying between nodes and the HTTP network
- **dev_message**: The identity device that handles message processing

### Computation Services

If you want to offer computation services to the network:

- **~wasm64@1.0**: Executes WebAssembly code using the Web Assembly Micro-Runtime
- **dev_stack**: Manages execution of a stack of devices in either fold or map mode
- **dev_scheduler**: Implements a simple scheduling scheme for handling process execution
- **~process@1.0**: Enables persistent, shared executions accessible by multiple users

### Payment and Metering

If you want to monetize your node's services:

- **p4@1.0**: Core payment framework that works with pricing and ledger devices
- **~simple-pay@1.0**: Implements simple, flexible pricing for flat-fee execution
- **~faff@1.0**: Allows nodes to restrict access to specific users (useful for personal nodes)

### Enhanced Security

If security is a priority:

- **~snp@1.0**: For generating and validating proofs that a node is executing inside a Trusted Execution Environment (TEE)
- **dev_codec_httpsig**: Implements HTTP Message Signatures as described in RFC-9421

### Legacynet Compatibility

If you need to support older AO systems:

- **~json-iface@1.0**: Translation layer between JSON-encoded messages and HyperBEAM's HTTP format
- **~compute-lite@1.0**: Lightweight device for executing legacynet AO processes
- **dev_genesis_wasm**: Provides an environment suitable for legacynet AO processes

## Device Details and Functions

### Communication and Relay Devices

### ~relay@1.0

This device is responsible for relaying messages between nodes and other HTTP(S) endpoints. It can operate in either "call" or "cast" mode:

- **Call mode**: Returns a response from the remote peer
- **Cast mode**: Returns immediately while the message is relayed asynchronously

```

curl /~relay@.1.0/call?method=GET?0.path=https://www.arweave.net/

```

### dev_router

Routes outbound messages from the node to appropriate network recipients via HTTP. It load-balances messages between downstream workers that perform the actual requests. Routes are defined in a precedence-ordered list of maps.

### Computation Devices

### ~wasm64@1.0

Executes WebAssembly code using the Web Assembly Micro-Runtime (WAMR). This device enables running WASM modules from any other device, supporting code written in Rust, C, and C++.

Requirements:

- Process definition
- WASM image
- Message with data to be processed

### dev_stack

Manages execution of multiple devices in sequence. It operates in two modes:

- **Fold mode (default)**: Executes devices in order, passing state forward
- **Map mode**: Executes all devices and combines their results

This is useful for creating complex execution patterns by combining different devices.

### ~process@1.0

Enables persistent, shared executions that can be accessed by multiple users. Each user can add additional inputs to its hashpath. This device allows customization of execution and scheduler devices.

Example process definition:

```

Device: Process/1.0
Scheduler-Device: Scheduler/1.0
Execution-Device: Stack/1.0
Execution-Stack: "Scheduler/1.0", "Cron/1.0", "WASM/1.0", "PoDA/1.0"
Cron-Frequency: 10-Minutes
WASM-Image: WASMImageID
PoDA:
    Device: PoDA/1.0
    Authority: A
    Authority: B
    Authority: C
    Quorum: 2

```

### Payment and Access Control Devices

### p4@1.0

Core payment framework that works with pricing and ledger devices. It requires the following node message settings:

- `p4_pricing_device`: Estimates request cost
- `p4_ledger_device`: Acts as payment ledger

### ~simple-pay@1.0

Implements simple, flexible pricing for per-message, flat-fee execution. The device's ledger is stored in the node message at `simple_pay_ledger`.

### ~faff@1.0

A "friends and family" pricing policy that allows users to process requests only if their addresses are in the node's allow-list. Useful for running a node for personal use or for a specific subset of users.

### Security Devices

### ~snp@1.0

Generates and validates proofs that a node is executing inside a Trusted Execution Environment (TEE). Nodes executing inside TEEs use an ephemeral key pair that provably exists only inside the TEE.

### dev_codec_httpsig

Implements HTTP Message Signatures as described in RFC-9421, providing a way to authenticate and verify the integrity of HTTP messages.

### Utility Devices

### dev_cron

Inserts new messages into the schedule to allow processes to passively 'call' themselves without user interaction. Useful for creating automated, scheduled tasks.

### dev_dedup

Deduplicates messages sent to a process. It runs on the first pass of a `compute` key call if executed in a stack, preventing duplicate processing.

### dev_monitor

Allows flexible monitoring of a process execution. Adding this device to a process will call specified functions with the current process state during each pass.

### dev_multipass

Triggers repass events until a certain counter has been reached. Useful for stacks that need various execution passes to be completed in sequence across devices.

### dev_patch

Finds `PATCH` requests in the `results/outbox` of its message and applies them. Useful for processes whose computation needs to manipulate data outside of the `results` key.

## Device Stacking Strategies

One of the powerful features of HyperBEAM is the ability to stack devices to create complex computational models. Here are some effective stacking strategies:

### Basic Process Execution Stack

```

Execution-Device: Stack/1.0
Execution-Stack: "Scheduler/1.0", "WASM/1.0"

```

This simple stack handles scheduling and WASM execution for basic process needs.

### Enhanced Security Stack

```

Execution-Device: Stack/1.0
Execution-Stack: "Scheduler/1.0", "WASM/1.0", "PoDA/1.0", "~snp@1.0"

```

Adds proof-of-authority consensus and TEE validation for enhanced security.

### Automated Process Stack

```

Execution-Device: Stack/1.0
Execution-Stack: "Scheduler/1.0", "Cron/1.0", "WASM/1.0"

```

Adds the Cron device to enable automated, scheduled execution.

### Analytics Stack

```

Execution-Device: Stack/1.0
Execution-Stack: "Scheduler/1.0", "WASM/1.0", "Monitor/1.0"

```

Adds monitoring capability to track process execution.

## Practical Setup Examples

### Setting Up a Personal Node

For a node intended for personal use only:

```bash

rebar3 shell --eval "hb:start_mainnet(#{
  port => 9001,
  key_location => 'path/to/my/wallet.key',
  p4_pricing_device => '~faff@1.0',
  p4_ledger_device => '~faff@1.0',
  faff_allow_list => ['my-wallet-address']
})."

```

### Setting Up a Public Computation Node

For a node offering computation services to the network:

```bash

rebar3 shell --eval "hb:start_mainnet(#{
  port => 9001,
  key_location => 'path/to/my/wallet.key',
  p4_pricing_device => '~simple-pay@1.0',
  p4_ledger_device => '~simple-pay@1.0',
  simple_pay_price => 0.01,
  preloaded_devices => ['~wasm64@1.0', '~process@1.0', 'dev_stack', 'dev_scheduler']
})."

```

### Setting Up a Secure TEE Node

For a node running in a Trusted Execution Environment:

```bash

rebar3 shell --eval "hb:start_mainnet(#{
  port => 9001,
  key_location => 'path/to/my/wallet.key',
  p4_pricing_device => '~simple-pay@1.0',
  p4_ledger_device => '~simple-pay@1.0',
  simple_pay_price => 0.05,
  preloaded_devices => ['~wasm64@1.0', '~process@1.0', 'dev_stack', 'dev_scheduler', '~snp@1.0']
})."

```

## Conclusion

Setting up a HyperBEAM node involves understanding and configuring various devices based on your specific requirements. By selecting and stacking devices, you can create a node that meets your computational needs, security requirements, and monetization goals.

For more detailed information about each device, refer to the HyperBEAM code repositories. Remember that the AO-Core protocol is designed to be flexible, allowing you to adapt your node to various use cases by combining different devices and computational models.
