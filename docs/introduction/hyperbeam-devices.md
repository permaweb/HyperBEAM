# AO Devices

In AO-Core and its implementation HyperBEAM, **Devices** are modular components responsible for processing and interpreting [Messages](./what-is-ao-core.md#core-concepts). They define the specific logic for how computations are performed, data is handled, or interactions occur within the AO ecosystem.

Think of Devices as specialized engines or services that can be plugged into the AO framework. This modularity is key to AO's flexibility and extensibility.

## Purpose of Devices

*   **Define Computation:** Devices dictate *how* a message's instructions are executed. One device might run WASM code, another might manage process state, and yet another might simply relay data.
*   **Enable Specialization:** Nodes running HyperBEAM can choose which Devices to support, allowing them to specialize in certain tasks (e.g., high-compute tasks, storage-focused tasks, secure TEE operations).
*   **Promote Modularity:** New functionalities can be added to AO by creating new Devices, without altering the core protocol.
*   **Distribute Workload:** Different Devices can handle different parts of a complex task, enabling parallel processing and efficient resource utilization across the network.

## Familiar Examples

HyperBEAM includes many preloaded devices that provide core functionality. Some key examples include:

*   **[`~meta@1.0`](../devices/meta-at-1-0.md):** Configures the node itself (hardware specs, supported devices, payment info).
*   **[`~process@1.0`](../devices/process-at-1-0.md):** Manages persistent, shared computational states (like traditional smart contracts, but more flexible).
*   **[`~scheduler@1.0`](../devices/scheduler-at-1-0.md):** Handles the ordering and execution of messages within a process.
*   **[`~wasm64@1.0`](../devices/wasm64-at-1-0.md):** Executes WebAssembly (WASM) code, allowing for complex computations written in languages like Rust, C++, etc.
*   **[`~lua@5.3a`](../devices/lua-at-5-3a.md):** Executes Lua scripts.
*   **[`~relay@1.0`](../devices/relay-at-1-0.md):** Forwards messages between AO nodes or to external HTTP endpoints.
*   **[`~json@1.0`](../devices/json-at-1-0.md):** Provides access to JSON data structures using HyperPATHs.
*   **[`~message@1.0`](../devices/message-at-1-0.md):** Manages message state and processing.
*   **[`~patch@1.0`](../guides/exposing-process-state.md):** Applies state updates directly to a process, often used for migrating or managing process data.

## Beyond the Basics

Devices aren't limited to just computation or state management. They can represent more abstract concepts:

*   **Security Devices ([`~snp@1.0`](../resources/source-code/dev_snp.md), [`dev_codec_httpsig`](../resources/source-code/dev_codec_httpsig.md)):** Handle tasks related to Trusted Execution Environments (TEEs) or message signing, adding layers of security and verification.
*   **Payment/Access Control Devices ([`~p4@1.0`](../resources/source-code/dev_p4.md), [`~faff@1.0`](../resources/source-code/dev_faff.md)):** Manage metering, billing, or access control for node services.
*   **Workflow/Utility Devices ([`dev_cron`](../resources/source-code/dev_cron.md), [`dev_stack`](../resources/source-code/dev_stack.md), [`dev_monitor`](../resources/source-code/dev_monitor.md)):** Coordinate complex execution flows, schedule tasks, or monitor process activity.

## Using Devices

Devices are typically invoked via [HyperPATHs](./pathing-in-ao-core.md). The path specifies which Device should interpret the subsequent parts of the path or the request body.

```
# Example: Execute the 'now' key on the process device for a specific process
/<procId>~process@1.0/now

# Example: Relay a GET request via the relay device
/~relay@1.0/call?method=GET&path=https://example.com
```

The specific functions or 'keys' available for each Device are documented individually. See the [Devices section](../devices/index.md) for details on specific built-in devices. 

## The Potential of Devices

The modular nature of AO Devices opens up vast possibilities for future expansion and innovation. The current set of preloaded and community devices is just the beginning. As the AO ecosystem evolves, we can anticipate the development of new devices catering to increasingly specialized needs:

*   **Specialized Hardware Integration:** Devices could be created to interface directly with specialized hardware accelerators like GPUs (for AI/ML tasks such as running large language models), TPUs, or FPGAs, allowing AO processes to leverage high-performance computing resources securely and verifiably.
*   **Advanced Cryptography:** New devices could implement cutting-edge cryptographic techniques, such as zero-knowledge proofs (ZKPs) or fully homomorphic encryption (FHE), enabling enhanced privacy and complex computations on encrypted data.
*   **Cross-Chain & Off-Chain Bridges:** Devices could act as secure bridges to other blockchain networks or traditional Web2 APIs, facilitating seamless interoperability and data exchange between AO and the wider digital world.
*   **AI/ML Specific Devices:** Beyond raw GPU access, specialized devices could offer higher-level AI/ML functionalities, like optimized model inference engines or distributed training frameworks.
*   **Domain-Specific Logic:** Communities or organizations could develop devices tailored to specific industries or use cases, such as decentralized finance (DeFi) primitives, scientific computing libraries, or decentralized identity management systems.

The Device framework ensures that AO can adapt and grow, incorporating new technologies and computational paradigms without requiring fundamental changes to the core protocol. This extensibility is key to AO's long-term vision of becoming a truly global, decentralized computer.
