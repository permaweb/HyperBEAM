# Frequently Asked Questions

This page answers common questions about HyperBEAM, its components, and how to use them effectively.

## General Questions

### What is HyperBEAM?

HyperBEAM is a client implementation of the AO-Core protocol written in Erlang. It serves as the node software for a decentralized operating system that allows operators to offer computational resources to users in the AO network.

### How does HyperBEAM differ from other distributed systems?

HyperBEAM focuses on true decentralization with asynchronous message passing between isolated processes. Unlike many distributed systems that rely on central coordination, HyperBEAM nodes can operate independently while still forming a cohesive network. Additionally, its Erlang foundation provides robust fault tolerance and concurrency capabilities.

### What can I build with HyperBEAM?

You can build a wide range of applications, including:

- Decentralized applications (dApps)
- Distributed computation systems
- Peer-to-peer services
- Resilient microservices
- IoT device networks
- Decentralized storage solutions

### Is HyperBEAM open source?

Yes, HyperBEAM is open-source software licensed under the Business Source License License.

### What is the current focus or phase of HyperBEAM development?

The initial development phase focuses on integrating AO processes more deeply with HyperBEAM. A key part of this is phasing out the reliance on traditional "dryrun" simulations for reading process state. Instead, processes are encouraged to use the [~patch@1.0 device](../../resources/source-code/dev_patch.md) to expose specific parts of their state directly via HyperPATH GET requests. This allows for more efficient and direct state access, particularly for web interfaces and external integrations. You can learn more about this mechanism in the [Exposing Process State with the Patch Device](../../build/exposing-process-state.md) guide.

## Installation and Setup

### What are the system requirements for running HyperBEAM?

Currently, HyperBEAM is primarily tested and documented for Ubuntu 22.04. Support for macOS and other platforms will be added in future updates. For detailed requirements, see the [System Requirements](../getting-started/requirements.md) page.

### Can I run HyperBEAM in a container?

While technically possible, running HyperBEAM in Docker containers or other containerization technologies is currently not recommended. The containerization approach may introduce additional complexity and potential performance issues. We recommend running HyperBEAM directly on the host system until container support is more thoroughly tested and optimized.

### How do I update HyperBEAM to the latest version?

To update HyperBEAM:

1. Pull the latest code from the repository
2. Rebuild the application
3. Restart the HyperBEAM service

Specific update instructions will vary depending on your installation method.

### Can I run multiple HyperBEAM nodes on a single machine?

Yes, you can run multiple HyperBEAM nodes on a single machine, but you'll need to configure them to use different ports and data directories to avoid conflicts. However, this is not recommended for production environments as each node should ideally have a unique IP address to properly participate in the network. Running multiple nodes on a single machine is primarily useful for development and testing purposes.

## Architecture and Components

### What is the difference between HyperBEAM and Compute Unit?

- **HyperBEAM**: The Erlang-based node software that handles message routing, process management, and device coordination.
- **Compute Unit (CU)**: A NodeJS implementation that executes WebAssembly modules and handles computational tasks.

Together, these components form a complete execution environment for AO processes.

## Development and Usage

### What programming languages can I use with HyperBEAM?

You can use any programming language that compiles to WebAssembly (WASM) for creating modules that run on the Compute Unit. This includes languages like:

- Lua
- Rust
- C/C++
- And many others with WebAssembly support

### How do I debug processes running in HyperBEAM?

Debugging processes in HyperBEAM can be done through:

1. Logging messages to the system log
2. Monitoring process state and message flow
3. Inspecting memory usage and performance metrics

### Is there a limit to how many processes can run on a node?

The practical limit depends on your hardware resources. Erlang is designed to handle millions of lightweight processes efficiently, but the actual number will be determined by:

- Available memory
- CPU capacity
- Network bandwidth
- Storage speed
- The complexity of your processes


## Troubleshooting

### What should I do if a node becomes unresponsive?

If a node becomes unresponsive:

1. Check the node's logs for error messages
2. Verify network connectivity
3. Ensure sufficient system resources
4. Restart the node if necessary
5. Check for configuration issues

For persistent problems, consult the [Troubleshooting](troubleshooting.md) page.

### Where can I get help if I encounter issues?

If you encounter issues:

- Check the [Troubleshooting](troubleshooting.md) guide
- Search or ask questions on [GitHub Issues](https://github.com/permaweb/HyperBEAM/issues)
- Join the community on [Discord](https://discord.gg/V3yjzrBxPM)