# Developer FAQ

This page answers common questions about building applications and processes on HyperBEAM.

### What can I build with HyperBEAM?

You can build a wide range of applications, including:

- Decentralized applications (dApps)
- Distributed computation systems
- Peer-to-peer services
- Resilient microservices
- IoT device networks
- Decentralized storage solutions

### What is the current focus or phase of HyperBEAM development?

The initial development phase focuses on integrating AO processes more deeply with HyperBEAM. A key part of this is phasing out the reliance on traditional "dryrun" simulations for reading process state. Instead, processes are encouraged to use the [~patch@1.0 device](../../../build/devices/source-code/dev_patch.html) to expose specific parts of their state directly via GET requests. This allows for more efficient and direct state access, particularly for web interfaces and external integrations. You can learn more about this mechanism in the [Exposing Process State with the Patch Device](../../../build/migrating-from-legacynet.html#exposing-process-state-with-the-patch-device) guide.

### What is the difference between HyperBEAM and Compute Unit?

- **HyperBEAM**: The Erlang-based node software that handles message routing, process management, and device coordination.
- **Compute Unit (CU)**: A NodeJS implementation that executes WebAssembly modules and handles computational tasks.

Together, these components form a complete execution environment for AO processes.

### What programming languages can I use with HyperBEAM?

You can use any programming language that compiles to WebAssembly (WASM) for creating modules that run on the Compute Unit. This includes languages like:

- Lua
- Rust
- C/C++
- And many others with WebAssembly support

### How do I debug processes running in HyperBEAM?

Debugging processes in HyperBEAM can be done through:

1. Logging messages to the system log (`DEBUG=HB_PRINT rebar3 shell`)
2. Monitoring process state and message flow
3. Inspecting memory usage and performance metrics

### Where can I get help if I encounter issues?

If you encounter issues:

- Check the [Troubleshooting](troubleshooting.md) guide
- Search or ask questions on [GitHub Issues](https://github.com/permaweb/HyperBEAM/issues)
- Join the community on [Discord](https://discord.gg/V3yjzrBxPM) 