# HyperBEAM: Your Decentralized Development Toolkit

HyperBEAM is a versatile, multi-purpose tool that serves as the primary gateway to the AO Computer. It's not a single-purpose application, but rather a powerful, extensible engine—a "Swiss Army knife"—for developers building in the decentralized ecosystem.

Its power stems from three core design principles: **Modularity**, **Composability**, and **Extensibility**. By understanding these concepts, you can unlock the full potential of HyperBEAM and build anything from simple data transformations to complex, high-performance decentralized applications.

### Modularity: A System of Devices

At its core, HyperBEAM is a modular system built on **Devices**. Each device is a specialized module responsible for a specific task. This modular architecture means you can think of HyperBEAM's functionality as a set of building blocks.

**Use Case:** Imagine you need to create a serverless API that takes a number, runs a calculation, and returns a result.

- You would use the `~wasm64@1.0` or `~lua@5.3a` **devices** to execute your calculation logic without needing to manage a server.
- If your API needs to return JSON, you can pipe the output to the `~json@1.0` **device** to ensure it's formatted correctly.

> **Ready to build an AO process?**
> The serverless compute capability is a powerful application of HyperBEAM's modular design. To learn how to create and manage AO processes with WASM or Lua, please refer to the [AO Processes Cookbook](https://cookbook.ao.arweave.net/).

### Composability: Chaining Logic with URL Paths

HyperBEAM's modular devices become even more powerful when combined. Its **pathing** routing mechanism leverages standard URLs to create powerful, composable pipelines. By constructing a URL, you can define a "path" of messages that are executed in sequence, with the output of one message becoming the input for the next.

**Use Case:** Suppose you have a token process and want to calculate the total circulating supply without making the client download and compute all balances. You can construct a single URL that:

1.  Reads the token's balance list from the process state.
2.  Pipes the list to a Lua script that sums the balances.
3.  Formats the final result as a JSON object.

The request would look something like this:

`/{process-id}~process@1.0/now/~lua@5.3a&module={module-id}/sum/serialize~json@1.0`

This path chains together the operations, returning just the computed supply in a single, efficient request.

> Learn more about [Pathing in HyperBEAM](./pathing-in-hyperbeam.md).

### Extensibility: Building Beyond the Core

HyperBEAM is not a closed system. It is designed to be extended, allowing developers to add new functionality tailored to their specific needs.

#### Build Custom Devices
You can build and deploy your own devices in Erlang to introduce entirely new, high-level functionality to the network.

**Use Case:** You could build a custom device that acts as a bridge to another blockchain's API, allowing your AO processes to interact with external systems seamlessly.

> Learn how to [Build Your Own Device](./devices/building-devices.md).

#### Achieve Raw Performance with Native Code
For the most demanding, performance-critical tasks, you can write Native Implemented Functions (NIFs) in low-level languages like C or Rust. These NIFs integrate directly with the Erlang VM, offering the highest possible performance.

**Use Case:** If you were building a sophisticated cryptographic application, you could implement a new, high-speed hashing algorithm as a NIF to ensure maximum performance and security. This "raw" extensibility provides an escape hatch for ultimate control. 