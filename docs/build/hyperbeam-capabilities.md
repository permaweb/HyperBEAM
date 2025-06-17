# HyperBEAM: Your Decentralized Development Toolkit

HyperBEAM is a versatile, multi-purpose tool that serves as the primary gateway to the AO Computer. It's not a single-purpose application, but rather a powerful, extensible engine—a "Swiss Army knife"—for developers building in the decentralized ecosystem.

Designed to be **modular**, **composable**, and **extensible**, HyperBEAM lets you build anything from simple data transformations to complex, high-performance decentralized applications.

## **Thinking in HyperBEAM**

While [AO-Core](./introduction/what-is-ao-core.md) establishes the foundational concepts of Messages, Devices, and Paths, building on HyperBEAM can be simplified to four key principles:

1.  **Everything is a [message](./devices/message-at-1-0.md).** You can compute on any message by calling its keys by name. The [`device`](./devices/hyperbeam-devices.md) specified in the message determines how these keys are resolved. The default device, `message@1.0`, resolves keys to their literal values within the message.

2.  **[Paths](./pathing-in-hyperbeam.md) are pipelines of messages.** A path defines a sequence of 'request' messages to be executed. You can set a key in a message directly within the path using the `&key=value` syntax. Headers and parameters added after a `?` are applied to all messages in the pipeline.

3.  **Device-specific requests with [`~x@y`](./pathing-in-hyperbeam.md).** The `~x@y` syntax allows you to apply a request as if the base message had a different `device`. This provides a powerful way to execute messages using specific compute or storage logic defined by a device.

4.  **Signed responses over HTTP.** The final message in a pipeline is returned as an HTTP response. This response is signed against the [`hashpath`](./pathing-in-hyperbeam.md) that generated it, ensuring the integrity and verifiability of the computation.

> **Ready to build an AO process?**
> The serverless compute capability is a powerful application of HyperBEAM's modular design. To learn how to create and manage AO processes with WASM or Lua, please refer to the [AO Processes Cookbook](https://cookbook.ao.arweave.net/).

### Modularity: A System of Devices

At its core, HyperBEAM is a modular system built on **Devices**. Each device is a specialized module responsible for a specific task. This modular architecture means you can think of HyperBEAM's functionality as a set of building blocks.

**Use Case:** Imagine you need to create a serverless API that takes a number, runs a calculation, and returns a result.

- You would use the `~wasm64@1.0` or `~lua@5.3a` **devices** to execute your calculation logic without needing to manage a server.
- If your API needs to return JSON, you can pipe the output to the `~json@1.0` **device** to ensure it's formatted correctly.

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