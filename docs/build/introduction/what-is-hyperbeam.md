# What is HyperBEAM?

<div style="width: 100%; max-width: 100vw; margin-bottom: 2em;">
  <video
    class="theme-invert-video"
    src="https://arweave.net/CAMmkYu41bEAmxpd8H7hhjj8WbH_Uxi_s4NGgE5DauE"
    style="width: 100%; height: auto; display: block;"
    autoplay
    muted
    playsinline
    loop
    controlslist="nodownload nofullscreen noremoteplayback"
    disablepictureinpicture
    preload="auto"
  ></video>
</div>

HyperBEAM is the primary, production-ready implementation of the [AO-Core protocol](./what-is-ao-core.md), built on the robust Erlang/OTP framework. It serves as a decentralized operating system, powering the [AO Computer](https://ao.arweave.net)—a scalable, trust-minimized, distributed supercomputer built on permanent storage of [Arweave](https://arweave.org).

## **Implementing AO-Core**

HyperBEAM transforms the abstract concepts of AO-Core—Messages, Devices, and Paths—into a concrete, operational system. It provides the runtime environment and essential services to execute these computations across a network of distributed nodes.

### Messages: Modular Data Packets
In HyperBEAM, every interaction within the AO Computer is handled as a **message**. A message is a binary item or a map of functions. These cryptographically-linked data units are the foundation for communication, allowing processes to trigger computations, query state, and transfer value. HyperBEAM nodes are responsible for routing and processing these messages according to the rules of the AO-Core protocol.

### Devices: Extensible Execution Engines
HyperBEAM introduces a uniquely modular architecture centered around **[Devices](../devices/hyperbeam-devices.md)**. These pluggable components are Erlang modules that define specific computational logic—like running WASM, managing state, or relaying data—allowing for unprecedented flexibility. This design allows developers to extend the system by creating custom Devices to fit their specific computational needs.

### Paths: Composable Pipelines
HyperBEAM exposes a powerful HTTP API that uses structured URL patterns to interact with processes and data. This **[pathing mechanism](../pathing-in-hyperbeam.md)** allows developers to create verifiable data pipelines, composing functionality from multiple devices into a single, atomic request. The URL bar effectively becomes a command-line interface for AO's trustless compute environment.

## A Robust and Scalable Foundation

Built on the Erlang/OTP framework, HyperBEAM provides a robust and secure foundation that leverages the BEAM virtual machine for exceptional concurrency, fault tolerance, and scalability. This abstracts away underlying hardware, allowing diverse nodes to contribute resources without compatibility issues. The system governs how nodes coordinate and interact, forming a decentralized network that is resilient and permissionless.