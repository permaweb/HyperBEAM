# What is HyperBEAM?

<video class="theme-invert-video" src="https://arweave.net/pc73dj9tZtj7AOeIKBGiiOm5ta13FYXzgsqWSePAxiM" style="width: 100%; height: auto; display: block;" autoplay="" muted="" playsinline="" loop="" controlslist="nodownload nofullscreen noremoteplayback" disablepictureinpicture="" preload="auto"></video>

HyperBEAM is the primary, production-ready implementation of the [AO-Core protocol](/introduction/what-is-ao-core.md), built on the robust Erlang/OTP framework. It serves as a decentralized operating system, powering the [AO Computer](https://ao.arweave.net)—a scalable, trust-minimized, distributed supercomputer built on permanent storage of [Arweave](https://arweave.org).

## **Implementing AO-Core**

HyperBEAM transforms the abstract concepts of AO-Core—Messages, Devices, and Paths—into a concrete, operational system. It provides the runtime environment and essential services to execute these computations across a network of distributed nodes.

<div class="core-concepts-flex">
<img class="core-concepts-fig messages" src="/docs/assets/images/aosvg1.svg" alt="" loading="lazy">
<div class="core-concepts-column">
<p class="core-concept-header-messages"><b>Messages</b></p>
<span class="core-concept-subtitle">Modular Data Packets</span>
<p class="core-concept-copy">In HyperBEAM, every interaction within the AO Computer is handled as a <b>message</b>. A message is a binary item or a map of functions. These cryptographically-linked data units are the foundation for communication, allowing processes to trigger computations, query state, and transfer value. HyperBEAM nodes are responsible for routing and processing these messages according to the rules of the AO-Core protocol.</p>
</div>
</div>

<div class="core-concepts-flex">
<img class="core-concepts-fig devices" src="/docs/assets/images/aosvg2.svg" alt="" loading="lazy">
<div class="core-concepts-column">
<p class="core-concept-header-devices"><b>Devices</b></p>
<span class="core-concept-subtitle">Extensible Execution Engines</span>
<p class="core-concept-copy">HyperBEAM introduces a uniquely modular architecture centered around <b>Devices</b>. These pluggable components are Erlang modules that define specific computational logic—like running WASM, managing state, or relaying data—allowing for unprecedented flexibility. This design allows developers to extend the system by creating custom Devices to fit their specific computational needs.</p>
</div>
</div>

<div class="core-concepts-flex">
<img class="core-concepts-fig paths" src="/docs/assets/images/aosvg3.svg" alt="" loading="lazy">
<div class="core-concepts-column">
<p class="core-concept-header-paths"><b>Paths</b></p>
<span class="core-concept-subtitle">Composable Pipelines</span>
<p class="core-concept-copy">HyperBEAM exposes a powerful HTTP API that uses structured URL patterns to interact with processes and data. This <b>pathing mechanism</b> allows developers to create verifiable data pipelines, composing functionality from multiple devices into a single, atomic request. The URL bar effectively becomes a command-line interface for AO's trustless compute environment.</p>
</div>
</div>

## A Robust and Scalable Foundation

Built on the Erlang/OTP framework, HyperBEAM provides a robust and secure foundation that leverages the BEAM virtual machine for exceptional concurrency, fault tolerance, and scalability. This abstracts away underlying hardware, allowing diverse nodes to contribute resources without compatibility issues. The system governs how nodes coordinate and interact, forming a decentralized network that is resilient and permissionless.
