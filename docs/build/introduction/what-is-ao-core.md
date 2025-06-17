# What is AO-Core?

AO-Core is the foundational protocol underpinning the [AO Computer](https://ao.arweave.net). It defines a minimal, generalized model for decentralized computation built around standard web technologies like HTTP. Think of it as a way to interpret the [Arweave permaweb](https://arweave.org) not just as static storage, but as a dynamic, programmable, and infinitely scalable computing environment.

## Core Concepts

AO-Core revolves around three fundamental components:

<div class="grid-docs">

<div class="grid-docs-card">
<p><strong>Messages</strong></p>
<div class="grid-docs-card-fig">
<svg width="73" height="26" viewBox="0 0 73 26" fill="none" xmlns="http://www.w3.org/2000/svg">
<line x1="0.5" x2="0.5" y2="26" stroke="#5F5F5F"/>
<line x1="72.5" x2="72.5" y2="26" stroke="#5F5F5F"/>
<path d="M9 13H21.0416" stroke="#5F5F5F" stroke-width="2" stroke-linecap="square"/>
<path d="M31.0416 13H43.0832" stroke="#5F5F5F" stroke-width="2" stroke-linecap="square"/>
<path d="M53.0832 13H65.1248" stroke="#5F5F5F" stroke-width="2" stroke-linecap="square"/>
</svg>
</div>
<p>The smallest units of data and computation. Messages can be simple data blobs or maps of named functions.</p> <p>They are the primary means of communication and triggering execution within the system, processed independently or combined to build complex logic.</p><p>Messages are cryptographically linked, forming a verifiable computation graph.</p>
</div>

<div class="grid-docs-card">
<p><strong>Devices</strong></p>

<div class="grid-docs-card-fig">
<svg width="85" height="68" viewBox="0 0 85 68" fill="none" xmlns="http://www.w3.org/2000/svg">
<path d="M54 26H72V50" stroke="#6F6F6F" stroke-width="1" stroke-linejoin="round" stroke-dasharray="1 1"/>
<path d="M25 1H43V25" stroke="#6F6F6F" stroke-width="1" stroke-linejoin="round" stroke-dasharray="1 1"/>
<rect x="0.238661" y="0.625025" width="25.4567" height="18.2528" rx="0.954642" fill="white" stroke="#6F6F6F" stroke-width="1"/>
<rect x="29.8775" y="25.0153" width="25.4567" height="18.2528" rx="0.954642" fill="white" stroke="#6F6F6F" stroke-width="1"/>
<rect x="59.2387" y="49.2387" width="25.4567" height="18.2528" rx="0.954642" fill="white" stroke="#6F6F6F" stroke-width="1"/>
</svg>
</div>
<div>
<p>Modules responsible for interpreting and processing messages. </p> <p>Each device defines specific logic for how messages are handled (e.g., executing WASM, storing data, relaying information). </p><p>This modular design allows nodes to specialize, making the system highly flexible and extensible.</p>
</div>
</div>

<div class="grid-docs-card">
<p><strong>Paths</strong></p>
<div class="grid-docs-card-fig">
<svg width="66" height="66" viewBox="0 0 66 66" fill="none" xmlns="http://www.w3.org/2000/svg">
<path d="M15 31H28" stroke="#5F5F5F" stroke-width="1" stroke-dasharray="1 1"/>
<path d="M35 51V38" stroke="#5F5F5F" stroke-width="1" stroke-dasharray="1 1"/>
<path d="M42 24.1924L51.1924 15" stroke="#5F5F5F" stroke-width="1" stroke-dasharray="1 1"/>
<rect x="0.8" y="23.8" width="14.4" height="14.4" fill="white" stroke="#5F5F5F" stroke-width="1"/>
<rect x="27.8" y="23.8" width="14.4" height="14.4" fill="white" stroke="#5F5F5F" stroke-width="1"/>
<rect x="27.8" y="50.8" width="14.4" height="14.4" fill="white" stroke="#5F5F5F" stroke-width="1"/>
<rect x="50.8" y="0.8" width="14.4" height="14.4" fill="white" stroke="#5F5F5F" stroke-width="1"/>
</svg>
</div>
<p>Structures that link messages over time, creating a verifiable history of computations. </p> <p>Paths allow users to navigate the computation graph and access specific states or results. </p> <p>They leverage <code>HashPaths</code>, cryptographic fingerprints representing the sequence of operations leading to a specific message state, ensuring traceability and integrity.</p>
</div>

</div>

## How it works

AO-Core protocol defines a programming environment that makes it easy to compute on data using standard web technologies like HTTP. 

In AO-Core, everything is a message. Messages are the smallest units of data and computation, and they can be processed, stored, and combined by devices, which define how those messages behave. You can compute on any message by calling keys by name, and the way those keys resolve is determined by the message’s device. By default, this is [message@1.0](../devices/message-at-1-0.md), which simply returns the literal keys from the message itself. Learn more about [how devices work ](../devices/hyperbeam-devices.md)

The output of one message can be used as the input of another. Your computation path in AO-Core is like a pipeline of request messages to execute in a series, each describing an action or transformation to apply. The output you get back being the result of the final request message in your pipeline. 

You can set specific keys in a message using & in the path (for example, &key=value), and any headers or query parameters after a ? apply to all messages in the pipeline. The ~x@y syntax lets you apply a request to a different device, giving you the flexibility to define custom computation logic or storage rules. The final message in this pipeline is returned over HTTP, cryptographically signed against the hashpath that generated it, ensuring transparency, security, and verifiability of the entire computation. Learn more about [how paths works](../pathing-in-hyperbeam.md)

## Key Principles

*   **Minimalism:** AO-Core provides the simplest possible representation of data and computation, avoiding prescriptive consensus mechanisms or specific VM requirements.
*   **HTTP Native:** Designed for compatibility with HTTP protocols, making it accessible via standard web tools and infrastructure.
*   **Scalability:** By allowing parallel message processing and modular device execution, AO-Core enables hyper-parallel computing, overcoming the limitations of traditional sequential blockchains.
*   **Permissionlessness & Trustlessness:** While AO-Core itself is minimal, it provides the framework upon which higher-level protocols like AO can build systems that allow anyone to participate (`permissionlessness`) without needing to trust intermediaries (`trustlessness`). Users can choose their desired security and performance trade-offs.

AO-Core transforms the permanent data storage of Arweave into a global, shared computation space, enabling the creation of complex, autonomous, and scalable decentralized applications.

*See also: [The AO-Core Protocol Specification (Draft)](../../misc/ao-core-protocol.md)* 