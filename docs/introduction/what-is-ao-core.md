# What is AO-Core?

AO-Core is the foundational protocol underpinning the [AO Computer](https://ao.arweave.net). It defines a minimal, generalized model for decentralized computation built around standard web technologies like HTTP. Think of it as a way to interpret the Arweave permaweb not just as static storage, but as a dynamic, programmable, and infinitely scalable computing environment.

## Core Concepts

AO-Core revolves around three fundamental components:

1.  **Messages:** The smallest units of data and computation. Messages can be simple data blobs or maps of named functions. They are the primary means of communication and triggering execution within the system. Messages are cryptographically linked, forming a verifiable computation graph.
2.  **Devices:** Modules responsible for interpreting and processing messages. Each device defines specific logic for how messages are handled (e.g., executing WASM, storing data, relaying information). This modular design allows nodes to specialize and the system to be highly extensible.
3.  **Paths:** Structures that link messages over time, creating a verifiable history of computations. Paths allow users to navigate the computation graph and access specific states or results. They leverage `HashPaths`, cryptographic fingerprints representing the sequence of operations leading to a specific message state, ensuring traceability and integrity.

## Key Principles

*   **Minimalism:** AO-Core provides the simplest possible representation of data and computation, avoiding prescriptive consensus mechanisms or specific VM requirements.
*   **HTTP Native:** Designed for compatibility with HTTP protocols, making it accessible via standard web tools and infrastructure.
*   **Scalability:** By allowing parallel message processing and modular device execution, AO-Core enables hyper-parallel computing, overcoming the limitations of traditional sequential blockchains.
*   **Permissionlessness & Trustlessness:** While AO-Core itself is minimal, it provides the framework upon which higher-level protocols like AO can build systems that allow anyone to participate (`permissionlessness`) without needing to trust intermediaries (`trustlessness`). Users can choose their desired security and performance trade-offs.

AO-Core transforms the permanent data storage of Arweave into a global, shared computation space, enabling the creation of complex, autonomous, and scalable decentralized applications.

<!-- *See also: [The AO-Core Protocol Specification (Draft)](https://github.com/permaweb/ao-core/blob/main/spec.md)*  -->