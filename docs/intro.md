# AO

Over the years, blockchain technology and decentralized systems have advanced significantly, yet they still face several fundamental challenges. Many traditional smart contract platforms struggle with scalability, meaning they cannot process many transactions quickly. They also have high costs for executing transactions and often come with limitations on how developers can program complex applications.

Decentralized storage solutions have made it possible to store data permanently without relying on centralized servers. 
However, these solutions primarily focus on storing data, not executing programs. 
This means that while they provide reliable storage, they lack the computational layer necessary to create fully autonomous applications that can operate without ongoing human intervention.

AO is a decentralized computing system built on Arweave, which provides permanent storage. 
Unlike traditional blockchain platforms, AO is designed as a decentralized supercomputer capable of running thousands of processes in parallel. 
This creates what is known as a hyper-parallel processing engine, allowing computations to happen at a much larger scale and with greater efficiency.

AO is built to support autonomous agents—self-executing programs that run independently. These agents can respond to events, process data, and execute logic without requiring centralized control. Because AO combines Arweave’s permanent storage with a fully decentralized execution environment, it overcomes many of the limitations found in traditional blockchains, making it more scalable, cost-efficient, and resilient for decentralized applications.

Decentralized systems like Bitcoin have introduced strong guarantees of trustlessness and permissionlessness, but they are not absolute. Users still need to trust that the majority of mining power follows the rules and that cryptographic security remains intact.
AO introduces a new approach where users do not need to trust any central authority or even other participants in the network. Instead, it relies entirely on cryptographic proofs and decentralized execution. The Converge Protocol, which powers AO, is built on two key principles:
- Trustlessness – Anyone can participate in the network without having to trust other participants to act honestly. Everything is verifiable and executed based on predefined rules.
- Permissionlessness – No entity, individual, or group has the power to block another participant from accessing or using the network. Everyone has equal access to computation and storage resources.

Unlike rigid blockchain models that enforce the same rules for all users, AO and Converge provide flexibility by allowing participants to choose from different levels of security, scalability, and computational resources. This way, users can collaborate while still maintaining control over their trade-offs.

At the heart of AO is a unique concept: executable and isolated processes. These processes are self-contained pieces of code that are permanently stored on Arweave. Once stored, they can be executed at any time to produce specific results.

One of the key advantages of AO is that multiple processes can run in parallel, independently of one another. Unlike traditional blockchains that require every node to process every transaction, AO allows computations to happen separately and simultaneously.

Each process in AO can:
- Read and write data on Arweave.
- Act as an autonomous agent, continuously responding to new events.
- Communicate with other processes and nodes using messages.

Instead of relying on a global consensus mechanism like Bitcoin or Ethereum, AO uses a more modular approach where messages trigger processes. These messages function like transactions or function calls in traditional systems, enabling communication and execution between nodes.
The execution of these processes is supported by modular layers, including transmission, scheduling, and execution components. This modular structure allows the network to scale efficiently and enables what is known as hyper-parallel computing, where thousands of computations can run simultaneously without interference.

## Converge Protocol: A new way to Process Data

The Converge Protocol is a foundational system that makes it easier to process and manage data across decentralized networks. It is designed to work with standard web technologies like HTTP, making it more accessible and flexible than traditional blockchain-based architectures.

At its core, Converge helps users create, track, and combine pieces of information efficiently by organizing them into three main components:
- Messages – The smallest units of data and computation.
- Devices – Systems that process and store messages.
- Paths – The structures that connect and organize messages over time.

By keeping messages small and lightweight, Converge reduces storage and computation costs. At the same time, it ensures scalability by distributing computations across multiple machines, increasing both speed and efficiency. Security is guaranteed through cryptographic proofs, ensuring that all data remains transparent and verifiable.

### Messages: The Building Blocks of Converge

At the heart of Converge’s decentralized system is the concept of Messages. Messages are the smallest units of data and computation, acting as the primary way that processes communicate and execute functions.

Messages are processed by devices, stored permanently, and can be referenced at any time. This eliminates the need for centralized servers and ensures that every computation remains verifiable and traceable.

Unlike traditional blockchains, where transactions are processed in a strict order, Converge allows messages to be processed in parallel. This is a critical innovation that enables AO’s hyper-parallel computing model.
Because messages are lightweight and cryptographically linked, they can be distributed across many devices without causing congestion. This makes Converge much more scalable than traditional blockchain-based smart contract platforms.

Messages don’t just exist in isolation. Instead, they form a computation graph, where new messages reference previous ones to build complex logic and decision-making processes.
For example, one message might contain raw data, while another message applies a transformation to that data. A third message might then combine multiple previous messages to create a final result.
This graph-based structure eliminates the need for sequential processing and allows AO to operate in a truly decentralized and parallel manner.

### Devices: Systems That Process Messages

In the Converge Protocol, a Device is a system responsible for processing and interpreting messages. Devices determine how messages are executed and play a key role in making AO a modular and flexible decentralized computing framework.

Unlike traditional blockchains, where every node processes every transaction, Converge distributes tasks among different devices. This eliminates bottlenecks, improves efficiency, and enables AO to support a wide variety of use cases without enforcing a single computation model.

Devices are not fixed; they can be added or upgraded over time. Each node in the network can select which devices to support based on their hardware capabilities and desired functionality. This decentralized approach allows nodes to contribute computing power and storage in a way that best suits their resources.

Each device has specific logic that defines:
- How a message is processed. Some devices might execute code, while others might store or relay information.
- What kind of data or computation it handles. For example, some devices execute WebAssembly code, while others schedule tasks.
- How it interacts with other devices. Devices can communicate and depend on each other to perform more complex tasks.

For example, a high-performance node with a powerful CPU may choose to run compute-intensive devices, while a lightweight node might only handle relay or scheduling tasks. This flexibility ensures scalability and prevents wasted resources.

### Paths: Tracing the history of computation

A Path in Converge is a way to link and structure messages over time, creating an organized and verifiable history of computations.

Paths ensure that every transformation applied to a message is traceable, reproducible, and secure. Instead of storing each version of data separately, Converge keeps only the essential transformation steps, reducing storage costs while maintaining a full history of how data evolved.

Each Path generates a HashPath, a cryptographic fingerprint that records every change applied to a message. This ensures once a message is recorded in a HashPath, it cannot be altered without detection.
Instead of storing every version of a message, Converge saves only the essential transformation steps. At the end, large-scale distributed computing becomes possible because only the necessary parts of a message are retrieved and processed, reducing network congestion.

Paths function like version control systems (similar to Git), allowing Converge to maintain a structured and efficient way to track changes without wasting resources.

## HyperBEAM

HyperBEAM is the main implementation of the Converge protocol, written in Erlang, and serves as the backbone of AO’s decentralized operating system. By using Erlang and its BEAM runtime, HyperBEAM benefits from a battle-tested ecosystem designed for fault tolerance and distributed computing. 
This architecture ensures key features such as process isolation, message-passing, and efficient task scheduling, all of which align with the core principles of AO. Erlang’s lightweight processes and actor-based concurrency model mirror AO’s execution requirements, where independent agents must operate in parallel, communicate asynchronously, and recover from failures automatically—ensuring a robust, decentralized computing environment.

One of HyperBEAM’s most important capabilities is its abstraction and modular design, which allows programs to execute independently of the underlying hardware. HyperBEAM provides a hardware-agnostic execution layer, enabling computations to run across a distributed network of nodes. 
This means that programs can function seamlessly across various infrastructures, ensuring scalability and resilience. By allowing nodes to dynamically select and execute different devices, HyperBEAM ensures adaptability while maintaining the robustness needed for a truly fault-tolerant and decentralized system.

HyperBEAM plays a crucial role in coordinating node operations, allowing individual operators to contribute their machine’s resources to the network. Instead of enforcing a rigid infrastructure, AO enables flexible participation, where each node can expose specific services by running designated devices. 
These devices act as modular execution units, providing different functionalities such as processing, communication, scheduling, and interaction with external networks. This decentralized approach ensures that AO can support a diverse set of computational needs, from handling autonomous agents to executing smart contract logic efficiently.

To facilitate these operations, HyperBEAM includes several preloaded devices that serve distinct purposes. 
- Then `~meta` device manages node configuration, defining hardware specifications and supported execution environments. 
- Then `~relay` device ensures seamless communication both within the network and with external systems, acting as the bridge between AO’s decentralized computation and the broader digital ecosystem. 
- Then `~process` device is responsible for maintaining persistent and shared executions, allowing long-running computations to be coordinated across multiple nodes. 
- Then `~scheduling` device ensures deterministic execution ordering by leveraging hashpath-based computations, guaranteeing consistency across distributed processes. 
- Then `~wasm64` device enables nodes to execute WebAssembly code, expanding the range of supported applications and ensuring compatibility with lightweight, portable execution environments.
etc.

Through this modular approach, HyperBEAM enables AO to function as a highly flexible and extensible decentralized supercomputer, where computation is not restricted to a single paradigm but can evolve dynamically based on the network’s needs.


