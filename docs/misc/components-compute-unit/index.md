# Compute Unit Overview

The ao Compute Unit (CU) is a spec-compliant implementation built with NodeJS that serves as the computational processing component in the ao ecosystem, handling WASM execution and state management.

## What is the Compute Unit?

The Compute Unit is responsible for executing WebAssembly modules and handling computational tasks within the ao ecosystem. It works in conjunction with HyperBEAM but runs as a separate process, providing the actual execution environment for ao processes.

Key responsibilities include:
- Executing WebAssembly modules
- Managing process memory and state
- Handling process checkpointing
- Processing evaluation requests from HyperBEAM

## Architecture

The Compute Unit follows a Ports and Adapters Architecture (also known as Hexagonal Architecture):

- **Business Logic**: Located in `src/domain`, this contains all core functionality
- **Driven Adapters**: Located in `effects`, these implement contracts for various platforms
- **Driving Adapter**: Also in `effects`, this exposes the public API

This architecture separates business logic from external interfaces, making the system more maintainable and testable.

## Project Structure

- **domain**: Contains all business logic and public APIs
	- **api**: Implements public interfaces
	- **lib**: Contains business logic components
	- **dal.js**: Defines contracts for driven adapters

- **effects**: Contains implementations of external interfaces
	- **ao-http**: Exposes the HTTP API consumed by other ao units

## Technical Requirements

The Compute Unit requires:

- Node.ja
- Access to local file system for state persistence
- Network access to communicate with HyperBEAM
- An Arweave wallet for identity

### System Requirements

The ao Compute Unit is a stateless application that can be deployed to any containerized environment using its Dockerfile or directly with Node.js. It requires:

- A containerization environment or Node.js runtime
- A filesystem to store files and an embedded database
- Ingress capability from the Internet
- Egress capability to other ao units and the Internet

## Key Features

- **WASM Execution**: Executes WebAssembly modules for ao processes
- **State Management**: Maintains process memory and state
- **Checkpointing**: Creates and manages checkpoints of process state
- **Configurable Limits**: Memory and compute limits can be adjusted
- **Event-Driven Architecture**: Processes messages asynchronously
- **Robust Logging**: Comprehensive logging with configurable levels

## Next Steps

- [Setup](setup.md): Learn how to install and run the Compute Unit
- [Configuration](configuration.md): Understand available configuration options
- [API Reference](api.md): Explore the Compute Unit's API 