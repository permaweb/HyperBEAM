!!! warning "Platform Support"
	This documentation is currently written specifically for **Ubuntu 22.04**. Support for macOS and other platforms will be added in future updates.

## Overview

HyperBEAM is a client implementation of the AO-Core protocol, written in Erlang. It enables a decentralized computing platform where programs run as independent processes, communicate via asynchronous message passing, and operate across a distributed network of nodes.

For detailed technical information about HyperBEAM's architecture and functionality, see the [HyperBEAM Overview](hyperbeam/index.md).

### What is AO-Core?

AO-Core is a protocol built to enable decentralized computations, offering a series of universal primitives. Instead of enforcing a single, monolithic architecture, AO-Core provides a framework into which any number of different computational models, encapsulated as primitive and composable devices, can be attached.

AO-Core's protocol is built upon the following primitives:

- **Hashpaths**: A mechanism for referencing locations in a program's state-space prior to execution
- **Unified data structure**: For representing program states as HTTP documents
- **Attestation protocol**: For expressing attestations of states found at particular hashpaths
- **Meta-VM**: Allowing various state transformation programs (virtual machines and computational models, expressed in the form of devices) to be executed inside the AO-Core protocol

## Installation Process Overview

Setting up HyperBEAM involves several steps:

1.  **Check System Requirements** - Ensure your hardware and operating system meet the [minimum requirements](getting-started/requirements.md).
2.  **Install System Dependencies** - Set up the necessary system packages via the [Installation Guide](getting-started/installation/index.md).
3.  **Setup & Configure HyperBEAM** - Clone, Compile, Configure, and Run [HyperBEAM itself](hyperbeam/setup.md).
4.  **Setup & Configure the Compute Unit** - Clone, Compile, Configure, and Run the [Local Compute Unit](compute-unit/setup.md).
5.  **(Optional) Verify Installation** - Follow guides to ensure everything is working. (We might need to create or link to a verification guide here, e.g., `guides/verification.md`)

### Before You Begin

Before starting the installation process, make sure to:

- Have access to a terminal/command line with administrative privileges.
- Have a stable internet connection for downloading packages.
- Allocate sufficient time (approximately 30-60 minutes for a complete setup).
- Review the [System Requirements](getting-started/requirements.md) first.

## Documentation Structure

This documentation is organized into the following main sections accessible via the top navigation:

- **[Home](.)**: This page - overview and starting points.
- **[Installation & Core](getting-started/installation/index.md)**: Detailed steps for system dependencies and HyperBEAM setup/configuration.
- **[Components](compute-unit/index.md)**: Information on related components like the Compute Unit and TEE.
- **[Usage](guides/index.md)**: Practical guides and examples for using HyperBEAM.
- **[Resources](source-code-docs/index.md)**: Source code documentation and reference materials (Troubleshooting, Glossary, FAQ).
- **[Community](contribute/guidelines.md)**: How to contribute and get involved.

## Community and Support

- **GitHub HyperBEAM**: [permaweb/HyperBEAM](https://github.com/permaweb/HyperBEAM)
- **Github Local CU**: [permaweb/local-cu](https://github.com/permaweb/local-cu)
- **Discord**: [Join the community](https://discord.gg/V3yjzrBxPM)
- **Issues**: [File a bug report](https://github.com/permaweb/HyperBEAM/issues)

## License

HyperBEAM is open-source software licensed under the [MIT License](https://github.com/permaweb/HyperBEAM/blob/main/LICENSE.md).
