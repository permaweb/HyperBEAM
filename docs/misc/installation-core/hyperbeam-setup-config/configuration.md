# HyperBEAM Configuration

HyperBEAM can be configured using a variety of methods and options. This document provides an overview of the configuration system and links to specialized configuration topics.

## Configuration System Overview

HyperBEAM is a highly configurable node runtime for decentralized applications. Its configuration system allows operators to:

- Define connection parameters
- Set up storage backends
- Configure routing rules
- Control execution behavior
- Optimize performance characteristics
- Enable debugging features

## Configuration Documentation Sections

For detailed information about specific aspects of HyperBEAM configuration, please refer to the following documentation:

- [Configuration Methods](./configuration/configuration-methods.md) - Different ways to configure HyperBEAM
- [Configuration Options](./configuration/configuration-options.md) - Complete reference of all configuration options
- [Storage Configuration](./configuration/storage-configuration.md) - Setting up file systems, RocksDB, and other storage backends
- [Routing Configuration](./configuration/routing-configuration.md) - Configuring request routing and connectivity
- [Configuration Examples](./configuration/configuration-examples.md) - Common deployment scenarios and sample configurations

## Getting Started

If you're new to HyperBEAM, we recommend starting with a basic configuration file:

1. Create a file named `config.flat` in your project directory
2. Add basic configuration:
   ```
   port: 10000
   priv-key-location: /path/to/wallet.key
   mode: debug
   ```
3. Start HyperBEAM with `rebar3 shell`

HyperBEAM will automatically load your configuration and display the active settings in the startup log.

## Core Configuration Priorities

When multiple configuration methods are used simultaneously, HyperBEAM follows this precedence order:

1. Environment variables (highest precedence)
2. Runtime configuration via HTTP
3. Command line arguments 
4. Configuration file
5. Default values (lowest precedence)

See [Configuration Methods](configuration-methods.md) for more details on these approaches.

## Configuration Resources

- [HyperBEAM GitHub Repository](https://github.com/hyperbeam-core/hyperbeam)
- [Quick Start Guide](../getting-started.md)
- [API Reference](../api-reference.md)
