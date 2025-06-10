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

- [Configuration Methods](configuration-methods.md) - Different ways to configure HyperBEAM
- [Configuration Options](configuration-options.md) - Complete reference of all configuration options
- [Storage Configuration](storage-configuration.md) - Setting up file systems, RocksDB, and other storage backends
- [Routing Configuration](routing-configuration.md) - Configuring request routing and connectivity
- [Configuration Examples](configuration-examples.md) - Common deployment scenarios and sample configurations

## Getting Started

If you're new to HyperBEAM, you can start with a simple configuration file for basic settings:

1. Create a file named `config.flat` in your project directory
2. Add **only** simple configuration values:
   ```
   port: 10000
   priv_key_location: /path/to/wallet.json
   ```
3. Start HyperBEAM with `rebar3 shell`

HyperBEAM will automatically load your configuration and display the active settings in the startup log.

### Configuration File Limitations - IMPORTANT

The flat@1.0 format used by `config.flat` has critical limitations:

- **ONLY use simple atom values and binary values** 
- **DO NOT include maps, lists, or any complex data structures** in config.flat
- Complex configurations in config.flat will either fail to parse or silently fail to apply correctly

### Recommended Configuration Approach

For any non-trivial configuration, especially those with complex data types, use the Erlang API directly:

```bash
rebar3 shell --eval "hb:start_mainnet(#{ 
  port => 10001,
  priv_key_location => <<\"./wallet.json\">>, 
  mode => debug,
  
  http_extra_opts => #{
	force_message => true,
	store => {hb_store_fs, #{ prefix => \"local-storage\" }}
	cache_control => [<<\"always\">>]
  }
})."
```

This method allows you to use any Erlang data type and structure without limitations and is the **recommended approach for production deployments**.

## Core Configuration Priorities

When multiple configuration methods are used simultaneously, HyperBEAM follows this precedence order:

1. Command line arguments (highest priority)
2. Configuration file 
3. Environment variables
4. Default values (lowest priority)

See [Configuration Methods](configuration-methods.md) for more details on these approaches.

## Configuration Resources

- [HyperBEAM GitHub Repository](https://github.com/permaweb/HyperBEAM)
- [Quick Start Guide](../../getting-started.md)
- [API Reference](../../api-reference.md) 