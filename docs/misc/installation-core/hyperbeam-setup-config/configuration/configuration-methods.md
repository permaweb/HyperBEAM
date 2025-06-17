# HyperBEAM Configuration Methods

HyperBEAM offers multiple ways to set configuration options, each with different use cases. This document details these methods and explains when to use each one.

## Available Configuration Methods

HyperBEAM can be configured using these methods:

1. **Configuration File** - Use a flat@1.0 encoded settings file 
2. **Command Line Arguments** - Pass configuration when starting HyperBEAM **(Recommended)**
3. **Environment Variables** - Set options via environment variables

!!! warning
	The current flat@1.0 format has limitations in HyperBEAM. For now, it is recommended to use the `start_mainnet` approach for configuration. We plan to update config.flat in the future to allow for more complex configuration options.

## Using a Configuration File

The recommended way to configure HyperBEAM is through a flat@1.0 encoded settings file. By default, HyperBEAM looks for a file named `config.flat` in the current directory.

### Basic Syntax

The configuration file uses a simple `key: value` format:

```
port: 10000
cache_lookup_hueristics: true
priv_key_location: /path/to/wallet.json
```

### Limitations of flat@1.0 Format

**Important:** The flat@1.0 format has significant limitations in HyperBEAM:

- Values can only be simple atoms (like `true`, `false`, `./wallet.json`)
- **DO NOT include complex data structures** (maps, lists, tuples) in the config.flat file.
- Attempting to use complex data in config.flat may result in parsing errors or silently failing configurations.

For any configurations with complex data types, you **must** use `hb:start_mainnet/1` directly instead (see Command Line Arguments section).

### Appropriate Values for config.flat

In your config.flat file, stick to these types of values:

```
mode: debug
priv_key_location: /path/to/wallet.key
port: 10000
```

### Complex Data Types - NOT for config.flat

For complex structures like maps and lists, **do not use config.flat**. Instead, use the `hb:start_mainnet/1` approach:

```bash
rebar3 shell --eval "
  hb:start_mainnet(#{
    port => 10001,
    http_extra_opts => #{
		force_message => true,
		store => [{hb_store_fs, #{ prefix => \"local-cache\" }}, {hb_store_gateway, #{}}],
		cache_control => [<<\"always\">>]
    }
  }).
"
```

### Loading Configuration Files

HyperBEAM automatically loads `config.flat` when starting:

```bash
rebar3 shell
```

## Command Line Arguments

You can pass configuration options directly when starting HyperBEAM:

```bash
rebar3 shell --eval "hb:start_mainnet(#{ port => 10001, priv_key_location => <<\"path/to/wallet.json\">> })."
```

### Recommended Approach for Complex Configurations

Using `hb:start_mainnet/1` with a map of options is the **recommended approach** for any non-trivial configuration:

- **Full Type Support**: You can use any Erlang data type directly, not just atoms and binaries.
- **Complex Data Structures**: Maps, lists, tuples, and other complex structures work without limitations.
- **Direct Validation**: Configuration errors are caught immediately at startup.
- **Runtime Flexibility**: Options can be computed or combined with other configurations at runtime.

This approach is recommended for:

- Any configurations with maps, lists, or other complex data structures
- Routing and storage configurations
- Testing different configurations without editing files
- Production deployments where reliability is critical

## Environment Variables

HyperBEAM recognizes these environment variables:

| Variable | Corresponding Option | Example |
|----------|----------------------|---------|
| `HB_PORT` | `port` | `export HB_PORT=9001` |
| `HB_KEY` | `priv_key_location` | `export HB_KEY=/path/to/wallet.key` |
| `HB_CONFIG` | `hb_config_location` | `export HB_CONFIG=config.flat` |
| `HB_STORE` | `store` | `export HB_STORE=/path/to/store` |
| `HB_MODE` | `mode` | `export HB_MODE=debug` |
| `HB_PRINT` | `debug_print` | `export HB_PRINT=dev_meta` |

## Configuration Precedence

When multiple configuration methods are used, HyperBEAM follows this precedence order:

1. Command line arguments (highest priority)
2. Configuration file 
3. Environment variables
4. Default values (lowest priority)
