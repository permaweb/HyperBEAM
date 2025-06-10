# HyperBEAM Configuration Options Reference

This document provides a comprehensive reference of all configuration options available in HyperBEAM, organized by functional category.

## Core Configuration

These options control fundamental HyperBEAM behavior.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | Integer | 8734 | HTTP API port |
| `hb_config_location` | String | "config.flat" | Path to configuration file |
| `priv_key_location` | String | "hyperbeam-key.json" | Path to operator wallet key file |
| `mode` | Atom | debug | Execution mode (debug, prod) |

## Server & Network Configuration

These options control networking behavior and HTTP settings.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `host` | String | "localhost" | Choice of remote node for non-local tasks |
| `gateway` | String | "https://arweave.net" | Default gateway |
| `bundler_ans104` | String | "https://up.arweave.net:443" | Location of ANS-104 bundler |
| `protocol` | Atom | http2 | Protocol for HTTP requests (http1, http2, http3) |
| `http_client` | Atom | gun | HTTP client to use (gun, httpc) |
| `http_connect_timeout` | Integer | 5000 | HTTP connection timeout in milliseconds |
| `http_keepalive` | Integer | 120000 | HTTP keepalive time in milliseconds |
| `http_request_send_timeout` | Integer | 60000 | HTTP request send timeout in milliseconds |
| `relay_http_client` | Atom | httpc | HTTP client for the relay device |
| `http_extra_opts` | Map | See below | Additional HTTP options |

### http_extra_opts Subcomponents

| Subcomponent | Type | Default | Description |
|--------------|------|---------|-------------|
| `force_message` | Boolean | true | Whether to force a message format |
| `store` | List | See [Storage Configuration](storage-configuration.md) | Storage backends and their configurations |
| `cache_control` | List | [<<"always">>] | Cache control directives for HTTP requests |

## Security & Identity

These options control identity and security settings.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `trusted_device_signers` | List | [] | List of device signers the node should trust |
| `trusted` | Map | {} | Trusted entities |
| `scheduler_location_ttl` | Integer | 604800000 | TTL for scheduler registration (7 days in ms) |

## Caching & Storage

These options control caching behavior. For detailed storage configuration, see [Storage Configuration](storage-configuration.md).

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `cache_lookup_hueristics` | Boolean | false | Whether to use caching heuristics or always consult the local data store |
| `access_remote_cache_for_client` | Boolean | false | Whether to access data from remote caches for client requests |
| `store_all_signed` | Boolean | true | Whether the node should store all signed messages |
| `await_inprogress` | Atom/Boolean | named | Whether to await in-progress executions (false, named, true) |
| `cache_control` | List | ["no-cache", "no-store"] | Default cache control headers |

## Execution & Processing

These options control how HyperBEAM executes messages and processes.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `scheduling_mode` | Atom | local_confirmation | When to inform recipients about scheduled assignments (aggressive, local_confirmation, remote_confirmation) |
| `compute_mode` | Atom | lazy | Whether to execute more messages after returning a result (aggressive, lazy) |
| `process_workers` | Boolean | true | Whether the node should use persistent processes |
| `client_error_strategy` | Atom | throw | What to do if a client error occurs |
| `wasm_allow_aot` | Boolean | false | Allow ahead-of-time compilation for WASM |

## Device Management

These options control how HyperBEAM manages devices.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `preloaded_devices` | Map | (see code) | Devices for the node to use, overriding resolution via ID |
| `load_remote_devices` | Boolean | false | Whether to load devices from remote signers |
| `devices` | List | [] | Additional devices to load |

## Routing & Connectivity

See [Routing Configuration](routing-configuration.md) for detailed information on routing options.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `routes` | List | See Routing docs | Routing configuration for different request patterns |

## Debug & Development

These options control debugging and development features.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `debug_print` | Boolean/List | false | Debug printing control |
| `debug_stack_depth` | Integer | 40 | Maximum stack depth for debug printing |
| `debug_print_map_line_threshold` | Integer | 30 | Maximum lines for map printing |
| `debug_print_binary_max` | Integer | 60 | Maximum binary size for debug printing |
| `debug_print_indent` | Integer | 2 | Indentation for debug printing |
| `debug_print_trace` | Atom | short | Trace mode (short, false) |
| `short_trace_len` | Integer | 5 | Length of short traces |
| `debug_hide_metadata` | Boolean | true | Whether to hide metadata in debug output |
| `debug_ids` | Boolean | false | Whether to print IDs in debug output |
| `debug_hide_priv` | Boolean | true | Whether to hide private data in debug output |
| `stack_print_prefixes` | List | ["hb", "dev", "ar"] | Prefixes for stack printing |

## Complete Option List

For the most up-to-date list of configuration options, refer to the `default_message/0` function in the `hb_opts` module in the HyperBEAM source code. 