# Configuring Your HyperBEAM Node

This guide details the various ways to configure your HyperBEAM node's behavior, including ports, storage, keys, and logging.

## Configuration (`config.json`)

The primary way to configure your HyperBEAM node is through a `config.json` file located in the node's working directory or specified by the `HB_CONFIG` environment variable.

### Flat config file

Another possibility is to use `config.flat` that uses a simple `Key: Value` format.

**Example `config.flat`:**

```
% Set the HTTP port
port: 8080

% Specify the Arweave key file
priv_key_location: /path/to/your/wallet.json

% Maps can be used with forward dash (/)
default_store/lmdb/ao-types: store-module=atom
default_store/lmdb/store-module: hb_store_lmdb
default_store/lmdb/name: /tmp/store

% Lists can be used with dot (.) and sequential integer key map
store/ao-types: .=list
store/1/ao-types: store-module=atom
store/1/store-module: hb_store_lmdb
store/1/name: /tmp/store

store/2/ao-types: store-module=atom
store/2/store-module: hb_store_s3
store/2/bucket: hb-s3
store/2/priv_access_key_id: minioadmin
store/2/priv_secret_access_key: minioadmin
store/2/endpoint: http://localhost:9000
store/2/force_path_style: true
store/2/region: us-east-1
```

Below is a reference of commonly used configuration keys. Remember that `config.flat` only supports the following value types (Atoms, Strings, Integers, Booleans, Maps and List).

### Core Configuration

These options control fundamental HyperBEAM behavior.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `port` | Integer | 8734 | HTTP API port |
| `hb_config_location` | String | "config.flat" | Path to configuration file |
| `priv_key_location` | String | "hyperbeam-key.json" | Path to operator wallet key file |
| `mode` | Atom | debug | Execution mode (debug, prod) |

### Server & Network Configuration

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
<!-- Complex options like http_extra_opts are omitted as they are not suitable for config.flat -->

### Security & Identity

These options control identity and security settings.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `scheduler_location_ttl` | Integer | 604800000 | TTL for scheduler registration (7 days in ms) |
<!-- Complex options like trusted_device_signers, trusted are omitted -->

### Caching & Storage

These options control caching behavior. **Note:** Detailed storage configuration (`store` option) involves complex data structures and cannot be set via `config.flat`.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `cache_lookup_hueristics` | Boolean | false | Whether to use caching heuristics or always consult the local data store |
| `access_remote_cache_for_client` | Boolean | false | Whether to access data from remote caches for client requests |
| `store_all_signed` | Boolean | true | Whether the node should store all signed messages |
| `await_inprogress` | Atom/Boolean | named | Whether to await in-progress executions (false, named, true) |
<!-- Complex options like cache_control are omitted -->

### Execution & Processing

These options control how HyperBEAM executes messages and processes.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `scheduling_mode` | Atom | local_confirmation | When to inform recipients about scheduled assignments (aggressive, local_confirmation, remote_confirmation) |
| `compute_mode` | Atom | lazy | Whether to execute more messages after returning a result (aggressive, lazy) |
| `process_workers` | Boolean | true | Whether the node should use persistent processes |
| `client_error_strategy` | Atom | throw | What to do if a client error occurs |
| `wasm_allow_aot` | Boolean | false | Allow ahead-of-time compilation for WASM |

### Device Management

These options control how HyperBEAM manages devices.

Remote device loading is enabled by configuring `trusted_device_signers`;
omit it or set it to `[]` to disable remote signer lookup.
<!-- Complex options like preloaded-store, devices are omitted -->

### Debug & Development

These options control debugging and development features.

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `debug_stack_depth` | Integer | 40 | Maximum stack depth for debug printing |
| `debug_print_map_line_threshold` | Integer | 30 | Maximum lines for map printing |
| `debug_print_binary_max` | Integer | 60 | Maximum binary size for debug printing |
| `debug_print_indent` | Integer | 2 | Indentation for debug printing |
| `debug_print_trace` | Atom | short | Trace mode (short, false) |
| `short_trace_len` | Integer | 5 | Length of short traces |
| `debug_hide_metadata` | Boolean | true | Whether to hide metadata in debug output |
| `debug_ids` | Boolean | false | Whether to print IDs in debug output |
| `debug_hide_priv` | Boolean | true | Whether to hide private data in debug output |
<!-- Complex options like debug_print, stack_print_prefixes are omitted -->

**Note:** For the *absolute complete* and most up-to-date list, including complex options not suitable for `config.flat`, refer to the `default_message/0` function in the `hb_opts` module source code.

## Overrides (Environment Variables & Args)

You can override settings from `config.flat` or provide values if the file is missing using environment variables or command-line arguments.

**Using Environment Variables:**

Environment variables typically use an `HB_` prefix followed by the configuration key in uppercase.

*   **`HB_PORT=<port_number>`:** Overrides `hb_port`.
    *   Example: `HB_PORT=8080 rebar3 shell`
*   **`HB_KEY=<path/to/wallet.key>`:** Overrides `hb_key`.
    *   Example: `HB_KEY=~/.keys/arweave_key.json rebar3 shell`
*   **`HB_STORE=<directory_path>`:** Overrides `hb_store`.
    *   Example: `HB_STORE=./node_data_1 rebar3 shell`
*   **`HB_PRINT=<setting>`:** Overrides `hb_print`. `<setting>` can be `true` (or `1`), or a comma-separated list of modules/topics (e.g., `hb_path,hb_ao,ao_result`).
    *   Example: `HB_PRINT=hb_http,dev_router rebar3 shell`
*   **`HB_CONFIG_LOCATION=<path/to/config.flat>`:** Specifies a custom location for the configuration file.

**Using `erl_opts` (Direct Erlang VM Arguments):**

You can also pass arguments directly to the Erlang VM using the `-<key> <value>` format within `erl_opts`. This is generally less common for application configuration than `config.flat` or environment variables.

```bash
rebar3 shell --erl_opts "-hb_port 8080 -hb_key path/to/key.json"
```

**Order of Precedence:**

1.  Command-line arguments (`erl_opts`).
2.  Settings in `config.flat`.
3.  Environment variables (`HB_*`).
4.  Default values from `hb_opts.erl`.

## Configuration in Releases

When running a release build (see [Running a HyperBEAM Node](./running-a-hyperbeam-node.md)), configuration works similarly:

1.  A `config.flat` file will be present in the release directory (e.g., `_build/default/rel/hb/config.flat`). Edit this file to set your desired parameters for the release environment.
2.  Environment variables (`HB_*`) can still be used to override the settings in the release's `config.flat` when starting the node using the `bin/hb` script.
