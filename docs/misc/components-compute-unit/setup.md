# **Local CU Setup**

This guide explains how to set up the local Compute Unit (CU) for HyperBEAM.

## What is Local CU?

The ao Compute Unit (CU) is a spec-compliant implementation built with NodeJS. It serves as the computational processing component in the ao ecosystem, handling WASM execution and state management.

## Prerequisites

* Node.js
* Git

## Installation Steps

### 1. Clone the Repository

```bash
git clone https://github.com/permaweb/local-cu
cd local-cu
```

### 2. Install Dependencies

```bash
npm i
```

### 3. Generate a Wallet (if needed)

If you don't already have an Arweave wallet, you can generate one:

```bash
npx --yes @permaweb/wallet > wallet.json
```

### 4. Configure Environment

Create a `.env` file with the following minimal configuration:

```bash
UNIT_MODE=hbu
HB_URL=http://localhost:10000
PORT=6363
WALLET_FILE=./wallet.json
NODE_CONFIG_ENV="development"
```

### 5. Start the Compute Unit

```bash
npm start
```

For development with hot-reloading, you can use:

```bash
npm run dev
```

## Environment Variables

The CU supports numerous environment variables for configuration. Here are the key ones:

### Essential Configuration

* **WALLET/WALLET_FILE**: The JWK Interface stringified JSON or a file to load it from
* **PORT**: Which port the web server should listen on (defaults to 6363)
* **UNIT_MODE**: Set to "hbu" for HyperBEAM mode
* **HB_URL**: URL of your HyperBEAM instance

### Gateway Configuration

* **GATEWAY_URL**: The URL of the Arweave gateway (defaults to https://arweave.net)
* **ARWEAVE_URL**: The URL for the Arweave HTTP API (defaults to GATEWAY_URL)
* **GRAPHQL_URL**: The URL for the Arweave GraphQL server (defaults to ${GATEWAY_URL}/graphql)
* **UPLOADER_URL**: The URL of the uploader for Process Checkpoints (defaults to up.arweave.net)

### Process Limits

* **PROCESS_WASM_MEMORY_MAX_LIMIT**: Maximum memory limit for processes (defaults to 1GB)
* **PROCESS_WASM_COMPUTE_MAX_LIMIT**: Maximum compute limit for processes (defaults to 9 billion)
* **PROCESS_WASM_SUPPORTED_FORMATS**: Supported wasm module formats (comma-delimited)
* **PROCESS_WASM_SUPPORTED_EXTENSIONS**: Supported wasm extensions (comma-delimited)

### Caching and Performance

* **WASM_EVALUATION_MAX_WORKERS**: Number of workers for evaluating messages (defaults to CPU count - 1)
* **WASM_BINARY_FILE_DIRECTORY**: Directory to cache wasm binaries downloaded from Arweave
* **WASM_MODULE_CACHE_MAX_SIZE**: Maximum size of the in-memory wasm module cache
* **WASM_INSTANCE_CACHE_MAX_SIZE**: Maximum size of the in-memory wasm instance cache
* **PROCESS_MEMORY_CACHE_MAX_SIZE**: Maximum size of the process memory cache
* **PROCESS_MEMORY_CACHE_TTL**: Time-to-live for process memory cache entries

### Checkpoint Configuration

* **PROCESS_MEMORY_CACHE_CHECKPOINT_INTERVAL**: Interval for checkpointing processes (0 to disable)
* **PROCESS_CHECKPOINT_CREATION_THROTTLE**: Time to wait before creating another checkpoint for a process
* **DISABLE_PROCESS_CHECKPOINT_CREATION**: Whether to disable process checkpoint uploads to Arweave
* **DISABLE_PROCESS_FILE_CHECKPOINT_CREATION**: Whether to disable process checkpoint creation to the filesystem
* **EAGER_CHECKPOINT_ACCUMULATED_GAS_THRESHOLD**: Gas threshold for immediate checkpoint creation

### Database Configuration

* **DB_MODE**: Whether the database is embedded or remote (defaults to embedded)
* **DB_URL**: The name of the embedded database (defaults to ao-cache)

### Other Settings

* **ENABLE_METRICS_ENDPOINT**: Whether to enable the OpenTelemetry /metrics endpoint
* **DEFAULT_LOG_LEVEL**: The logging level to use (defaults to debug)
* **LOG_CONFIG_PATH**: Path to the file used to dynamically set logging level
* **BUSY_THRESHOLD**: Wait time before sending a "busy" response to clients
* **RESTRICT_PROCESSES**: List of process IDs to restrict (blacklist)
* **ALLOW_PROCESSES**: List of process IDs to allow (whitelist)
* **ALLOW_OWNERS**: List of process owners to allow (owner whitelist)


## Verification

To verify that your CU is running correctly, you can check:

```bash
curl http://localhost:6363
```

You should receive a response confirming the CU is operational.

### Dynamically Change Log Level

If you need to change the log level while the CU is running, you can create or modify a `.loglevel` file in the working directory with the desired level (error, warn, info, http, verbose, debug, silly).

### Manually Trigger Checkpointing

To manually trigger checkpointing for all processes in memory:

1. Obtain the process ID of the CU: `pgrep node` or `lsof -i :6363`
2. Send a SIGUSR2 signal: `kill -USR2 <process_id>`

This will cause the CU to checkpoint all processes in its in-memory cache.

## Next Steps

After setting up the Compute Unit, see the [Configuration](configuration.md) page for more detailed configuration options and the [API Reference](api.md) for information on interacting with the CU.
