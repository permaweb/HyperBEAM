# Compute Unit Configuration

The Compute Unit (CU) supports numerous environment variables and configuration options. This document details the available options and recommended settings.

## Configuration Methods

The Compute Unit can be configured using:

1. **Environment Variables**: Set directly in the shell or via a `.env` file
2. **Command Line Arguments**: Pass when starting the CU
3. **Configuration Files**: Use JSON configuration files

## Essential Configuration Options

### Core Settings

| Variable | Description | Default |
|----------|-------------|---------|
| `UNIT_MODE` | Operating mode (set to "hbu" for HyperBEAM) | - |
| `HB_URL` | URL of your HyperBEAM instance | http://localhost:10000 |
| `PORT` | The port on which the CU server will listen | 6363 |
| `WALLET_FILE` | Path to your Arweave wallet JSON file | - |
| `NODE_CONFIG_ENV` | Configuration environment | "development" |

### Network Settings

| Variable | Description | Default |
|----------|-------------|---------|
| `GATEWAY_URL` | The url of the Arweave gateway | https://arweave.net |
| `ARWEAVE_URL` | URL for the Arweave HTTP API | GATEWAY_URL |
| `GRAPHQL_URL` | URL for the Arweave GraphQL server | ${GATEWAY_URL}/graphql |
| `CHECKPOINT_GRAPHQL_URL` | URL for querying Checkpoints | GRAPHQL_URL |
| `UPLOADER_URL` | URL for uploading Process Checkpoints | up.arweave.net |

### WASM Execution Settings

| Variable | Description | Default |
|----------|-------------|---------|
| `PROCESS_WASM_MEMORY_MAX_LIMIT` | Maximum memory limit for processes in bytes | 1GB |
| `PROCESS_WASM_COMPUTE_MAX_LIMIT` | Maximum compute limit for processes | 9 billion |
| `PROCESS_WASM_SUPPORTED_FORMATS` | Supported wasm module formats (comma-delimited) | wasm32-unknown-emscripten,wasm32-unknown-emscripten2 |
| `PROCESS_WASM_SUPPORTED_EXTENSIONS` | Supported wasm extensions (comma-delimited) | - |
| `WASM_EVALUATION_MAX_WORKERS` | Number of workers for message evaluation | CPU count - 1 |

### Caching Settings

| Variable | Description | Default |
|----------|-------------|---------|
| `WASM_BINARY_FILE_DIRECTORY` | Directory to cache wasm binaries | OS temp directory |
| `WASM_MODULE_CACHE_MAX_SIZE` | Maximum size of the wasm module cache | 5 modules |
| `WASM_INSTANCE_CACHE_MAX_SIZE` | Maximum size of the wasm instance cache | 5 instances |
| `PROCESS_MEMORY_CACHE_MAX_SIZE` | Maximum size of the process memory cache in bytes | - |
| `PROCESS_MEMORY_CACHE_TTL` | Time-to-live for memory cache entries | - |
| `PROCESS_MEMORY_CACHE_FILE_DIR` | Directory for drained process memory | OS temp directory |

### Checkpoint Settings

| Variable | Description | Default |
|----------|-------------|---------|
| `PROCESS_MEMORY_FILE_CHECKPOINTS_DIR` | Directory for file checkpoints | OS temp/file_checkpoints |
| `PROCESS_MEMORY_CACHE_CHECKPOINT_INTERVAL` | Checkpoint interval (0 to disable) | 0 |
| `PROCESS_CHECKPOINT_CREATION_THROTTLE` | Time between checkpoints for a process | 30 minutes |
| `DISABLE_PROCESS_CHECKPOINT_CREATION` | Disable Arweave checkpoint uploads | Enabled (must set to 'false' to enable) |
| `DISABLE_PROCESS_FILE_CHECKPOINT_CREATION` | Disable file checkpoint creation | Enabled (must set to 'false' to enable) |
| `EAGER_CHECKPOINT_ACCUMULATED_GAS_THRESHOLD` | Gas threshold for immediate checkpoint | - |

### Performance and Monitoring

| Variable | Description | Default |
|----------|-------------|---------|
| `MEM_MONITOR_INTERVAL` | Interval for logging memory usage | - |
| `BUSY_THRESHOLD` | Timeout for "busy" response to clients | 0 (disabled) |
| `ENABLE_METRICS_ENDPOINT` | Enable OpenTelemetry metrics endpoint | Disabled |
| `DEFAULT_LOG_LEVEL` | Logging level | debug |
| `LOG_CONFIG_PATH` | Path to log level configuration file | .loglevel |

### Access Control

| Variable | Description | Default |
|----------|-------------|---------|
| `RESTRICT_PROCESSES` | Process IDs to restrict (blacklist) | - |
| `ALLOW_PROCESSES` | Process IDs to allow (whitelist) | - |
| `ALLOW_OWNERS` | Process owners to allow (whitelist) | - |
| `PROCESS_CHECKPOINT_TRUSTED_OWNERS` | Wallets whose checkpoints are trusted | - |

## Example .env File

A minimal configuration file looks like this:

```bash
UNIT_MODE=hbu
HB_URL=http://localhost:10000
PORT=6363
WALLET_FILE=./wallet.json
NODE_CONFIG_ENV="development"
```

## Logging Configuration

The CU uses logging levels that conform to RFC5424 severity semantics:

```
{
  error: 0,   // Errors that must be addressed immediately
  warn: 1,    // Warnings that should be monitored
  info: 2,    // Important operational information
  http: 3,    // HTTP request/response details
  verbose: 4, // More detailed operational information
  debug: 5,   // Debugging information
  silly: 6    // Very detailed debugging information
}
```

You can set the logging level with the `DEFAULT_LOG_LEVEL` environment variable or dynamically change it by creating or modifying a `.loglevel` file in the working directory.

## Applying Configuration Changes

For most configuration changes to take effect, you need to restart the Compute Unit service. When running in development mode with hot reloading, some configuration changes may require a full restart. 