# HyperBEAM API

This document describes the HTTP API exposed by HyperBEAM for interacting with devices and processes.

## API Overview

HyperBEAM's API is built around the concept of messages and devices. Every interaction with HyperBEAM is done by sending HTTP requests to specific device endpoints.

## Message Format

In HyperBEAM, every piece of data is described as a message, which can be interpreted as a binary term or as a collection of named functions (a Map of functions).

HTTP messages in HyperBEAM follow a standard format based on HTTP semantics as described in RFC specifications.

## Core API Endpoints

### Metadata Device (~meta@1.0)

The ~meta@1.0 device provides information about the node and allows configuration changes.

#### Get Node Information

```
GET /~meta@1.0/info
```

**Response**: Information about the node configuration, supported devices, and other metadata.

#### Update Node Configuration

```
POST /~meta@1.0/info
Your-Config-Tag: Your-Config-Value
```

**Description**: Update the node's configuration. The headers provided in the message are interpreted as configuration options.

### Relay Device (~relay@1.0)

The ~relay@1.0 device is used to relay messages between nodes and the wider HTTP network.

#### Send Message

```
POST /~relay@1.0/send
Content-Type: application/json

{
  "destination": "target-process-id",
  "message": "Your message content"
}
```

**Description**: Sends a message to the specified destination.

### Process Device (~process@1.0)

The ~process@1.0 device enables the creation and management of persistent, shared executions.

#### Create Process

```
POST /~process@1.0/create
Content-Type: application/json

{
  "module": "module-id",
  "scheduler": "scheduler-id",
  "parameters": {}
}
```

**Description**: Creates a new AO process with the specified module and scheduler.

#### Push Message to Process

```
POST /~process@1.0/push
Content-Type: application/json

{
  "process": "process-id",
  "message": "Your message content"
}
```

**Description**: Pushes a message to the specified process's execution outbox.

### WebAssembly Device (~wasm64@1.0)

The ~wasm64@1.0 device executes WebAssembly code using the Web Assembly Micro-Runtime (WAMR).

#### Execute WASM Module

```
POST /~wasm64@1.0/execute
Content-Type: application/json

{
  "module": "module-id",
  "function": "function-name",
  "parameters": {}
}
```

**Description**: Executes the specified function in the WASM module with the provided parameters.

## API Authentication

Some API endpoints require authentication using a signed message. This is done using the RFC-9421 standard for HTTP Message Signatures.

## Example Usage

### Getting Node Information

```bash
curl http://localhost:10000/~meta@1.0/info
```

### Creating a Process

```bash
curl -X POST http://localhost:10000/~process@1.0/create \
  -H "Content-Type: application/json" \
  -d '{"module": "module-id", "scheduler": "scheduler-id"}'
```

### Sending a Message to a Process

```bash
curl -X POST http://localhost:10000/~process@1.0/push \
  -H "Content-Type: application/json" \
  -d '{"process": "process-id", "message": "Hello, World!"}'
```

## Error Handling

HyperBEAM returns standard HTTP status codes to indicate the success or failure of API requests. Common error codes include:

- **400 Bad Request**: The request was malformed or invalid
- **401 Unauthorized**: Authentication is required
- **403 Forbidden**: The request is not allowed
- **404 Not Found**: The requested resource does not exist
- **500 Internal Server Error**: An error occurred on the server

Error responses include JSON payloads with more detailed information about the error.

## API Limitations

- API rate limits may be enforced depending on node configuration
- Some operations may require payment depending on the pricing device configuration
- Large messages may be rejected depending on node configuration