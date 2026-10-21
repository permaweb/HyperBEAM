# Pathing in AO-Core


## Overview

Understanding how to construct and interpret paths in AO-Core is fundamental to working with HyperBEAM. This guide explains the structure and components of AO-Core paths, enabling you to effectively interact with processes and access their data.

## HyperPATH Structure

Let's examine a typical HyperBEAM endpoint piece-by-piece:

```text
http://localhost:8734/<procId>~process@1.0/now
```

### Node URL (`localhost:8734`)

The node URL is the HyperBEAM node that evaluates the path. The examples in this corpus use `http://localhost:8734` so the request runs against a node you control. The same path shape can be sent to another HyperBEAM node when you intentionally want that node's cache, configuration, trusted devices, and signatures.

### Process Path (`/<procId>~process@1.0`)

Every path in AO-Core represents a program. Think of the URL bar as a Unix-style command-line interface, providing access to AO's trustless and verifiable compute. Each path component (between `/` characters) represents a step in the computation. In this example, we instruct the AO-Core node to:

1. Load a specific message from its caches (local, another node, or Arweave)
2. Interpret it with the [`~process@1.0`](/~process@1.0/docs) device
3. The process device implements a shared computing environment with consistent state between users

Process paths are only one example. A path can also call a format, cache, Arweave, Lua, message, bundle, auth, or metering device directly.

### State Access (`/now` or `/compute`)

Devices in AO-Core expose keys accessible via path components. Each key executes a function on the device:

- `now`: Calculates real-time process state
- `compute`: Serves the latest known state (faster than checking for new messages)

Under the surface, these keys represent AO-Core messages. As we progress through the path, AO-Core applies each message to the existing state. You can access the full process state by visiting:
```text
/<procId>~process@1.0/now
```

### State Navigation

You can browse through sub-messages and data fields by accessing them as keys. For example, if a process stores its interaction count in a field named `cache`, you can access it like this:
```text
/<procId>~process@1.0/compute/cache
```
This shows the 'cache' of your process. Each response is:

- A message with a signature attesting to its correctness
- A hashpath describing its generation
- Transferable to other AO-Core nodes for uninterrupted execution

### Query Parameters and Type Casting

Beyond path segments, HyperBEAM URLs can include query parameters that utilize a special type casting syntax. This allows specifying the desired data type for a parameter directly within the URL using the format `key+type=value`.

- **Syntax**: A `+` symbol separates the parameter key from its intended type (e.g., `count+integer=42`, `items+list="apple",7`).
- **Mechanism**: The HyperBEAM node identifies the `+type` suffix (e.g., `+integer`, `+list`, `+map`, `+float`, `+atom`, `+resolve`). It then uses internal functions ([`hb_singleton:maybe_typed`](https://github.com/permaweb/HyperBEAM/blob/edge/docs/resources/source-code/hb_singleton.md) and [`dev_structured:decode_value`](/~structured@1.0/docs)) to decode and cast the provided value string into the corresponding Erlang data type before incorporating it into the message.
- **Supported Types**: Common types include `integer`, `float`, `list`, `map`, `atom`, `binary` (often implicit), and `resolve` (for path resolution). List values often follow the [HTTP Structured Fields format (RFC 8941)](https://www.rfc-editor.org/rfc/rfc8941.html).

This powerful feature enables the expression of complex data structures directly in URLs.

## Examples

The following examples illustrate HyperPATHs for both processes and direct device composition. HyperBEAM's extensible nature allows interaction with any device or process via HyperPATH.

### Example 1: Accessing Full Process State

To get the complete, real-time state of a process identified by `<procId>`, use the `/now` path component with the [`~process@1.0`](/~process@1.0/docs) device:

```bash
HB="${HB:-http://localhost:8734}"
PROCESS_ID="<process-id>"
curl -fsS "$HB/$PROCESS_ID~process@1.0/now/counter"
```

This instructs the AO-Core node to load the process and execute the `now` function on the [`~process@1.0`](/~process@1.0/docs) device.

### Example 2: Navigating to Specific Process Data

If a process maintains its state in a map and you want to access a specific field, like `at-slot`, using the faster `/compute` endpoint:

```bash
HB="${HB:-http://localhost:8734}"
PROCESS_ID="<process-id>"
curl -fsS "$HB/$PROCESS_ID~process@1.0/compute/status"
```

This accesses the `compute` key on the [`~process@1.0`](/~process@1.0/docs) device and then navigates to a patched state key. Every piece of relevant information your process exposes can be accessed similarly, effectively providing a native API.

This represents direct navigation within the process state structure. For accessing data specifically published via the `~patch@1.0` device, see [`~patch@1.0`](/~patch@1.0/docs).

### Example 3: Basic `~message@1.0` Usage

Here's a simple example of using [`~message@1.0`](/~message@1.0/docs) to create a message and retrieve a value:

```bash
curl -sS 'http://localhost:8734/~message@1.0&greeting="Hello"&count+integer=42/count'
```

1.  **Base:** `/` - The base URL of the HyperBEAM node.
2.  **Root Device:** [`~message@1.0`](/~message@1.0/docs)
3.  **Query Params:** `greeting="Hello"` (binary) and `count+integer=42` (integer), forming the message `#{ <<"greeting">> => <<"Hello">>, <<"count">> => 42 }`.
4.  **Path:** `/count` tells `~message@1.0` to retrieve the value associated with the key `count`.

**Response:** The integer `42`.

### Example 4: Device Composition Without A Process

The same path model works without a process ID. This request builds a transient message with `~message@1.0`, then serializes that message with `~json@1.0`:

```bash
curl -sS 'http://localhost:8734/~message@1.0&greeting="Hello"&count+integer=42/~json@1.0/serialize'
```

The output of the message device becomes the input to the JSON device. Larger pipelines follow the same pattern: each path segment receives the message produced by the previous segment.

### Example 5: Using the `~message@1.0` Device with Type Casting

The [`~message@1.0`](/~message@1.0/docs) device can be used to construct and query transient messages, utilizing type casting in query parameters.

Consider the following URL:

```bash
curl -sS 'http://localhost:8734/~message@1.0&name="Alice"&age+integer=30&items+list="apple",1,"banana"&config+map=key1="val1";key2=true/items/1'
```

HyperBEAM processes this as follows:

1.  **Base:** `/` - The base URL of the HyperBEAM node.
2.  **Root Device:** [`~message@1.0`](/~message@1.0/docs)
3.  **Query Parameters (with type casting):**
    *   `name="Alice"` -> `#{ <<"name">> => <<"Alice">> }` (binary)
    *   `age+integer=30` -> `#{ <<"age">> => 30 }` (integer)
    *   `items+list="apple",1,"banana"` -> `#{ <<"items">> => [<<"apple">>, 1, <<"banana">>] }` (list)
    *   `config+map=key1="val1";key2=true` -> `#{ <<"config">> => #{<<"key1">> => <<"val1">>, <<"key2">> => true} }` (map)
4.  **Initial Message Map:** A combination of the above key-value pairs.
5.  **Path Evaluation:**
    *   If `[PATH]` is `/items/1`, the response is the integer `1`.
    *   If `[PATH]` is `/config/key1`, the response is the binary `<<"val1">>`.

## Best Practices

1. Always verify cryptographic signatures on responses
2. Use appropriate caching strategies for frequently accessed data
3. Implement proper error handling for network requests
4. Consider rate limits and performance implications
5. Keep sensitive data secure and use appropriate authentication methods
