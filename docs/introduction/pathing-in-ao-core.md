# Pathing in AO-Core

## Overview

Understanding how to construct and interpret paths in AO-Core is fundamental to working with HyperBEAM. This guide explains the structure and components of AO-Core paths, enabling you to effectively interact with processes and access their data.

## HyperPATH Structure

Let's examine a typical HyperBEAM endpoint piece-by-piece:

```
https://router-1.forward.computer/<procId>~process@1.0/now
```

### Node URL (`router-1.forward.computer`)

The HTTP response from this node includes a signature from the host's key. By accessing the [`~snp@1.0`](../resources/source-code/dev_snp.md) device, you can verify that the node is running in a genuine Trusted Execution Environment (TEE), ensuring computation integrity. You can replace `router-1.forward.computer` with any HyperBEAM TEE node operated by any party while maintaining trustless guarantees.

### Process Path (`/<procId>~process@1.0`)

Every path in AO-Core represents a program. Think of the URL bar as a Unix-style command-line interface, providing access to AO's trustless and verifiable compute. Each path component (between `/` characters) represents a step in the computation. In this example, we instruct the AO-Core node to:

1. Load a specific message from its caches (local, another node, or Arweave)
2. Interpret it with the [`~process@1.0`](../devices/process-at-1-0.md) device
3. The process device implements a shared computing environment with consistent state between users

### State Access (`/now` or `/compute`)

Devices in AO-Core expose keys accessible via path components. Each key executes a function on the device:

- `now`: Calculates real-time process state
- `compute`: Serves the latest known state (faster than checking for new messages)

Under the surface, these keys represent AO-Core messages. As we progress through the path, AO-Core applies each message to the existing state. You can access the full process state by visiting:
```
/<procId>~process@1.0/now
```

### State Navigation

You can browse through sub-messages and data fields by accessing them as keys. For example, if a process stores its interaction count in a field named `cache`, you can access it like this:
```
/<procId>~process@1.0/compute/cache
```
This shows the 'cache' of your process. Each response is:

- A message with a signature attesting to its correctness
- A hashpath describing its generation
- Transferable to other AO-Core nodes for uninterrupted execution

### Query Parameters and Type Casting

Beyond path segments, HyperBEAM URLs can include query parameters that utilize a special type casting syntax. This allows specifying the desired data type for a parameter directly within the URL using the format `key+type=value`.

- **Syntax**: A `+` symbol separates the parameter key from its intended type (e.g., `count+integer=42`, `items+list="apple",7`).
- **Mechanism**: The HyperBEAM node identifies the `+type` suffix (e.g., `+integer`, `+list`, `+map`, `+float`, `+atom`, `+resolve`). It then uses internal functions ([`hb_singleton:maybe_typed`](../resources/source-code/hb_singleton.md) and [`dev_codec_structured:decode_value`](../resources/source-code/dev_codec_structured.md)) to decode and cast the provided value string into the corresponding Erlang data type before incorporating it into the message.
- **Supported Types**: Common types include `integer`, `float`, `list`, `map`, `atom`, `binary` (often implicit), and `resolve` (for path resolution). List values often follow the [HTTP Structured Fields format (RFC 8941)](https://www.rfc-editor.org/rfc/rfc8941.html).

This powerful feature enables the expression of complex data structures directly in URLs.

## Examples

The following examples illustrate using HyperPATH with various AO-Core processes and devices. While these cover a few specific use cases, HyperBEAM's extensible nature allows interaction with any device or process via HyperPATH. For a deeper understanding, we encourage exploring the [source code](https://github.com/permaweb/hyperbeam) and experimenting with different paths.

### Example 1: Accessing Full Process State

To get the complete, real-time state of a process identified by `<procId>`, use the `/now` path component with the [`~process@1.0`](../devices/process-at-1-0.md) device:

```
GET /<procId>~process@1.0/now
```

This instructs the AO-Core node to load the process and execute the `now` function on the [`~process@1.0`](../devices/process-at-1-0.md) device.

### Example 2: Navigating to Specific Process Data

If a process maintains its state in a map and you want to access a specific field, like `at-slot`, using the faster `/compute` endpoint:

```
GET /<procId>~process@1.0/compute/cache
```

This accesses the `compute` key on the [`~process@1.0`](../devices/process-at-1-0.md) device and then navigates to the `cache` key within the resulting state map. Using this path, you will see the latest 'cache' of your process (the number of interactions it has received). Every piece of relevant information about your process can be accessed similarly, effectively providing a native API.

(Note: This represents direct navigation within the process state structure. For accessing data specifically published via the `~patch@1.0` device, see the documentation on [Exposing Process State](../build/exposing-process-state.md), which typically uses the `/cache/` path.)

### Example 3: Basic `~message@1.0` Usage

Here's a simple example of using [`~message@1.0`](../devices/message-at-1-0.md) to create a message and retrieve a value:

```
GET /~message@1.0&greeting="Hello"&count+integer=42/count
```

1.  **Base:** `/` - The base URL of the HyperBEAM node.
2.  **Root Device:** [`~message@1.0`](../devices/message-at-1-0.md)
3.  **Query Params:** `greeting="Hello"` (binary) and `count+integer=42` (integer), forming the message `#{ <<"greeting">> => <<"Hello">>, <<"count">> => 42 }`.
4.  **Path:** `/count` tells `~message@1.0` to retrieve the value associated with the key `count`.

**Response:** The integer `42`.

### Example 4: Using the `~message@1.0` Device with Type Casting

The [`~message@1.0`](../devices/message-at-1-0.md) device can be used to construct and query transient messages, utilizing type casting in query parameters.

Consider the following URL:

```
GET /~message@1.0&name="Alice"&age+integer=30&items+list="apple",1,"banana"&config+map=key1="val1";key2=true/[PATH]
```

HyperBEAM processes this as follows:

1.  **Base:** `/` - The base URL of the HyperBEAM node.
2.  **Root Device:** [`~message@1.0`](../devices/message-at-1-0.md)
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