# Device: ~message@1.0

## Overview

The [`~message@1.0`](../resources/source-code/dev_message.md) device is a fundamental built-in device in HyperBEAM. It serves as the identity device for standard AO-Core messages, which are represented as Erlang maps internally. Its primary function is to allow manipulation and inspection of these message maps directly via HyperPATH requests, without needing a persistent process state.

This device is particularly useful for:

*   Creating and modifying transient messages on the fly using query parameters.
*   Retrieving specific values from a message map.
*   Inspecting the keys of a message.
*   Handling message commitments and verification (though often delegated to specialized commitment devices like [`httpsig@1.0`](../resources/source-code/dev_codec_httpsig.md)).

## Core Functionality

The `message@1.0` device treats the message itself as the state it operates on. Key operations are accessed via path segments in the HyperPATH.

### Key Access (`/key`)

To retrieve the value associated with a specific key in the message map, simply append the key name to the path. Key lookup is case-insensitive.

**Example:**

```
GET /~message@1.0&hello=world&Key=Value/key
```

**Response:**

```
"Value"
```

### Reserved Keys

The `message@1.0` device reserves several keys for specific operations:

*   **`get`**: (Default operation if path segment matches a key in the map) Retrieves the value of a specified key. Behaves identically to accessing `/key` directly.
*   **`set`**: Modifies the message by adding or updating key-value pairs. Requires additional parameters (usually in the request body or subsequent path segments/query params, depending on implementation specifics).
    *   Supports deep merging of maps.
    *   Setting a key to `unset` removes it.
    *   Overwriting keys that are part of existing commitments will typically remove those commitments unless the new value matches the old one.
*   **`set_path`**: A special case for setting the `path` key itself, which cannot be done via the standard `set` operation.
*   **`remove`**: Removes one or more specified keys from the message. Requires an `item` or `items` parameter.
*   **`keys`**: Returns a list of all public (non-private) keys present in the message map.
*   **`id`**: Calculates and returns the ID (hash) of the message. Considers active commitments based on specified `committers`. May delegate ID calculation to a device specified by the message\'s `id-device` key or the default ([`httpsig@1.0`](../resources/source-code/dev_codec_httpsig.md)).
*   **`commit`**: Creates a commitment (e.g., a signature) for the message. Requires parameters like `commitment-device` and potentially committer information. Delegates the actual commitment generation to the specified device (default [`httpsig@1.0`](../resources/source-code/dev_codec_httpsig.md)).
*   **`committers`**: Returns a list of committers associated with the commitments in the message. Can be filtered by request parameters.
*   **`commitments`**: Used internally and in requests to filter or specify which commitments to operate on (e.g., for `id` or `verify`).
*   **`verify`**: Verifies the commitments attached to the message. Can be filtered by `committers` or specific `commitment` IDs in the request. Delegates verification to the device specified in each commitment (`commitment-device`).

### Private Keys

Keys prefixed with `priv` (e.g., `priv_key`, `private.data`) are considered private and cannot be accessed or listed via standard `get` or `keys` operations.

## HyperPATH Example

This example demonstrates creating a transient message and retrieving a value:

```
GET /~message@1.0&hello=world&k=v/k
```

**Breakdown:**

1.  `~message@1.0`: Sets the root device.
2.  `&hello=world&k=v`: Query parameters create the initial message: `#{ <<"hello">> => <<"world">>, <<"k">> => <<"v">> }`.
3.  `/k`: The path segment requests the value for the key `k`.

**Response:**

```
"v"
``` 