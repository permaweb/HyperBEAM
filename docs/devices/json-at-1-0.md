# Device: ~json@1.0

## Overview

The [`~json@1.0`](../resources/source-code/dev_json_iface.md) device provides a mechanism to interact with JSON (JavaScript Object Notation) data structures using HyperPATHs. It allows treating a JSON document or string as a stateful entity against which HyperPATH queries can be executed.

This device is useful for:

*   Serializing and deserializing JSON data.
*   Querying and modifying JSON objects.
*   Integrating with other devices and operations via HyperPATH chaining.

## Core Functions (Keys)

### Serialization

*   **`GET /~json@1.0/serialize` (Direct Serialize Action)**
    *   **Action:** Serializes the input message or data into a JSON string.
    *   **Example:** `GET /~json@1.0/serialize` - serializes the current message as JSON.
    *   **HyperPATH:** The path segment `/serialize` directly follows the device identifier.

*   **`GET /<PreviousPath>/~json@1.0/serialize` (Chained Serialize Action)**
    *   **Action:** Takes arbitrary data output from `<PreviousPath>` (another device or operation) and returns its serialized JSON string representation.
    *   **Example:** `GET /~meta@1.0/info/~json@1.0/serialize` - fetches node info from the meta device and then pipes it to the JSON device to serialize the result as JSON.
    *   **HyperPATH:** This segment (`/~json@1.0/serialize`) is appended to a previous HyperPATH segment.

## HyperPATH Chaining Example

The JSON device is particularly useful in HyperPATH chains to convert output from other devices into JSON format:

```
GET /~meta@1.0/info/~json@1.0/serialize
```

This retrieves the node configuration from the meta device and serializes it to JSON.

## See Also

- [Message Device](../resources/source-code/dev_message.md) - Works well with JSON serialization
- [Meta Device](../resources/source-code/dev_meta.md) - Can provide configuration data to serialize

[json module](../resources/source-code/dev_codec_json.md)