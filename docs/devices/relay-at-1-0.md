# Device: ~relay@1.0

## Overview

The [`~relay@1.0`](../resources/source-code/dev_relay.md) device enables HyperBEAM nodes to send messages to external HTTP endpoints or other AO nodes.

## Core Concept: Message Forwarding

This device acts as an HTTP client within the AO ecosystem. It allows a node or process to make outbound HTTP requests.

## Key Functions (Keys)

*   **`call`**
    *   **Action:** Sends an HTTP request to a specified target and waits synchronously for the response.
    *   **Inputs (from Request Message or Base Message M1):**
        *   `target`: (Optional) A message map defining the request to be sent. Defaults to the original incoming request (`Msg2` or `M1`).
        *   `relay-path` or `path`: The URL/path to send the request to.
        *   `relay-method` or `method`: The HTTP method (GET, POST, etc.).
        *   `relay-body` or `body`: The request body.
        *   `requires-sign`: (Optional, boolean) If true, the request message (`target`) will be signed using the node's key before sending. Defaults to `false`.
        *   `http-client`: (Optional) Specify a custom HTTP client module to use (defaults to node's configured `relay_http_client`).
    *   **Response:** `{ok, <ResponseMessage>}` where `<ResponseMessage>` is the full message received from the remote peer, or `{error, Reason}`.
    *   **Example HyperPATH:**
        ```
        GET /~relay@1.0/call?method=GET&path=https://example.com
        ```
*   **`cast`**
    *   **Action:** Sends an HTTP request asynchronously. The device returns immediately after spawning a process to send the request; it does not wait for or return the response from the remote peer.
    *   **Inputs:** Same as `call`.
    *   **Response:** `{ok, <<"OK">>}`.
*   **`preprocess`**
    *   **Action:** This function is designed to be used as a node's global `preprocessor` (configured via [`~meta@1.0`](../resources/source-code/dev_meta.md)). When configured, it intercepts *all* incoming requests to the node and automatically rewrites them to be relayed via the `call` key. This effectively turns the node into a pure forwarding proxy, using its routing table ([`dev_router`](../resources/source-code/dev_router.md)) to determine the destination.
    *   **Response:** A message structure that invokes `/~relay@1.0/call` with the original request as the target body.

## Use Cases

*   **Inter-Node Communication:** Sending messages between HyperBEAM nodes.
*   **External API Calls:** Allowing AO processes to interact with traditional web APIs.
*   **Routing Nodes:** Nodes configured with the `preprocess` key act as dedicated routers/proxies.
*   **Client-Side Relaying:** A local HyperBEAM instance can use `~relay@1.0` to forward requests to public compute nodes.

## Interaction with Routing

When `call` or `cast` is invoked, the actual HTTP request dispatch is handled by `hb_http:request/2`. This function often utilizes the node's routing configuration ([`dev_router`](../resources/source-code/dev_router.md)) to determine the specific peer/URL to send the request to, especially if the target path is an AO process ID or another internal identifier rather than a full external URL.

[relay module](../resources/source-code/dev_relay.md)
