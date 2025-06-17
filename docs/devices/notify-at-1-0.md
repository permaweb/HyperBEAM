# Device: ~notify@1.0

## Overview

The [`~notify@1.0`](../resources/source-code/dev_notify.md) device provides real-time event streaming capabilities for HyperBEAM, enabling clients to subscribe to specific events and receive notifications via HTTP/3 Server-Sent Events (SSE) streams.

## Use Cases

*   **Real-time monitoring**: Subscribe to AO process events as they occur
*   **Event-driven applications**: Build reactive applications that respond to process state changes
*   **Performance monitoring**: Track events efficiently with high-throughput architecture
*   **Custom filtering**: Use flexible templates to receive only relevant events

## Core Architecture

The notify device operates using a separate long-running **notification manager process** for optimal performance. This architecture ensures:

*   **Asynchronous processing**: Events are dispatched without blocking the main request flow
*   **High throughput**: Dedicated process handles event matching and distribution
*   **Fault isolation**: Event processing failures don't affect other operations
*   **Resource efficiency**: Centralized ETS table management for fast lookups

## Core Functions (Keys)

### `info`

Provides information about the notification device capabilities and endpoints.

*   **`GET /~notify@1.0/info`**
    *   **Action:** Returns device metadata including available paths and version information.
    *   **Response:** JSON object describing the device's capabilities.

### `stream` (Primary API)

Creates a real-time HTTP/3 streaming connection for receiving events. **This is the main method for most use cases.**

*   **`GET /~notify@1.0/stream`**
    *   **Action:** Establishes a Server-Sent Events (SSE) connection for real-time event delivery. Automatically registers the listener when the connection is established.
    *   **Query Parameters:** 
        *   `template`: Event matching pattern (map or regex)
    *   **Response:** HTTP/3 stream with `Content-Type: text/event-stream`
    *   **Headers:** Includes CORS headers and cache-control directives for web compatibility

**Stream URL Examples:**
```
GET /~notify@1.0/stream?template={"device":"process@1.0"}
GET /~notify@1.0/stream?template=/.*compute.*/
```

**Template Types:**
*   **Map Templates**: Structured message matching using key-value pairs
*   **Regex Templates**: Path-based pattern matching using regular expressions

**Map Template Example:**
```json
GET /~notify@1.0/stream?template={"device":"process@1.0","action":"compute"}
```

**Regex Template Example:**
```json
GET /~notify@1.0/stream?template="/.*process.*/.*/.*"
```

### `register` (For Persistent Use Cases)

Registers a new event listener with a specific template for event matching. **Use this for persistent subscriptions that need to survive across multiple stream connections or for programmatic subscription management.**

*   **`POST /~notify@1.0/register`**
    *   **Action:** Creates a persistent event subscription using the provided template and stream identifier.
    *   **Request Body:** Must contain `template` (event pattern) and `stream` (stream identifier) fields.
    *   **Response:** Success confirmation or error message.
    *   **Use Cases:** Pre-registration, batch registration, persistent subscriptions, programmatic management

**Registration Example:**
```json
{
  "template": {
    "device": "process@1.0",
    "action": "compute"
  },
  "stream": "persistent-stream-001"
}
```

### `unregister` (For Persistent Use Cases)

Removes an existing persistent event listener registration.

*   **`POST /~notify@1.0/unregister`**
    *   **Action:** Removes the persistent listener associated with the specified template.
    *   **Request Body:** Must contain the `template` field matching the original registration.
    *   **Response:** Success confirmation or error message.
    *   **Use Cases:** Cleanup of persistent subscriptions, programmatic subscription management

### `dispatch`

Dispatches events to registered listeners (typically called internally by the system).

*   **`POST /~notify@1.0/dispatch`**
    *   **Action:** Processes an event and sends it to all matching registered listeners.
    *   **Request Body:** Event data to be matched against registered templates.
    *   **Response:** Dispatch confirmation.
    *   **Note:** This is primarily used internally by `hb_persistent:notify/4` for automatic event triggering.

## Template System

The notify device supports two types of templates for flexible event filtering:

### Map Templates

Structured templates that match against message fields using exact key-value matching.

```erlang
Template = #{
    <<"device">> => <<"process@1.0">>,
    <<"action">> => <<"compute">>,
    <<"status">> => <<"completed">>
}
```

**Matching Logic:**
*   All specified keys must be present in the event
*   All specified values must match exactly
*   Events can contain additional keys (partial matching allowed)

### Regex Templates

Path-based templates that match against the event's `path` field using regular expressions.

```erlang
Template = <<"/processes/.*/compute/.*">>
```

**Matching Logic:**
*   Event's `path` field is extracted (defaults to `"/"` if not present)
*   Regex pattern is applied to the path string
*   Match succeeds if the pattern matches any part of the path

## Event Flow

1. **Registration**: Client registers listener with template via `/register` endpoint
2. **Template Storage**: Template is validated and stored in ETS table with stream process ID
3. **Event Trigger**: AO process completes operation, triggering `hb_persistent:notify/4`
4. **Event Dispatch**: Notification manager receives event and checks against all templates
5. **Template Matching**: Event is tested against each registered template
6. **Event Delivery**: Matching events are sent to appropriate stream processes
7. **HTTP Streaming**: Events are delivered to clients via Server-Sent Events format

## Integration with HyperBEAM

The notify device integrates seamlessly with HyperBEAM's persistence layer:

*   **Automatic Startup**: Notification manager starts automatically when `notify_device` is configured in `hb_opts.erl`
*   **Event Integration**: `hb_persistent:notify/4` automatically dispatches events to the notification manager
*   **Configuration**: Enable via `notify_device` option in node configuration
*   **Performance**: Uses ETS tables for O(1) listener lookups and efficient event dispatching

## Configuration

To enable the notify device, configure it in your HyperBEAM node:

```erlang
% In hb_opts.erl or config file
notify_device => #{
    <<"name">> => <<"notify@1.0">>,
    <<"module">> => dev_notify
}
```

The notification manager will start automatically during application startup when this configuration is present.

## Performance Characteristics

*   **Lookup Performance**: O(1) listener lookup via ETS named tables
*   **Dispatch Performance**: O(n) where n = number of registered listeners
*   **Memory Usage**: Linear with number of active listeners and templates
*   **Concurrency**: Parallel event processing using short-lived worker processes

## Error Handling

The notify device implements comprehensive error handling:

*   **Template Validation**: Templates are validated at registration time to fail fast
*   **Dead Process Cleanup**: Automatic cleanup of terminated listener processes during dispatch
*   **Graceful Degradation**: Individual listener failures don't affect other listeners

[notify module](../resources/source-code/dev_notify.md)