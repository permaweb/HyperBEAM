# Devices

Devices are the core functional units within HyperBEAM and AO-Core. They define how messages are processed and what actions can be performed.

Each device listed here represents a specific capability available to AO processes and nodes. Understanding these devices is key to building complex applications and configuring your HyperBEAM node effectively.

## Available Devices

Below is a list of documented built-in devices. Each page details the device's purpose, available functions (keys), and usage examples where applicable.

*   **[`~message@1.0`](./message-at-1-0.md):** Base message handling and manipulation.
*   **[`~meta@1.0`](./meta-at-1-0.md):** Node configuration and metadata.
*   **[`~process@1.0`](./process-at-1-0.md):** Persistent, shared process execution environment.
*   **[`~scheduler@1.0`](./scheduler-at-1-0.md):** Message scheduling and execution ordering for processes.
*   **[`~wasm64@1.0`](./wasm64-at-1-0.md):** WebAssembly (WASM) execution engine.
*   **[`~lua@5.3a`](./lua-at-5-3a.md):** Lua script execution engine.
*   **[`~relay@1.0`](./relay-at-1-0.md):** Relaying messages to other nodes or HTTP endpoints.
*   **[`~json@1.0`](./json-at-1-0.md):** Provides access to JSON data structures using HyperPATHs.

*(More devices will be documented here as specifications are finalized and reviewed.)*

## Device Naming and Versioning

Devices are typically referenced using a name and version, like `~<name>@<version>` (e.g., `~process@1.0`). The tilde (`~`) often indicates a primary, user-facing device, while internal or utility devices might use a `dev_` prefix in the source code (e.g., `dev_router`).

Versioning indicates the specific interface and behavior of the device. Changes to a device that break backward compatibility usually result in a version increment.
