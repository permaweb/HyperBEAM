# Device: ~lua@5.3a

## Overview

The [`~lua@5.3a`](../resources/source-code/dev_lua.md) device enables the execution of Lua scripts within the HyperBEAM environment. It provides an isolated sandbox where Lua code can process incoming messages, interact with other devices, and manage state.

## Core Concept: Lua Script Execution

This device allows processes to perform computations defined in Lua scripts. Similar to the [`~wasm64@1.0`](../resources/source-code/dev_wasm.md) device, it manages the lifecycle of a Lua execution state associated with the process.

## Key Functions (Keys)

These keys are typically used within an execution stack (managed by [`dev_stack`](../resources/source-code/dev_stack.md)) for an AO process.

*   **`init`**
    *   **Action:** Initializes the Lua environment for the process. It finds and loads the Lua script(s) associated with the process, creates a `luerl` state, applies sandboxing rules if specified, installs the [`dev_lua_lib`](../resources/source-code/dev_lua_lib.md) (providing AO-specific functions like `ao.send`), and stores the initialized state in the process's private area (`priv/state`).
    *   **Inputs (Expected in Process Definition or `init` Message):**
        *   `script`: Can be:
            *   An Arweave Transaction ID of the Lua script file.
            *   A list of script IDs or script message maps.
            *   A message map containing the Lua script in its `body` tag (Content-Type `application/lua` or `text/x-lua`).
            *   A map where keys are module names and values are script IDs/messages.
        *   `sandbox`: (Optional) Controls Lua sandboxing. Can be `true` (uses default sandbox list), `false` (no sandbox), or a map/list specifying functions to disable and their return values.
    *   **Outputs (Stored in `priv/`):**
        *   `state`: The initialized `luerl` state handle.
*   **`<FunctionName>` (Default Handler - `compute`)**
    *   **Action:** Executes a specific function within the loaded Lua script(s). This is the default handler; if a key matching a Lua function name is called on the device, this logic runs.
    *   **Inputs (Expected in Process State or Incoming Message):**
        *   `priv/state`: The Lua state obtained during `init`.
        *   The **key** being accessed (used as the default function name).
        *   `function` or `body/function`: (Optional) Overrides the function name derived from the key.
        *   `parameters` or `body/parameters`: (Optional) Arguments to pass to the Lua function. Defaults to a list containing the process message, the request message, and an empty options map.
    *   **Response:** The results returned by the Lua function call, typically encoded. The device also updates the `priv/state` with the Lua state after execution.
*   **`snapshot`**
    *   **Action:** Captures the current state of the running Lua environment. `luerl` state is serializable.
    *   **Inputs:** `priv/state`.
    *   **Outputs:** A message containing the serialized Lua state, typically tagged with `[Prefix]/State`.
*   **`normalize` (Internal Helper)**
    *   **Action:** Ensures a consistent state representation by loading a Lua state from a snapshot (`[Prefix]/State`) if a live state (`priv/state`) isn't already present.
*   **`functions`**
    *   **Action:** Returns a list of all globally defined functions within the current Lua state.
    *   **Inputs:** `priv/state`.
    *   **Response:** A list of function names.

## Sandboxing

The `sandbox` option in the process definition restricts potentially harmful Lua functions (like file I/O, OS commands, loading arbitrary code). By default (`sandbox = true`), common dangerous functions are disabled. You can customize the sandbox rules.

## AO Library (`dev_lua_lib`)

The `init` function automatically installs a helper library ([`dev_lua_lib`](../resources/source-code/dev_lua_lib.md)) into the Lua state. This library typically provides functions for interacting with the AO environment from within the Lua script, such as:

*   `ao.send({ Target = ..., ... })`: To send messages from the process.
*   Access to message tags and data.

## Usage within `dev_stack`

Like [`~wasm64@1.0`](../resources/source-code/dev_wasm.md), the `~lua@5.3a` device is typically used within an execution stack.

```text
# Example Process Definition Snippet
Execution-Device: stack@1.0
Execution-Stack: scheduler@1.0, lua@5.3a
Script: <LuaScriptTxID>
Sandbox: true
```

This device offers a lightweight, integrated scripting capability for AO processes, suitable for a wide range of tasks from simple logic to more complex state management and interactions.

[lua module](../resources/source-code/dev_lua.md)
