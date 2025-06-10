# Device: ~wasm64@1.0

## Overview

The [`~wasm64@1.0`](../resources/source-code/dev_wasm.md) device enables the execution of 64-bit WebAssembly (WASM) code within the HyperBEAM environment. It provides a sandboxed environment for running compiled code from various languages (like Rust, C++, Go) that target WASM.

## Core Concept: WASM Execution

This device allows AO processes to perform complex computations defined in WASM modules, which can be written in languages like Rust, C++, C, Go, etc., and compiled to WASM.

The device manages the lifecycle of a WASM instance associated with the process state.

## Key Functions (Keys)

These keys are typically used within an execution stack (managed by [`dev_stack`](../resources/source-code/dev_stack.md)) for an AO process.

*   **`init`**
    *   **Action:** Initializes the WASM environment for the process. It locates the WASM image (binary), starts a WAMR instance, and stores the instance handle and helper functions (for reading/writing WASM memory) in the process's private state (`priv/...`).
    *   **Inputs (Expected in Process Definition or `init` Message):**
        *   `[Prefix]/image`: The Arweave Transaction ID of the WASM binary, or the WASM binary itself, or a message containing the WASM binary in its body.
        *   `[Prefix]/Mode`: (Optional) Specifies execution mode (`WASM` (default) or `AOT` if allowed by node config).
    *   **Outputs (Stored in `priv/`):**
        *   `[Prefix]/instance`: The handle to the running WAMR instance.
        *   `[Prefix]/write`: A function to write data into the WASM instance's memory.
        *   `[Prefix]/read`: A function to read data from the WASM instance's memory.
        *   `[Prefix]/import-resolver`: A function used to handle calls *from* the WASM module back *to* the AO environment (imports).
*   **`compute`**
    *   **Action:** Executes a function within the initialized WASM instance. It retrieves the target function name and parameters from the incoming message or process definition and calls the WASM instance via `hb_beamr`.
    *   **Inputs (Expected in Process State or Incoming Message):**
        *   `priv/[Prefix]/instance`: The handle obtained during `init`.
        *   `function` or `body/function`: The name of the WASM function to call.
        *   `parameters` or `body/parameters`: A list of parameters to pass to the WASM function.
    *   **Outputs (Stored in `results/`):**
        *   `results/[Prefix]/type`: The result type returned by the WASM function.
        *   `results/[Prefix]/output`: The actual result value returned by the WASM function.
*   **`import`**
    *   **Action:** Handles calls originating *from* the WASM module (imports). The default implementation (`default_import_resolver`) resolves these calls by treating them as sub-calls within the AO environment, allowing WASM code to invoke other AO device functions or access process state via the `hb_ao:resolve` mechanism.
    *   **Inputs (Provided by `hb_beamr`):** Module name, function name, arguments, signature.
    *   **Response:** Returns the result of the resolved AO call back to the WASM instance.
*   **`snapshot`**
    *   **Action:** Captures the current memory state of the running WASM instance. This is used for checkpointing and restoring process state.
    *   **Inputs:** `priv/[Prefix]/instance`.
    *   **Outputs:** A message containing the raw binary snapshot of the WASM memory state, typically tagged with `[Prefix]/State`.
*   **`normalize` (Internal Helper)**
    *   **Action:** Ensures a consistent state representation for computation, primarily by loading a WASM instance from a snapshot (`[Prefix]/State`) if a live instance (`priv/[Prefix]/instance`) isn't already present. This allows resuming execution from a cached state.
*   **`terminate`**
    *   **Action:** Stops and cleans up the running WASM instance associated with the process.
    *   **Inputs:** `priv/[Prefix]/instance`.

## Usage within `dev_stack`

The `~wasm64@1.0` device is almost always used as part of an execution stack configured in the Process Definition Message and managed by [`dev_stack`](../resources/source-code/dev_stack.md). [`dev_stack`](../resources/source-code/dev_stack.md) ensures that `init` is called on the first pass, `compute` on subsequent passes, and potentially `snapshot` or `terminate` as needed.

```text
# Example Process Definition Snippet
Execution-Device: [`stack@1.0`](../resources/source-code/dev_stack.md)
Execution-Stack: "[`scheduler@1.0`](../resources/source-code/dev_scheduler.md)", "wasm64@1.0"
WASM-Image: <WASMImageTxID>
```

This setup allows AO processes to leverage the computational power and language flexibility offered by WebAssembly in a decentralized, verifiable manner.

[wasm module](../resources/source-code/dev_wasm.md)
