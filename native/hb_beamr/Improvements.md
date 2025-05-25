# hb_beamr Port Driver and Library Rewrite: Improvement Plan

This document outlines the proposed improvements for rewriting the `hb_beamr` Erlang port driver and its underlying WebAssembly interaction library. The primary goal is to create a robust, maintainable, and testable system by separating concerns: the Erlang port driver will handle Erlang-specific interactions, while a new C library will manage all WebAssembly (Wasm) operations.

## 1. Core Library Design (`hb_beamr_lib`)

A new **static library**, `hb_beamr_lib` (e.g., `libhb_beamr_lib.a`), will be created. This library will encapsulate all WAMR (WebAssembly Micro Runtime) interactions and Wasm module lifecycle management. The Erlang port driver (`hb_beamr.so`) will be a separate shared library that statically links against `hb_beamr_lib` and WAMR's `vmlib`.

**Key characteristics of `hb_beamr_lib`:**

*   **Self-Contained Wasm Logic:** Minimal dependencies on Erlang driver specifics. It will be buildable and testable independently.
*   **Transparent WAMR API Usage:** The library will utilize the `wasm_export.h` API directly. Testing will be performed using this real API with actual Wasm modules, rather than mocking the WAMR API.
*   **Explicit C API (Internal Modularization):** A well-defined C API for:
    *   Global WAMR runtime initialization/destruction.
    *   Creating/destroying an `hb_beamr_lib` context (representing a Wasm module instance and its environment).
    *   Loading **AOT-compiled Wasm modules** (`.aot` files) into a context. Raw `.wasm` files must be pre-compiled to `.aot` format using `wamrc` (WAMR AOT Compiler) before loading.
    *   Registering host functions (imports) for a context.
    *   Invoking exported Wasm functions.
    *   Accessing Wasm memory.
    *   Managing Wasm resources.
*   **Strict Resource Management:** Meticulous memory allocation and deallocation. Clear ownership rules for all resources.
*   **Comprehensive Error Handling:** Internal functions will use clear error codes (e.g., `HB_BEAMR_LIB_SUCCESS`, `HB_BEAMR_LIB_ERROR_WAMR_API_FAILED`). These will be translated by the port driver into appropriate Erlang terms.
*   **Testability:** C-based unit tests will link against `hb_beamr_lib` and WAMR, executing real Wasm modules.

## 2. Port Driver (`hb_beamr.so`) Responsibilities

The Erlang port driver (`hb_beamr.so`, compiled from e.g. `hb_beamr.c`) will serve as a bridge between the Erlang runtime and `hb_beamr_lib`.

**Key responsibilities of the port driver:**

*   Implementing Erlang driver callbacks (`init`, `stop`, `control`/`output`).
*   Managing the lifecycle of `hb_beamr_lib` contexts, mapping Erlang port instances to these contexts.
*   Decoding Erlang terms into C data structures suitable for `hb_beamr_lib` API calls.
*   Calling `hb_beamr_lib` functions.
*   Translating `hb_beamr_lib` return codes/errors into Erlang responses or exceptions.
*   Providing the concrete C functions that `hb_beamr_lib` will call for Wasm imports. These C functions will handle the Erlang communication part of an import call.

## 3. State Management and State Machine Model (within `hb_beamr_lib`)

The interaction with a Wasm module will follow a well-defined state machine, managed by `hb_beamr_lib` within its context structure.

*   **`hb_beamr_lib_context_t`:** An opaque handle (internal struct in `hb_beamr_lib`) representing the state of a Wasm module instance and its environment.
*   **States:**
    *   `UNINITIALIZED`: Initial WAMR runtime not yet set up.
    *   `RUNTIME_INITIALIZED`: Global WAMR runtime is active. `hb_beamr_lib_init_runtime_global()` moves to this state.
    *   `CONTEXT_CREATED`: An `hb_beamr_lib_context_t` exists, but no module is loaded.
    *   `MODULE_LOADED`: An AOT Wasm module is loaded into the context, its imports and exports are known. `hb_beamr_lib_load_module(context, aot_binary, import_definitions)` transitions to this state. Import definitions will include module name, function name, signature, and a C function pointer (provided by the `hb_beamr.so` port driver) to handle the host call.
    *   `INSTANTIATED`: The Wasm module is instantiated. `hb_beamr_lib_instantiate(context)` transitions here.
    *   `RUNNING_EXPORT`: An exported Wasm function is currently executing.
    *   `AWAITING_IMPORT`: A Wasm function has called an import, and `hb_beamr_lib` (via WAMR) has invoked the port driver's C host function. This C function is now responsible for communicating with Erlang and eventually providing a result back to `hb_beamr_lib` to resume Wasm execution.
    *   `TRAPPED`: The Wasm instance has encountered a trap/error.
    *   `DESTROYED`: Context and its resources are cleaned up.
    *   `RUNTIME_DESTROYED`: Global WAMR runtime shut down.
*   **Transitions:** Erlang commands received by `hb_beamr.so` will trigger calls to `hb_beamr_lib` functions, which in turn manage these state transitions. Invalid operations for a given state will result in errors returned to Erlang via `hb_beamr.so`.
*   **Import Handling within State Machine:**
    1.  Wasm execution calls an import.
    2.  WAMR calls the generic native symbol function registered by `hb_beamr_lib` for that import slot.
    3.  This dispatcher function in `hb_beamr_lib` identifies the specific import, packages arguments, and calls the corresponding C function pointer (from the port driver logic, associated with this import during module loading). The context state transitions to `AWAITING_IMPORT`.
    4.  The port driver's C host function sends a message to Erlang and manages waiting for a response.
    5.  Once Erlang responds, the port driver logic calls an `hb_beamr_lib_resolve_import(context, results)` function.
    6.  `hb_beamr_lib` takes these results, provides them back to WAMR execution, and the state transitions out of `AWAITING_IMPORT`.

## 4. Memory Management in `hb_beamr_lib`

*   **Standard Allocators:** `hb_beamr_lib` will use standard `malloc`/`free`. WAMR will be configured to use these allocators via `RuntimeInitArgs` during `hb_beamr_lib_init_runtime_global()`.
*   **Ownership:**
    *   Binary data for Wasm AOT modules passed for loading will be handled by WAMR as per its `wasm_runtime_load()` semantics.
    *   Strings returned by `hb_beamr_lib` API calls (e.g., for error details if any) will have clear ownership rules (e.g., caller must free, or library uses static buffers per context).
*   **Error Paths:** Rigorous cleanup of all allocated resources on error.

## 5. WAMR API Usage (`wasm_export.h` in `hb_beamr_lib`)

*   **Direct Usage:** All interactions with WAMR will use the `wasm_export.h` functions. The WAMR runtime is configured as per `lib/CMakeLists.txt` (AOT-only, Memory64, SIMD, minimal libc). Wasm modules must be AOT-compiled and rely on host imports for functionalities not built into WAMR or the Wasm module itself.
*   **Initialization:**
    *   `hb_beamr_lib_init_runtime_global()`: Calls `wasm_runtime_full_init()` once.
    *   `hb_beamr_lib_destroy_runtime_global()`: Calls `wasm_runtime_destroy()` once (e.g. when driver unloads).
*   **Module Lifecycle (API for `hb_beamr_lib_context_t`):
    *   `hb_beamr_lib_load_module(context, ...)`:
        *   Uses `wasm_runtime_load()` with AOT binary to get a `wasm_module_t`.
        *   Inspects imports/exports.
        *   Stores `wasm_module_t`.
        *   Registers native symbols for defined imports using `wasm_runtime_register_natives_raw()`. `NativeSymbol` array constructed by `hb_beamr_lib` using definitions from port driver. Attachment links to port driver's C host function.
    *   `hb_beamr_lib_instantiate(context, ...)`: Calls `wasm_runtime_instantiate()` to get `wasm_module_inst_t`.
    *   `hb_beamr_lib_destroy_context(context)`: Calls `wasm_runtime_deinstantiate()`, `wasm_runtime_unload()`.
*   **Function Calls:**
    *   `hb_beamr_lib_call_export(context, ...)`: Uses `wasm_runtime_lookup_function()`, then `wasm_runtime_call_wasm_a()`. Manages `wasm_exec_env_t`.
*   **Memory Access:** `hb_beamr_lib` provides functions that wrap relevant WAMR memory APIs.

## 6. Build System and Testing

*   **`hb_beamr_lib` CMake Build:** The `CMakeLists.txt` in `native/hb_beamr/lib` builds `libhb_beamr_lib.a` (static library). It also builds WAMR (`vmlib`) as a static library.
*   **Port Driver (`hb_beamr.so`) Build (Rebar):** The `rebar.config` defines how `hb_beamr.so` is compiled from its C sources (e.g. `hb_beamr.c`, common driver files) and linked statically against `libhb_beamr_lib.a` and `libvmlib.a`. The `pre_hook` (`make port_libs`) is responsible for invoking CMake to build these static libraries and place them where `rebar.config`'s linker paths expect them.
*   **Testing (`BUILD_TESTING` in CMake):
    *   When `BUILD_TESTING` is enabled, `wamrc` (WAMR AOT Compiler) is built.
    *   C-based unit tests (in `native/hb_beamr/lib/test`) will be compiled and linked against `libhb_beamr_lib.a` and `libvmlib.a`. These tests will AOT-compile test `.wasm` files using the built `wamrc` and then execute these `.aot` files using `hb_beamr_lib` API functions.

By adopting these changes, `hb_beamr_lib` will serve as a well-defined static library for Wasm operations, and `hb_beamr.so` will be the Erlang port driver. This separation enhances modularity, testability, and maintainability. 