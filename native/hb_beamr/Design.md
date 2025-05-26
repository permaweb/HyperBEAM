# hb_beamr Port Driver and Library: High-Level Design

## 1. Introduction

This document describes the high-level design for the rewritten `hb_beamr` Erlang port driver and its associated Wasm (WebAssembly) interaction library. The design aims to create a modular, robust, and testable system by separating the Erlang-facing port driver logic from the Wasm runtime management and execution logic. This design is based on the plan outlined in `Improvements.md`.

## 2. Overall Architecture

The system will consist of two primary components:

1.  **`hb_beamr_lib` (Static C Library)**: A self-contained static library responsible for all interactions with the WebAssembly Micro Runtime (WAMR). It will manage the lifecycle of Wasm modules, execute Wasm functions, and handle Wasm memory access. It will expose a C API for use by the port driver.
2.  **`hb_beamr.so` (Erlang Port Driver)**: A shared library that implements the Erlang port protocol. It will serve as a bridge between the Erlang VM and `hb_beamr_lib`, handling Erlang term encoding/decoding and managing the state of Wasm instances as perceived by Erlang.

`hb_beamr.so` will statically link against `hb_beamr_lib` and the WAMR static library (`libvmlib.a`).

## 3. `hb_beamr_lib` - The Wasm Interaction Library

### 3.1. Purpose and Responsibilities

*   Mediate interactions with the Wasm C API (`wasm_c_api.h`), providing a higher-level, context-aware interface for Wasm operations.
*   Manage the global Wasm engine lifecycle (initialization and destruction).
*   Manage the lifecycle of individual Wasm execution contexts (store, module, instance), including loading Wasm binary modules, instantiation, and de-instantiation.
*   Provide mechanisms for linking native host functions (imports) that Wasm modules can call, using a trampoline mechanism.
*   Execute exported Wasm functions.
*   Provide controlled access to Wasm memory (indirectly, via store and instance management).
*   Implement a state machine to manage the Wasm module/instance state.
*   Provide clear error reporting.

### 3.2. Key Data Structures

*   **`hb_beamr_capi_lib_context_t`**: An opaque C struct (defined in C++) representing a single Wasm module instance and its associated environment (Wasm store, module, instance, error details, registered host functions, etc.). This will be the primary handle used by the port driver to interact with a specific Wasm instance.
*   **`hb_beamr_capi_lib_rc_t`**: An enumeration defining return codes for `hb_beamr_capi_lib` functions.
*   **`hb_beamr_capi_native_symbol_t`**: A structure to facilitate passing native symbol definitions (for host functions) from the port driver to `hb_beamr_capi_lib`. This is used to override or provide implementations for Wasm imports.
*   **`HostFuncEnv`**: Internal structure to manage the environment for trampoline host functions.

### 3.3. Conceptual Internal API (`hb_beamr_capi_lib.h`)

The library will expose a C API, defined in `hb_beamr_capi_lib.h`, for the port driver. These functions are designed to operate on the `hb_beamr_capi_lib_context_t`, managing Wasm C API resources and state internally, and providing consolidated error reporting. Key functions will include:

```c
// Opaque context handle (actual definition in C++)
typedef struct hb_beamr_capi_lib_context hb_beamr_capi_lib_context_t;

// Return codes (subset from hb_beamr_capi_lib.h)
typedef enum {
    HB_BEAMR_CAPI_LIB_SUCCESS = 0,
    HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED,
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_RUNTIME_INIT_FAILED, // Or ENGINE_INIT_FAILED
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_LOAD_FAILED,      // Or MODULE_LOAD_FAILED
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_INSTANTIATE_FAILED, // Or INSTANTIATE_FAILED
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED,
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_CALL_FAILED,
    HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE,
    HB_BEAMR_CAPI_LIB_ERROR_NATIVE_LINKING_FAILED,
    // ... other specific errors
} hb_beamr_capi_lib_rc_t;

// For registering native symbols (host functions)
// This structure is used to provide C functions for Wasm imports.
typedef struct {
    const char *import_module_name; // Import module name (e.g., "env")
    const char *import_function_name; // Import function name
    void       *user_function;      // Pointer to the C function (actual user callback)
                                      // This function must match user_host_func_callback_t:
                                      // wasm_trap_t* (*user_host_func_callback_t)(
                                      //     void* user_env, // will be hb_beamr_capi_lib_context_t*
                                      //     const wasm_val_vec_t* args,
                                      //     wasm_val_vec_t* results);
    // No signature string here, it's derived from the Wasm module's import section.
} hb_beamr_capi_native_symbol_t;

// Global Wasm Engine Management
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_init_runtime_global(wasm_config_t* config);
void hb_beamr_capi_lib_destroy_runtime_global(void);

// Context Management
hb_beamr_capi_lib_context_t* hb_beamr_capi_lib_create_context(void);
void hb_beamr_capi_lib_destroy_context(hb_beamr_capi_lib_context_t* ctx);
const char* hb_beamr_capi_lib_get_last_error(hb_beamr_capi_lib_context_t* ctx);

// Module and Instance Lifecycle
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_load_wasm_module(hb_beamr_capi_lib_context_t* ctx,
                                                          const uint8_t* wasm_binary_buf,
                                                          uint32_t wasm_binary_size);
// Note: AOT compilation is external to this library. This loads a standard .wasm binary.
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_instantiate(hb_beamr_capi_lib_context_t* ctx,
                                                     void* default_import_function, // Function pointer for unlinked imports
                                                     const hb_beamr_capi_native_symbol_t* override_symbols,
                                                     uint32_t num_override_symbols);
// Stack and heap sizes are typically configured via WAMR build options or runtime config,
// not directly in this instantiation call for the wasm_c_api.

// Function Execution
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_call_export(hb_beamr_capi_lib_context_t* ctx,
                                                     const char* func_name,
                                                     uint32_t num_args,
                                                     wasm_val_t args[], // C-API wasm_val_t
                                                     uint32_t num_results,
                                                     wasm_val_t results[]); // C-API wasm_val_t

// Import Handling (called by port driver after Erlang responds to an import call)
// This is still conceptual for how the port driver might signal back.
// The current hb_beamr_capi_lib.cpp has a placeholder.
// The actual host function (user_function in hb_beamr_capi_native_symbol_t)
// is called synchronously by the trampoline. If that function needs to communicate
// with Erlang and wait, it must manage that itself.
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_resolve_import(hb_beamr_capi_lib_context_t* ctx,
                                                        uint32_t num_results,
                                                        wasm_val_t results[]);

// Memory Access (Helper for getting the store, other access is via Wasm execution or host functions)
wasm_store_t* hb_beamr_capi_lib_context_get_store(hb_beamr_capi_lib_context_t* ctx);
// Other WAMR memory functions (e.g., wasm_runtime_validate_app_addr) would be used
// internally by host functions if they need to access Wasm memory directly.
// The C-API itself doesn't provide direct memory access functions on the instance/module.
// Access is typically through Wasm functions or via host functions that get a Wasm context.
```

### 3.4. State Management

`hb_beamr_capi_lib` will manage the state of each Wasm context (`hb_beamr_capi_lib_context_t`) internally. The states will align with those defined in `Improvements.md`, Section 3 (e.g., `UNINITIALIZED`, `RUNTIME_INITIALIZED` (Engine created), `CONTEXT_CREATED` (Store created), `MODULE_LOADED` (Module compiled/loaded into store), `INSTANTIATED`, `AWAITING_IMPORT` (if a host function blocks), `TRAPPED`, `DESTROYED`). API calls will validate the current state before proceeding.

### 3.5. Wasm C API (`wasm_c_api.h`) Interaction

Internally, `hb_beamr_capi_lib` will make direct calls to the Wasm C API (as defined in `wasm_c_api.h` and implemented by WAMR) to implement the functionalities exposed through `hb_beamr_capi_lib.h`. Key Wasm C API functions to be utilized are listed in Section 6 of this document. Interactions will include engine/store creation, module compilation/loading, instantiation, function lookup and invocation, and host function registration (via `wasm_func_new_with_env`).

### 3.6. Error Handling

Internal `hb_beamr_capi_lib` functions will return `hb_beamr_capi_lib_rc_t` error codes. A function like `hb_beamr_capi_lib_get_last_error(ctx)` will provide a more detailed, human-readable error message, stored as `std::string` in the context.

## 4. `hb_beamr.so` - The Erlang Port Driver

### 4.1. Purpose and Responsibilities

*   Implement the Erlang port driver interface (callbacks like `init`, `stop`, `control`/`output`).
*   Manage the mapping between Erlang port instances/PIDs and `hb_beamr_capi_lib_context_t` handles.
*   Decode Erlang terms received from `control` into C data structures suitable for `hb_beamr_capi_lib` API calls.
*   Call the appropriate `hb_beamr_capi_lib` functions based on decoded commands.
*   Translate `hb_beamr_capi_lib_rc_t` return codes and error messages into appropriate Erlang responses (e.g., atoms, tuples, exceptions).
*   Provide the concrete C functions that `hb_beamr_capi_lib` will register as Wasm host functions (imports). These C functions will handle communication with Erlang for pending import calls.

### 4.2. Erlang Driver Interface

The port driver will expose commands to Erlang for:
*   Initializing/destroying the global WAMR runtime (likely managed implicitly by driver `init`/`stop`).
*   Creating and destroying Wasm contexts (representing Wasm module instances).
*   Loading AOT-compiled Wasm modules into a context.
*   Instantiating loaded modules.
*   Calling exported Wasm functions.
*   Responding to Wasm import calls that have been forwarded to Erlang.

### 4.3. Interaction with `hb_beamr_lib`

The port driver (`hb_beamr_driver.c`) will include `hb_beamr_capi_lib.h` and link against `libhb_beamr_lib.a`. It will use the API provided by `hb_beamr_capi_lib` to perform all Wasm-related operations.

### 4.4. Handling Asynchronous Import Calls

1.  When a Wasm module calls an import:
    *   WAMR (implementing `wasm_c_api.h`) executes the `generic_trampoline_callback` registered via `wasm_func_new_with_env` for that import.
    *   The trampoline, using `HostFuncEnv`, calls the actual `user_function` (from `hb_beamr_capi_native_symbol_t`) provided by the port driver. This `user_function` receives the `hb_beamr_capi_lib_context_t*` as its environment.
2.  This port driver `user_function` is responsible for:
    *   Performing its logic. If it needs to communicate with Erlang asynchronously:
        *   It must package the import call details (module, function, arguments) into Erlang terms.
        *   Send a message to the controlling Erlang process.
        *   It then needs to block or manage a callback mechanism. **Crucially, the Wasm execution is paused within this `user_function` call.**
3.  When Erlang sends the results back to the port driver (e.g., via a `control` message or other IPC):
    *   The port driver logic (which is waiting for this response) decodes the Erlang terms into `wasm_val_t` results.
    *   These results are placed into the `wasm_val_vec_t* results` argument of the `user_function`.
    *   The `user_function` returns, and Wasm execution resumes with the provided results.
    *   The conceptual `hb_beamr_capi_lib_resolve_import` is not directly used in this synchronous trampoline model; the results are returned directly by the `user_function`. The existing stub might be for a different async model not currently implemented.

## 5. Directory Structure and Build

*   **`native/hb_beamr/lib/`**: Contains the source and build files for `hb_beamr_lib`.
    *   `lib/src/hb_beamr_lib.c` (and other C source files for the library).
    *   `lib/include/hb_beamr_lib.h` (public C API for the library).
    *   `lib/CMakeLists.txt` (to build `libhb_beamr_lib.a` and `libvmlib.a`).
    *   `lib/test/` (C-based unit tests for `hb_beamr_lib`).
*   **`native/hb_beamr/src/`**: Contains the source for the Erlang port driver.
    *   `src/hb_beamr_driver.c` (main C source for `hb_beamr.so`).
*   **`native/hb_beamr/include/`**: May contain any C headers shared specifically between the port driver and C-nodes if applicable (less likely for this design).
*   **`rebar.config`**: (At project root or Elixir equivalent) will define how `hb_beamr.so` is compiled and linked against `libhb_beamr_lib.a` and `libvmlib.a`. A `pre_hook` will invoke CMake to build the static libraries.

## 6. Key Wasm C API Functions Utilized (by `hb_beamr_capi_lib`)

The `hb_beamr_capi_lib` will primarily interact with WAMR using its implementation of the standard Wasm C API (`wasm_c_api.h`). Key functions include:

*   **Engine & Store Lifecycle:**
    *   `wasm_engine_new()` or `wasm_engine_new_with_config()`
    *   `wasm_engine_delete()`
    *   `wasm_store_new()`
    *   `wasm_store_delete()`
*   **Module Lifecycle:**
    *   `wasm_module_new()` (takes `wasm_byte_vec_t` with Wasm binary)
    *   `wasm_module_delete()`
    *   `wasm_module_imports()` (to get `wasm_importtype_vec_t`)
    *   `wasm_module_exports()` (to get `wasm_exporttype_vec_t`)
*   **Instance Lifecycle:**
    *   `wasm_instance_new()` (takes imports as `wasm_extern_vec_t`)
    *   `wasm_instance_delete()`
    *   `wasm_instance_exports()` (to get `wasm_extern_vec_t` of actual exports)
*   **Function Definition & Execution:**
    *   `wasm_func_new_with_env()` (to create host functions with a trampoline)
    *   `wasm_func_delete()` (though often managed by store)
    *   `wasm_func_call()`
    *   `wasm_func_as_extern()`
    *   `wasm_extern_as_func()`
*   **Types and Values:**
    *   `wasm_functype_params()`, `wasm_functype_results()`
    *   `wasm_valtype_kind()`
    *   Structs like `wasm_val_t`, `wasm_val_vec_t`, `wasm_byte_vec_t`, `wasm_name_t`.
*   **Traps & Errors:**
    *   `wasm_trap_new()`
    *   `wasm_trap_message()`
    *   `wasm_trap_delete()`
    *   Error messages are primarily managed internally in `hb_beamr_capi_lib_context_t` rather than relying on a global C API error function.

This design promotes separation of concerns, enhances testability of the Wasm logic independently from Erlang, and provides a clear path for managing Wasm execution using the standard Wasm C API. 