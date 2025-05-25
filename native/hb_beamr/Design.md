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

*   Mediate interactions with the WAMR API (`wasm_export.h`), providing a higher-level, context-aware interface for Wasm operations rather than a direct one-to-one mapping of WAMR functions.
*   Manage the global WAMR runtime lifecycle (initialization and destruction).
*   Manage the lifecycle of individual Wasm execution contexts, including loading AOT-compiled Wasm modules, instantiation, and de-instantiation.
*   Provide mechanisms for registering native host functions (imports) that Wasm modules can call.
*   Execute exported Wasm functions.
*   Provide controlled access to Wasm memory.
*   Implement a state machine to manage the Wasm module/instance state.
*   Provide clear error reporting.

### 3.2. Key Data Structures

*   **`hb_beamr_lib_context_t`**: An opaque C struct representing a single Wasm module instance and its associated environment (WAMR module, instance, execution environment, state, error details, etc.). This will be the primary handle used by the port driver to interact with a specific Wasm instance.
*   **`hb_beamr_lib_error_t`**: An enumeration or set of constants defining error codes returned by `hb_beamr_lib` functions.
*   **`hb_beamr_native_symbol_t`**: A structure to facilitate passing native symbol definitions (for host functions) from the port driver to `hb_beamr_lib`, compatible with WAMR's `NativeSymbol` structure.

### 3.3. Conceptual Internal API (`hb_beamr_lib.h`)

The library will expose a C API, conceptually defined in `hb_beamr_lib.h`, for the port driver. These functions are designed to operate on the `hb_beamr_lib_context_t`, managing WAMR resources and state internally, and providing consolidated error reporting. Key functions will include:

```c
// Opaque context handle
typedef struct hb_beamr_lib_context hb_beamr_lib_context_t;

// Error codes
typedef enum {
    HB_BEAMR_LIB_SUCCESS = 0,
    HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED,
    HB_BEAMR_LIB_ERROR_WAMR_RUNTIME_INIT_FAILED,
    HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED,
    HB_BEAMR_LIB_ERROR_WAMR_INSTANTIATE_FAILED,
    HB_BEAMR_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED,
    HB_BEAMR_LIB_ERROR_WAMR_CALL_FAILED,
    HB_BEAMR_LIB_ERROR_WAMR_VALIDATION_FAILED,
    HB_BEAMR_LIB_ERROR_INVALID_STATE,
    HB_BEAMR_LIB_ERROR_MODULE_NOT_LOADED,
    HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED,
    // ... other specific errors
} hb_beamr_lib_rc_t;

// For registering native symbols (host functions)
// This structure mirrors WAMR's NativeSymbol but allows for easier construction
// by the port driver. The void* user_function is the C function in the port driver.
typedef struct {
    const char *module_name; // Import module name (e.g., "env")
    const char *function_name; // Import function name
    void       *user_function; // Pointer to the C function in hb_beamr.so
    const char *signature;     // WAMR signature string, e.g., "(ii)i"
    void       *attachment;     // Optional attachment for the native function
} hb_beamr_native_symbol_t;

// Global WAMR Runtime Management
hb_beamr_lib_rc_t hb_beamr_lib_init_runtime_global(RuntimeInitArgs *init_args);
void hb_beamr_lib_destroy_runtime_global(void);

// Context Management
hb_beamr_lib_context_t* hb_beamr_lib_create_context(void);
void hb_beamr_lib_destroy_context(hb_beamr_lib_context_t* ctx);
const char* hb_beamr_lib_get_last_error(hb_beamr_lib_context_t* ctx); // Gets detailed error string

// Module and Instance Lifecycle
hb_beamr_lib_rc_t hb_beamr_lib_load_aot_module(hb_beamr_lib_context_t* ctx,
                                               uint8_t* aot_binary_buf,
                                               uint32_t aot_binary_size,
                                               const hb_beamr_native_symbol_t* import_symbols,
                                               uint32_t num_import_symbols);
hb_beamr_lib_rc_t hb_beamr_lib_instantiate(hb_beamr_lib_context_t* ctx,
                                           uint32_t stack_size,
                                           uint32_t heap_size);

// Function Execution
hb_beamr_lib_rc_t hb_beamr_lib_call_export(hb_beamr_lib_context_t* ctx,
                                           const char* func_name,
                                           uint32_t num_args,
                                           wasm_val_t args[],
                                           uint32_t num_results,
                                           wasm_val_t results[]);

// Import Handling (called by port driver after Erlang responds to an import call)
hb_beamr_lib_rc_t hb_beamr_lib_resolve_import(hb_beamr_lib_context_t* ctx,
                                              uint32_t num_results,
                                              wasm_val_t results[]);

// Memory Access (Wrappers around WAMR APIs)
bool hb_beamr_lib_validate_app_addr(hb_beamr_lib_context_t* ctx, uint64_t app_offset, uint64_t size);
void* hb_beamr_lib_addr_app_to_native(hb_beamr_lib_context_t* ctx, uint64_t app_offset);
// ... other memory functions as needed
```

### 3.4. State Management

`hb_beamr_lib` will manage the state of each Wasm context (`hb_beamr_lib_context_t`) internally. The states will align with those defined in `Improvements.md`, Section 3 (e.g., `UNINITIALIZED`, `RUNTIME_INITIALIZED`, `CONTEXT_CREATED`, `MODULE_LOADED`, `INSTANTIATED`, `AWAITING_IMPORT`, `TRAPPED`, `DESTROYED`). API calls will validate the current state before proceeding.

### 3.5. WAMR API (`wasm_export.h`) Interaction

Internally, `hb_beamr_lib` will make direct calls to the WAMR C API (as defined in `wasm_export.h`) to implement the functionalities exposed through `hb_beamr_lib.h`. Key WAMR functions to be utilized are listed in Section 6 of this document. Interactions will include runtime initialization, module loading, instantiation, function lookup and invocation, and native symbol registration.

### 3.6. Error Handling

Internal `hb_beamr_lib` functions will return `hb_beamr_lib_rc_t` error codes. A function like `hb_beamr_lib_get_last_error(ctx)` will provide a more detailed, human-readable error message, potentially incorporating WAMR's own error messages (`wasm_runtime_get_exception`).

## 4. `hb_beamr.so` - The Erlang Port Driver

### 4.1. Purpose and Responsibilities

*   Implement the Erlang port driver interface (callbacks like `init`, `stop`, `control`/`output`).
*   Manage the mapping between Erlang port instances/PIDs and `hb_beamr_lib_context_t` handles.
*   Decode Erlang terms received from `control` into C data structures suitable for `hb_beamr_lib` API calls.
*   Call the appropriate `hb_beamr_lib` functions based on decoded commands.
*   Translate `hb_beamr_lib_rc_t` return codes and error messages into appropriate Erlang responses (e.g., atoms, tuples, exceptions).
*   Provide the concrete C functions that `hb_beamr_lib` will register as Wasm host functions (imports). These C functions will handle communication with Erlang for pending import calls.

### 4.2. Erlang Driver Interface

The port driver will expose commands to Erlang for:
*   Initializing/destroying the global WAMR runtime (likely managed implicitly by driver `init`/`stop`).
*   Creating and destroying Wasm contexts (representing Wasm module instances).
*   Loading AOT-compiled Wasm modules into a context.
*   Instantiating loaded modules.
*   Calling exported Wasm functions.
*   Responding to Wasm import calls that have been forwarded to Erlang.

### 4.3. Interaction with `hb_beamr_lib`

The port driver (`hb_beamr_driver.c`) will include `hb_beamr_lib.h` and link against `libhb_beamr_lib.a`. It will use the API provided by `hb_beamr_lib` to perform all Wasm-related operations.

### 4.4. Handling Asynchronous Import Calls

1.  When a Wasm module calls an import, WAMR (via `hb_beamr_lib`) will invoke a C function provided by `hb_beamr.so` (this function pointer is passed during `hb_beamr_lib_load_aot_module`).
2.  This port driver C function will:
    *   Transition the `hb_beamr_lib_context_t` state to `AWAITING_IMPORT` (likely done by `hb_beamr_lib` itself when it dispatches to the host function).
    *   Encode the import call details (module, function, arguments) into Erlang terms.
    *   Send a message to the controlling Erlang process.
    *   The port driver will then wait for a response from Erlang (e.g., via a `control` message).
3.  When Erlang sends the results of the import back to the port driver:
    *   The driver decodes the Erlang terms into `wasm_val_t` results.
    *   It calls `hb_beamr_lib_resolve_import(ctx, results_count, results_array)` to provide the results back to the Wasm execution.
    *   `hb_beamr_lib` will then resume Wasm execution via WAMR.

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

## 6. Key WAMR API Functions Utilized (by `hb_beamr_lib`)

The `hb_beamr_lib` will primarily interact with WAMR using the following functions from `wasm_export.h`:

*   **Runtime Lifecycle:**
    *   `wasm_runtime_full_init()`
    *   `wasm_runtime_destroy()`
*   **Module Lifecycle:**
    *   `wasm_runtime_load()` (for AOT binary)
    *   `wasm_runtime_unload()`
    *   `wasm_runtime_register_natives_raw()` (or `wasm_runtime_register_natives`)
    *   `wasm_runtime_instantiate()`
    *   `wasm_runtime_deinstantiate()`
*   **Function Execution:**
    *   `wasm_runtime_lookup_function()`
    *   `wasm_runtime_call_wasm_a()`
    *   `wasm_runtime_create_exec_env()` (if managing exec_env explicitly, or rely on internal ones for simple calls)
    *   `wasm_runtime_destroy_exec_env()`
*   **Memory Access:**
    *   `wasm_runtime_validate_app_addr()`
    *   `wasm_runtime_addr_app_to_native()`
    *   (and others as needed for robust memory interaction from host)
*   **Error Handling:**
    *   `wasm_runtime_get_exception()`

This design promotes separation of concerns, enhances testability of the Wasm logic independently from Erlang, and provides a clear path for managing Wasm execution. 