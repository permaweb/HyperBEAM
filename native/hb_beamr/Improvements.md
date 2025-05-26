# hb_beamr Port Driver and Library Rewrite: Improvement Plan

This document outlines the proposed improvements for rewriting the `hb_beamr` Erlang port driver and its underlying WebAssembly interaction library. The primary goal is to create a robust, maintainable, and testable system by separating concerns: the Erlang port driver will handle Erlang-specific interactions, while a new C library will manage all WebAssembly (Wasm) operations.

## 1. Core Library Design (`hb_beamr_capi_lib`)

A new **static library**, `hb_beamr_capi_lib` (e.g., `libhb_beamr_lib.a`), will be created. This library will encapsulate all Wasm C API (`wasm_c_api.h`) interactions and Wasm module lifecycle management. The Erlang port driver (`hb_beamr.so`) will be a separate shared library that statically links against `hb_beamr_capi_lib` and WAMR's `vmlib` (which provides the `wasm_c_api.h` implementation).

**Key characteristics of `hb_beamr_capi_lib`:**

*   **Self-Contained Wasm Logic:** Minimal dependencies on Erlang driver specifics. It will be buildable and testable independently.
*   **Standard Wasm C API Usage:** The library will utilize the `wasm_c_api.h` API directly, as implemented by WAMR. Testing will be performed using this real API with actual Wasm modules.
*   **Explicit C API (Internal Modularization):** A well-defined C API (exposed via `hb_beamr_capi_lib.h`) for:
    *   Global Wasm engine initialization/destruction (`wasm_engine_t`).
    *   Creating/destroying an `hb_beamr_capi_lib_context_t` (representing a Wasm store, module, instance, and their environment).
    *   Loading **Wasm binary modules** (`.wasm` files) into a context. AOT compilation (e.g., using `wamrc`) is an external step; this library loads the standard binary format.
    *   Registering host functions (imports) for a context using a trampoline mechanism.
    *   Invoking exported Wasm functions.
    *   Managing Wasm resources (store, module, instance, etc.).
*   **Strict Resource Management:** Meticulous memory allocation and deallocation using C++ RAII principles internally where possible (e.g., for `wasm_importtype_vec_t`, `wasm_extern_vec_t`) and explicit deletion for core Wasm C API objects. Clear ownership rules for all resources.
*   **Comprehensive Error Handling:** Internal functions will use clear return codes (`hb_beamr_capi_lib_rc_t`). Detailed error messages are stored in the context (`std::string last_error_msg`).
*   **Testability:** C-based unit tests will link against `hb_beamr_capi_lib` and WAMR, executing real Wasm modules.

## 2. Port Driver (`hb_beamr.so`) Responsibilities

The Erlang port driver (`hb_beamr.so`, compiled from e.g. `hb_beamr_driver.c`) will serve as a bridge between the Erlang runtime and `hb_beamr_capi_lib`.

**Key responsibilities of the port driver:**

*   Implementing Erlang driver callbacks (`init`, `stop`, `control`/`output`).
*   Managing the lifecycle of `hb_beamr_capi_lib` contexts, mapping Erlang port instances to these contexts.
*   Decoding Erlang terms into C data structures suitable for `hb_beamr_capi_lib` API calls.
*   Calling `hb_beamr_capi_lib` functions.
*   Translating `hb_beamr_capi_lib` return codes/errors into Erlang responses or exceptions.
*   Providing the concrete C functions that `hb_beamr_capi_lib` will call (via trampoline) for Wasm imports. These C functions will handle the Erlang communication part of an import call.

## 3. State Management and State Machine Model (within `hb_beamr_capi_lib`)

The interaction with a Wasm module will follow a well-defined state machine, managed by `hb_beamr_capi_lib` within its context structure.

*   **`hb_beamr_capi_lib_context_t`:** An opaque handle (internal C++ struct in `hb_beamr_capi_lib.cpp`) representing the state of a Wasm module instance and its environment (store, module, instance, error message, host functions).
*   **States:**
    *   `UNINITIALIZED`: Global Wasm Engine not yet set up.
    *   `RUNTIME_INITIALIZED` (Engine Created): Global Wasm Engine (`wasm_engine_t`) is active. `hb_beamr_capi_lib_init_runtime_global()` moves to this state.
    *   `CONTEXT_CREATED` (Store Created): An `hb_beamr_capi_lib_context_t` exists, and a `wasm_store_t` is created within it.
    *   `MODULE_LOADED`: A Wasm binary module (`.wasm`) is loaded and compiled into a `wasm_module_t` within the context's store. `hb_beamr_capi_lib_load_wasm_module(context, wasm_binary_buf, ...)` transitions to this state.
    *   `INSTANTIATED`: The Wasm module is instantiated into a `wasm_instance_t`. `hb_beamr_capi_lib_instantiate(context, ...)` transitions here. Imports are linked during this phase using the provided native symbols and trampoline.
    *   `RUNNING_EXPORT`: An exported Wasm function is currently executing (conceptual state during `hb_beamr_capi_lib_call_export`).
    *   `AWAITING_IMPORT`: A Wasm function has called an import, and the Wasm C API (via WAMR) has invoked the `generic_trampoline_callback`. This trampoline then calls the user-supplied C function (from the port driver logic). If this user function blocks (e.g., waiting for Erlang), the Wasm execution is paused. The context is in this state from the perspective of the host.
    *   `TRAPPED`: The Wasm instance has encountered a trap/error during instantiation or execution.
    *   `DESTROYED`: Context and its Wasm resources (instance, module, store) are cleaned up.
    *   `RUNTIME_DESTROYED`: Global Wasm Engine shut down.
*   **Transitions:** Erlang commands received by `hb_beamr.so` will trigger calls to `hb_beamr_capi_lib` functions, which in turn manage these state transitions. Invalid operations for a given state will result in errors returned to Erlang via `hb_beamr.so`.
*   **Import Handling within State Machine:**
    1.  Wasm execution calls an import.
    2.  The Wasm C API (WAMR) calls the `generic_trampoline_callback` registered for that import via `wasm_func_new_with_env`.
    3.  This trampoline function in `hb_beamr_capi_lib.cpp` identifies the specific import, prepares arguments, and calls the corresponding C function pointer (the `user_function` from `hb_beamr_capi_native_symbol_t`, provided by the port driver). The `hb_beamr_capi_lib_context_t*` is passed as the environment to this user function. The Wasm execution is effectively paused inside this user function call.
    4.  The port driver's C user function handles the logic, potentially sending a message to Erlang and waiting for a response (blocking).
    5.  Once the Erlang response is received (or the user function completes its task), the user function populates the `wasm_val_vec_t* results` and returns. If a trap is to be signaled, it returns a `wasm_trap_t*`.
    6.  The Wasm C API (WAMR) resumes Wasm execution with the results or handles the trap. The state transitions out of `AWAITING_IMPORT` upon return from the user function.

## 4. Memory Management in `hb_beamr_capi_lib`

*   **Standard Allocators:** `hb_beamr_capi_lib` uses `new`/`delete` for its own C++ structures (like `hb_beamr_capi_lib_context_t`, `HostFuncEnv`) and for managing `std::vector` and `std::string`. The Wasm C API objects (e.g., `wasm_engine_t`, `wasm_store_t`, `wasm_module_t`, `wasm_instance_t`, `wasm_func_t`) are managed by their respective `_delete` functions from `wasm_c_api.h`.
*   **Ownership:**
    *   Binary data for Wasm modules passed for loading: `hb_beamr_capi_lib_load_wasm_module` copies this data into a `wasm_byte_vec_t` which is then passed to `wasm_module_new`. The `wasm_byte_vec_t` is deleted after `wasm_module_new` returns.
    *   `hb_beamr_capi_lib_context_t` owns its internal `wasm_store_t`, `wasm_module_t`, `wasm_instance_t`, and vectors of `HostFuncEnv*` and `wasm_func_t*` (for host functions). These are cleaned up in `hb_beamr_capi_lib_destroy_context`.
    *   `HostFuncEnv` objects are owned by the `hb_beamr_capi_lib_context_t` and deleted when the context is destroyed.
    *   `wasm_func_t` objects created for host functions are also tracked in the context and deleted (if necessary, though typically store-managed).
*   **Error Paths:** Rigorous cleanup of all allocated resources on error, primarily managed by the `hb_beamr_capi_lib_destroy_context` function and RAII for internal C++ collections.

## 5. Wasm C API Usage (`wasm_c_api.h` in `hb_beamr_capi_lib`)

*   **Direct Usage:** All interactions with the Wasm runtime will use the `wasm_c_api.h` functions, as implemented by WAMR. WAMR is configured as per `lib/CMakeLists.txt` (AOT is now an external step, other options like Memory64, SIMD, minimal libc still apply to the WAMR build itself). Wasm modules are loaded as standard `.wasm` binaries.
*   **Initialization:**
    *   `hb_beamr_capi_lib_init_runtime_global()`: Calls `wasm_engine_new_with_config()` (or `wasm_engine_new()`) once.
    *   `hb_beamr_capi_lib_destroy_runtime_global()`: Calls `wasm_engine_delete()` once.
*   **Module Lifecycle (API for `hb_beamr_capi_lib_context_t`):
    *   `hb_beamr_capi_lib_create_context()`: Creates `hb_beamr_capi_lib_context_t` and calls `wasm_store_new()`.
    *   `hb_beamr_capi_lib_load_wasm_module(context, ...)`:
        *   Uses `wasm_module_new()` with the Wasm binary to get a `wasm_module_t` associated with the context's store.
        *   Stores `wasm_module_t` in the context.
    *   `hb_beamr_capi_lib_instantiate(context, ...)`:
        *   Retrieves import types using `wasm_module_imports()`.
        *   For each functional import, creates a `wasm_func_t` using `wasm_func_new_with_env()`, passing the `generic_trampoline_callback` and a `HostFuncEnv` (which contains the port driver's user function pointer and the context itself).
        *   Collects these host functions as `wasm_extern_t*` into a `wasm_extern_vec_t`.
        *   Calls `wasm_instance_new()` with the module and the import extern vector to get `wasm_instance_t`.
    *   `hb_beamr_capi_lib_destroy_context(context)`: Calls `wasm_instance_delete()`, `wasm_module_delete()`, `wasm_store_delete()`, and cleans up `HostFuncEnv` and `wasm_func_t` collections.
*   **Function Calls:**
    *   `hb_beamr_capi_lib_call_export(context, ...)`:
        *   Uses `wasm_instance_exports()` to get all exports.
        *   Iterates through module export types (`wasm_module_exports()`) to find the target function by name and verify it's a function.
        *   Retrieves the `wasm_func_t*` using `wasm_extern_as_func()` from the instance's exports.
        *   Calls `wasm_func_call()`.
*   **Memory Access:** Direct memory access from the host into Wasm memory is not explicitly exposed by `hb_beamr_capi_lib`'s public API, but host functions (provided by the port driver) receive the `hb_beamr_capi_lib_context_t*` (which contains the `wasm_store_t*`) and can use Wasm C API functions if necessary to interact with memory, or rely on Wasm functions to pass/receive data.

## 6. Build System and Testing

*   **`hb_beamr_capi_lib` CMake Build:** The `CMakeLists.txt` in `native/hb_beamr/lib` builds `libhb_beamr_lib.a` (static library from C++ sources like `hb_beamr_capi_lib.cpp`). It also builds WAMR (`vmlib`) as a static library (providing `wasm_c_api.h` implementation).
*   **Port Driver (`hb_beamr.so`) Build (Rebar):** The `rebar.config` defines how `hb_beamr.so` is compiled from its C sources (e.g. `hb_beamr_driver.c`, common driver files) and linked statically against `libhb_beamr_lib.a` and `libvmlib.a`. The `pre_hook` (`make port_libs`) is responsible for invoking CMake to build these static libraries and place them where `rebar.config`'s linker paths expect them.
*   **Testing (`BUILD_TESTING` in CMake):
    *   When `BUILD_TESTING` is enabled, `wamrc` (WAMR AOT Compiler) might still be built if other parts of the project use it, but `hb_beamr_capi_lib` itself now consumes `.wasm` files directly.
    *   C-based unit tests (in `native/hb_beamr/lib/test`) will be compiled and linked against `libhb_beamr_lib.a` and `libvmlib.a`. These tests will load and execute test `.wasm` files using `hb_beamr_capi_lib` API functions.

By adopting these changes, `hb_beamr_capi_lib` will serve as a well-defined static library for Wasm operations using the standard Wasm C API, and `hb_beamr.so` will be the Erlang port driver. This separation enhances modularity, testability, and maintainability. 