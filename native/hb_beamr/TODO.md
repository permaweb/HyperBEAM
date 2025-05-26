Okay, I've analyzed the current driver code (`hb_beamr.c`, `hb_helpers.c`, `hb_wasm.c`) and compared its Wasm-related functionality with the design goals and current implementation of `hb_beamr_capi_lib.c`.

The main difference is that the existing driver code in `hb_wasm.c` uses many WAMR-specific APIs (from `wasm_export.h`), while `hb_beamr_capi_lib.c` is correctly focused on using the standard `wasm_c_api.h`. The `hb_beamr_capi_lib.c` already covers the core lifecycle operations (engine, store, module, instance, function call, import handling via trampoline) using the standard C API.

Here's a summary of remaining considerations and potential enhancements for `hb_beamr_capi_lib.c`, which I will write into `native/hb_beamr/TODO.md`:

# TODO for hb_beamr_capi_lib.c

This list outlines remaining tasks and considerations for `hb_beamr_capi_lib.c` to fully align with the design goals and potentially incorporate relevant functionalities currently in the older driver structure (while adhering to `wasm_c_api.h`).

## Core API and Functionality Review

1.  **`hb_beamr_capi_lib_resolve_import` Functionality:**
    *   The current design (and implementation of `generic_trampoline_callback`) suggests that the user-supplied C host function (called by the trampoline) will block and return results/traps directly. This makes `hb_beamr_capi_lib_resolve_import` likely redundant.
    *   **Action**: Confirm this understanding. If redundant, remove `hb_beamr_capi_lib_resolve_import` and update documentation accordingly.

## Memory Access API (Optional, mirrors old driver features)

2.  **Direct Wasm Memory Access Helpers:**
    *   The old driver provided `read`, `write`, and `size` commands for direct memory manipulation using WAMR-specific helpers.
    *   If similar capabilities are desired from `hb_beamr_capi_lib.c` (accessible by the port driver), they must be implemented using `wasm_c_api.h` primitives.
    *   **Action**: Design and implement functions in `hb_beamr_capi_lib.h/.c` such as:
        *   `hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_get_export_memory_info(hb_beamr_capi_lib_context_t* ctx, const char* memory_name, uint8_t** out_data_ptr, size_t* out_data_size, wasm_memory_t** out_memory_ptr);` (to get memory pointer and size by export name).
        *   `hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_read_memory(hb_beamr_capi_lib_context_t* ctx, wasm_memory_t* memory, uint32_t offset, uint8_t* buffer, uint32_t length);`
        *   `hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_write_memory(hb_beamr_capi_lib_context_t* ctx, wasm_memory_t* memory, uint32_t offset, const uint8_t* buffer, uint32_t length);`
        *   `size_t hb_beamr_capi_lib_get_memory_size_bytes(wasm_memory_t* memory);` (wrapper for `wasm_memory_size` to get page count, and potentially a helper to get byte size).
    *   These would involve finding the `wasm_memory_t*` from instance exports and then using `wasm_memory_data()`, `wasm_memory_size()`, etc.
    *   **Status (Update after testing):** Implemented as per C-API spec. However, WAMR's C-API implementation appears to return NULL data pointer and 0 size for exported memories queried via `wasm_extern_as_memory()` followed by `wasm_memory_data()`/`wasm_memory_size()`, even if the Wasm instance internally uses the memory (verified by successful Wasm function calls operating on memory). This means direct memory access via these C-API calls on handles obtained from exports might not be feasible with WAMR as expected. The `hb_beamr_capi_lib.c` functions are C-API compliant, but tests for them will fail with WAMR under these conditions.

## Testing and Robustness

3.  **Comprehensive Testing:**
