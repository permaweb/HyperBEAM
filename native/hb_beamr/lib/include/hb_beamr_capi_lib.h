#ifndef HB_BEAMR_CAPI_LIB_H
#define HB_BEAMR_CAPI_LIB_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>
#include "wasm_c_api.h" // For wasm_config_t, wasm_val_t, etc.

// Opaque context type
typedef struct hb_beamr_capi_lib_context hb_beamr_capi_lib_context_t;

// Return codes (mirroring hb_beamr_lib_rc_t)
typedef enum {
    HB_BEAMR_CAPI_LIB_SUCCESS = 0,
    HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE = 1,
    HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED = 2,
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_RUNTIME_INIT_FAILED = 3, // May mean engine creation failed
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_LOAD_FAILED = 4,       // May mean wasm_module_new failed
    HB_BEAMR_CAPI_LIB_ERROR_MODULE_NOT_LOADED = 5,
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_INSTANTIATE_FAILED = 6,// May mean wasm_instance_new failed
    HB_BEAMR_CAPI_LIB_ERROR_INSTANCE_NOT_CREATED = 7,
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED = 8,
    HB_BEAMR_CAPI_LIB_ERROR_WAMR_CALL_FAILED = 9,
    HB_BEAMR_CAPI_LIB_ERROR_NATIVE_LINKING_FAILED = 10,
    HB_BEAMR_CAPI_LIB_ERROR_SIGNATURE_PARSE_FAILED = 11,
} hb_beamr_capi_lib_rc_t;

// Native symbol structure for providing host functions during instantiation.
// The module_name here refers to the Wasm import module name (e.g., "env").
typedef struct {
    const char* import_module_name; // Wasm import module name (e.g., "env")
    const char* import_function_name; // Wasm import function name
    void* user_function;         // Pointer to the C host function
    const char* signature;       // Function signature string (e.g., "(ii)i")
    // void* attachment;         // Attachment is usually handled by the 'env' in wasm_func_new_with_env
} hb_beamr_capi_native_symbol_t;


// Initialize the runtime globally using wasm_c_api style config
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_init_runtime_global(wasm_config_t* config);
void hb_beamr_capi_lib_destroy_runtime_global(void);

// Context management
hb_beamr_capi_lib_context_t* hb_beamr_capi_lib_create_context(void);
void hb_beamr_capi_lib_destroy_context(hb_beamr_capi_lib_context_t* ctx);

// Error handling
const char* hb_beamr_capi_lib_get_last_error(hb_beamr_capi_lib_context_t* ctx);

// Accessor for a store pointer from the context, useful for host functions
wasm_store_t* hb_beamr_capi_lib_context_get_store(hb_beamr_capi_lib_context_t* ctx);

// Module loading (from .wasm binary using standard C API)
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_load_wasm_module(
    hb_beamr_capi_lib_context_t* ctx,
    const uint8_t* wasm_binary_buf, // Input WASM binary
    uint32_t wasm_binary_size
);

// Instantiate the loaded WASM module, linking native symbols (host functions).
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_instantiate(
    hb_beamr_capi_lib_context_t* ctx, 
    void* default_import_function, // User-provided C function pointer for unoverridden imports
    const hb_beamr_capi_native_symbol_t* override_symbols, // Array of specific host functions to override
    uint32_t num_override_symbols // Number of symbols in the override_symbols array
);

// Call an exported function by name.
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_call_export(
    hb_beamr_capi_lib_context_t* ctx,
    const char* func_name,
    uint32_t num_args,
    wasm_val_t args[],     // wasm_val_t is from wasm_c_api.h
    uint32_t num_results,
    wasm_val_t results[]   // wasm_val_t is from wasm_c_api.h
);

// Placeholder for resolving imports if async behavior is introduced (standard C API has no direct equivalent)
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_resolve_import(
    hb_beamr_capi_lib_context_t* ctx,
    uint32_t num_results,
    wasm_val_t results[]
);

#ifdef __cplusplus
}
#endif

#endif // HB_BEAMR_CAPI_LIB_H 