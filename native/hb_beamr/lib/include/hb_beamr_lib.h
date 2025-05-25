#ifndef HB_BEAMR_LIB_H
#define HB_BEAMR_LIB_H

#include "wasm_export.h" // For WAMR types like RuntimeInitArgs

// Opaque context handle declaration
// The actual struct definition will be in hb_beamr_lib.c
struct hb_beamr_lib_context;
typedef struct hb_beamr_lib_context hb_beamr_lib_context_t;

// Error codes for hb_beamr_lib functions
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
    // ... other specific errors to be added
} hb_beamr_lib_rc_t;

/**
 * @brief Initializes the global WAMR runtime.
 *
 * This function must be called once before any other WAMR operations.
 * It wraps wasm_runtime_full_init().
 *
 * @param init_args Pointer to the WAMR RuntimeInitArgs structure.
 *                  If NULL, default initialization arguments will be used.
 * @return HB_BEAMR_LIB_SUCCESS on success, or an error code on failure.
 */
hb_beamr_lib_rc_t hb_beamr_lib_init_runtime_global(RuntimeInitArgs *init_args);

/**
 * @brief Destroys the global WAMR runtime.
 *
 * This function must be called once when WAMR is no longer needed,
 * typically when the port driver is unloaded.
 * It wraps wasm_runtime_destroy().
 */
void hb_beamr_lib_destroy_runtime_global(void);

// Context Management
hb_beamr_lib_context_t* hb_beamr_lib_create_context(void);
void hb_beamr_lib_destroy_context(hb_beamr_lib_context_t* ctx);
const char* hb_beamr_lib_get_last_error(hb_beamr_lib_context_t* ctx); // Gets detailed error string

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

// Module and Instance Lifecycle
hb_beamr_lib_rc_t hb_beamr_lib_load_aot_module(hb_beamr_lib_context_t* ctx,
                                               uint8_t* aot_binary_buf,
                                               uint32_t aot_binary_size);

// New function for pre-registering natives globally
hb_beamr_lib_rc_t hb_beamr_lib_register_global_natives(const char* for_module_name,
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

// Memory Access (Wrappers around WAMR APIs) - Placeholders for now
// bool hb_beamr_lib_validate_app_addr(hb_beamr_lib_context_t* ctx, uint64_t app_offset, uint64_t size);
// void* hb_beamr_lib_addr_app_to_native(hb_beamr_lib_context_t* ctx, uint64_t app_offset);

#endif // HB_BEAMR_LIB_H 