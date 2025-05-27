#ifndef HB_BEAMR_LIB_H
#define HB_BEAMR_LIB_H

#ifndef HB_BEAMR_LIB_API
  #if defined(_WIN32) || defined(__CYGWIN__)
    #ifdef HB_BEAMR_LIB_COMPILING_DLL // Define this when building the DLL
      #define HB_BEAMR_LIB_API __declspec(dllexport)
    #else
      #define HB_BEAMR_LIB_API __declspec(dllimport)
    #endif
  #elif __GNUC__ >= 4 // Or __clang__
    #define HB_BEAMR_LIB_API __attribute__((visibility("default")))
  #else
    #define HB_BEAMR_LIB_API // Default to empty for other compilers/static linking
  #endif
#endif

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h> // For size_t
#include "wasm_export.h" // For WAMR types like RuntimeInitArgs, wasm_module_inst_t, wasm_memory_inst_t
// Note: wasm_val_t is also needed for call_export, it might come from wasm_export.h or a C-API header.
// If not, #include "wasm_c_api.h" or similar might be needed for wasm_val_t if it's used by functions here.

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
    HB_BEAMR_LIB_ERROR_WAMR_CALL_FAILED = 7,
    HB_BEAMR_LIB_ERROR_WAMR_VALIDATION_FAILED,
    HB_BEAMR_LIB_ERROR_INVALID_STATE,
    HB_BEAMR_LIB_ERROR_MODULE_NOT_LOADED,
    HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED,
    HB_BEAMR_LIB_ERROR_NATIVE_LINKING_FAILED = 8,
    HB_BEAMR_LIB_ERROR_SIGNATURE_PARSE_FAILED = 9,
    HB_BEAMR_LIB_ERROR_INVALID_ARGS = 10,            
    HB_BEAMR_LIB_ERROR_MEMORY_ACCESS_OUT_OF_BOUNDS = 11,
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

// New accessor
WASM_RUNTIME_API_EXTERN wasm_module_inst_t hb_beamr_lib_get_module_instance(hb_beamr_lib_context_t* ctx);

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

// Loads a WASM module from a binary buffer (can be .wasm or .aot)
// Note: wasm_runtime_load intelligently handles both.
// Consider if this should replace or complement hb_beamr_lib_load_aot_module.
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_load_wasm_module(
    hb_beamr_lib_context_t* ctx,
    uint8_t* wasm_binary_buf, // Input WASM or AOT binary
    uint32_t wasm_binary_size
);

// Retrieves information about an exported or default memory from the instance.
// If memory_name is NULL or empty, it attempts to get the default memory.
// out_memory_inst_ptr will point to the WAMR internal wasm_memory_inst_t.
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_get_memory_info(
    hb_beamr_lib_context_t* ctx,
    const char* memory_name,             // Name of the exported memory, or NULL/empty for default
    uint8_t** out_data_ptr,              // Output: Pointer to the start of the memory data
    size_t* out_data_size_bytes,         // Output: Size of the memory in bytes
    wasm_memory_inst_t* out_memory_inst_ptr // Output: Pointer to WAMR's internal memory instance handle
);

// Reads directly from the default Wasm memory of the instance.
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_direct_read_memory(
    hb_beamr_lib_context_t* ctx,
    size_t offset,                      // Byte offset in Wasm memory to start reading
    uint8_t* buffer,                    // Buffer to store the read data
    size_t length                       // Number of bytes to read
);

// Writes directly to the default Wasm memory of the instance.
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_direct_write_memory(
    hb_beamr_lib_context_t* ctx,
    size_t offset,                      // Byte offset in Wasm memory to start writing
    const uint8_t* buffer,              // Data to write
    size_t length                       // Number of bytes to write
);

// Calls an indirectly referenced function from a table.
// table_name is typically "__indirect_function_table" or the specific export name of the table.
// func_index is the index within that table.
// Args and results use wasm_val_t for consistency with call_export.
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_call_indirect(
    hb_beamr_lib_context_t* ctx,
    const char* table_name, // Name of the exported table (e.g., "__indirect_function_table")
    uint32_t func_index,    // Index of the function in the table
    uint32_t num_args,      // Number of arguments in the args array
    wasm_val_t args[],      // Arguments for the function call
    uint32_t num_results,   // Expected number of results
    wasm_val_t results[]    // Buffer to store results
);

#ifdef __cplusplus
// ... existing code ...
#endif

#endif // HB_BEAMR_LIB_H 