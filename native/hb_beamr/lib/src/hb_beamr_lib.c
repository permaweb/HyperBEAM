#include "hb_beamr_lib.h"
#include "wasm_export.h"
#include <stdlib.h> // For malloc/free, though WAMR uses its own allocator setup
#include <string.h> // For memset, strcpy
#include <stdarg.h> // For va_list, va_start, va_end, vsnprintf
#include <stdio.h>  // For vsnprintf, snprintf
#include <stdbool.h> // For bool type
#include <pthread.h> // For mutex to guard global runtime operations
#include <stdatomic.h> // For atomic bool if available
#include <ei.h>

#define MAX_LAST_ERROR_MSG_SIZE 256

// Global state for the library
static atomic_bool g_wamr_runtime_initialized = ATOMIC_VAR_INIT(false);
static RuntimeInitArgs g_wamr_init_args;
// A mutex to protect WAMR runtime initialization/destruction and global native
// registration operations.  This avoids races when multiple threads attempt
// to initialise or tear-down the runtime concurrently, or when one thread is
// registering natives while another thread is instantiating modules.
//
// The mutex is kept internal to this translation unit to avoid leaking a
// synchronisation primitive into the public API.
static pthread_mutex_t g_wamr_runtime_mutex = PTHREAD_MUTEX_INITIALIZER;
// We need to be careful if g_wamr_init_args itself points to memory that might be freed.
// For now, assuming the relevant parts (allocator settings) are by value or long-lived.
// The RuntimeInitArgs struct itself will be copied.

// Definition of the opaque context struct
struct hb_beamr_lib_context {
    wasm_module_t wasm_module;          // WAMR module handle
    wasm_module_inst_t module_inst;    // WAMR module instance handle
    wasm_exec_env_t exec_env;          // WAMR execution environment for the current instance
    char last_error_msg[MAX_LAST_ERROR_MSG_SIZE]; // Buffer for the last error message
    // NEW: keep an owned copy of the original binary buffer so that
    //       WAMR's internal pointers into it remain valid for the
    //       entire lifetime of the module.
    uint8_t *owned_module_bytes;
    uint32_t owned_module_size;
    // TODO: Add state management enum as per Design.md/Improvements.md
    // hb_beamr_lib_state_t current_state;
};

// Helper function to set the last error message in a context
// This is an internal helper, not exposed in the header.
static void set_error_msg(hb_beamr_lib_context_t* ctx, const char* msg) {
    if (ctx) {
        strncpy(ctx->last_error_msg, msg, MAX_LAST_ERROR_MSG_SIZE - 1);
        ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\0'; // Ensure null termination
    }
}

static void set_error_msg_v(hb_beamr_lib_context_t* ctx, const char* format, ...) {
    if (ctx) {
        va_list args;
        va_start(args, format);
        vsnprintf(ctx->last_error_msg, MAX_LAST_ERROR_MSG_SIZE, format, args);
        va_end(args);
        ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\0'; // Ensure null termination
    }
}

hb_beamr_lib_rc_t hb_beamr_lib_init_runtime_global(RuntimeInitArgs *init_args) {
    pthread_mutex_lock(&g_wamr_runtime_mutex);

    if (atomic_load_explicit(&g_wamr_runtime_initialized, memory_order_acquire)) {
        pthread_mutex_unlock(&g_wamr_runtime_mutex);
        // Already initialized by another thread.
        return HB_BEAMR_LIB_SUCCESS;
    }

    RuntimeInitArgs effective_init_args;
    if (init_args) {
        effective_init_args = *init_args; // Copy user-provided args
    } else {
        memset(&effective_init_args, 0, sizeof(RuntimeInitArgs));
        effective_init_args.mem_alloc_type = Alloc_With_System_Allocator;
        effective_init_args.max_thread_num = 1; 
    }

    if (!wasm_runtime_full_init(&effective_init_args)) {
        pthread_mutex_unlock(&g_wamr_runtime_mutex);
        return HB_BEAMR_LIB_ERROR_WAMR_RUNTIME_INIT_FAILED;
    }

    // Store the effective init args for potential re-use (e.g., in register_global_natives)
    g_wamr_init_args = effective_init_args;
    atomic_store_explicit(&g_wamr_runtime_initialized, true, memory_order_release);
    pthread_mutex_unlock(&g_wamr_runtime_mutex);
    return HB_BEAMR_LIB_SUCCESS;
}

void hb_beamr_lib_destroy_runtime_global(void) {
    pthread_mutex_lock(&g_wamr_runtime_mutex);

    if (atomic_load_explicit(&g_wamr_runtime_initialized, memory_order_acquire)) {
        wasm_runtime_destroy();
        atomic_store_explicit(&g_wamr_runtime_initialized, false, memory_order_release);
        memset(&g_wamr_init_args, 0, sizeof(RuntimeInitArgs)); // Clear stored args
    }

    pthread_mutex_unlock(&g_wamr_runtime_mutex);
}

// Implementation for hb_beamr_lib_create_context
hb_beamr_lib_context_t* hb_beamr_lib_create_context(void) {
    hb_beamr_lib_context_t* ctx = (hb_beamr_lib_context_t*)malloc(sizeof(hb_beamr_lib_context_t));
    if (!ctx) {
        // Not much we can do if allocation fails here, cant even set last_error_msg in a context
        // The port driver will have to handle a NULL return.
        return NULL;
    }
    memset(ctx, 0, sizeof(hb_beamr_lib_context_t));
    // Initialize context members
    ctx->wasm_module = NULL;
    ctx->module_inst = NULL;
    // ctx->current_state = CONTEXT_CREATED; // When state enum is added
    strcpy(ctx->last_error_msg, "No error"); // Default initial message
    ctx->owned_module_bytes = NULL;
    ctx->owned_module_size = 0;
    return ctx;
}

// Implementation for hb_beamr_lib_destroy_context
void hb_beamr_lib_destroy_context(hb_beamr_lib_context_t* ctx) {
    if (ctx) {
        if (ctx->exec_env) {
            wasm_runtime_destroy_exec_env(ctx->exec_env);
            ctx->exec_env = NULL;
        }
        if (ctx->module_inst) {
            wasm_runtime_deinstantiate(ctx->module_inst);
            ctx->module_inst = NULL;
        }
        if (ctx->wasm_module) {
            wasm_runtime_unload(ctx->wasm_module);
            ctx->wasm_module = NULL;
        }
        if (ctx->owned_module_bytes) {
            free(ctx->owned_module_bytes);
            ctx->owned_module_bytes = NULL;
            ctx->owned_module_size = 0;
        }
        free(ctx);
    }
}

// Implementation for hb_beamr_lib_get_last_error
const char* hb_beamr_lib_get_last_error(hb_beamr_lib_context_t* ctx) {
    if (ctx && ctx->last_error_msg[0] != '\0') {
        return ctx->last_error_msg;
    }
    // This case should ideally not be hit if create_context initializes last_error_msg
    return "Context is NULL or error message not set.";
}

hb_beamr_lib_rc_t hb_beamr_lib_load_aot_module(
    hb_beamr_lib_context_t* ctx,
    uint8_t* aot_binary_buf,
    uint32_t aot_binary_size
) {
    if (!ctx) {
        return HB_BEAMR_LIB_ERROR_INVALID_STATE;
    }
    if (ctx->wasm_module) {
        set_error_msg(ctx, "Module already loaded in this context.");
        return HB_BEAMR_LIB_ERROR_INVALID_STATE;
    }
    if (!aot_binary_buf || aot_binary_size == 0) {
        set_error_msg(ctx, "AOT binary buffer is NULL or size is zero.");
        return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED;
    }

    char error_buf[128];

    // Own a copy of the buffer for the same lifetime reasons discussed in
    // hb_beamr_lib_load_wasm_module().  For AOT files this is technically not
    // required by WAMR today, but doing so makes the code path consistent and
    // future-proof.
    uint8_t *owned_copy = (uint8_t *)malloc(aot_binary_size);
    if (!owned_copy) {
        set_error_msg(ctx, "Failed to allocate memory for internal aot copy.");
        return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
    }
    memcpy(owned_copy, aot_binary_buf, aot_binary_size);

    ctx->wasm_module = wasm_runtime_load(owned_copy, aot_binary_size, error_buf, sizeof(error_buf));
    if (!ctx->wasm_module) {
        set_error_msg_v(ctx, "Failed to load AOT module: %s", error_buf);
        free(owned_copy);
        return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED;
    }

    ctx->owned_module_bytes = owned_copy;
    ctx->owned_module_size = aot_binary_size;

    // Native symbol registration moved to hb_beamr_lib_instantiate

    set_error_msg(ctx, "Module loaded successfully (no symbols registered at this stage).");
    return HB_BEAMR_LIB_SUCCESS;
}

hb_beamr_lib_rc_t hb_beamr_lib_instantiate(
    hb_beamr_lib_context_t* ctx,
    uint32_t stack_size, 
    uint32_t heap_size,
    void* user_data_override
) {
    if (!ctx) {
        return HB_BEAMR_LIB_ERROR_INVALID_STATE;
    }
    if (!ctx->wasm_module) {
        set_error_msg(ctx, "Module not loaded, cannot instantiate.");
        return HB_BEAMR_LIB_ERROR_MODULE_NOT_LOADED;
    }
    if (ctx->module_inst) {
        set_error_msg(ctx, "Module already instantiated.");
        return HB_BEAMR_LIB_ERROR_INVALID_STATE;
    }

    // --- Debug: Print module imports and exports BEFORE instantiation ---
    // fprintf(stderr, "[DEBUG hb_beamr_lib_instantiate] Pre-instantiation module info for wasm_module: %p\n", (void*)ctx->wasm_module);
    // if (ctx->wasm_module) { ... existing print logic ... }
    // --- End Debug ---

    uint32_t effective_heap_size = heap_size > 0 ? heap_size : 0;
    uint32_t effective_stack_size = stack_size > 0 ? stack_size : (128 * 1024);

    char error_buf[128];
    ctx->module_inst = wasm_runtime_instantiate(ctx->wasm_module, effective_stack_size, effective_heap_size, error_buf, sizeof(error_buf));

    if (!ctx->module_inst) {
        set_error_msg_v(ctx, "Failed to instantiate module: %s", error_buf);
        // Native symbols were registered globally; they persist. No specific cleanup needed here for them.
        return HB_BEAMR_LIB_ERROR_WAMR_INSTANTIATE_FAILED;
    }

    ctx->exec_env = wasm_runtime_create_exec_env(ctx->module_inst, effective_stack_size);
    if (!ctx->exec_env) {
        wasm_runtime_deinstantiate(ctx->module_inst);
        ctx->module_inst = NULL;
        set_error_msg(ctx, "Failed to create execution environment for instance.");
        return HB_BEAMR_LIB_ERROR_WAMR_INSTANTIATE_FAILED;
    }

    if (user_data_override) {
        wasm_runtime_set_custom_data(ctx->module_inst, user_data_override);
    } else {
        // If no override, use the context itself.
        wasm_runtime_set_custom_data(ctx->module_inst, (void*)ctx);
    }

    set_error_msg(ctx, "Module instantiated successfully.");
    return HB_BEAMR_LIB_SUCCESS;
}

hb_beamr_lib_rc_t hb_beamr_lib_call_export(
    hb_beamr_lib_context_t* ctx,
    const char* func_name,
    uint32_t num_args,
    wasm_val_t args[],
    uint32_t num_results,
    wasm_val_t results[]
) {
    if (!ctx || !ctx->module_inst || !ctx->exec_env) {
        if (ctx) set_error_msg(ctx, "Context, instance, or exec_env not initialized for call_export.");
        return HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED; // Or invalid state
    }
    if (!func_name) {
        set_error_msg(ctx, "Function name is NULL for call_export.");
        return HB_BEAMR_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED;
    }

    wasm_function_inst_t func_inst = wasm_runtime_lookup_function(ctx->module_inst, func_name);
    if (!func_inst) {
        set_error_msg_v(ctx, "Failed to lookup exported function: %s", func_name);
        return HB_BEAMR_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED;
    }

    // Verify arity (param_count and result_count) if possible/needed.
    // wasm_func_get_param_count(func_inst, ctx->module_inst) requires module_inst, which we have.
    // wasm_func_get_result_count(func_inst, ctx->module_inst)
    // For now, assume caller provides correct counts and types.

    // ctx->current_state = RUNNING_EXPORT;
    if (!wasm_runtime_call_wasm_a(ctx->exec_env, func_inst, num_results, results, num_args, args)) {
        const char* exception = wasm_runtime_get_exception(ctx->module_inst);
        set_error_msg_v(ctx, "Error calling wasm function '%s': %s", func_name, exception ? exception : "N/A");
        // ctx->current_state = TRAPPED; (or back to INSTANTIATED if exception is cleared)
        // Consume the exception
        wasm_runtime_clear_exception(ctx->module_inst);
        return HB_BEAMR_LIB_ERROR_WAMR_CALL_FAILED;
    }
    // ctx->current_state = INSTANTIATED; (after successful call)
    // Error message should probably not be overwritten on success here, 
    // or set to something like "Function call successful"
    // set_error_msg_v(ctx, "Function '%s' called successfully.", func_name);
    return HB_BEAMR_LIB_SUCCESS;
}

hb_beamr_lib_rc_t hb_beamr_lib_resolve_import(
    hb_beamr_lib_context_t* ctx,
    uint32_t num_results,
    wasm_val_t results[]
) {
    if (!ctx || !ctx->module_inst || !ctx->exec_env) {
        if (ctx) set_error_msg(ctx, "Context, instance, or exec_env not initialized for resolve_import.");
        return HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED; // Or invalid state
    }

    // This function will be called by the port driver when Erlang provides
    // the results for a pending import. WAMR needs to be informed to resume execution.
    // The actual mechanism for resuming WAMR after an import call returns from the host
    // needs to be carefully reviewed with WAMR's API for asynchronous host functions or
    // how it handles host functions that block and then resume.

    // For now, this is a placeholder. The Design.md Section 3. Import Handling implies
    // that the host C function (called by WAMR) will block or manage async state,
    // and then this resolve_import is called from the same thread or a way that can
    // signal the original WAMR-invoked host function to return the results to WAMR.

    // If using WAMR's fiber-based approach or asyncify, the call to resume might be different.
    // If the host function (user_function) itself blocks and waits for this, then this function
    // might just fill a shared structure that the host function then reads.

    // TODO: Implement actual WAMR resumption logic based on chosen import handling strategy.
    set_error_msg(ctx, "hb_beamr_lib_resolve_import - Not yet implemented.");
    return HB_BEAMR_LIB_ERROR_INVALID_STATE; // Placeholder return
}

// Implementation for hb_beamr_lib_load_wasm_module
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_load_wasm_module(
    hb_beamr_lib_context_t* ctx,
    uint8_t* wasm_binary_buf,
    uint32_t wasm_binary_size
) {
    if (!ctx) {
        return HB_BEAMR_LIB_ERROR_INVALID_STATE; // Or a more specific NULL context error if desired
    }
    if (ctx->wasm_module) {
        set_error_msg(ctx, "Module already loaded in this context.");
        return HB_BEAMR_LIB_ERROR_INVALID_STATE;
    }
    if (!wasm_binary_buf || wasm_binary_size == 0) {
        set_error_msg(ctx, "WASM binary buffer is NULL or size is zero.");
        return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED; // Or INVALID_ARGS
    }

    char error_buf[128]; // WAMR functions often use a small error buffer
    // Make an owned copy of the incoming buffer so that the original may be
    // freed by the caller immediately after this function returns.
    uint8_t *owned_copy = (uint8_t *)malloc(wasm_binary_size);
    if (!owned_copy) {
        set_error_msg(ctx, "Failed to allocate memory for internal wasm copy.");
        return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
    }
    memcpy(owned_copy, wasm_binary_buf, wasm_binary_size);

    ctx->wasm_module = wasm_runtime_load(owned_copy, wasm_binary_size, error_buf, sizeof(error_buf));
    if (!ctx->wasm_module) {
        set_error_msg_v(ctx, "Failed to load WASM/AOT module: %s", error_buf);
        free(owned_copy);
        return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED;
    }

    // Keep the copy around for the lifetime of the context
    ctx->owned_module_bytes = owned_copy;
    ctx->owned_module_size = wasm_binary_size;

    set_error_msg(ctx, "WASM/AOT module loaded successfully.");
    return HB_BEAMR_LIB_SUCCESS;
}

// Implementation for hb_beamr_lib_get_memory_info
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_get_memory_info(
    hb_beamr_lib_context_t* ctx,
    const char* memory_name,             
    uint8_t** out_data_ptr,              
    size_t* out_data_size_bytes,         
    wasm_memory_inst_t* out_memory_inst_ptr 
) {
    if (!ctx || !ctx->module_inst) {
        if (ctx) set_error_msg(ctx, "Instance not created/valid, cannot get memory info.");
        return HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED;
    }
    if (!out_data_ptr || !out_data_size_bytes || !out_memory_inst_ptr) {
        if (ctx) set_error_msg(ctx, "Invalid output arguments for hb_beamr_lib_get_memory_info.");
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    }

    *out_data_ptr = NULL;
    *out_data_size_bytes = 0;
    *out_memory_inst_ptr = NULL;

    wasm_memory_inst_t mem_inst = NULL;
    if (memory_name != NULL && strlen(memory_name) > 0) {
        mem_inst = wasm_runtime_lookup_memory(ctx->module_inst, memory_name);
    } else {
        mem_inst = wasm_runtime_get_default_memory(ctx->module_inst);
    }

    if (!mem_inst) {
        set_error_msg_v(ctx, "Failed to find memory instance (name: %s).", memory_name ? memory_name : "default");
        return HB_BEAMR_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED; // Or a more specific error
    }

    *out_memory_inst_ptr = mem_inst;
    *out_data_ptr = (uint8_t*)wasm_memory_get_base_address(mem_inst);
    uint64_t page_count = wasm_memory_get_cur_page_count(mem_inst);
    // uint64_t bytes_per_page = wasm_memory_get_bytes_per_page(mem_inst); // This is usually WASM_PAGE_SIZE (65536)
    *out_data_size_bytes = page_count * 65536; // Using fixed 64KiB page size, common in Wasm

    if (*out_data_ptr == NULL && *out_data_size_bytes > 0) {
        set_error_msg_v(ctx, "Memory '%s' data pointer is NULL but size is non-zero.", memory_name ? memory_name : "default");
        return HB_BEAMR_LIB_ERROR_INVALID_STATE;
    }
    
    // If data ptr is NULL and size is 0, it might be a memory that's not yet mapped or genuinely empty.
    // The caller can decide if this is an error based on expectations.

    set_error_msg(ctx, "Memory info retrieved successfully.");
    return HB_BEAMR_LIB_SUCCESS;
}

// Implementation for hb_beamr_lib_direct_read_memory
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_direct_read_memory(
    hb_beamr_lib_context_t* ctx,
    size_t offset,
    uint8_t* buffer,
    size_t length
) {
    if (!ctx) { 
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS; // No context to set error on, but arg is invalid
    }
    if (!ctx->module_inst) {
        set_error_msg(ctx, "Instance not created/valid, cannot read memory.");
        return HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED;
    }
    if (!buffer && length > 0) { // Allow NULL buffer if length is 0
        set_error_msg(ctx, "Output buffer is NULL for direct_read_memory.");
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    }
    if (length == 0) {
        set_error_msg(ctx, "No error (read 0 bytes).");
        return HB_BEAMR_LIB_SUCCESS; 
    }

    uint8_t* mem_base_ptr = NULL;
    size_t mem_total_size = 0;
    wasm_memory_inst_t mem_inst_handle = NULL; // Not strictly needed for read itself, but good practice to get it

    hb_beamr_lib_rc_t rc_info = hb_beamr_lib_get_memory_info(ctx, NULL, &mem_base_ptr, &mem_total_size, &mem_inst_handle);
    if (rc_info != HB_BEAMR_LIB_SUCCESS) {
        // Error message already set by hb_beamr_lib_get_memory_info
        return rc_info;
    }

    if (!mem_base_ptr) {
        set_error_msg(ctx, "Failed to get memory base pointer for read, though get_memory_info succeeded.");
        return HB_BEAMR_LIB_ERROR_INVALID_STATE;
    }

    if (offset + length > mem_total_size) {
        set_error_msg_v(ctx, "Direct read memory out of bounds (offset %zu + length %zu > size %zu).", offset, length, mem_total_size);
        return HB_BEAMR_LIB_ERROR_MEMORY_ACCESS_OUT_OF_BOUNDS;
    }

    memcpy(buffer, mem_base_ptr + offset, length);
    set_error_msg(ctx, "Memory read successfully.");
    return HB_BEAMR_LIB_SUCCESS;
}

// Implementation for hb_beamr_lib_direct_write_memory
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_direct_write_memory(
    hb_beamr_lib_context_t* ctx,
    size_t offset,
    const uint8_t* buffer,
    size_t length
) {
    if (!ctx) { 
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    }
    if (!ctx->module_inst) {
        set_error_msg(ctx, "Instance not created/valid, cannot write memory.");
        return HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED;
    }
    if (!buffer && length > 0) { // Allow NULL buffer if length is 0
        set_error_msg(ctx, "Input buffer is NULL for direct_write_memory.");
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    }
    if (length == 0) {
        set_error_msg(ctx, "No error (wrote 0 bytes).");
        return HB_BEAMR_LIB_SUCCESS;
    }

    uint8_t* mem_base_ptr = NULL;
    size_t mem_total_size = 0;
    wasm_memory_inst_t mem_inst_handle = NULL;

    hb_beamr_lib_rc_t rc_info = hb_beamr_lib_get_memory_info(ctx, NULL, &mem_base_ptr, &mem_total_size, &mem_inst_handle);
    if (rc_info != HB_BEAMR_LIB_SUCCESS) {
        return rc_info;
    }

    if (!mem_base_ptr) {
        set_error_msg(ctx, "Failed to get memory base pointer for write, though get_memory_info succeeded.");
        return HB_BEAMR_LIB_ERROR_INVALID_STATE;
    }

    if (offset + length > mem_total_size) {
        set_error_msg_v(ctx, "Direct write memory out of bounds (offset %zu + length %zu > size %zu).", offset, length, mem_total_size);
        return HB_BEAMR_LIB_ERROR_MEMORY_ACCESS_OUT_OF_BOUNDS;
    }

    memcpy(mem_base_ptr + offset, buffer, length);
    set_error_msg(ctx, "Memory written successfully.");
    return HB_BEAMR_LIB_SUCCESS;
}

// Helper function to convert wasm_val_t (API type) to uint32_t array (WAMR runtime call type)
// Returns the number of uint32_t cells used by parameters.
uint32_t hb_beamr_lib_convert_wasm_vals_to_argv(uint32_t num_wasm_args, const wasm_val_t wasm_args[], 
                                          uint32_t max_argv_cells, uint32_t argv[], 
                                          const wasm_valkind_t param_kinds[]) {
    uint32_t current_argv_idx = 0;
    for (uint32_t i = 0; i < num_wasm_args; ++i) {
        if (current_argv_idx >= max_argv_cells) return (uint32_t)-1; // Not enough space in argv

        // Ensure wasm_args[i].kind matches param_kinds[i] from func signature if available
        // For simplicity, this helper currently relies on caller to ensure kinds match.
        // A more robust version would check wasm_args[i].kind against param_kinds[i].

        switch (param_kinds[i]) { // Use kind from function signature for packing logic
            case WASM_I32:
            case WASM_F32:
                if (current_argv_idx + 1 > max_argv_cells) return (uint32_t)-1;
                argv[current_argv_idx++] = wasm_args[i].of.i32; // f32 also passed as i32 bits
                break;
            case WASM_I64:
            case WASM_F64:
                if (current_argv_idx + 2 > max_argv_cells) return (uint32_t)-1;
                // WAMR expects LSB first for 64-bit values in argv (uint32_t array)
                memcpy(&argv[current_argv_idx], &wasm_args[i].of.i64, sizeof(int64_t)); // f64 also passed as i64 bits
                current_argv_idx += 2;
                break;
            // TODO: Handle WASM_EXTERNREF, WASM_FUNCREF, WASM_V128 if supported/needed by WAMR indirect calls
            default:
                // Unsupported type for argv packing
                return (uint32_t)-1; 
        }
    }
    return current_argv_idx; // Number of uint32_t cells used for parameters
}

// Helper function to convert uint32_t array (WAMR runtime call type) back to wasm_val_t (API type)
// Returns the number of uint32_t cells used by results.
uint32_t hb_beamr_lib_convert_argv_to_wasm_vals(uint32_t num_wasm_results, const wasm_valkind_t result_kinds[], 
                                      const uint32_t argv[], wasm_val_t wasm_results[]) {
    uint32_t current_argv_idx = 0;
    for (uint32_t i = 0; i < num_wasm_results; ++i) {
        wasm_results[i].kind = result_kinds[i];
        switch (result_kinds[i]) {
            case WASM_I32:
            case WASM_F32:
                wasm_results[i].of.i32 = argv[current_argv_idx];
                current_argv_idx++;
                break;
            case WASM_I64:
            case WASM_F64:
                memcpy(&wasm_results[i].of.i64, &argv[current_argv_idx], sizeof(int64_t));
                current_argv_idx += 2;
                break;
            // TODO: Handle WASM_EXTERNREF, WASM_FUNCREF, WASM_V128
            default:
                return -1; // Unsupported result type
        }
    }
    return current_argv_idx;
}

HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_get_indirect_func_inst(hb_beamr_lib_context_t* ctx, const char* table_name, uint32_t func_index, wasm_function_inst_t* out_func_inst) {
    if (!ctx || !ctx->module_inst) {
        if (ctx) set_error_msg(ctx, "Context, instance not initialized for indirect function lookup.");
        return HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED;
    }
    
    fprintf(stderr, "[DEBUG hb_beamr_lib_get_indirect_func_inst] Attempting to get table: '%s' from module_inst: %p\n", table_name, (void*)ctx->module_inst);

    const char *effective_table_name = table_name;
    if (effective_table_name == NULL || effective_table_name[0] == '\0') {
        effective_table_name = "__indirect_function_table";
    }

    wasm_table_inst_t table_inst;
    if (!wasm_runtime_get_export_table_inst(ctx->module_inst, effective_table_name, &table_inst)) {
        set_error_msg_v(ctx, "Failed to get export table: %s", effective_table_name);
        return HB_BEAMR_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED; // Or a table specific error
    }

    // Check if func_index is within table bounds
    if (func_index >= table_inst.cur_size) {
        set_error_msg_v(ctx, "Function index %u out of bounds for table %s (size %u).", 
                        func_index, effective_table_name, table_inst.cur_size);
        return HB_BEAMR_LIB_ERROR_WAMR_CALL_FAILED; // Or specific OOB error
    }

    // Get the function instance from the table to retrieve its signature
    wasm_function_inst_t target_func_inst = wasm_table_get_func_inst(ctx->module_inst, &table_inst, func_index);
    if (!target_func_inst) {
        set_error_msg_v(ctx, "Failed to get function instance from table %s at index %u.", effective_table_name, func_index);
        return HB_BEAMR_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED;
    }

    *out_func_inst = target_func_inst;
    return HB_BEAMR_LIB_SUCCESS;
}

// Implementation for hb_beamr_lib_call_indirect
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_call_indirect(
    hb_beamr_lib_context_t* ctx,
    const char* table_name,
    uint32_t func_index,   
    uint32_t num_args,     
    wasm_val_t args[],     
    uint32_t num_results,  
    wasm_val_t results[]   
) {
    if (!ctx || !ctx->module_inst || !ctx->exec_env) {
        if (ctx) set_error_msg(ctx, "Context, instance, or exec_env not initialized for call_indirect.");
        return HB_BEAMR_LIB_ERROR_INSTANCE_NOT_CREATED;
    }
    // Temporarily allow empty string for table_name for testing WAMR behavior
    if (!table_name) { // Still check for NULL
        if (ctx) set_error_msg(ctx, "Table name is NULL for call_indirect.");
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    }

    // --- Enhanced Debugging: List all exports from the module --- 
    if (ctx->wasm_module) {
        int32_t export_count = wasm_runtime_get_export_count(ctx->wasm_module);
        fprintf(stderr, "[DEBUG hb_beamr_lib_call_indirect] Module export count: %d\n", export_count);
        // for (int32_t i = 0; i < export_count; ++i) {
        //     wasm_export_t export_info;
        //     wasm_runtime_get_export_type(ctx->wasm_module, i, &export_info);
        //     const char* kind_str = "unknown";
        //     switch (export_info.kind) {
        //         case WASM_IMPORT_EXPORT_KIND_FUNC: kind_str = "func"; break;
        //         case WASM_IMPORT_EXPORT_KIND_TABLE: kind_str = "table"; break;
        //         case WASM_IMPORT_EXPORT_KIND_MEMORY: kind_str = "memory"; break;
        //         case WASM_IMPORT_EXPORT_KIND_GLOBAL: kind_str = "global"; break;
        //     }
        //     fprintf(stderr, "[DEBUG hb_beamr_lib_call_indirect]   Module Export [%d]: Name='%s', Kind=%s (%d)\n", 
        //             i, export_info.name, kind_str, export_info.kind);
        // }
    } else {
        fprintf(stderr, "[DEBUG hb_beamr_lib_call_indirect] ctx->wasm_module is NULL, cannot list exports.\n");
    }
    // --- End Enhanced Debugging ---

    wasm_function_inst_t target_func_inst;
    hb_beamr_lib_rc_t rc = hb_beamr_lib_get_indirect_func_inst(ctx, table_name, func_index, &target_func_inst);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "[DEBUG hb_beamr_lib_call_indirect] Failed to get function instance from table: %d\n", rc);
        // set_error_msg(ctx, "Failed to get function instance from table.");
        return rc;
    }

    uint32_t actual_param_count = wasm_func_get_param_count(target_func_inst, ctx->module_inst);
    wasm_valkind_t* actual_param_types = (wasm_valkind_t*)malloc(sizeof(wasm_valkind_t) * actual_param_count);
    if (!actual_param_types && actual_param_count > 0) {
        set_error_msg(ctx, "Failed to allocate for param types in call_indirect.");
        return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
    }
    wasm_func_get_param_types(target_func_inst, ctx->module_inst, actual_param_types);

    uint32_t actual_result_count = wasm_func_get_result_count(target_func_inst, ctx->module_inst);
    wasm_valkind_t* actual_result_types = (wasm_valkind_t*)malloc(sizeof(wasm_valkind_t) * actual_result_count);
    if (!actual_result_types && actual_result_count > 0) {
        if (actual_param_types) free(actual_param_types);
        set_error_msg(ctx, "Failed to allocate for result types in call_indirect.");
        return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
    }
    wasm_func_get_result_types(target_func_inst, ctx->module_inst, actual_result_types);

    if (num_args != actual_param_count) {
        if (actual_param_types) free(actual_param_types);
        if (actual_result_types) free(actual_result_types);
        set_error_msg_v(ctx, "Argument count mismatch for indirect call: expected %u, got %u.", actual_param_count, num_args);
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    }
    if (num_results != actual_result_count) {
        if (actual_param_types) free(actual_param_types);
        if (actual_result_types) free(actual_result_types);
        set_error_msg_v(ctx, "Result count mismatch for indirect call: expected %u, got %u.", actual_result_count, num_results);
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    }

    // Calculate cells needed for WAMR's argv (uint32_t array)
    uint32_t param_argv_cells = 0;
    for(uint32_t i = 0; i < actual_param_count; ++i) {
        param_argv_cells += (actual_param_types[i] == WASM_I64 || actual_param_types[i] == WASM_F64) ? 2 : 1;
    }
    uint32_t result_argv_cells = 0;
    for(uint32_t i = 0; i < actual_result_count; ++i) {
        result_argv_cells += (actual_result_types[i] == WASM_I64 || actual_result_types[i] == WASM_F64) ? 2 : 1;
    }

    uint32_t total_argv_cells = param_argv_cells > result_argv_cells ? param_argv_cells : result_argv_cells;
    if (total_argv_cells == 0 && (actual_param_count > 0 || actual_result_count > 0)) { // Handle case of only V128 etc. if they were supported
         total_argv_cells = actual_param_count > 0 ? actual_param_count : (actual_result_count > 0 ? actual_result_count : 1); // Min 1 cell if void func for safety
    }
    if (total_argv_cells == 0) total_argv_cells = 1; // Ensure argv is not zero-sized for malloc

    uint32_t* argv_runtime = (uint32_t*)malloc(sizeof(uint32_t) * total_argv_cells);
    if (!argv_runtime) {
        if (actual_param_types) free(actual_param_types);
        if (actual_result_types) free(actual_result_types);
        set_error_msg(ctx, "Failed to allocate argv for indirect call.");
        return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
    }
    memset(argv_runtime, 0, sizeof(uint32_t) * total_argv_cells);

    uint32_t cells_for_params = hb_beamr_lib_convert_wasm_vals_to_argv(num_args, args, total_argv_cells, argv_runtime, actual_param_types);
    if (cells_for_params == (uint32_t)-1) {
        if (actual_param_types) free(actual_param_types);
        if (actual_result_types) free(actual_result_types);
        free(argv_runtime);
        set_error_msg(ctx, "Failed to convert arguments for indirect call (buffer overflow or type mismatch).");
        return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    }

    // fprintf(stderr, "[DEBUG hb_beamr_lib_call_indirect] Calling wasm_runtime_call_wasm with cells_for_params: %d and argv_runtime: %p\n", cells_for_params, argv_runtime);
    // bool success = wasm_runtime_call_wasm(ctx->exec_env, target_func_inst, cells_for_params, argv_runtime);
    bool success = wasm_runtime_call_indirect(ctx->exec_env, func_index, cells_for_params, argv_runtime);

    hb_beamr_lib_rc_t final_rc = HB_BEAMR_LIB_SUCCESS;
    if (!success) {
        const char* exception = wasm_runtime_get_exception(ctx->module_inst);
        set_error_msg_v(ctx, "Error during indirect call to table '%s' index %u: %s", 
                        table_name, func_index, exception ? exception : "N/A");
        final_rc = HB_BEAMR_LIB_ERROR_WAMR_CALL_FAILED;
    } else {
        if (num_results > 0) {
            if (!hb_beamr_lib_convert_argv_to_wasm_vals(num_results, actual_result_types, argv_runtime, results)) {
                set_error_msg(ctx, "Failed to convert results from indirect call.");
                final_rc = HB_BEAMR_LIB_ERROR_WAMR_CALL_FAILED; // Or a different error code
            }
        }
    }

    if (actual_param_types) free(actual_param_types);
    if (actual_result_types) free(actual_result_types);
    free(argv_runtime);

    return final_rc;
}

wasm_module_inst_t hb_beamr_lib_get_module_instance(hb_beamr_lib_context_t* ctx) {
    if (!ctx) return NULL;
    return ctx->module_inst;
}

// ============================ Structured Generated Natives Helper ===========
/*
 * ... native API should extract arguments one by one from args array with macro
 *   native_raw_get_arg
 * and write the return value back to args[0] with macro
 *   native_raw_return_type and native_raw_set_return
 */

hb_beamr_lib_rc_t hb_beamr_lib_convert_raw_args_to_wasm_vals(uint64_t args[], wasm_valkind_t val_types[], uint32_t val_count, wasm_val_t** out_vals) {
    if (!args || !out_vals) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    if (val_count > 0 && !val_types) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    wasm_val_t* vals = (wasm_val_t*)malloc(sizeof(wasm_val_t) * val_count);
    if (!vals) return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
    for (uint32_t i = 0; i < val_count; i++) {
        switch (val_types[i]) {
            case WASM_I32:
                vals[i].of.i32 = (int32_t)args[i];
                break;
            case WASM_I64:
                vals[i].of.i64 = (int64_t)args[i];
                break;
            case WASM_F32:
                vals[i].of.f32 = (float)args[i];
                break;
            case WASM_F64:
                vals[i].of.f64 = (double)args[i];
                break;
            default:
                free(vals);
                return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
        }
        vals[i].kind = val_types[i];
    }
    *out_vals = vals;
    return HB_BEAMR_LIB_SUCCESS;
}

hb_beamr_lib_rc_t hb_beamr_lib_convert_wasm_vals_to_raw_args(wasm_val_t* vals, uint32_t val_count, uint64_t* out_args) {
    if (!vals || !out_args) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    for (uint32_t i = 0; i < val_count; i++) {
        out_args[i] = vals[i].of.i32;
    }
    return HB_BEAMR_LIB_SUCCESS;
}

static char valkind_to_sig_char(wasm_valkind_t k){
    switch(k){case WASM_I32: return 'i'; case WASM_I64: return 'I'; case WASM_F32: return 'f'; case WASM_F64: return 'F'; default: return '?';}}

static hb_beamr_lib_rc_t build_signature_from_ftype(wasm_func_type_t ftype, char **out_sig){
    uint32_t pc = wasm_func_type_get_param_count(ftype);
    uint32_t rc = wasm_func_type_get_result_count(ftype);
    size_t len = 2 + pc + rc; // '(' params ')' results
    char *sig = (char*)malloc(len+1);
    if(!sig) return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
    size_t idx=0; sig[idx++]='(';
    for(uint32_t i=0;i<pc;i++){ sig[idx++]=valkind_to_sig_char(wasm_func_type_get_param_valkind(ftype,i)); }
    sig[idx++]=')';
    for(uint32_t i=0;i<rc;i++){ sig[idx++]=valkind_to_sig_char(wasm_func_type_get_result_valkind(ftype,i)); }
    sig[idx]='\0';
    *out_sig=sig; return HB_BEAMR_LIB_SUCCESS;
}

HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_generate_natives(
        const hb_beamr_meta_module_t *meta, void *user_function,
        hb_beamr_native_symbols_structured_t *out_struct){
    if(!meta||!user_function||!out_struct) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    memset(out_struct,0,sizeof(*out_struct));
    int32_t import_cnt = meta->import_count;
    if(import_cnt<=0){ /* no imports */ return HB_BEAMR_LIB_SUCCESS; }
    // First pass: count unique module names and function imports per module
    typedef struct { char *name; uint32_t count; } mod_count_t;
    mod_count_t *mods=NULL; uint32_t mod_cap=0, mod_used=0;
    for(int32_t i=0;i<import_cnt;i++){
        hb_beamr_meta_import_t imp = meta->imports[i];
        if(imp.kind!=WASM_IMPORT_EXPORT_KIND_FUNC) continue;
        // find or add module name
        uint32_t idx; for(idx=0; idx<mod_used; idx++) if(strcmp(mods[idx].name, imp.module_name)==0) break;
        if(idx==mod_used){ // add
            if(mod_used==mod_cap){ mod_cap = mod_cap?mod_cap*2:4; mods = realloc(mods, mod_cap*sizeof(mod_count_t)); }
            mods[mod_used].name = strdup(imp.module_name?imp.module_name:"env"); mods[mod_used].count = 0; mod_used++; }
        mods[idx].count++; }
    // allocate groups
    hb_beamr_native_symbol_group_t *groups = (hb_beamr_native_symbol_group_t*)malloc(sizeof(*groups)*mod_used);
    if(!groups){ for(uint32_t i=0;i<mod_used;i++) free(mods[i].name); free(mods); return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED; }
    // allocate symbols arrays per group
    for(uint32_t i=0;i<mod_used;i++){ groups[i].module_name = mods[i].name; groups[i].num_symbols = mods[i].count; groups[i].symbols = malloc(sizeof(hb_beamr_native_symbol_t)*mods[i].count); }

    // second pass populate
    uint32_t *cursor = calloc(mod_used,sizeof(uint32_t));
    for(int32_t i=0;i<import_cnt;i++){
        hb_beamr_meta_import_t imp = meta->imports[i];
        if(imp.kind!=WASM_IMPORT_EXPORT_KIND_FUNC) continue;
        // locate group index
        uint32_t gidx; for(gidx=0;gidx<mod_used;gidx++) if(strcmp(groups[gidx].module_name, imp.module_name)==0) break;
        uint32_t pos = cursor[gidx]++;
        hb_beamr_native_symbol_t *sym = (hb_beamr_native_symbol_t*)&groups[gidx].symbols[pos];
        sym->module_name = NULL; // WAMR ignores this field in NativeSymbol
        sym->function_name = strdup(imp.field_name?imp.field_name:"");
        sym->user_function = user_function;
        sym->signature = strdup(imp.func.signature?imp.func.signature:"");
        hb_beamr_meta_import_t *att = malloc(sizeof(*att));
        memcpy(att, &imp, sizeof(hb_beamr_meta_import_t));
        sym->attachment = att;
    }
    free(cursor); free(mods);
    out_struct->groups=groups; out_struct->num_groups=mod_used; return HB_BEAMR_LIB_SUCCESS;
}

HB_BEAMR_LIB_API void hb_beamr_lib_free_natives(hb_beamr_native_symbols_structured_t *st){
    if(!st||!st->groups) return; for(uint32_t g=0;g<st->num_groups;g++){
        hb_beamr_native_symbol_group_t *grp=&st->groups[g];
        for(uint32_t i=0;i<grp->num_symbols;i++){
            hb_beamr_native_symbol_t *s=&grp->symbols[i];
            hb_beamr_meta_import_t *att = (hb_beamr_meta_import_t*)s->attachment;
            if(att){ free((void*)att->module_name); free((void*)att->field_name); free(att);} free((void*)s->function_name); free((void*)s->signature);
        }
        free(grp->symbols); free((void*)grp->module_name);
    }
    free(st->groups); st->groups=NULL; st->num_groups=0;
}

HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_register_global_natives(const hb_beamr_native_symbols_structured_t *st){
    if(!st) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    for(uint32_t g=0; g<st->num_groups; g++){
        hb_beamr_native_symbol_group_t grp = st->groups[g];
        // Convert to NativeSymbol array on the fly
        NativeSymbol *tmp = (NativeSymbol*)malloc(sizeof(NativeSymbol)*grp.num_symbols);
        if(!tmp) return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
        for(uint32_t i=0;i<grp.num_symbols;i++){
            tmp[i].symbol    = grp.symbols[i].function_name;
            tmp[i].func_ptr  = grp.symbols[i].user_function;
            tmp[i].signature = grp.symbols[i].signature;
            tmp[i].attachment= grp.symbols[i].attachment;
        }
        bool ok = wasm_runtime_register_natives_raw(grp.module_name, tmp, grp.num_symbols);
        /* Do NOT free tmp: WAMR may access it later */
        if(!ok) return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED;
    }
    return HB_BEAMR_LIB_SUCCESS;
}

// ============================ end helper ===================================== 

// Helper to gather meta
HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_meta_module(wasm_module_t module,
                                                           hb_beamr_meta_module_t *out_meta){
    if(!module||!out_meta) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    memset(out_meta,0,sizeof(*out_meta));
    int32_t import_cnt = wasm_runtime_get_import_count(module);
    if(import_cnt<0){ return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED; }
    hb_beamr_meta_import_t *imports = NULL;
    if(import_cnt>0){
        imports = (hb_beamr_meta_import_t*)calloc(import_cnt, sizeof(hb_beamr_meta_import_t));
        if(!imports){ return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED; }
        for(int32_t i=0;i<import_cnt;i++){
            wasm_import_t imp_tmp; wasm_runtime_get_import_type(module,i,&imp_tmp);
            hb_beamr_meta_import_t *dst = &imports[i];
            dst->module_name = strdup(imp_tmp.module_name?imp_tmp.module_name:"");
            dst->field_name  = strdup(imp_tmp.name?imp_tmp.name:"");
            dst->kind = imp_tmp.kind;
            if(imp_tmp.kind == WASM_IMPORT_EXPORT_KIND_FUNC){
                wasm_func_type_t ftype = imp_tmp.u.func_type;
                dst->func.param_count = wasm_func_type_get_param_count(ftype);
                if(dst->func.param_count){
                    dst->func.param_types = (wasm_valkind_t*)malloc(sizeof(wasm_valkind_t)*dst->func.param_count);
                    for(uint32_t p=0; p<dst->func.param_count; p++){ dst->func.param_types[p] = wasm_func_type_get_param_valkind(ftype,p); }
                }
                dst->func.result_count = wasm_func_type_get_result_count(ftype);
                if(dst->func.result_count){
                    dst->func.result_types = (wasm_valkind_t*)malloc(sizeof(wasm_valkind_t)*dst->func.result_count);
                    for(uint32_t r=0; r<dst->func.result_count; r++){ dst->func.result_types[r] = wasm_func_type_get_result_valkind(ftype,r); }
                }
                build_signature_from_ftype(ftype, &dst->func.signature);
            }
        }
    }

    int32_t export_cnt = wasm_runtime_get_export_count(module);
    if(export_cnt<0){ /* cleanup */ if(imports){ /* deep free later */ }
        return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED; }
    hb_beamr_meta_export_t *exports = NULL;
    if(export_cnt>0){
        exports = (hb_beamr_meta_export_t*)calloc(export_cnt, sizeof(hb_beamr_meta_export_t));
        if(!exports){ /* free imports */ for(int32_t i=0;i<import_cnt;i++){ free(imports[i].module_name); free(imports[i].field_name); free(imports[i].func.signature); free(imports[i].func.param_types); free(imports[i].func.result_types);} free(imports); return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED; }
        for(int32_t i=0;i<export_cnt;i++){
            wasm_export_t exp_tmp; wasm_runtime_get_export_type(module,i,&exp_tmp);
            hb_beamr_meta_export_t *dst=&exports[i];
            dst->name = strdup(exp_tmp.name?exp_tmp.name:"");
            dst->kind = exp_tmp.kind;
            if(exp_tmp.kind == WASM_IMPORT_EXPORT_KIND_FUNC){
                wasm_func_type_t ftype = exp_tmp.u.func_type;
                dst->func.param_count = wasm_func_type_get_param_count(ftype);
                if(dst->func.param_count){ dst->func.param_types = (wasm_valkind_t*)malloc(sizeof(wasm_valkind_t)*dst->func.param_count); for(uint32_t p=0;p<dst->func.param_count;p++){ dst->func.param_types[p]=wasm_func_type_get_param_valkind(ftype,p);} }
                dst->func.result_count = wasm_func_type_get_result_count(ftype);
                if(dst->func.result_count){ dst->func.result_types = (wasm_valkind_t*)malloc(sizeof(wasm_valkind_t)*dst->func.result_count); for(uint32_t r=0;r<dst->func.result_count;r++){ dst->func.result_types[r]=wasm_func_type_get_result_valkind(ftype,r);} }
                build_signature_from_ftype(ftype,&dst->func.signature);
            }
        }
    }
    out_meta->imports = imports;
    out_meta->import_count = import_cnt;
    out_meta->exports = exports;
    out_meta->export_count = export_cnt;
    return HB_BEAMR_LIB_SUCCESS;
}

HB_BEAMR_LIB_API void hb_beamr_lib_free_meta_module(hb_beamr_meta_module_t *meta){
    if(!meta) return;
    if(meta->imports){
        for(uint32_t i=0;i<meta->import_count;i++){
            hb_beamr_meta_import_t *imp=&meta->imports[i];
            free(imp->module_name); free(imp->field_name); free(imp->func.signature);
            free(imp->func.param_types); free(imp->func.result_types);
        }
        free(meta->imports);
    }
    if(meta->exports){
        for(uint32_t i=0;i<meta->export_count;i++){
            hb_beamr_meta_export_t *exp=&meta->exports[i];
            free(exp->name); free(exp->func.signature);
            free(exp->func.param_types); free(exp->func.result_types);
        }
        free(meta->exports);
    }
    memset(meta,0,sizeof(*meta));
}

HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_meta_export_func(hb_beamr_meta_module_t *meta, const char* func_name, hb_beamr_meta_func_t **out_func_meta) {
    if (!meta || !func_name || !out_func_meta) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    // printf("meta_export_func: %s\n", func_name);
    for (uint32_t i = 0; i < meta->export_count; i++) {
        hb_beamr_meta_export_t *exp = &meta->exports[i];
        if (exp->kind == WASM_IMPORT_EXPORT_KIND_FUNC) {
            // printf("- test: %s\n", exp->name);
            if (strcmp(exp->name, func_name) == 0) {
                *out_func_meta = &exp->func;
                return HB_BEAMR_LIB_SUCCESS;
            }
        }
    }
    return HB_BEAMR_LIB_ERROR_NOT_FOUND;
}

HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_meta_import_func(hb_beamr_meta_module_t *meta, const char* module_name, const char* field_name, hb_beamr_meta_func_t **out_func_meta) {
    if (!meta || !module_name || !field_name || !out_func_meta) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    for (uint32_t i = 0; i < meta->import_count; i++) {
        hb_beamr_meta_import_t *imp = &meta->imports[i];
        if (imp->kind == WASM_IMPORT_EXPORT_KIND_FUNC) {
            if (strcmp(imp->module_name, module_name) == 0 && strcmp(imp->field_name, field_name) == 0) {
                *out_func_meta = &imp->func;
                return HB_BEAMR_LIB_SUCCESS;
            }
        }
    }
    return HB_BEAMR_LIB_ERROR_NOT_FOUND;
}

HB_BEAMR_LIB_API hb_beamr_lib_rc_t hb_beamr_lib_meta_indirect_func(hb_beamr_lib_context_t* ctx, const char* table_name, int index, hb_beamr_meta_func_t **out_func_meta) {
    if (!ctx || !table_name || !out_func_meta) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    wasm_function_inst_t target_func_inst;
    hb_beamr_lib_rc_t rc = hb_beamr_lib_get_indirect_func_inst(ctx, table_name, index, &target_func_inst);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        set_error_msg(ctx, "Failed to get function instance from table.");
        return rc;
    }
    *out_func_meta = (hb_beamr_meta_func_t*)malloc(sizeof(hb_beamr_meta_func_t));
    (*out_func_meta)->param_count = wasm_func_get_param_count(target_func_inst, ctx->module_inst);
    (*out_func_meta)->result_count = wasm_func_get_result_count(target_func_inst, ctx->module_inst);
    (*out_func_meta)->param_types = (wasm_valkind_t*)malloc(sizeof(wasm_valkind_t)*(*out_func_meta)->param_count);
    (*out_func_meta)->result_types = (wasm_valkind_t*)malloc(sizeof(wasm_valkind_t)*(*out_func_meta)->result_count);
    wasm_func_get_param_types(target_func_inst, ctx->module_inst, (*out_func_meta)->param_types);
    wasm_func_get_result_types(target_func_inst, ctx->module_inst, (*out_func_meta)->result_types);
    (*out_func_meta)->signature = "()"; // We don't need this for indirect functions

    return HB_BEAMR_LIB_SUCCESS;
}

HB_BEAMR_LIB_API void hb_beamr_lib_meta_indirect_func_free(hb_beamr_meta_func_t *func_meta) {
    if (!func_meta) return;
    free(func_meta->param_types);
    free(func_meta->result_types);
    free(func_meta);
}
