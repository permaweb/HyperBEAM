#include "hb_beamr_lib.h"
#include "wasm_export.h"
#include <stdlib.h> // For malloc/free, though WAMR uses its own allocator setup
#include <string.h> // For memset, strcpy
#include <stdarg.h> // For va_list, va_start, va_end, vsnprintf
#include <stdio.h>  // For vsnprintf, snprintf
#include <stdbool.h> // For bool type

#define MAX_LAST_ERROR_MSG_SIZE 256

// Global state for the library
static bool g_wamr_runtime_initialized = false;
static RuntimeInitArgs g_wamr_init_args;
// We need to be careful if g_wamr_init_args.native_symbols itself points to memory that might be freed.
// For now, assuming the relevant parts (allocator settings) are by value or long-lived.
// The RuntimeInitArgs struct itself will be copied.

// Definition of the opaque context struct
struct hb_beamr_lib_context {
    wasm_module_t wasm_module;          // WAMR module handle
    wasm_module_inst_t module_inst;    // WAMR module instance handle
    wasm_exec_env_t exec_env;          // WAMR execution environment for the current instance
    char last_error_msg[MAX_LAST_ERROR_MSG_SIZE]; // Buffer for the last error message
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
    if (g_wamr_runtime_initialized) {
        // Or return success if already initialized with compatible args? For now, treat as error or ignore.
        return HB_BEAMR_LIB_ERROR_INVALID_STATE; // Already initialized
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
        return HB_BEAMR_LIB_ERROR_WAMR_RUNTIME_INIT_FAILED;
    }

    // Store the effective init args for potential re-use (e.g., in register_global_natives)
    g_wamr_init_args = effective_init_args;
    g_wamr_runtime_initialized = true;
    return HB_BEAMR_LIB_SUCCESS;
}

void hb_beamr_lib_destroy_runtime_global(void) {
    if (g_wamr_runtime_initialized) {
        wasm_runtime_destroy();
        g_wamr_runtime_initialized = false;
        memset(&g_wamr_init_args, 0, sizeof(RuntimeInitArgs)); // Clear stored args
    }
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
    ctx->wasm_module = wasm_runtime_load(aot_binary_buf, aot_binary_size, error_buf, sizeof(error_buf));
    if (!ctx->wasm_module) {
        set_error_msg_v(ctx, "Failed to load AOT module: %s", error_buf);
        return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED;
    }

    // Native symbol registration moved to hb_beamr_lib_instantiate

    set_error_msg(ctx, "Module loaded successfully (no symbols registered at this stage).");
    return HB_BEAMR_LIB_SUCCESS;
}

hb_beamr_lib_rc_t hb_beamr_lib_register_global_natives(
    const char* for_module_name, 
    const hb_beamr_native_symbol_t* import_symbols, 
    uint32_t num_import_symbols
) {
    if (!g_wamr_runtime_initialized) {
        // Cannot register natives if runtime wasn't even first initialized.
        return HB_BEAMR_LIB_ERROR_WAMR_RUNTIME_INIT_FAILED; // Or a more specific "not initialized" error
    }
    if (!for_module_name || !import_symbols || num_import_symbols == 0) {
        return HB_BEAMR_LIB_ERROR_INVALID_STATE; 
    }

    NativeSymbol* wamr_symbols = (NativeSymbol*)malloc(sizeof(NativeSymbol) * num_import_symbols);
    if (!wamr_symbols) {
        return HB_BEAMR_LIB_ERROR_ALLOCATION_FAILED;
    }

    for (uint32_t i = 0; i < num_import_symbols; ++i) {
        wamr_symbols[i].symbol = import_symbols[i].function_name;
        wamr_symbols[i].func_ptr = import_symbols[i].user_function;
        wamr_symbols[i].signature = import_symbols[i].signature;
        wamr_symbols[i].attachment = import_symbols[i].attachment;
    }

    if (!wasm_runtime_register_natives_raw(for_module_name, wamr_symbols, num_import_symbols)) {
        free(wamr_symbols); // Still free if registration itself fails
        return HB_BEAMR_LIB_ERROR_WAMR_LOAD_FAILED; 
    }

    // Do NOT free wamr_symbols here if WAMR takes ownership, as per documentation.
    // free(wamr_symbols);
    // TODO: This implies a memory leak until a proper unregistration or runtime shutdown frees these.
    return HB_BEAMR_LIB_SUCCESS;
}

hb_beamr_lib_rc_t hb_beamr_lib_instantiate(
    hb_beamr_lib_context_t* ctx, 
    uint32_t stack_size, 
    uint32_t heap_size
    // Removed import_symbols arguments
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

    // Native symbols are assumed to be globally registered by hb_beamr_lib_register_global_natives
    // BEFORE hb_beamr_lib_load_aot_module was called.

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

    // Set the custom data for this instance to be the context itself.
    // This allows native functions to retrieve the context and access per-instance state.
    wasm_runtime_set_custom_data(ctx->module_inst, (void*)ctx);

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