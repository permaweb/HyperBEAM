#include "hb_beamr_capi_lib.h"
#include "wasm_c_api.h"    // Ensure this is the primary include for Wasm types
#include <stdlib.h> 
#include <string.h> 
#include <stdarg.h> 
#include <stdio.h>  
#include <stdbool.h> 

#define MAX_LAST_ERROR_MSG_SIZE 256

// Global state for the C API based library
static wasm_engine_t* g_wasm_engine = NULL; // This will be a real engine
static bool g_runtime_initialized = false;

// Environment for the trampoline host function
typedef struct HostFuncEnvStruct {
    void* user_function_ptr;         
    hb_beamr_capi_lib_context_t* instance_ctx; // Pointer to the main library context for the trampoline
} HostFuncEnv;

// Forward declaration of the context struct's content needed by HostFuncEnv
struct hb_beamr_capi_lib_context {
    wasm_store_t* store;
    wasm_module_t* module;      
    wasm_instance_t* instance;  
    char last_error_msg[MAX_LAST_ERROR_MSG_SIZE];
    wasm_func_t** created_host_funcs; // For cleanup of wasm_func_t objects themselves
    size_t num_created_host_funcs;
    size_t capacity_created_host_funcs;
    HostFuncEnv** created_host_envs; // To manually manage HostFuncEnv lifetime
    size_t num_created_host_envs;
    size_t capacity_created_host_envs;
};

// Typedef for the user's actual host function signature (matches wasm_func_callback_with_env_t)
typedef wasm_trap_t* (*user_host_func_callback_t)(
    void* user_env, // This will be store_for_user_callback
    const wasm_val_vec_t* args, 
    wasm_val_vec_t* results);

// Generic trampoline callback function
static wasm_trap_t* generic_trampoline_callback(
    void* env, 
    const wasm_val_vec_t* args, 
    wasm_val_vec_t* results
) {
    HostFuncEnv* tramp_env = (HostFuncEnv*)env;

    if (!tramp_env || !tramp_env->instance_ctx || !tramp_env->instance_ctx->store) { // store check is still good for trap creation below
        fprintf(stderr, "CRITICAL: generic_trampoline_callback called with invalid HostFuncEnv or context/store.\n");
        return NULL; 
    }

    if (!tramp_env->user_function_ptr) {
        char trap_msg_buf[128];
        snprintf(trap_msg_buf, sizeof(trap_msg_buf), "trap: Call to unlinked or unimplemented host function");
        wasm_name_t trap_name;
        wasm_name_new_from_string_nt(&trap_name, trap_msg_buf);
        wasm_trap_t* trap = wasm_trap_new(tramp_env->instance_ctx->store, (const wasm_message_t*)&trap_name);
        wasm_name_delete(&trap_name);
        results->num_elems = 0; 
        return trap;
    }

    user_host_func_callback_t actual_callback = (user_host_func_callback_t)tramp_env->user_function_ptr;
    // Pass the entire instance_ctx as the environment to the user's actual C function
    return actual_callback(tramp_env->instance_ctx, args, results);
}

// Helper function to set the last error message in a context
static void set_error_msg(hb_beamr_capi_lib_context_t* ctx, const char* msg) {
    if (ctx) {
        strncpy(ctx->last_error_msg, msg, MAX_LAST_ERROR_MSG_SIZE - 1);
        ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\0'; // Ensure null termination
    }
}

static void set_error_msg_v(hb_beamr_capi_lib_context_t* ctx, const char* format, ...) {
    if (ctx) {
        va_list args;
        va_start(args, format);
        vsnprintf(ctx->last_error_msg, MAX_LAST_ERROR_MSG_SIZE, format, args);
        va_end(args);
        ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\0'; // Ensure null termination
    }
}

// Accessor for a store pointer from the context
wasm_store_t* hb_beamr_capi_lib_context_get_store(hb_beamr_capi_lib_context_t* ctx) {
    if (!ctx) {
        return NULL;
    }
    return ctx->store;
}

// Initialize the runtime globally using wasm_c_api style config
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_init_runtime_global(wasm_config_t* config) {
    if (g_runtime_initialized) {
        return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE; 
    }

    // wasm_engine_new_with_config takes ownership of the config if successful.
    // If config is NULL, wasm_engine_new() can be used (or WAMR's wasm_engine_new_with_config(NULL) behavior).
    // Let's stick to wasm_engine_new_with_config as it's more general.
    // If config is NULL, the caller of this function should have ideally passed wasm_config_new() result or NULL.
    // WAMR's wasm_c_api.c for wasm_engine_new_with_config will create a default config if NULL is passed.
    g_wasm_engine = wasm_engine_new_with_config(config);
    if (!g_wasm_engine) {
        // If config was non-NULL and wasm_engine_new_with_config failed, 
        // 'config' is NOT consumed by wasm_engine_new_with_config by strict C API interpretation.
        // The caller is responsible for this 'config'.
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_RUNTIME_INIT_FAILED;
    }
    // On success, 'config' (if it was non-NULL) is now owned by the engine.

    g_runtime_initialized = true;
    return HB_BEAMR_CAPI_LIB_SUCCESS;
}

void hb_beamr_capi_lib_destroy_runtime_global(void) {
    if (g_runtime_initialized) {
        if (g_wasm_engine) {
            wasm_engine_delete(g_wasm_engine); // This should handle the config it consumed.
            g_wasm_engine = NULL;
        }
        g_runtime_initialized = false;
    }
}

hb_beamr_capi_lib_context_t* hb_beamr_capi_lib_create_context(void) {
    if (!g_runtime_initialized || !g_wasm_engine) {
        return NULL;
    }

    hb_beamr_capi_lib_context_t* ctx = (hb_beamr_capi_lib_context_t*)malloc(sizeof(hb_beamr_capi_lib_context_t));
    if (!ctx) {
        return NULL; 
    }
    memset(ctx, 0, sizeof(hb_beamr_capi_lib_context_t));

    ctx->store = wasm_store_new(g_wasm_engine);
    if (!ctx->store) {
        free(ctx);
        return NULL; 
    }

    ctx->module = NULL;
    ctx->instance = NULL;
    // Initialize new fields for HostFuncEnv tracking
    ctx->created_host_envs = NULL;
    ctx->num_created_host_envs = 0;
    ctx->capacity_created_host_envs = 0;
    strcpy(ctx->last_error_msg, "No error");
    return ctx;
}

void hb_beamr_capi_lib_destroy_context(hb_beamr_capi_lib_context_t* ctx) {
    if (ctx) {
        if (ctx->instance) {
            wasm_instance_delete(ctx->instance);
            ctx->instance = NULL;
        }
        if (ctx->module) {
            wasm_module_delete(ctx->module);
            ctx->module = NULL;
        }
        if (ctx->created_host_envs) {
            for (size_t i = 0; i < ctx->num_created_host_envs; ++i) {
                if (ctx->created_host_envs[i]) {
                    free(ctx->created_host_envs[i]);
                }
            }
            free(ctx->created_host_envs);
            ctx->created_host_envs = NULL;
            ctx->num_created_host_envs = 0;
            ctx->capacity_created_host_envs = 0;
        }
        if (ctx->created_host_funcs) {
            free(ctx->created_host_funcs);
            ctx->created_host_funcs = NULL;
            ctx->num_created_host_funcs = 0; 
            ctx->capacity_created_host_funcs = 0;
        }
        if (ctx->store) {
            wasm_store_delete(ctx->store); 
            ctx->store = NULL; 
        }
        free(ctx);
    }
}

const char* hb_beamr_capi_lib_get_last_error(hb_beamr_capi_lib_context_t* ctx) {
    if (ctx && ctx->last_error_msg[0] != '\0') {
        return ctx->last_error_msg;
    }
    return "Context is NULL or error message not set.";
}

// Module loading (from .wasm binary using standard C API)
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_load_wasm_module(
    hb_beamr_capi_lib_context_t* ctx,
    const uint8_t* wasm_binary_buf, 
    uint32_t wasm_binary_size
) {
    if (!ctx || !ctx->store) {
        if(ctx) set_error_msg(ctx, "Context or store not initialized for load_wasm_module.");
        return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE;
    }
    if (ctx->module) {
        set_error_msg(ctx, "Module already loaded in this context.");
        return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE;
    }
    if (!wasm_binary_buf || wasm_binary_size == 0) {
        set_error_msg(ctx, "WASM binary buffer is NULL or size is zero.");
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_LOAD_FAILED;
    }
    wasm_byte_vec_t binary_vec;
    wasm_byte_vec_new_uninitialized(&binary_vec, wasm_binary_size);
    if (!binary_vec.data && wasm_binary_size > 0) {
        set_error_msg(ctx, "Failed to allocate buffer for WASM binary vector.");
        return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
    }
    if (binary_vec.data) {
        memcpy(binary_vec.data, wasm_binary_buf, wasm_binary_size);
    }
    ctx->module = wasm_module_new(ctx->store, &binary_vec);
    wasm_byte_vec_delete(&binary_vec);
    if (!ctx->module) {
        // wasm_store_trap is not available in the provided wasm_c_api.h from WAMR
        // So, we can't get detailed trap information here.
        set_error_msg(ctx, "Failed to load WASM module (wasm_module_new returned NULL).");
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_LOAD_FAILED;
    }
    set_error_msg(ctx, "WASM Module loaded successfully.");
    return HB_BEAMR_CAPI_LIB_SUCCESS;
}

// Helper for comparing wasm_name_t with a C string
static bool names_match(const wasm_name_t* wasm_name, const char* c_string) {
    if (!wasm_name || !c_string) return false;
    size_t c_len = strlen(c_string);
    // Check if wasm_name matches c_string exactly up to c_len,
    // and if wasm_name->size indicates it's either the same length string
    // or a null-terminated string of that c_len.
    if (wasm_name->size == c_len) {
        return memcmp(wasm_name->data, c_string, c_len) == 0;
    } else if (wasm_name->size == c_len + 1) {
        return memcmp(wasm_name->data, c_string, c_len) == 0 && wasm_name->data[c_len] == '\0';
    }
    return false;
}

// Helper function to print wasm_valtype_kind_t as string for debugging
const char* valkind_to_string(wasm_valkind_t kind) {
    switch (kind) {
        case WASM_I32: return "I32";
        case WASM_I64: return "I64";
        case WASM_F32: return "F32";
        case WASM_F64: return "F64";
        case WASM_V128: return "V128";
        case WASM_FUNCREF: return "FUNCREF";
        case WASM_EXTERNREF: return "EXTERNREF";
        default: return "UNKNOWN_VALKIND";
    }
}

// Helper function to print wasm_functype_t details for debugging
void print_functype_details(const wasm_functype_t* ftype) {
    if (!ftype) {
        printf("[DEBUG FT] functype is NULL\n");
        return;
    }
    const wasm_valtype_vec_t* params = wasm_functype_params(ftype);
    const wasm_valtype_vec_t* results = wasm_functype_results(ftype);
    printf("[DEBUG FT] Params (%zu): ", params->size);
    for (size_t i = 0; i < params->size; ++i) {
        printf("%s ", valkind_to_string(wasm_valtype_kind(params->data[i])));
    }
    printf(", Results (%zu): ", results->size);
    for (size_t i = 0; i < results->size; ++i) {
        printf("%s ", valkind_to_string(wasm_valtype_kind(results->data[i])));
    }
    printf("\n");
}

hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_instantiate(
    hb_beamr_capi_lib_context_t* ctx, 
    void* default_import_function,
    const hb_beamr_capi_native_symbol_t* override_symbols,
    uint32_t num_override_symbols
) {
    if (!ctx || !ctx->store || !ctx->module) { 
        if(ctx) set_error_msg(ctx, "Ctx/store/module invalid for instantiate."); 
        return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE; 
    }
    if (ctx->instance) { set_error_msg(ctx, "Already instantiated."); return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE; }

    wasm_importtype_vec_t import_types_vec;
    wasm_module_imports(ctx->module, &import_types_vec);
    size_t num_imports = import_types_vec.size;

    wasm_extern_t* extern_stubs_array_on_stack[num_imports > 0 ? num_imports : 1]; 
    wasm_extern_t** extern_stubs_array_ptr = extern_stubs_array_on_stack;

    if (ctx->created_host_funcs) { free(ctx->created_host_funcs); ctx->created_host_funcs = NULL; }
    ctx->capacity_created_host_funcs = num_imports > 0 ? num_imports : 1; 
    ctx->created_host_funcs = (wasm_func_t**)calloc(ctx->capacity_created_host_funcs, sizeof(wasm_func_t*));
    ctx->num_created_host_funcs = 0;
    if (num_imports > 0 && !ctx->created_host_funcs) {
        wasm_importtype_vec_delete(&import_types_vec);
        set_error_msg(ctx, "Alloc for host func tracking failed.");
        return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
    }

    if (ctx->created_host_envs) { free(ctx->created_host_envs); ctx->created_host_envs = NULL; }
    ctx->capacity_created_host_envs = num_imports > 0 ? num_imports : 1;
    ctx->created_host_envs = (HostFuncEnv**)calloc(ctx->capacity_created_host_envs, sizeof(HostFuncEnv*));
    ctx->num_created_host_envs = 0;
    if (num_imports > 0 && !ctx->created_host_envs) {
        wasm_importtype_vec_delete(&import_types_vec);
        if(ctx->created_host_funcs) { free(ctx->created_host_funcs); ctx->created_host_funcs = NULL; }
        set_error_msg(ctx, "Alloc for host env tracking failed.");
        return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
    }

    hb_beamr_capi_lib_rc_t final_rc = HB_BEAMR_CAPI_LIB_SUCCESS;

    for (size_t i = 0; i < num_imports; ++i) {
        const wasm_importtype_t* import_type = import_types_vec.data[i];
        const wasm_name_t* module_name_vec = wasm_importtype_module(import_type);
        const wasm_name_t* func_name_vec = wasm_importtype_name(import_type);
        const wasm_externtype_t* extern_type = wasm_importtype_type(import_type);
        wasm_externkind_t kind = wasm_externtype_kind(extern_type);

        extern_stubs_array_ptr[i] = NULL; 

        if (kind != WASM_EXTERN_FUNC) {
            continue; 
        }
        
        void* user_function_for_import = NULL;
        bool found_override = false;
        for (uint32_t j = 0; j < num_override_symbols; ++j) {
            const hb_beamr_capi_native_symbol_t* override_sym = &override_symbols[j];
            if (names_match(module_name_vec, override_sym->import_module_name) &&
                names_match(func_name_vec, override_sym->import_function_name)) {
                user_function_for_import = override_sym->user_function;
                found_override = true;
                break; 
            }
        }
        if (!found_override && default_import_function != NULL) {
            user_function_for_import = default_import_function;
        }
        if (user_function_for_import == NULL && !found_override) {
        }

        const wasm_functype_t* expected_functype = wasm_externtype_as_functype_const(extern_type);
        if (!expected_functype) { 
            char import_mod_name_buf[64]; char import_func_name_buf[128];
            snprintf(import_mod_name_buf, sizeof(import_mod_name_buf), "%.*s", (int)module_name_vec->size, module_name_vec->data);
            snprintf(import_func_name_buf, sizeof(import_func_name_buf), "%.*s", (int)func_name_vec->size, func_name_vec->data);
            set_error_msg_v(ctx, "Could not get functype for import '%s::%s'.", import_mod_name_buf, import_func_name_buf);
            final_rc = HB_BEAMR_CAPI_LIB_ERROR_NATIVE_LINKING_FAILED; goto cleanup_and_fail;
        }
        
        HostFuncEnv* tramp_env_alloc = (HostFuncEnv*)malloc(sizeof(HostFuncEnv));
        if (!tramp_env_alloc) {
            set_error_msg(ctx, "Failed to allocate trampoline env.");
            final_rc = HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED; goto cleanup_and_fail;
        }
        tramp_env_alloc->user_function_ptr = user_function_for_import;
        tramp_env_alloc->instance_ctx = ctx; // Set the instance_ctx field
        
        wasm_func_t* host_func = wasm_func_new_with_env(
            ctx->store, expected_functype, 
            generic_trampoline_callback, 
            tramp_env_alloc, 
            NULL );
        
        if (!host_func) { 
             if (tramp_env_alloc) free(tramp_env_alloc);
             char import_mod_name_buf[64]; char import_func_name_buf[128];
             snprintf(import_mod_name_buf, sizeof(import_mod_name_buf), "%.*s", (int)module_name_vec->size, module_name_vec->data);
             snprintf(import_func_name_buf, sizeof(import_func_name_buf), "%.*s", (int)func_name_vec->size, func_name_vec->data);
             set_error_msg_v(ctx, "wasm_func_new_with_env failed for '%s::%s'.", import_mod_name_buf, import_func_name_buf);
             final_rc = HB_BEAMR_CAPI_LIB_ERROR_NATIVE_LINKING_FAILED; goto cleanup_and_fail;
        }

        extern_stubs_array_ptr[i] = wasm_func_as_extern(host_func);
        if (ctx->num_created_host_funcs < ctx->capacity_created_host_funcs && 
            ctx->num_created_host_envs < ctx->capacity_created_host_envs) {
            ctx->created_host_funcs[ctx->num_created_host_funcs++] = host_func;
            ctx->created_host_envs[ctx->num_created_host_envs++] = tramp_env_alloc;
        } else { 
            if (tramp_env_alloc) free(tramp_env_alloc);
            if (host_func) wasm_func_delete(host_func); 
            set_error_msg(ctx, "Internal error: exceeded capacity for host_funcs/envs arrays.");
            final_rc = HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED; goto cleanup_and_fail; 
        }
    }

    wasm_extern_vec_t imports_for_instance;
    wasm_extern_vec_new(&imports_for_instance, num_imports, extern_stubs_array_ptr);

    wasm_trap_t* trap = NULL;
    ctx->instance = wasm_instance_new(ctx->store, ctx->module, &imports_for_instance, &trap);
    
    wasm_extern_vec_delete(&imports_for_instance); 

    if (trap) {
        wasm_message_t trap_message;
        wasm_trap_message(trap, &trap_message);
        char trap_msg_str[256];
        snprintf(trap_msg_str, sizeof(trap_msg_str), "%.*s", (int)trap_message.size, trap_message.data);
        set_error_msg_v(ctx, "Instantiation trapped: %s", trap_msg_str);
        wasm_name_delete(&trap_message); wasm_trap_delete(trap);
        if (ctx->instance) { wasm_instance_delete(ctx->instance); ctx->instance = NULL;}
        final_rc = HB_BEAMR_CAPI_LIB_ERROR_WAMR_INSTANTIATE_FAILED; goto cleanup_and_fail;
    }
    if (!ctx->instance) {
        set_error_msg(ctx, "Instantiation failed: Unknown error, no trap.");
        final_rc = HB_BEAMR_CAPI_LIB_ERROR_WAMR_INSTANTIATE_FAILED; goto cleanup_and_fail;
    }

    wasm_importtype_vec_delete(&import_types_vec);
    set_error_msg(ctx, "Module instantiated successfully.");
    return HB_BEAMR_CAPI_LIB_SUCCESS;

cleanup_and_fail:
    if (ctx->created_host_envs) {
         for(size_t k=0; k < ctx->num_created_host_envs; ++k) {
            if(ctx->created_host_envs[k]) free(ctx->created_host_envs[k]);
         }
         free(ctx->created_host_envs); 
         ctx->created_host_envs = NULL; 
         ctx->num_created_host_envs = 0;
         ctx->capacity_created_host_envs = 0;
    }
    if (ctx->created_host_funcs) {
         for(size_t k=0; k < ctx->num_created_host_funcs; ++k) {
            if(ctx->created_host_funcs[k]) wasm_func_delete(ctx->created_host_funcs[k]);
         }
         free(ctx->created_host_funcs); 
         ctx->created_host_funcs = NULL; 
         ctx->num_created_host_funcs = 0;
         ctx->capacity_created_host_funcs = 0;
    }
    wasm_importtype_vec_delete(&import_types_vec); 
    return final_rc;
}

hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_call_export(
    hb_beamr_capi_lib_context_t* ctx,
    const char* func_name,
    uint32_t num_args,
    wasm_val_t c_api_args[], 
    uint32_t num_results,
    wasm_val_t c_api_results[] 
) {
    if (!ctx || !ctx->instance) { if (ctx) set_error_msg(ctx, "Instance invalid for call."); return HB_BEAMR_CAPI_LIB_ERROR_INSTANCE_NOT_CREATED;}
    if (!func_name) { set_error_msg(ctx, "Func name NULL for call."); return HB_BEAMR_CAPI_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED;}

    wasm_extern_vec_t exports_vec;
    wasm_instance_exports(ctx->instance, &exports_vec);
    wasm_func_t* func = NULL; bool found_func = false;
    wasm_exporttype_vec_t module_export_types;
    wasm_module_exports(ctx->module, &module_export_types);

    for (size_t i = 0; i < module_export_types.size; ++i) {
        const wasm_exporttype_t* export_type = module_export_types.data[i];
        const wasm_name_t* name_vec = wasm_exporttype_name(export_type);
        if (names_match(name_vec, func_name)) {
            if (wasm_externtype_kind(wasm_exporttype_type(export_type)) == WASM_EXTERN_FUNC && i < exports_vec.size) {
                func = wasm_extern_as_func(exports_vec.data[i]);
                if (func) found_func = true;
            }
            break; 
        }
    }
    wasm_exporttype_vec_delete(&module_export_types); 
    if (!found_func) {
        set_error_msg_v(ctx, "Export func '%s' not found.", func_name);
        wasm_extern_vec_delete(&exports_vec); 
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED;
    }
    
    wasm_val_vec_t args_as_vec = { .num_elems = num_args, .size = num_args, .data = c_api_args };
    wasm_val_vec_t results_as_vec = { .num_elems = 0, .size = num_results, .data = c_api_results };
    wasm_trap_t* trap = wasm_func_call(func, &args_as_vec, &results_as_vec);
    wasm_extern_vec_delete(&exports_vec); 

    if (trap) {
        wasm_message_t trap_message;
        wasm_trap_message(trap, &trap_message);
        set_error_msg_v(ctx, "Call to '%s' trapped: %.*s", func_name, (int)trap_message.size, trap_message.data);
        wasm_name_delete(&trap_message); wasm_trap_delete(trap);
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_CALL_FAILED;
    }
    if (results_as_vec.num_elems != num_results) {
        set_error_msg_v(ctx, "Call '%s' result count mismatch.", func_name);
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_CALL_FAILED; 
    }
    return HB_BEAMR_CAPI_LIB_SUCCESS;
}

hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_resolve_import(
    hb_beamr_capi_lib_context_t* ctx, uint32_t num_results, wasm_val_t results[]
) {
    if (!ctx) return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE;
    set_error_msg(ctx, "resolve_import not impl.");
    return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE; 
}


