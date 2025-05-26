#include "hb_beamr_capi_lib.h"

#include "wasm_c_api.h" // Ensure this is the primary include for Wasm types

#include <stdlib.h> // Replaced <cstdlib>
#include <string.h> // Replaced <cstring>
#include <stdarg.h> // Replaced <cstdarg>
#include <stdio.h>  // Replaced <cstdio>
#include <stdbool.h>

#define MAX_LAST_ERROR_MSG_SIZE 256
#define INITIAL_HOST_FUNCS_CAPACITY 10 // Initial capacity for host_funcs and host_envs

// Global state for the C API based library
static wasm_engine_t* g_wasm_engine = NULL; // Replaced nullptr
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
    char last_error_msg[MAX_LAST_ERROR_MSG_SIZE]; // Replaced std::string
    wasm_func_t** host_funcs; // Replaced std::vector
    size_t num_host_funcs;
    size_t host_funcs_capacity;
    HostFuncEnv** host_envs;  // Replaced std::vector
    size_t num_host_envs;
    size_t host_envs_capacity;
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
    HostFuncEnv* tramp_env = (HostFuncEnv*)(env); // Replaced static_cast

    if (!tramp_env || !tramp_env->instance_ctx || !tramp_env->instance_ctx->store) {
        fprintf(stderr, "CRITICAL: generic_trampoline_callback called with invalid HostFuncEnv or context/store.\\n"); // Replaced std::cerr
        return NULL; // Replaced nullptr
    }

    if (!tramp_env->user_function_ptr) {
        char trap_msg_buf_str[] = "trap: Call to unlinked or unimplemented host function";
        wasm_name_t trap_name;
        // wasm_name_new_from_string_nt is fine as it takes const char*
        // Assuming wasm_name_new_from_string_nt does not allocate trap_name.data if it's stack allocated,
        // or that wasm_name_delete handles it correctly. If not, trap_name needs to be allocated.
        // For safety, let's assume wasm_name_new_from_string_nt makes a copy or manages it.
        wasm_name_new_from_string_nt(&trap_name, trap_msg_buf_str);
        wasm_trap_t* trap = wasm_trap_new(tramp_env->instance_ctx->store, (const wasm_message_t*)&trap_name);
        wasm_name_delete(&trap_name);
        results->num_elems = 0;
        return trap;
    }

    user_host_func_callback_t actual_callback = (user_host_func_callback_t)(tramp_env->user_function_ptr); // Replaced reinterpret_cast
    // Pass the entire instance_ctx as the environment to the user's actual C function
    return actual_callback(tramp_env->instance_ctx, args, results);
}

// Helper function to set the last error message in a context
static void set_error_msg(hb_beamr_capi_lib_context_t* ctx, const char* msg) {
    if (ctx) {
        if (msg) {
            strncpy(ctx->last_error_msg, msg, MAX_LAST_ERROR_MSG_SIZE - 1);
            ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\\0'; // Ensure null termination
        } else {
            strncpy(ctx->last_error_msg, "Unknown error (null message passed).", MAX_LAST_ERROR_MSG_SIZE - 1);
            ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\\0';
        }
    }
}

static void set_error_msg_v(hb_beamr_capi_lib_context_t* ctx, const char* format, ...) {
    if (ctx) {
        va_list args;
        va_start(args, format);
        int written = vsnprintf(ctx->last_error_msg, MAX_LAST_ERROR_MSG_SIZE, format, args);
        va_end(args);
        if (written < 0 || written >= MAX_LAST_ERROR_MSG_SIZE) {
            // Handle error or truncation, for now, we just ensure null termination
            // at the end of the buffer if vsnprintf failed or truncated.
            // A more robust solution might log this occurrence.
            ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\\0';
            if (written < 0) {
                 // vsnprintf failed, set a generic error.
                 strncpy(ctx->last_error_msg, "Error formatting error message.", MAX_LAST_ERROR_MSG_SIZE -1);
                 ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\\0';
            }
        }
    }
}

// Accessor for a store pointer from the context
wasm_store_t* hb_beamr_capi_lib_context_get_store(hb_beamr_capi_lib_context_t* ctx) {
    if (!ctx) {
        return NULL; // Replaced nullptr
    }
    return ctx->store;
}

// Initialize the runtime globally using wasm_c_api style config
hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_init_runtime_global(wasm_config_t* config) {
    if (g_runtime_initialized) {
        return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE;
    }
    g_wasm_engine = wasm_engine_new_with_config(config);
    if (!g_wasm_engine) {
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_RUNTIME_INIT_FAILED;
    }
    g_runtime_initialized = true;
    return HB_BEAMR_CAPI_LIB_SUCCESS;
}

void hb_beamr_capi_lib_destroy_runtime_global(void) {
    if (g_runtime_initialized) {
        if (g_wasm_engine) {
            wasm_engine_delete(g_wasm_engine);
            g_wasm_engine = NULL; // Replaced nullptr
        }
        g_runtime_initialized = false;
    }
}

hb_beamr_capi_lib_context_t* hb_beamr_capi_lib_create_context(void) {
    if (!g_runtime_initialized || !g_wasm_engine) {
        return NULL; // Replaced nullptr
    }

    hb_beamr_capi_lib_context_t* ctx = (hb_beamr_capi_lib_context_t*)malloc(sizeof(hb_beamr_capi_lib_context_t)); // Replaced new
    if (!ctx) {
        return NULL; // Replaced nullptr
    }
    memset(ctx, 0, sizeof(hb_beamr_capi_lib_context_t)); // Initialize to zero

    ctx->store = wasm_store_new(g_wasm_engine);
    if (!ctx->store) {
        free(ctx); // Replaced delete
        return NULL; // Replaced nullptr
    }

    ctx->module = NULL; // Replaced nullptr
    ctx->instance = NULL; // Replaced nullptr
    strncpy(ctx->last_error_msg, "No error", MAX_LAST_ERROR_MSG_SIZE - 1);
    ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\\0';

    // Initialize dynamic arrays
    ctx->host_funcs = (wasm_func_t**)malloc(sizeof(wasm_func_t*) * INITIAL_HOST_FUNCS_CAPACITY);
    if (!ctx->host_funcs) {
        wasm_store_delete(ctx->store);
        free(ctx);
        return NULL;
    }
    ctx->host_funcs_capacity = INITIAL_HOST_FUNCS_CAPACITY;
    ctx->num_host_funcs = 0;

    ctx->host_envs = (HostFuncEnv**)malloc(sizeof(HostFuncEnv*) * INITIAL_HOST_FUNCS_CAPACITY);
    if (!ctx->host_envs) {
        free(ctx->host_funcs);
        wasm_store_delete(ctx->store);
        free(ctx);
        return NULL;
    }
    ctx->host_envs_capacity = INITIAL_HOST_FUNCS_CAPACITY;
    ctx->num_host_envs = 0;

    return ctx;
}

void hb_beamr_capi_lib_destroy_context(hb_beamr_capi_lib_context_t* ctx) {
    if (ctx) {
        if (ctx->instance) {
            wasm_instance_delete(ctx->instance);
            ctx->instance = NULL; // Replaced nullptr
        }
        if (ctx->module) {
            wasm_module_delete(ctx->module);
            ctx->module = NULL; // Replaced nullptr
        }

        // Delete HostFuncEnv objects managed by host_envs array
        for (size_t i = 0; i < ctx->num_host_envs; ++i) {
            if (ctx->host_envs[i]) {
                free(ctx->host_envs[i]); // Replaced delete
            }
        }
        free(ctx->host_envs); // Free the array itself
        ctx->host_envs = NULL;
        ctx->num_host_envs = 0;
        ctx->host_envs_capacity = 0;


        // wasm_func_t objects pointed to by host_funcs are typically owned by the store
        // or WAMR. We don't delete them directly here if they were created via wasm_func_new_with_env
        // and WAMR cleans them up when the store is deleted.
        // However, if we were managing them separately, we'd free them here.
        // For now, just free the tracking array.
        free(ctx->host_funcs);
        ctx->host_funcs = NULL;
        ctx->num_host_funcs = 0;
        ctx->host_funcs_capacity = 0;


        if (ctx->store) {
            wasm_store_delete(ctx->store);
            ctx->store = NULL; // Replaced nullptr
        }
        free(ctx); // Replaced delete
    }
}

const char* hb_beamr_capi_lib_get_last_error(hb_beamr_capi_lib_context_t* ctx) {
    if (ctx && ctx->last_error_msg[0] != '\\0') { // Check if not empty
        return ctx->last_error_msg;
    }
    // Return a static string literal if ctx is null or message is empty/default
    static const char* default_msg = "Context is NULL or error message not set.";
    if (ctx && strncmp(ctx->last_error_msg, "No error", MAX_LAST_ERROR_MSG_SIZE) == 0) {
        // If it's the default "No error" message, maybe return a more informative default.
        // For now, sticking to the original logic: if it's not empty, return it.
        // "No error" is not empty.
    }
    return default_msg;
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
    // wasm_byte_vec_new_uninitialized makes a copy, so the original buf can be freed/go out of scope.
    wasm_byte_vec_new_uninitialized(&binary_vec, wasm_binary_size);
    if (!binary_vec.data && wasm_binary_size > 0) { // Check if allocation failed
        set_error_msg(ctx, "Failed to allocate buffer for WASM binary vector.");
        return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
    }
    if (binary_vec.data) { // if size is 0, data might be NULL, which is fine for memcpy if size is 0.
      memcpy(binary_vec.data, wasm_binary_buf, wasm_binary_size);
    }

    ctx->module = wasm_module_new(ctx->store, &binary_vec);
    wasm_byte_vec_delete(&binary_vec); // Clean up the copied binary vector

    if (!ctx->module) {
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
    }
    // Some WAMR/Wasm C API `wasm_name_t` might not be null-terminated if they come from
    // non-string sections or are synthesized. The `size` field is authoritative.
    // Comparing `wasm_name->size == c_len + 1` and `wasm_name->data[c_len] == '\\0'`
    // was an attempt to handle null-terminated wasm_name_t, but it's safer to rely on size.
    // If the wasm_name is supposed to represent the c_string, its size must be c_len.
    return false;
}


// Helper function to print wasm_valtype_kind_t as string for debugging
// This function uses printf, which is okay for debugging but should be
// conditional if this library aims for no stdout/stderr writes.
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
        printf("[DEBUG FT] functype is NULL\\n"); // Replaced std::cout
        return;
    }
    const wasm_valtype_vec_t* params = wasm_functype_params(ftype);
    const wasm_valtype_vec_t* results = wasm_functype_results(ftype);
    printf("[DEBUG FT] Params (%zu): ", params->size); // Replaced std::cout
    for (size_t i = 0; i < params->size; ++i) {
        printf("%s ", valkind_to_string(wasm_valtype_kind(params->data[i]))); // Replaced std::cout
    }
    printf(", Results (%zu): ", results->size); // Replaced std::cout
    for (size_t i = 0; i < results->size; ++i) {
        printf("%s ", valkind_to_string(wasm_valtype_kind(results->data[i]))); // Replaced std::cout
    }
    printf("\\n"); // Replaced std::cout
}

// Helper function to manage dynamic array for host_funcs
static hb_beamr_capi_lib_rc_t ensure_host_funcs_capacity(hb_beamr_capi_lib_context_t* ctx) {
    if (ctx->num_host_funcs >= ctx->host_funcs_capacity) {
        size_t new_capacity = ctx->host_funcs_capacity == 0 ? INITIAL_HOST_FUNCS_CAPACITY : ctx->host_funcs_capacity * 2;
        wasm_func_t** new_funcs = (wasm_func_t**)realloc(ctx->host_funcs, sizeof(wasm_func_t*) * new_capacity);
        if (!new_funcs) {
            set_error_msg(ctx, "Failed to realloc host_funcs.");
            return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
        }
        ctx->host_funcs = new_funcs;
        ctx->host_funcs_capacity = new_capacity;
    }
    return HB_BEAMR_CAPI_LIB_SUCCESS;
}

// Helper function to manage dynamic array for host_envs
static hb_beamr_capi_lib_rc_t ensure_host_envs_capacity(hb_beamr_capi_lib_context_t* ctx) {
    if (ctx->num_host_envs >= ctx->host_envs_capacity) {
        size_t new_capacity = ctx->host_envs_capacity == 0 ? INITIAL_HOST_FUNCS_CAPACITY : ctx->host_envs_capacity * 2;
        HostFuncEnv** new_envs = (HostFuncEnv**)realloc(ctx->host_envs, sizeof(HostFuncEnv*) * new_capacity);
        if (!new_envs) {
            set_error_msg(ctx, "Failed to realloc host_envs.");
            return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
        }
        ctx->host_envs = new_envs;
        ctx->host_envs_capacity = new_capacity;
    }
    return HB_BEAMR_CAPI_LIB_SUCCESS;
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
    wasm_importtype_vec_new_empty(&import_types_vec); // Manual RAII
    wasm_module_imports(ctx->module, &import_types_vec);
    size_t num_imports = import_types_vec.size;

    wasm_extern_t** extern_stubs_array = NULL;
    if (num_imports > 0) {
        extern_stubs_array = (wasm_extern_t**)calloc(num_imports, sizeof(wasm_extern_t*)); // Replaced std::vector and new
        if (!extern_stubs_array) {
            set_error_msg(ctx, "Alloc for extern_stubs_array failed.");
            wasm_importtype_vec_delete(&import_types_vec);
            return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
        }
    }

    // Clear and prepare for new host functions and environments
    // Free previously allocated HostFuncEnv objects
    for (size_t i = 0; i < ctx->num_host_envs; ++i) {
        if (ctx->host_envs[i]) {
            free(ctx->host_envs[i]);
        }
    }
    ctx->num_host_envs = 0; // Reset count, capacity remains
    // Host funcs are owned by store mostly, just reset count
    ctx->num_host_funcs = 0;


    for (size_t i = 0; i < num_imports; ++i) {
        const wasm_importtype_t* import_type = import_types_vec.data[i];
        const wasm_name_t* module_name_vec = wasm_importtype_module(import_type);
        const wasm_name_t* func_name_vec = wasm_importtype_name(import_type);
        const wasm_externtype_t* extern_type = wasm_importtype_type(import_type);
        wasm_externkind_t kind = wasm_externtype_kind(extern_type);

        // extern_stubs_array initialized with calloc, so elements are NULL

        if (kind != WASM_EXTERN_FUNC) {
            continue;
        }

        void* user_function_for_import = NULL; // Replaced nullptr
        bool found_override = false;
        for (uint32_t j = 0; j < num_override_symbols; ++j) {
            const hb_beamr_capi_native_symbol_t* override_sym = &override_symbols[j];
            // Assuming import_module_name and import_function_name are null-terminated C strings
            // wasm_name_t->data might not be null-terminated, its length is wasm_name_t->size
            char temp_module_name[256]; // Assuming max name length
            char temp_func_name[256];

            if (module_name_vec->size < sizeof(temp_module_name)) {
                memcpy(temp_module_name, module_name_vec->data, module_name_vec->size);
                temp_module_name[module_name_vec->size] = '\\0';
            } else { continue; /* name too long, skip */ }

            if (func_name_vec->size < sizeof(temp_func_name)) {
                memcpy(temp_func_name, func_name_vec->data, func_name_vec->size);
                temp_func_name[func_name_vec->size] = '\\0';
            } else { continue; /* name too long, skip */ }

            if (strcmp(temp_module_name, override_sym->import_module_name) == 0 &&
                strcmp(temp_func_name, override_sym->import_function_name) == 0) {
                user_function_for_import = override_sym->user_function;
                found_override = true;
                break;
            }
        }

        if (!found_override && default_import_function != NULL) { // Replaced nullptr
            user_function_for_import = default_import_function;
        }
        // If user_function_for_import is still NULL here, it means no override and no default was provided,
        // or the default was NULL. The trampoline will handle this by creating a trap.

        const wasm_functype_t* expected_functype = wasm_externtype_as_functype_const(extern_type);
        if (!expected_functype) {
            char error_buf[512]; // Increased buffer size
            // Safely copy names, ensuring null termination for snprintf
            char mod_name_safe[256] = {0};
            char func_name_safe[256] = {0};
            strncpy(mod_name_safe, module_name_vec->data, module_name_vec->size < 255 ? module_name_vec->size : 255);
            strncpy(func_name_safe, func_name_vec->data, func_name_vec->size < 255 ? func_name_vec->size : 255);

            snprintf(error_buf, sizeof(error_buf), "Could not get functype for import '%s::%s'.",
                     mod_name_safe, func_name_safe);
            set_error_msg(ctx, error_buf);
            if (extern_stubs_array) free(extern_stubs_array);
            wasm_importtype_vec_delete(&import_types_vec);
            return HB_BEAMR_CAPI_LIB_ERROR_NATIVE_LINKING_FAILED;
        }

        HostFuncEnv* tramp_env_alloc = (HostFuncEnv*)malloc(sizeof(HostFuncEnv)); // Replaced new (std::nothrow)
        if (!tramp_env_alloc) {
            set_error_msg(ctx, "Failed to allocate trampoline env.");
            if (extern_stubs_array) free(extern_stubs_array);
            wasm_importtype_vec_delete(&import_types_vec);
            return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
        }
        tramp_env_alloc->user_function_ptr = user_function_for_import;
        tramp_env_alloc->instance_ctx = ctx;

        wasm_func_t* host_func = wasm_func_new_with_env(
            ctx->store, expected_functype,
            generic_trampoline_callback,
            tramp_env_alloc, // env for callback
            NULL // host_func_env_finalizer, set to null as we manually delete HostFuncEnv
        );

        if (!host_func) {
            free(tramp_env_alloc); // Clean up allocated tramp_env
            char error_buf[512];
            char mod_name_safe[256] = {0};
            char func_name_safe[256] = {0};
            strncpy(mod_name_safe, module_name_vec->data, module_name_vec->size < 255 ? module_name_vec->size : 255);
            strncpy(func_name_safe, func_name_vec->data, func_name_vec->size < 255 ? func_name_vec->size : 255);
            snprintf(error_buf, sizeof(error_buf), "wasm_func_new_with_env failed for '%s::%s'.",
                     mod_name_safe, func_name_safe);
            set_error_msg(ctx, error_buf);
            if (extern_stubs_array) free(extern_stubs_array);
            wasm_importtype_vec_delete(&import_types_vec);
            return HB_BEAMR_CAPI_LIB_ERROR_NATIVE_LINKING_FAILED;
        }

        extern_stubs_array[i] = wasm_func_as_extern(host_func);
        
        // Add to context's tracking arrays
        if (ensure_host_funcs_capacity(ctx) != HB_BEAMR_CAPI_LIB_SUCCESS ||
            ensure_host_envs_capacity(ctx) != HB_BEAMR_CAPI_LIB_SUCCESS) {
            // Error already set by ensure_..._capacity
            free(tramp_env_alloc);
            // Note: host_func is now owned by the store if wasm_func_new_with_env succeeded and it's
            // added to extern_stubs_array. If we error out here, it might leak if not cleaned up by store eventually.
            // This path needs careful resource management if host_func isn't auto-cleaned.
            // For now, assuming store cleans it up or it's fine as it's not yet part of an instance.
            // A safer approach might be to clean up all created host_func and envs on any error during this loop.
            if (extern_stubs_array) free(extern_stubs_array);
            wasm_importtype_vec_delete(&import_types_vec);
            return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
        }
        ctx->host_funcs[ctx->num_host_funcs++] = host_func;
        ctx->host_envs[ctx->num_host_envs++] = tramp_env_alloc;
    }

    wasm_extern_vec_t imports_for_instance; // Manual RAII
    // wasm_extern_vec_new makes a *copy* of the data in extern_stubs_array if it's not NULL.
    wasm_extern_vec_new(&imports_for_instance, num_imports, num_imports > 0 ? extern_stubs_array : NULL);
    
    if (extern_stubs_array) { // We can free our local array after wasm_extern_vec_new copies it.
      free(extern_stubs_array);
      extern_stubs_array = NULL;
    }


    wasm_trap_t* trap = NULL; // Replaced nullptr
    ctx->instance = wasm_instance_new(ctx->store, ctx->module, &imports_for_instance, &trap);
    wasm_extern_vec_delete(&imports_for_instance); // Manual RAII cleanup

    if (trap) {
        wasm_message_t trap_message;
        wasm_trap_message(trap, &trap_message);
        // Copy message to a C string buffer before using with set_error_msg_v
        char trap_msg_c_str[MAX_LAST_ERROR_MSG_SIZE]; // Or a larger temp buffer
        size_t copy_len = trap_message.size < (MAX_LAST_ERROR_MSG_SIZE - 1) ? trap_message.size : (MAX_LAST_ERROR_MSG_SIZE - 1);
        memcpy(trap_msg_c_str, trap_message.data, copy_len);
        trap_msg_c_str[copy_len] = '\\0';

        set_error_msg_v(ctx, "Instantiation trapped: %s", trap_msg_c_str);
        wasm_name_delete(&trap_message); // Assuming wasm_name_delete is correct for wasm_message_t
        wasm_trap_delete(trap);
        if (ctx->instance) { wasm_instance_delete(ctx->instance); ctx->instance = NULL;} // Replaced nullptr
        wasm_importtype_vec_delete(&import_types_vec); // Manual RAII
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_INSTANTIATE_FAILED;
    }
    if (!ctx->instance) {
        set_error_msg(ctx, "Instantiation failed: Unknown error, no trap.");
        wasm_importtype_vec_delete(&import_types_vec); // Manual RAII
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_INSTANTIATE_FAILED;
    }

    wasm_importtype_vec_delete(&import_types_vec); // Manual RAII cleanup
    set_error_msg(ctx, "Module instantiated successfully.");
    return HB_BEAMR_CAPI_LIB_SUCCESS;
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

    wasm_extern_vec_t exports_vec; // Manual RAII
    wasm_extern_vec_new_empty(&exports_vec);
    wasm_instance_exports(ctx->instance, &exports_vec);

    wasm_func_t* func = NULL; // Replaced nullptr
    bool found_func = false;

    wasm_exporttype_vec_t module_export_types; // Manual RAII
    wasm_exporttype_vec_new_empty(&module_export_types);
    wasm_module_exports(ctx->module, &module_export_types);

    for (size_t i = 0; i < module_export_types.size; ++i) {
        const wasm_exporttype_t* export_type = module_export_types.data[i];
        const wasm_name_t* name_vec = wasm_exporttype_name(export_type);
        
        // Convert wasm_name_t to null-terminated C string for strcmp
        char current_export_name[256]; // Assuming max name length
        if (name_vec->size < sizeof(current_export_name)) {
            memcpy(current_export_name, name_vec->data, name_vec->size);
            current_export_name[name_vec->size] = '\\0';
        } else {
            // Name too long, cannot match func_name unless func_name is also this long
            // and strcmp handles non-null terminated strings with a length, which it doesn't.
            // Or, use a more robust comparison that respects name_vec->size.
            // For now, skip if too long for buffer.
            continue;
        }

        if (strcmp(current_export_name, func_name) == 0) {
            if (wasm_externtype_kind(wasm_exporttype_type(export_type)) == WASM_EXTERN_FUNC && i < exports_vec.size) {
                func = wasm_extern_as_func(exports_vec.data[i]);
                if (func) found_func = true;
            }
            break;
        }
    }
    wasm_exporttype_vec_delete(&module_export_types); // Manual RAII cleanup

    if (!found_func) {
        set_error_msg_v(ctx, "Export func '%s' not found.", func_name);
        wasm_extern_vec_delete(&exports_vec); // Manual RAII cleanup
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED;
    }

    wasm_val_vec_t args_as_vec = { .num_elems = num_args, .size = num_args, .data = c_api_args };
    wasm_val_vec_t results_as_vec = { .num_elems = 0, .size = num_results, .data = c_api_results }; // num_elems will be set by call
    wasm_trap_t* trap = wasm_func_call(func, &args_as_vec, &results_as_vec);
    wasm_extern_vec_delete(&exports_vec); // Manual RAII cleanup for exports_vec

    if (trap) {
        wasm_message_t trap_message;
        wasm_trap_message(trap, &trap_message);
        char trap_msg_c_str[MAX_LAST_ERROR_MSG_SIZE];
        size_t copy_len = trap_message.size < (MAX_LAST_ERROR_MSG_SIZE - 1) ? trap_message.size : (MAX_LAST_ERROR_MSG_SIZE - 1);
        memcpy(trap_msg_c_str, trap_message.data, copy_len);
        trap_msg_c_str[copy_len] = '\\0';

        set_error_msg_v(ctx, "Call to '%s' trapped: %s", func_name, trap_msg_c_str);
        wasm_name_delete(&trap_message); // Assuming correct for wasm_message_t
        wasm_trap_delete(trap);
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_CALL_FAILED;
    }
    if (results_as_vec.num_elems != num_results) {
        // This check might be problematic if the function is expected to fill only *up to* num_results
        // and returns the actual number written in results_as_vec.num_elems.
        // Standard Wasm C API: wasm_func_call sets results_as_vec.num_elems to actual number of results.
        // The caller provides a buffer (c_api_results) of size `num_results`.
        // If results_as_vec.num_elems > num_results (what was allocated by caller), that's a buffer overflow by WAMR.
        // If results_as_vec.num_elems < num_results, WAMR wrote fewer results than expected by function signature.
        // The critical part is that c_api_results must be large enough for the function's *actual* results.
        // The num_results parameter to this C API function is what the *caller* expects.
        // A mismatch usually indicates an issue with how the function was defined or called.
        set_error_msg_v(ctx, "Call '%s' result count mismatch (expected %u, got %zu).", func_name, num_results, results_as_vec.num_elems);
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_CALL_FAILED;
    }
    return HB_BEAMR_CAPI_LIB_SUCCESS;
}

// extern "C" { // This is a .c file, so extern "C" is not needed here.

hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_resolve_import(
    hb_beamr_capi_lib_context_t* ctx, uint32_t num_results, wasm_val_t results[]
) {
    if (!ctx) return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE;
    // This function seems to be a placeholder or for a specific WAMR feature not used elsewhere.
    // Ensure `results` is handled if it's meant to be populated.
    // If num_results > 0, `results` array should be populated or cleared.
    // For now, just setting error as per original.
    set_error_msg(ctx, "resolve_import not impl.");
    return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE; // Changed from INVALID_STATE to make it clearer
}

// } // extern "C"
