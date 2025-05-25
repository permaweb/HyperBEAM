#include "hb_beamr_capi_lib.h"

#include "wasm_c_api.h"    // Ensure this is the primary include for Wasm types

#include <cstdlib> 
#include <cstring> 
#include <cstdarg> 
#include <cstdio>  
#include <stdbool.h> 
#include <vector>
#include <string>
#include <memory> // For std::unique_ptr if needed for more complex RAII
#include <iostream> // For std::cout, std::cerr
#include <string_view> // For std::string_view

#define MAX_LAST_ERROR_MSG_SIZE 256

// Global state for the C API based library
static wasm_engine_t* g_wasm_engine = nullptr; // This will be a real engine
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
    std::string last_error_msg; // Changed from C-style char array
    std::vector<wasm_func_t*> host_funcs; // Changed from C-style array
    std::vector<HostFuncEnv*> host_envs;  // Changed from C-style array
    // wasm_func_t** created_host_funcs; // For cleanup of wasm_func_t objects themselves
    // size_t num_created_host_funcs;
    // size_t capacity_created_host_funcs;
    // HostFuncEnv** created_host_envs; // To manually manage HostFuncEnv lifetime
    // size_t num_created_host_envs;
    // size_t capacity_created_host_envs;
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
    HostFuncEnv* tramp_env = static_cast<HostFuncEnv*>(env);

    if (!tramp_env || !tramp_env->instance_ctx || !tramp_env->instance_ctx->store) { 
        std::cerr << "CRITICAL: generic_trampoline_callback called with invalid HostFuncEnv or context/store.\n";
        return nullptr; 
    }

    if (!tramp_env->user_function_ptr) {
        // Use std::format or stringstream if more complex formatting is needed in the future
        std::string trap_msg_buf_str = "trap: Call to unlinked or unimplemented host function";
        wasm_name_t trap_name;
        // wasm_name_new_from_string_nt is fine as it takes const char*
        wasm_name_new_from_string_nt(&trap_name, trap_msg_buf_str.c_str()); 
        wasm_trap_t* trap = wasm_trap_new(tramp_env->instance_ctx->store, (const wasm_message_t*)&trap_name);
        wasm_name_delete(&trap_name);
        results->num_elems = 0; 
        return trap;
    }

    user_host_func_callback_t actual_callback = reinterpret_cast<user_host_func_callback_t>(tramp_env->user_function_ptr);
    // Pass the entire instance_ctx as the environment to the user's actual C function
    return actual_callback(tramp_env->instance_ctx, args, results);
}

// Helper function to set the last error message in a context
static void set_error_msg(hb_beamr_capi_lib_context_t* ctx, const char* msg) {
    if (ctx) {
        try {
            ctx->last_error_msg = msg;
        } catch (const std::bad_alloc&) {
            // Not much we can do if string allocation fails here, 
            // perhaps log to stderr if this is critical path
            std::cerr << "CRITICAL: Failed to allocate memory for error message.\n";
        }
    }
}

static void set_error_msg_v(hb_beamr_capi_lib_context_t* ctx, const char* format, ...) {
    if (ctx) {
        va_list args;
        va_start(args, format);
        
        // Determine required size
        va_list args_copy;
        va_copy(args_copy, args);
        int size = std::vsnprintf(nullptr, 0, format, args_copy);
        va_end(args_copy);

        if (size < 0) { // Error in vsnprintf
            va_end(args);
            try {
                ctx->last_error_msg = "Error formatting error message.";
            } catch (const std::bad_alloc&) { /* Ignore */ }
            return;
        }

        try {
            std::string temp_msg(size + 1, '\0'); // +1 for null terminator
            std::vsnprintf(&temp_msg[0], temp_msg.length(), format, args);
            temp_msg.resize(size); // Remove trailing null characters if any beyond 'size'
            ctx->last_error_msg = temp_msg;
        } catch (const std::bad_alloc&) {
            // Allocation for temp_msg or ctx->last_error_msg failed
            std::cerr << "CRITICAL: Failed to allocate memory for formatted error message.\n";
        }
        va_end(args);
    }
}

// Accessor for a store pointer from the context
wasm_store_t* hb_beamr_capi_lib_context_get_store(hb_beamr_capi_lib_context_t* ctx) {
    if (!ctx) {
        return nullptr;
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
            g_wasm_engine = nullptr;
        }
        g_runtime_initialized = false;
    }
}

hb_beamr_capi_lib_context_t* hb_beamr_capi_lib_create_context(void) {
    if (!g_runtime_initialized || !g_wasm_engine) {
        return nullptr;
    }

    hb_beamr_capi_lib_context_t* ctx = new (std::nothrow) hb_beamr_capi_lib_context_t();
    if (!ctx) {
        return nullptr; 
    }

    ctx->store = wasm_store_new(g_wasm_engine);
    if (!ctx->store) {
        delete ctx;
        return nullptr; 
    }

    ctx->module = nullptr;
    ctx->instance = nullptr;
    try {
        ctx->last_error_msg = "No error";
    } catch (const std::bad_alloc&) {
        // If this fails, last_error_msg will be empty. 
        // The get_last_error function should handle this.
        delete ctx; // Can't really proceed if basic error string fails
        return nullptr;
    }
    return ctx;
}

void hb_beamr_capi_lib_destroy_context(hb_beamr_capi_lib_context_t* ctx) {
    if (ctx) {
        if (ctx->instance) {
            wasm_instance_delete(ctx->instance);
            ctx->instance = nullptr;
        }
        if (ctx->module) {
            wasm_module_delete(ctx->module);
            ctx->module = nullptr;
        }
        
        // Delete HostFuncEnv objects managed by host_envs vector
        for (HostFuncEnv* env : ctx->host_envs) {
            delete env;
        }
        ctx->host_envs.clear(); // Clear the vector of pointers

        // wasm_func_t objects pointed to by host_funcs are typically owned by the store
        // or WAMR. We don't delete them directly here, just clear our tracking vector.
        // WAMR will clean them up when the store is deleted.
        ctx->host_funcs.clear();

        if (ctx->store) {
            wasm_store_delete(ctx->store); 
            ctx->store = nullptr; 
        }
        delete ctx;
    }
}

const char* hb_beamr_capi_lib_get_last_error(hb_beamr_capi_lib_context_t* ctx) {
    if (ctx && !ctx->last_error_msg.empty()) {
        return ctx->last_error_msg.c_str();
    }
    // Return a static string literal if ctx is null or message is empty
    static const char* default_msg = "Context is NULL or error message not set.";
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
        std::cout << "[DEBUG FT] functype is NULL\n";
        return;
    }
    const wasm_valtype_vec_t* params = wasm_functype_params(ftype);
    const wasm_valtype_vec_t* results = wasm_functype_results(ftype);
    std::cout << "[DEBUG FT] Params (" << params->size << "): ";
    for (size_t i = 0; i < params->size; ++i) {
        std::cout << valkind_to_string(wasm_valtype_kind(params->data[i])) << " ";
    }
    std::cout << ", Results (" << results->size << "): ";
    for (size_t i = 0; i < results->size; ++i) {
        std::cout << valkind_to_string(wasm_valtype_kind(results->data[i])) << " ";
    }
    std::cout << "\n";
}

// RAII Wrappers for WAMR types
struct WasmImporttypeVecRAII {
    wasm_importtype_vec_t vec;
    WasmImporttypeVecRAII() { wasm_importtype_vec_new_empty(&vec); }
    ~WasmImporttypeVecRAII() { wasm_importtype_vec_delete(&vec); }
    // Disable copy and assign
    WasmImporttypeVecRAII(const WasmImporttypeVecRAII&) = delete;
    WasmImporttypeVecRAII& operator=(const WasmImporttypeVecRAII&) = delete;
};

struct WasmExternVecRAII {
    wasm_extern_vec_t vec;
    WasmExternVecRAII() { wasm_extern_vec_new_empty(&vec); } 
    // Constructor to take ownership of an existing initialized vec if needed (e.g. from wasm_instance_exports)
    // For wasm_extern_vec_new, it initializes and we'd populate. For wasm_instance_exports, it populates for us.
    // This basic version assumes we initialize then populate, or it's populated by WAMR for us to then delete.
    ~WasmExternVecRAII() { wasm_extern_vec_delete(&vec); }
    // Disable copy and assign
    WasmExternVecRAII(const WasmExternVecRAII&) = delete;
    WasmExternVecRAII& operator=(const WasmExternVecRAII&) = delete;
};

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

    WasmImporttypeVecRAII import_types_vec_raii; // RAII wrapper
    wasm_module_imports(ctx->module, &import_types_vec_raii.vec);
    size_t num_imports = import_types_vec_raii.vec.size;

    std::vector<wasm_extern_t*> extern_stubs_array(num_imports);

    // Clear and prepare for new host functions and environments
    for (HostFuncEnv* env : ctx->host_envs) {
        delete env; 
    }
    ctx->host_envs.clear();
    ctx->host_funcs.clear();

    if (num_imports > 0) {
        try {
            ctx->host_envs.reserve(num_imports);
            ctx->host_funcs.reserve(num_imports);
        } catch (const std::bad_alloc& e) {
            set_error_msg(ctx, "Alloc for host env/func tracking failed (vector reserve).");
            return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED; // Exits function
        }
    }

    for (size_t i = 0; i < num_imports; ++i) {
        const wasm_importtype_t* import_type = import_types_vec_raii.vec.data[i];
        const wasm_name_t* module_name_vec = wasm_importtype_module(import_type);
        const wasm_name_t* func_name_vec = wasm_importtype_name(import_type);
        const wasm_externtype_t* extern_type = wasm_importtype_type(import_type);
        wasm_externkind_t kind = wasm_externtype_kind(extern_type);

        extern_stubs_array[i] = nullptr; 

        if (kind != WASM_EXTERN_FUNC) {
            continue; 
        }
        
        void* user_function_for_import = nullptr;
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
        if (!found_override && default_import_function != nullptr) {
            user_function_for_import = default_import_function;
        }
        if (user_function_for_import == nullptr && !found_override) {
        }

        const wasm_functype_t* expected_functype = wasm_externtype_as_functype_const(extern_type);
        if (!expected_functype) { 
            std::string_view mod_name_sv(module_name_vec->data, module_name_vec->size);
            std::string_view func_name_sv(func_name_vec->data, func_name_vec->size);
            // Using set_error_msg_v which internally uses vsnprintf, so direct std::format not used here
            // but constructing the strings for the message can be cleaner.
            char error_buf[256]; // Temporary buffer for the combined message
            snprintf(error_buf, sizeof(error_buf), "Could not get functype for import '%.*s::%.*s'.",
                     static_cast<int>(mod_name_sv.length()), mod_name_sv.data(),
                     static_cast<int>(func_name_sv.length()), func_name_sv.data());
            set_error_msg(ctx, error_buf); // Use set_error_msg with the formatted C-string
            return HB_BEAMR_CAPI_LIB_ERROR_NATIVE_LINKING_FAILED;
        }
        
        HostFuncEnv* tramp_env_alloc = new (std::nothrow) HostFuncEnv();
        if (!tramp_env_alloc) {
            set_error_msg(ctx, "Failed to allocate trampoline env.");
            return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
        }
        tramp_env_alloc->user_function_ptr = user_function_for_import;
        tramp_env_alloc->instance_ctx = ctx; // Set the instance_ctx field
        
        wasm_func_t* host_func = wasm_func_new_with_env(
            ctx->store, expected_functype, 
            generic_trampoline_callback, 
            tramp_env_alloc, 
            nullptr ); // host_func_env_finalizer, set to null as we manually delete HostFuncEnv
        
        if (!host_func) { 
             delete tramp_env_alloc; 
             std::string_view mod_name_sv(module_name_vec->data, module_name_vec->size);
             std::string_view func_name_sv(func_name_vec->data, func_name_vec->size);
             char error_buf[256];
             snprintf(error_buf, sizeof(error_buf), "wasm_func_new_with_env failed for '%.*s::%.*s'.",
                      static_cast<int>(mod_name_sv.length()), mod_name_sv.data(),
                      static_cast<int>(func_name_sv.length()), func_name_sv.data());
             set_error_msg(ctx, error_buf);
             return HB_BEAMR_CAPI_LIB_ERROR_NATIVE_LINKING_FAILED;
        }

        extern_stubs_array[i] = wasm_func_as_extern(host_func);
        try {
            ctx->host_funcs.push_back(host_func);
            ctx->host_envs.push_back(tramp_env_alloc);
        } catch (const std::bad_alloc& e) {
            delete tramp_env_alloc;
            wasm_func_delete(host_func); 
            set_error_msg(ctx, "Internal error: failed to push to host_funcs/envs vectors.");
            return HB_BEAMR_CAPI_LIB_ERROR_ALLOCATION_FAILED;
        }
    }

    WasmExternVecRAII imports_for_instance_raii; // RAII wrapper
    wasm_extern_vec_new(&imports_for_instance_raii.vec, num_imports, num_imports > 0 ? extern_stubs_array.data() : nullptr);

    wasm_trap_t* trap = nullptr; 
    ctx->instance = wasm_instance_new(ctx->store, ctx->module, &imports_for_instance_raii.vec, &trap);
    // imports_for_instance_raii destructor will call wasm_extern_vec_delete

    if (trap) {
        wasm_message_t trap_message;
        wasm_trap_message(trap, &trap_message);
        // Use std::string for temporary buffer before set_error_msg_v
        std::string trap_msg_str_cpp(trap_message.size, '\0');
        memcpy(&trap_msg_str_cpp[0], trap_message.data, trap_message.size);
        set_error_msg_v(ctx, "Instantiation trapped: %s", trap_msg_str_cpp.c_str());
        wasm_name_delete(&trap_message); wasm_trap_delete(trap);
        if (ctx->instance) { wasm_instance_delete(ctx->instance); ctx->instance = nullptr;}
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_INSTANTIATE_FAILED;
    }
    if (!ctx->instance) {
        set_error_msg(ctx, "Instantiation failed: Unknown error, no trap.");
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_INSTANTIATE_FAILED;
    }

    // import_types_vec_raii destructor will call wasm_importtype_vec_delete
    set_error_msg(ctx, "Module instantiated successfully.");
    return HB_BEAMR_CAPI_LIB_SUCCESS;
}

// Let's make a specific RAII wrapper for wasm_exporttype_vec_t
struct WasmExporttypeVecRAII {
    wasm_exporttype_vec_t vec;
    WasmExporttypeVecRAII() { wasm_exporttype_vec_new_empty(&vec); }
    ~WasmExporttypeVecRAII() { wasm_exporttype_vec_delete(&vec); }
    WasmExporttypeVecRAII(const WasmExporttypeVecRAII&) = delete;
    WasmExporttypeVecRAII& operator=(const WasmExporttypeVecRAII&) = delete;
};

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

    WasmExternVecRAII exports_vec_raii; // RAII for exports
    wasm_instance_exports(ctx->instance, &exports_vec_raii.vec);
    wasm_func_t* func = nullptr; bool found_func = false;
    
    WasmExporttypeVecRAII module_export_types_raii_actual;
    wasm_module_exports(ctx->module, &module_export_types_raii_actual.vec);

    for (size_t i = 0; i < module_export_types_raii_actual.vec.size; ++i) {
        const wasm_exporttype_t* export_type = module_export_types_raii_actual.vec.data[i];
        const wasm_name_t* name_vec = wasm_exporttype_name(export_type);
        if (names_match(name_vec, func_name)) {
            if (wasm_externtype_kind(wasm_exporttype_type(export_type)) == WASM_EXTERN_FUNC && i < exports_vec_raii.vec.size) {
                func = wasm_extern_as_func(exports_vec_raii.vec.data[i]);
                if (func) found_func = true;
            }
            break; 
        }
    }
    // module_export_types_raii_actual destructor handles delete

    if (!found_func) {
        set_error_msg_v(ctx, "Export func '%s' not found.", func_name);
        // exports_vec_raii destructor handles delete
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_FUNCTION_LOOKUP_FAILED;
    }
    
    wasm_val_vec_t args_as_vec = { .num_elems = num_args, .size = num_args, .data = c_api_args };
    wasm_val_vec_t results_as_vec = { .num_elems = 0, .size = num_results, .data = c_api_results };
    wasm_trap_t* trap = wasm_func_call(func, &args_as_vec, &results_as_vec);
    // exports_vec_raii destructor handles delete

    if (trap) {
        wasm_message_t trap_message;
        wasm_trap_message(trap, &trap_message);
        std::string trap_msg_str_cpp(trap_message.size, '\0');
        memcpy(&trap_msg_str_cpp[0], trap_message.data, trap_message.size);
        set_error_msg_v(ctx, "Call to '%s' trapped: %s", func_name, trap_msg_str_cpp.c_str());
        wasm_name_delete(&trap_message); wasm_trap_delete(trap);
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_CALL_FAILED;
    }
    if (results_as_vec.num_elems != num_results) {
        set_error_msg_v(ctx, "Call '%s' result count mismatch.", func_name);
        return HB_BEAMR_CAPI_LIB_ERROR_WAMR_CALL_FAILED; 
    }
    return HB_BEAMR_CAPI_LIB_SUCCESS;
}

extern "C" {

hb_beamr_capi_lib_rc_t hb_beamr_capi_lib_resolve_import(
    hb_beamr_capi_lib_context_t* ctx, uint32_t num_results, wasm_val_t results[]
) {
    if (!ctx) return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE;
    set_error_msg(ctx, "resolve_import not impl.");
    return HB_BEAMR_CAPI_LIB_ERROR_INVALID_STATE; 
}

} // extern "C"


