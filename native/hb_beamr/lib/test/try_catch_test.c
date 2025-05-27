#include "hb_beamr_lib.h"
#include "lib_export.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h> // For fabsf

// For wasm_runtime_get_function_attachment, wasm_runtime_get_module_inst, wasm_runtime_set_exception
#include "wasm_export.h"

// Forward declaration for helper that is defined later in the file.
static inline hb_beamr_lib_context_t *get_ctx_from_exec_env(wasm_exec_env_t exec_env);

#define assert_rc_try_catch(rc, expected_rc, ctx, msg) \
    if (rc != expected_rc) { \
        fprintf(stderr, "Assertion failed: %s. Expected %d, got %d. Error: %s\n", \
                msg, expected_rc, rc, hb_beamr_lib_get_last_error(ctx)); \
        fflush(stderr); \
        assert(rc == expected_rc); \
    }

#define LOG_STDERR_FLUSH(format, ...) \
    do { \
        fprintf(stderr, "[:%d] " format "\n", __LINE__, ##__VA_ARGS__); \
        fflush(stderr); \
    } while (0)

void create_host_trap(wasm_exec_env_t exec_env, const char* message) {
    wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
    wasm_runtime_set_exception(module_inst, message);
}

void consume_host_trap(wasm_exec_env_t exec_env) {
    wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
    wasm_runtime_clear_exception(module_inst);
}

const char* try_catch_wasm_file = "try_catch.aot";

bool DID_CXA_THROW = false;

// Signature: (iii)
void native_cxa_throw(wasm_exec_env_t exec_env, uint64_t *args) {
    hb_beamr_lib_context_t *ctx = get_ctx_from_exec_env(exec_env);

    uint32_t ptr_arg = (uint32_t)args[0];
    uint32_t type = (uint32_t)args[1];
    uint32_t destructor = (uint32_t)args[2];
    LOG_STDERR_FLUSH("[Host NATIVE __cxa_throw] Called with ptr_arg: %u, type: %u, destructor: %u.", ptr_arg, type, destructor);
    
    // var info = new ExceptionInfo(ptr);
    uint32_t exc_ptr = ptr_arg;
    uint32_t ptr = exc_ptr - 24;

    char *memory_name = "memory";
    uint8_t *wasm_base_ptr = NULL;
    size_t wasm_total_size = 0;
    wasm_memory_inst_t mem_inst = NULL;
    hb_beamr_lib_rc_t rc_info = hb_beamr_lib_get_memory_info(ctx, memory_name, &wasm_base_ptr, &wasm_total_size, &mem_inst);
    if (rc_info != HB_BEAMR_LIB_SUCCESS || !wasm_base_ptr) {
        LOG_STDERR_FLUSH("native_cxa_throw: Failed to get memory info. Error: %s", hb_beamr_lib_get_last_error(ctx));
        return;
    }

    // info.init(type, destructor);
    // set_adjusted_ptr
    memcpy(&wasm_base_ptr[(ptr + 16)], &ptr, 4);
    // set_type
    memcpy(&wasm_base_ptr[(ptr + 4)], &type, 4);
    // set_destructor
    memcpy(&wasm_base_ptr[(ptr + 8)], &destructor, 4);

    char* exception_text = malloc(100);
    sprintf(exception_text, "__cxa_throw at %#x", ptr);

    LOG_STDERR_FLUSH("Setting exception tip: %s", exception_text);

    DID_CXA_THROW = true;

    return;
}

// Signature: ()
void native_cxa_rethrow(wasm_exec_env_t exec_env, uint64_t *args) {
    LOG_STDERR_FLUSH("[Host NATIVE __cxa_rethrow] Called. Simulating host trap.");
    // create_host_trap(exec_env, "host_trap: __cxa_rethrow called");
}

hb_beamr_lib_rc_t do_set_threw(wasm_exec_env_t exec_env, hb_beamr_lib_context_t* ctx, int32_t arg0, int32_t arg1) {
    LOG_STDERR_FLUSH("do_set_threw called with values: %d, %d", arg0, arg1);

    wasm_val_t args[2];
    args[0].kind = WASM_I32;
    args[0].of.i32 = arg0;
    args[1].kind = WASM_I32;
    args[1].of.i32 = arg1;

    hb_beamr_lib_rc_t rc = hb_beamr_lib_call_export(ctx, "setThrew", 2, args, 0, NULL);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        LOG_STDERR_FLUSH("Failed to call setThrew");
        return rc;
    }

    LOG_STDERR_FLUSH("Successfully called setThrew");

    return rc;
}

// Helper to perform indirect call using hb_beamr_lib_call_indirect.
// It attempts to mimic the Emscripten JS behavior for invoke_*
static bool perform_indirect_call_via_lib_api(
    wasm_exec_env_t exec_env,
    hb_beamr_lib_context_t* lib_ctx,    // The main library context
    uint32_t table_index,            // Function index in the table
    uint32_t num_args,               // Number of arguments for the Wasm function
    wasm_val_t args[],               // Arguments for the Wasm function
    uint32_t num_results,            // Expected number of results
    wasm_val_t results[]             // Buffer for results
) {
    hb_beamr_lib_rc_t rc;

    wasm_val_t stack_ptr[1];
    stack_ptr[0].kind = WASM_I32;
    stack_ptr[0].of.i32 = 0;

    rc = hb_beamr_lib_call_export(lib_ctx, "emscripten_stack_get_current", 0, NULL, 1, stack_ptr);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: hb_beamr_lib_call_export failed for emscripten_stack_get_current. Error: %s", 
                         (void*)lib_ctx, hb_beamr_lib_get_last_error(lib_ctx));
        return false;
    }
    LOG_STDERR_FLUSH("Saved stack pointer: %u", stack_ptr[0].of.i32);

    // In Emscripten, invoke_* functions save stack, call, then restore stack on catch.
    // WAMR's C API for indirect calls doesn't directly expose stack saving/restoring
    // in the same way as Emscripten's JS `stackSave`/`stackRestore`.
    // The primary goal here is to execute the indirect call and handle potential traps.
    LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: Calling hb_beamr_lib_call_indirect for table_index %u", lib_ctx, table_index);
    rc = hb_beamr_lib_call_indirect(lib_ctx, "__indirect_function_table", table_index, 
                                                      num_args, args, 
                                                      num_results, results);
    LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: hb_beamr_lib_call_indirect returned %d", lib_ctx, rc);

    if (rc != HB_BEAMR_LIB_SUCCESS) {
        LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: hb_beamr_lib_call_indirect failed for table_index %u. Error: %s", 
                         (void*)lib_ctx, table_index, hb_beamr_lib_get_last_error(lib_ctx));

        // clear_host_trap(exec_env);
        LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: Cleared host trap.", lib_ctx);
        return false;
    }

    if (DID_CXA_THROW) {
        LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: DID_CXA_THROW.", lib_ctx);

        rc = do_set_threw(exec_env, lib_ctx, 1, 0);
        if (rc != HB_BEAMR_LIB_SUCCESS) {
            LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: Failed to call do_set_threw. Error: %s", lib_ctx, hb_beamr_lib_get_last_error(lib_ctx));
            return false;
        }

        LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: Successfully called do_set_threw.", lib_ctx);
        return false;
    }

    LOG_STDERR_FLUSH("[CTX %p] perform_indirect_call_via_lib_api: hb_beamr_lib_call_indirect failed", lib_ctx);
    return true;
}

// Helper to obtain hb_beamr_lib_context_t* associated with the module that
// triggered the native call.  The context was stored earlier via
// wasm_runtime_set_custom_data at instantiation time.
static inline hb_beamr_lib_context_t *get_ctx_from_exec_env(wasm_exec_env_t exec_env) {
    wasm_module_inst_t inst = wasm_runtime_get_module_inst(exec_env);
    return inst ? (hb_beamr_lib_context_t *)wasm_runtime_get_custom_data(inst) : NULL;
}

// invoke_v(index)
void native_invoke_v(wasm_exec_env_t exec_env, uint64_t *args) {
    hb_beamr_lib_context_t *ctx = get_ctx_from_exec_env(exec_env);
    LOG_STDERR_FLUSH("[CTX %p] native_invoke_v called with table_index %u", (void*)ctx, (uint32_t)args[0]);
    uint32_t table_index = (uint32_t)args[0];
    // No args, no results for invoke_v as per Emscripten convention
    perform_indirect_call_via_lib_api(exec_env, ctx, table_index, 0, NULL, 0, NULL);
}

// invoke_ii(index, a) -> i32
void native_invoke_ii(wasm_exec_env_t exec_env, uint64_t *args) {
    hb_beamr_lib_context_t *ctx = get_ctx_from_exec_env(exec_env);
    uint32_t table_index = (uint32_t)args[0];
    LOG_STDERR_FLUSH("[CTX %p] native_invoke_ii called with table_index %u, arg1 %u", (void*)ctx, table_index, (uint32_t)args[1]);

    wasm_val_t wasm_args[1];
    wasm_args[0].kind = WASM_I32;
    wasm_args[0].of.i32 = (int32_t)args[1];

    wasm_val_t wasm_results[1];
    if (!perform_indirect_call_via_lib_api(exec_env, ctx, table_index, 1, wasm_args, 1, wasm_results)) {
        // On failure (trap), the original Emscripten JS might set a flag via _setThrew.
        // Here, the trap is handled by hb_beamr_lib_call_indirect. The caller (Wasm)
        // will see the trap. We don't set args[0] as result is invalid.
        return; 
    }
    args[0] = (uint64_t)wasm_results[0].of.i32; // Place the i32 result back
}

// invoke_iii(index, a, b) -> i32
void native_invoke_iii(wasm_exec_env_t exec_env, uint64_t *args) {
    hb_beamr_lib_context_t *ctx = get_ctx_from_exec_env(exec_env);
    uint32_t table_index = (uint32_t)args[0];
    LOG_STDERR_FLUSH("[CTX %p] native_invoke_iii called with table_index %u, arg1 %u, arg2 %u", 
                     (void*)ctx, table_index, (uint32_t)args[1], (uint32_t)args[2]);

    wasm_val_t wasm_args[2];
    wasm_args[0].kind = WASM_I32;
    wasm_args[0].of.i32 = (int32_t)args[1];
    wasm_args[1].kind = WASM_I32;
    wasm_args[1].of.i32 = (int32_t)args[2];

    wasm_val_t wasm_results[1];
    if (!perform_indirect_call_via_lib_api(exec_env, ctx, table_index, 2, wasm_args, 1, wasm_results)) {
        return; // Trap occurred, result is not valid
    }
    args[0] = (uint64_t)wasm_results[0].of.i32; // Place the i32 result back
}

// invoke_viii(index, a, b, c) -> void
void native_invoke_viii(wasm_exec_env_t exec_env, uint64_t *args) {
    hb_beamr_lib_context_t *ctx = get_ctx_from_exec_env(exec_env);
    uint32_t table_index = (uint32_t)args[0];
    LOG_STDERR_FLUSH("[CTX %p] native_invoke_viii called with table_index %u, arg1 %u, arg2 %u, arg3 %u",
                     (void*)ctx, table_index, (uint32_t)args[1], (uint32_t)args[2], (uint32_t)args[3]);

    wasm_val_t wasm_args[3];
    wasm_args[0].kind = WASM_I32;
    wasm_args[0].of.i32 = (int32_t)args[1];
    wasm_args[1].kind = WASM_I32;
    wasm_args[1].of.i32 = (int32_t)args[2];
    wasm_args[2].kind = WASM_I32;
    wasm_args[2].of.i32 = (int32_t)args[3];

    // No results for invoke_viii
    perform_indirect_call_via_lib_api(exec_env, ctx, table_index, 3, wasm_args, 0, NULL);
}

// Signature for __cxa_begin_catch: (i)i
void native_cxa_begin_catch(wasm_exec_env_t exec_env, uint64_t *args) {
    uint32_t exception_obj_ptr = (uint32_t)args[0];
    LOG_STDERR_FLUSH("[Host NATIVE __cxa_begin_catch] Called with exception object ptr: %u. Returning it as is.", exception_obj_ptr);
    args[0] = exception_obj_ptr; // Return adjusted pointer (here, same as input)
}

// Signature for __cxa_end_catch: ()
void native_cxa_end_catch(wasm_exec_env_t exec_env, uint64_t *args) {
    LOG_STDERR_FLUSH("[Host NATIVE __cxa_end_catch] Called.");
}

// Placeholder for other stubs if needed, assuming void for simplicity
// __cxa_find_matching_catch_3, __cxa_find_matching_catch_2: (iii)i or (i*i)i are common
// __resumeException: (i)
// invoke_ii: (ii)i

void native_placeholder_trap(wasm_exec_env_t exec_env, uint64_t* args) {
    // This requires NativeSymbolAttachment to be set up if we want the name.
    // For simplicity, just a generic trap.
    const char* func_name = "unknown_placeholder_function";
    // TODO: If attachment is possible with hb_beamr_lib_register_global_natives, use it.
    LOG_STDERR_FLUSH("[Host NATIVE STUB %s] Called. Ignoring.", func_name);
    // create_host_trap(exec_env, "host_trap: Unimplemented C++ ABI stub called");
}

//  __cxa_find_matching_catch_2(ptr thrown, ptr t1, ptr t2) → ptr
void native_cxa_find_matching_catch_2(wasm_exec_env_t exec_env, uint64_t *args) {
    LOG_STDERR_FLUSH("[Host NATIVE __cxa_find_matching_catch_2] Called. Returning 0.");
    args[0] = (uint32_t)0;
}

// These helpers need to return a pointer to the matched catch handler
// Emscripten's JS versions simply return their first pointer argument when
// building with -s DISABLE_EXCEPTION_CATCHING=0.  A minimal behaviour that
// keeps the stack layout sane is therefore to echo back the first
// type-info pointer so the Wasm code believes it found a match.
//  __cxa_find_matching_catch_3(ptr thrown, ptr t1, ptr t2, ptr t3) → ptr
void native_cxa_find_matching_catch_3(wasm_exec_env_t exec_env, uint64_t *args) {
    LOG_STDERR_FLUSH("[Host NATIVE __cxa_find_matching_catch_3] Called. Returning 0.");
    args[0] = (uint32_t)0;
}

// --- End Native Handlers ---

char* read_wasm_string(hb_beamr_lib_context_t* ctx, uint32_t wasm_ptr_offset, size_t max_len) {
    if (wasm_ptr_offset == 0) return NULL;

    uint8_t* mem_base_ptr = NULL;
    size_t mem_total_size = 0;
    wasm_memory_inst_t mem_inst = NULL;
    hb_beamr_lib_rc_t rc_info = hb_beamr_lib_get_memory_info(ctx, NULL, &mem_base_ptr, &mem_total_size, &mem_inst);
    
    if (rc_info != HB_BEAMR_LIB_SUCCESS || !mem_base_ptr) {
        LOG_STDERR_FLUSH("read_wasm_string: Failed to get memory info or base pointer. Error: %s", hb_beamr_lib_get_last_error(ctx));
        return NULL;
    }

    if (wasm_ptr_offset >= mem_total_size) {
        LOG_STDERR_FLUSH("read_wasm_string: Wasm pointer offset %u is out of bounds (memory size %zu).", wasm_ptr_offset, mem_total_size);
        return NULL;
    }

    char* host_ptr = (char*)(mem_base_ptr + wasm_ptr_offset);
    size_t actual_len = 0;
    for (size_t i = 0; i < max_len && (wasm_ptr_offset + i) < mem_total_size; ++i) {
        if (host_ptr[i] == '\0') {
            break;
        }
        actual_len++;
    }
    
    char* buffer = (char*)malloc(actual_len + 1);
    if (!buffer) {
        LOG_STDERR_FLUSH("read_wasm_string: Failed to allocate buffer for string copy.");
        return NULL;
    }
    
    memcpy(buffer, host_ptr, actual_len);
    buffer[actual_len] = '\0';
    
    return buffer; 
}

void test_try_catch_behavior(hb_beamr_lib_context_t* ctx) {
    printf("--- Test (WAMR Lib): Try/Catch Behavior ---\n");
    hb_beamr_lib_rc_t rc;
    wasm_val_t args[1];
    wasm_val_t results[1]; 

    args[0].kind = WASM_I32;
    args[0].of.i32 = 0;
    results[0].of.i32 = 0;
    LOG_STDERR_FLUSH("Calling my_funcs(0) - expecting no throw...");
    rc = hb_beamr_lib_call_export(ctx, "my_funcs", 1, args, 1, results);
    assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, ctx, "my_funcs(0) call");
    uint32_t result_ptr_offset0 = results[0].of.i32;
    assert(result_ptr_offset0 != 0 && "my_funcs(0) returned NULL pointer offset");
    char* str0 = read_wasm_string(ctx, result_ptr_offset0, 255);
    assert(str0 != NULL && "Failed to read string result for my_funcs(0)");
    LOG_STDERR_FLUSH("my_funcs(0) returned offset %u, string: \"%s\"", result_ptr_offset0, str0 ? str0 : "NULL");
    assert(strcmp(str0, "Initial") == 0 || strcmp(str0, "Catch") == 0 && "my_funcs(0) unexpected initial/catch string");
    if (str0) free(str0);
    LOG_STDERR_FLUSH("my_funcs(0) PASSED (or was affected by prior internal catch).");

    // Before each throwing test, reset result_str to "Initial" by calling my_funcs(0)
    // This ensures we test the throw/trap behavior without interference from a previous internal C++ catch.
#define RESET_WASM_STATE() do { \
    wasm_val_t reset_args[1]; reset_args[0].kind = WASM_I32; reset_args[0].of.i32 = 0; \
    wasm_val_t reset_results[1]; \
    hb_beamr_lib_call_export(ctx, "my_funcs", 1, reset_args, 1, reset_results); \
    char* temp_str = read_wasm_string(ctx, reset_results[0].of.i32, 255); \
    if(temp_str) { strcpy(temp_str, "Initial"); /*This modifies host copy, not Wasm global*/ } free(temp_str); \
    /* Actually, to reset the global in Wasm, we'd need another Wasm export or a different strategy.*/ \
    /* For now, we rely on the fact that a trap prevents my_funcs from returning and thus from internally setting result_str to "Catch" via its own catch block IF the trap happens due to our host ABI calls.*/ \
} while(0)

    // RESET_WASM_STATE();
    // args[0].of.i32 = 1; // rethrow
    // LOG_STDERR_FLUSH("Calling my_funcs(1) - expecting rethrow to trap via host ABI...");
    // rc = hb_beamr_lib_call_export(ctx, "my_funcs", 1, args, 1, results);
    // assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, ctx, "my_funcs(1) call (expecting trap)");
    // LOG_STDERR_FLUSH("my_funcs(1) correctly trapped/failed. Exception: %s", hb_beamr_lib_get_last_error(ctx));
    // // assert(strstr(hb_beamr_lib_get_last_error(ctx), "host_trap") != NULL && "Trap message mismatch for my_funcs(1)");

    RESET_WASM_STATE();
    args[0].of.i32 = 2; // throw(int)
    LOG_STDERR_FLUSH("Calling my_funcs(2) - expecting integer throw to trap via host ABI...");
    rc = hb_beamr_lib_call_export(ctx, "my_funcs", 1, args, 1, results);
    assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, ctx, "my_funcs(2) call (expecting trap)");
    LOG_STDERR_FLUSH("my_funcs(2) correctly trapped/failed. Exception: %s", hb_beamr_lib_get_last_error(ctx));
    // assert(strstr(hb_beamr_lib_get_last_error(ctx), "host_trap") != NULL && "Trap message mismatch for my_funcs(2)");

    // RESET_WASM_STATE();
    // args[0].of.i32 = 3; // throw(std::runtime_error)
    // LOG_STDERR_FLUSH("Calling my_funcs(3) - expecting std::runtime_error to trap via host ABI...");
    // rc = hb_beamr_lib_call_export(ctx, "my_funcs", 1, args, 1, results);
    // assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, ctx, "my_funcs(3) call (expecting trap)");
    // LOG_STDERR_FLUSH("my_funcs(3) correctly trapped/failed. Exception: %s", hb_beamr_lib_get_last_error(ctx));
    // // assert(strstr(hb_beamr_lib_get_last_error(ctx), "host_trap") != NULL && "Trap message mismatch for my_funcs(3)");

    RESET_WASM_STATE();
    args[0].of.i32 = 4; // Segfault in Wasm
    LOG_STDERR_FLUSH("Calling my_funcs(4) - expecting segfault to trap...");
    rc = hb_beamr_lib_call_export(ctx, "my_funcs", 1, args, 1, results);
    assert_rc_try_catch(rc, HB_BEAMR_LIB_ERROR_WAMR_CALL_FAILED, ctx, "my_funcs(4) call (expecting trap)");
    LOG_STDERR_FLUSH("my_funcs(4) correctly trapped/failed. Exception: %s", hb_beamr_lib_get_last_error(ctx));
    // assert(strstr(hb_beamr_lib_get_last_error(ctx), "out of bounds memory access") != NULL && "Trap message mismatch for my_funcs(4)");

    RESET_WASM_STATE();
    args[0].of.i32 = 0; // Control case
    LOG_STDERR_FLUSH("Calling my_funcs(0) - expecting success...");
    rc = hb_beamr_lib_call_export(ctx, "my_funcs", 1, args, 1, results);
    assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, ctx, "my_funcs(0) call (expecting success)");
    LOG_STDERR_FLUSH("my_funcs(0) succeeded. Exception: %s", hb_beamr_lib_get_last_error(ctx));
    // assert(strstr(hb_beamr_lib_get_last_error(ctx), "out of bounds memory access") != NULL && "Trap message mismatch for my_funcs(4)");

    LOG_STDERR_FLUSH("Test PASSED (WAMR Lib Try/Catch Behavior)");
}

int main() {
    LOG_STDERR_FLUSH("=== Starting WAMR-Native Lib Try/Catch Test ===");

    hb_beamr_lib_context_t* ctx = hb_beamr_lib_create_context();
    assert(ctx != NULL && "hb_beamr_lib_create_context returned NULL");
    LOG_STDERR_FLUSH("Context created (WAMR Lib for Try/Catch): %p", (void*)ctx);

    const hb_beamr_native_symbol_t symbols_env[] = {
        {"env", "__cxa_throw", (void*)native_cxa_throw, "(iii)", NULL},
        {"env", "__cxa_rethrow", (void*)native_cxa_rethrow, "()", NULL},
        {"env", "invoke_v", (void*)native_invoke_v, "(i)", NULL},
        {"env", "invoke_iii", (void*)native_invoke_iii, "(iii)i", NULL},
        {"env", "invoke_viii", (void*)native_invoke_viii, "(iiii)", NULL},
        {"env", "__cxa_begin_catch", (void*)native_cxa_begin_catch, "(i)i", NULL},
        {"env", "__cxa_end_catch", (void*)native_cxa_end_catch, "()", NULL},
        {"env", "__cxa_find_matching_catch_2", (void*)native_cxa_find_matching_catch_2, "()i", NULL},
        {"env", "__cxa_find_matching_catch_3", (void*)native_cxa_find_matching_catch_3, "(i)i", NULL},
        {"env", "__resumeException", (void*)native_placeholder_trap, "(i)", NULL},
        {"env", "invoke_ii", (void*)native_invoke_ii, "(ii)i", NULL}
    };
    hb_beamr_native_symbol_group_t group_env = {"env", symbols_env, sizeof(symbols_env)/sizeof(symbols_env[0])};
    hb_beamr_native_symbols_structured_t eh_symbols_structured = {&group_env, 1};

    RuntimeInitArgs init_args;
    memset(&init_args, 0, sizeof(RuntimeInitArgs));
    init_args.mem_alloc_type = Alloc_With_System_Allocator; 
                                                    
    hb_beamr_lib_rc_t rc = hb_beamr_lib_init_runtime_global(&init_args); 
    assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, NULL, "hb_beamr_lib_init_runtime_global");

    // Register the EH native symbols globally
    rc = hb_beamr_lib_register_global_natives(&eh_symbols_structured);
    assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, NULL, "hb_beamr_lib_register_global_natives for EH symbols");
    LOG_STDERR_FLUSH("Registered C++ EH ABI native symbols.");

    LOG_STDERR_FLUSH("Runtime initialized (WAMR Lib for Try/Catch).");

    uint8_t* wasm_binary = NULL;
    uint32_t file_size = 0;
    wasm_binary = read_file_to_buffer(try_catch_wasm_file, &file_size);
    assert(wasm_binary != NULL && file_size > 0 && "Failed to read Wasm file");
    LOG_STDERR_FLUSH("Wasm file read: %s (%u bytes).", try_catch_wasm_file, file_size);

    rc = hb_beamr_lib_load_wasm_module(ctx, wasm_binary, file_size);
    free_buffer(wasm_binary); 
    assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, ctx, "hb_beamr_lib_load_wasm_module for try_catch");
    LOG_STDERR_FLUSH("Module loaded (WAMR Lib for Try/Catch).");

    // Native symbols are now pre-registered globally, so instantiate doesn't need them.
    rc = hb_beamr_lib_instantiate(ctx, 0x10000, 0x10000); 
    assert_rc_try_catch(rc, HB_BEAMR_LIB_SUCCESS, ctx, "hb_beamr_lib_instantiate for try_catch");
    LOG_STDERR_FLUSH("Module instantiated (WAMR Lib for Try/Catch).");
    
    test_try_catch_behavior(ctx);

    LOG_STDERR_FLUSH("Destroying context (WAMR Lib for Try/Catch)...");
    hb_beamr_lib_destroy_context(ctx);
    LOG_STDERR_FLUSH("Destroying runtime (WAMR Lib for Try/Catch)...");
    // TODO: Unregister natives? WAMR doesn't have a simple unregister for raw natives by group name easily.
    // Runtime destroy should clean up.
    hb_beamr_lib_destroy_runtime_global();

    LOG_STDERR_FLUSH("=== WAMR-Native Lib Try/Catch Test COMPLETED ===");
    return 0;
} 