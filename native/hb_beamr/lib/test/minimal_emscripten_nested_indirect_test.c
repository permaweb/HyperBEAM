#include "utils.h"
#include "hb_beamr_lib.h"
#include "wasm_export.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Global context for the host import handler to use hb_beamr_lib functions
static hb_beamr_lib_context_t* g_test_ctx = NULL;

// Host import handler for my_simple_host_call from minimal_host_call.cpp
static void c_my_simple_host_call_handler(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) {
    printf("[NATIVE STUB env.my_simple_host_call] Called. ARGC: %u.\n", argc);
    if (argc > 0) {
        printf("[NATIVE STUB env.my_simple_host_call] Received value from Wasm: %d\n", (int32_t)argv[0]);
    }
    
    if (!g_test_ctx) {
        fprintf(stderr, "[NATIVE STUB env.my_simple_host_call] FATAL: g_test_ctx is NULL.\n");
        // In a real scenario, this might need to trigger a Wasm trap if possible.
        return; 
    }
    const char* table_to_call = "__indirect_function_table";
    // minimal_host_call.wasm, as compiled by Emscripten without explicit table exports/elements,
    // might not have __indirect_function_table, or it might be empty or not have index 0.
    // This indirect call is therefore expected to fail cleanly if the table/function isn't there.
    // The main goal here is to see if *this C host function gets called at all*.
    // If it does, and *then* this indirect call causes a bus error, we've found something.
    uint32_t index_to_call = 0; 
    printf("[NATIVE STUB env.my_simple_host_call] Attempting indirect call (expected to fail cleanly if table/func absent or to reproduce bus error if conditions met): table='%s', index=%u\n",
           table_to_call, index_to_call);

    hb_beamr_lib_rc_t rc = hb_beamr_lib_call_indirect(g_test_ctx, table_to_call, index_to_call, 0, NULL, 0, NULL);

    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "[NATIVE STUB env.my_simple_host_call] hb_beamr_lib_call_indirect result: %s (RC: %d)\n", 
                hb_beamr_lib_get_last_error(g_test_ctx), rc);
    } else {
        printf("[NATIVE STUB env.my_simple_host_call] hb_beamr_lib_call_indirect reported SUCCESS.\n");
    }
    printf("[NATIVE STUB env.my_simple_host_call] Finished.\n");
}

// Dummy stubs for other Emscripten imports from erlang_test_try.wasm
static void dummy_invoke_v(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.invoke_v] Called. ARGC: %u\n", argc); if (argc > 0) printf("  env.invoke_v arg0: %d\n", (int32_t)argv[0]); }
static int32_t dummy_invoke_iii(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.invoke_iii] Called. ARGC: %u\n", argc); return 0; }
static void dummy_cxa_throw(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.__cxa_throw] Called. ARGC: %u\n", argc); }
static void dummy_invoke_viii(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.invoke_viii] Called. ARGC: %u\n", argc); }
static int32_t dummy_cxa_find_matching_catch_3(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.__cxa_find_matching_catch_3] Called. ARGC: %u\n", argc); return 0; }
static int32_t dummy_cxa_begin_catch(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.__cxa_begin_catch] Called. ARGC: %u\n", argc); return 0; }
static void dummy_cxa_end_catch(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.__cxa_end_catch] Called.\n"); }
static int32_t dummy_cxa_find_matching_catch_2(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.__cxa_find_matching_catch_2] Called.\n"); return 0; }
static void dummy_resumeException(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.__resumeException] Called. ARGC: %u\n", argc); }
static int32_t dummy_invoke_ii(wasm_exec_env_t exec_env, uint32_t idx, uint32_t argc, uint64_t argv[]) { printf("[NATIVE STUB env.invoke_ii] Called. ARGC: %u\n", argc); return 0; }
// emscripten_notify_memory_growth was not in the import list for erlang_test_try.wasm

int main(int argc, char *argv[]) {
    printf("--- Minimal Emscripten Nested Indirect Call Test (using minimal_host_call.aot) ---\n");

    if (hb_beamr_lib_init_runtime_global(NULL) != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Failed to initialize runtime.\n"); return 1;
    }
    printf("Runtime initialized.\n");

    hb_beamr_lib_context_t* ctx = hb_beamr_lib_create_context();
    if (!ctx) {
        fprintf(stderr, "Failed to create context.\n"); 
        hb_beamr_lib_destroy_runtime_global(); return 1;
    }
    g_test_ctx = ctx; 
    printf("Context created: %p\n", (void*)ctx);

    const char* module_path = "minimal_host_call.aot"; // Use the new simple AOT
    uint8_t* module_buffer = NULL;
    uint32_t module_size = 0;
    printf("Loading AOT module: %s...\n", module_path);
    if (!(module_buffer = read_file_to_buffer(module_path, &module_size))) {
        fprintf(stderr, "Failed to load AOT module '%s'. Error: %s\n", 
                module_path, hb_beamr_lib_get_last_error(ctx) ? hb_beamr_lib_get_last_error(ctx) : "(ctx error not set)");
        hb_beamr_lib_destroy_context(ctx); hb_beamr_lib_destroy_runtime_global(); return 1;
    }
    printf("AOT module '%s' loaded (%u bytes).\n", module_path, module_size);

    hb_beamr_lib_rc_t load_rc = hb_beamr_lib_load_aot_module(ctx, module_buffer, module_size);
    free(module_buffer);
    if (load_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "hb_beamr_lib_load_aot_module for '%s' failed: %s (RC: %d)\n", 
                module_path, hb_beamr_lib_get_last_error(ctx), load_rc);
        hb_beamr_lib_destroy_context(ctx); hb_beamr_lib_destroy_runtime_global(); return 1;
    }
    printf("AOT module '%s' processed by hb_beamr_lib.\n", module_path);

    // Define native for env.my_simple_host_call - Wasm sig: (i32) -> nil, WAMR sig: "(i)"
    hb_beamr_native_symbol_t env_native_symbols[] = {
        { NULL, "my_simple_host_call", (void*)c_my_simple_host_call_handler, "(i)", NULL }
        // If Emscripten prepends an underscore, this would be "_my_simple_host_call"
    };

    hb_beamr_native_symbol_group_t native_groups[] = {
        {
            .module_name = "env", // Emscripten usually puts C imports in "env"
            .num_symbols = sizeof(env_native_symbols) / sizeof(hb_beamr_native_symbol_t),
            .symbols = env_native_symbols
        }
    };
    hb_beamr_native_symbols_structured_t structured_natives = {
        .num_groups = sizeof(native_groups) / sizeof(hb_beamr_native_symbol_group_t),
        .groups = native_groups
    };

    printf("Registering native for 'env.my_simple_host_call'...\n");
    hb_beamr_lib_rc_t reg_rc = hb_beamr_lib_register_global_natives(&structured_natives);
    if (reg_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "hb_beamr_lib_register_global_natives failed: %s (RC: %d)\n", 
                hb_beamr_lib_get_last_error(ctx), reg_rc);
        hb_beamr_lib_destroy_context(ctx); hb_beamr_lib_destroy_runtime_global(); return 1;
    }
    printf("Native registered for 'env.my_simple_host_call'.\n");

    printf("Instantiating module '%s'...\n", module_path);
    hb_beamr_lib_rc_t inst_rc = hb_beamr_lib_instantiate(ctx, 0, 0, NULL );
    if (inst_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "hb_beamr_lib_instantiate for '%s' failed: %s (RC: %d)\n", 
                module_path, hb_beamr_lib_get_last_error(ctx), inst_rc);
        hb_beamr_lib_destroy_context(ctx); hb_beamr_lib_destroy_runtime_global(); return 1;
    }
    printf("Module '%s' instantiated.\n", module_path);

    printf("Preparing to call exported Wasm function 'handle' from '%s'...\n", module_path);
    
    uint32_t wasm_env_str_ptr = 0;
    const char* env_arg_for_handle = "1"; // To make my_simple_host_call receive 1

    wasm_val_t malloc_args[1];
    malloc_args[0].kind = WASM_I32;
    malloc_args[0].of.i32 = strlen(env_arg_for_handle) + 1;
    wasm_val_t malloc_results[1];

    printf("Calling Wasm 'malloc' for env_arg_for_handle...\n");
    hb_beamr_lib_rc_t export_call_rc = hb_beamr_lib_call_export(ctx, "malloc", 1, malloc_args, 1, malloc_results);
    if (export_call_rc != HB_BEAMR_LIB_SUCCESS || malloc_results[0].of.i32 == 0) {
        fprintf(stderr, "Failed to call Wasm 'malloc': %s\n", hb_beamr_lib_get_last_error(ctx));
        hb_beamr_lib_destroy_context(ctx); hb_beamr_lib_destroy_runtime_global(); return 1;
    }
    wasm_env_str_ptr = malloc_results[0].of.i32;
    printf("Wasm 'malloc' returned pointer: %u\n", wasm_env_str_ptr);

    printf("Writing env_arg_for_handle string (\"%s\") to Wasm memory at %u...\n", env_arg_for_handle, wasm_env_str_ptr);
    hb_beamr_lib_rc_t write_rc = hb_beamr_lib_direct_write_memory(ctx, wasm_env_str_ptr, 
                                                                (const uint8_t*)env_arg_for_handle, 
                                                                strlen(env_arg_for_handle) + 1);
    if (write_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "hb_beamr_lib_direct_write_memory failed: %s\n", hb_beamr_lib_get_last_error(ctx));
        hb_beamr_lib_destroy_context(ctx); hb_beamr_lib_destroy_runtime_global(); return 1;
    }
    printf("String written to Wasm memory.\n");

    wasm_val_t handle_call_args[2];
    handle_call_args[0].kind = WASM_I32; 
    handle_call_args[0].of.i32 = 0;      // msg: char* (NULL)
    handle_call_args[1].kind = WASM_I32; 
    handle_call_args[1].of.i32 = wasm_env_str_ptr; // env_str: char*

    wasm_val_t handle_call_results[1]; // handle returns char*

    printf("Calling exported Wasm function 'handle' with msg=NULL, env_str@%u ('%s')...\n", 
           wasm_env_str_ptr, env_arg_for_handle);
    // This call should trigger env.my_simple_host_call, which calls c_my_simple_host_call_handler,
    // which then attempts the indirect call to table[0].
    export_call_rc = hb_beamr_lib_call_export(ctx, "handle", 2, handle_call_args, 1, handle_call_results);
    
    if (export_call_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Call to exported Wasm function 'handle' failed: %s (RC: %d)\n", 
                hb_beamr_lib_get_last_error(ctx), export_call_rc);
    } else {
        printf("Call to Wasm 'handle' apparently succeeded. Result pointer: %u\n", handle_call_results[0].of.i32);
    }
    printf("Call to 'handle' finished processing.\n");

    if (wasm_env_str_ptr != 0) {
        wasm_val_t free_args[1];
        free_args[0].kind = WASM_I32;
        free_args[0].of.i32 = wasm_env_str_ptr;
        printf("Calling Wasm 'free' for env_arg_for_handle string at %u...\n", wasm_env_str_ptr);
        hb_beamr_lib_call_export(ctx, "free", 1, free_args, 0, NULL); 
    }

    printf("Cleaning up context and runtime...\n");
    hb_beamr_lib_destroy_context(ctx);
    g_test_ctx = NULL;
    hb_beamr_lib_destroy_runtime_global();
    printf("--- Minimal Emscripten Nested Indirect Call Test Finished ---\n");
    return 0; 
} 