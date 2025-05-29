#include "hb_beamr_lib.h"
#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <assert.h>
#include <stdbool.h> // For bool

// Re-use read_file_to_buffer (ideally in a shared test_utils.c)
uint8_t* read_file_to_buffer(const char* filename, uint32_t* out_size) {
    FILE* file = fopen(filename, "rb");
    if (!file) { fprintf(stderr, "ERROR: Cannot open file %s\n", filename); return NULL; }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    if (size < 0) { fclose(file); fprintf(stderr, "ERROR: File size invalid %s\n", filename); return NULL; }
    *out_size = (uint32_t)size;
    uint8_t* buffer = (uint8_t*)malloc(*out_size);
    if (!buffer) { fclose(file); fprintf(stderr, "ERROR: Malloc failed for %s\n", filename); return NULL; }
    size_t read_size = fread(buffer, 1, *out_size, file);
    fclose(file);
    if (read_size != *out_size) { free(buffer); fprintf(stderr, "ERROR: Read size mismatch %s\n", filename); return NULL; }
    return buffer;
}

// Actual typed C function
static int32_t actual_host_add_one_impl(wasm_exec_env_t exec_env, int32_t val) {
    printf("[actual_host_add_one_impl] Called with: %d\n", val);
    return val + 1;
}

// Generic dispatcher registered with WAMR
// WAMR AOT expects a C function pointer that takes exec_env and uint64_t* (or uint32_t* sometimes) for arguments/results.
// The Wasm signature "(i)i" tells WAMR how to pack/unpack for this raw function.
static void generic_dispatcher_for_add_one(wasm_exec_env_t exec_env, uint64_t *args_raw) {
    // Arg 0 (input int32_t) is in args_raw[0]
    int32_t input_val = (int32_t)args_raw[0];
    printf("[generic_dispatcher_for_add_one] Received raw arg[0]=0x%llx, interpreted as input_val=%d\n", (unsigned long long)args_raw[0], input_val);

    int32_t result = actual_host_add_one_impl(exec_env, input_val);

    // Result (output int32_t) goes back into args_raw[0]
    args_raw[0] = (uint64_t)(uint32_t)result; // Ensure proper zero/sign extension if int32_t is placed in uint64_t slot
    printf("[generic_dispatcher_for_add_one] Placed result=%d into raw arg[0]=0x%llx\n", result, (unsigned long long)args_raw[0]);
}

int main() {
    printf("Running test: Import Call Test (generic dispatcher, explicit Wasm signature)\n");
    bool test_succeeded = false; // Initialize to false
    hb_beamr_lib_context_t* ctx = NULL; // Initialize to NULL
    uint8_t* aot_file_buffer = NULL; // Initialize to NULL

    hb_beamr_lib_rc_t rc = hb_beamr_lib_init_runtime_global(NULL);
    assert(rc == HB_BEAMR_LIB_SUCCESS && "Runtime init failed");

    ctx = hb_beamr_lib_create_context();
    assert(ctx != NULL && "Context creation failed");

    const char* aot_filename = "./import_test_module.aot";
    uint32_t aot_file_size = 0;
    aot_file_buffer = read_file_to_buffer(aot_filename, &aot_file_size);
    if (!aot_file_buffer) {
        fprintf(stderr, "Test FAILED: Could not read AOT file %s.\n", aot_filename);
        goto cleanup_fail;
    }
    printf("AOT file %s read, size: %u bytes.\n", aot_filename, aot_file_size);

    hb_beamr_native_symbols_structured_t import_symbols = {
        .groups = (hb_beamr_native_symbol_group_t[]){
        {
            .module_name = "env", 
            .symbols = (hb_beamr_native_symbol_t[]){
                {
                    .function_name = "host_add_one", 
                    .user_function = (void*)generic_dispatcher_for_add_one, // Register generic dispatcher
                    .signature = "(i)i", // WAMR uses this to prepare args_raw for the dispatcher
                    .attachment = NULL // No attachment needed for this simple test dispatcher
                }
            },
            .num_symbols = 1
        }
        },
        .num_groups = 1
    };

    rc = hb_beamr_lib_register_global_natives(&import_symbols);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_register_global_natives failed: %d\n", rc);
        goto cleanup_fail; 
    }
    printf("Global natives registered for 'env'.\n");

    rc = hb_beamr_lib_load_aot_module(ctx, aot_file_buffer, aot_file_size);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_load_aot_module failed: %s\n", hb_beamr_lib_get_last_error(ctx));
        goto cleanup_fail;
    }
    printf("Module loaded: %s\n", hb_beamr_lib_get_last_error(ctx));

    rc = hb_beamr_lib_instantiate(ctx, 128 * 1024, 0, NULL);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_instantiate failed: %s\n", hb_beamr_lib_get_last_error(ctx));
        goto cleanup_fail;
    }
    printf("Module instantiated: %s\n", hb_beamr_lib_get_last_error(ctx));

    const char* func_name = "wasm_add_two_via_host";
    wasm_val_t args[1];
    args[0].kind = WASM_I32;
    args[0].of.i32 = 10; 

    wasm_val_t results[1]; 

    rc = hb_beamr_lib_call_export(ctx, func_name, 1, args, 1, results);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_call_export for '%s' failed: %s\n", func_name, hb_beamr_lib_get_last_error(ctx));
        goto cleanup_fail;
    }

    printf("Function '%s' called successfully.\n", func_name);
    assert(results[0].kind == WASM_I32 && "Result kind is not I32");
    int32_t final_result = results[0].of.i32;
    printf("wasm_add_two_via_host(10) result: %d\n", final_result);
    assert(final_result == 12 && "Result should be 12");

    printf("Test PASSED: Import Call Test (generic dispatcher, explicit Wasm signature)\n");
    test_succeeded = true; // Set to true only if all steps complete successfully

cleanup_fail:
    if (aot_file_buffer) free(aot_file_buffer);
    if (ctx) hb_beamr_lib_destroy_context(ctx);
    hb_beamr_lib_destroy_runtime_global();
    return test_succeeded ? 0 : 1;
} 