#include "hb_beamr_lib.h"
#include <stdio.h>
#include <stdlib.h> // For fopen, fread, fclose, NULL
#include <string.h>
#include <assert.h>

// Helper to read a binary file into a buffer
// Caller is responsible for freeing the returned buffer.
uint8_t* read_file_to_buffer(const char* filename, uint32_t* out_size) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "ERROR: Cannot open file %s\n", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (size < 0) {
        fprintf(stderr, "ERROR: File size is invalid for %s\n", filename);
        fclose(file);
        return NULL;
    }
    *out_size = (uint32_t)size;

    uint8_t* buffer = (uint8_t*)malloc(*out_size);
    if (!buffer) {
        fprintf(stderr, "ERROR: Malloc failed for reading file %s\n", filename);
        fclose(file);
        return NULL;
    }

    size_t read_size = fread(buffer, 1, *out_size, file);
    fclose(file);

    if (read_size != *out_size) {
        fprintf(stderr, "ERROR: Read size mismatch for file %s (expected %u, got %zu)\n", filename, *out_size, read_size);
        free(buffer);
        return NULL;
    }
    return buffer;
}

// Dummy host function for testing import registration
// This function signature must match what Wasm expects, including exec_env.
// For a simple fib(i32) -> i32, an import might be (env, host_fib, (i)i)
// static void host_fib_impl(wasm_exec_env_t exec_env, int32_t n, int32_t* result) {
//    *result = n * 2; // dummy implementation
// }

int main() {
    printf("Running test: Load AOT Module Test\n");

    hb_beamr_lib_rc_t rc_rt = hb_beamr_lib_init_runtime_global(NULL);
    assert(rc_rt == HB_BEAMR_LIB_SUCCESS && "Runtime init failed");

    hb_beamr_lib_context_t* ctx = hb_beamr_lib_create_context();
    assert(ctx != NULL && "Context creation failed");

    const char* aot_filename = "./basic_fib.aot"; // Copied by CMake into build dir
    uint32_t aot_file_size = 0;
    uint8_t* aot_file_buffer = read_file_to_buffer(aot_filename, &aot_file_size);

    if (!aot_file_buffer) {
        fprintf(stderr, "Test FAILED: Could not read AOT file %s. Ensure it was compiled and copied.\n", aot_filename);
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("AOT file %s read successfully, size: %u bytes.\n", aot_filename, aot_file_size);

    // Test loading without imports first
    hb_beamr_lib_rc_t load_rc = hb_beamr_lib_load_aot_module(ctx, aot_file_buffer, aot_file_size);
    
    if (load_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_load_aot_module (no imports) failed. Error: %s\n", hb_beamr_lib_get_last_error(ctx));
        free(aot_file_buffer);
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("hb_beamr_lib_load_aot_module (no imports) successful. Last message: %s\n", hb_beamr_lib_get_last_error(ctx));

    // Module is loaded. Destroy context should unload it.
    hb_beamr_lib_destroy_context(ctx);
    ctx = NULL; // Avoid use after free

    // Re-create context for next test with imports (if we add one)
    // For now, just test loading again to ensure context is clean.
    ctx = hb_beamr_lib_create_context();
    assert(ctx != NULL && "Second context creation failed");

    // Test loading again (make sure no residue from previous load in global state or uncleaned context)
    // This time, let's imagine we have a host function to register.
    // hb_beamr_native_symbol_t import_symbols[] = {
    //     { "env", "host_double", (void*)host_fib_impl, "(i)i", NULL }
    // };
    // uint32_t num_imports = sizeof(import_symbols) / sizeof(hb_beamr_native_symbol_t);
    // load_rc = hb_beamr_lib_load_aot_module(ctx, aot_file_buffer, aot_file_size, import_symbols, num_imports);

    // For now, just repeat the no-imports load on a fresh context
    load_rc = hb_beamr_lib_load_aot_module(ctx, aot_file_buffer, aot_file_size);
    if (load_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_load_aot_module (second load, no imports) failed. Error: %s\n", hb_beamr_lib_get_last_error(ctx));
        free(aot_file_buffer);
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("hb_beamr_lib_load_aot_module (second load, no imports) successful. Last message: %s\n", hb_beamr_lib_get_last_error(ctx));


    free(aot_file_buffer);
    hb_beamr_lib_destroy_context(ctx);
    hb_beamr_lib_destroy_runtime_global();

    printf("Test PASSED: Load AOT Module Test\n");
    return 0;
}
