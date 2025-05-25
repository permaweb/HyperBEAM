#include "hb_beamr_lib.h"
#include <stdio.h>
#include <stdlib.h> // For fopen, fread, fclose, NULL, malloc, free
#include <string.h>
#include <assert.h>

// Re-use read_file_to_buffer from load_aot_module_test.c (ideally in a shared test_utils.c)
uint8_t* read_file_to_buffer(const char* filename, uint32_t* out_size) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "ERROR: Cannot open file %s\n", filename);
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    if (size < 0) { fclose(file); return NULL; }
    *out_size = (uint32_t)size;
    uint8_t* buffer = (uint8_t*)malloc(*out_size);
    if (!buffer) { fclose(file); return NULL; }
    size_t read_size = fread(buffer, 1, *out_size, file);
    fclose(file);
    if (read_size != *out_size) { free(buffer); return NULL; }
    return buffer;
}

int main() {
    printf("Running test: Instantiate and Call Export Test\n");

    hb_beamr_lib_rc_t rc = hb_beamr_lib_init_runtime_global(NULL);
    assert(rc == HB_BEAMR_LIB_SUCCESS && "Runtime init failed");

    hb_beamr_lib_context_t* ctx = hb_beamr_lib_create_context();
    assert(ctx != NULL && "Context creation failed");

    const char* aot_filename = "./basic_fib.aot";
    uint32_t aot_file_size = 0;
    uint8_t* aot_file_buffer = read_file_to_buffer(aot_filename, &aot_file_size);
    assert(aot_file_buffer != NULL && "AOT file read failed");
    printf("AOT file %s read successfully, size: %u bytes.\n", aot_filename, aot_file_size);

    rc = hb_beamr_lib_load_aot_module(ctx, aot_file_buffer, aot_file_size);
    assert(rc == HB_BEAMR_LIB_SUCCESS && "Module load failed");
    printf("Module loaded: %s\n", hb_beamr_lib_get_last_error(ctx));

    // Instantiate with default stack and heap. No symbols needed for basic_fib.
    rc = hb_beamr_lib_instantiate(ctx, 128 * 1024, 0);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_instantiate failed. Error: %s\n", hb_beamr_lib_get_last_error(ctx));
        free(aot_file_buffer);
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("Module instantiated: %s\n", hb_beamr_lib_get_last_error(ctx));

    // Call the exported "fib" function: fib(10)
    const char* func_name = "fib";
    wasm_val_t args[1];
    args[0].kind = WASM_I32;
    args[0].of.i32 = 10;

    wasm_val_t results[1]; // fib returns one i32 value

    rc = hb_beamr_lib_call_export(ctx, func_name, 1, args, 1, results);

    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_call_export for '%s' failed. Error: %s\n", func_name, hb_beamr_lib_get_last_error(ctx));
        free(aot_file_buffer);
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }

    printf("Function '%s' called successfully.\n", func_name);
    assert(results[0].kind == WASM_I32 && "Result kind is not I32");
    int32_t fib_result = results[0].of.i32;
    printf("fib(10) result: %d\n", fib_result);
    assert(fib_result == 55 && "fib(10) did not return 55");

    free(aot_file_buffer);
    hb_beamr_lib_destroy_context(ctx);
    hb_beamr_lib_destroy_runtime_global();

    printf("Test PASSED: Instantiate and Call Export Test\n");
    return 0;
} 