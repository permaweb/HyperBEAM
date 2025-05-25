#include "hb_beamr_capi_lib.h"
#include "wasm_c_api.h" // For wasm_val_t, wasm_valkind_enum
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>


// Helper to read a file into a buffer (same as in capi_load_wasm_module_test.c)
static uint8_t* read_file_to_buffer(const char* filename, uint32_t* ret_size) {
    FILE* file = fopen(filename, "rb");
    if (!file) { fprintf(stderr, "fopen failed for %s\n", filename); return NULL; }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    if (size < 0) { fclose(file); return NULL; }
    uint8_t* buffer = (uint8_t*)malloc(size);
    if (!buffer) { fclose(file); return NULL; }
    if (fread(buffer, 1, size, file) != (size_t)size) { free(buffer); fclose(file); return NULL; }
    fclose(file);
    *ret_size = (uint32_t)size;
    return buffer;
}

int main(int argc, char* argv[]) {
    printf("Starting C-API Instantiate and Call Export Test...\n");
    hb_beamr_capi_lib_rc_t rc;

    wasm_config_t* config = wasm_config_new();
    assert(config != NULL);
    rc = hb_beamr_capi_lib_init_runtime_global(config);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("Runtime init failed: %d\n", rc);
        if(config) wasm_config_delete(config);
        return 1;
    }

    hb_beamr_capi_lib_context_t* ctx = hb_beamr_capi_lib_create_context();
    assert(ctx != NULL);

    uint32_t wasm_size;
    uint8_t* wasm_buf = read_file_to_buffer("./basic_fib.wasm", &wasm_size);
    assert(wasm_buf != NULL && wasm_size > 0);

    rc = hb_beamr_capi_lib_load_wasm_module(ctx, wasm_buf, wasm_size);
    assert(rc == HB_BEAMR_CAPI_LIB_SUCCESS);
    printf("WASM module loaded. Last error: %s\n", hb_beamr_capi_lib_get_last_error(ctx));
    free(wasm_buf);

    printf("Instantiating module...\n");
    // Provide dummy WASI implementations as override_symbols for basic_fib.wasm

    rc = hb_beamr_capi_lib_instantiate(ctx, 
                                       NULL, // No default import function
                                       NULL, 
                                       0);
    assert(rc == HB_BEAMR_CAPI_LIB_SUCCESS);
    printf("Module instantiated. Last error: %s\n", hb_beamr_capi_lib_get_last_error(ctx));

    printf("Calling exported function 'fib' with input 10...\n");
    wasm_val_t args[1];
    args[0].kind = WASM_I32;
    args[0].of.i32 = 10;

    wasm_val_t results[1];

    rc = hb_beamr_capi_lib_call_export(ctx, "fib", 1, args, 1, results);
    printf("call_export rc: %d, Last error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
    assert(rc == HB_BEAMR_CAPI_LIB_SUCCESS);

    assert(results[0].kind == WASM_I32);
    printf("Fib(10) result: %d (expected 55)\n", results[0].of.i32);
    assert(results[0].of.i32 == 55);

    hb_beamr_capi_lib_destroy_context(ctx);
    hb_beamr_capi_lib_destroy_runtime_global();

    printf("\nC-API Instantiate and Call Export Test PASSED.\n");
    return 0;
} 