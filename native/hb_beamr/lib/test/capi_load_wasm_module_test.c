#include "hb_beamr_capi_lib.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Helper to read a file into a buffer
static uint8_t* read_file_to_buffer(const char* filename, uint32_t* ret_size) {
    FILE* file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);
    if (size < 0) {
        fprintf(stderr, "Failed to get file size: %s\n", filename);
        fclose(file);
        return NULL;
    }
    uint8_t* buffer = (uint8_t*)malloc(size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate buffer for file: %s\n", filename);
        fclose(file);
        return NULL;
    }
    size_t read_size = fread(buffer, 1, size, file);
    fclose(file);
    if (read_size != (size_t)size) {
        fprintf(stderr, "Failed to read entire file: %s\n", filename);
        free(buffer);
        return NULL;
    }
    *ret_size = (uint32_t)size;
    return buffer;
}

int main(int argc, char* argv[]) {
    printf("Starting C-API Load WASM Module Test...\n");

    hb_beamr_capi_lib_rc_t rc;
    wasm_config_t* config = wasm_config_new();
    assert(config != NULL);

    rc = hb_beamr_capi_lib_init_runtime_global(config);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("Failed to initialize runtime: %d\n", rc);
        wasm_config_delete(config);
        return 1;
    }
    // config is consumed by init_runtime_global on success

    hb_beamr_capi_lib_context_t* ctx = hb_beamr_capi_lib_create_context();
    assert(ctx != NULL);

    const char* wasm_file = "./basic_fib.wasm";
    uint32_t wasm_buffer_size = 0;
    uint8_t* wasm_buffer = read_file_to_buffer(wasm_file, &wasm_buffer_size);
    assert(wasm_buffer != NULL && wasm_buffer_size > 0);

    printf("Loading WASM module: %s (%u bytes)...\n", wasm_file, wasm_buffer_size);
    rc = hb_beamr_capi_lib_load_wasm_module(ctx, wasm_buffer, wasm_buffer_size);
    printf("hb_beamr_capi_lib_load_wasm_module returned: %d\n", rc);
    const char* error_msg = hb_beamr_capi_lib_get_last_error(ctx);
    printf("Last error: %s\n", error_msg);
    assert(rc == HB_BEAMR_CAPI_LIB_SUCCESS);

    printf("Module loaded. Destroying context...\n");
    hb_beamr_capi_lib_destroy_context(ctx);
    printf("Context destroyed.\n");

    hb_beamr_capi_lib_destroy_runtime_global();
    printf("Runtime destroyed.\n");

    free(wasm_buffer);
    printf("\nC-API Load WASM Module Test PASSED.\n");
    return 0;
} 