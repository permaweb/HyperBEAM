#include "hb_beamr_capi_lib.h"
#include "wasm_c_api.h" // For wasm_val_t, wasm_valkind_enum
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Dummy WASI function implementations
static wasm_trap_t* wasi_dummy_generic_ii_i(void* env_store, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    printf("[WASI DUMMY] '%s' called (params: %zu, results: %zu)\n", __func__, args->size, results->size);
    if (results->size > 0) { // For functions like args_sizes_get that return an errno
        results->data[0].kind = WASM_I32;
        results->data[0].of.i32 = 0; // __WASI_ERRNO_SUCCESS
        results->num_elems = 1;
    }
    return NULL;
}

static wasm_trap_t* wasi_dummy_generic_i_v(void* env_store, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    printf("[WASI DUMMY] '%s' called (proc_exit with %d)\n", __func__, args->data[0].of.i32);
    results->num_elems = 0;
    // To truly exit or trap: 
    // wasm_name_t msg; wasm_name_new_from_string_nt(&msg, "wasi proc_exit called"); 
    // wasm_trap_t* trap = wasm_trap_new((wasm_store_t*)env_store, (const wasm_message_t*)&msg); 
    // wasm_name_delete(&msg); return trap;
    return NULL; // For now, just print and continue for test purposes
}

static wasm_trap_t* wasi_dummy_generic_iiii_i(void* env_store, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    printf("[WASI DUMMY] '%s' called (fd_write with fd=%d)\n", __func__, args->data[0].of.i32);
    if (results->size > 0) { // fd_write returns errno
        results->data[0].kind = WASM_I32;
        results->data[0].of.i32 = 0; // __WASI_ERRNO_SUCCESS
        results->num_elems = 1;
    }
    return NULL;
}

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

    printf("Instantiating module with dummy WASI imports...\n");
    hb_beamr_capi_native_symbol_t native_symbols[] = {
        { "wasi_snapshot_preview1", "args_sizes_get", (void*)wasi_dummy_generic_ii_i, "(ii)i" },
        { "wasi_snapshot_preview1", "args_get",       (void*)wasi_dummy_generic_ii_i, "(ii)i" }, // Same dummy for simplicity
        { "wasi_snapshot_preview1", "proc_exit",      (void*)wasi_dummy_generic_i_v,  "(i)v" },
        { "wasi_snapshot_preview1", "fd_write",       (void*)wasi_dummy_generic_iiii_i, "(iiii)i" } // Corrected signature
    };

    rc = hb_beamr_capi_lib_instantiate(ctx, native_symbols, sizeof(native_symbols)/sizeof(native_symbols[0]) ); 
    printf("Instantiation rc: %d, Last error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
    assert(rc == HB_BEAMR_CAPI_LIB_SUCCESS);

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