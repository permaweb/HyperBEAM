#include "hb_beamr_capi_lib.h"
#include "wasm_c_api.h" // For wasm_val_t, etc.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Host function implementations
static int global_host_value = 100;

// Import: env.host_increment(i32) -> i32
wasm_trap_t* host_increment_impl(void* env_store, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    wasm_store_t* store = (wasm_store_t*)env_store;
    if (args->num_elems != 1 || args->data[0].kind != WASM_I32 || results->size < 1) {
        wasm_name_t trap_msg;
        wasm_name_new_from_string_nt(&trap_msg, "trap: host_increment signature mismatch");
        wasm_trap_t* trap = wasm_trap_new(store, (const wasm_message_t*)&trap_msg); 
        wasm_name_delete(&trap_msg); // trap_msg (the vec shell and its data) can be deleted after trap_new
        return trap;
    }
    global_host_value += args->data[0].of.i32;
    results->data[0].kind = WASM_I32;
    results->data[0].of.i32 = global_host_value;
    results->num_elems = 1; // Indicate one result is produced
    return NULL; // No trap
}

// Import: env.host_log(i32, i32)
wasm_trap_t* host_log_impl(void* env_store, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    wasm_store_t* store = (wasm_store_t*)env_store;
    if (args->num_elems != 2 || args->data[0].kind != WASM_I32 || args->data[1].kind != WASM_I32) {
        wasm_name_t trap_msg;
        wasm_name_new_from_string_nt(&trap_msg, "trap: host_log signature mismatch");
        wasm_trap_t* trap = wasm_trap_new(store, (const wasm_message_t*)&trap_msg);
        wasm_name_delete(&trap_msg); 
        return trap;
    }
    printf("[host_log_impl] Log from Wasm: %d, %d\n", args->data[0].of.i32, args->data[1].of.i32);
    // No results for host_log (void)
    results->num_elems = 0;
    return NULL; // No trap
}

// Host function: env.host_add_one (i) -> i
wasm_trap_t* host_add_one_impl(void* env_param, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    wasm_store_t* store = (wasm_store_t*)env_param;
    printf("[host_add_one_impl] Entered. env_param (store): %p\n", (void*)store);
    fflush(stdout); fflush(stderr);

    if (!store) {
        fprintf(stderr, "[host_add_one_impl] CRITICAL: store is NULL.\n");
        return NULL; 
    }

    if (args->num_elems != 1 || args->data[0].kind != WASM_I32 || results->size < 1) {
        fprintf(stderr, "[host_add_one_impl] Signature mismatch!\n");
        fflush(stdout); fflush(stderr);
        wasm_name_t trap_msg;
        wasm_name_new_from_string_nt(&trap_msg, "trap: host_add_one_impl signature mismatch");
        wasm_trap_t* trap = wasm_trap_new(store, (const wasm_message_t*)&trap_msg);
        wasm_name_delete(&trap_msg); 
        return trap;
    }
    int input_val = args->data[0].of.i32;
    global_host_value += input_val; 
    
    printf("[host_add_one_impl] Input: %d, global_host_value now: %d\n", 
           input_val, global_host_value);
    fflush(stdout); fflush(stderr);
    results->data[0].kind = WASM_I32;
    results->data[0].of.i32 = global_host_value; 
    results->num_elems = 1;
    printf("[host_add_one_impl] Returning %d. Trap: NULL\n", results->data[0].of.i32);
    fflush(stdout); fflush(stderr);
    return NULL; 
}

// Helper to read a file into a buffer
static uint8_t* read_file_to_buffer(const char* filename, uint32_t* ret_size) {
    FILE* file = fopen(filename, "rb");
    if (!file) { 
        fprintf(stderr, "fopen failed for %s\n", filename); 
        fflush(stderr);
        return NULL; 
    }
    fseek(file, 0, SEEK_END); long size = ftell(file); fseek(file, 0, SEEK_SET);
    if (size < 0) { fclose(file); return NULL; }
    uint8_t* buffer = (uint8_t*)malloc(size);
    if (!buffer) { fclose(file); return NULL; }
    if (fread(buffer, 1, size, file) != (size_t)size) { free(buffer); fclose(file); return NULL; }
    fclose(file); *ret_size = (uint32_t)size; return buffer;
}

int main(int argc, char* argv[]) {
    printf("DEBUG: Main Start\n"); fflush(stdout); fflush(stderr);

    hb_beamr_capi_lib_rc_t rc;

    wasm_config_t* config = wasm_config_new(); 
    printf("DEBUG: Config created\n"); fflush(stdout); fflush(stderr);
    assert(config != NULL);
    rc = hb_beamr_capi_lib_init_runtime_global(config);
    printf("DEBUG: Runtime init called, rc=%d\n", rc); fflush(stdout); fflush(stderr);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("Runtime init failed: %d\n", rc); fflush(stdout); fflush(stderr);
        if (config) wasm_config_delete(config); // Config is consumed on success by init_runtime_global
        return 1;
    }

    hb_beamr_capi_lib_context_t* ctx = hb_beamr_capi_lib_create_context();
    printf("DEBUG: Context created %p\n", (void*)ctx); fflush(stdout); fflush(stderr);
    assert(ctx != NULL);

    uint32_t wasm_size;
    uint8_t* wasm_buf = read_file_to_buffer("./import_test_module.aot", &wasm_size);
    printf("DEBUG: Wasm file read, buf %p, size %u\n", (void*)wasm_buf, wasm_size); fflush(stdout); fflush(stderr);
    assert(wasm_buf != NULL && wasm_size > 0);

    rc = hb_beamr_capi_lib_load_wasm_module(ctx, wasm_buf, wasm_size);
    printf("DEBUG: Module loaded, rc=%d, last_error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx)); fflush(stdout); fflush(stderr);
    assert(rc == HB_BEAMR_CAPI_LIB_SUCCESS);
    free(wasm_buf);

    hb_beamr_capi_native_symbol_t override_symbols[] = {
        { "env", "host_add_one", (void*)host_add_one_impl, "(i)i" } 
    };

    printf("DEBUG: Instantiating module...\n"); fflush(stdout); fflush(stderr);
    rc = hb_beamr_capi_lib_instantiate(ctx, 
                                       NULL, // No default import function for this test
                                       override_symbols, 
                                       sizeof(override_symbols)/sizeof(override_symbols[0]));
    printf("DEBUG: Instantiation done, rc=%d, last_error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx)); fflush(stdout); fflush(stderr);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }

    global_host_value = 100; 
    printf("DEBUG: Calling export 'wasm_add_two_via_host'...\n"); fflush(stdout); fflush(stderr);
    wasm_val_t args_call[1]; args_call[0].kind = WASM_I32; args_call[0].of.i32 = 10;
    wasm_val_t results_call[1];
    rc = hb_beamr_capi_lib_call_export(ctx, "wasm_add_two_via_host", 1, args_call, 1, results_call);
    printf("DEBUG: Call export done, rc=%d, last_error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx)); fflush(stdout); fflush(stderr);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1; 
    }
    
    printf("DEBUG: Result: %d\n", results_call[0].of.i32); fflush(stdout); fflush(stderr);
    assert(results_call[0].kind == WASM_I32);
    assert(results_call[0].of.i32 == 220); 
    assert(global_host_value == 220);

    printf("DEBUG: Destroying context...\n"); fflush(stdout); fflush(stderr);
    hb_beamr_capi_lib_destroy_context(ctx);
    printf("DEBUG: Destroying runtime...\n"); fflush(stdout); fflush(stderr);
    hb_beamr_capi_lib_destroy_runtime_global(); 

    printf("DEBUG: Test PASSED criteria met.\n"); fflush(stdout); fflush(stderr);
    return 0;
} 