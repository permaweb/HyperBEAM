#include "hb_beamr_capi_lib.h"
#include "wasm_c_api.h" // For wasm_val_t, etc.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "utils.h" // Use the shared utility

// Host function implementations
static int global_host_value = 100;

// Host function: env.host_add_one (i) -> i
wasm_trap_t* host_add_one_impl(void* env_param, const wasm_val_vec_t* args, wasm_val_vec_t* results) {
    hb_beamr_capi_lib_context_t* instance_ctx = (hb_beamr_capi_lib_context_t*)env_param;
    wasm_store_t* store = hb_beamr_capi_lib_context_get_store(instance_ctx);

    if (!store) {
        fprintf(stderr, "[host_add_one_impl] CRITICAL: Failed to get store from instance_ctx.\n");
        return NULL; 
    }

    if (args->num_elems != 1 || args->data[0].kind != WASM_I32 || results->size < 1) {
        fprintf(stderr, "[host_add_one_impl] Signature mismatch!\n");
        wasm_name_t trap_msg;
        wasm_name_new_from_string_nt(&trap_msg, "trap: host_add_one_impl signature mismatch");
        wasm_trap_t* trap = wasm_trap_new(store, (const wasm_message_t*)&trap_msg); 
        wasm_name_delete(&trap_msg); 
        return trap;
    }
    int input_val = args->data[0].of.i32;
    global_host_value += input_val; 
    
    results->data[0].kind = WASM_I32;
    results->data[0].of.i32 = global_host_value; 
    results->num_elems = 1;
    return NULL; 
}

int main(int argc, char* argv[]) {
    printf("C-API Import Call Test Started...\n"); 

    hb_beamr_capi_lib_rc_t rc;

    wasm_config_t* config = wasm_config_new(); 
    assert(config != NULL);
    rc = hb_beamr_capi_lib_init_runtime_global(config);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("Runtime init failed: %d\n", rc); 
        if (config) wasm_config_delete(config);
        return 1;
    }

    hb_beamr_capi_lib_context_t* ctx = hb_beamr_capi_lib_create_context();
    assert(ctx != NULL);

    uint32_t wasm_size;
    // Changed to .wasm to match CMakeLists.txt copy operation, was .aot
    uint8_t* wasm_buf = read_file_to_buffer("./import_test_module_capi.aot", &wasm_size);
    assert(wasm_buf != NULL && wasm_size > 0);

    rc = hb_beamr_capi_lib_load_wasm_module(ctx, wasm_buf, wasm_size);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("Module load failed: rc=%d, error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
        free_buffer(wasm_buf);
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }
    free_buffer(wasm_buf);

    hb_beamr_capi_native_symbol_t override_symbols[] = {
        { "env", "host_add_one", (void*)host_add_one_impl, "(i)i" } 
    };

    rc = hb_beamr_capi_lib_instantiate(ctx, 
                                       NULL, 
                                       override_symbols, 
                                       sizeof(override_symbols)/sizeof(override_symbols[0]));
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("Instantiation failed: rc=%d, error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }

    global_host_value = 100; 
    wasm_val_t args_call[1]; args_call[0].kind = WASM_I32; args_call[0].of.i32 = 10;
    wasm_val_t results_call[1];
    rc = hb_beamr_capi_lib_call_export(ctx, "wasm_add_two_via_host", 1, args_call, 1, results_call);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("Call export failed: rc=%d, error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1; 
    }
    
    printf("Call 'wasm_add_two_via_host(10)' succeeded. Result: %d\n", results_call[0].of.i32); 
    assert(results_call[0].kind == WASM_I32);
    assert(results_call[0].of.i32 == 220); 
    assert(global_host_value == 220);

    hb_beamr_capi_lib_destroy_context(ctx);
    hb_beamr_capi_lib_destroy_runtime_global(); 

    printf("C-API Import Call Test PASSED.\n");
    return 0;
} 