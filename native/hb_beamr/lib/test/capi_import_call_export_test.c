// native/hb_beamr/lib/test/capi_import_call_export_test.c
#include "wasm_c_api.h" // Include the Wasm C API header for wasm_instance_memory etc.
#include "hb_beamr_capi_lib.h"
#include "utils.h" // For read_file_to_buffer and other test utils
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// int my_index = 3; // This global is no longer the primary index for the host-to-Wasm xor_memory call in this flow.

// --- Host Function Implementation ---
// This function will be called by Wasm. Its signature must match what Wasm expects,
// and it will be wrapped by the generic_trampoline_callback.

// This is the actual C function the user (port driver) would provide.
// The `env` here will be the `hb_beamr_capi_lib_context_t*` passed to `HostFuncEnv`.
static wasm_trap_t* my_give_host_control_impl(
    void* user_env, // This is hb_beamr_capi_lib_context_t*
    const wasm_val_vec_t* args, 
    wasm_val_vec_t* results
) {
    hb_beamr_capi_lib_context_t* ctx = (hb_beamr_capi_lib_context_t*)user_env;
    fprintf(stderr, "[Host DEBUG] my_give_host_control_impl entered.\n");

    // give_host_control(unsigned int current_index) in Wasm (import_nested.c) now takes 1 I32 argument.
    if (args->size != 1 || args->data[0].kind != WASM_I32) {
        fprintf(stderr, "[Host DEBUG] ERROR: my_give_host_control_impl called with wrong args. Expected 1 (I32), Got %zu. Arg 0 kind (if any): %d\n", 
                args->size, (args->size > 0 ? args->data[0].kind : -1));
        // ... (trap generation code, if desired) ...
        results->num_elems = 0;
        return NULL; 
    }

    uint32_t index_from_wasm = args->data[0].of.i32;
    fprintf(stderr, "[Host DEBUG] give_host_control() received index %u from Wasm.\n", index_from_wasm);
    fprintf(stderr, "[Host DEBUG] Now, host (my_give_host_control_impl) will call Wasm export 'xor_memory' on this index.\n");

    uint32_t xor_val_from_host = 0xABCDEFFF; 
    fprintf(stderr, "[Host DEBUG] Calling Wasm's xor_memory with index %u and xor_val 0x%X\n", index_from_wasm, xor_val_from_host);

    wasm_val_t xor_call_args[2];
    xor_call_args[0].kind = WASM_I32;
    xor_call_args[0].of.i32 = index_from_wasm;      // Arg 1: index for xor_memory (received from Wasm)
    xor_call_args[1].kind = WASM_I32;
    xor_call_args[1].of.i32 = xor_val_from_host;    // Arg 2: value to XOR with

    hb_beamr_capi_lib_rc_t rc_xor = hb_beamr_capi_lib_call_export(ctx, "xor_memory", 2, xor_call_args, 0, NULL);

    if (rc_xor != HB_BEAMR_CAPI_LIB_SUCCESS) {
        fprintf(stderr, "[Host DEBUG] ERROR: Call from host to Wasm export 'xor_memory' failed: %d. Error: %s\n", rc_xor, hb_beamr_capi_lib_get_last_error(ctx));
    } else {
        fprintf(stderr, "[Host DEBUG] Successfully called Wasm export 'xor_memory' from host on index %u.\n", index_from_wasm);
    }

    results->num_elems = 0; // give_host_control() is void
    return NULL; 
}

int main() {
    printf("--- Test: C-API Import, Call Export, Host Modifies Memory ---\n");

    hb_beamr_capi_lib_rc_t rc;
    wasm_config_t* config = wasm_config_new();
    // Configure engine if needed, e.g. wasm_config_set_engine(...)
    rc = hb_beamr_capi_lib_init_runtime_global(config); // config is consumed or NULL
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        fprintf(stderr, "Failed to init runtime: %d\n", rc);
        return 1;
    }

    hb_beamr_capi_lib_context_t* ctx = hb_beamr_capi_lib_create_context();
    if (!ctx) {
        fprintf(stderr, "Failed to create context\n");
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }

    const char* wasm_file_path = "./import_nested.aot"; // Relative to where test runs
    uint32_t wasm_size = 0;
    uint8_t* wasm_binary = read_file_to_buffer(wasm_file_path, &wasm_size);
    if (!wasm_binary) {
        fprintf(stderr, "Failed to read WASM file: %s\n", wasm_file_path);
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }

    rc = hb_beamr_capi_lib_load_wasm_module(ctx, wasm_binary, wasm_size);
    free(wasm_binary); // Buffer is copied by the library or no longer needed
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        fprintf(stderr, "Failed to load WASM module: %d. Error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }
    printf("WASM module loaded successfully.\n");

    // Define the native symbol for the import
    hb_beamr_capi_native_symbol_t import_symbols[] = {
        {
            .import_module_name = "env",
            .import_function_name = "give_host_control",
            .user_function = (void*)my_give_host_control_impl
            // Signature is derived by the library from the Wasm module's import section
        }
    };

    // Instantiate with the host function
    rc = hb_beamr_capi_lib_instantiate(ctx, NULL /*default_import_function*/, import_symbols, 1);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        fprintf(stderr, "Failed to instantiate module: %d. Error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }
    printf("Module instantiated successfully.\n");

    // 1. Get the pointer to the Wasm global data buffer
    wasm_val_t get_ptr_results[1];
    rc = hb_beamr_capi_lib_call_export(ctx, "get_data_ptr", 0, NULL, 1, get_ptr_results);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        fprintf(stderr, "Failed to call 'get_data_ptr': %d. Error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }
    if (get_ptr_results[0].kind != WASM_I32) {
        fprintf(stderr, "'get_data_ptr' did not return I32\n");
        return 1;
    }
    uint32_t data_ptr_offset = get_ptr_results[0].of.i32;
    printf("'get_data_ptr' returned offset: %u\n", data_ptr_offset);

    // Define the index to use for operations within global_data_buffer by main
    uint32_t target_index_main = 0; 
    // Define the initial value to be set by Wasm's call_host_and_read
    uint32_t initial_value_for_wasm = 77; 
    printf("Using index %u and initial_value %u for Wasm's call_host_and_read.\n", target_index_main, initial_value_for_wasm);

    // 2. Call `call_host_and_read` which internally calls the host function
    // Wasm's `call_host_and_read` now expects (index, init_val)
    wasm_val_t call_args[2]; // Two arguments
    call_args[0].kind = WASM_I32;
    call_args[0].of.i32 = target_index_main;      // Arg 1: index
    call_args[1].kind = WASM_I32;
    call_args[1].of.i32 = initial_value_for_wasm; // Arg 2: init_val

    wasm_val_t call_results[1];
    // Update argument count to 2 for hb_beamr_capi_lib_call_export
    rc = hb_beamr_capi_lib_call_export(ctx, "call_host_and_read", 2, call_args, 1, call_results);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        fprintf(stderr, "Failed to call 'call_host_and_read': %d. Error: %s\n", rc, hb_beamr_capi_lib_get_last_error(ctx));
        hb_beamr_capi_lib_destroy_context(ctx);
        hb_beamr_capi_lib_destroy_runtime_global();
        return 1;
    }

    if (call_results[0].kind != WASM_I32) {
        fprintf(stderr, "'call_host_and_read' did not return I32\n");
        return 1;
    }
    uint32_t result_val = call_results[0].of.i32;
    printf("'call_host_and_read' returned: %u\n", result_val);

    // Assertion:
    // - call_host_and_read(target_index_main, initial_value_for_wasm) is called.
    // - Wasm sets global_data_buffer[target_index_main] = initial_value_for_wasm.
    // - Wasm calls give_host_control().
    // - Host's my_give_host_control_impl calls Wasm's xor_memory(index_from_wasm, xor_val_from_host).
    //   (index_from_wasm is received from Wasm, xor_val_from_host is 0xABCDEFFF).
    //   This modifies global_data_buffer[index_from_wasm].
    // - If target_index_main (e.g., 0) is different from index_from_wasm (received from Wasm), then global_data_buffer[target_index_main]
    //   is still initial_value_for_wasm.
    // - call_host_and_read returns global_data_buffer[target_index_main].
    uint32_t expected_result_val = initial_value_for_wasm ^ 0xABCDEFFF; // Host XORs the value at target_index_main
    assert(result_val == expected_result_val); 
    printf("SUCCESS: 'call_host_and_read' returned the expected value (%u) after host interaction which modified the same index.\n", expected_result_val);

    hb_beamr_capi_lib_destroy_context(ctx);
    hb_beamr_capi_lib_destroy_runtime_global();
    printf("--- Test Finished Successfully ---\n");
    return 0;
}
