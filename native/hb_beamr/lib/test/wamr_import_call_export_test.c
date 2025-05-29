#include "hb_beamr_lib.h" 
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "wasm_export.h" 

#define assert_rc_wice(rc, expected_rc, ctx, msg) \
    if (rc != expected_rc) { \
        fprintf(stderr, "Assertion failed (%s): Expected %d, got %d. Error: %s\n", \
                msg, expected_rc, rc, ctx ? hb_beamr_lib_get_last_error(ctx) : "NULL context"); \
        assert(rc == expected_rc); \
    }

void my_give_host_control_impl_wamr(wasm_exec_env_t exec_env, uint64_t *args_wamr) {
    wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
    hb_beamr_lib_context_t* ctx = (hb_beamr_lib_context_t*)wasm_runtime_get_custom_data(module_inst);

    if (!ctx) {
        fprintf(stderr, "[Host WAMR DEBUG] my_give_host_control_impl_wamr: ctx is NULL! Cannot proceed.\n");
        wasm_runtime_set_exception(module_inst, "Host context not found in give_host_control");
        return;
    }
    fprintf(stderr, "[Host WAMR DEBUG] my_give_host_control_impl_wamr entered. ctx: %p\n", (void*)ctx);

    uint32_t index_from_wasm = (uint32_t)args_wamr[0]; 
    fprintf(stderr, "[Host WAMR DEBUG] give_host_control() received index %u from Wasm.\n", index_from_wasm);
    fprintf(stderr, "[Host WAMR DEBUG] Now, host will call Wasm export 'xor_memory' on this index.\n");

    uint32_t xor_val_from_host = 0xABCDEFFF; 
    fprintf(stderr, "[Host WAMR DEBUG] Calling Wasm's xor_memory with index %u and xor_val 0x%X\n", index_from_wasm, xor_val_from_host);

    wasm_val_t xor_call_args[2]; 
    xor_call_args[0].kind = WASM_I32;
    xor_call_args[0].of.i32 = index_from_wasm;
    xor_call_args[1].kind = WASM_I32;
    xor_call_args[1].of.i32 = xor_val_from_host;

    hb_beamr_lib_rc_t rc_xor = hb_beamr_lib_call_export(ctx, "xor_memory", 2, xor_call_args, 0, NULL);

    if (rc_xor != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "[Host WAMR DEBUG] ERROR: Call from host to Wasm export 'xor_memory' failed: %d. Error: %s\n", 
                rc_xor, hb_beamr_lib_get_last_error(ctx));
        wasm_runtime_set_exception(module_inst, "Host failed to call xor_memory");
    } else {
        fprintf(stderr, "[Host WAMR DEBUG] Successfully called Wasm export 'xor_memory' from host on index %u.\n", index_from_wasm);
    }
}


int main() {
    printf("--- Test (WAMR Lib): Import, Call Export, Host Modifies Memory ---\n");

    RuntimeInitArgs init_args_wamr;
    memset(&init_args_wamr, 0, sizeof(RuntimeInitArgs));
    init_args_wamr.mem_alloc_type = Alloc_With_System_Allocator;
    
    hb_beamr_lib_rc_t rc = hb_beamr_lib_init_runtime_global(&init_args_wamr); 
    assert_rc_wice(rc, HB_BEAMR_LIB_SUCCESS, NULL, "hb_beamr_lib_init_runtime_global");

    hb_beamr_native_symbols_structured_t import_symbols = {
        .groups = (hb_beamr_native_symbol_group_t[]){
        {
            .module_name = "env", 
            .symbols = (hb_beamr_native_symbol_t[]){
                {
                    .function_name = "give_host_control",
                    .user_function = (void*)my_give_host_control_impl_wamr,
                    .signature = "(i)", 
                    .attachment = NULL 
                }
            },
            .num_symbols = 1
        }},
        .num_groups = 1
    };
    rc = hb_beamr_lib_register_global_natives(&import_symbols);
    assert_rc_wice(rc, HB_BEAMR_LIB_SUCCESS, NULL, "hb_beamr_lib_register_global_natives");
    printf("Registered host functions globally.\n");

    hb_beamr_lib_context_t* ctx = hb_beamr_lib_create_context();
    assert(ctx != NULL && "Context creation failed");

    fprintf(stderr, "[WAMRImportCallExportTest DEBUG] Explicitly loading: %s\n", "./import_nested.aot");
    uint32_t wasm_size = 0;
    uint8_t* wasm_binary = read_file_to_buffer("./import_nested.aot", &wasm_size);
    if (!wasm_binary) {
        fprintf(stderr, "Failed to read Wasm file specified in test: %s\n", "./import_nested.aot");
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }

    rc = hb_beamr_lib_load_wasm_module(ctx, wasm_binary, wasm_size); 
    free_buffer(wasm_binary); 
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Failed to load module: %d. Error: %s\n", rc, hb_beamr_lib_get_last_error(ctx));
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("WASM module loaded successfully.\n");

    rc = hb_beamr_lib_instantiate(ctx, 128 * 1024, 64 * 1024, NULL); 
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Failed to instantiate module: %d. Error: %s\n", rc, hb_beamr_lib_get_last_error(ctx));
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("Module instantiated successfully.\n");

    wasm_val_t get_ptr_results[1];
    rc = hb_beamr_lib_call_export(ctx, "get_data_ptr", 0, NULL, 1, get_ptr_results);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Failed to call 'get_data_ptr': %d. Error: %s\n", rc, hb_beamr_lib_get_last_error(ctx));
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    assert(get_ptr_results[0].kind == WASM_I32 && "'get_data_ptr' did not return I32");
    uint32_t data_ptr_offset = get_ptr_results[0].of.i32;
    printf("'get_data_ptr' returned offset: %u\n", data_ptr_offset);

    uint32_t target_index_main = 0; 
    uint32_t initial_value_for_wasm = 77; 
    printf("Using index %u and initial_value %u for Wasm's call_host_and_read.\n", target_index_main, initial_value_for_wasm);

    wasm_val_t call_args[2];
    call_args[0].kind = WASM_I32;
    call_args[0].of.i32 = target_index_main;
    call_args[1].kind = WASM_I32;
    call_args[1].of.i32 = initial_value_for_wasm;

    wasm_val_t call_results[1];
    rc = hb_beamr_lib_call_export(ctx, "call_host_and_read", 2, call_args, 1, call_results);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Failed to call 'call_host_and_read': %d. Error: %s\n", rc, hb_beamr_lib_get_last_error(ctx));
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }

    assert(call_results[0].kind == WASM_I32 && "'call_host_and_read' did not return I32");
    uint32_t result_val = call_results[0].of.i32;
    printf("'call_host_and_read' returned: %u\n", result_val);

    uint32_t expected_result_val = initial_value_for_wasm ^ 0xABCDEFFF; 
    assert(result_val == expected_result_val && "Result from call_host_and_read mismatch");
    printf("SUCCESS: 'call_host_and_read' returned the expected value (0x%X) after host interaction which modified the same index.\n", expected_result_val);

    hb_beamr_lib_destroy_context(ctx);
    hb_beamr_lib_destroy_runtime_global();
    printf("--- Test (WAMR Lib) Finished Successfully ---\n");
    return 0;
} 