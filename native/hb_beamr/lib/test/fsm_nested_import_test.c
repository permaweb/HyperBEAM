#include "hb_beamr_fsm.h"
#include "hb_beamr_lib.h"
#include "utils.h"
#include "wasm_export.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const char *nested_wasm_file = "import_nested.wasm";
static const uint32_t XOR_CONST = 0xABCDEFFF;

// give_host_control native – mirrors earlier test but uses FSM-owned context.
static void native_give_host_control(wasm_exec_env_t exec_env, uint64_t *args){
    uint32_t index = (uint32_t)args[0];
    wasm_module_inst_t inst = wasm_runtime_get_module_inst(exec_env);
    hb_beamr_lib_context_t *ctx = (hb_beamr_lib_context_t*)wasm_runtime_get_custom_data(inst);
    if(!ctx){ wasm_runtime_set_exception(inst,"ctx missing"); return; }
    wasm_val_t xor_args[2];
    xor_args[0].kind=WASM_I32; xor_args[0].of.i32=index;
    xor_args[1].kind=WASM_I32; xor_args[1].of.i32=XOR_CONST;
    hb_beamr_lib_call_export(ctx,"xor_memory",2,xor_args,0,NULL);
}

int main(){
    uint32_t wasm_size=0; uint8_t *wasm_buf=read_file_to_buffer(nested_wasm_file,&wasm_size);
    assert(wasm_buf && wasm_size>0);
    assert(hb_beamr_lib_init_runtime_global(NULL)==HB_BEAMR_LIB_SUCCESS);
    const hb_beamr_native_symbol_t sym_arr[] = {{"env", "give_host_control", (void*)native_give_host_control, "(i)", NULL}};
    hb_beamr_native_symbol_group_t group_env = {"env", sym_arr, sizeof(sym_arr)/sizeof(sym_arr[0])};
    hb_beamr_native_symbols_structured_t symbols_structured = {&group_env, 1};
    assert(hb_beamr_lib_register_global_natives(&symbols_structured)==HB_BEAMR_LIB_SUCCESS);

    hb_beamr_lib_context_t *ctx = hb_beamr_lib_create_context();
    hb_beamr_fsm_t fsm; hb_beamr_fsm_init(&fsm);

    assert(hb_beamr_fsm_load_module(&fsm,ctx,wasm_buf,wasm_size)==HB_BEAMR_LIB_SUCCESS);
    assert(hb_beamr_fsm_instantiate(&fsm,ctx,128*1024,64*1024)==HB_BEAMR_LIB_SUCCESS);

    wasm_val_t args[2]; wasm_val_t res[1];
    uint32_t init_val=77, index=0;
    args[0].kind=WASM_I32; args[0].of.i32=index;
    args[1].kind=WASM_I32; args[1].of.i32=init_val;
    res[0].kind=WASM_I32;
    assert(hb_beamr_fsm_call_export(&fsm,ctx,"call_host_and_read",2,args,1,res)==HB_BEAMR_LIB_SUCCESS);
    uint32_t expected = init_val ^ XOR_CONST;
    assert(res[0].of.i32 == expected);
    printf("fsm_nested_import_test PASSED (got %u)\n", res[0].of.i32);

    hb_beamr_fsm_quit(&fsm,ctx);
    hb_beamr_lib_destroy_runtime_global();
    free_buffer(wasm_buf);
    return 0;
} 