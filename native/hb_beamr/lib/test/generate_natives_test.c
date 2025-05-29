#include "hb_beamr_lib.h"
#include "utils.h"
#include "wasm_export.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>

#define MAX_LOG 64
typedef struct {
    char module[32];
    char field[64];
} log_entry_t;

static log_entry_t g_log[MAX_LOG];
static atomic_int g_log_pos = 0;

static void generic_stub(wasm_exec_env_t exec_env, uint64_t *args){
    hb_beamr_meta_import_t *att = (hb_beamr_meta_import_t*)wasm_runtime_get_function_attachment(exec_env);
    int pos = atomic_fetch_add_explicit(&g_log_pos,1,memory_order_relaxed);
    if(pos < MAX_LOG){ strncpy(g_log[pos].module, att->module_name,31); strncpy(g_log[pos].field, att->field_name,63); }
    // Simple behaviour: if single i32->i32, add 1
    (void)att;
    int32_t val = (int32_t)args[0];
    args[0] = (uint64_t)(val + 1);
}

static void run_import_test_module(){
    uint32_t sz=0; uint8_t *buf=read_file_to_buffer("import_test_module.aot",&sz); assert(buf);
    
    uint8_t *buf_cpy = malloc(sz);
    assert(buf_cpy);
    memcpy(buf_cpy,buf,sz);
    char err[1024];
    wasm_module_t mod = wasm_runtime_load(buf_cpy,sz,err,sizeof(err));
    assert(mod);

    hb_beamr_meta_module_t meta;
    memset(&meta,0,sizeof(meta));
    fprintf(stderr, "[DEBUG] hb_beamr_lib_meta_module\n");
    assert(hb_beamr_lib_meta_module(mod,&meta)==HB_BEAMR_LIB_SUCCESS);
    fprintf(stderr, "[DEBUG] hb_beamr_lib_meta_module done\n");
    wasm_runtime_unload(mod);

    hb_beamr_meta_func_t *func_meta;
    assert(hb_beamr_lib_meta_export_func(&meta,"wasm_add_two_via_host",&func_meta)==HB_BEAMR_LIB_SUCCESS);
    fprintf(stderr, "func_meta: param_count: %d, param_types[0]: %d, result_count: %d, result_types[0]: %d, signature: %s\n", func_meta->param_count, func_meta->param_types[0], func_meta->result_count, func_meta->result_types[0], func_meta->signature);
    assert(func_meta->param_count==1);
    assert(func_meta->param_types[0]==WASM_I32);
    assert(func_meta->result_count==1);
    assert(func_meta->result_types[0]==WASM_I32);
    assert(strcmp(func_meta->signature,"(i)i")==0);

    hb_beamr_native_symbols_structured_t structured;
    memset(&structured,0,sizeof(structured));
    assert(hb_beamr_lib_generate_natives(&meta,(void*)generic_stub,&structured)==HB_BEAMR_LIB_SUCCESS && structured.num_groups>0);

    assert(hb_beamr_lib_register_global_natives(&structured)==HB_BEAMR_LIB_SUCCESS);

    hb_beamr_lib_context_t *ctx=hb_beamr_lib_create_context();
    assert(hb_beamr_lib_load_wasm_module(ctx,buf,sz)==HB_BEAMR_LIB_SUCCESS);
    assert(hb_beamr_lib_instantiate(ctx,128*1024,0,NULL)==HB_BEAMR_LIB_SUCCESS);

    wasm_val_t a={.kind=WASM_I32,.of.i32=5}; wasm_val_t r; r.kind=WASM_I32;
    assert(hb_beamr_lib_call_export(ctx,"wasm_add_two_via_host",1,&a,1,&r)==HB_BEAMR_LIB_SUCCESS);
    assert(r.of.i32==7);

    hb_beamr_lib_destroy_context(ctx);
    hb_beamr_lib_free_natives(&structured);
    free_buffer(buf);

    // Validate that stub logged at least twice (function called twice inside wasm_add_two_via_host)
    assert(atomic_load_explicit(&g_log_pos,memory_order_relaxed)==2);
    for(int i=0;i<atomic_load(&g_log_pos);++i){
        printf("log[%d]: %s.%s\n",i,g_log[i].module,g_log[i].field);
    }
}

int main(){
    assert(hb_beamr_lib_init_runtime_global(NULL)==HB_BEAMR_LIB_SUCCESS);
    run_import_test_module();
    hb_beamr_lib_destroy_runtime_global();
    printf("generate_natives_test PASSED\n");
    return 0;
}
