#include "hb_beamr_fsm.h"
#include "hb_beamr_lib.h"
#include "utils.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include <stdatomic.h>

#define NUM_WORKERS_PHASE 10
#define ITERATIONS_PER_WORKER 50

static const char *fib_wasm_filename = "basic_fib.wasm";
static const char *import_wasm_filename = "import_test_module.wasm";

static uint8_t *g_fib_bytes = NULL;  static uint32_t g_fib_size = 0;
static uint8_t *g_import_bytes = NULL; static uint32_t g_import_size = 0;

static volatile int g_failure = 0;

static int fib_expected(int n){int a=0,b=1;for(int i=0;i<n;++i){int t=a+b;a=b;b=t;}return a;}

// host native
static void native_host_add_one(wasm_exec_env_t env, uint64_t *args){int32_t v=(int32_t)args[0];args[0]=(uint64_t)(v+1);} 

static void *worker_fib(void *arg){
    int wid=*(int*)arg; free(arg);
    if(!wasm_runtime_init_thread_env()){g_failure=1;return NULL;}
    int fib_arg=20; int expected=fib_expected(fib_arg);
    for(int i=0;i<ITERATIONS_PER_WORKER && !g_failure;++i){
        hb_beamr_lib_context_t *ctx=hb_beamr_lib_create_context();
        hb_beamr_fsm_t fsm; hb_beamr_fsm_init(&fsm);
        if(hb_beamr_fsm_load_module(&fsm,ctx,g_fib_bytes,g_fib_size)!=HB_BEAMR_LIB_SUCCESS){g_failure=1;break;}
        if(hb_beamr_fsm_instantiate(&fsm,ctx,128*1024,0)!=HB_BEAMR_LIB_SUCCESS){g_failure=1;break;}
        wasm_val_t args_v[1]={{.kind=WASM_I32,.of.i32=fib_arg}}; wasm_val_t res_v[1]={{.kind=WASM_I32}};
        if(hb_beamr_fsm_call_export(&fsm,ctx,"fib",1,args_v,1,res_v)!=HB_BEAMR_LIB_SUCCESS||res_v[0].of.i32!=expected){g_failure=1;}
        hb_beamr_fsm_quit(&fsm,ctx);
    }
    wasm_runtime_destroy_thread_env();
    return NULL;
}

static void *worker_import(void *arg){
    int wid=*(int*)arg;free(arg);
    if(!wasm_runtime_init_thread_env()){g_failure=1;return NULL;}
    for(int i=0;i<ITERATIONS_PER_WORKER && !g_failure;++i){
        hb_beamr_lib_context_t *ctx=hb_beamr_lib_create_context(); hb_beamr_fsm_t fsm; hb_beamr_fsm_init(&fsm);
        if(hb_beamr_fsm_load_module(&fsm,ctx,g_import_bytes,g_import_size)!=HB_BEAMR_LIB_SUCCESS){g_failure=1;break;}
        if(hb_beamr_fsm_instantiate(&fsm,ctx,128*1024,0)!=HB_BEAMR_LIB_SUCCESS){g_failure=1;break;}
        wasm_val_t a[1]={{.kind=WASM_I32,.of.i32=5}}; wasm_val_t r[1]={{.kind=WASM_I32}};
        if(hb_beamr_fsm_call_export(&fsm,ctx,"wasm_add_two_via_host",1,a,1,r)!=HB_BEAMR_LIB_SUCCESS||r[0].of.i32!=7){g_failure=1;}
        hb_beamr_fsm_quit(&fsm,ctx);
    }
    wasm_runtime_destroy_thread_env();
    return NULL;
}

static void *registrar_thread(void *unused){(void)unused; if(!wasm_runtime_init_thread_env()){g_failure=1;return NULL;} usleep(20000);
    hb_beamr_fsm_t fsm; hb_beamr_fsm_init(&fsm);
    const hb_beamr_native_symbol_t sym_arr[] = {{"env", "host_add_one", (void *)native_host_add_one, "(i)i", NULL}};
    hb_beamr_native_symbol_group_t group_env = {"env", sym_arr, sizeof(sym_arr)/sizeof(sym_arr[0])};
    hb_beamr_native_symbols_structured_t s_structured = {&group_env, 1};
    if(hb_beamr_fsm_register_natives(&fsm, &s_structured)!=HB_BEAMR_LIB_SUCCESS){g_failure=1;return NULL;}
    pthread_t workers[NUM_WORKERS_PHASE];
    for(int i=0;i<NUM_WORKERS_PHASE;++i){int *wid=malloc(sizeof(int));*wid=i;pthread_create(&workers[i],NULL,worker_import,wid);} for(int i=0;i<NUM_WORKERS_PHASE;++i) pthread_join(workers[i],NULL);
    wasm_runtime_destroy_thread_env(); return NULL;}

int main(){
    g_fib_bytes=read_file_to_buffer(fib_wasm_filename,&g_fib_size);
    g_import_bytes=read_file_to_buffer(import_wasm_filename,&g_import_size);
    assert(g_fib_bytes&&g_import_bytes);
    assert(hb_beamr_lib_init_runtime_global(NULL)==HB_BEAMR_LIB_SUCCESS);
    pthread_t workers[NUM_WORKERS_PHASE];
    for(int i=0;i<NUM_WORKERS_PHASE;++i){int *wid=malloc(sizeof(int));*wid=i;pthread_create(&workers[i],NULL,worker_fib,wid);} pthread_t registrar; pthread_create(&registrar,NULL,registrar_thread,NULL);
    for(int i=0;i<NUM_WORKERS_PHASE;++i) pthread_join(workers[i],NULL);
    pthread_join(registrar,NULL);
    hb_beamr_lib_destroy_runtime_global(); free_buffer(g_fib_bytes); free_buffer(g_import_bytes);
    if(g_failure){fprintf(stderr,"MultithreadStressFSMTest FAILED\n");
        return 1;
    }
    printf("MultithreadStressFSMTest PASSED\n"); return 0;
}