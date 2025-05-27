// async_import_threads_test.c
// Demonstrates nested import calls where every depth runs on its own
// pthread using a cloned ExecEnv, and the main thread coordinates xor
// values and resume signals.  Goal: all imports of depth 0..4 are
// entered before any returns.

#include "hb_beamr_lib.h"
#include "utils.h"
#include "wasm_export.h"

#include <assert.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>

#define MAX_DEPTH 5

#define LOG(fmt, ...)                                                      \
    do {                                                                  \
        fprintf(stderr, "[async_import_threads:%d] " fmt "\n", __LINE__,\
                ##__VA_ARGS__);                                            \
        fflush(stderr);                                                   \
    } while (0)

/* --------------------------------------------------------------------- */
/* Shared context                                                        */
/* --------------------------------------------------------------------- */

typedef struct async_ctx {
    pthread_mutex_t mutex;
    /* import → main */
    pthread_cond_t  cond_request[MAX_DEPTH];
    int             request_flag[MAX_DEPTH];
    /* main → import */
    pthread_cond_t  cond_resume[MAX_DEPTH];
    int             may_resume[MAX_DEPTH];
    /* worker → main */
    pthread_cond_t  cond_result[MAX_DEPTH];
    int             result_ready[MAX_DEPTH];
    int             result_val[MAX_DEPTH];

    int             xor_val[MAX_DEPTH];

    /* ExecEnvs per depth */
    wasm_exec_env_t exec_env_depth[MAX_DEPTH];

    /* Module & function handles */
    wasm_module_inst_t module_inst;
    wasm_function_inst_t fn_call_host_and_read;

} async_ctx_t;

static async_ctx_t g_ctx; /* single global for simplicity */

/* --------------------------------------------------------------------- */
/* Fibonacci helper                                                      */
/* --------------------------------------------------------------------- */
static int fib(int n){ int a=0,b=1; for(int i=0;i<n;i++){int t=a+b;a=b;b=t;}return a; }

/* --------------------------------------------------------------------- */
/* Import handler                                                        */
/* --------------------------------------------------------------------- */
static void native_give_host_control(wasm_exec_env_t exec_env, uint64_t *args){
    unsigned int idx = (unsigned int)args[0];

    /* depth number is stored in ExecEnv user_data */
    int depth = (int)(intptr_t)wasm_runtime_get_user_data(exec_env);
    LOG("native_give_host_control[%d] ENTER (index=%u)", depth, idx);

    async_ctx_t *ctx = &g_ctx;

    pthread_mutex_lock(&ctx->mutex);
    ctx->request_flag[depth] = 1;
    pthread_cond_signal(&ctx->cond_request[depth]);
    while(!ctx->may_resume[depth])
        pthread_cond_wait(&ctx->cond_resume[depth], &ctx->mutex);
    pthread_mutex_unlock(&ctx->mutex);

    int xor_val = ctx->xor_val[depth];

    /* perform xor_memory via hb_beamr */
    hb_beamr_lib_context_t *lib_ctx = (hb_beamr_lib_context_t *)
        wasm_runtime_get_custom_data(ctx->module_inst);
    assert(lib_ctx);
    wasm_val_t cargs[2];
    cargs[0].kind = WASM_I32; cargs[0].of.i32 = idx;
    cargs[1].kind = WASM_I32; cargs[1].of.i32 = xor_val;
    LOG("hb_beamr_lib_call_export[%d] xor_memory BEGIN", depth);
    hb_beamr_lib_rc_t rc = hb_beamr_lib_call_export(lib_ctx,
                                                    "xor_memory", 2, cargs,
                                                    0, NULL);
    LOG("hb_beamr_lib_call_export[%d] xor_memory END", depth);
    assert(rc == HB_BEAMR_LIB_SUCCESS);

    LOG("native_give_host_control[%d] EXIT", depth);
}

/* --------------------------------------------------------------------- */
/* Worker thread per depth                                               */
/* --------------------------------------------------------------------- */
typedef struct { int depth; } worker_arg_t;

static void *depth_thread(void *arg){
    worker_arg_t *wa = (worker_arg_t*)arg; int d = wa->depth; free(wa);
    async_ctx_t *ctx = &g_ctx;

    wasm_runtime_init_thread_env();

    wasm_exec_env_t ee = ctx->exec_env_depth[d];
    wasm_runtime_set_user_data(ee, (void*)(intptr_t)d);

    wasm_val_t args[2];
    args[0].kind = WASM_I32; args[0].of.i32 = 0;
    args[1].kind = WASM_I32; args[1].of.i32 = 100;
    wasm_val_t res[1];

    LOG("depth_thread[%d] call_host_and_read BEGIN", d);
    bool ok = wasm_runtime_call_wasm_a(ee, ctx->fn_call_host_and_read,
                                       1, res, 2, args);
    assert(ok);
    LOG("depth_thread[%d] call_host_and_read RETURN %d", d, res[0].of.i32);

    pthread_mutex_lock(&ctx->mutex);
    ctx->result_val[d]   = res[0].of.i32;
    ctx->result_ready[d] = 1;
    pthread_cond_signal(&ctx->cond_result[d]);
    pthread_mutex_unlock(&ctx->mutex);

    wasm_runtime_destroy_thread_env();
    return NULL;
}

/* --------------------------------------------------------------------- */
/* Recursive main-thread coordinator                                      */
/* --------------------------------------------------------------------- */
static pthread_t g_tids[MAX_DEPTH]; /* worker thread handles */

static void handle_depth(int d){
    async_ctx_t *ctx = &g_ctx;

    /* Wait until worker depth d has entered its import */
    pthread_mutex_lock(&ctx->mutex);
    while(!ctx->request_flag[d])
        pthread_cond_wait(&ctx->cond_request[d], &ctx->mutex);
    pthread_mutex_unlock(&ctx->mutex);
    LOG("Main got import request depth %d", d);

    /* Provide xor value for this depth */
    ctx->xor_val[d] = fib(10 + d);

    if (d < MAX_DEPTH - 1) {
        /* Spawn next-depth worker and process it fully before resuming d */
        worker_arg_t *wa = malloc(sizeof *wa); wa->depth = d + 1;
        pthread_create(&g_tids[d + 1], NULL, depth_thread, wa);

        /* Recurse: this call returns only after depth d+1 has finished */
        handle_depth(d + 1);
    }

    /* Now resume depth d (its import will return) */
    pthread_mutex_lock(&ctx->mutex);
    ctx->may_resume[d] = 1;
    pthread_cond_signal(&ctx->cond_resume[d]);
    pthread_mutex_unlock(&ctx->mutex);
    LOG("Main resumed depth %d", d);

    /* Wait for the worker of depth d to finish and deliver its result */
    pthread_mutex_lock(&ctx->mutex);
    while(!ctx->result_ready[d])
        pthread_cond_wait(&ctx->cond_result[d], &ctx->mutex);
    int res = ctx->result_val[d];
    pthread_mutex_unlock(&ctx->mutex);

    /* The memory value observed by this depth is either the initial 100
       (for the deepest depth) or the result returned by the next deeper
       depth which has already finished. */
    int prev_val = (d == MAX_DEPTH - 1) ? 100 : ctx->result_val[d + 1];
    int expected = prev_val ^ ctx->xor_val[d];

    LOG("Depth %d result %d expected %d", d, res, expected);
    assert(res == expected);
    LOG("Depth %d result ok (%d)", d, res);
}

/* --------------------------------------------------------------------- */
/* Main                                                                   */
/* --------------------------------------------------------------------- */
int main(){
    assert(hb_beamr_lib_init_runtime_global(NULL) == HB_BEAMR_LIB_SUCCESS);

    /* Init ctx */
    memset(&g_ctx, 0, sizeof(g_ctx));
    pthread_mutex_init(&g_ctx.mutex, NULL);
    for(int i=0;i<MAX_DEPTH;i++){
        pthread_cond_init(&g_ctx.cond_request[i], NULL);
        pthread_cond_init(&g_ctx.cond_resume[i], NULL);
        pthread_cond_init(&g_ctx.cond_result[i], NULL);
    }

    /* Register native */
    const hb_beamr_native_symbol_t sym[] = {
        {"env","give_host_control", (void*)native_give_host_control, "(i)", NULL}
    };
    hb_beamr_native_symbol_group_t group = {"env", sym, 1};
    hb_beamr_native_symbols_structured_t structured = { &group, 1};
    assert(hb_beamr_lib_register_global_natives(&structured)==HB_BEAMR_LIB_SUCCESS);

    /* Load module once in a temp context to extract module/instance for spawning exec envs. */
    hb_beamr_lib_context_t *loader_ctx = hb_beamr_lib_create_context();
    uint32_t wasm_sz=0; uint8_t *wasm_buf = read_file_to_buffer("import_nested.aot", &wasm_sz);
    assert(hb_beamr_lib_load_wasm_module(loader_ctx, wasm_buf, wasm_sz)==HB_BEAMR_LIB_SUCCESS);
    assert(hb_beamr_lib_instantiate(loader_ctx, 128*1024, 0)==HB_BEAMR_LIB_SUCCESS);
    g_ctx.module_inst = hb_beamr_lib_get_module_instance(loader_ctx);
    g_ctx.fn_call_host_and_read = wasm_runtime_lookup_function(g_ctx.module_inst, "call_host_and_read");
    assert(g_ctx.fn_call_host_and_read);

    /* Prepare ExecEnvs – create a dedicated ExecEnv for every depth so that
       each worker thread owns its execution environment and they are not
       shared concurrently across threads. */
    for(int i = 0; i < MAX_DEPTH; i++) {
        /* 128 KiB matches the stack size used when the module was first
           instantiated above.  Adjust if stack requirements change. */
        g_ctx.exec_env_depth[i] = wasm_runtime_create_exec_env(g_ctx.module_inst, 128 * 1024);
        assert(g_ctx.exec_env_depth[i]);
    }

    /* Spawn the first (outermost) worker and kick off recursive handling */
    worker_arg_t *wa0 = malloc(sizeof *wa0); wa0->depth = 0;
    pthread_create(&g_tids[0], NULL, depth_thread, wa0);

    handle_depth(0); /* blocks until all depths complete */

    /* Join all spawned worker threads */
    for (int i = 0; i < MAX_DEPTH; i++) pthread_join(g_tids[i], NULL);

    /* Clean-up: free the per-depth ExecEnvs now that all worker threads have
       completed. */
    for (int i = 0; i < MAX_DEPTH; i++) {
        wasm_runtime_destroy_exec_env(g_ctx.exec_env_depth[i]);
        g_ctx.exec_env_depth[i] = NULL;
    }

    LOG("Test PASSED");
    printf("async_import_threads_test PASSED\n");
    return 0;
}
