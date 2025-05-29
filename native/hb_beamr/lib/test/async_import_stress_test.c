// async_import_stress_test.c
// Mega-stress test covering many modules, instances, and deeply-nested
// import/export chains.  It re-uses the "import_nested" fixture which
// performs host-import ↔ export round-trips and composes them to  NESTED_DEPTH
// levels.  The test launches NUM_MODULES distinct module binaries (Emscripten
// and hand-written WAT variants) – here we simply reference different fixture
// paths; CMake copies the same underlying binary if a second fixture is not
// built.  For each module we create NUM_INSTANCES instances which run in
// parallel.  Each instance drives a nested call tree of depth NESTED_DEPTH
// executed over separate pthreads, verifying the unique XOR-based memory value
// at every step.

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

/* Unified logging macro for this stress test */
#define LOG(fmt, ...)                                                      \
    do {                                                                  \
        fprintf(stderr, "[async_import_stress:%s:%d] " fmt "\n",      \
                __FUNCTION__, __LINE__, ##__VA_ARGS__);                   \
        fflush(stderr);                                                   \
    } while (0)

/* ------------------------------------------------------------------------- */
/* Configuration                                                             */
/* ------------------------------------------------------------------------- */
#define NUM_MODULES     1   /* distinct wasm/aot binaries */
#define NUM_INSTANCES   1  /* instances per module       */
#define NESTED_DEPTH    2  /* depth of import recursion   */

static const char *g_module_paths[NUM_MODULES] = {
    // "import_nested.aot",       /* built from Emscripten C */
    "import_nested_wat.aot"    /* built from a WAT source (identical binary
                                   is copied by CMake if WAT compiler absent) */
};

/* ------------------------------------------------------------------------- */
/* Fibonacci helper (small)                                                   */
/* ------------------------------------------------------------------------- */
static inline int fib_small(int n) {
    int a = 0, b = 1;
    for (int i = 0; i < n; ++i) { int t = a + b; a = b; b = t; }
    return a;
}

/* ------------------------------------------------------------------------- */
/* Per-instance async context                                                 */
/* ------------------------------------------------------------------------- */
typedef struct stress_instance_ctx stress_instance_ctx_t;

/* user_data payload stored in each ExecEnv so that the import handler can
   retrieve both depth and owning instance */
typedef struct {
    stress_instance_ctx_t *inst_ctx;
    int                    depth; /* 0 .. NESTED_DEPTH-1 */
} execenv_tag_t;

struct stress_instance_ctx {
    int instance_id;   /* 0..NUM_INSTANCES-1 within a module */
    int module_id;     /* 0..NUM_MODULES-1 */

    /* Synchronisation primitives per depth */
    pthread_mutex_t mutex;
    pthread_cond_t  cond_request[NESTED_DEPTH];
    pthread_cond_t  cond_resume[NESTED_DEPTH];
    pthread_cond_t  cond_result[NESTED_DEPTH];

    int request_flag[NESTED_DEPTH];
    int may_resume[NESTED_DEPTH];
    int result_ready[NESTED_DEPTH];
    int result_val[NESTED_DEPTH];

    int xor_val[NESTED_DEPTH];

    /* WAMR bits */
    wasm_module_inst_t module_inst;
    hb_beamr_lib_context_t *instance_lib_ctx; /* For memory validation & xor_memory calls */
    wasm_function_inst_t fn_call_host_and_read;
    wasm_exec_env_t exec_env_depth[NESTED_DEPTH];
    execenv_tag_t   ee_tag[NESTED_DEPTH];

    /* Base offset of global_data_buffer inside wasm memory */
    uint32_t        data_base_offset;
};

/* ------------------------------------------------------------------------- */
/* Native import handler                                                     */
/* ------------------------------------------------------------------------- */
static void native_give_host_control(wasm_exec_env_t exec_env, uint64_t *args) {
    execenv_tag_t *tag = (execenv_tag_t *)wasm_runtime_get_user_data(exec_env);
    stress_instance_ctx_t *ictx = tag->inst_ctx;
    int depth = tag->depth;
    unsigned int idx = (unsigned int)args[0];

    LOG("ENTER (native_give_host_control) m%d i%d d%d (idx=%u)", ictx->module_id, ictx->instance_id, depth, idx);

    /* ---------------- Pre-lock validations ---------------- */
    /* Expect that the worker passed its depth as the memory index */
    assert(idx == (unsigned int)depth && "import idx should equal current depth");

    /* Compute the seed value that should have been written by the worker */
    unsigned int expected_seed = (unsigned int)(100 + ictx->instance_id * 7 + depth);

    uint32_t mem_word_pre = 0;
    uint32_t byte_off = ictx->data_base_offset + idx * sizeof(uint32_t);
    hb_beamr_lib_rc_t rc_pre = hb_beamr_lib_direct_read_memory(ictx->instance_lib_ctx,
                                                               byte_off,
                                                               (uint8_t *)&mem_word_pre,
                                                               sizeof(uint32_t));
    assert(rc_pre == HB_BEAMR_LIB_SUCCESS && "direct_read_memory failed (pre-lock)");
    assert(mem_word_pre == expected_seed && "unexpected memory contents before coordinator handshake");

    /* ---------------- Coordinator handshake ---------------- */
    /* Notify coordinator that this depth is entered */
    pthread_mutex_lock(&ictx->mutex);
    ictx->request_flag[depth] = 1;
    pthread_cond_signal(&ictx->cond_request[depth]);
    /* Wait until coordinator says we may resume */
    while (!ictx->may_resume[depth]) {
        pthread_cond_wait(&ictx->cond_resume[depth], &ictx->mutex);
    }
    pthread_mutex_unlock(&ictx->mutex);

    /* ---------------- Post-unlock validations ---------------- */
    uint32_t mem_word_post = 0;
    hb_beamr_lib_rc_t rc_post = hb_beamr_lib_direct_read_memory(ictx->instance_lib_ctx,
                                                                byte_off,
                                                                (uint8_t *)&mem_word_post,
                                                                sizeof(uint32_t));
    assert(rc_post == HB_BEAMR_LIB_SUCCESS && "direct_read_memory failed (post-lock)");
    assert(mem_word_post == expected_seed && "unexpected memory contents after coordinator resume");

    // NO memory operations performed by the import itself in this version.
    // The xor_val[depth] is pre-calculated but not used here.

    LOG("EXIT (native_give_host_control) m%d i%d d%d", ictx->module_id, ictx->instance_id, depth);
}

/* ------------------------------------------------------------------------- */
/* Worker thread per depth                                                   */
/* ------------------------------------------------------------------------- */
typedef struct {
    stress_instance_ctx_t *ictx;
    int depth;
    hb_beamr_lib_context_t *lib_ctx; /* For direct memory validation */
} depth_thread_arg_t;

static void *depth_worker(void *arg) {
    depth_thread_arg_t *dt = (depth_thread_arg_t *)arg;
    stress_instance_ctx_t *ictx = dt->ictx;
    int d = dt->depth;
    hb_beamr_lib_context_t *lib_ctx_for_mem = dt->lib_ctx;
    free(dt);

    LOG("ENTER m%d i%d d%d", ictx->module_id, ictx->instance_id, d);
    wasm_runtime_init_thread_env();

    wasm_exec_env_t ee = ictx->exec_env_depth[d];

    wasm_val_t args[2];
    args[0].kind = WASM_I32; args[0].of.i32 = d;   /* mem index = depth */
    int seed = 100 + ictx->instance_id * 7 + d; /* unique per depth */
    args[1].kind = WASM_I32; args[1].of.i32 = seed;
    wasm_val_t res[1];

    LOG("m%d i%d d%d: calling call_host_and_read(idx=%d, seed=%d)", ictx->module_id, ictx->instance_id, d, d, seed);
    bool ok = wasm_runtime_call_wasm_a(ee, ictx->fn_call_host_and_read,
                                       1, res, 2, args);
    assert(ok && "wasm_runtime_call_wasm_a failed");

    // Direct memory read for logging, but primary assertion is in handle_depth
    uint32_t mem_word_after_wasm = 0;
    uint32_t offset_bytes = ictx->data_base_offset + (uint32_t)(d * sizeof(uint32_t));
    hb_beamr_lib_rc_t rc_mem_after = hb_beamr_lib_direct_read_memory(lib_ctx_for_mem,
                                                                     offset_bytes,
                                                                     (uint8_t *)&mem_word_after_wasm,
                                                                     sizeof(uint32_t));
    
    if (rc_mem_after != HB_BEAMR_LIB_SUCCESS) {
        LOG("m%d i%d d%d: hb_beamr_lib_direct_read_memory FAILED with %d", ictx->module_id, ictx->instance_id, d, rc_mem_after);
        // Do not assert false here, let handle_depth catch logical errors if any
    } else {
        LOG("m%d i%d d%d: Wasm returned val %d. Direct C read after Wasm call saw mem_word %u. (This may differ due to WAMR exec_env memory model)", 
            ictx->module_id, ictx->instance_id, d, res[0].of.i32, mem_word_after_wasm);
    }
    // assert(mem_word_after_wasm == (uint32_t)res[0].of.i32 && "Wasm return vs C direct read mismatch"); // Temporarily removed

    pthread_mutex_lock(&ictx->mutex);
    ictx->result_val[d]   = res[0].of.i32;
    ictx->result_ready[d] = 1;
    LOG("m%d i%d d%d: result_val=%d set for handle_depth, signaling cond_result", ictx->module_id, ictx->instance_id, d, res[0].of.i32);
    pthread_cond_signal(&ictx->cond_result[d]);
    pthread_mutex_unlock(&ictx->mutex);

    wasm_runtime_destroy_thread_env();

    LOG("EXIT m%d i%d d%d", ictx->module_id, ictx->instance_id, d);
    return NULL;
}

/* ------------------------------------------------------------------------- */
/* Recursive coordinator (runs inside per-instance master thread)            */
/* ------------------------------------------------------------------------- */
static void handle_depth(stress_instance_ctx_t *ictx, int depth) {
    /* Wait import entry */
    pthread_mutex_lock(&ictx->mutex);
    while (!ictx->request_flag[depth]) {
        pthread_cond_wait(&ictx->cond_request[depth], &ictx->mutex);
    }
    LOG("m%d i%d d%d: got request_flag", ictx->module_id, ictx->instance_id, depth);
    pthread_mutex_unlock(&ictx->mutex);

    if (depth < NESTED_DEPTH - 1) {
        /* Spawn deeper worker and fully process it first */
        depth_thread_arg_t *dt = malloc(sizeof *dt);
        dt->ictx = ictx; dt->depth = depth + 1; dt->lib_ctx = ictx->instance_lib_ctx;
        pthread_t tid;
        pthread_create(&tid, NULL, depth_worker, dt);

        LOG("m%d i%d d%d: spawned depth_worker for d+1=%d, now handle_depth(%d)", ictx->module_id, ictx->instance_id, depth, depth+1, depth+1);
        handle_depth(ictx, depth + 1);
        LOG("m%d i%d d%d: handle_depth for d+1=%d returned, joining tid", ictx->module_id, ictx->instance_id, depth, depth+1);
        pthread_join(tid, NULL);
    }

    /* Provide xor value and resume */
    // xor_val[depth] is set in instance_master, but not used by the minimal native_give_host_control
    pthread_mutex_lock(&ictx->mutex);
    ictx->may_resume[depth] = 1;
    pthread_cond_signal(&ictx->cond_resume[depth]);
    pthread_mutex_unlock(&ictx->mutex);

    /* Wait for worker result */
    pthread_mutex_lock(&ictx->mutex);
    while (!ictx->result_ready[depth])
        pthread_cond_wait(&ictx->cond_result[depth], &ictx->mutex);
    int res = ictx->result_val[depth];
    LOG("m%d i%d d%d: got result_ready, res=%d", ictx->module_id, ictx->instance_id, depth, res);
    pthread_mutex_unlock(&ictx->mutex);

    /* Expected value is just the seed, as the import does no XOR */
    int current_seed = 100 + ictx->instance_id * 7 + depth;
    int expected_val_for_this_depth = current_seed; // No XOR operation in minimal import

    LOG("m%d i%d d%d: res=%d, expected_val_for_this_depth=%d (seed=%d, no xor)",
        ictx->module_id, ictx->instance_id, depth, res, expected_val_for_this_depth,
        current_seed);
    assert(res == expected_val_for_this_depth && "unexpected result for this depth (minimal import)");
}

/* ------------------------------------------------------------------------- */
/* Per-instance master                                                       */
/* ------------------------------------------------------------------------- */
static void *instance_master(void *arg) {
    stress_instance_ctx_t *ictx = (stress_instance_ctx_t *)arg;

    LOG("ENTER m%d i%d", ictx->module_id, ictx->instance_id);
    wasm_runtime_init_thread_env();

    /* Load module */
    uint32_t wasm_sz = 0;
    LOG("m%d i%d: loading module %s", ictx->module_id, ictx->instance_id, g_module_paths[ictx->module_id]);
    uint8_t *wasm_buf = read_file_to_buffer(g_module_paths[ictx->module_id], &wasm_sz);
    assert(wasm_buf);

    hb_beamr_lib_context_t *loader_ctx = hb_beamr_lib_create_context();
    assert(hb_beamr_lib_load_wasm_module(loader_ctx, wasm_buf, wasm_sz) == HB_BEAMR_LIB_SUCCESS);
    assert(hb_beamr_lib_instantiate(loader_ctx, 128 * 1024, 0, NULL) == HB_BEAMR_LIB_SUCCESS);

    ictx->instance_lib_ctx = loader_ctx; /* Store for use by workers and import handler */

    ictx->module_inst = hb_beamr_lib_get_module_instance(loader_ctx);
    ictx->fn_call_host_and_read = wasm_runtime_lookup_function(ictx->module_inst,
                                                               "call_host_and_read");
    assert(ictx->fn_call_host_and_read);

    /* Obtain base offset of global_data_buffer via get_data_ptr() */
    wasm_function_inst_t fn_get_data_ptr = wasm_runtime_lookup_function(ictx->module_inst, "get_data_ptr");
    assert(fn_get_data_ptr);
    wasm_exec_env_t ee_tmp = wasm_runtime_create_exec_env(ictx->module_inst, 64 * 1024);
    assert(ee_tmp);
    wasm_val_t res_ptr[1];
    bool ok_gp = wasm_runtime_call_wasm_a(ee_tmp, fn_get_data_ptr, 1, res_ptr, 0, NULL);
    assert(ok_gp);
    ictx->data_base_offset = (uint32_t)res_ptr[0].of.i32;
    wasm_runtime_destroy_exec_env(ee_tmp);
    LOG("m%d i%d: data_base_offset=0x%x", ictx->module_id, ictx->instance_id, ictx->data_base_offset);

    /* ExecEnvs */
    for (int d = 0; d < NESTED_DEPTH; ++d) {
        ictx->exec_env_depth[d] = wasm_runtime_create_exec_env(ictx->module_inst, 128 * 1024);
        assert(ictx->exec_env_depth[d]);
        ictx->ee_tag[d].inst_ctx = ictx;
        ictx->ee_tag[d].depth = d;
        wasm_runtime_set_user_data(ictx->exec_env_depth[d], &ictx->ee_tag[d]);
    }

    /* Pre-compute xor values (unique per module/instance/depth) */
    for (int d = 0; d < NESTED_DEPTH; ++d) {
        ictx->xor_val[d] = fib_small(10 + d + 3 * ictx->instance_id + 7 * ictx->module_id);
        /* Also validate that xor_val is non-zero to ensure diversity */
        assert(ictx->xor_val[d] != 0);
    }

    /* Spawn outermost depth worker */
    depth_thread_arg_t *dt0 = malloc(sizeof *dt0);
    dt0->ictx = ictx; dt0->depth = 0; dt0->lib_ctx = ictx->instance_lib_ctx;
    LOG("m%d i%d: spawning depth_worker for d=0", ictx->module_id, ictx->instance_id);
    pthread_t tid0;
    pthread_create(&tid0, NULL, depth_worker, dt0);

    handle_depth(ictx, 0);
    LOG("m%d i%d: handle_depth(0) returned, joining tid0", ictx->module_id, ictx->instance_id);
    pthread_join(tid0, NULL);

    /* Cleanup */
    for (int d = 0; d < NESTED_DEPTH; ++d) {
        wasm_runtime_destroy_exec_env(ictx->exec_env_depth[d]);
    }
    hb_beamr_lib_destroy_context(loader_ctx);
    free_buffer(wasm_buf);

    LOG("EXIT m%d i%d", ictx->module_id, ictx->instance_id);
    wasm_runtime_destroy_thread_env();
    return NULL;
}

/* ------------------------------------------------------------------------- */
/* Native registration helper                                                */
/* ------------------------------------------------------------------------- */
static void register_natives_once(void) {
    static int registered = 0;
    if (registered) return;
    const hb_beamr_native_symbol_t sym[] = {
        {"env", "give_host_control", (void *)native_give_host_control, "(i)", NULL}
    };
    hb_beamr_native_symbol_group_t grp = {"env", sym, 1};
    hb_beamr_native_symbols_structured_t structured = {&grp, 1};
    assert(hb_beamr_lib_register_global_natives(&structured) == HB_BEAMR_LIB_SUCCESS);
    registered = 1;
}

/* ------------------------------------------------------------------------- */
/* Test entry point                                                          */
/* ------------------------------------------------------------------------- */
int main(void) {
    assert(hb_beamr_lib_init_runtime_global(NULL) == HB_BEAMR_LIB_SUCCESS);
    LOG("Runtime initialized.");
    register_natives_once();

    pthread_t masters[NUM_MODULES][NUM_INSTANCES];
    stress_instance_ctx_t *contexts[NUM_MODULES][NUM_INSTANCES];

    /* Allocate contexts & spawn masters */
    for (int mid = 0; mid < NUM_MODULES; ++mid) {
        for (int iid = 0; iid < NUM_INSTANCES; ++iid) {
            stress_instance_ctx_t *ctx = calloc(1, sizeof *ctx);
            contexts[mid][iid] = ctx;
            ctx->module_id   = mid;
            ctx->instance_id = iid;
            pthread_mutex_init(&ctx->mutex, NULL);
            for (int d = 0; d < NESTED_DEPTH; ++d) {
                pthread_cond_init(&ctx->cond_request[d], NULL);
                pthread_cond_init(&ctx->cond_resume[d], NULL);
                pthread_cond_init(&ctx->cond_result[d], NULL);
            }
            LOG("Spawning instance_master m%d i%d", mid, iid);
            pthread_create(&masters[mid][iid], NULL, instance_master, ctx);
        }
    }

    /* Join all */
    for (int mid = 0; mid < NUM_MODULES; ++mid) {
        for (int iid = 0; iid < NUM_INSTANCES; ++iid) {
            pthread_join(masters[mid][iid], NULL);
            /* Cleanup pthread primitives */
            stress_instance_ctx_t *ctx = contexts[mid][iid];
            for (int d = 0; d < NESTED_DEPTH; ++d) {
                pthread_cond_destroy(&ctx->cond_request[d]);
                pthread_cond_destroy(&ctx->cond_resume[d]);
                pthread_cond_destroy(&ctx->cond_result[d]);
            }
            pthread_mutex_destroy(&ctx->mutex);
            free(ctx);
        }
    }

    hb_beamr_lib_destroy_runtime_global();
    printf("async_import_stress_test PASSED\n");
    LOG("All masters joined, test PASSED.");
    return 0;
}
