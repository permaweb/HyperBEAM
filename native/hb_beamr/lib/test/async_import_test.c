// async_import_test.c - Test asynchronous host import handling using import_nested module.
// This test spawns a worker thread that runs a Wasm module (import_nested.aot).
// The module calls the imported host function give_host_control which blocks
// waiting for the main thread to compute and provide a value. After a short
// delay, the main thread signals the worker thread which resumes execution,
// completes the Wasm call, and returns the final result. The main thread then
// validates the result.

#include "hb_beamr_lib.h"
#include "utils.h"
#include "wasm_export.h"

#include <pthread.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h> // for sleep()

/* ------------------------------------------------------------------------- */
/* Simple logging macro                                                       */
/* ------------------------------------------------------------------------- */
#define LOG(format, ...)                                                        \
    do {                                                                       \
        fprintf(stderr, "[async_import_test:%d] " format "\n", __LINE__,      \
                ##__VA_ARGS__);                                               \
        fflush(stderr);                                                        \
    } while (0)

/* ------------------------------------------------------------------------- */
/* Synchronisation structure shared between worker & main threads             */
/* ------------------------------------------------------------------------- */

typedef struct async_ctx {
    pthread_mutex_t mutex;
    pthread_cond_t  cond_request;   // signalled when Wasm import is invoked
    pthread_cond_t  cond_response;  // signalled when main thread provides xor
    int             request_idx;    // index received from Wasm
    int             xor_val;        // value provided by main thread
    int             has_request;    // flag set by import handler
    int             has_response;   // flag set by main thread
    int             final_result;   // result of Wasm call (set by worker)
} async_ctx_t;

static async_ctx_t *g_async_ctx = NULL; // global for the native import handler

/* ------------------------------------------------------------------------- */
/* Utility: Fibonacci (for demo; may be used to compute xor_val)              */
/* ------------------------------------------------------------------------- */
static int fib(int n) {
    int a = 0, b = 1;
    for (int i = 0; i < n; ++i) { int t = a + b; a = b; b = t; }
    return a;
}

/* ------------------------------------------------------------------------- */
/* Native import: give_host_control                                           */
/* ------------------------------------------------------------------------- */
static void native_give_host_control(wasm_exec_env_t exec_env, uint64_t *args) {
    unsigned int index_from_wasm = (unsigned int)args[0];

    // Grab shared async context.
    async_ctx_t *actx = g_async_ctx;
    if (!actx) { LOG("[ERROR] async context is NULL"); return; }

    // ------------------------------------------------------------------
    // 1) Notify main thread that Wasm requests a value.
    // ------------------------------------------------------------------
    pthread_mutex_lock(&actx->mutex);
    actx->request_idx = (int)index_from_wasm;
    actx->has_request = 1;
    pthread_cond_signal(&actx->cond_request);

    // ------------------------------------------------------------------
    // 2) Wait for main thread to supply xor_val.
    // ------------------------------------------------------------------
    while (!actx->has_response) {
        pthread_cond_wait(&actx->cond_response, &actx->mutex);
    }
    int xor_val = actx->xor_val;
    actx->has_response = 0; // one-shot
    pthread_mutex_unlock(&actx->mutex);

    // ------------------------------------------------------------------
    // 3) Call Wasm export xor_memory(index, xor_val) to mutate memory.
    // ------------------------------------------------------------------
    wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
    if (!module_inst) {
        LOG("[ERROR] wasm_runtime_get_module_inst returned NULL");
        return;
    }
    hb_beamr_lib_context_t *lib_ctx = (hb_beamr_lib_context_t *)
        wasm_runtime_get_custom_data(module_inst);
    if (!lib_ctx) {
        LOG("[ERROR] custom_data (lib_ctx) is NULL");
        return;
    }

    wasm_val_t call_args[2];
    call_args[0].kind   = WASM_I32;
    call_args[0].of.i32 = index_from_wasm;
    call_args[1].kind   = WASM_I32;
    call_args[1].of.i32 = xor_val;

    hb_beamr_lib_rc_t rc = hb_beamr_lib_call_export(lib_ctx,
                                                    "xor_memory",
                                                    2, call_args,
                                                    0, NULL);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        LOG("[ERROR] call to xor_memory failed: %d – %s", rc,
            hb_beamr_lib_get_last_error(lib_ctx));
    }
}

/* ------------------------------------------------------------------------- */
/* Worker thread: loads module, instantiates, invokes call_host_and_read      */
/* ------------------------------------------------------------------------- */
static void *worker_thread(void *arg) {
    async_ctx_t *actx = (async_ctx_t *)arg;

    // Each WAMR thread must initialise its own environment.
    wasm_runtime_init_thread_env();

    hb_beamr_lib_context_t *ctx = hb_beamr_lib_create_context();

    // Load Wasm binary (AOT).
    uint32_t wasm_size = 0;
    uint8_t *wasm_bytes = read_file_to_buffer("import_nested.aot", &wasm_size);
    assert(wasm_bytes != NULL);
    assert(hb_beamr_lib_load_wasm_module(ctx, wasm_bytes, wasm_size) ==
           HB_BEAMR_LIB_SUCCESS);

    // Instantiate.
    assert(hb_beamr_lib_instantiate(ctx, 128 * 1024, 0, NULL) == HB_BEAMR_LIB_SUCCESS);

    // call_host_and_read(index = 0, init_val = 100)
    wasm_val_t args[2];
    args[0].kind   = WASM_I32; args[0].of.i32 = 0;   // index
    args[1].kind   = WASM_I32; args[1].of.i32 = 100; // init_val
    wasm_val_t res[1];

    hb_beamr_lib_rc_t rc_call = hb_beamr_lib_call_export(ctx,
                                                         "call_host_and_read",
                                                         2, args, 1, res);
    assert(rc_call == HB_BEAMR_LIB_SUCCESS);

    // Store result for the main thread.
    actx->final_result = res[0].of.i32;

    // Cleanup.
    hb_beamr_lib_destroy_context(ctx);
    free_buffer(wasm_bytes);
    wasm_runtime_destroy_thread_env();
    return NULL;
}

/* ------------------------------------------------------------------------- */
/* Test entry point                                                           */
/* ------------------------------------------------------------------------- */
int main(void) {
    // Initialise global WAMR runtime.
    assert(hb_beamr_lib_init_runtime_global(NULL) == HB_BEAMR_LIB_SUCCESS);

    // ------------------------------------------------------------------
    // Prepare shared synchronisation context.
    // ------------------------------------------------------------------
    async_ctx_t actx;
    memset(&actx, 0, sizeof(actx));
    pthread_mutex_init(&actx.mutex, NULL);
    pthread_cond_init(&actx.cond_request, NULL);
    pthread_cond_init(&actx.cond_response, NULL);
    g_async_ctx = &actx;

    // ------------------------------------------------------------------
    // Register native give_host_control.
    // ------------------------------------------------------------------
    const hb_beamr_native_symbol_t sym_arr[] = {
        {"env", "give_host_control", (void *)native_give_host_control, "(i)", NULL}
    };
    hb_beamr_native_symbol_group_t group_env = {
        .module_name  = "env",
        .symbols      = sym_arr,
        .num_symbols  = 1
    };
    hb_beamr_native_symbols_structured_t symbols_struct = {
        .groups      = &group_env,
        .num_groups  = 1
    };
    assert(hb_beamr_lib_register_global_natives(&symbols_struct) ==
           HB_BEAMR_LIB_SUCCESS);

    // ------------------------------------------------------------------
    // Spawn worker thread running the Wasm module.
    // ------------------------------------------------------------------
    pthread_t worker;
    pthread_create(&worker, NULL, worker_thread, &actx);

    // ------------------------------------------------------------------
    // Wait for import request from worker.
    // ------------------------------------------------------------------
    pthread_mutex_lock(&actx.mutex);
    while (!actx.has_request) {
        pthread_cond_wait(&actx.cond_request, &actx.mutex);
    }
    int requested_index = actx.request_idx;
    actx.has_request = 0; // reset
    pthread_mutex_unlock(&actx.mutex);

    LOG("Main thread: received request for index %d", requested_index);

    // Simulate delay (e.g., external computation) before responding.
    usleep(10 * 1000); // 10ms

    int xor_val = fib(requested_index + 10); // simple deterministic value

    LOG("Main thread: providing xor_val = %d", xor_val);

    pthread_mutex_lock(&actx.mutex);
    actx.xor_val      = xor_val;
    actx.has_response = 1;
    pthread_cond_signal(&actx.cond_response);
    pthread_mutex_unlock(&actx.mutex);

    // ------------------------------------------------------------------
    // Wait for worker to finish and gather the result.
    // ------------------------------------------------------------------
    pthread_join(worker, NULL);
    int expected = 100 ^ xor_val;
    LOG("Worker returned %d, expected %d", actx.final_result, expected);
    assert(actx.final_result == expected);

    // ------------------------------------------------------------------
    // Cleanup
    // ------------------------------------------------------------------
    pthread_cond_destroy(&actx.cond_request);
    pthread_cond_destroy(&actx.cond_response);
    pthread_mutex_destroy(&actx.mutex);
    hb_beamr_lib_destroy_runtime_global();

    printf("async_import_test PASSED\n");
    return 0;
}
