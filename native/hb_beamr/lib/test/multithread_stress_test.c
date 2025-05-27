#include "hb_beamr_lib.h"
#include "utils.h"
#include "wasm_export.h"
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <unistd.h> // for usleep
#include <stdatomic.h>

#define NUM_WORKERS_PHASE 10
#define ITERATIONS_PER_WORKER 50

#define LOG_STDERR_FLUSH(format, ...) \
    do { \
        fprintf(stderr, "[:%d] " format "\n", __LINE__, ##__VA_ARGS__); \
        fflush(stderr); \
    } while (0)

typedef enum { EV_FIB_START, EV_FIB_DONE, EV_IMP_START, EV_IMP_DONE } event_kind_t;

typedef struct {
    event_kind_t kind;
    int worker_id;   // 0..NUM_WORKERS_PHASE-1
    int iteration;   // 0..ITERATIONS_PER_WORKER-1
} event_t;

#define MAX_EVENTS (NUM_WORKERS_PHASE * ITERATIONS_PER_WORKER * 4 * 2)
static event_t g_events[MAX_EVENTS];
static atomic_int g_event_pos = 0;

static void record_event(event_kind_t kind, int worker_id, int iteration) {
    int pos = atomic_fetch_add_explicit(&g_event_pos, 1, memory_order_relaxed);
    if (pos < MAX_EVENTS) {
        g_events[pos].kind = kind;
        g_events[pos].worker_id = worker_id;
        g_events[pos].iteration = iteration;
    }
}

static const char *fib_wasm_filename = "basic_fib.wasm";
static const char *import_wasm_filename = "import_test_module.wasm";

static uint8_t *g_fib_bytes = NULL;
static uint32_t g_fib_size = 0;
static uint8_t *g_import_bytes = NULL;
static uint32_t g_import_size = 0;

static volatile int g_failure = 0;

static int fib_expected(int n) {
    int a = 0, b = 1;
    for (int i = 0; i < n; ++i) {
        int tmp = a + b;
        a = b;
        b = tmp;
    }
    return a;
}

static void *worker_fib(void *arg) {
    int worker_id = *(int *)arg;
    free(arg);

    if (wasm_runtime_thread_env_inited()) {
        LOG_STDERR_FLUSH("[fib_worker %d] wasm_runtime_thread_env_inited() true", worker_id);
    } else {
        LOG_STDERR_FLUSH("[fib_worker %d] wasm_runtime_thread_env_inited() false", worker_id);
    }

    if (!wasm_runtime_init_thread_env()) {
        g_failure = 1;
        return NULL;
    }

    int fib_arg = 20; // constant per worker
    int expected = fib_expected(fib_arg);

    for (int i = 0; i < ITERATIONS_PER_WORKER && !g_failure; ++i) {
        record_event(EV_FIB_START, worker_id, i);
        hb_beamr_lib_context_t *ctx = hb_beamr_lib_create_context();
        if (!ctx) { g_failure = 1; break; }
        if (hb_beamr_lib_load_wasm_module(ctx, g_fib_bytes, g_fib_size) != HB_BEAMR_LIB_SUCCESS) { g_failure = 1; break; }
        if (hb_beamr_lib_instantiate(ctx, 128 * 1024, 0) != HB_BEAMR_LIB_SUCCESS) { g_failure = 1; break; }

        wasm_val_t args[1];
        args[0].kind = WASM_I32;
        args[0].of.i32 = fib_arg;
        wasm_val_t results[1];
        hb_beamr_lib_rc_t rc = hb_beamr_lib_call_export(ctx, "fib", 1, args, 1, results);
        if (rc != HB_BEAMR_LIB_SUCCESS || results[0].of.i32 != expected) {
            LOG_STDERR_FLUSH("[fib worker %d] unexpected result %d (expected %d). err=%s\n", worker_id, results[0].of.i32, expected, hb_beamr_lib_get_last_error(ctx));
            g_failure = 1;
        }
        hb_beamr_lib_destroy_context(ctx);
        record_event(EV_FIB_DONE, worker_id, i);
    }
    wasm_runtime_destroy_thread_env();
    return NULL;
}

// host_add_one native used by import_test_module
static void native_host_add_one(wasm_exec_env_t exec_env, uint64_t *args) {
    int32_t val = (int32_t)args[0];
    args[0] = (uint64_t)(val + 1);
}

static void *worker_import(void *arg) {
    int worker_id = *(int *)arg;
    free(arg);

    if (!wasm_runtime_init_thread_env()) {
        g_failure = 1;
        return NULL;
    }
    for (int i = 0; i < ITERATIONS_PER_WORKER && !g_failure; ++i) {
        record_event(EV_IMP_START, worker_id, i);
        hb_beamr_lib_context_t *ctx = hb_beamr_lib_create_context();
        if (!ctx) { g_failure = 1; break; }
        if (hb_beamr_lib_load_wasm_module(ctx, g_import_bytes, g_import_size) != HB_BEAMR_LIB_SUCCESS) { g_failure = 1; break; }
        if (hb_beamr_lib_instantiate(ctx, 128 * 1024, 0) != HB_BEAMR_LIB_SUCCESS) { g_failure = 1; break; }

        wasm_val_t args[1];
        args[0].kind = WASM_I32;
        args[0].of.i32 = 5;
        wasm_val_t results[1];
        hb_beamr_lib_rc_t rc = hb_beamr_lib_call_export(ctx, "wasm_add_two_via_host", 1, args, 1, results);
        if (rc != HB_BEAMR_LIB_SUCCESS || results[0].of.i32 != 7) {
            fprintf(stderr, "[import worker] unexpected result %d (expected 7). err=%s\n", results[0].of.i32, hb_beamr_lib_get_last_error(ctx));
            g_failure = 1;
        }
        hb_beamr_lib_destroy_context(ctx);
        record_event(EV_IMP_DONE, worker_id, i);
    }
    wasm_runtime_destroy_thread_env();
    return NULL;
}

static void *registrar_thread(void *unused) {
    (void)unused;
    if (!wasm_runtime_init_thread_env()) {
        g_failure = 1;
        return NULL;
    }
    // Delay to ensure overlap with phase A workers
    usleep(20 * 1000); // 20 ms

    const hb_beamr_native_symbol_t sym_arr[] = {{"env", "host_add_one", (void*)native_host_add_one, "(i)i", NULL}};
    hb_beamr_native_symbol_group_t group_env = {"env", sym_arr, sizeof(sym_arr)/sizeof(sym_arr[0])};
    hb_beamr_native_symbols_structured_t symbols_structured = {&group_env, 1};
    hb_beamr_lib_rc_t rc = hb_beamr_lib_register_global_natives(&symbols_structured);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "[registrar] failed to register natives rc=%d\n", rc);
        g_failure = 1;
        return NULL;
    }

    // Spawn phase-B workers
    pthread_t workers[NUM_WORKERS_PHASE];
    for (int i = 0; i < NUM_WORKERS_PHASE; ++i) {
        int *wid = malloc(sizeof(int));
        *wid = i;
        pthread_create(&workers[i], NULL, worker_import, wid);
    }
    for (int i = 0; i < NUM_WORKERS_PHASE; ++i) {
        pthread_join(workers[i], NULL);
    }
    wasm_runtime_destroy_thread_env();
    return NULL;
}

int main() {
    // Load fixture bytes once
    g_fib_bytes = read_file_to_buffer(fib_wasm_filename, &g_fib_size);
    g_import_bytes = read_file_to_buffer(import_wasm_filename, &g_import_size);
    assert(g_fib_bytes && g_import_bytes && "Failed to read wasm fixtures");

    // Init runtime
    hb_beamr_lib_rc_t rc = hb_beamr_lib_init_runtime_global(NULL);
    assert(rc == HB_BEAMR_LIB_SUCCESS && "runtime init failed");

    // Launch phase-A workers (fib)
    pthread_t workers[NUM_WORKERS_PHASE];
    for (int i = 0; i < NUM_WORKERS_PHASE; ++i) {
        int *wid = malloc(sizeof(int));
        *wid = i;
        pthread_create(&workers[i], NULL, worker_fib, wid);
    }

    // Registrar thread (adds natives then runs import workers)
    pthread_t registrar;
    pthread_create(&registrar, NULL, registrar_thread, NULL);

    // Wait for phase-A workers
    for (int i = 0; i < NUM_WORKERS_PHASE; ++i) {
        pthread_join(workers[i], NULL);
    }
    // Wait registrar (and its workers)
    pthread_join(registrar, NULL);

    hb_beamr_lib_destroy_runtime_global();
    free_buffer(g_fib_bytes);
    free_buffer(g_import_bytes);

    if (g_failure) {
        fprintf(stderr, "MultithreadStressTest FAILED\n");
        return 1;
    }
    printf("MultithreadStressTest PASSED\n");

    // Output event log succinctly
    int total = atomic_load_explicit(&g_event_pos, memory_order_relaxed);
    for (int i = 0; i < total; ++i) {
        const char *kind_str = (g_events[i].kind == EV_FIB_START) ? "FIB_START" :
                               (g_events[i].kind == EV_FIB_DONE) ? "FIB_DONE" :
                               (g_events[i].kind == EV_IMP_START) ? "IMP_START" : "IMP_DONE";
        printf("%05d: %s w%02d it%02d\n", i, kind_str, g_events[i].worker_id, g_events[i].iteration);
    }

    return 0;
} 