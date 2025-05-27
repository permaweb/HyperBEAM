#include "hb_beamr_lib.h"
#include "utils.h"
#include "async_queue.h" // Contains queue type defs and extern declarations
#include "wasm_export.h" // For WAMR API types and functions

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>


#define LOG_STDERR_FLUSH(format, ...) \
    do { \
        fprintf(stderr, "[:%d] " format "\n", __LINE__, ##__VA_ARGS__); \
        fflush(stderr); \
    } while (0)


// // Simple lock-free-ish ring buffer for commands/events (single producer single consumer)
// #define QUEUE_CAP 64 // Now in async_queue.h

// // Command types are now in async_queue.h
// typedef enum {
// CMD_LOAD_MODULE,
// CMD_INSTANTIATE,
// CMD_CALL_EXPORT, // Consolidated from CMD_CALL_FIB and CMD_CALL_ADD_TWO
// CMD_QUIT,
// CMD_REGISTER_NATIVES
// } command_kind_t;
// typedef struct { 
//     command_kind_t kind; 
//     int arg; // module_id for LOAD/REGISTER, value for CALL_EXPORT
//     const char* func_name; // for CMD_CALL_EXPORT
// } command_t;

// // Event types are now in async_queue.h
// typedef enum {
// EVT_OK,
// EVT_RESULT,
// EVT_ERROR,
// // EVT_IMPORT_CALL, // Future use
// // EVT_IMPORT_RESULT // Future use
// } event_kind_t;
// typedef struct { event_kind_t kind; int value; } event_t;

// Command queue and its operations are extern and implemented in async_queue.c
// Event queue and its operations are extern and implemented in async_queue.c
// Blocking import helpers are extern and implemented in async_queue.c

static uint8_t *g_fib_bytes = NULL;
static uint32_t g_fib_size = 0;

static uint8_t *g_import_bytes = NULL;
static uint32_t g_import_size = 0;

static uint8_t *g_nested_import_bytes = NULL;
static uint32_t g_nested_import_size = 0;

// host_add_one native
static void native_host_add_one(wasm_exec_env_t exec_env, uint64_t *args) {
    int32_t val = (int32_t)args[0];
    args[0] = (uint64_t)(val + 1);
}

// native_give_host_control for import_nested module
static void native_give_host_control(wasm_exec_env_t exec_env, uint64_t *args) {
    unsigned int current_index_from_wasm = (unsigned int)args[0];

    wasm_module_inst_t module_inst_from_exec_env = wasm_runtime_get_module_inst(exec_env);
    if (!module_inst_from_exec_env) {
        fprintf(stderr, "[!] native_give_host_control: wasm_runtime_get_module_inst failed!\n");
        return;
    }
    hb_beamr_lib_context_t *lib_ctx = (hb_beamr_lib_context_t *)wasm_runtime_get_custom_data(module_inst_from_exec_env);

    if (!lib_ctx) {
        fprintf(stderr, "[!] native_give_host_control: lib_ctx (from custom_data) is NULL!\n");
        return;
    }

    wasm_module_inst_t module_inst = module_inst_from_exec_env;
    if (!module_inst) {
        fprintf(stderr, "[!] native_give_host_control: retrieved module_inst is NULL!\n");
        return;
    }

    LOG_STDERR_FLUSH("NATIVE_GHC: Entry. index_from_wasm=%u", current_index_from_wasm);
    push_evt((event_t){EVT_IMPORT_CALL, (int)current_index_from_wasm});

    wait_for_import_signal();

    LOG_STDERR_FLUSH("NATIVE_GHC: Master signaled.");

    int value_from_master = get_pending_import_value();

    LOG_STDERR_FLUSH("NATIVE_GHC: index_from_wasm=%u, val_from_master (this is xor_val)=%d", 
            current_index_from_wasm, value_from_master);

    // Call Wasm export "xor_memory" using hb_beamr_lib_call_export.
    // lib_ctx is obtained from custom_data and its exec_env should be valid here.
    LOG_STDERR_FLUSH("NATIVE_GHC: Calling Wasm's xor_memory(index=%u, xor_val=%d)", 
            current_index_from_wasm, value_from_master);

    wasm_val_t xor_call_args[2];
    xor_call_args[0].kind = WASM_I32;
    xor_call_args[0].of.i32 = current_index_from_wasm;
    xor_call_args[1].kind = WASM_I32;
    xor_call_args[1].of.i32 = value_from_master; // This is the xor_val

    hb_beamr_lib_rc_t rc_xor = hb_beamr_lib_call_export(lib_ctx, "xor_memory", 2, xor_call_args, 0, NULL);

    if (rc_xor != HB_BEAMR_LIB_SUCCESS) {
        LOG_STDERR_FLUSH("[!] NATIVE_GHC: Call to Wasm export 'xor_memory' failed: %d. Error: %s", 
                rc_xor, hb_beamr_lib_get_last_error(lib_ctx));
        // Consider setting a WAMR exception if appropriate, though the import is void.
        // wasm_runtime_set_exception(module_inst, "Host failed to call xor_memory");
    } else {
        LOG_STDERR_FLUSH("NATIVE_GHC: Successfully called Wasm export 'xor_memory' on index %u with xor_val %d.", 
                current_index_from_wasm, value_from_master);
    }
    LOG_STDERR_FLUSH("NATIVE_GHC: Exiting.");
    // This import is void, Wasm will read the (now modified) memory itself.
}

// Keep separate context per module id, allowing the slave to be otherwise
// "stateless" about which context is currently active.
static void *slave_thread(void *arg) {
    (void)arg;
    wasm_runtime_init_thread_env();

    while (1) {
        command_t cmd = pop_cmd();
        if (cmd.kind == CMD_QUIT) break;

        switch (cmd.kind) {
            case CMD_LOAD_MODULE: {
                hb_beamr_lib_context_t *ctx_local = cmd.ctx;
                LOG_STDERR_FLUSH("SLAVE: CMD_LOAD_MODULE (module_id %d) ctx=%p", cmd.arg, (void*)ctx_local);
                if (!ctx_local) { push_evt((event_t){EVT_ERROR, HB_BEAMR_LIB_ERROR_INVALID_ARGS}); break; }
                uint8_t* bytes = NULL;
                uint32_t size = 0;
                if (cmd.arg == 0) { bytes = g_fib_bytes; size = g_fib_size; }
                else if (cmd.arg == 1) { bytes = g_import_bytes; size = g_import_size; }
                else if (cmd.arg == 2) { bytes = g_nested_import_bytes; size = g_nested_import_size; }
                hb_beamr_lib_rc_t rc = hb_beamr_lib_load_wasm_module(ctx_local, bytes, size);
                push_evt((event_t){ rc == HB_BEAMR_LIB_SUCCESS ? EVT_OK : EVT_ERROR, rc });
                break; }
            case CMD_INSTANTIATE: {
                hb_beamr_lib_context_t *ctx_local = cmd.ctx; if (!ctx_local){ push_evt((event_t){EVT_ERROR, HB_BEAMR_LIB_ERROR_INVALID_STATE}); break; }
                LOG_STDERR_FLUSH("SLAVE: CMD_INSTANTIATE ctx=%p", (void*)ctx_local);
                hb_beamr_lib_rc_t rc = hb_beamr_lib_instantiate(ctx_local, 128*1024, 0);
                push_evt((event_t){ rc == HB_BEAMR_LIB_SUCCESS ? EVT_OK : EVT_ERROR, rc });
                break; }
            case CMD_REGISTER_NATIVES: {
                hb_beamr_lib_rc_t rc = HB_BEAMR_LIB_ERROR_INVALID_ARGS;
                if (cmd.arg == 1) {
                    const hb_beamr_native_symbol_t sym_arr[] = {{"env", "host_add_one", (void*)native_host_add_one, "(i)i", NULL}};
                    hb_beamr_native_symbol_group_t group_env = {"env", sym_arr, sizeof(sym_arr)/sizeof(sym_arr[0])};
                    hb_beamr_native_symbols_structured_t symbols_structured = {&group_env, 1};
                    rc = hb_beamr_lib_register_global_natives(&symbols_structured);
                } else if (cmd.arg == 2) {
                    const hb_beamr_native_symbol_t sym_arr_nested[] = {{"env", "give_host_control", (void*)native_give_host_control, "(i)", NULL}};
                    hb_beamr_native_symbol_group_t group_env_nested = {"env", sym_arr_nested, sizeof(sym_arr_nested)/sizeof(sym_arr_nested[0])};
                    hb_beamr_native_symbols_structured_t symbols_structured_nested = {&group_env_nested, 1};
                    rc = hb_beamr_lib_register_global_natives(&symbols_structured_nested);
                }
                push_evt((event_t){ rc == HB_BEAMR_LIB_SUCCESS ? EVT_OK : EVT_ERROR, rc});
                break; }
            case CMD_CALL_EXPORT: {
                hb_beamr_lib_context_t *ctx_local = cmd.ctx; if (!ctx_local){ push_evt((event_t){EVT_ERROR, HB_BEAMR_LIB_ERROR_INVALID_STATE}); break; }
                int val_arg = cmd.arg;
                wasm_val_t args[1] = { {.kind=WASM_I32,.of.i32=val_arg} };
                wasm_val_t results[1];
                hb_beamr_lib_rc_t rc = hb_beamr_lib_call_export(ctx_local, cmd.func_name, 1, args, 1, results);
                if(rc==HB_BEAMR_LIB_SUCCESS)
                    push_evt((event_t){EVT_RESULT,results[0].of.i32});
                else push_evt((event_t){EVT_ERROR,rc});
                break; }
            case CMD_CALL_NESTED_IMPORT_EXPORT: {
                hb_beamr_lib_context_t *ctx_local = cmd.ctx; if (!ctx_local){ push_evt((event_t){EVT_ERROR, HB_BEAMR_LIB_ERROR_INVALID_STATE}); break; }
                int init_val = cmd.arg;
                wasm_val_t call_args[2]; call_args[0].kind = WASM_I32; call_args[0].of.i32 = 0; call_args[1].kind = WASM_I32; call_args[1].of.i32 = init_val;
                wasm_val_t call_results[1];
                hb_beamr_lib_rc_t rc_call = hb_beamr_lib_call_export(ctx_local, cmd.func_name, 2, call_args, 1, call_results);
                if (rc_call == HB_BEAMR_LIB_SUCCESS)
                    push_evt((event_t){EVT_RESULT, call_results[0].of.i32});
                else push_evt((event_t){EVT_ERROR, rc_call});
                break; }
            default: break;
        }
    }

    wasm_runtime_destroy_thread_env();
    return NULL;
}

int fib_expected(int n) { int a=0,b=1; for(int i=0;i<n;i++){int t=a+b;a=b;b=t;} return a; }

int main() {
    g_fib_bytes = read_file_to_buffer("basic_fib.wasm", &g_fib_size);
    g_import_bytes = read_file_to_buffer("import_test_module.wasm", &g_import_size);
    g_nested_import_bytes = read_file_to_buffer("import_nested.wasm", &g_nested_import_size);
    assert(g_fib_bytes && g_import_bytes && g_nested_import_bytes);
    LOG_STDERR_FLUSH("MASTER: All Wasm fixture bytes loaded.");
    assert(hb_beamr_lib_init_runtime_global(NULL) == HB_BEAMR_LIB_SUCCESS);
    LOG_STDERR_FLUSH("MASTER: Runtime initialized.");

    pthread_t slave;
    pthread_create(&slave, NULL, slave_thread, NULL);
    LOG_STDERR_FLUSH("MASTER: Slave thread created.");

    // Phase: load fib module
    hb_beamr_lib_context_t *ctx_fib    = hb_beamr_lib_create_context();
    hb_beamr_lib_context_t *ctx_import = hb_beamr_lib_create_context();
    hb_beamr_lib_context_t *ctx_nested = hb_beamr_lib_create_context();
    push_cmd((command_t){CMD_LOAD_MODULE, ctx_fib, 0, "basic_fib.wasm"});
    event_t e = pop_evt();
    LOG_STDERR_FLUSH("MASTER: Load fib module event kind: %d, val: %d", e.kind, e.value);
    assert(e.kind == EVT_OK);

    // Instantiate fib module
    push_cmd((command_t){CMD_INSTANTIATE, ctx_fib, 0, NULL});
    e = pop_evt();
    LOG_STDERR_FLUSH("MASTER: Instantiate fib module event kind: %d, val: %d", e.kind, e.value);
    assert(e.kind == EVT_OK);

    // Call fib with various numbers
    for (int n=3; n<10; n++) {
        push_cmd((command_t){CMD_CALL_EXPORT, ctx_fib, n, "fib"});
        e = pop_evt();
        LOG_STDERR_FLUSH("MASTER: Call fib(%d) event kind: %d, val: %d. Expected: %d", n, e.kind, e.value, fib_expected(n));
        assert(e.kind == EVT_RESULT && e.value == fib_expected(n));
    }

    // Now exercise import module path
    // Register natives FIRST
    push_cmd((command_t){CMD_REGISTER_NATIVES, NULL, 1, NULL});
    e = pop_evt(); assert(e.kind==EVT_OK);
    LOG_STDERR_FLUSH("MASTER: Register host_add_one natives event kind: %d, val: %d", e.kind, e.value);

    // Then load the module that needs them
    push_cmd((command_t){CMD_LOAD_MODULE, ctx_import, 1, "import_test_module.wasm"});
    e = pop_evt(); assert(e.kind==EVT_OK);
    LOG_STDERR_FLUSH("MASTER: Load import_test_module event kind: %d, val: %d", e.kind, e.value);

    // Instantiate the import module
    push_cmd((command_t){CMD_INSTANTIATE, ctx_import, 0, NULL});
    e = pop_evt(); assert(e.kind==EVT_OK);
    LOG_STDERR_FLUSH("MASTER: Instantiate import_test_module event kind: %d, val: %d", e.kind, e.value);

    // Call the exported function that uses imports
    for(int v=1; v<=5; v++){
        push_cmd((command_t){CMD_CALL_EXPORT, ctx_import, v, "wasm_add_two_via_host"});
        e = pop_evt();
        LOG_STDERR_FLUSH("MASTER: Call wasm_add_two_via_host(%d) event kind: %d, val: %d. Expected: %d", v, e.kind, e.value, v+2);
        assert(e.kind==EVT_RESULT && e.value==v+2);
    }

    // Phase: Test blocking import with import_nested.wasm
    push_cmd((command_t){CMD_REGISTER_NATIVES, NULL, 2, NULL});
    e = pop_evt(); assert(e.kind == EVT_OK);
    LOG_STDERR_FLUSH("MASTER: Register give_host_control natives event kind: %d, val: %d", e.kind, e.value);

    push_cmd((command_t){CMD_LOAD_MODULE, ctx_nested, 2, "import_nested.wasm"});
    e = pop_evt(); assert(e.kind == EVT_OK);
    LOG_STDERR_FLUSH("MASTER: Load import_nested event kind: %d, val: %d", e.kind, e.value);

    push_cmd((command_t){CMD_INSTANTIATE, ctx_nested, 0, NULL});
    e = pop_evt(); assert(e.kind == EVT_OK);
    LOG_STDERR_FLUSH("MASTER: Instantiate import_nested event kind: %d, val: %d", e.kind, e.value);

    // Call call_host_and_read(index=0, init_val=100)
    // Master will receive EVT_IMPORT_CALL with value=0 (the index)
    // Master will then: simulate fib(0+10)=fib(10)=55. xor_val = 55.
    // Master sets pending_import_value = 55 (this is the xor_val for xor_memory).
    // Slave's give_host_control calls xor_memory(0, 55). Wasm memory[0] was 100, becomes 100^55=83.
    // Slave unblocks. Wasm's call_host_and_read returns memory[0] which is 83.
    int test_init_val = 100;
    int test_index = 0; 
    push_cmd((command_t){CMD_CALL_NESTED_IMPORT_EXPORT, ctx_nested, test_init_val, "call_host_and_read"});
    LOG_STDERR_FLUSH("MASTER: Sent CMD_CALL_NESTED_IMPORT_EXPORT with init_val %d", test_init_val);

    // Slave calls give_host_control(test_index), master gets this event
    e = pop_evt(); 
    LOG_STDERR_FLUSH("MASTER: Received event from slave for import_call: kind=%d, val(index)=%d", e.kind, e.value);
    assert(e.kind == EVT_IMPORT_CALL && e.value == test_index);

    // Master: Calculate xor_val (fib_res) to be used by Wasm's xor_memory
    int fib_arg = e.value + 10; // e.value is current_index from give_host_control
    int fib_res_xor_val = fib_expected(fib_arg); 
    LOG_STDERR_FLUSH("MASTER: Calculated fib_res_xor_val = %d (for fib(%d)). Signalling slave.", fib_res_xor_val, fib_arg);

    // Master: Unblock slave by providing the xor_val (fib_res_xor_val)
    set_pending_import_value(fib_res_xor_val); 
    // No direct event push from master for this, slave's import handler will complete its own event.
    // We can log that master *would* have pushed EVT_IMPORT_RESULT if it were sending data back via that queue.
    LOG_STDERR_FLUSH("MASTER: Called set_pending_import_value(%d) and signaled slave.", fib_res_xor_val);

    // Calculate the final expected result on the master side for assertion
    // init_val (100) was written by Wasm. xor_memory in Wasm will use fib_res_xor_val (55).
    // So expected final value is 100 ^ 55 = 83.
    int host_modified_val = test_init_val ^ fib_res_xor_val; 
    LOG_STDERR_FLUSH("MASTER: Expecting final result from call_host_and_read to be %d (init_val %d ^ xor_val %d)", 
                     host_modified_val, test_init_val, fib_res_xor_val);

    // Slave: call_host_and_read returns, master gets final result
    e = pop_evt(); 
    LOG_STDERR_FLUSH("MASTER: Received final event from slave: kind=%d, val(result)=%d. Expected value: %d", 
                     e.kind, e.value, host_modified_val);
    assert(e.kind == EVT_RESULT && e.value == host_modified_val);

    push_cmd((command_t){CMD_QUIT, NULL, 0, NULL});
    pthread_join(slave,NULL);
    LOG_STDERR_FLUSH("MASTER: Slave thread joined.");

    // Clean up contexts in master BEFORE destroying the runtime to avoid
    // "memory hasn't been initialize" warnings from WAMR.
    hb_beamr_lib_destroy_context(ctx_fib);
    hb_beamr_lib_destroy_context(ctx_import);
    hb_beamr_lib_destroy_context(ctx_nested);

    hb_beamr_lib_destroy_runtime_global();
    free_buffer(g_fib_bytes);
    free_buffer(g_import_bytes);
    free_buffer(g_nested_import_bytes);
    LOG_STDERR_FLUSH("MASTER: Test PASSED.");
    printf("asynchronous_comms_test PASSED\n");

    return 0;
} 