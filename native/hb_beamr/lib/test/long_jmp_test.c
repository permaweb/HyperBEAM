#include "hb_beamr_lib.h"
#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "wasm_export.h"

#define LOG_STDERR_FLUSH(format, ...) \
    do { \
        fprintf(stderr, "[:%d] " format "\n", __LINE__, ##__VA_ARGS__); \
        fflush(stderr); \
    } while (0)


/*
 * Test description:
 * ------------------
 * This test exercises hb_beamr_lib against a Wasm module that makes use of
 * C setjmp/longjmp.  The module is produced from fixtures/jmp.cpp and exports
 * a single C function `funcs()` which returns a pointer to a global string
 * buffer.  Internally `funcs()` will perform a `setjmp`, dive five recursion
 * levels deep and then trigger a `longjmp`.  After the jump the buffer will
 * contain the following text:
 *     "last_level: 5, ret: 42, local_state: 7"
 * The test loads the compiled AOT artifact (jmp.aot), instantiates it, calls
 * `funcs`, reads the returned string back out of the module linear memory and
 * validates the contents.
 */

static char *read_wasm_string(hb_beamr_lib_context_t *ctx,
                              uint32_t offset,
                              size_t max_len) {
    if (offset == 0) {
        return NULL;
    }

    uint8_t *mem_base = NULL;
    size_t   mem_size = 0;
    wasm_memory_inst_t mem_inst = NULL;
    hb_beamr_lib_rc_t rc = hb_beamr_lib_get_memory_info(ctx, NULL,
                                                        &mem_base,
                                                        &mem_size,
                                                        &mem_inst);
    if (rc != HB_BEAMR_LIB_SUCCESS || !mem_base) {
        fprintf(stderr, "read_wasm_string: get_memory_info failed – %s\n",
                hb_beamr_lib_get_last_error(ctx));
        return NULL;
    }

    if (offset >= mem_size) {
        fprintf(stderr, "read_wasm_string: offset out of bounds (%u >= %zu)\n",
                offset, mem_size);
        return NULL;
    }

    const uint8_t *src = mem_base + offset;
    size_t actual_len = 0;
    while (actual_len < max_len && (offset + actual_len) < mem_size) {
        if (src[actual_len] == '\0') {
            break;
        }
        actual_len++;
    }

    char *out = (char *)malloc(actual_len + 1);
    if (!out) {
        return NULL;
    }
    memcpy(out, src, actual_len);
    out[actual_len] = '\0';
    return out;
}

static hb_beamr_lib_rc_t perform_indirect_call_via_lib_api(
    wasm_exec_env_t exec_env,
    hb_beamr_lib_context_t* lib_ctx,
    uint32_t table_index,
    uint32_t num_args,
    wasm_val_t args[],
    uint32_t num_results,
    wasm_val_t results[]
) {
    // Directly delegate to hb_beamr_lib_call_indirect and interpret error.
    hb_beamr_lib_rc_t rc = hb_beamr_lib_call_indirect(lib_ctx,
                                                      "__indirect_function_table",
                                                      table_index,
                                                      num_args,
                                                      args,
                                                      num_results,
                                                      results);
    return rc;
}

// Added helper to map from exec_env -> module_inst -> hb_beamr_lib_context
static inline hb_beamr_lib_context_t *get_ctx_from_exec_env(wasm_exec_env_t exec_env) {
    wasm_module_inst_t inst = wasm_runtime_get_module_inst(exec_env);
    return inst ? (hb_beamr_lib_context_t *)wasm_runtime_get_custom_data(inst) : NULL;
}

// invoke_vii(index, a, b) -> void
static void native_invoke_vii(wasm_exec_env_t exec_env, uint64_t *raw_args) {
    hb_beamr_lib_context_t *ctx = get_ctx_from_exec_env(exec_env);
    LOG_STDERR_FLUSH("[CTX %p] native_invoke_vii", ctx);

    uint32_t table_index = (uint32_t)raw_args[0];

    wasm_val_t args[2];
    args[0].kind = WASM_I32;
    args[0].of.i32 = (int32_t)raw_args[1];
    args[1].kind = WASM_I32;
    args[1].of.i32 = (int32_t)raw_args[2];

    perform_indirect_call_via_lib_api(exec_env, ctx, table_index, 2, args, 0, NULL);
    // No results to return
}

// invoke_iiii(index, a, b, c) -> i32
static void native_invoke_iiii(wasm_exec_env_t exec_env, uint64_t *raw_args) {
    hb_beamr_lib_context_t *ctx = get_ctx_from_exec_env(exec_env);
    LOG_STDERR_FLUSH("native_invoke_iiii: %p", ctx);

    uint32_t table_index = (uint32_t)raw_args[0];

    wasm_val_t args[3];
    args[0].kind = WASM_I32;
    args[0].of.i32 = (int32_t)raw_args[1];
    args[1].kind = WASM_I32;
    args[1].of.i32 = (int32_t)raw_args[2];
    args[2].kind = WASM_I32;
    args[2].of.i32 = (int32_t)raw_args[3];

    wasm_val_t results[1];

    if (perform_indirect_call_via_lib_api(exec_env, ctx, table_index, 3, args, 1, results)) {
        raw_args[0] = (uint64_t)results[0].of.i32; // place i32 result back as per WAMR ABI
    }
}

// _emscripten_throw_longjmp(env_ptr, value) -> void (traps)
static void native_emscripten_throw_longjmp(wasm_exec_env_t exec_env, uint64_t *raw_args) {
    hb_beamr_lib_context_t *ctx = get_ctx_from_exec_env(exec_env);
    LOG_STDERR_FLUSH("[CTX %p] native_emscripten_throw_longjmp", ctx);
    // wasm_module_inst_t module_inst = wasm_runtime_get_module_inst(exec_env);
    // wasm_runtime_set_exception(module_inst, "_emscripten_throw_longjmp");
}

int main(void) {
    const char *aot_filename = "jmp.aot";
    printf("=== LongJmpTest: loading %s ===\n", aot_filename);

    hb_beamr_lib_rc_t rc;

    hb_beamr_lib_context_t *ctx = hb_beamr_lib_create_context();
    assert(ctx && "Context creation failed");

    rc = hb_beamr_lib_init_runtime_global(NULL);
    LOG_STDERR_FLUSH("hb_beamr_lib_init_runtime_global: %d", rc);
    assert(rc == HB_BEAMR_LIB_SUCCESS && "Runtime init failed");

    hb_beamr_native_symbol_t sjlj_symbols[] = {
        { "env", "invoke_vii", (void*)native_invoke_vii, "(iii)", NULL },
        { "env", "invoke_iiii", (void*)native_invoke_iiii, "(iiii)i", NULL },
        { "env", "_emscripten_throw_longjmp", (void*)native_emscripten_throw_longjmp, "()", NULL }
    };
    const uint32_t num_sjlj_symbols = sizeof(sjlj_symbols) / sizeof(hb_beamr_native_symbol_t);

    rc = hb_beamr_lib_register_global_natives("env", sjlj_symbols, num_sjlj_symbols);
    LOG_STDERR_FLUSH("hb_beamr_lib_register_global_natives: %d, %s", rc, hb_beamr_lib_get_last_error(ctx));
    assert(rc == HB_BEAMR_LIB_SUCCESS && "Failed to register SJLJ natives");

    uint32_t file_size = 0;
    uint8_t *file_buf = read_file_to_buffer(aot_filename, &file_size);
    assert(file_buf && file_size > 0 && "Failed to read AOT file");

    rc = hb_beamr_lib_load_aot_module(ctx, file_buf, file_size);
    assert(rc == HB_BEAMR_LIB_SUCCESS && "Module load failed");

    rc = hb_beamr_lib_instantiate(ctx, 128 * 1024, 0);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Instantiate failed: %s\n",
                hb_beamr_lib_get_last_error(ctx));
        free_buffer(file_buf);
        return 1;
    }

    // Call exported function `funcs()` which takes no parameters and
    // returns i32 (pointer offset).
    wasm_val_t results[1];

    rc = hb_beamr_lib_call_export(ctx, "funcs", 0, NULL, 1, results);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Call to funcs failed: %s\n",
                hb_beamr_lib_get_last_error(ctx));
        free_buffer(file_buf);
        return 1;
    }

    assert(results[0].kind == WASM_I32);
    uint32_t str_offset = results[0].of.i32;

    char *returned_str = read_wasm_string(ctx, str_offset, 255);
    assert(returned_str && "Failed to read returned string");

    printf("funcs() returned string: '%s'\n", returned_str);

    const char *expected = "last_level: 5, ret: 42, local_state: 7";
    assert(strcmp(returned_str, expected) == 0 && "Returned string did not match expected value");

    printf("=== LongJmpTest PASSED ===\n");

    free(returned_str);
    free_buffer(file_buf);
    hb_beamr_lib_destroy_context(ctx);
    hb_beamr_lib_destroy_runtime_global();

    return 0;
} 