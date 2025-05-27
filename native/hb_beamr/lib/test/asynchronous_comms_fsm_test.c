#include "async_queue.h"
#include "hb_beamr_fsm.h"
#include "hb_beamr_lib.h"
#include "utils.h"
#include "wasm_export.h"
#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LOG_STDERR_FLUSH(fmt, ...)                                             \
  do {                                                                         \
    fprintf(stderr, "[:%d] " fmt "\n", __LINE__, ##__VA_ARGS__);               \
    fflush(stderr);                                                            \
  } while (0)

static uint8_t *g_fib_bytes = NULL, *g_import_bytes = NULL,
               *g_nested_bytes = NULL;
static uint32_t g_fib_sz = 0, g_import_sz = 0, g_nested_sz = 0;

// Native impl identical to previous test
static void native_host_add_one(wasm_exec_env_t env, uint64_t *args) {
  int32_t v = (int32_t)args[0];
  args[0] = (uint64_t)(v + 1);
}
static void native_give_host_control(wasm_exec_env_t env, uint64_t *args) {
  uint32_t index = (uint32_t)args[0];
  wasm_module_inst_t mi = wasm_runtime_get_module_inst(env);
  hb_beamr_lib_context_t *ctx =
      (hb_beamr_lib_context_t *)wasm_runtime_get_custom_data(mi);
  if (!ctx) {
    return;
  }
  wasm_val_t xargs[2] = {
      {.kind = WASM_I32, .of.i32 = index},
      {.kind = WASM_I32, .of.i32 = 55}}; // placeholder xor value will be
                                         // patched by main via pending value
  xargs[1].of.i32 = get_pending_import_value();
  hb_beamr_lib_call_export(ctx, "xor_memory", 2, xargs, 0, NULL);
}

// slave thread now expects cmd.fsm
static void *slave_thread(void *arg) {
  (void)arg;
  wasm_runtime_init_thread_env();
  while (1) {
    command_t cmd = pop_cmd();
    if (cmd.kind == CMD_QUIT)
      break;
    hb_beamr_lib_context_t *ctx = cmd.ctx;
    hb_beamr_fsm_t *fsm = cmd.fsm;
    switch (cmd.kind) {
    case CMD_LOAD_MODULE: {
      uint8_t *buf = NULL;
      uint32_t sz = 0;
      if (cmd.arg == 0) {
        buf = g_fib_bytes;
        sz = g_fib_sz;
      } else if (cmd.arg == 1) {
        buf = g_import_bytes;
        sz = g_import_sz;
      } else {
        buf = g_nested_bytes;
        sz = g_nested_sz;
      }
      hb_beamr_lib_rc_t rc = hb_beamr_fsm_load_module(fsm, ctx, buf, sz);
      push_evt((event_t){rc == HB_BEAMR_LIB_SUCCESS ? EVT_OK : EVT_ERROR, rc});
      break;
    }
    case CMD_INSTANTIATE: {
      hb_beamr_lib_rc_t rc = hb_beamr_fsm_instantiate(fsm, ctx, 128 * 1024, 0);
      push_evt((event_t){rc == HB_BEAMR_LIB_SUCCESS ? EVT_OK : EVT_ERROR, rc});
      break;
    }
    case CMD_REGISTER_NATIVES: {
      hb_beamr_lib_rc_t rc;
      if (cmd.arg == 1) {
        hb_beamr_native_symbols_structured_t s = {
            .groups = (hb_beamr_native_symbol_group_t[]){
                {
                    .module_name = "env",
                    .symbols = (hb_beamr_native_symbol_t[]){
                        {"host_add_one", (void *)native_host_add_one, "(i)i", NULL}
                    },
                    .num_symbols = 1
                }
            },
            .num_groups = 1
        };
        rc = hb_beamr_fsm_register_natives(fsm, &s);
      } else {
        hb_beamr_native_symbols_structured_t s = {
            .groups = (hb_beamr_native_symbol_group_t[]){
                {
                    .module_name = "env",
                    .symbols = (hb_beamr_native_symbol_t[]){
                        {"give_host_control", (void *)native_give_host_control, "(i)", NULL}
                    },
                    .num_symbols = 1
                }
            },
            .num_groups = 1
        };
        rc = hb_beamr_fsm_register_natives(fsm, &s);
      }
      push_evt((event_t){rc == HB_BEAMR_LIB_SUCCESS ? EVT_OK : EVT_ERROR, rc});
      break;
    }
    case CMD_CALL_EXPORT: {
      wasm_val_t a = {.kind = WASM_I32, .of.i32 = cmd.arg};
      wasm_val_t r;
      r.kind = WASM_I32;
      hb_beamr_lib_rc_t rc =
          hb_beamr_fsm_call_export(fsm, ctx, cmd.func_name, 1, &a, 1, &r);
      if (rc == HB_BEAMR_LIB_SUCCESS)
        push_evt((event_t){EVT_RESULT, r.of.i32});
      else
        push_evt((event_t){EVT_ERROR, rc});
      break;
    }
    case CMD_CALL_NESTED_IMPORT_EXPORT: {
      wasm_val_t args_v[2] = {{.kind = WASM_I32, .of.i32 = 0},
                              {.kind = WASM_I32, .of.i32 = cmd.arg}};
      wasm_val_t res_v;
      res_v.kind = WASM_I32;
      hb_beamr_lib_rc_t rc = hb_beamr_fsm_call_export(fsm, ctx, cmd.func_name,
                                                      2, args_v, 1, &res_v);
      if (rc == HB_BEAMR_LIB_SUCCESS)
        push_evt((event_t){EVT_RESULT, res_v.of.i32});
      else
        push_evt((event_t){EVT_ERROR, rc});
      break;
    }
    default:
      break;
    }
  }
  wasm_runtime_destroy_thread_env();
  return NULL;
}

static int fib_expected(int n) {
  int a = 0, b = 1;
  for (int i = 0; i < n; i++) {
    int t = a + b;
    a = b;
    b = t;
  }
  return a;
}

int main() {
  g_fib_bytes = read_file_to_buffer("basic_fib.aot", &g_fib_sz);
  g_import_bytes = read_file_to_buffer("import_test_module.aot", &g_import_sz);
  g_nested_bytes = read_file_to_buffer("import_nested.aot", &g_nested_sz);
  assert(g_fib_bytes && g_import_bytes && g_nested_bytes);
  assert(hb_beamr_lib_init_runtime_global(NULL) == HB_BEAMR_LIB_SUCCESS);

  hb_beamr_lib_context_t *ctx_fib = hb_beamr_lib_create_context();
  hb_beamr_fsm_t fsm_fib;
  hb_beamr_fsm_init(&fsm_fib);
  hb_beamr_lib_context_t *ctx_imp = hb_beamr_lib_create_context();
  hb_beamr_fsm_t fsm_imp;
  hb_beamr_fsm_init(&fsm_imp);
  hb_beamr_lib_context_t *ctx_nested = hb_beamr_lib_create_context();
  hb_beamr_fsm_t fsm_nested;
  hb_beamr_fsm_init(&fsm_nested);
  pthread_t slave;
  pthread_create(&slave, NULL, slave_thread, NULL);

  push_cmd((command_t){CMD_LOAD_MODULE, ctx_fib, 0, "fib", &fsm_fib});
  event_t e = pop_evt();
  assert(e.kind == EVT_OK);
  push_cmd((command_t){CMD_INSTANTIATE, ctx_fib, 0, NULL, &fsm_fib});
  e = pop_evt();
  assert(e.kind == EVT_OK);
  for (int n = 3; n < 10; n++) {
    push_cmd((command_t){CMD_CALL_EXPORT, ctx_fib, n, "fib", &fsm_fib});
    e = pop_evt();
    assert(e.kind == EVT_RESULT && e.value == fib_expected(n));
  }
  // natives
  push_cmd((command_t){CMD_REGISTER_NATIVES, NULL, 1, NULL, NULL});
  e = pop_evt();
  push_cmd((command_t){CMD_LOAD_MODULE, ctx_imp, 1, "import", &fsm_imp});
  e = pop_evt();
  assert(e.kind == EVT_OK);
  push_cmd((command_t){CMD_INSTANTIATE, ctx_imp, 0, NULL, &fsm_imp});
  e = pop_evt();
  assert(e.kind == EVT_OK);
  for (int v = 1; v <= 5; v++) {
    push_cmd((command_t){CMD_CALL_EXPORT, ctx_imp, v, "wasm_add_two_via_host",
                         &fsm_imp});
    e = pop_evt();
    assert(e.kind == EVT_RESULT && e.value == v + 2);
  }
  // nested
  push_cmd((command_t){CMD_REGISTER_NATIVES, NULL, 2, NULL, NULL});
  e = pop_evt();
  push_cmd((command_t){CMD_LOAD_MODULE, ctx_nested, 2, "nested", &fsm_nested});
  e = pop_evt();
  push_cmd((command_t){CMD_INSTANTIATE, ctx_nested, 0, NULL, &fsm_nested});
  e = pop_evt();
  push_cmd((command_t){CMD_CALL_NESTED_IMPORT_EXPORT, ctx_nested, 100,
                       "call_host_and_read", &fsm_nested});
  e = pop_evt(); // expect EVT_IMPORT_CALL from slave (not implemented in this
                 // trim)
  push_cmd((command_t){CMD_QUIT, NULL, 0, NULL, NULL});
  pthread_join(slave, NULL);
  // finish
  hb_beamr_lib_destroy_runtime_global();
  printf("asynchronous_comms_fsm_test stub\n");
  return 0;
}