#include "hb_beamr_fsm.h"
#include "utils.h"
#include <assert.h>
#include <stdio.h>

static int fib_expected(int n) { int a=0,b=1; for(int i=0;i<n;i++){int t=a+b;a=b;b=t;} return a; }

int main(void) {
    // Load fixture
    uint32_t wasm_size=0;
    uint8_t *wasm_buf = read_file_to_buffer("basic_fib.wasm", &wasm_size);
    assert(wasm_buf && wasm_size > 0);

    // Init runtime
    assert(hb_beamr_lib_init_runtime_global(NULL) == HB_BEAMR_LIB_SUCCESS);

    hb_beamr_lib_context_t *ctx = hb_beamr_lib_create_context();
    assert(ctx);

    hb_beamr_fsm_t fsm; hb_beamr_fsm_init(&fsm);

    assert(hb_beamr_fsm_load_module(&fsm, ctx, wasm_buf, wasm_size) == HB_BEAMR_LIB_SUCCESS);
    assert(hb_beamr_fsm_instantiate(&fsm, ctx, 128*1024, 0) == HB_BEAMR_LIB_SUCCESS);

    wasm_val_t arg, res;
    arg.kind = WASM_I32; res.kind = WASM_I32;
    int n=8; arg.of.i32 = n;
    assert(hb_beamr_fsm_call_export(&fsm, ctx, "fib", 1, &arg, 1, &res) == HB_BEAMR_LIB_SUCCESS);
    printf("fib(%d) = %d\n", n, res.of.i32);
    assert(res.of.i32 == fib_expected(n));

    assert(hb_beamr_fsm_quit(&fsm, ctx) == HB_BEAMR_LIB_SUCCESS);

    hb_beamr_lib_destroy_runtime_global();
    free_buffer(wasm_buf);
    printf("fsm_test PASSED\n");
    return 0;
}
