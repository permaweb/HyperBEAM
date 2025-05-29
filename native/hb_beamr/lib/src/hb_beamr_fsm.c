#include "hb_beamr_fsm.h"
#include <string.h>

void hb_beamr_fsm_init(hb_beamr_fsm_t *fsm) {
    if (fsm) {
        fsm->state = HB_BEAMR_FSM_STATE_INITIAL;
    }
}

static inline hb_beamr_lib_rc_t invalid_state(hb_beamr_fsm_t *fsm) {
    (void)fsm;
    return HB_BEAMR_LIB_ERROR_INVALID_STATE;
}

hb_beamr_lib_rc_t hb_beamr_fsm_load_module(hb_beamr_fsm_t *fsm,
                                           hb_beamr_lib_context_t *ctx,
                                           const uint8_t *binary,
                                           uint32_t binary_size) {
    if (!fsm || !ctx) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    if (fsm->state != HB_BEAMR_FSM_STATE_INITIAL)
        return invalid_state(fsm);
    hb_beamr_lib_rc_t rc = hb_beamr_lib_load_wasm_module(ctx, (uint8_t*)binary, binary_size);
    if (rc == HB_BEAMR_LIB_SUCCESS) {
        fsm->state = HB_BEAMR_FSM_STATE_MODULE_LOADED;
    }
    return rc;
}

hb_beamr_lib_rc_t hb_beamr_fsm_register_natives(hb_beamr_fsm_t *fsm,
                                                const hb_beamr_native_symbols_structured_t *syms) {
    if (!fsm) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    if (fsm->state == HB_BEAMR_FSM_STATE_FINAL)
        return invalid_state(fsm);
    return hb_beamr_lib_register_global_natives(syms);
}

hb_beamr_lib_rc_t hb_beamr_fsm_instantiate(hb_beamr_fsm_t *fsm,
                                           hb_beamr_lib_context_t *ctx,
                                           uint32_t stack_size,
                                           uint32_t heap_size) {
    if (!fsm || !ctx) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    if (fsm->state != HB_BEAMR_FSM_STATE_MODULE_LOADED)
        return invalid_state(fsm);
    hb_beamr_lib_rc_t rc = hb_beamr_lib_instantiate(ctx, stack_size, heap_size, NULL);
    if (rc == HB_BEAMR_LIB_SUCCESS) {
        fsm->state = HB_BEAMR_FSM_STATE_INSTANTIATED;
    }
    return rc;
}

hb_beamr_lib_rc_t hb_beamr_fsm_call_export(hb_beamr_fsm_t *fsm,
                                           hb_beamr_lib_context_t *ctx,
                                           const char *func_name,
                                           uint32_t num_args,
                                           wasm_val_t args[],
                                           uint32_t num_results,
                                           wasm_val_t results[]) {
    if (!fsm || !ctx) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    if (fsm->state != HB_BEAMR_FSM_STATE_INSTANTIATED)
        return invalid_state(fsm);
    return hb_beamr_lib_call_export(ctx, func_name, num_args, args, num_results, results);
}

hb_beamr_lib_rc_t hb_beamr_fsm_quit(hb_beamr_fsm_t *fsm,
                                    hb_beamr_lib_context_t *ctx) {
    if (!fsm || !ctx) return HB_BEAMR_LIB_ERROR_INVALID_ARGS;
    if (fsm->state == HB_BEAMR_FSM_STATE_FINAL)
        return invalid_state(fsm);
    // Destroy context resources – caller owns ctx lifetime but FSM can deinstantiate to keep symmetry.
    hb_beamr_lib_destroy_context(ctx);
    fsm->state = HB_BEAMR_FSM_STATE_FINAL;
    return HB_BEAMR_LIB_SUCCESS;
}
