#ifndef HB_BEAMR_FSM_H
#define HB_BEAMR_FSM_H

#include "hb_beamr_lib.h"
#include "wasm_export.h" // for wasm_val_t and types

#ifdef __cplusplus
extern "C" {
#endif

// A very generous upper-bound for parameter/result vectors when the FSM
// surfaces an IMPORT_CALL back to the host.  (Not used in the first
// implementation but reserved for future-proofing.)
#define HB_BEAMR_FSM_MAX_ARITY 256

// --- FSM State ----------------------------------------------------------------

typedef enum {
    HB_BEAMR_FSM_STATE_INITIAL = 0,      // No module loaded yet
    HB_BEAMR_FSM_STATE_MODULE_LOADED,    // ctx has a module but no instance
    HB_BEAMR_FSM_STATE_INSTANTIATED,     // ctx has an instance & exec_env
    HB_BEAMR_FSM_STATE_FINAL             // quit() called or irrecoverable
} hb_beamr_fsm_state_t;

// Opaque handle – right now just carries the state enum.  The caller owns the
// hb_beamr_lib_context_t and passes it into every step.
typedef struct {
    hb_beamr_fsm_state_t state;
} hb_beamr_fsm_t;

// --- PUBLIC API ---------------------------------------------------------------

// Initialise an FSM handle – sets state to INITIAL.
void hb_beamr_fsm_init(hb_beamr_fsm_t *fsm);

// Load a WASM/AOT module.  Valid only in INITIAL state.
hb_beamr_lib_rc_t
hb_beamr_fsm_load_module(hb_beamr_fsm_t *fsm,
                         hb_beamr_lib_context_t *ctx,
                         const uint8_t *binary,
                         uint32_t binary_size);

// Register global native symbols.  Permitted in any non-FINAL state – does not
// change the FSM state.
hb_beamr_lib_rc_t
hb_beamr_fsm_register_natives(hb_beamr_fsm_t *fsm,
                              const hb_beamr_native_symbols_structured_t *syms);

// Instantiate the previously loaded module.  Valid only in MODULE_LOADED.
hb_beamr_lib_rc_t
hb_beamr_fsm_instantiate(hb_beamr_fsm_t *fsm,
                         hb_beamr_lib_context_t *ctx,
                         uint32_t stack_size,
                         uint32_t heap_size);

// Call an exported function.  Valid only in INSTANTIATED.
hb_beamr_lib_rc_t
hb_beamr_fsm_call_export(hb_beamr_fsm_t *fsm,
                         hb_beamr_lib_context_t *ctx,
                         const char *func_name,
                         uint32_t num_args,
                         wasm_val_t args[],
                         uint32_t num_results,
                         wasm_val_t results[]);

// Finalise the FSM (destroy instance/module).  After this call the FSM moves
// to FINAL and further calls will return HB_BEAMR_LIB_ERROR_INVALID_STATE.
hb_beamr_lib_rc_t
hb_beamr_fsm_quit(hb_beamr_fsm_t *fsm,
                  hb_beamr_lib_context_t *ctx);

#ifdef __cplusplus
}
#endif

#endif // HB_BEAMR_FSM_H
