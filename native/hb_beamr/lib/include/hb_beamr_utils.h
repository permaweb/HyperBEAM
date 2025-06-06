#include "wasm_export.h"

void hb_beamr_utils_print_wasm_vals(const wasm_val_t* vals, const int val_count);
void hb_beamr_utils_print_wasm_val_kinds(const wasm_valkind_t* vals, const int val_count);
void hb_beamr_utils_print_module_info(wasm_module_t wasm_module);