#include "hb_core.h"

enum erl_port_buffer_to_wasm_vals_rc {
    ERL_PORT_BUFFER_TO_WASM_VALS_SUCCESS = 0,
    ERL_PORT_BUFFER_TO_WASM_VALS_MEMORY_ERROR = -1,
    ERL_PORT_BUFFER_TO_WASM_VALS_UNSPECIFIED_ERROR = -2,
    ERL_PORT_BUFFER_TO_WASM_VALS_INVALID_ARGUMENTS = -3,
    ERL_PORT_BUFFER_TO_WASM_VALS_MALFORMED_BUFFER = -4,
    ERL_PORT_BUFFER_TO_WASM_VALS_UNSUPPORTED_WASM_TYPE = -5,
    ERL_PORT_BUFFER_TO_WASM_VALS_VALUE_OUT_OF_RANGE = -6,
};

enum erl_port_buffer_to_wasm_vals_rc erl_port_buffer_to_wasm_vals(const char* buff, int* index, wasm_valkind_t *val_kinds, uint32_t val_count, wasm_val_t **out_vals);

enum erl_port_buffer_to_wasm_vals_rc wasm_vals_to_erl_msg(const wasm_val_t* vals, int val_count, ErlDrvTermData** msg, int* index, int msg_base_size);
