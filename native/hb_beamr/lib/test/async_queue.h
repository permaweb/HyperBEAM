#ifndef ASYNC_QUEUE_H
#define ASYNC_QUEUE_H

#include <pthread.h>
#include "hb_beamr_lib.h"
#include "hb_beamr_fsm.h"

// Command (Master -> Slave)

typedef enum {
    CMD_LOAD_MODULE,        // arg = module_id (0 for fib, 1 for import)
    CMD_INSTANTIATE,        // arg = N/A
    CMD_REGISTER_NATIVES,   // arg = module_id that natives are for
    CMD_CALL_EXPORT,        // arg = value for func (fib_n or add_x)
    CMD_QUIT,                // arg = N/A
    CMD_CALL_NESTED_IMPORT_EXPORT // arg = init_val for call_host_and_read, func_name = "call_host_and_read"
} command_kind_t;

typedef struct {
    command_kind_t kind;
    hb_beamr_lib_context_t *ctx; // context pointer owned by master
    int arg; // module_id or function_arg
    const char* func_name; // for CMD_CALL_EXPORT / nested import
    hb_beamr_fsm_t *fsm;         // FSM handle pointer (optional, can be NULL)
} command_t;

// Event (Slave -> Master)

typedef enum {
    EVT_OK,                 // value = hb_beamr_lib_rc_t or 0 for success
    EVT_RESULT,             // value = function call result
    EVT_ERROR,              // value = hb_beamr_lib_rc_t
    EVT_IMPORT_CALL,        // value = value passed from wasm to import
} event_kind_t;

typedef struct {
    event_kind_t kind;
    int value;
} event_t;

#define QUEUE_CAP 64

extern void push_cmd(command_t cmd);
extern command_t pop_cmd(void);
extern void push_evt(event_t evt);
extern event_t pop_evt(void);

// For blocking imports
extern void wait_for_import_signal(void);
extern void set_pending_import_value(int value);
extern int get_pending_import_value(void);

#endif // ASYNC_QUEUE_H 