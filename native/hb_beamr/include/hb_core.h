#ifndef HB_CORE_H
#define HB_CORE_H

#include <erl_driver.h>
#include <ei.h>
#include "wasm_c_api.h"
#include "wasm_export.h"
#include "hb_beamr_lib.h"
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <pthread.h>

#define MAX_IMPORT_STACK_DEPTH 10

// Structure to represent a WASM process instance
typedef struct {
    ErlDrvPort port;                // Erlang port associated with this process
    ErlDrvTermData port_term;       // Erlang term representation of the port
    unsigned int port_key;          // TODO: Justify or remove
    ErlDrvMutex* is_running;        // Mutex to track if the process is running
    ErlDrvTermData pid;            // PID of the Erlang process
    bool is_initialized;            // Flag to check if the process is initialized
    time_t start_time;             // Start time of the process
    hb_beamr_lib_context_t* wasm_ctx;   // Context for the WASM module'
    hb_beamr_meta_module_t* wasm_meta;   // Meta data for the WASM module
} Proc;

// Structure to represent the request for loading a WASM binary
typedef struct {
    Proc* proc;                    // The associated process
    uint8_t* binary;                  // Binary data for the WASM module
    long size;                     // Size of the binary
    char* mode;                    // Mode of the WASM module
} InitHandlerReq;

typedef struct {
    Proc* proc;                    // The associated process
    char* function_name;           // The name of the function to call
    wasm_val_t* args;              // The arguments to the function
    int arg_count;                 // The number of arguments
    wasm_valkind_t* result_types;     // The types of the arguments
    int result_count;                 // The number of results
} CallExportHandlerReq;

typedef struct {
    Proc* proc;                    // The associated process
    char* table_name;           // The name of the function to call
    int table_index;             // The index of the function to call
    wasm_val_t* args;              // The arguments to the function
    int arg_count;                 // The number of arguments
    wasm_valkind_t* result_types;     // The types of the arguments
    int result_count;                 // The number of results
} CallIndirectHandlerReq;

// Structure to represent the response for an import operation
typedef struct {
    ErlDrvMutex* response_ready;    // Mutex to synchronize response readiness
    ErlDrvCond* cond;               // Condition variable to signal readiness
    int ready;                       // Flag indicating if the response is ready
    char* error_message;            // Error message (if any)
    wasm_val_t* results;            // The results of the function
    int result_count;              // The number of results
} ImportHandlerRes;

#endif // HB_CORE_H