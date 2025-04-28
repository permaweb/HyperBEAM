#ifndef HB_CORE_H
#define HB_CORE_H

#include <erl_driver.h>
#include <ei.h>
#include "wasm_c_api.h"
#include "wasm_export.h"
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <pthread.h>

#define MAX_IMPORT_STACK_DEPTH 10

// Structure to represent the response for an import operation
typedef struct {
    ErlDrvMutex* response_ready;    // Mutex to synchronize response readiness
    ErlDrvCond* cond;               // Condition variable to signal readiness
    int ready;                       // Flag indicating if the response is ready
    char* error_message;            // Error message (if any)
    ei_term* result_terms;          // List of result terms from the import
    int result_length;              // Length of the result_terms
} ImportResponse;

#define PROC_DEFINED

// Structure to represent a WASM process instance
typedef struct {
    // wasm_engine_t engine;          // WASM engine instance
    wasm_module_t module;          // WASM module
    wasm_module_inst_t instance;      // WASM instance
    // wasm_store_t* store;            // WASM store
    ErlDrvPort port;                // Erlang port associated with this process
    ErlDrvTermData port_term;       // Erlang term representation of the port
    unsigned int port_key;
    ErlDrvMutex* is_running;        // Mutex to track if the process is running
    wasm_table_inst_t indirect_func_table; // Indirect function table
    wasm_exec_env_t exec_env;      // Execution environment for the WASM instance
    char* current_function;        // Current function being executed
    long current_function_ix;   // Index of the current function
    int indirect_func_table_ix;    // Index of the indirect function table
    ei_term* current_args;         // Arguments for the current function
    int current_args_length;       // Length of the current arguments
    ImportResponse* current_import; // Import response structure
    ImportResponse* import_stack[MAX_IMPORT_STACK_DEPTH]; // Stack of import responses
    int import_stack_depth;        // Depth of the current import stack
    ErlDrvTermData pid;            // PID of the Erlang process
    int is_initialized;            // Flag to check if the process is initialized
    time_t start_time;             // Start time of the process
} Proc;

// Structure to represent the request for loading a WASM binary
typedef struct {
    void* binary;                  // Binary data for the WASM module
    long size;                     // Size of the binary
    Proc* proc;                    // The associated process
    char* mode;                    // Mode of the WASM module
} LoadWasmReq;

// NO_PROD: Import these from headers instead

// Structure for a common WASM module instance
typedef struct WASMModuleInstanceCommon {
    uint32_t module_type;          // Type of the module
    uint8_t module_inst_data[1];   // Module instance data
} WASMModuleInstanceCommon;

// Structure to store host information about the WASM instance
struct wasm_host_info {
    void *info;                        // Pointer to host info
    void (*finalizer)(void *);         // Finalizer function for the host info
};

// Structure representing a WASM function (extended with host-specific details)
struct wasm_func_t {
    wasm_name_t *module_name;        // Module name for the function
    wasm_name_t *name;               // Function name
    uint16_t kind;                   // Function kind (e.g., export)
    struct wasm_host_info host_info; // Host-specific information
    wasm_functype_t *type;          // Function type (parameters and results)
    uint16_t param_count;            // Number of parameters
    uint16_t result_count;           // Number of results
    bool with_env;                   // Whether the function has an environment
    union {
        wasm_func_callback_t cb;         // Callback function
        struct callback_ext {
            void *env;                  // Environment for the callback
            wasm_func_callback_with_env_t cb; // Callback function with environment
            void (*finalizer)(void *);  // Finalizer for the callback
        } cb_env;
    } u;
    uint16_t func_idx_rt;            // Function index in the runtime
    WASMModuleInstanceCommon *inst_comm_rt; // Module instance data
    WASMFunctionInstanceCommon *func_comm_rt; // Function instance data
};

typedef struct {
    const char *module_name;
    NativeSymbol *symbols;
    int count;
} ImportModuleSymbols;

typedef struct {
    Proc *proc;
    const char *module_name;
    const char *field_name;
    const char *signature;
    uint32_t param_count;
    wasm_valkind_t *param_kinds;
    uint32_t result_count;
    wasm_valkind_t *result_kinds;
} NativeSymbolAttachment;

#endif // HB_CORE_H