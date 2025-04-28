#ifndef HB_WASM_H
#define HB_WASM_H

#include "hb_core.h"

/*
 * Function:  wasm_handle_import
 * --------------------
 * Handles the import for processing WASM imports.
 * 
 *  env: The environment (import hook) associated with the import.
 *  args: The arguments for the import function.
 *  results: The results of the import will be stored here.
 *
 *  returns: A WASM trap in case of an error, or NULL on success.
 */
wasm_trap_t* wasm_handle_import(void* env, const wasm_val_vec_t* args, wasm_val_vec_t* results);

/*
 * Function:  wasm_initialize_runtime
 * --------------------
 * Initializes the WASM module asynchronously.
 * 
 *  raw: A pointer to the raw data for the initialization.
 */
void wasm_initialize_runtime(void* raw);

/*
 * Function:  wasm_execute_function_async
 * --------------------
 * Asynchronously executes a WASM function.
 * 
 *  raw: A pointer to the process structure containing the function call details.
 */
void wasm_execute_exported_function(void* raw);

/*
 * Function:  wasm_execute_indirect_function
 * --------------------
 * Executes an indirect WASM function asynchronously.
 * 
 *  proc: The current process structure.
 *  function_name: The name of the indirect function to call.
 *  input_args: The input arguments for the function call.
 *  output_results: The results of the function call will be stored here.
 *
 */
void wasm_execute_indirect_function(void *raw);

#endif 