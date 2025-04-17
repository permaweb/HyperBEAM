#ifndef HB_HELPERS_H
#define HB_HELPERS_H

#include "hb_core.h"

/*
 * Function: get_wasm_type_name
 * --------------------
 * Returns the string name corresponding to the given WASM value type.
 * 
 *  kind: The WASM value type kind (e.g., WASM_I32, WASM_I64, WASM_F32, WASM_F64).
 * 
 *  returns: A string representing the value type (e.g., "i32", "i64", etc.).
 */
const char* get_wasm_type_name(wasm_valkind_t kind);

/*
 * Function: wasm_externtype_to_kind_string
 * --------------------
 * Converts a WASM external type to its corresponding kind string.
 * 
 *  type: A pointer to the WASM external type to convert.
 * 
 *  returns: A string representing the kind of the external type (e.g., "func", "global", "table", "memory").
 */
const char* wasm_externtype_to_kind_string(const wasm_externtype_t* type);

/*
 * Function: wasm_valtype_kind_to_char
 * --------------------
 * Converts a WASM value type to its corresponding character representation.
 * 
 *  valtype: The WASM value type to convert.
 * 
 *  returns: A character representing the value type (e.g., 'i' for i32, 'f' for f32).
 */
char wasm_valtype_kind_to_char(const wasm_valtype_t* valtype);

/*
 * Function: wasm_val_to_erl_term
 * --------------------
 * Converts a WASM value to an Erlang term.
 * 
 *  term: The Erlang term data to be filled with the converted value.
 *  val: The WASM value to convert.
 * 
 *  returns: 2 on success (size of the term), 0 if conversion fails.
 */
int wasm_val_to_erl_term(ErlDrvTermData* term, const wasm_val_t* val);

/*
 * Function: erl_term_to_wasm_val
 * --------------------
 * Converts an Erlang term to a WASM value.
 * 
 *  val: The WASM value to be populated.
 *  term: The Erlang term to convert.
 * 
 *  returns: 0 on success, -1 if conversion fails.
 */
int erl_term_to_wasm_val(wasm_val_t* val, ei_term* term);

/*
 * Function: erl_terms_to_wasm_vals
 * --------------------
 * Converts a list of Erlang terms to a vector of WASM values.
 * 
 *  vals: The WASM values vector to be filled.
 *  terms: The list of Erlang terms to convert.
 * 
 *  returns: 0 on success, -1 on failure.
 */
int erl_terms_to_wasm_vals(wasm_val_vec_t* vals, ei_term* terms);

/*
 * Function: decode_list
 * --------------------
 * Decodes a list of Erlang terms from a provided binary buffer.
 * 
 *  buff: The binary buffer containing the Erlang encoded terms.
 *  index: The index in the buffer to start decoding from.
 * 
 *  returns: A pointer to the decoded list of Erlang terms, or NULL if an error occurs.
 */
ei_term* decode_list(char* buff, int* index);

/*
 * Function: get_function_sig
 * --------------------
 * Retrieves the function signature as a string from a WASM external type.
 * 
 *  type: The WASM external type representing a function.
 *  type_str: The string that will hold the function signature.
 * 
 *  returns: 1 on success, 0 if the external type is not a function or there is an error.
 */
int get_function_sig(const wasm_externtype_t* type, char* type_str);

/*
 * Function: get_exported_function
 * --------------------
 * Retrieves an exported function from the WASM instance by its name.
 * 
 *  proc: The process structure containing the WASM instance.
 *  target_name: The name of the exported function to retrieve.
 * 
 *  returns: A pointer to the exported WASM function, or NULL if the function is not found.
 */
wasm_func_t* get_exported_function(Proc* proc, const char* target_name);

/*
 * Function: get_memory
 * --------------------
 * Retrieves the WASM memory associated with the given process.
 * 
 *  proc: The process structure containing the WASM instance.
 * 
 *  returns: A pointer to the WASM memory object, or NULL if not found.
 */
wasm_memory_t* get_memory(Proc* proc);

/*
 * Function: get_memory_size
 * --------------------
 * Retrieves the size of the WASM memory in bytes.
 * 
 *  proc: The process structure containing the WASM instance.
 * 
 *  returns: The size of the WASM memory in bytes.
 */
long get_memory_size(Proc* proc);

#endif // HB_HELPERS_H
