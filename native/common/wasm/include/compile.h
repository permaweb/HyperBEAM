#ifndef _HB_WASM_COMPILE_H_
#define _HB_WASM_COMPILE_H_

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Compile WebAssembly binary to AOT module.
 *
 * @param wasm_file_path Path to the WebAssembly binary file
 * @param wasm_file_path_size Length of the WebAssembly file path
 * @param aot_file_path Path to write the output AOT file
 * @param aot_file_path_size Length of the AOT file path
 *
 * @return 0 if successful, non-zero otherwise
 */
int hb_wasm_aot_compile(uint8_t *wasm_module_data, size_t wasm_module_data_size, uint8_t **out_wasm_aot_data, size_t *out_wasm_aot_size);

#ifdef __cplusplus
}
#endif

#endif /* _HB_WASM_COMPILE_H_ */ 