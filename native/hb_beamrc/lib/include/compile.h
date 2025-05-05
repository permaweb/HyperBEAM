#ifndef _HB_WASM_COMPILE_H_
#define _HB_WASM_COMPILE_H_

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>
#include "wasm_export.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct CompileOpts {
    MemAllocOption mem_alloc_option;
} CompileOpts;

typedef struct WasmInterfaceFunction {
    char *module_name;
    char *field_name;
    wasm_import_export_kind_t kind;
    const char *signature;
} WasmInterfaceFunction;

/**
 * Compile WebAssembly binary to AOT module.
 *
 * @param compile_opts Compile options
 * @param wasm_module_data Pointer to the WebAssembly module data
 * @param wasm_module_size Size of the WebAssembly module data
 * @param out_imports Pointer to the imports
 * @param out_import_count Count of the imports
 * @param out_exports Pointer to the exports
 * @param out_export_count Count of the exports
 * @param out_wasm_aot_data Pointer to the AOT data
 * @param out_wasm_aot_size Size of the AOT data
 *
 * @return 0 if successful, non-zero otherwise
 */
int hb_wasm_aot_compile(CompileOpts *compile_opts, uint8_t *wasm_module_data, size_t wasm_module_size, WasmInterfaceFunction **out_imports, size_t *out_import_count, WasmInterfaceFunction **out_exports, size_t *out_export_count, uint8_t **out_wasm_aot_data, size_t *out_wasm_aot_size);

#ifdef __cplusplus
}
#endif

#endif /* _HB_WASM_COMPILE_H_ */ 