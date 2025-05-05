#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../include/compile.h"
#include "helpers.h"

int main() {
    int result;
    
    printf("Starting WAMR AOT compilation test\n");

    CompileOpts compile_opts = { 0 };
    compile_opts.mem_alloc_option.allocator.malloc_func = malloc;
    compile_opts.mem_alloc_option.allocator.realloc_func = realloc;
    compile_opts.mem_alloc_option.allocator.free_func = free;
    
    WasmInterfaceFunction* imports; 
    size_t import_count;
    WasmInterfaceFunction* exports;
    size_t export_count;
    uint8_t *out_wasm_aot_data = NULL;
    size_t out_wasm_aot_size = 0;

    result = hb_wasm_aot_compile(&compile_opts, dummy_wasm_file, sizeof(dummy_wasm_file), &imports, &import_count, &exports, &export_count, &out_wasm_aot_data, &out_wasm_aot_size);
    
    if (result != 0) {
        fprintf(stderr, "Failed to compile WebAssembly module\n");
        return 1;
    }
    
    printf("Compilation successful, output size: %zu bytes\n", out_wasm_aot_size);
    
    return result;
}