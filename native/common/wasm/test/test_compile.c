#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../include/compile.h"
#include "helpers.h"

int main() {
    int result;
    
    printf("Starting WAMR AOT compilation test\n");
    
    /* Compile WebAssembly module to AOT module */
    uint8_t *out_wasm_aot_data = NULL;
    size_t out_wasm_aot_size = 0;
    result = hb_wasm_aot_compile(dummy_wasm_file, sizeof(dummy_wasm_file), &out_wasm_aot_data, &out_wasm_aot_size);
    
    if (result != 0) {
        fprintf(stderr, "Failed to compile WebAssembly module\n");
        return 1;
    }
    
    printf("Compilation successful, output size: %zu bytes\n", out_wasm_aot_size);
    
    return result;
}