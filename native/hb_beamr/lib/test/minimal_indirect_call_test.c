// Forward declaration for load_file_to_buffer from utils.c
#include <stddef.h> // For size_t
#include <stdint.h> // For uint8_t
#include <stdbool.h> // For bool
#include "utils.h" // For load_file_to_buffer. Should be included early.
#include "hb_beamr_lib.h"
#include "wasm_export.h" // For wasm_val_t
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    printf("--- Minimal Indirect Call Segfault Test ---\\\\n");

    // 1. Initialize runtime
    printf("Initializing runtime...\\\\n");
    if (hb_beamr_lib_init_runtime_global(NULL) != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Failed to initialize runtime.\\\\n");
        return 1;
    }
    printf("Runtime initialized.\\\\n");

    // 2. Create context
    printf("Creating context...\\\\n");
    hb_beamr_lib_context_t* ctx = hb_beamr_lib_create_context();
    if (!ctx) {
        fprintf(stderr, "Failed to create context.\\\\n");
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("Context created: %p\\\\n", (void*)ctx);

    // 3. Load AOT module (simple_indirect.aot)
    const char* module_path = "simple_indirect.aot";
    uint8_t* buffer = NULL;
    uint32_t size = 0;
    printf("Loading AOT module: %s...\\\\n", module_path);
    if (!(buffer = read_file_to_buffer(module_path, &size))) {
        fprintf(stderr, "Failed to load AOT module \'%s\'. Error: %s\\\\n", module_path, hb_beamr_lib_get_last_error(ctx));
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("AOT module loaded (%d bytes).\\\\n", size);

    hb_beamr_lib_rc_t load_rc = hb_beamr_lib_load_aot_module(ctx, buffer, (uint32_t)size);
    free(buffer); // Buffer is copied internally by load_aot_module
    if (load_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "hb_beamr_lib_load_aot_module failed: %s (RC: %d)\\\\n", hb_beamr_lib_get_last_error(ctx), load_rc);
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("AOT module processed by hb_beamr_lib.\\\\n");

    // 4. Instantiate module
    printf("Instantiating module...\\\\n");
    // Default stack and heap sizes, no user data override
    hb_beamr_lib_rc_t inst_rc = hb_beamr_lib_instantiate(ctx, 0, 0, NULL); 
    if (inst_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "hb_beamr_lib_instantiate failed: %s (RC: %d)\\\\n", hb_beamr_lib_get_last_error(ctx), inst_rc);
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("Module instantiated.\\\\n");

    // 5. Attempt indirect call
    const char* table_name = "__indirect_function_table"; // Common default, and seen in logs
    uint32_t func_index = 0;
    uint32_t num_args = 1;
    wasm_val_t args[1] = {
        {
            .kind = WASM_I32,
            .of.i32 = 1,
            ._paddings = {0, 0, 0, 0, 0, 0, 0}
        }
    };
    uint32_t num_results = 1;
    wasm_val_t results[1] = {0}; // No results expected

    printf("Attempting indirect call: table=\'%s\', index=%u, num_args=%u, num_results=%u\\\\n",
           table_name, func_index, num_args, num_results);
    
    // The segfault is expected within this call, specifically in wasm_table_get_func_inst
    // which is called by hb_beamr_lib_get_indirect_func_inst, itself called by hb_beamr_lib_call_indirect.
    hb_beamr_lib_rc_t call_rc = hb_beamr_lib_call_indirect(ctx, table_name, func_index, num_args, args, num_results, results);

    if (call_rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "hb_beamr_lib_call_indirect failed: %s (RC: %d)\\\\n", hb_beamr_lib_get_last_error(ctx), call_rc);
        // Even if it fails but doesn\'t segfault, it\'s a data point.
    } else {
        printf("hb_beamr_lib_call_indirect reported success (RC: %d). This is unexpected if replicating segfault.\\\\n", call_rc);
    }
    printf("Indirect call attempt finished.\\\\n");

    // 6. Cleanup
    printf("Cleaning up...\\\\n");
    hb_beamr_lib_destroy_context(ctx);
    hb_beamr_lib_destroy_runtime_global();
    printf("Cleanup complete.\\\\n");

    if (call_rc != HB_BEAMR_LIB_SUCCESS) {
         // If it failed cleanly, return specific error. If it segfaulted, this won't be reached.
        return 2; 
    }

    return 0; // Should not be reached if segfault occurs
} 