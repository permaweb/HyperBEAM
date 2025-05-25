#include "hb_beamr_capi_lib.h"
#include "wasm_c_api.h" // For wasm_config_new, wasm_config_delete
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int main(int argc, char* argv[]) {
    printf("Starting C-API Runtime Init/Destroy Test (Isolated Failing Case)...\n");

    hb_beamr_capi_lib_rc_t rc;
    wasm_config_t* config = NULL;

    // Test with a custom config 
    printf("Initializing runtime with a new default config object...\n");
    config = wasm_config_new();
    if (!config) {
        printf("wasm_config_new() failed!\n");
        return 1;
    }
    
    rc = hb_beamr_capi_lib_init_runtime_global(config);
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("hb_beamr_capi_lib_init_runtime_global failed with rc = %d\n", rc);
        // Config was not consumed by init_runtime_global on failure if it returned early
        // or if wasm_engine_new_with_config itself failed before consuming.
        // If wasm_engine_new_with_config consumes on its own failure, this is an issue.
        // Based on WAMR code, it does not consume if wasm_runtime_full_init fails.
        wasm_config_delete(config); 
        return 1;
    }
    printf("Runtime initialized successfully with a new config object.\n");

    // If we reached here, 'config' was consumed by a successful init.

    printf("Destroying runtime...\n");
    hb_beamr_capi_lib_destroy_runtime_global();
    printf("Runtime destroyed successfully.\n");

    printf("\nC-API Runtime Init/Destroy Test (Isolated) PASSED.\n");
    return 0;
} 