#include "hb_beamr_capi_lib.h"
#include "wasm_c_api.h" // For wasm_config_new, wasm_config_delete
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Forward declare the internal set_error_msg_v for testing one specific case.
// This is normally a static internal function. We make it callable for this test only.
// In a real build, this would not be exposed or tested directly this way.
#ifdef TEST_SET_ERROR_MSG
void set_error_msg_v_for_test(hb_beamr_capi_lib_context_t* ctx, const char* format, ...);
#endif

int main(int argc, char* argv[]) {
    printf("Simplified C-API Context + Runtime Test...\n");
    hb_beamr_capi_lib_rc_t rc;

    printf("Initializing runtime (with user-created config)...\n");
    wasm_config_t* user_config_for_init = wasm_config_new();
    assert(user_config_for_init != NULL);

    rc = hb_beamr_capi_lib_init_runtime_global(user_config_for_init); 
    // If successful, init_runtime_global (via wasm_engine_new_with_config) consumes user_config_for_init.
    // If it fails, we must delete it.
    if (rc != HB_BEAMR_CAPI_LIB_SUCCESS) {
        printf("Runtime initialization failed! rc=%d\n", rc);
        if (user_config_for_init) wasm_config_delete(user_config_for_init);
        return 1;
    }
    printf("Runtime initialized.\n");

    printf("Creating context...\n");
    hb_beamr_capi_lib_context_t* ctx = hb_beamr_capi_lib_create_context();
    assert(ctx != NULL); // This is where the previous assertion failed
    printf("Context created successfully (store exists).\n");

    const char* error_msg = hb_beamr_capi_lib_get_last_error(ctx);
    assert(strcmp(error_msg, "No error") == 0);
    printf("Default error message from context: '%s'\n", error_msg);

    printf("Destroying context...\n");
    hb_beamr_capi_lib_destroy_context(ctx);
    ctx = NULL; 
    printf("Context destroyed.\n");

    error_msg = hb_beamr_capi_lib_get_last_error(NULL);
    assert(strstr(error_msg, "Context is NULL") != NULL);
    printf("Error from NULL context: '%s'\n", error_msg);

    printf("Destroying runtime...\n");
    hb_beamr_capi_lib_destroy_runtime_global(); 
    printf("Runtime destroyed successfully.\n");

    printf("Attempting context creation after runtime destroy...\n");
    ctx = hb_beamr_capi_lib_create_context();
    assert(ctx == NULL);
    printf("Context creation failed as expected after runtime destroy.\n");

    printf("\nSimplified C-API Context + Runtime Test PASSED.\n");
    return 0;
}

#ifdef TEST_SET_ERROR_MSG
// This is a test-only function to access the static set_error_msg_v
// It would require set_error_msg_v to be non-static or exposed via a test-only header.
// For now, we acknowledge this kind of testing is harder for static internal functions.
// The real test comes when other API calls set errors.

// Simulate access if we made set_error_msg_v non-static for testing:
/*
void set_error_msg_v_for_test(hb_beamr_capi_lib_context_t* ctx, const char* format, ...) {
    if (ctx) {
        va_list args;
        va_start(args, format);
        // Assuming MAX_LAST_ERROR_MSG_SIZE is available or defined
        vsnprintf(ctx->last_error_msg, MAX_LAST_ERROR_MSG_SIZE, format, args);
        va_end(args);
        ctx->last_error_msg[MAX_LAST_ERROR_MSG_SIZE - 1] = '\0';
    }
}
*/
#endif 