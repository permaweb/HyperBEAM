#include "hb_beamr_lib.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

int main() {
    printf("Running test: Context Management Test\n");

    // Initialize runtime first (required by WAMR even if context doesn't directly use it yet)
    hb_beamr_lib_rc_t rc_rt = hb_beamr_lib_init_runtime_global(NULL);
    assert(rc_rt == HB_BEAMR_LIB_SUCCESS && "Runtime init failed");

    // Test context creation
    hb_beamr_lib_context_t* ctx = hb_beamr_lib_create_context();
    if (ctx == NULL) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_create_context returned NULL\n");
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("hb_beamr_lib_create_context successful.\n");

    // Test get_last_error on a new context
    const char* err_msg = hb_beamr_lib_get_last_error(ctx);
    if (strcmp(err_msg, "No error") != 0) {
        fprintf(stderr, "Test FAILED: Expected 'No error' on new context, got '%s'\n", err_msg);
        hb_beamr_lib_destroy_context(ctx);
        hb_beamr_lib_destroy_runtime_global();
        return 1;
    }
    printf("hb_beamr_lib_get_last_error on new context successful.\n");

    // Test context destruction
    hb_beamr_lib_destroy_context(ctx);
    printf("hb_beamr_lib_destroy_context called.\n");

    // Test get_last_error on a NULL context (after destruction or if creation failed)
    // Note: Accessing ctx after free is UB. This test should be on a genuinely NULL pointer.
    err_msg = hb_beamr_lib_get_last_error(NULL);
    if (strcmp(err_msg, "Context is NULL or error message not set.") != 0) {
        fprintf(stderr, "Test FAILED: Expected 'Context is NULL or error message not set.' for NULL context, got '%s'\n", err_msg);
        hb_beamr_lib_destroy_runtime_global(); // Ensure runtime is cleaned up
        return 1;
    }
    printf("hb_beamr_lib_get_last_error on NULL context successful.\n");

    hb_beamr_lib_destroy_runtime_global();
    printf("Test PASSED: Context Management Test\n");
    return 0;
} 