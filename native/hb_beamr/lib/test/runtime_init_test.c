#include "hb_beamr_lib.h"
// #include "test_utils.h" // For TEST_ASSERT and test runner macros if we create them
#include <stdio.h>
#include <string.h> // For memset

int main() {
    printf("Running test: Runtime Init/Destroy Test\n");

    // Test with NULL init_args (should use defaults)
    hb_beamr_lib_rc_t rc = hb_beamr_lib_init_runtime_global(NULL);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_init_runtime_global(NULL) returned %d\n", rc);
        return 1;
    }
    printf("hb_beamr_lib_init_runtime_global(NULL) successful.\n");

    hb_beamr_lib_destroy_runtime_global();
    printf("hb_beamr_lib_destroy_runtime_global() called.\n");

    // Test with specific init_args
    RuntimeInitArgs init_args;
    memset(&init_args, 0, sizeof(RuntimeInitArgs));
    init_args.mem_alloc_type = Alloc_With_System_Allocator;
    init_args.max_thread_num = 1;
    // We could also try to allocate a small heap to see if it works
    // char global_heap_buf[1024 * 10]; // 10KB
    // init_args.mem_alloc_option.allocator.malloc_func = my_malloc; // If using custom allocator
    // init_args.mem_alloc_option.allocator.free_func = my_free;
    // init_args.mem_alloc_option.allocator.realloc_func = my_realloc;
    // init_args.global_heap_buf = global_heap_buf;
    // init_args.global_heap_size = sizeof(global_heap_buf);

    rc = hb_beamr_lib_init_runtime_global(&init_args);
    if (rc != HB_BEAMR_LIB_SUCCESS) {
        fprintf(stderr, "Test FAILED: hb_beamr_lib_init_runtime_global with specific args returned %d\n", rc);
        return 1;
    }
    printf("hb_beamr_lib_init_runtime_global with specific args successful.\n");

    hb_beamr_lib_destroy_runtime_global();
    printf("hb_beamr_lib_destroy_runtime_global() called again.\n");

    printf("Test PASSED: Runtime Init/Destroy Test\n");
    return 0;
} 