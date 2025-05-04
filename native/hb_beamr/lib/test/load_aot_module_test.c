#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "wasm_export.h"
#include "utils.h" // Include our new utils header

int main(int argc, char *argv[]) {
    char *aot_file_path = "./basic_fib.aot"; // Relative path to the AOT file copied by CMake
    char *buffer = NULL;
    uint32_t buffer_size = 0;
    wasm_module_t module = NULL;
    wasm_module_inst_t module_inst = NULL;
    wasm_exec_env_t exec_env = NULL;
    wasm_function_inst_t func_handle = NULL;
    char error_buf[128];
    int result = 1; // Default to failure

    printf("Running AOT module test: %s\n", aot_file_path);

    // Initialize WAMR runtime
    RuntimeInitArgs init_args;
    memset(&init_args, 0, sizeof(RuntimeInitArgs));
    init_args.mem_alloc_type = Alloc_With_Allocator;
    init_args.mem_alloc_option.allocator.malloc_func = malloc;
    init_args.mem_alloc_option.allocator.realloc_func = realloc;
    init_args.mem_alloc_option.allocator.free_func = free;
    // init_args.max_thread_num = 1; // Default is 4

    if (!wasm_runtime_full_init(&init_args)) {
        fprintf(stderr, "Error: Failed to initialize WAMR runtime.\n");
        goto fail_1;
    }

    // Read the AOT file using our utility function
    buffer = read_file_to_buffer(aot_file_path, &buffer_size);
    if (!buffer) {
        fprintf(stderr, "Error: Failed to read AOT file %s (using read_file_to_buffer).\n", aot_file_path);
        goto fail_2;
    }
    printf("Successfully read AOT file (%u bytes).\n", buffer_size);

    // Load the AOT module
    module = wasm_runtime_load((uint8_t *)buffer, buffer_size, error_buf, sizeof(error_buf));
    if (!module) {
        fprintf(stderr, "Error: Failed to load AOT module: %s\n", error_buf);
        goto fail_3;
    }
    printf("Successfully loaded AOT module.\n");

    // Instantiate the module
    module_inst = wasm_runtime_instantiate(module, 1024 * 1024, // stack size
                                           1024 * 1024, // heap size
                                           error_buf, sizeof(error_buf));
    if (!module_inst) {
        fprintf(stderr, "Error: Failed to instantiate AOT module: %s\n", error_buf);
        goto fail_4;
    }
    printf("Successfully instantiated AOT module.\n");

    // Create execution environment
    exec_env = wasm_runtime_create_exec_env(module_inst, 1024 * 1024); // stack size
    if (!exec_env) {
        fprintf(stderr, "Error: Failed to create execution environment.\n");
        goto fail_5;
    }
    printf("Successfully created execution environment.\n");

    // Look up the exported 'handle' function
    func_handle = wasm_runtime_lookup_function(module_inst, "handle");
    if (!func_handle) {
        fprintf(stderr, "Error: Failed to find exported function 'handle'.\n");
        goto fail_6;
    }
    printf("Successfully found function 'handle'.\n");

    // Prepare arguments for the 'handle' function (char* msg, char* env)
    // In the C++ code, these are char*. In WASM, they are pointers (i32/i64).
    // For this test, we'll pass NULL, which corresponds to 0 in WASM.
    uint32_t wasm_argv[2] = {0, 0};

    // Call the 'handle' function
    printf("Calling handle function...\n");
    if (!wasm_runtime_call_wasm(exec_env, func_handle, 2, wasm_argv)) {
        fprintf(stderr, "Error calling handle function: %s\n", wasm_runtime_get_exception(module_inst));
        goto fail_6;
    }

    // The function returns a char* (WASM pointer). Retrieve it.
    uint32_t result_ptr = wasm_argv[0]; // Result is placed back into argv[0]
    printf("handle function returned successfully (result ptr: %u).\n", result_ptr);

    // Validate the result pointer (should not be 0 if successful and returned something)
    if (result_ptr == 0) {
         fprintf(stderr, "Error: handle function returned NULL pointer.\n");
         goto fail_6;
    }

    // Convert the WASM pointer to a native pointer
    // IMPORTANT: Ensure the pointer is valid within the module instance memory
    if (!wasm_runtime_validate_app_addr(module_inst, result_ptr, 1)) { // Check if at least 1 byte is valid
        fprintf(stderr, "Error: Result pointer %u is not a valid app address.\n", result_ptr);
        goto fail_6;
    }

    char *result_native_ptr = (char *)wasm_runtime_addr_app_to_native(module_inst, result_ptr);
    printf("Result string (native): %s\n", result_native_ptr);

    // Basic check on the result string content
    const char* expected_prefix = "Fibonacci index 10 is 55";
    if (strncmp(result_native_ptr, expected_prefix, strlen(expected_prefix)) != 0) {
         fprintf(stderr, "Error: Result string does not match expected prefix.\nExpected: '%s...'\nActual:   '%s'\n", expected_prefix, result_native_ptr);
         goto fail_6;
    }

    printf("Result validation successful!\n");
    result = 0; // Success!

fail_6:
    wasm_runtime_destroy_exec_env(exec_env);
fail_5:
    wasm_runtime_deinstantiate(module_inst);
fail_4:
    wasm_runtime_unload(module);
fail_3:
    if (buffer) free_buffer(buffer); // Use our utility free function
fail_2:
    wasm_runtime_destroy();
fail_1:
    if (result == 0) {
        printf("--- Test PASSED ---\n");
    } else {
        fprintf(stderr, "--- Test FAILED ---\n");
    }
    return result;
}
