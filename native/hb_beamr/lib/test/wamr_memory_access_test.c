#include "hb_beamr_lib.h" // Using the WAMR-native library
#include "utils.h"      // For read_file_to_buffer
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// Custom assert for hb_beamr_lib_rc_t
#define assert_rc_wamr(rc, expected_rc, ctx, msg) \
    if (rc != expected_rc) { \
        fprintf(stderr, "Assertion failed: %s. Expected %d, got %d. Error: %s\n", \
                msg, expected_rc, rc, hb_beamr_lib_get_last_error(ctx)); \
        assert(rc == expected_rc); \
    }

const char* wasm_file_wamr = "simple_memory.aot"; // Can reuse the same wasm file
const char* default_memory_export_name_wamr = "memory"; // Or NULL for default memory

void test_get_memory_info_success_wamr(hb_beamr_lib_context_t* ctx) {
    printf("--- Test (WAMR Lib): Get Memory Info (Success) ---\n");
    uint8_t* data_ptr = NULL;
    size_t data_size_bytes = 0;
    wasm_memory_inst_t memory_inst = NULL; // WAMR internal memory instance type

    // Get default memory info
    hb_beamr_lib_rc_t rc = hb_beamr_lib_get_memory_info(ctx, NULL, &data_ptr, &data_size_bytes, &memory_inst);
    assert_rc_wamr(rc, HB_BEAMR_LIB_SUCCESS, ctx, "hb_beamr_lib_get_memory_info (default)");
    assert(data_ptr != NULL && "Data pointer should not be NULL after successful get_memory_info");
    assert(memory_inst != NULL && "Memory instance should not be NULL after successful get_memory_info");
    
    size_t expected_initial_size_bytes = 65536; // 1 page = 64KiB
    assert(data_size_bytes == expected_initial_size_bytes && "Initial memory size mismatch");
    printf("Got default memory: ptr=%p, size=%zu bytes, wasm_memory_inst_t=%p\n", (void*)data_ptr, data_size_bytes, (void*)memory_inst);

    printf("Test PASSED (WAMR Lib Get Memory Info)\n");
}

void test_read_write_memory_success_wamr(hb_beamr_lib_context_t* ctx) {
    printf("--- Test (WAMR Lib): Read/Write Memory (Success) ---\n");
    hb_beamr_lib_rc_t rc;

    uint8_t write_buf[10];
    for (int i = 0; i < 10; ++i) write_buf[i] = (uint8_t)(i + 100);
    size_t offset = 16;

    printf("Writing 10 bytes at offset %zu (direct)\n", offset);
    rc = hb_beamr_lib_direct_write_memory(ctx, offset, write_buf, sizeof(write_buf));
    assert_rc_wamr(rc, HB_BEAMR_LIB_SUCCESS, ctx, "hb_beamr_lib_direct_write_memory");

    uint8_t read_buf[10] = {0};
    printf("Reading 10 bytes from offset %zu (direct)\n", offset);
    rc = hb_beamr_lib_direct_read_memory(ctx, offset, read_buf, sizeof(read_buf));
    assert_rc_wamr(rc, HB_BEAMR_LIB_SUCCESS, ctx, "hb_beamr_lib_direct_read_memory");

    for (int i = 0; i < 10; ++i) {
        assert(read_buf[i] == write_buf[i] && "Memory content mismatch after direct write/read");
    }
    printf("Read content matches written content (direct).\n");
    printf("Test PASSED (WAMR Lib Read/Write)\n");
}

void test_read_write_out_of_bounds_wamr(hb_beamr_lib_context_t* ctx) {
    printf("--- Test (WAMR Lib): Read/Write Memory (Out of Bounds) ---\n");
    hb_beamr_lib_rc_t rc;
    
    uint8_t* data_ptr_check = NULL;
    size_t data_size_bytes_check = 0;
    wasm_memory_inst_t mem_inst_check = NULL;
    rc = hb_beamr_lib_get_memory_info(ctx, NULL, &data_ptr_check, &data_size_bytes_check, &mem_inst_check);
    assert_rc_wamr(rc, HB_BEAMR_LIB_SUCCESS, ctx, "hb_beamr_lib_get_memory_info for OOB size check");
    assert(data_size_bytes_check > 0 && "Memory size is 0, cannot perform OOB test reliably.");

    uint8_t buf[10];
    size_t large_offset = data_size_bytes_check > 5 ? data_size_bytes_check - 5 : 0; 
    size_t large_length = 10;         

    if (large_offset + large_length > data_size_bytes_check) {
        printf("Attempting to write out of bounds (direct: offset %zu, length %zu, size %zu)\n", large_offset, large_length, data_size_bytes_check);
        rc = hb_beamr_lib_direct_write_memory(ctx, large_offset, buf, large_length);
        assert_rc_wamr(rc, HB_BEAMR_LIB_ERROR_MEMORY_ACCESS_OUT_OF_BOUNDS, ctx, "hb_beamr_lib_direct_write_memory (OOB)");
        printf("Write OOB correctly failed (direct): %s\n", hb_beamr_lib_get_last_error(ctx));

        printf("Attempting to read out of bounds (direct: offset %zu, length %zu, size %zu)\n", large_offset, large_length, data_size_bytes_check);
        rc = hb_beamr_lib_direct_read_memory(ctx, large_offset, buf, large_length);
        assert_rc_wamr(rc, HB_BEAMR_LIB_ERROR_MEMORY_ACCESS_OUT_OF_BOUNDS, ctx, "hb_beamr_lib_direct_read_memory (OOB)");
        printf("Read OOB correctly failed (direct): %s\n", hb_beamr_lib_get_last_error(ctx));
    } else {
        printf("Skipping OOB direct write/read test as calculated offset is not out of bounds (offset %zu, length %zu, size %zu). This might indicate very small memory size.\n", large_offset, large_length, data_size_bytes_check);
    }
    printf("Test PASSED (WAMR Lib OOB)\n");
}

int main() {
    printf("=== Starting WAMR-Native Lib Memory Access Test ===\n");
    fflush(stdout);

    RuntimeInitArgs init_args;
    memset(&init_args, 0, sizeof(RuntimeInitArgs));
    init_args.mem_alloc_type = Alloc_With_System_Allocator; 
                                                    
    hb_beamr_lib_rc_t rc = hb_beamr_lib_init_runtime_global(&init_args); 
    assert_rc_wamr(rc, HB_BEAMR_LIB_SUCCESS, NULL, "hb_beamr_lib_init_runtime_global");
    printf("Runtime initialized (WAMR Lib).\n");
    fflush(stdout);

    hb_beamr_lib_context_t* ctx = hb_beamr_lib_create_context();
    assert(ctx != NULL && "hb_beamr_lib_create_context returned NULL");
    printf("Context created (WAMR Lib): %p\n", (void*)ctx);
    fflush(stdout);

    uint8_t* wasm_binary = NULL;
    uint32_t file_size = 0;
    wasm_binary = read_file_to_buffer(wasm_file_wamr, &file_size);
    assert(wasm_binary != NULL && file_size > 0 && "Failed to read Wasm file");
    printf("Wasm file read: %s (%u bytes).\n", wasm_file_wamr, file_size);
    fflush(stdout);

    rc = hb_beamr_lib_load_wasm_module(ctx, wasm_binary, file_size);
    free_buffer(wasm_binary); 
    assert_rc_wamr(rc, HB_BEAMR_LIB_SUCCESS, ctx, "hb_beamr_lib_load_wasm_module");
    printf("Module loaded (WAMR Lib).\n");
    fflush(stdout);

    rc = hb_beamr_lib_instantiate(ctx, 0, 0); 
    assert_rc_wamr(rc, HB_BEAMR_LIB_SUCCESS, ctx, "hb_beamr_lib_instantiate");
    printf("Module instantiated (WAMR Lib).\n");
    
    wasm_val_t args_dummy[2];
    args_dummy[0].kind = WASM_I32; 
    args_dummy[0].of.i32 = 0; 
    args_dummy[1].kind = WASM_I32;
    args_dummy[1].of.i32 = 123; 
    
    hb_beamr_lib_rc_t call_rc = hb_beamr_lib_call_export(ctx, "write_byte_wat", 2, args_dummy, 0, NULL);
    if (call_rc == HB_BEAMR_LIB_SUCCESS) {
        printf("write_byte_wat(0, 123) called successfully (WAMR Lib).\n");
    } else {
        printf("Call to write_byte_wat(0, 123) failed (WAMR Lib): %s\n", hb_beamr_lib_get_last_error(ctx));
    }

    test_get_memory_info_success_wamr(ctx);
    test_read_write_memory_success_wamr(ctx);
    test_read_write_out_of_bounds_wamr(ctx);

    printf("Destroying context (WAMR Lib)...\n");
    fflush(stdout);
    hb_beamr_lib_destroy_context(ctx);
    printf("Destroying runtime (WAMR Lib)...\n");
    fflush(stdout);
    hb_beamr_lib_destroy_runtime_global();

    printf("=== WAMR-Native Lib Memory Access Test COMPLETED ===\n");
    fflush(stdout);
    return 0;
} 