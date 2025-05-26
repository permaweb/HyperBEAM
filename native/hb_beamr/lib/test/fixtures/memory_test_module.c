#define WASM_EXPORT __attribute__((visibility("default")))
#include <stdint.h>
#include <stddef.h>

// Declare a memory section (WAMR default is to export "memory" if present)
// This is typically handled by the linker script or compiler flags when building for Wasm.
// For emscripten, it usually exports memory by default if you use it.
unsigned char wasm_memory_buffer[2 * 65536]; // 2 pages of memory

WASM_EXPORT
void initialize_memory_pattern(uint32_t offset, uint8_t value, uint32_t length) {
    if (offset + length <= sizeof(wasm_memory_buffer)) {
        for (uint32_t i = 0; i < length; ++i) {
            wasm_memory_buffer[offset + i] = value + (uint8_t)i;
        }
    }
}

WASM_EXPORT
uint8_t verify_memory_pattern(uint32_t offset, uint8_t expected_value, uint32_t length) {
    if (offset + length <= sizeof(wasm_memory_buffer)) {
        for (uint32_t i = 0; i < length; ++i) {
            if (wasm_memory_buffer[offset + i] != (expected_value + (uint8_t)i)) {
                return 0; // Verification failed
            }
        }
        return 1; // Verification succeeded
    }
    return 0; // Out of bounds
}

WASM_EXPORT
uint32_t get_buffer_size() {
    return sizeof(wasm_memory_buffer);
}

// Host function to be imported by the Wasm module
extern uint8_t host_read_wasm_memory(uint32_t offset);

WASM_EXPORT
uint8_t call_host_to_read_memory(uint32_t offset) {
    return host_read_wasm_memory(offset);
} 