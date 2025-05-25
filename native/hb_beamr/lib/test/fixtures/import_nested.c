#ifndef EMSCRIPTEN_KEEPALIVE
#define EMSCRIPTEN_KEEPALIVE __attribute__((used)) __attribute__((visibility("default")))
#endif

// Global variable in Wasm memory
volatile unsigned int global_data_buffer[16]; // 16 * 4 = 64 bytes

// Forward declaration for the imported function from module "env"
// It takes an offset in Wasm memory.
extern void give_host_control(unsigned int current_index);

EMSCRIPTEN_KEEPALIVE
volatile unsigned int* get_data_ptr() {
    return &global_data_buffer[0];
}

EMSCRIPTEN_KEEPALIVE
void xor_memory(unsigned int index, unsigned int xor_val) {
    // Check if index is within bounds
    if (index < 16) { // 16 is the size of global_data_buffer
        global_data_buffer[index] = global_data_buffer[index] ^ xor_val;
    }
    // Optionally, handle out-of-bounds access, e.g., by trapping or logging.
    // For this example, it will just do nothing if out of bounds.
}

EMSCRIPTEN_KEEPALIVE
unsigned int call_host_and_read(unsigned int index, unsigned int init_val) {
    // Check if index is within bounds
    if (index < 16) { // 16 is the size of global_data_buffer
        volatile unsigned int* ptr_in_wasm_memory = &global_data_buffer[index];
        *ptr_in_wasm_memory = init_val; // Set memory to init_val
        give_host_control(index); // Call the imported host function, passing the current index
        return global_data_buffer[index]; // Return the (hopefully modified) value
    }
    // Return a distinct value on error or if index is out of bounds.
    return 0xFFFFFFFF;
}
