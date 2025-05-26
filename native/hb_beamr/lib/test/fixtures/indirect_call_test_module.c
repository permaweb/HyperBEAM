#include <stdint.h>

// Functions intended for the indirect call table

// EMSCRIPTEN_KEEPALIVE is often used to prevent functions from being removed
// and to ensure they are candidates for the function table.
#if defined(__EMSCRIPTEN__)
#include <emscripten.h>
#else
#define EMSCRIPTEN_KEEPALIVE
#endif

EMSCRIPTEN_KEEPALIVE
int32_t wasm_add_one(int32_t x) {
    return x + 100; // Make it distinct
}

EMSCRIPTEN_KEEPALIVE
int32_t wasm_multiply_by_three(int32_t x) {
    return x * 3;
}

EMSCRIPTEN_KEEPALIVE
void wasm_do_nothing_indirect(void) {
    // This function does nothing, called indirectly.
}

EMSCRIPTEN_KEEPALIVE
float wasm_add_floats_indirect(float a, float b) {
    return a + b;
}

// To ensure these functions are included in the table by emscripten,
// they are marked with EMSCRIPTEN_KEEPALIVE.
// Emscripten often places functions eligible for indirect calls into
// a table named "__indirect_function_table".
// We don't need to export the table itself explicitly from C with Emscripten;
// it creates one if function pointers are used. 