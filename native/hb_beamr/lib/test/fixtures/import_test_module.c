#include <emscripten.h> // For EMSCRIPTEN_KEEPALIVE

#ifdef __cplusplus
extern "C" {
#endif

// Imported host function: (env, host_add_one) with signature (i)i
// The actual implementation is provided by the host (our C test code).
extern int host_add_one(int val);

EMSCRIPTEN_KEEPALIVE
int wasm_add_two_via_host(int val) {
    int val_plus_one = host_add_one(val);
    return host_add_one(val_plus_one); 
}

#ifdef __cplusplus
}
#endif 