#ifndef _WASM_TEST_HELPERS_H
#define _WASM_TEST_HELPERS_H

#include <stdint.h>

/* 
 * Simple helpers for WebAssembly tests
 */

/* 
 * A minimal valid WebAssembly module (magic number + version)
 * magic header: \0asm
 * version: 1
 */
static uint8_t dummy_wasm_file[] = {
    0x00, 0x61, 0x73, 0x6D, /* magic */
    0x01, 0x00, 0x00, 0x00  /* version */
};

#endif /* _WASM_TEST_HELPERS_H */
