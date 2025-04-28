/*
 * These functions should be defined by calling applicaiton
 */

#ifndef STUB_H
#define STUB_H

#ifndef DEBUG
#include <stdio.h>
#define DEBUG(fmt, ...) do { \
        printf("[DEBUG] " fmt "\n", ##__VA_ARGS__); \
    } while (0)
#endif

#ifndef TRACE
#define TRACE(fmt, ...) do {} while (0)
#endif

#endif /* STUB_H */
