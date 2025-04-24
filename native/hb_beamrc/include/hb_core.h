#ifndef HB_CORE_H
#define HB_CORE_H

#include <erl_driver.h>
#include <ei.h>
#include "aot_export.h"
#include <string.h>
#include <stdarg.h>
#include <time.h>
#include <pthread.h>

#define PROC_DEFINED

typedef struct {
    ErlDrvPort port;
    ErlDrvTermData port_term;
    unsigned int port_key;
    ErlDrvMutex* is_running;
    ErlDrvMutex* response_ready;
    ErlDrvCond* cond;
    time_t start_time;
    ErlDrvTermData pid;
    int is_initialized;
} Proc;

typedef struct {
    Proc* proc;
    void* binary;
    long size;
} CompileWasmReq;

void invoke_compile(void *raw);

void send_compilation_result(Proc* proc, uint8_t* aot_module, uint32_t aot_module_size);

#endif // HB_CORE_H