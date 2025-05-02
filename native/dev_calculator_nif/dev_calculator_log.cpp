#include "include/dev_calculator_log.h"

void beamr_print(int print, const char* file, int line, const char* format, ...) {
    va_list args;
    va_start(args, format);
    if(print) {
        pthread_t thread_id = pthread_self();
        printf("[DBG#%p @ %s:%d] ", thread_id, file, line);
        vprintf(format, args);
        printf("\r\n");
    }
    va_end(args);
}
