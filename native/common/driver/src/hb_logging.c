#include "../include/hb_logging.h"
#include <erl_driver.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <pthread.h>

extern ErlDrvTermData atom_error;

void beamr_print(int print, const char* file, int line, const char* format, ...) {
    va_list args;
    va_start(args, format);
    if(print) {
        pthread_t thread_id = pthread_self();
        printf("[DBG#%p @ %s:%d] ", thread_id, file, line);
        vprintf(format, args);
        printf("\r\n");
        fflush(stdout);
    }
    va_end(args);
}

void send_error(ErlDrvTermData port_term, const char* message_fmt, ...) {
    va_list args;
    va_start(args, message_fmt);
    char* message = driver_alloc(256);
    vsnprintf(message, 256, message_fmt, args);
    DRV_DEBUG("Sending error message: %s", message);

    ErlDrvTermData* msg = driver_alloc(sizeof(ErlDrvTermData) * (
        + 2 // atom_error
        + 3 // message string
        + 2 // tuple
    ));
    int msg_i = 0;

    msg[msg_i++] = ERL_DRV_ATOM;
    msg[msg_i++] = atom_error;

    msg[msg_i++] = ERL_DRV_STRING;
    msg[msg_i++] = (ErlDrvTermData)message;
    msg[msg_i++] = strlen(message);

    msg[msg_i++] = ERL_DRV_TUPLE;
    msg[msg_i++] = 2;

    int msg_res = erl_drv_output_term(port_term, msg, msg_i);
    DRV_DEBUG("Sent error message. Res: %d", msg_res);
    va_end(args);
}