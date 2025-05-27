#include "hb_beamr_lib.h"
#include "utils.h"
#include "async_queue.h" // Contains queue type defs and extern declarations

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

// Definitions for g_cmd_queue and its sync primitives, and push/pop_cmd
static command_t g_cmd_queue[QUEUE_CAP];
static int g_cmd_head = 0, g_cmd_tail = 0;
static pthread_mutex_t g_cmd_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_cmd_cv = PTHREAD_COND_INITIALIZER;

void push_cmd(command_t cmd) {
    pthread_mutex_lock(&g_cmd_mutex);
    g_cmd_queue[g_cmd_head] = cmd;
    g_cmd_head = (g_cmd_head + 1) % QUEUE_CAP;
    pthread_cond_signal(&g_cmd_cv);
    pthread_mutex_unlock(&g_cmd_mutex);
}

command_t pop_cmd() {
    pthread_mutex_lock(&g_cmd_mutex);
    while (g_cmd_tail == g_cmd_head) pthread_cond_wait(&g_cmd_cv, &g_cmd_mutex);
    command_t cmd = g_cmd_queue[g_cmd_tail];
    g_cmd_tail = (g_cmd_tail + 1) % QUEUE_CAP;
    pthread_mutex_unlock(&g_cmd_mutex);
    return cmd;
}

// Definitions for g_evt_queue and its sync primitives, and push/pop_evt
static event_t g_evt_queue[QUEUE_CAP];
static int g_evt_head = 0, g_evt_tail = 0;
static pthread_mutex_t g_evt_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_evt_cv = PTHREAD_COND_INITIALIZER;

void push_evt(event_t evt) {
    pthread_mutex_lock(&g_evt_mutex);
    g_evt_queue[g_evt_head] = evt;
    g_evt_head = (g_evt_head + 1) % QUEUE_CAP;
    pthread_cond_signal(&g_evt_cv);
    pthread_mutex_unlock(&g_evt_mutex);
}

event_t pop_evt() {
    pthread_mutex_lock(&g_evt_mutex);
    while (g_evt_tail == g_evt_head) pthread_cond_wait(&g_evt_cv, &g_evt_mutex);
    event_t evt = g_evt_queue[g_evt_tail];
    g_evt_tail = (g_evt_tail + 1) % QUEUE_CAP;
    pthread_mutex_unlock(&g_evt_mutex);
    return evt;
}

// For blocking imports
static int g_pending_import_value = 0;
static pthread_mutex_t g_import_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_import_cv = PTHREAD_COND_INITIALIZER;

void wait_for_import_signal(void) {
    pthread_mutex_lock(&g_import_mutex);
    pthread_cond_wait(&g_import_cv, &g_import_mutex); // Slave waits here
    pthread_mutex_unlock(&g_import_mutex);
}

void set_pending_import_value(int value) {
    pthread_mutex_lock(&g_import_mutex);
    g_pending_import_value = value;
    pthread_cond_signal(&g_import_cv); // Master signals slave
    pthread_mutex_unlock(&g_import_mutex);
}

int get_pending_import_value(void) {
    pthread_mutex_lock(&g_import_mutex);
    int val = g_pending_import_value;
    pthread_mutex_unlock(&g_import_mutex);
    return val;
} 