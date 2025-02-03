#include "include/hb_driver.h"
#include "include/hb_logging.h"
#include "include/hb_helpers.h"


void drv_lock(ErlDrvMutex* mutex) {
    DRV_DEBUG("Locking: %s", erl_drv_mutex_name(mutex));
    erl_drv_mutex_lock(mutex);
    DRV_DEBUG("Locked: %s", erl_drv_mutex_name(mutex));
}


void drv_unlock(ErlDrvMutex* mutex) {
    // Copy the mutex_name, so we do not race with the part of the code in async
    // function that cleans it
    char mutex_name[64];
    strcpy(mutex_name, erl_drv_mutex_name(mutex));

    DRV_DEBUG("Unlocking: %s", mutex_name);
    erl_drv_mutex_unlock(mutex);
    DRV_DEBUG("Unlocked: %s", mutex_name);
}

void drv_signal(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready) {
    DRV_DEBUG("Signaling: %s. Pre-signal ready state: %d", erl_drv_cond_name(cond), *ready);
    drv_lock(mut);
    *ready = 1;

    // Copy the cond_name, so we do not race with the part of the code in async
    // function that cleans it
    char cond_name[64];
    strcpy(cond_name, erl_drv_cond_name(cond));

    erl_drv_cond_signal(cond);
    drv_unlock(mut);

    DRV_DEBUG("Signaled: %s. Post-signal ready state: %d", cond_name, *ready);
}

void drv_wait(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready) {
    char mutex_name[64];
    char cond_name[64];
    strcpy(cond_name, erl_drv_cond_name(cond));
    strcpy(mutex_name, erl_drv_mutex_name(mut));

    DRV_DEBUG("Started to wait: %s. Ready: %d", cond_name, *ready);
    DRV_DEBUG("Mutex: %s", mutex_name);
    drv_lock(mut);
    while (!*ready) {
        DRV_DEBUG("Waiting: %s", cond_name);
        erl_drv_cond_wait(cond, mut);
        DRV_DEBUG("Woke up: Ready: %d", *ready);
    }
    drv_unlock(mut);
    DRV_DEBUG("Finish waiting: %s", cond_name);
}
