#ifndef HB_DRIVER_H
#define HB_DRIVER_H

#include "hb_core.h"

/*
 * Function: drv_lock
 * --------------------
 * Locks the specified mutex to ensure exclusive access to shared resources.
 * 
 *  mutex: The mutex to be locked.
 */
void drv_lock(ErlDrvMutex* mutex);

/*
 * Function: drv_unlock
 * --------------------
 * Unlocks the specified mutex, allowing other threads to access the shared resource.
 * 
 *  mutex: The mutex to be unlocked.
 */
void drv_unlock(ErlDrvMutex* mutex);

/*
 * Function: drv_signal
 * --------------------
 * Signals the condition variable, notifying one or more threads waiting for the condition.
 * 
 *  mut: The mutex used to synchronize access to shared resources.
 *  cond: The condition variable to signal.
 *  ready: A flag indicating the state of the condition, typically set to 1 to signal that the condition is met.
 */
void drv_signal(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready);

/*
 * Function: drv_wait
 * --------------------
 * Causes the current thread to wait for a signal on the condition variable, holding the provided mutex.
 * The thread will be blocked until the condition variable is signaled.
 * 
 *  mut: The mutex used to synchronize access to shared resources.
 *  cond: The condition variable to wait on.
 *  ready: A flag indicating the state of the condition. The thread will wait until this is set to 1.
 */
void drv_wait(ErlDrvMutex* mut, ErlDrvCond* cond, int* ready);

#endif
