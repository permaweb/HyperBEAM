
#ifndef HB_STUBS_H
#define HB_STUBS_H

#ifndef HB_DEBUG
#define HB_DEBUG 0
#endif

#ifndef PROC_DEFINED
#define PROC_DEFINED

#include <erl_driver.h>
typedef struct {
    ErlDrvTermData port_term;
} Proc;

#endif

#endif // HB_STUBS_H