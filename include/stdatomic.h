#ifndef _STDATOMIC_H_
#define _STDATOMIC_H_

//#define VATOMIC_ENABLE_ATOMIC_SC
#include <vsync/atomic.h>
#include <vsync/atomic/dispatch.h>

/* vatomic32_t and vatomic64_t types are unsigned integers. Nevertheless,
 * for the kind of test cases generated here, this should be fine. */
#define atomic_int vatomic32_t
#define atomic_long vatomic64_t

#define atomic_exchange_explicit(a, v, mo) vatomic_xchg(a, v)
#define atomic_store_explicit(a, v, mo) vatomic_write(a, v)
#define atomic_load_explicit(a, mo) vatomic_read(a)

#endif
