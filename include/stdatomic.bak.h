#ifndef _STDATOMIC_H_
#define _STDATOMIC_H_

#include <stdint.h>
#include <stddef.h>
//#define VATOMIC_ENABLE_ATOMIC_SC
#include <vsync/common/macros.h>
#include <vsync/atomic/internal/dispatch.h>
#include <vsync/atomic/dispatch.h>
#include <vsync/atomic.h>

#define memory_order_seq_cst 1

/* vatomic32_t and vatomic64_t types are unsigned integers. Nevertheless,
 * for the kind of test cases generated here, this should be fine. */
#define atomic_int vatomic32_t
#define atomic_long vatomic64_t

#define atomic_store_explicit(a, v, mo) vatomic_write(a, v)
#define atomic_load_explicit(a, mo) vatomic_read(a)

#define atomic_exchange_explicit(a, v, mo) vatomic_xchg(a, v)
#define atomic_compare_exchange_strong_explicit(a, e, v, mos, mof) ({ \
    long exp = *e; \
    *e = vatomic_cmpxchg(a, exp, v); \
    *e == exp;})
#endif
