typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;
typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;
typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;
typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;
typedef long int __quad_t;
typedef unsigned long int __u_quad_t;
typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;
typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;
typedef int __daddr_t;
typedef int __key_t;
typedef int __clockid_t;
typedef void * __timer_t;
typedef int __blksize_t;
typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;
typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;
typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;
typedef long int __fsword_t;
typedef long int __ssize_t;
typedef long int __syscall_slong_t;
typedef unsigned long int __syscall_ulong_t;
typedef __off64_t __loff_t;
typedef char *__caddr_t;
typedef long int __intptr_t;
typedef unsigned int __socklen_t;
typedef int __sig_atomic_t;
typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;
typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;
typedef __int_least8_t int_least8_t;
typedef __int_least16_t int_least16_t;
typedef __int_least32_t int_least32_t;
typedef __int_least64_t int_least64_t;
typedef __uint_least8_t uint_least8_t;
typedef __uint_least16_t uint_least16_t;
typedef __uint_least32_t uint_least32_t;
typedef __uint_least64_t uint_least64_t;
typedef signed char int_fast8_t;
typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
typedef unsigned char uint_fast8_t;
typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
typedef long int intptr_t;
typedef unsigned long int uintptr_t;
typedef __intmax_t intmax_t;
typedef __uintmax_t uintmax_t;
typedef unsigned int __gwchar_t;

typedef struct
  {
    long int quot;
    long int rem;
  } imaxdiv_t;
extern intmax_t imaxabs (intmax_t __n) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
extern imaxdiv_t imaxdiv (intmax_t __numer, intmax_t __denom)
      __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__const__));
extern intmax_t strtoimax (const char *__restrict __nptr,
      char **__restrict __endptr, int __base) __attribute__ ((__nothrow__ , __leaf__));
extern uintmax_t strtoumax (const char *__restrict __nptr,
       char ** __restrict __endptr, int __base) __attribute__ ((__nothrow__ , __leaf__));
extern intmax_t wcstoimax (const __gwchar_t *__restrict __nptr,
      __gwchar_t **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__));
extern uintmax_t wcstoumax (const __gwchar_t *__restrict __nptr,
       __gwchar_t ** __restrict __endptr, int __base)
     __attribute__ ((__nothrow__ , __leaf__));

typedef long int ptrdiff_t;
typedef long unsigned int size_t;
typedef unsigned int wchar_t;
typedef struct {
  long long __max_align_ll __attribute__((__aligned__(__alignof__(long long))));
  long double __max_align_ld __attribute__((__aligned__(__alignof__(long double))));
} max_align_t;
typedef uint8_t vuint8_t;
typedef uint16_t vuint16_t;
typedef uint32_t vuint32_t;
typedef uint64_t vuint64_t;
typedef uintptr_t vuintptr_t;
typedef int8_t vint8_t;
typedef int16_t vint16_t;
typedef int32_t vint32_t;
typedef int64_t vint64_t;
typedef intptr_t vintptr_t;
typedef size_t vsize_t;
typedef _Bool vbool_t;
typedef struct vatomic8_s {
    vuint8_t _v;
} vatomic8_t;
typedef struct vatomic16_s {
    vuint16_t _v;
} __attribute__((aligned(2))) vatomic16_t;
typedef struct vatomic32_s {
    vuint32_t _v;
} __attribute__((aligned(4))) vatomic32_t;
typedef struct vatomic64_s {
    vuint64_t _v;
} __attribute__((aligned(8))) vatomic64_t;
typedef struct vatomicptr_s {
    void *_v;
} __attribute__((aligned(sizeof(void *)))) vatomicptr_t;
typedef struct vatomicsz_s {
    vsize_t _v;
} __attribute__((aligned(sizeof(vsize_t)))) vatomicsz_t;
static inline vuint32_t vatomic32_await_lt(const vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_await_lt_acq(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_lt_rlx(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_le(const vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_await_le_acq(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_le_rlx(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_gt(const vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_await_gt_acq(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_gt_rlx(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_ge(const vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_await_ge_acq(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_ge_rlx(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_neq(const vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_await_neq_acq(const vatomic32_t *a,
                                                vuint32_t v);
static inline vuint32_t vatomic32_await_neq_rlx(const vatomic32_t *a,
                                                vuint32_t v);
static inline vuint32_t vatomic32_await_eq(const vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_await_eq_acq(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_eq_rlx(const vatomic32_t *a,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_eq_add(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_eq_add_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_eq_add_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_eq_add_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_eq_sub(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_eq_sub_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_eq_sub_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_eq_sub_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_eq_set(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_eq_set_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_eq_set_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_eq_set_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_neq_add(vatomic32_t *a, vuint32_t c,
                                                vuint32_t v);
static inline vuint32_t vatomic32_await_neq_add_acq(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_neq_add_rel(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_neq_add_rlx(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_neq_sub(vatomic32_t *a, vuint32_t c,
                                                vuint32_t v);
static inline vuint32_t vatomic32_await_neq_sub_acq(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_neq_sub_rel(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_neq_sub_rlx(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_neq_set(vatomic32_t *a, vuint32_t c,
                                                vuint32_t v);
static inline vuint32_t vatomic32_await_neq_set_acq(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_neq_set_rel(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_neq_set_rlx(vatomic32_t *a, vuint32_t c,
                                                    vuint32_t v);
static inline vuint32_t vatomic32_await_lt_add(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_lt_add_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_lt_add_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_lt_add_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_lt_sub(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_lt_sub_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_lt_sub_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_lt_sub_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_lt_set(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_lt_set_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_lt_set_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_lt_set_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_add(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_le_add_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_add_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_add_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_sub(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_le_sub_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_sub_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_sub_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_set(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_le_set_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_set_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_le_set_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_add(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_gt_add_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_add_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_add_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_sub(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_gt_sub_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_sub_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_sub_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_set(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_gt_set_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_set_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_gt_set_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_add(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_ge_add_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_add_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_add_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_sub(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_ge_sub_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_sub_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_sub_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_set(vatomic32_t *a, vuint32_t c,
                                               vuint32_t v);
static inline vuint32_t vatomic32_await_ge_set_acq(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_set_rel(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint32_t vatomic32_await_ge_set_rlx(vatomic32_t *a, vuint32_t c,
                                                   vuint32_t v);
static inline vuint64_t vatomic64_await_lt(const vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_await_lt_acq(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_lt_rlx(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_le(const vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_await_le_acq(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_le_rlx(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_gt(const vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_await_gt_acq(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_gt_rlx(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_ge(const vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_await_ge_acq(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_ge_rlx(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_neq(const vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_await_neq_acq(const vatomic64_t *a,
                                                vuint64_t v);
static inline vuint64_t vatomic64_await_neq_rlx(const vatomic64_t *a,
                                                vuint64_t v);
static inline vuint64_t vatomic64_await_eq(const vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_await_eq_acq(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_eq_rlx(const vatomic64_t *a,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_eq_add(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_eq_add_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_eq_add_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_eq_add_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_eq_sub(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_eq_sub_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_eq_sub_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_eq_sub_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_eq_set(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_eq_set_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_eq_set_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_eq_set_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_neq_add(vatomic64_t *a, vuint64_t c,
                                                vuint64_t v);
static inline vuint64_t vatomic64_await_neq_add_acq(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_neq_add_rel(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_neq_add_rlx(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_neq_sub(vatomic64_t *a, vuint64_t c,
                                                vuint64_t v);
static inline vuint64_t vatomic64_await_neq_sub_acq(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_neq_sub_rel(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_neq_sub_rlx(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_neq_set(vatomic64_t *a, vuint64_t c,
                                                vuint64_t v);
static inline vuint64_t vatomic64_await_neq_set_acq(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_neq_set_rel(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_neq_set_rlx(vatomic64_t *a, vuint64_t c,
                                                    vuint64_t v);
static inline vuint64_t vatomic64_await_lt_add(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_lt_add_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_lt_add_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_lt_add_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_lt_sub(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_lt_sub_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_lt_sub_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_lt_sub_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_lt_set(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_lt_set_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_lt_set_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_lt_set_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_add(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_le_add_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_add_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_add_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_sub(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_le_sub_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_sub_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_sub_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_set(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_le_set_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_set_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_le_set_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_add(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_gt_add_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_add_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_add_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_sub(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_gt_sub_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_sub_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_sub_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_set(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_gt_set_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_set_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_gt_set_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_add(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_ge_add_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_add_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_add_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_sub(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_ge_sub_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_sub_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_sub_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_set(vatomic64_t *a, vuint64_t c,
                                               vuint64_t v);
static inline vuint64_t vatomic64_await_ge_set_acq(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_set_rel(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline vuint64_t vatomic64_await_ge_set_rlx(vatomic64_t *a, vuint64_t c,
                                                   vuint64_t v);
static inline void *vatomicptr_await_neq(const vatomicptr_t *a, void *v);
static inline void *vatomicptr_await_neq_acq(const vatomicptr_t *a, void *v);
static inline void *vatomicptr_await_neq_rlx(const vatomicptr_t *a, void *v);
static inline void *vatomicptr_await_eq(const vatomicptr_t *a, void *v);
static inline void *vatomicptr_await_eq_acq(const vatomicptr_t *a, void *v);
static inline void *vatomicptr_await_eq_rlx(const vatomicptr_t *a, void *v);
static inline void *vatomicptr_await_eq_set(vatomicptr_t *a, void *c, void *v);
static inline void *vatomicptr_await_eq_set_acq(vatomicptr_t *a, void *c,
                                                void *v);
static inline void *vatomicptr_await_eq_set_rel(vatomicptr_t *a, void *c,
                                                void *v);
static inline void *vatomicptr_await_eq_set_rlx(vatomicptr_t *a, void *c,
                                                void *v);
static inline void *vatomicptr_await_neq_set(vatomicptr_t *a, void *c, void *v);
static inline void *vatomicptr_await_neq_set_acq(vatomicptr_t *a, void *c,
                                                 void *v);
static inline void *vatomicptr_await_neq_set_rel(vatomicptr_t *a, void *c,
                                                 void *v);
static inline void *vatomicptr_await_neq_set_rlx(vatomicptr_t *a, void *c,
                                                 void *v);
static inline void vatomic_fence(void);
static inline void vatomic_fence_acq(void);
static inline void vatomic_fence_rel(void);
static inline void vatomic_fence_rlx(void);
static inline void vatomic8_init(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_read(const vatomic8_t *a);
static inline vuint8_t vatomic8_read_acq(const vatomic8_t *a);
static inline vuint8_t vatomic8_read_rlx(const vatomic8_t *a);
static inline void vatomic8_write(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_write_rel(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_write_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_xchg(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_xchg_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_xchg_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_xchg_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_cmpxchg(vatomic8_t *a, vuint8_t e, vuint8_t v);
static inline vuint8_t vatomic8_cmpxchg_acq(vatomic8_t *a, vuint8_t e,
                                            vuint8_t v);
static inline vuint8_t vatomic8_cmpxchg_rel(vatomic8_t *a, vuint8_t e,
                                            vuint8_t v);
static inline vuint8_t vatomic8_cmpxchg_rlx(vatomic8_t *a, vuint8_t e,
                                            vuint8_t v);
static inline vuint8_t vatomic8_get_max(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_max_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_max_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_max_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_max_get(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_max_get_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_max_get_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_max_get_rlx(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_max(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_max_rel(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_max_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_and(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_and_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_and_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_and_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_and_get(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_and_get_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_and_get_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_and_get_rlx(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_and(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_and_rel(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_and_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_or(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_or_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_or_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_or_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_or_get(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_or_get_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_or_get_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_or_get_rlx(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_or(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_or_rel(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_or_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_xor(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_xor_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_xor_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_xor_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_xor_get(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_xor_get_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_xor_get_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_xor_get_rlx(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_xor(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_xor_rel(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_xor_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_add(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_add_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_add_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_add_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_add_get(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_add_get_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_add_get_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_add_get_rlx(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_add(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_add_rel(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_add_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_inc(vatomic8_t *a);
static inline vuint8_t vatomic8_get_inc_acq(vatomic8_t *a);
static inline vuint8_t vatomic8_get_inc_rel(vatomic8_t *a);
static inline vuint8_t vatomic8_get_inc_rlx(vatomic8_t *a);
static inline vuint8_t vatomic8_inc_get(vatomic8_t *a);
static inline vuint8_t vatomic8_inc_get_acq(vatomic8_t *a);
static inline vuint8_t vatomic8_inc_get_rel(vatomic8_t *a);
static inline vuint8_t vatomic8_inc_get_rlx(vatomic8_t *a);
static inline void vatomic8_inc(vatomic8_t *a);
static inline void vatomic8_inc_rel(vatomic8_t *a);
static inline void vatomic8_inc_rlx(vatomic8_t *a);
static inline vuint8_t vatomic8_get_sub(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_sub_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_sub_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_sub_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_sub_get(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_sub_get_acq(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_sub_get_rel(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_sub_get_rlx(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_sub(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_sub_rel(vatomic8_t *a, vuint8_t v);
static inline void vatomic8_sub_rlx(vatomic8_t *a, vuint8_t v);
static inline vuint8_t vatomic8_get_dec(vatomic8_t *a);
static inline vuint8_t vatomic8_get_dec_acq(vatomic8_t *a);
static inline vuint8_t vatomic8_get_dec_rel(vatomic8_t *a);
static inline vuint8_t vatomic8_get_dec_rlx(vatomic8_t *a);
static inline vuint8_t vatomic8_dec_get(vatomic8_t *a);
static inline vuint8_t vatomic8_dec_get_acq(vatomic8_t *a);
static inline vuint8_t vatomic8_dec_get_rel(vatomic8_t *a);
static inline vuint8_t vatomic8_dec_get_rlx(vatomic8_t *a);
static inline void vatomic8_dec(vatomic8_t *a);
static inline void vatomic8_dec_rel(vatomic8_t *a);
static inline void vatomic8_dec_rlx(vatomic8_t *a);
static inline void vatomic16_init(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_read(const vatomic16_t *a);
static inline vuint16_t vatomic16_read_acq(const vatomic16_t *a);
static inline vuint16_t vatomic16_read_rlx(const vatomic16_t *a);
static inline void vatomic16_write(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_write_rel(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_write_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_xchg(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_xchg_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_xchg_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_xchg_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_cmpxchg(vatomic16_t *a, vuint16_t e,
                                          vuint16_t v);
static inline vuint16_t vatomic16_cmpxchg_acq(vatomic16_t *a, vuint16_t e,
                                              vuint16_t v);
static inline vuint16_t vatomic16_cmpxchg_rel(vatomic16_t *a, vuint16_t e,
                                              vuint16_t v);
static inline vuint16_t vatomic16_cmpxchg_rlx(vatomic16_t *a, vuint16_t e,
                                              vuint16_t v);
static inline vuint16_t vatomic16_get_max(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_max_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_max_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_max_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_max_get(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_max_get_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_max_get_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_max_get_rlx(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_max(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_max_rel(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_max_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_and(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_and_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_and_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_and_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_and_get(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_and_get_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_and_get_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_and_get_rlx(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_and(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_and_rel(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_and_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_or(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_or_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_or_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_or_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_or_get(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_or_get_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_or_get_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_or_get_rlx(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_or(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_or_rel(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_or_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_xor(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_xor_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_xor_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_xor_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_xor_get(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_xor_get_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_xor_get_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_xor_get_rlx(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_xor(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_xor_rel(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_xor_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_add(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_add_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_add_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_add_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_add_get(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_add_get_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_add_get_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_add_get_rlx(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_add(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_add_rel(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_add_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_inc(vatomic16_t *a);
static inline vuint16_t vatomic16_get_inc_acq(vatomic16_t *a);
static inline vuint16_t vatomic16_get_inc_rel(vatomic16_t *a);
static inline vuint16_t vatomic16_get_inc_rlx(vatomic16_t *a);
static inline vuint16_t vatomic16_inc_get(vatomic16_t *a);
static inline vuint16_t vatomic16_inc_get_acq(vatomic16_t *a);
static inline vuint16_t vatomic16_inc_get_rel(vatomic16_t *a);
static inline vuint16_t vatomic16_inc_get_rlx(vatomic16_t *a);
static inline void vatomic16_inc(vatomic16_t *a);
static inline void vatomic16_inc_rel(vatomic16_t *a);
static inline void vatomic16_inc_rlx(vatomic16_t *a);
static inline vuint16_t vatomic16_get_sub(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_sub_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_sub_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_sub_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_sub_get(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_sub_get_acq(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_sub_get_rel(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_sub_get_rlx(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_sub(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_sub_rel(vatomic16_t *a, vuint16_t v);
static inline void vatomic16_sub_rlx(vatomic16_t *a, vuint16_t v);
static inline vuint16_t vatomic16_get_dec(vatomic16_t *a);
static inline vuint16_t vatomic16_get_dec_acq(vatomic16_t *a);
static inline vuint16_t vatomic16_get_dec_rel(vatomic16_t *a);
static inline vuint16_t vatomic16_get_dec_rlx(vatomic16_t *a);
static inline vuint16_t vatomic16_dec_get(vatomic16_t *a);
static inline vuint16_t vatomic16_dec_get_acq(vatomic16_t *a);
static inline vuint16_t vatomic16_dec_get_rel(vatomic16_t *a);
static inline vuint16_t vatomic16_dec_get_rlx(vatomic16_t *a);
static inline void vatomic16_dec(vatomic16_t *a);
static inline void vatomic16_dec_rel(vatomic16_t *a);
static inline void vatomic16_dec_rlx(vatomic16_t *a);
static inline void vatomic32_init(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_read(const vatomic32_t *a);
static inline vuint32_t vatomic32_read_acq(const vatomic32_t *a);
static inline vuint32_t vatomic32_read_rlx(const vatomic32_t *a);
static inline void vatomic32_write(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_write_rel(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_write_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_xchg(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_xchg_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_xchg_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_xchg_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_cmpxchg(vatomic32_t *a, vuint32_t e,
                                          vuint32_t v);
static inline vuint32_t vatomic32_cmpxchg_acq(vatomic32_t *a, vuint32_t e,
                                              vuint32_t v);
static inline vuint32_t vatomic32_cmpxchg_rel(vatomic32_t *a, vuint32_t e,
                                              vuint32_t v);
static inline vuint32_t vatomic32_cmpxchg_rlx(vatomic32_t *a, vuint32_t e,
                                              vuint32_t v);
static inline vuint32_t vatomic32_get_max(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_max_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_max_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_max_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_max_get(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_max_get_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_max_get_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_max_get_rlx(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_max(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_max_rel(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_max_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_and(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_and_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_and_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_and_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_and_get(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_and_get_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_and_get_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_and_get_rlx(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_and(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_and_rel(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_and_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_or(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_or_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_or_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_or_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_or_get(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_or_get_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_or_get_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_or_get_rlx(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_or(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_or_rel(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_or_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_xor(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_xor_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_xor_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_xor_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_xor_get(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_xor_get_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_xor_get_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_xor_get_rlx(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_xor(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_xor_rel(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_xor_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_add(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_add_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_add_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_add_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_add_get(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_add_get_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_add_get_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_add_get_rlx(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_add(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_add_rel(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_add_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_inc(vatomic32_t *a);
static inline vuint32_t vatomic32_get_inc_acq(vatomic32_t *a);
static inline vuint32_t vatomic32_get_inc_rel(vatomic32_t *a);
static inline vuint32_t vatomic32_get_inc_rlx(vatomic32_t *a);
static inline vuint32_t vatomic32_inc_get(vatomic32_t *a);
static inline vuint32_t vatomic32_inc_get_acq(vatomic32_t *a);
static inline vuint32_t vatomic32_inc_get_rel(vatomic32_t *a);
static inline vuint32_t vatomic32_inc_get_rlx(vatomic32_t *a);
static inline void vatomic32_inc(vatomic32_t *a);
static inline void vatomic32_inc_rel(vatomic32_t *a);
static inline void vatomic32_inc_rlx(vatomic32_t *a);
static inline vuint32_t vatomic32_get_sub(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_sub_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_sub_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_sub_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_sub_get(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_sub_get_acq(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_sub_get_rel(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_sub_get_rlx(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_sub(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_sub_rel(vatomic32_t *a, vuint32_t v);
static inline void vatomic32_sub_rlx(vatomic32_t *a, vuint32_t v);
static inline vuint32_t vatomic32_get_dec(vatomic32_t *a);
static inline vuint32_t vatomic32_get_dec_acq(vatomic32_t *a);
static inline vuint32_t vatomic32_get_dec_rel(vatomic32_t *a);
static inline vuint32_t vatomic32_get_dec_rlx(vatomic32_t *a);
static inline vuint32_t vatomic32_dec_get(vatomic32_t *a);
static inline vuint32_t vatomic32_dec_get_acq(vatomic32_t *a);
static inline vuint32_t vatomic32_dec_get_rel(vatomic32_t *a);
static inline vuint32_t vatomic32_dec_get_rlx(vatomic32_t *a);
static inline void vatomic32_dec(vatomic32_t *a);
static inline void vatomic32_dec_rel(vatomic32_t *a);
static inline void vatomic32_dec_rlx(vatomic32_t *a);
static inline void vatomic64_init(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_read(const vatomic64_t *a);
static inline vuint64_t vatomic64_read_acq(const vatomic64_t *a);
static inline vuint64_t vatomic64_read_rlx(const vatomic64_t *a);
static inline void vatomic64_write(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_write_rel(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_write_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_xchg(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_xchg_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_xchg_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_xchg_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_cmpxchg(vatomic64_t *a, vuint64_t e,
                                          vuint64_t v);
static inline vuint64_t vatomic64_cmpxchg_acq(vatomic64_t *a, vuint64_t e,
                                              vuint64_t v);
static inline vuint64_t vatomic64_cmpxchg_rel(vatomic64_t *a, vuint64_t e,
                                              vuint64_t v);
static inline vuint64_t vatomic64_cmpxchg_rlx(vatomic64_t *a, vuint64_t e,
                                              vuint64_t v);
static inline vuint64_t vatomic64_get_max(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_max_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_max_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_max_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_max_get(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_max_get_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_max_get_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_max_get_rlx(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_max(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_max_rel(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_max_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_and(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_and_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_and_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_and_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_and_get(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_and_get_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_and_get_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_and_get_rlx(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_and(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_and_rel(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_and_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_or(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_or_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_or_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_or_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_or_get(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_or_get_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_or_get_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_or_get_rlx(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_or(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_or_rel(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_or_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_xor(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_xor_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_xor_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_xor_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_xor_get(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_xor_get_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_xor_get_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_xor_get_rlx(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_xor(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_xor_rel(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_xor_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_add(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_add_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_add_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_add_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_add_get(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_add_get_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_add_get_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_add_get_rlx(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_add(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_add_rel(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_add_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_inc(vatomic64_t *a);
static inline vuint64_t vatomic64_get_inc_acq(vatomic64_t *a);
static inline vuint64_t vatomic64_get_inc_rel(vatomic64_t *a);
static inline vuint64_t vatomic64_get_inc_rlx(vatomic64_t *a);
static inline vuint64_t vatomic64_inc_get(vatomic64_t *a);
static inline vuint64_t vatomic64_inc_get_acq(vatomic64_t *a);
static inline vuint64_t vatomic64_inc_get_rel(vatomic64_t *a);
static inline vuint64_t vatomic64_inc_get_rlx(vatomic64_t *a);
static inline void vatomic64_inc(vatomic64_t *a);
static inline void vatomic64_inc_rel(vatomic64_t *a);
static inline void vatomic64_inc_rlx(vatomic64_t *a);
static inline vuint64_t vatomic64_get_sub(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_sub_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_sub_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_sub_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_sub_get(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_sub_get_acq(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_sub_get_rel(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_sub_get_rlx(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_sub(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_sub_rel(vatomic64_t *a, vuint64_t v);
static inline void vatomic64_sub_rlx(vatomic64_t *a, vuint64_t v);
static inline vuint64_t vatomic64_get_dec(vatomic64_t *a);
static inline vuint64_t vatomic64_get_dec_acq(vatomic64_t *a);
static inline vuint64_t vatomic64_get_dec_rel(vatomic64_t *a);
static inline vuint64_t vatomic64_get_dec_rlx(vatomic64_t *a);
static inline vuint64_t vatomic64_dec_get(vatomic64_t *a);
static inline vuint64_t vatomic64_dec_get_acq(vatomic64_t *a);
static inline vuint64_t vatomic64_dec_get_rel(vatomic64_t *a);
static inline vuint64_t vatomic64_dec_get_rlx(vatomic64_t *a);
static inline void vatomic64_dec(vatomic64_t *a);
static inline void vatomic64_dec_rel(vatomic64_t *a);
static inline void vatomic64_dec_rlx(vatomic64_t *a);
static inline void vatomicsz_init(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_read(const vatomicsz_t *a);
static inline vsize_t vatomicsz_read_acq(const vatomicsz_t *a);
static inline vsize_t vatomicsz_read_rlx(const vatomicsz_t *a);
static inline void vatomicsz_write(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_write_rel(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_write_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_xchg(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_xchg_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_xchg_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_xchg_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_cmpxchg(vatomicsz_t *a, vsize_t e, vsize_t v);
static inline vsize_t vatomicsz_cmpxchg_acq(vatomicsz_t *a, vsize_t e,
                                            vsize_t v);
static inline vsize_t vatomicsz_cmpxchg_rel(vatomicsz_t *a, vsize_t e,
                                            vsize_t v);
static inline vsize_t vatomicsz_cmpxchg_rlx(vatomicsz_t *a, vsize_t e,
                                            vsize_t v);
static inline vsize_t vatomicsz_get_max(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_max_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_max_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_max_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_max_get(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_max_get_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_max_get_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_max_get_rlx(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_max(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_max_rel(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_max_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_and(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_and_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_and_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_and_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_and_get(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_and_get_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_and_get_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_and_get_rlx(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_and(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_and_rel(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_and_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_or(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_or_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_or_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_or_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_or_get(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_or_get_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_or_get_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_or_get_rlx(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_or(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_or_rel(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_or_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_xor(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_xor_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_xor_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_xor_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_xor_get(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_xor_get_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_xor_get_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_xor_get_rlx(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_xor(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_xor_rel(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_xor_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_add(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_add_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_add_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_add_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_add_get(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_add_get_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_add_get_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_add_get_rlx(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_add(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_add_rel(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_add_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_inc(vatomicsz_t *a);
static inline vsize_t vatomicsz_get_inc_acq(vatomicsz_t *a);
static inline vsize_t vatomicsz_get_inc_rel(vatomicsz_t *a);
static inline vsize_t vatomicsz_get_inc_rlx(vatomicsz_t *a);
static inline vsize_t vatomicsz_inc_get(vatomicsz_t *a);
static inline vsize_t vatomicsz_inc_get_acq(vatomicsz_t *a);
static inline vsize_t vatomicsz_inc_get_rel(vatomicsz_t *a);
static inline vsize_t vatomicsz_inc_get_rlx(vatomicsz_t *a);
static inline void vatomicsz_inc(vatomicsz_t *a);
static inline void vatomicsz_inc_rel(vatomicsz_t *a);
static inline void vatomicsz_inc_rlx(vatomicsz_t *a);
static inline vsize_t vatomicsz_get_sub(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_sub_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_sub_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_sub_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_sub_get(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_sub_get_acq(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_sub_get_rel(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_sub_get_rlx(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_sub(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_sub_rel(vatomicsz_t *a, vsize_t v);
static inline void vatomicsz_sub_rlx(vatomicsz_t *a, vsize_t v);
static inline vsize_t vatomicsz_get_dec(vatomicsz_t *a);
static inline vsize_t vatomicsz_get_dec_acq(vatomicsz_t *a);
static inline vsize_t vatomicsz_get_dec_rel(vatomicsz_t *a);
static inline vsize_t vatomicsz_get_dec_rlx(vatomicsz_t *a);
static inline vsize_t vatomicsz_dec_get(vatomicsz_t *a);
static inline vsize_t vatomicsz_dec_get_acq(vatomicsz_t *a);
static inline vsize_t vatomicsz_dec_get_rel(vatomicsz_t *a);
static inline vsize_t vatomicsz_dec_get_rlx(vatomicsz_t *a);
static inline void vatomicsz_dec(vatomicsz_t *a);
static inline void vatomicsz_dec_rel(vatomicsz_t *a);
static inline void vatomicsz_dec_rlx(vatomicsz_t *a);
static inline void vatomicptr_init(vatomicptr_t *a, void *v);
static inline void *vatomicptr_read(const vatomicptr_t *a);
static inline void *vatomicptr_read_acq(const vatomicptr_t *a);
static inline void *vatomicptr_read_rlx(const vatomicptr_t *a);
static inline void vatomicptr_write(vatomicptr_t *a, void *v);
static inline void vatomicptr_write_rel(vatomicptr_t *a, void *v);
static inline void vatomicptr_write_rlx(vatomicptr_t *a, void *v);
static inline void *vatomicptr_xchg(vatomicptr_t *a, void *v);
static inline void *vatomicptr_xchg_acq(vatomicptr_t *a, void *v);
static inline void *vatomicptr_xchg_rel(vatomicptr_t *a, void *v);
static inline void *vatomicptr_xchg_rlx(vatomicptr_t *a, void *v);
static inline void *vatomicptr_cmpxchg(vatomicptr_t *a, void *e, void *v);
static inline void *vatomicptr_cmpxchg_acq(vatomicptr_t *a, void *e, void *v);
static inline void *vatomicptr_cmpxchg_rel(vatomicptr_t *a, void *e, void *v);
static inline void *vatomicptr_cmpxchg_rlx(vatomicptr_t *a, void *e, void *v);
static inline vuint32_t
vatomic32_xchg(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "stlxr  %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline vuint32_t
vatomic32_xchg_acq(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "stxr  %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline vuint32_t
vatomic32_xchg_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "stlxr  %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline vuint32_t
vatomic32_xchg_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "stxr  %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline vuint64_t
vatomic64_xchg(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "stlxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline vuint64_t
vatomic64_xchg_acq(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "stxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline vuint64_t
vatomic64_xchg_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "stlxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline vuint64_t
vatomic64_xchg_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "stxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline void *
vatomicptr_xchg(vatomicptr_t *a, void *v)
{
    void *oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "stlxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline void *
vatomicptr_xchg_acq(vatomicptr_t *a, void *v)
{
    void *oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "stxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline void *
vatomicptr_xchg_rel(vatomicptr_t *a, void *v)
{
    void *oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "stlxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline void *
vatomicptr_xchg_rlx(vatomicptr_t *a, void *v)
{
    void *oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "stxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [a] "Q"(a->_v)
        : "memory");
    return oldv;
}
static inline vuint32_t
vatomic32_cmpxchg(vatomic32_t *a, vuint32_t e, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[exp]\n"
        "b.ne 2f\n"
        "stlxr  %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_cmpxchg_acq(vatomic32_t *a, vuint32_t e, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[exp]\n"
        "b.ne 2f\n"
        "stxr  %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_cmpxchg_rel(vatomic32_t *a, vuint32_t e, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[exp]\n"
        "b.ne 2f\n"
        "stlxr  %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_cmpxchg_rlx(vatomic32_t *a, vuint32_t e, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[exp]\n"
        "b.ne 2f\n"
        "stxr  %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_cmpxchg(vatomic64_t *a, vuint64_t e, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[exp]\n"
        "b.ne 2f\n"
        "stlxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_cmpxchg_acq(vatomic64_t *a, vuint64_t e, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[exp]\n"
        "b.ne 2f\n"
        "stxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_cmpxchg_rel(vatomic64_t *a, vuint64_t e, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[exp]\n"
        "b.ne 2f\n"
        "stlxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_cmpxchg_rlx(vatomic64_t *a, vuint64_t e, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[exp]\n"
        "b.ne 2f\n"
        "stxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline void *
vatomicptr_cmpxchg(vatomicptr_t *a, void *e, void *v)
{
    void *oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[exp]\n"
        "b.ne 2f\n"
        "stlxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline void *
vatomicptr_cmpxchg_acq(vatomicptr_t *a, void *e, void *v)
{
    void *oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[exp]\n"
        "b.ne 2f\n"
        "stxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline void *
vatomicptr_cmpxchg_rel(vatomicptr_t *a, void *e, void *v)
{
    void *oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[exp]\n"
        "b.ne 2f\n"
        "stlxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline void *
vatomicptr_cmpxchg_rlx(vatomicptr_t *a, void *e, void *v)
{
    void *oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[exp]\n"
        "b.ne 2f\n"
        "stxr  %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp)
        : [newv] "r"(v), [exp] "r"(e), [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_max(vatomic32_t *a, vuint32_t v)
{
    vuint32_t tmp;
    vuint32_t oldv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[v]\n"
        "b.hs 2f\n"
        "stlxr %w[tmp], %w[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_max_acq(vatomic32_t *a, vuint32_t v)
{
    vuint32_t tmp;
    vuint32_t oldv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[v]\n"
        "b.hs 2f\n"
        "stxr %w[tmp], %w[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_max_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t tmp;
    vuint32_t oldv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[v]\n"
        "b.hs 2f\n"
        "stlxr %w[tmp], %w[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_max_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t tmp;
    vuint32_t oldv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[v]\n"
        "b.hs 2f\n"
        "stxr %w[tmp], %w[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_max(vatomic64_t *a, vuint64_t v)
{
    vuint32_t tmp;
    vuint64_t oldv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[v]\n"
        "b.hs 2f\n"
        "stlxr %w[tmp], %x[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_max_acq(vatomic64_t *a, vuint64_t v)
{
    vuint32_t tmp;
    vuint64_t oldv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[v]\n"
        "b.hs 2f\n"
        "stxr %w[tmp], %x[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_max_rel(vatomic64_t *a, vuint64_t v)
{
    vuint32_t tmp;
    vuint64_t oldv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[v]\n"
        "b.hs 2f\n"
        "stlxr %w[tmp], %x[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_max_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint32_t tmp;
    vuint64_t oldv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[v]\n"
        "b.hs 2f\n"
        "stxr %w[tmp], %x[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline void
vatomic32_max(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[v]\n"
        "b.hs 2f\n"
        "stlxr  %w[tmp], %w[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_max_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[v]\n"
        "b.hs 2f\n"
        "stlxr  %w[tmp], %w[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_max_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "cmp %w[oldv], %w[v]\n"
        "b.hs 2f\n"
        "stxr  %w[tmp], %w[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_max(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[v]\n"
        "b.hs 2f\n"
        "stlxr  %w[tmp], %x[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_max_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[v]\n"
        "b.hs 2f\n"
        "stlxr  %w[tmp], %x[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_max_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "cmp %x[oldv], %x[v]\n"
        "b.hs 2f\n"
        "stxr  %w[tmp], %x[v], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        "2:\n"
        : [oldv] "=&r"(oldv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline vuint32_t
vatomic32_get_and(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "and %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_or(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "orr %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_xor(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "eor %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_add(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "add %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_sub(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "sub %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_and_acq(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "and %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_or_acq(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "orr %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_xor_acq(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "eor %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_add_acq(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "add %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_sub_acq(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "sub %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_and_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "and %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_or_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "orr %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_xor_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "eor %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_add_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "add %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_sub_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "sub %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_and_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "and %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_or_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "orr %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_xor_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "eor %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_add_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "add %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint32_t
vatomic32_get_sub_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t tmp;
    vuint32_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "sub %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_and(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "and %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_or(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "orr %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_xor(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "eor %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_add(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "add %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_sub(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "sub %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_and_acq(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "and %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_or_acq(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "orr %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_xor_acq(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "eor %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_add_acq(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "add %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_sub_acq(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "sub %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_and_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "and %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_or_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "orr %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_xor_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "eor %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_add_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "add %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_sub_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "sub %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_and_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "and %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_or_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "orr %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_xor_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "eor %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_add_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "add %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline vuint64_t
vatomic64_get_sub_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint32_t tmp;
    vuint64_t newv;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "sub %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
    return oldv;
}
static inline void
vatomic32_and(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "and %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_or(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "orr %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_xor(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "eor %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_add(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "add %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_sub(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %w[oldv], %[a]\n"
        "sub %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_and_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "and %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_or_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "orr %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_xor_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "eor %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_add_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "add %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_sub_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "sub %w[newv], %w[oldv], %w[v]\n"
        "stlxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_and_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "and %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_or_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "orr %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_xor_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "eor %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_add_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "add %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic32_sub_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t oldv;
    vuint32_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %w[oldv], %[a]\n"
        "sub %w[newv], %w[oldv], %w[v]\n"
        "stxr %w[tmp], %w[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_and(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "and %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_or(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "orr %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_xor(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "eor %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_add(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "add %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_sub(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldaxr %x[oldv], %[a]\n"
        "sub %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_and_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "and %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_or_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "orr %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_xor_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "eor %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_add_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "add %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_sub_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "sub %x[newv], %x[oldv], %x[v]\n"
        "stlxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_and_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "and %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_or_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "orr %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_xor_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "eor %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_add_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "add %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic64_sub_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t oldv;
    vuint64_t newv;
    vuint32_t tmp;
    __asm__ volatile(
        "prfm pstl1strm, %[a]\n"
        "1:\n"
        "ldxr %x[oldv], %[a]\n"
        "sub %x[newv], %x[oldv], %x[v]\n"
        "stxr %w[tmp], %x[newv], %[a]\n"
        "cbnz %w[tmp], 1b\n"
        : [oldv] "=&r"(oldv), [newv] "=&r"(newv), [tmp] "=&r"(tmp), [v] "+&r"(v)
        : [a] "Q"(a->_v)
        : "memory", "cc");
}
static inline void
vatomic_fence(void)
{
    __asm__ volatile("dmb ish" ::: "memory");
}
static inline void
vatomic_fence_acq(void)
{
    __asm__ volatile("dmb ishld" ::: "memory");
}
static inline void
vatomic_fence_rel(void)
{
    __asm__ volatile("dmb ish" ::: "memory");
}
static inline void
vatomic_fence_rlx(void)
{
    __asm__ volatile(""::: "memory");
}
static inline vuint32_t
vatomic32_read(const vatomic32_t *a)
{
    vuint32_t val;
    __asm__ volatile("ldar %w[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline vuint32_t
vatomic32_read_acq(const vatomic32_t *a)
{
    vuint32_t val;
    __asm__ volatile("ldar %w[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline vuint32_t
vatomic32_read_rlx(const vatomic32_t *a)
{
    vuint32_t val;
    __asm__ volatile("ldr %w[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline vuint64_t
vatomic64_read(const vatomic64_t *a)
{
    vuint64_t val;
    __asm__ volatile("ldar %x[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline vuint64_t
vatomic64_read_acq(const vatomic64_t *a)
{
    vuint64_t val;
    __asm__ volatile("ldar %x[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline vuint64_t
vatomic64_read_rlx(const vatomic64_t *a)
{
    vuint64_t val;
    __asm__ volatile("ldr %x[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline void *
vatomicptr_read(const vatomicptr_t *a)
{
    void *val;
    __asm__ volatile("ldar %x[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline void *
vatomicptr_read_acq(const vatomicptr_t *a)
{
    void *val;
    __asm__ volatile("ldar %x[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline void *
vatomicptr_read_rlx(const vatomicptr_t *a)
{
    void *val;
    __asm__ volatile("ldr %x[v], %[a]"
                     : [v] "=&r"(val)
                     : [a] "Q"(a->_v)
                     : "memory");
    return val;
}
static inline void
vatomic32_write(vatomic32_t *a, vuint32_t v)
{
    __asm__ volatile("stlr %w[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline void
vatomic32_write_rel(vatomic32_t *a, vuint32_t v)
{
    __asm__ volatile("stlr %w[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline void
vatomic32_write_rlx(vatomic32_t *a, vuint32_t v)
{
    __asm__ volatile("str %w[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline void
vatomic64_write(vatomic64_t *a, vuint64_t v)
{
    __asm__ volatile("stlr %x[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline void
vatomic64_write_rel(vatomic64_t *a, vuint64_t v)
{
    __asm__ volatile("stlr %x[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline void
vatomic64_write_rlx(vatomic64_t *a, vuint64_t v)
{
    __asm__ volatile("str %x[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline void
vatomicptr_write(vatomicptr_t *a, void *v)
{
    __asm__ volatile("stlr %x[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline void
vatomicptr_write_rel(vatomicptr_t *a, void *v)
{
    __asm__ volatile("stlr %x[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline void
vatomicptr_write_rlx(vatomicptr_t *a, void *v)
{
    __asm__ volatile("str %x[v], %[a]"
                     :
                     : [v] "r"(v), [a] "Q"(a->_v)
                     : "memory");
}
static inline vuint32_t
vatomic32_await_eq(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_neq(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_lt(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.lo 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.hs 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_le(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.ls 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.hi 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_gt(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.hi 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.ls 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_ge(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.hs 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.lo 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_eq_acq(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_neq_acq(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_lt_acq(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.lo 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.hs 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_le_acq(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.ls 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.hi 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_gt_acq(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.hi 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.ls 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_ge_acq(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldar %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.hs 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.lo 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_eq_rlx(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldr %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_neq_rlx(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldr %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_lt_rlx(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldr %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.lo 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.hs 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_le_rlx(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldr %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.ls 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.hi 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_gt_rlx(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldr %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.hi 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.ls 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint32_t
vatomic32_await_ge_rlx(const vatomic32_t *a, vuint32_t v)
{
    vuint32_t val;
    __asm__ volatile(
        "ldr %w[val], %[a]\n"
        "cmp %w[val], %w[exp]\n"
        "b.hs 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %w[val], %[a]\n"
        "cmp %w[val],  %w[exp]\n"
        "b.lo 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_eq(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_neq(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_lt(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.lo 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.hs 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_le(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ls 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.hi 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_gt(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.hi 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ls 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_ge(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.hs 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.lo 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_eq_acq(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_neq_acq(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_lt_acq(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.lo 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.hs 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_le_acq(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ls 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.hi 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_gt_acq(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.hi 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ls 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_ge_acq(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.hs 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.lo 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_eq_rlx(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldr %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_neq_rlx(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldr %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_lt_rlx(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldr %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.lo 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.hs 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_le_rlx(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldr %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ls 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.hi 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_gt_rlx(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldr %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.hi 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ls 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint64_t
vatomic64_await_ge_rlx(const vatomic64_t *a, vuint64_t v)
{
    vuint64_t val;
    __asm__ volatile(
        "ldr %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.hs 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.lo 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline void *
vatomicptr_await_eq(const vatomicptr_t *a, void *v)
{
    void *val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline void *
vatomicptr_await_neq(const vatomicptr_t *a, void *v)
{
    void *val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline void *
vatomicptr_await_eq_acq(const vatomicptr_t *a, void *v)
{
    void *val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline void *
vatomicptr_await_neq_acq(const vatomicptr_t *a, void *v)
{
    void *val;
    __asm__ volatile(
        "ldar %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldaxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline void *
vatomicptr_await_eq_rlx(const vatomicptr_t *a, void *v)
{
    void *val;
    __asm__ volatile(
        "ldr %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.eq 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.ne 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline void *
vatomicptr_await_neq_rlx(const vatomicptr_t *a, void *v)
{
    void *val;
    __asm__ volatile(
        "ldr %x[val], %[a]\n"
        "cmp %x[val], %x[exp]\n"
        "b.ne 2f\n"
        "sevl\n"
        ".align 5\n"
        "1:\n"
        "wfe\n"
        "ldxr %x[val], %[a]\n"
        "cmp %x[val],  %x[exp]\n"
        "b.eq 1b\n"
        "2:\n"
        : [val] "=&r"(val)
        : [exp] "r"(v), [a] "Q"(a->_v)
        : "memory", "cc");
    return val;
}
static inline vuint8_t
vatomic8_read(const vatomic8_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = (vuint8_t)__atomic_load_n(&a->_v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_read_acq(const vatomic8_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = (vuint8_t)__atomic_load_n(&a->_v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_read_rlx(const vatomic8_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = (vuint8_t)__atomic_load_n(&a->_v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_read(const vatomic16_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = (vuint16_t)__atomic_load_n(&a->_v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_read_acq(const vatomic16_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = (vuint16_t)__atomic_load_n(&a->_v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_read_rlx(const vatomic16_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = (vuint16_t)__atomic_load_n(&a->_v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_read(const vatomicsz_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = (vsize_t)__atomic_load_n(&a->_v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_read_acq(const vatomicsz_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = (vsize_t)__atomic_load_n(&a->_v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_read_rlx(const vatomicsz_t *a)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = (vsize_t)__atomic_load_n(&a->_v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline void
vatomic8_write(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
}
static inline void
vatomic8_write_rel(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
}
static inline void
vatomic8_write_rlx(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
}
static inline void
vatomic16_write(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
}
static inline void
vatomic16_write_rel(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
}
static inline void
vatomic16_write_rlx(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
}
static inline void
vatomicsz_write(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
}
static inline void
vatomicsz_write_rel(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
}
static inline void
vatomicsz_write_rlx(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    __atomic_store_n(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
}
static inline vuint8_t
vatomic8_xchg(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_exchange_n(&a->_v, (vuint8_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_xchg_acq(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_exchange_n(&a->_v, (vuint8_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_xchg_rel(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_exchange_n(&a->_v, (vuint8_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_xchg_rlx(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_exchange_n(&a->_v, (vuint8_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_xchg(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_exchange_n(&a->_v, (vuint16_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_xchg_acq(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_exchange_n(&a->_v, (vuint16_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_xchg_rel(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_exchange_n(&a->_v, (vuint16_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_xchg_rlx(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_exchange_n(&a->_v, (vuint16_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_xchg(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_exchange_n(&a->_v, (vsize_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_xchg_acq(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_exchange_n(&a->_v, (vsize_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_xchg_rel(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_exchange_n(&a->_v, (vsize_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_xchg_rlx(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_exchange_n(&a->_v, (vsize_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_cmpxchg(vatomic8_t *a, vuint8_t e, vuint8_t v)
{
    vuint8_t exp = (vuint8_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vuint8_t)v, 0, 5,
                                5);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vuint8_t
vatomic8_cmpxchg_acq(vatomic8_t *a, vuint8_t e, vuint8_t v)
{
    vuint8_t exp = (vuint8_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vuint8_t)v, 0, 2,
                                2);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vuint8_t
vatomic8_cmpxchg_rel(vatomic8_t *a, vuint8_t e, vuint8_t v)
{
    vuint8_t exp = (vuint8_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vuint8_t)v, 0, 3,
                                0);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vuint8_t
vatomic8_cmpxchg_rlx(vatomic8_t *a, vuint8_t e, vuint8_t v)
{
    vuint8_t exp = (vuint8_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vuint8_t)v, 0, 0,
                                0);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vuint16_t
vatomic16_cmpxchg(vatomic16_t *a, vuint16_t e, vuint16_t v)
{
    vuint16_t exp = (vuint16_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vuint16_t)v, 0, 5,
                                5);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vuint16_t
vatomic16_cmpxchg_acq(vatomic16_t *a, vuint16_t e, vuint16_t v)
{
    vuint16_t exp = (vuint16_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vuint16_t)v, 0, 2,
                                2);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vuint16_t
vatomic16_cmpxchg_rel(vatomic16_t *a, vuint16_t e, vuint16_t v)
{
    vuint16_t exp = (vuint16_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vuint16_t)v, 0, 3,
                                0);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vuint16_t
vatomic16_cmpxchg_rlx(vatomic16_t *a, vuint16_t e, vuint16_t v)
{
    vuint16_t exp = (vuint16_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vuint16_t)v, 0, 0,
                                0);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vsize_t
vatomicsz_cmpxchg(vatomicsz_t *a, vsize_t e, vsize_t v)
{
    vsize_t exp = (vsize_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vsize_t)v, 0, 5,
                                5);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vsize_t
vatomicsz_cmpxchg_acq(vatomicsz_t *a, vsize_t e, vsize_t v)
{
    vsize_t exp = (vsize_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vsize_t)v, 0, 2,
                                2);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vsize_t
vatomicsz_cmpxchg_rel(vatomicsz_t *a, vsize_t e, vsize_t v)
{
    vsize_t exp = (vsize_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vsize_t)v, 0, 3,
                                0);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vsize_t
vatomicsz_cmpxchg_rlx(vatomicsz_t *a, vsize_t e, vsize_t v)
{
    vsize_t exp = (vsize_t)e;
    __asm__ __volatile__("" ::: "memory");
    __atomic_compare_exchange_n(&a->_v, &exp, (vsize_t)v, 0, 0,
                                0);
    __asm__ __volatile__("" ::: "memory");
    return exp;
}
static inline vuint8_t
vatomic8_get_and(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_and(&a->_v, (vuint8_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_and_acq(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_and(&a->_v, (vuint8_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_and_rel(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_and(&a->_v, (vuint8_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_and_rlx(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_and(&a->_v, (vuint8_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_and(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_and(&a->_v, (vuint16_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_and_acq(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_and(&a->_v, (vuint16_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_and_rel(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_and(&a->_v, (vuint16_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_and_rlx(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_and(&a->_v, (vuint16_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_and(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_and(&a->_v, (vsize_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_and_acq(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_and(&a->_v, (vsize_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_and_rel(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_and(&a->_v, (vsize_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_and_rlx(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_and(&a->_v, (vsize_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_or(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_or(&a->_v, (vuint8_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_or_acq(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_or(&a->_v, (vuint8_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_or_rel(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_or(&a->_v, (vuint8_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_or_rlx(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_or(&a->_v, (vuint8_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_or(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_or(&a->_v, (vuint16_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_or_acq(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_or(&a->_v, (vuint16_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_or_rel(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_or(&a->_v, (vuint16_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_or_rlx(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_or(&a->_v, (vuint16_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_or(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_or(&a->_v, (vsize_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_or_acq(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_or(&a->_v, (vsize_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_or_rel(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_or(&a->_v, (vsize_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_or_rlx(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_or(&a->_v, (vsize_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_xor(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_xor(&a->_v, (vuint8_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_xor_acq(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_xor(&a->_v, (vuint8_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_xor_rel(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_xor(&a->_v, (vuint8_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_xor_rlx(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp =
        (vuint8_t)__atomic_fetch_xor(&a->_v, (vuint8_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_xor(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_xor(&a->_v, (vuint16_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_xor_acq(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_xor(&a->_v, (vuint16_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_xor_rel(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_xor(&a->_v, (vuint16_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_xor_rlx(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp =
        (vuint16_t)__atomic_fetch_xor(&a->_v, (vuint16_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_xor(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_xor(&a->_v, (vsize_t)v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_xor_acq(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_xor(&a->_v, (vsize_t)v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_xor_rel(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_xor(&a->_v, (vsize_t)v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_xor_rlx(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp =
        (vsize_t)__atomic_fetch_xor(&a->_v, (vsize_t)v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_add(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = (vuint8_t)__atomic_fetch_add(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_add_acq(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = (vuint8_t)__atomic_fetch_add(&a->_v, v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_add_rel(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = (vuint8_t)__atomic_fetch_add(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_add_rlx(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = (vuint8_t)__atomic_fetch_add(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_add(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = (vuint16_t)__atomic_fetch_add(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_add_acq(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = (vuint16_t)__atomic_fetch_add(&a->_v, v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_add_rel(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = (vuint16_t)__atomic_fetch_add(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_add_rlx(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = (vuint16_t)__atomic_fetch_add(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_add(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = (vsize_t)__atomic_fetch_add(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_add_acq(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = (vsize_t)__atomic_fetch_add(&a->_v, v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_add_rel(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = (vsize_t)__atomic_fetch_add(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_add_rlx(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = (vsize_t)__atomic_fetch_add(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_sub(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = __atomic_fetch_sub(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_sub_acq(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = __atomic_fetch_sub(&a->_v, v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_sub_rel(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = __atomic_fetch_sub(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_sub_rlx(vatomic8_t *a, vuint8_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint8_t tmp = __atomic_fetch_sub(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_sub(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = __atomic_fetch_sub(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_sub_acq(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = __atomic_fetch_sub(&a->_v, v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_sub_rel(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = __atomic_fetch_sub(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint16_t
vatomic16_get_sub_rlx(vatomic16_t *a, vuint16_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vuint16_t tmp = __atomic_fetch_sub(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_sub(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = __atomic_fetch_sub(&a->_v, v, 5);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_sub_acq(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = __atomic_fetch_sub(&a->_v, v, 2);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_sub_rel(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = __atomic_fetch_sub(&a->_v, v, 3);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vsize_t
vatomicsz_get_sub_rlx(vatomicsz_t *a, vsize_t v)
{
    __asm__ __volatile__("" ::: "memory");
    vsize_t tmp = __atomic_fetch_sub(&a->_v, v, 0);
    __asm__ __volatile__("" ::: "memory");
    return tmp;
}
static inline vuint8_t
vatomic8_get_max(vatomic8_t *a, vuint8_t v)
{
    vuint8_t old = 0;
    vuint8_t cur = vatomic8_read(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomic8_cmpxchg(a, old, v);
    } while (cur != old);
    return old;
}
static inline vuint8_t
vatomic8_get_max_acq(vatomic8_t *a, vuint8_t v)
{
    vuint8_t old = 0;
    vuint8_t cur = vatomic8_read_acq(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomic8_cmpxchg_acq(a, old, v);
    } while (cur != old);
    return old;
}
static inline vuint8_t
vatomic8_get_max_rel(vatomic8_t *a, vuint8_t v)
{
    vuint8_t old = 0;
    vuint8_t cur = vatomic8_read_rlx(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomic8_cmpxchg_rel(a, old, v);
    } while (cur != old);
    return old;
}
static inline vuint8_t
vatomic8_get_max_rlx(vatomic8_t *a, vuint8_t v)
{
    vuint8_t old = 0;
    vuint8_t cur = vatomic8_read_rlx(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomic8_cmpxchg_rlx(a, old, v);
    } while (cur != old);
    return old;
}
static inline vuint16_t
vatomic16_get_max(vatomic16_t *a, vuint16_t v)
{
    vuint16_t old = 0;
    vuint16_t cur = vatomic16_read(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomic16_cmpxchg(a, old, v);
    } while (cur != old);
    return old;
}
static inline vuint16_t
vatomic16_get_max_acq(vatomic16_t *a, vuint16_t v)
{
    vuint16_t old = 0;
    vuint16_t cur = vatomic16_read_acq(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomic16_cmpxchg_acq(a, old, v);
    } while (cur != old);
    return old;
}
static inline vuint16_t
vatomic16_get_max_rel(vatomic16_t *a, vuint16_t v)
{
    vuint16_t old = 0;
    vuint16_t cur = vatomic16_read_rlx(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomic16_cmpxchg_rel(a, old, v);
    } while (cur != old);
    return old;
}
static inline vuint16_t
vatomic16_get_max_rlx(vatomic16_t *a, vuint16_t v)
{
    vuint16_t old = 0;
    vuint16_t cur = vatomic16_read_rlx(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomic16_cmpxchg_rlx(a, old, v);
    } while (cur != old);
    return old;
}
static inline vsize_t
vatomicsz_get_max(vatomicsz_t *a, vsize_t v)
{
    vsize_t old = 0;
    vsize_t cur = vatomicsz_read(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomicsz_cmpxchg(a, old, v);
    } while (cur != old);
    return old;
}
static inline vsize_t
vatomicsz_get_max_acq(vatomicsz_t *a, vsize_t v)
{
    vsize_t old = 0;
    vsize_t cur = vatomicsz_read_acq(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomicsz_cmpxchg_acq(a, old, v);
    } while (cur != old);
    return old;
}
static inline vsize_t
vatomicsz_get_max_rel(vatomicsz_t *a, vsize_t v)
{
    vsize_t old = 0;
    vsize_t cur = vatomicsz_read_rlx(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomicsz_cmpxchg_rel(a, old, v);
    } while (cur != old);
    return old;
}
static inline vsize_t
vatomicsz_get_max_rlx(vatomicsz_t *a, vsize_t v)
{
    vsize_t old = 0;
    vsize_t cur = vatomicsz_read_rlx(a);
    do {
        old = cur;
        if (old >= v) {
            break;
        }
        cur = vatomicsz_cmpxchg_rlx(a, old, v);
    } while (cur != old);
    return old;
}
static inline vuint8_t
vatomic8_max_get(vatomic8_t *a, vuint8_t v)
{
    vuint8_t o = vatomic8_get_max(a, v);
    return o >= v ? o : v;
}
static inline vuint8_t
vatomic8_max_get_acq(vatomic8_t *a, vuint8_t v)
{
    vuint8_t o = vatomic8_get_max_acq(a, v);
    return o >= v ? o : v;
}
static inline vuint8_t
vatomic8_max_get_rel(vatomic8_t *a, vuint8_t v)
{
    vuint8_t o = vatomic8_get_max_rel(a, v);
    return o >= v ? o : v;
}
static inline vuint8_t
vatomic8_max_get_rlx(vatomic8_t *a, vuint8_t v)
{
    vuint8_t o = vatomic8_get_max_rlx(a, v);
    return o >= v ? o : v;
}
static inline vuint16_t
vatomic16_max_get(vatomic16_t *a, vuint16_t v)
{
    vuint16_t o = vatomic16_get_max(a, v);
    return o >= v ? o : v;
}
static inline vuint16_t
vatomic16_max_get_acq(vatomic16_t *a, vuint16_t v)
{
    vuint16_t o = vatomic16_get_max_acq(a, v);
    return o >= v ? o : v;
}
static inline vuint16_t
vatomic16_max_get_rel(vatomic16_t *a, vuint16_t v)
{
    vuint16_t o = vatomic16_get_max_rel(a, v);
    return o >= v ? o : v;
}
static inline vuint16_t
vatomic16_max_get_rlx(vatomic16_t *a, vuint16_t v)
{
    vuint16_t o = vatomic16_get_max_rlx(a, v);
    return o >= v ? o : v;
}
static inline vuint32_t
vatomic32_max_get(vatomic32_t *a, vuint32_t v)
{
    vuint32_t o = vatomic32_get_max(a, v);
    return o >= v ? o : v;
}
static inline vuint32_t
vatomic32_max_get_acq(vatomic32_t *a, vuint32_t v)
{
    vuint32_t o = vatomic32_get_max_acq(a, v);
    return o >= v ? o : v;
}
static inline vuint32_t
vatomic32_max_get_rel(vatomic32_t *a, vuint32_t v)
{
    vuint32_t o = vatomic32_get_max_rel(a, v);
    return o >= v ? o : v;
}
static inline vuint32_t
vatomic32_max_get_rlx(vatomic32_t *a, vuint32_t v)
{
    vuint32_t o = vatomic32_get_max_rlx(a, v);
    return o >= v ? o : v;
}
static inline vuint64_t
vatomic64_max_get(vatomic64_t *a, vuint64_t v)
{
    vuint64_t o = vatomic64_get_max(a, v);
    return o >= v ? o : v;
}
static inline vuint64_t
vatomic64_max_get_acq(vatomic64_t *a, vuint64_t v)
{
    vuint64_t o = vatomic64_get_max_acq(a, v);
    return o >= v ? o : v;
}
static inline vuint64_t
vatomic64_max_get_rel(vatomic64_t *a, vuint64_t v)
{
    vuint64_t o = vatomic64_get_max_rel(a, v);
    return o >= v ? o : v;
}
static inline vuint64_t
vatomic64_max_get_rlx(vatomic64_t *a, vuint64_t v)
{
    vuint64_t o = vatomic64_get_max_rlx(a, v);
    return o >= v ? o : v;
}
static inline vsize_t
vatomicsz_max_get(vatomicsz_t *a, vsize_t v)
{
    vsize_t o = vatomicsz_get_max(a, v);
    return o >= v ? o : v;
}
static inline vsize_t
vatomicsz_max_get_acq(vatomicsz_t *a, vsize_t v)
{
    vsize_t o = vatomicsz_get_max_acq(a, v);
    return o >= v ? o : v;
}
static inline vsize_t
vatomicsz_max_get_rel(vatomicsz_t *a, vsize_t v)
{
    vsize_t o = vatomicsz_get_max_rel(a, v);
    return o >= v ? o : v;
}
static inline vsize_t
vatomicsz_max_get_rlx(vatomicsz_t *a, vsize_t v)
{
    vsize_t o = vatomicsz_get_max_rlx(a, v);
    return o >= v ? o : v;
}
static inline void
vatomic8_max(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_max(a, v);
}
static inline void
vatomic8_max_rel(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_max_rel(a, v);
}
static inline void
vatomic8_max_rlx(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_max_rlx(a, v);
}
static inline void
vatomic16_max(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_max(a, v);
}
static inline void
vatomic16_max_rel(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_max_rel(a, v);
}
static inline void
vatomic16_max_rlx(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_max_rlx(a, v);
}
static inline void
vatomicsz_max(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_max(a, v);
}
static inline void
vatomicsz_max_rel(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_max_rel(a, v);
}
static inline void
vatomicsz_max_rlx(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_max_rlx(a, v);
}
static inline vuint8_t
vatomic8_and_get(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_and(a, v) & v;
}
static inline vuint8_t
vatomic8_and_get_acq(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_and_acq(a, v) & v;
}
static inline vuint8_t
vatomic8_and_get_rel(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_and_rel(a, v) & v;
}
static inline vuint8_t
vatomic8_and_get_rlx(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_and_rlx(a, v) & v;
}
static inline vuint16_t
vatomic16_and_get(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_and(a, v) & v;
}
static inline vuint16_t
vatomic16_and_get_acq(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_and_acq(a, v) & v;
}
static inline vuint16_t
vatomic16_and_get_rel(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_and_rel(a, v) & v;
}
static inline vuint16_t
vatomic16_and_get_rlx(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_and_rlx(a, v) & v;
}
static inline vuint32_t
vatomic32_and_get(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_and(a, v) & v;
}
static inline vuint32_t
vatomic32_and_get_acq(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_and_acq(a, v) & v;
}
static inline vuint32_t
vatomic32_and_get_rel(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_and_rel(a, v) & v;
}
static inline vuint32_t
vatomic32_and_get_rlx(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_and_rlx(a, v) & v;
}
static inline vuint64_t
vatomic64_and_get(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_and(a, v) & v;
}
static inline vuint64_t
vatomic64_and_get_acq(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_and_acq(a, v) & v;
}
static inline vuint64_t
vatomic64_and_get_rel(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_and_rel(a, v) & v;
}
static inline vuint64_t
vatomic64_and_get_rlx(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_and_rlx(a, v) & v;
}
static inline vsize_t
vatomicsz_and_get(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_and(a, v) & v;
}
static inline vsize_t
vatomicsz_and_get_acq(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_and_acq(a, v) & v;
}
static inline vsize_t
vatomicsz_and_get_rel(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_and_rel(a, v) & v;
}
static inline vsize_t
vatomicsz_and_get_rlx(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_and_rlx(a, v) & v;
}
static inline void
vatomic8_and(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_and(a, v);
}
static inline void
vatomic8_and_rel(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_and_rel(a, v);
}
static inline void
vatomic8_and_rlx(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_and_rlx(a, v);
}
static inline void
vatomic16_and(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_and(a, v);
}
static inline void
vatomic16_and_rel(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_and_rel(a, v);
}
static inline void
vatomic16_and_rlx(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_and_rlx(a, v);
}
static inline void
vatomicsz_and(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_and(a, v);
}
static inline void
vatomicsz_and_rel(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_and_rel(a, v);
}
static inline void
vatomicsz_and_rlx(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_and_rlx(a, v);
}
static inline vuint8_t
vatomic8_or_get(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_or(a, v) | v;
}
static inline vuint8_t
vatomic8_or_get_acq(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_or_acq(a, v) | v;
}
static inline vuint8_t
vatomic8_or_get_rel(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_or_rel(a, v) | v;
}
static inline vuint8_t
vatomic8_or_get_rlx(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_or_rlx(a, v) | v;
}
static inline vuint16_t
vatomic16_or_get(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_or(a, v) | v;
}
static inline vuint16_t
vatomic16_or_get_acq(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_or_acq(a, v) | v;
}
static inline vuint16_t
vatomic16_or_get_rel(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_or_rel(a, v) | v;
}
static inline vuint16_t
vatomic16_or_get_rlx(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_or_rlx(a, v) | v;
}
static inline vuint32_t
vatomic32_or_get(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_or(a, v) | v;
}
static inline vuint32_t
vatomic32_or_get_acq(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_or_acq(a, v) | v;
}
static inline vuint32_t
vatomic32_or_get_rel(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_or_rel(a, v) | v;
}
static inline vuint32_t
vatomic32_or_get_rlx(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_or_rlx(a, v) | v;
}
static inline vuint64_t
vatomic64_or_get(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_or(a, v) | v;
}
static inline vuint64_t
vatomic64_or_get_acq(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_or_acq(a, v) | v;
}
static inline vuint64_t
vatomic64_or_get_rel(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_or_rel(a, v) | v;
}
static inline vuint64_t
vatomic64_or_get_rlx(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_or_rlx(a, v) | v;
}
static inline vsize_t
vatomicsz_or_get(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_or(a, v) | v;
}
static inline vsize_t
vatomicsz_or_get_acq(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_or_acq(a, v) | v;
}
static inline vsize_t
vatomicsz_or_get_rel(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_or_rel(a, v) | v;
}
static inline vsize_t
vatomicsz_or_get_rlx(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_or_rlx(a, v) | v;
}
static inline void
vatomic8_or(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_or(a, v);
}
static inline void
vatomic8_or_rel(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_or_rel(a, v);
}
static inline void
vatomic8_or_rlx(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_or_rlx(a, v);
}
static inline void
vatomic16_or(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_or(a, v);
}
static inline void
vatomic16_or_rel(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_or_rel(a, v);
}
static inline void
vatomic16_or_rlx(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_or_rlx(a, v);
}
static inline void
vatomicsz_or(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_or(a, v);
}
static inline void
vatomicsz_or_rel(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_or_rel(a, v);
}
static inline void
vatomicsz_or_rlx(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_or_rlx(a, v);
}
static inline vuint8_t
vatomic8_xor_get(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_xor(a, v) ^ v;
}
static inline vuint8_t
vatomic8_xor_get_acq(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_xor_acq(a, v) ^ v;
}
static inline vuint8_t
vatomic8_xor_get_rel(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_xor_rel(a, v) ^ v;
}
static inline vuint8_t
vatomic8_xor_get_rlx(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_xor_rlx(a, v) ^ v;
}
static inline vuint16_t
vatomic16_xor_get(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_xor(a, v) ^ v;
}
static inline vuint16_t
vatomic16_xor_get_acq(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_xor_acq(a, v) ^ v;
}
static inline vuint16_t
vatomic16_xor_get_rel(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_xor_rel(a, v) ^ v;
}
static inline vuint16_t
vatomic16_xor_get_rlx(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_xor_rlx(a, v) ^ v;
}
static inline vuint32_t
vatomic32_xor_get(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_xor(a, v) ^ v;
}
static inline vuint32_t
vatomic32_xor_get_acq(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_xor_acq(a, v) ^ v;
}
static inline vuint32_t
vatomic32_xor_get_rel(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_xor_rel(a, v) ^ v;
}
static inline vuint32_t
vatomic32_xor_get_rlx(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_xor_rlx(a, v) ^ v;
}
static inline vuint64_t
vatomic64_xor_get(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_xor(a, v) ^ v;
}
static inline vuint64_t
vatomic64_xor_get_acq(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_xor_acq(a, v) ^ v;
}
static inline vuint64_t
vatomic64_xor_get_rel(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_xor_rel(a, v) ^ v;
}
static inline vuint64_t
vatomic64_xor_get_rlx(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_xor_rlx(a, v) ^ v;
}
static inline vsize_t
vatomicsz_xor_get(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_xor(a, v) ^ v;
}
static inline vsize_t
vatomicsz_xor_get_acq(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_xor_acq(a, v) ^ v;
}
static inline vsize_t
vatomicsz_xor_get_rel(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_xor_rel(a, v) ^ v;
}
static inline vsize_t
vatomicsz_xor_get_rlx(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_xor_rlx(a, v) ^ v;
}
static inline void
vatomic8_xor(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_xor(a, v);
}
static inline void
vatomic8_xor_rel(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_xor_rel(a, v);
}
static inline void
vatomic8_xor_rlx(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_xor_rlx(a, v);
}
static inline void
vatomic16_xor(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_xor(a, v);
}
static inline void
vatomic16_xor_rel(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_xor_rel(a, v);
}
static inline void
vatomic16_xor_rlx(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_xor_rlx(a, v);
}
static inline void
vatomicsz_xor(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_xor(a, v);
}
static inline void
vatomicsz_xor_rel(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_xor_rel(a, v);
}
static inline void
vatomicsz_xor_rlx(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_xor_rlx(a, v);
}
static inline vuint8_t
vatomic8_add_get(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_add(a, v) + v;
}
static inline vuint8_t
vatomic8_add_get_acq(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_add_acq(a, v) + v;
}
static inline vuint8_t
vatomic8_add_get_rel(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_add_rel(a, v) + v;
}
static inline vuint8_t
vatomic8_add_get_rlx(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_add_rlx(a, v) + v;
}
static inline vuint16_t
vatomic16_add_get(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_add(a, v) + v;
}
static inline vuint16_t
vatomic16_add_get_acq(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_add_acq(a, v) + v;
}
static inline vuint16_t
vatomic16_add_get_rel(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_add_rel(a, v) + v;
}
static inline vuint16_t
vatomic16_add_get_rlx(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_add_rlx(a, v) + v;
}
static inline vuint32_t
vatomic32_add_get(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_add(a, v) + v;
}
static inline vuint32_t
vatomic32_add_get_acq(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_add_acq(a, v) + v;
}
static inline vuint32_t
vatomic32_add_get_rel(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_add_rel(a, v) + v;
}
static inline vuint32_t
vatomic32_add_get_rlx(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_add_rlx(a, v) + v;
}
static inline vuint64_t
vatomic64_add_get(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_add(a, v) + v;
}
static inline vuint64_t
vatomic64_add_get_acq(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_add_acq(a, v) + v;
}
static inline vuint64_t
vatomic64_add_get_rel(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_add_rel(a, v) + v;
}
static inline vuint64_t
vatomic64_add_get_rlx(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_add_rlx(a, v) + v;
}
static inline vsize_t
vatomicsz_add_get(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_add(a, v) + v;
}
static inline vsize_t
vatomicsz_add_get_acq(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_add_acq(a, v) + v;
}
static inline vsize_t
vatomicsz_add_get_rel(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_add_rel(a, v) + v;
}
static inline vsize_t
vatomicsz_add_get_rlx(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_add_rlx(a, v) + v;
}
static inline void
vatomic8_add(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_add(a, v);
}
static inline void
vatomic8_add_rel(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_add_rel(a, v);
}
static inline void
vatomic8_add_rlx(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_add_rlx(a, v);
}
static inline void
vatomic16_add(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_add(a, v);
}
static inline void
vatomic16_add_rel(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_add_rel(a, v);
}
static inline void
vatomic16_add_rlx(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_add_rlx(a, v);
}
static inline void
vatomicsz_add(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_add(a, v);
}
static inline void
vatomicsz_add_rel(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_add_rel(a, v);
}
static inline void
vatomicsz_add_rlx(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_add_rlx(a, v);
}
static inline vuint8_t
vatomic8_get_inc(vatomic8_t *a)
{
    return vatomic8_get_add(a, 1U);
}
static inline vuint8_t
vatomic8_get_inc_acq(vatomic8_t *a)
{
    return vatomic8_get_add_acq(a, 1U);
}
static inline vuint8_t
vatomic8_get_inc_rel(vatomic8_t *a)
{
    return vatomic8_get_add_rel(a, 1U);
}
static inline vuint8_t
vatomic8_get_inc_rlx(vatomic8_t *a)
{
    return vatomic8_get_add_rlx(a, 1U);
}
static inline vuint16_t
vatomic16_get_inc(vatomic16_t *a)
{
    return vatomic16_get_add(a, 1U);
}
static inline vuint16_t
vatomic16_get_inc_acq(vatomic16_t *a)
{
    return vatomic16_get_add_acq(a, 1U);
}
static inline vuint16_t
vatomic16_get_inc_rel(vatomic16_t *a)
{
    return vatomic16_get_add_rel(a, 1U);
}
static inline vuint16_t
vatomic16_get_inc_rlx(vatomic16_t *a)
{
    return vatomic16_get_add_rlx(a, 1U);
}
static inline vuint32_t
vatomic32_get_inc(vatomic32_t *a)
{
    return vatomic32_get_add(a, 1U);
}
static inline vuint32_t
vatomic32_get_inc_acq(vatomic32_t *a)
{
    return vatomic32_get_add_acq(a, 1U);
}
static inline vuint32_t
vatomic32_get_inc_rel(vatomic32_t *a)
{
    return vatomic32_get_add_rel(a, 1U);
}
static inline vuint32_t
vatomic32_get_inc_rlx(vatomic32_t *a)
{
    return vatomic32_get_add_rlx(a, 1U);
}
static inline vuint64_t
vatomic64_get_inc(vatomic64_t *a)
{
    return vatomic64_get_add(a, 1U);
}
static inline vuint64_t
vatomic64_get_inc_acq(vatomic64_t *a)
{
    return vatomic64_get_add_acq(a, 1U);
}
static inline vuint64_t
vatomic64_get_inc_rel(vatomic64_t *a)
{
    return vatomic64_get_add_rel(a, 1U);
}
static inline vuint64_t
vatomic64_get_inc_rlx(vatomic64_t *a)
{
    return vatomic64_get_add_rlx(a, 1U);
}
static inline vsize_t
vatomicsz_get_inc(vatomicsz_t *a)
{
    return vatomicsz_get_add(a, 1U);
}
static inline vsize_t
vatomicsz_get_inc_acq(vatomicsz_t *a)
{
    return vatomicsz_get_add_acq(a, 1U);
}
static inline vsize_t
vatomicsz_get_inc_rel(vatomicsz_t *a)
{
    return vatomicsz_get_add_rel(a, 1U);
}
static inline vsize_t
vatomicsz_get_inc_rlx(vatomicsz_t *a)
{
    return vatomicsz_get_add_rlx(a, 1U);
}
static inline vuint8_t
vatomic8_inc_get(vatomic8_t *a)
{
    return vatomic8_add_get(a, 1U);
}
static inline vuint8_t
vatomic8_inc_get_acq(vatomic8_t *a)
{
    return vatomic8_add_get_acq(a, 1U);
}
static inline vuint8_t
vatomic8_inc_get_rel(vatomic8_t *a)
{
    return vatomic8_add_get_rel(a, 1U);
}
static inline vuint8_t
vatomic8_inc_get_rlx(vatomic8_t *a)
{
    return vatomic8_add_get_rlx(a, 1U);
}
static inline vuint16_t
vatomic16_inc_get(vatomic16_t *a)
{
    return vatomic16_add_get(a, 1U);
}
static inline vuint16_t
vatomic16_inc_get_acq(vatomic16_t *a)
{
    return vatomic16_add_get_acq(a, 1U);
}
static inline vuint16_t
vatomic16_inc_get_rel(vatomic16_t *a)
{
    return vatomic16_add_get_rel(a, 1U);
}
static inline vuint16_t
vatomic16_inc_get_rlx(vatomic16_t *a)
{
    return vatomic16_add_get_rlx(a, 1U);
}
static inline vuint32_t
vatomic32_inc_get(vatomic32_t *a)
{
    return vatomic32_add_get(a, 1U);
}
static inline vuint32_t
vatomic32_inc_get_acq(vatomic32_t *a)
{
    return vatomic32_add_get_acq(a, 1U);
}
static inline vuint32_t
vatomic32_inc_get_rel(vatomic32_t *a)
{
    return vatomic32_add_get_rel(a, 1U);
}
static inline vuint32_t
vatomic32_inc_get_rlx(vatomic32_t *a)
{
    return vatomic32_add_get_rlx(a, 1U);
}
static inline vuint64_t
vatomic64_inc_get(vatomic64_t *a)
{
    return vatomic64_add_get(a, 1U);
}
static inline vuint64_t
vatomic64_inc_get_acq(vatomic64_t *a)
{
    return vatomic64_add_get_acq(a, 1U);
}
static inline vuint64_t
vatomic64_inc_get_rel(vatomic64_t *a)
{
    return vatomic64_add_get_rel(a, 1U);
}
static inline vuint64_t
vatomic64_inc_get_rlx(vatomic64_t *a)
{
    return vatomic64_add_get_rlx(a, 1U);
}
static inline vsize_t
vatomicsz_inc_get(vatomicsz_t *a)
{
    return vatomicsz_add_get(a, 1U);
}
static inline vsize_t
vatomicsz_inc_get_acq(vatomicsz_t *a)
{
    return vatomicsz_add_get_acq(a, 1U);
}
static inline vsize_t
vatomicsz_inc_get_rel(vatomicsz_t *a)
{
    return vatomicsz_add_get_rel(a, 1U);
}
static inline vsize_t
vatomicsz_inc_get_rlx(vatomicsz_t *a)
{
    return vatomicsz_add_get_rlx(a, 1U);
}
static inline void
vatomic8_inc(vatomic8_t *a)
{
    (void)vatomic8_get_inc(a);
}
static inline void
vatomic8_inc_acq(vatomic8_t *a)
{
    (void)vatomic8_get_inc_acq(a);
}
static inline void
vatomic8_inc_rel(vatomic8_t *a)
{
    (void)vatomic8_get_inc_rel(a);
}
static inline void
vatomic8_inc_rlx(vatomic8_t *a)
{
    (void)vatomic8_get_inc_rlx(a);
}
static inline void
vatomic16_inc(vatomic16_t *a)
{
    (void)vatomic16_get_inc(a);
}
static inline void
vatomic16_inc_acq(vatomic16_t *a)
{
    (void)vatomic16_get_inc_acq(a);
}
static inline void
vatomic16_inc_rel(vatomic16_t *a)
{
    (void)vatomic16_get_inc_rel(a);
}
static inline void
vatomic16_inc_rlx(vatomic16_t *a)
{
    (void)vatomic16_get_inc_rlx(a);
}
static inline void
vatomic32_inc(vatomic32_t *a)
{
    (void)vatomic32_get_inc(a);
}
static inline void
vatomic32_inc_acq(vatomic32_t *a)
{
    (void)vatomic32_get_inc_acq(a);
}
static inline void
vatomic32_inc_rel(vatomic32_t *a)
{
    (void)vatomic32_get_inc_rel(a);
}
static inline void
vatomic32_inc_rlx(vatomic32_t *a)
{
    (void)vatomic32_get_inc_rlx(a);
}
static inline void
vatomic64_inc(vatomic64_t *a)
{
    (void)vatomic64_get_inc(a);
}
static inline void
vatomic64_inc_acq(vatomic64_t *a)
{
    (void)vatomic64_get_inc_acq(a);
}
static inline void
vatomic64_inc_rel(vatomic64_t *a)
{
    (void)vatomic64_get_inc_rel(a);
}
static inline void
vatomic64_inc_rlx(vatomic64_t *a)
{
    (void)vatomic64_get_inc_rlx(a);
}
static inline void
vatomicsz_inc(vatomicsz_t *a)
{
    (void)vatomicsz_get_inc(a);
}
static inline void
vatomicsz_inc_acq(vatomicsz_t *a)
{
    (void)vatomicsz_get_inc_acq(a);
}
static inline void
vatomicsz_inc_rel(vatomicsz_t *a)
{
    (void)vatomicsz_get_inc_rel(a);
}
static inline void
vatomicsz_inc_rlx(vatomicsz_t *a)
{
    (void)vatomicsz_get_inc_rlx(a);
}
static inline vuint8_t
vatomic8_sub_get(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_sub(a, v) - v;
}
static inline vuint8_t
vatomic8_sub_get_acq(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_sub_acq(a, v) - v;
}
static inline vuint8_t
vatomic8_sub_get_rel(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_sub_rel(a, v) - v;
}
static inline vuint8_t
vatomic8_sub_get_rlx(vatomic8_t *a, vuint8_t v)
{
    return vatomic8_get_sub_rlx(a, v) - v;
}
static inline vuint16_t
vatomic16_sub_get(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_sub(a, v) - v;
}
static inline vuint16_t
vatomic16_sub_get_acq(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_sub_acq(a, v) - v;
}
static inline vuint16_t
vatomic16_sub_get_rel(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_sub_rel(a, v) - v;
}
static inline vuint16_t
vatomic16_sub_get_rlx(vatomic16_t *a, vuint16_t v)
{
    return vatomic16_get_sub_rlx(a, v) - v;
}
static inline vuint32_t
vatomic32_sub_get(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_sub(a, v) - v;
}
static inline vuint32_t
vatomic32_sub_get_acq(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_sub_acq(a, v) - v;
}
static inline vuint32_t
vatomic32_sub_get_rel(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_sub_rel(a, v) - v;
}
static inline vuint32_t
vatomic32_sub_get_rlx(vatomic32_t *a, vuint32_t v)
{
    return vatomic32_get_sub_rlx(a, v) - v;
}
static inline vuint64_t
vatomic64_sub_get(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_sub(a, v) - v;
}
static inline vuint64_t
vatomic64_sub_get_acq(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_sub_acq(a, v) - v;
}
static inline vuint64_t
vatomic64_sub_get_rel(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_sub_rel(a, v) - v;
}
static inline vuint64_t
vatomic64_sub_get_rlx(vatomic64_t *a, vuint64_t v)
{
    return vatomic64_get_sub_rlx(a, v) - v;
}
static inline vsize_t
vatomicsz_sub_get(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_sub(a, v) - v;
}
static inline vsize_t
vatomicsz_sub_get_acq(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_sub_acq(a, v) - v;
}
static inline vsize_t
vatomicsz_sub_get_rel(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_sub_rel(a, v) - v;
}
static inline vsize_t
vatomicsz_sub_get_rlx(vatomicsz_t *a, vsize_t v)
{
    return vatomicsz_get_sub_rlx(a, v) - v;
}
static inline void
vatomic8_sub(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_sub(a, v);
}
static inline void
vatomic8_sub_acq(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_sub_acq(a, v);
}
static inline void
vatomic8_sub_rel(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_sub_rel(a, v);
}
static inline void
vatomic8_sub_rlx(vatomic8_t *a, vuint8_t v)
{
    (void)vatomic8_get_sub_rlx(a, v);
}
static inline void
vatomic16_sub(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_sub(a, v);
}
static inline void
vatomic16_sub_acq(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_sub_acq(a, v);
}
static inline void
vatomic16_sub_rel(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_sub_rel(a, v);
}
static inline void
vatomic16_sub_rlx(vatomic16_t *a, vuint16_t v)
{
    (void)vatomic16_get_sub_rlx(a, v);
}
static inline void
vatomic32_sub_acq(vatomic32_t *a, vuint32_t v)
{
    (void)vatomic32_get_sub_acq(a, v);
}
static inline void
vatomic64_sub_acq(vatomic64_t *a, vuint64_t v)
{
    (void)vatomic64_get_sub_acq(a, v);
}
static inline void
vatomicsz_sub(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_sub(a, v);
}
static inline void
vatomicsz_sub_acq(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_sub_acq(a, v);
}
static inline void
vatomicsz_sub_rel(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_sub_rel(a, v);
}
static inline void
vatomicsz_sub_rlx(vatomicsz_t *a, vsize_t v)
{
    (void)vatomicsz_get_sub_rlx(a, v);
}
static inline vuint8_t
vatomic8_get_dec(vatomic8_t *a)
{
    return vatomic8_get_sub(a, 1U);
}
static inline vuint8_t
vatomic8_get_dec_acq(vatomic8_t *a)
{
    return vatomic8_get_sub_acq(a, 1U);
}
static inline vuint8_t
vatomic8_get_dec_rel(vatomic8_t *a)
{
    return vatomic8_get_sub_rel(a, 1U);
}
static inline vuint8_t
vatomic8_get_dec_rlx(vatomic8_t *a)
{
    return vatomic8_get_sub_rlx(a, 1U);
}
static inline vuint16_t
vatomic16_get_dec(vatomic16_t *a)
{
    return vatomic16_get_sub(a, 1U);
}
static inline vuint16_t
vatomic16_get_dec_acq(vatomic16_t *a)
{
    return vatomic16_get_sub_acq(a, 1U);
}
static inline vuint16_t
vatomic16_get_dec_rel(vatomic16_t *a)
{
    return vatomic16_get_sub_rel(a, 1U);
}
static inline vuint16_t
vatomic16_get_dec_rlx(vatomic16_t *a)
{
    return vatomic16_get_sub_rlx(a, 1U);
}
static inline vuint32_t
vatomic32_get_dec(vatomic32_t *a)
{
    return vatomic32_get_sub(a, 1U);
}
static inline vuint32_t
vatomic32_get_dec_acq(vatomic32_t *a)
{
    return vatomic32_get_sub_acq(a, 1U);
}
static inline vuint32_t
vatomic32_get_dec_rel(vatomic32_t *a)
{
    return vatomic32_get_sub_rel(a, 1U);
}
static inline vuint32_t
vatomic32_get_dec_rlx(vatomic32_t *a)
{
    return vatomic32_get_sub_rlx(a, 1U);
}
static inline vuint64_t
vatomic64_get_dec(vatomic64_t *a)
{
    return vatomic64_get_sub(a, 1U);
}
static inline vuint64_t
vatomic64_get_dec_acq(vatomic64_t *a)
{
    return vatomic64_get_sub_acq(a, 1U);
}
static inline vuint64_t
vatomic64_get_dec_rel(vatomic64_t *a)
{
    return vatomic64_get_sub_rel(a, 1U);
}
static inline vuint64_t
vatomic64_get_dec_rlx(vatomic64_t *a)
{
    return vatomic64_get_sub_rlx(a, 1U);
}
static inline vsize_t
vatomicsz_get_dec(vatomicsz_t *a)
{
    return vatomicsz_get_sub(a, 1U);
}
static inline vsize_t
vatomicsz_get_dec_acq(vatomicsz_t *a)
{
    return vatomicsz_get_sub_acq(a, 1U);
}
static inline vsize_t
vatomicsz_get_dec_rel(vatomicsz_t *a)
{
    return vatomicsz_get_sub_rel(a, 1U);
}
static inline vsize_t
vatomicsz_get_dec_rlx(vatomicsz_t *a)
{
    return vatomicsz_get_sub_rlx(a, 1U);
}
static inline vuint8_t
vatomic8_dec_get(vatomic8_t *a)
{
    return vatomic8_sub_get(a, 1U);
}
static inline vuint8_t
vatomic8_dec_get_acq(vatomic8_t *a)
{
    return vatomic8_sub_get_acq(a, 1U);
}
static inline vuint8_t
vatomic8_dec_get_rel(vatomic8_t *a)
{
    return vatomic8_sub_get_rel(a, 1U);
}
static inline vuint8_t
vatomic8_dec_get_rlx(vatomic8_t *a)
{
    return vatomic8_sub_get_rlx(a, 1U);
}
static inline vuint16_t
vatomic16_dec_get(vatomic16_t *a)
{
    return vatomic16_sub_get(a, 1U);
}
static inline vuint16_t
vatomic16_dec_get_acq(vatomic16_t *a)
{
    return vatomic16_sub_get_acq(a, 1U);
}
static inline vuint16_t
vatomic16_dec_get_rel(vatomic16_t *a)
{
    return vatomic16_sub_get_rel(a, 1U);
}
static inline vuint16_t
vatomic16_dec_get_rlx(vatomic16_t *a)
{
    return vatomic16_sub_get_rlx(a, 1U);
}
static inline vuint32_t
vatomic32_dec_get(vatomic32_t *a)
{
    return vatomic32_sub_get(a, 1U);
}
static inline vuint32_t
vatomic32_dec_get_acq(vatomic32_t *a)
{
    return vatomic32_sub_get_acq(a, 1U);
}
static inline vuint32_t
vatomic32_dec_get_rel(vatomic32_t *a)
{
    return vatomic32_sub_get_rel(a, 1U);
}
static inline vuint32_t
vatomic32_dec_get_rlx(vatomic32_t *a)
{
    return vatomic32_sub_get_rlx(a, 1U);
}
static inline vuint64_t
vatomic64_dec_get(vatomic64_t *a)
{
    return vatomic64_sub_get(a, 1U);
}
static inline vuint64_t
vatomic64_dec_get_acq(vatomic64_t *a)
{
    return vatomic64_sub_get_acq(a, 1U);
}
static inline vuint64_t
vatomic64_dec_get_rel(vatomic64_t *a)
{
    return vatomic64_sub_get_rel(a, 1U);
}
static inline vuint64_t
vatomic64_dec_get_rlx(vatomic64_t *a)
{
    return vatomic64_sub_get_rlx(a, 1U);
}
static inline vsize_t
vatomicsz_dec_get(vatomicsz_t *a)
{
    return vatomicsz_sub_get(a, 1U);
}
static inline vsize_t
vatomicsz_dec_get_acq(vatomicsz_t *a)
{
    return vatomicsz_sub_get_acq(a, 1U);
}
static inline vsize_t
vatomicsz_dec_get_rel(vatomicsz_t *a)
{
    return vatomicsz_sub_get_rel(a, 1U);
}
static inline vsize_t
vatomicsz_dec_get_rlx(vatomicsz_t *a)
{
    return vatomicsz_sub_get_rlx(a, 1U);
}
static inline void
vatomic8_dec(vatomic8_t *a)
{
    (void)vatomic8_get_dec(a);
}
static inline void
vatomic8_dec_acq(vatomic8_t *a)
{
    (void)vatomic8_get_dec_acq(a);
}
static inline void
vatomic8_dec_rel(vatomic8_t *a)
{
    (void)vatomic8_get_dec_rel(a);
}
static inline void
vatomic8_dec_rlx(vatomic8_t *a)
{
    (void)vatomic8_get_dec_rlx(a);
}
static inline void
vatomic16_dec(vatomic16_t *a)
{
    (void)vatomic16_get_dec(a);
}
static inline void
vatomic16_dec_acq(vatomic16_t *a)
{
    (void)vatomic16_get_dec_acq(a);
}
static inline void
vatomic16_dec_rel(vatomic16_t *a)
{
    (void)vatomic16_get_dec_rel(a);
}
static inline void
vatomic16_dec_rlx(vatomic16_t *a)
{
    (void)vatomic16_get_dec_rlx(a);
}
static inline void
vatomic32_dec(vatomic32_t *a)
{
    (void)vatomic32_get_dec(a);
}
static inline void
vatomic32_dec_acq(vatomic32_t *a)
{
    (void)vatomic32_get_dec_acq(a);
}
static inline void
vatomic32_dec_rel(vatomic32_t *a)
{
    (void)vatomic32_get_dec_rel(a);
}
static inline void
vatomic32_dec_rlx(vatomic32_t *a)
{
    (void)vatomic32_get_dec_rlx(a);
}
static inline void
vatomic64_dec(vatomic64_t *a)
{
    (void)vatomic64_get_dec(a);
}
static inline void
vatomic64_dec_acq(vatomic64_t *a)
{
    (void)vatomic64_get_dec_acq(a);
}
static inline void
vatomic64_dec_rel(vatomic64_t *a)
{
    (void)vatomic64_get_dec_rel(a);
}
static inline void
vatomic64_dec_rlx(vatomic64_t *a)
{
    (void)vatomic64_get_dec_rlx(a);
}
static inline void
vatomicsz_dec(vatomicsz_t *a)
{
    (void)vatomicsz_get_dec(a);
}
static inline void
vatomicsz_dec_acq(vatomicsz_t *a)
{
    (void)vatomicsz_get_dec_acq(a);
}
static inline void
vatomicsz_dec_rel(vatomicsz_t *a)
{
    (void)vatomicsz_get_dec_rel(a);
}
static inline void
vatomicsz_dec_rlx(vatomicsz_t *a)
{
    (void)vatomicsz_get_dec_rlx(a);
}
static inline void
vatomic8_init(vatomic8_t *a, vuint8_t v)
{
    vatomic8_write(a, v);
}
static inline void
vatomic16_init(vatomic16_t *a, vuint16_t v)
{
    vatomic16_write(a, v);
}
static inline void
vatomic32_init(vatomic32_t *a, vuint32_t v)
{
    vatomic32_write(a, v);
}
static inline void
vatomic64_init(vatomic64_t *a, vuint64_t v)
{
    vatomic64_write(a, v);
}
static inline void
vatomicsz_init(vatomicsz_t *a, vsize_t v)
{
    vatomicsz_write(a, v);
}
static inline void
vatomicptr_init(vatomicptr_t *a, void *v)
{
    vatomicptr_write(a, v);
}
static inline vuint32_t
vatomic32_await_le_add(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_add_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_add_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_add_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_sub(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_sub_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_sub_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_sub_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_set(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_set_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_set_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_le_set_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_add(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_add_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_add_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_add_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_sub(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_sub_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_sub_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_sub_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_set(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_set_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_set_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_lt_set_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_add(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_add_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_add_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_add_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_sub(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_sub_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_sub_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_sub_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_set(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_set_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_set_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_ge_set_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_add(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_add_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_add_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_add_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_sub(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_sub_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_sub_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_sub_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_set(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq(a, cur);
        }
    } while ((old = vatomic32_cmpxchg(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_set_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_acq(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_set_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rel(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_gt_set_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t cur = 0;
    vuint32_t old = vatomic32_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic32_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic32_cmpxchg_rlx(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_add(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_add_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_add_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_add_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_sub(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_sub_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_sub_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_sub_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_set(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_set_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_set_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_le_set_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur <= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_add(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_add_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_add_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_add_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_sub(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_sub_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_sub_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_sub_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_set(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_set_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_set_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_lt_set_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur < c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_add(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_add_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_add_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_add_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_sub(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_sub_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_sub_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_sub_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_set(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_set_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_set_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_ge_set_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur >= c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_add(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_add_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_add_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_add_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, cur + v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_sub(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_sub_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_sub_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_sub_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, cur - v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_set(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq(a, cur);
        }
    } while ((old = vatomic64_cmpxchg(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_set_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_acq(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_set_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rel(a, cur, v)) != cur);
    return old;
}
static inline vuint64_t
vatomic64_await_gt_set_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t cur = 0;
    vuint64_t old = vatomic64_read_rlx(a);
    do {
        cur = old;
        while (!(cur > c)) {
            cur = vatomic64_await_neq_rlx(a, cur);
        }
    } while ((old = vatomic64_cmpxchg_rlx(a, cur, v)) != cur);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_add(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq(a, c);
    } while (vatomic32_cmpxchg(a, old, old + v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_add_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_acq(a, old, old + v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_add_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_rel(a, old, old + v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_add_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_rlx(a, old, old + v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_sub(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq(a, c);
    } while (vatomic32_cmpxchg(a, old, old - v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_sub_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_acq(a, old, old - v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_sub_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_rel(a, old, old - v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_sub_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_rlx(a, old, old - v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_set(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq(a, c);
    } while (vatomic32_cmpxchg(a, old, v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_set_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_acq(a, old, v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_set_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_rel(a, old, v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_neq_set_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    vuint32_t old = 0;
    do {
        old = vatomic32_await_neq_rlx(a, c);
    } while (vatomic32_cmpxchg_rlx(a, old, v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_add(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq(a, c);
    } while (vatomic64_cmpxchg(a, old, old + v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_add_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_acq(a, old, old + v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_add_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_rel(a, old, old + v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_add_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_rlx(a, old, old + v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_sub(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq(a, c);
    } while (vatomic64_cmpxchg(a, old, old - v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_sub_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_acq(a, old, old - v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_sub_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_rel(a, old, old - v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_sub_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_rlx(a, old, old - v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_set(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq(a, c);
    } while (vatomic64_cmpxchg(a, old, v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_set_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_acq(a, old, v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_set_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_rel(a, old, v) != old);
    return old;
}
static inline vuint64_t
vatomic64_await_neq_set_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    vuint64_t old = 0;
    do {
        old = vatomic64_await_neq_rlx(a, c);
    } while (vatomic64_cmpxchg_rlx(a, old, v) != old);
    return old;
}
static inline void *
vatomicptr_await_neq_set(vatomicptr_t *a, void *c, void *v)
{
    void *old = ((void *)0);
    do {
        old = vatomicptr_await_neq(a, c);
    } while (vatomicptr_cmpxchg(a, old, v) != old);
    return old;
}
static inline void *
vatomicptr_await_neq_set_acq(vatomicptr_t *a, void *c, void *v)
{
    void *old = ((void *)0);
    do {
        old = vatomicptr_await_neq_rlx(a, c);
    } while (vatomicptr_cmpxchg_acq(a, old, v) != old);
    return old;
}
static inline void *
vatomicptr_await_neq_set_rel(vatomicptr_t *a, void *c, void *v)
{
    void *old = ((void *)0);
    do {
        old = vatomicptr_await_neq_rlx(a, c);
    } while (vatomicptr_cmpxchg_rel(a, old, v) != old);
    return old;
}
static inline void *
vatomicptr_await_neq_set_rlx(vatomicptr_t *a, void *c, void *v)
{
    void *old = ((void *)0);
    do {
        old = vatomicptr_await_neq_rlx(a, c);
    } while (vatomicptr_cmpxchg_rlx(a, old, v) != old);
    return old;
}
static inline vuint32_t
vatomic32_await_eq_add(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq(a, c);
    } while (vatomic32_cmpxchg(a, c, c + v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_add_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_acq(a, c, c + v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_add_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_rel(a, c, c + v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_add_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_rlx(a, c, c + v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_sub(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq(a, c);
    } while (vatomic32_cmpxchg(a, c, c - v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_sub_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_acq(a, c, c - v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_sub_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_rel(a, c, c - v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_sub_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_rlx(a, c, c - v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_set(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq(a, c);
    } while (vatomic32_cmpxchg(a, c, v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_set_acq(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_acq(a, c, v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_set_rel(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_rel(a, c, v) != c);
    return c;
}
static inline vuint32_t
vatomic32_await_eq_set_rlx(vatomic32_t *a, vuint32_t c, vuint32_t v)
{
    do {
        (void)vatomic32_await_eq_rlx(a, c);
    } while (vatomic32_cmpxchg_rlx(a, c, v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_add(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq(a, c);
    } while (vatomic64_cmpxchg(a, c, c + v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_add_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_acq(a, c, c + v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_add_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_rel(a, c, c + v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_add_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_rlx(a, c, c + v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_sub(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq(a, c);
    } while (vatomic64_cmpxchg(a, c, c - v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_sub_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_acq(a, c, c - v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_sub_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_rel(a, c, c - v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_sub_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_rlx(a, c, c - v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_set(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq(a, c);
    } while (vatomic64_cmpxchg(a, c, v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_set_acq(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_acq(a, c, v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_set_rel(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_rel(a, c, v) != c);
    return c;
}
static inline vuint64_t
vatomic64_await_eq_set_rlx(vatomic64_t *a, vuint64_t c, vuint64_t v)
{
    do {
        (void)vatomic64_await_eq_rlx(a, c);
    } while (vatomic64_cmpxchg_rlx(a, c, v) != c);
    return c;
}
static inline void *
vatomicptr_await_eq_set(vatomicptr_t *a, void *c, void *v)
{
    do {
        (void)vatomicptr_await_eq(a, c);
    } while (vatomicptr_cmpxchg(a, c, v) != c);
    return c;
}
static inline void *
vatomicptr_await_eq_set_acq(vatomicptr_t *a, void *c, void *v)
{
    do {
        (void)vatomicptr_await_eq_rlx(a, c);
    } while (vatomicptr_cmpxchg_acq(a, c, v) != c);
    return c;
}
static inline void *
vatomicptr_await_eq_set_rel(vatomicptr_t *a, void *c, void *v)
{
    do {
        (void)vatomicptr_await_eq_rlx(a, c);
    } while (vatomicptr_cmpxchg_rel(a, c, v) != c);
    return c;
}
static inline void *
vatomicptr_await_eq_set_rlx(vatomicptr_t *a, void *c, void *v)
{
    do {
        (void)vatomicptr_await_eq_rlx(a, c);
    } while (vatomicptr_cmpxchg_rlx(a, c, v) != c);
    return c;
}
