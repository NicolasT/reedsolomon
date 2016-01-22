#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <stddef.h>
#if HAVE_CPUID_H
# include <cpuid.h>
#endif

#include "reedsolomon.h"

#define unlikely(x)     __builtin_expect(!!(x), 0)

#if HAVE_CPUID_H

#if !RS_HAVE_GENERIC
# error Generic routines not available on x86. This is not supported.
#endif

#ifndef bit_AVX2
# define bit_AVX2       (1 << 5)
#endif

#define LOG(s)                          \
        do {                            \
                access(s, F_OK);        \
        } while(0)

#if HAVE_FUNC_ATTRIBUTE_ALWAYS_INLINE
# define ALWAYS_INLINE __attribute__((always_inline))
#else
# define ALWAYS_INLINE
#endif

static inline ALWAYS_INLINE int __get_cpuid_count(
        const unsigned int level,
        const unsigned int count,
        unsigned int *eax,
        unsigned int *ebx,
        unsigned int *ecx,
        unsigned int *edx) {
        unsigned int ext = level & 0x80000000;

        if((unsigned int)__get_cpuid_max(ext, 0) < level) {
                return 0;
        }

        __cpuid_count(level, count, *eax, *ebx, *ecx, *edx);
        return 1;
}

struct cpuid_registers {
        unsigned int eax, ebx, ecx, edx;
};

reedsolomon_cpu_support reedsolomon_determine_cpu_support(void) {
        struct cpuid_registers cpuid1 = { 0, 0, 0, 0 };
        unsigned int rc1 = 0;
        reedsolomon_cpu_support result = REEDSOLOMON_CPU_GENERIC;

        rc1 = __get_cpuid(1, &cpuid1.eax, &cpuid1.ebx, &cpuid1.ecx, &cpuid1.edx);

        if(rc1 == 0) {
                result = REEDSOLOMON_CPU_GENERIC;
        }
        else {
#if RS_HAVE_AVX2
                struct cpuid_registers cpuid7 = { 0, 0, 0, 0 };
                unsigned int rc7 = 0;

                rc7 = __get_cpuid_count(7, 0, &cpuid7.eax, &cpuid7.ebx, &cpuid7.ecx, &cpuid7.edx);
                if(rc7 != 0 && (cpuid7.ebx & bit_AVX2) != 0) {
                        result = REEDSOLOMON_CPU_AVX2;
                }
                else {
#else
                if(1) {
#endif
#if RS_HAVE_AVX
                        if((cpuid1.ecx & bit_AVX) != 0) {
                                result = REEDSOLOMON_CPU_AVX;
                        }
                        else
#endif
#if RS_HAVE_SSSE3
                        if((cpuid1.ecx & bit_SSSE3) != 0) {
                                result = REEDSOLOMON_CPU_SSSE3;
                        }
                        else
#endif
#if RS_HAVE_SSE2
                        if((cpuid1.edx & bit_SSE2) != 0) {
                                result = REEDSOLOMON_CPU_SSE2;
                        }
                        else
#endif
                        {
                                result = REEDSOLOMON_CPU_GENERIC;
                        }
                }
        }

        return result;
}

#define CASE(n, lower, upper)                                   \
        case REEDSOLOMON_CPU_ ## upper: {                       \
                LOG("reedsolomon: using " #n "_" #lower);       \
                result = n ## _ ## lower;                       \
        } break

#if RS_HAVE_SSE2
# define MAYBE_SSE2(n) \
        CASE(n, sse2, SSE2)
#else
# define MAYBE_SSE2(n)
#endif
#if RS_HAVE_SSSE3
# define MAYBE_SSSE3(n) \
        CASE(n, ssse3, SSSE3)
#else
# define MAYBE_SSSE3(n)
#endif
#if RS_HAVE_AVX
# define MAYBE_AVX(n) \
        CASE(n, avx, AVX)
#else
# define MAYBE_AVX(n)
#endif
#if RS_HAVE_AVX2
# define MAYBE_AVX2(n) \
        CASE(n, avx2, AVX2)
#else
# define MAYBE_AVX2(n)
#endif

#define IFUNC(n, proto)                                                                 \
        static proto n ## _ifunc(void) {                                                \
                reedsolomon_cpu_support level = reedsolomon_determine_cpu_support();    \
                proto result = n ## _generic;                                           \
                                                                                        \
                switch(level) {                                                         \
                        MAYBE_AVX2(n);                                                  \
                        MAYBE_AVX(n);                                                   \
                        MAYBE_SSSE3(n);                                                 \
                        MAYBE_SSE2(n);                                                  \
                        CASE(n, generic, GENERIC);                                      \
                        default: {                                                      \
                                LOG("reedsolomon: using " #n "_generic");               \
                                result = n ## _generic;                                 \
                        }                                                               \
                }                                                                       \
                                                                                        \
                return result;                                                          \
        }


#elif RS_HAVE_NEON /* !HAVE_CPUID_H */

reedsolomon_cpu_support reedsolomon_determine_cpu_support(void) {
        return REEDSOLOMON_CPU_NEON;
}

#define IFUNC(n, proto)                         \
        static proto n ## _ifunc(void) {        \
                return n ## _neon;              \
        }

#elif RS_HAVE_ALTIVEC /* !HAVE_CPUID_H && !RS_HAVE_NEON */

reedsolomon_cpu_support reedsolomon_determine_cpu_support(void) {
        return REEDSOLOMON_CPU_ALTIVEC;
}

#define IFUNC(n, proto)                         \
        static proto n ## _ifunc(void) {        \
                return n ## _altivec;           \
        }

#else /* !HAVE_CPUID_H && !RS_HAVE_NEON && !RS_HAVE_ALTIVEC */
#if !RS_HAVE_GENERIC
# error Generic routines not available, and no fallback. This is not supported.
#endif

reedsolomon_cpu_support reedsolomon_determine_cpu_support(void) {
        return REEDSOLOMON_CPU_GENERIC;
}

#define IFUNC(n, proto)                         \
        static proto n ## _ifunc(void) {        \
                return n ## _generic;           \
        }

#endif /* HAVE_CPUID_H */


typedef PROTO_RETURN(*gal_mul_proto)(PROTO_ARGS);
typedef PROTO_RETURN(*gal_mul_xor_proto)(PROTO_ARGS);

IFUNC(reedsolomon_gal_mul, gal_mul_proto)
IFUNC(reedsolomon_gal_mul_xor, gal_mul_xor_proto)

#ifdef HAVE_FUNC_ATTRIBUTE_IFUNC
PROTO(reedsolomon_gal_mul)
        __attribute__((ifunc("reedsolomon_gal_mul_ifunc")));

PROTO(reedsolomon_gal_mul_xor)
        __attribute__((ifunc("reedsolomon_gal_mul_xor_ifunc")));
#else
PROTO(reedsolomon_gal_mul) {
        static gal_mul_proto impl = NULL;

        if(unlikely(impl == NULL)) {
                impl = reedsolomon_gal_mul_ifunc();
        }

        return impl(low, high, in, out, len);
}

PROTO(reedsolomon_gal_mul_xor) {
        static gal_mul_xor_proto impl = NULL;

        if(unlikely(impl == NULL)) {
                impl = reedsolomon_gal_mul_xor_ifunc();
        }

        return impl(low, high, in, out, len);
}
#endif
