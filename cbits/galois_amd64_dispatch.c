#include <stddef.h>
#include <cpuid.h>

#include "config.h"
#include "galois_amd64.h"

#ifndef bit_AVX2
# define bit_AVX2       (1 << 5)
#endif

#define unlikely(x)     __builtin_expect(!!(x), 0)

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

#if RS_HAVE_AVX2
# define IFUNC_AVX2(n, result)                                                                          \
        do {                                                                                            \
                int avx2_rc = 0;                                                                        \
                struct {                                                                                \
                        unsigned int eax, ebx, ecx, edx;                                                \
                } cpuid7 = { 0, 0, 0, 0 };                                                              \
                                                                                                        \
                avx2_rc = __get_cpuid_count(7, 0, &cpuid7.eax, &cpuid7.ebx, &cpuid7.ecx, &cpuid7.edx);  \
                                                                                                        \
                if(avx2_rc != 0 && (cpuid7.ebx & bit_AVX2) != 0) {                                      \
                        LOG("reedsolomon: using " #n "_avx2");                                          \
                        result = n ## _avx2;                                                            \
                }                                                                                       \
        } while(0)
#else
# define IFUNC_AVX2(n, result)
#endif

#define IFUNC(n, proto)                                                                         \
        static proto n ## _ifunc(void) {                                                        \
                struct {                                                                        \
                        unsigned int eax, ebx, ecx, edx;                                        \
                } cpuid1 = { 0, 0, 0, 0 };                                                      \
                int rc = 0;                                                                     \
                proto func = NULL;                                                              \
                                                                                                \
                rc = __get_cpuid(1, &cpuid1.eax, &cpuid1.ebx, &cpuid1.ecx, &cpuid1.edx);        \
                if(rc == 0) {                                                                   \
                        LOG("reedsolomon: cpuid failed, using " #n "_generic");                 \
                        func = n ## _generic;                                                   \
                }                                                                               \
                else {                                                                          \
                        IFUNC_AVX2(n, func);                                                    \
                        if(func == NULL) {                                                      \
                                if((cpuid1.ecx & bit_SSSE3) != 0) {                             \
                                        LOG("reedsolomon: using " #n "_ssse3");                 \
                                        func = n ## _ssse3;                                     \
                                }                                                               \
                                else if((cpuid1.edx & bit_SSE2) != 0) {                         \
                                        LOG("reedsolomon: using " #n "_sse2");                  \
                                        func = n ## _sse2;                                      \
                                }                                                               \
                                else {                                                          \
                                        LOG("reedsolomon: using " #n "_generic");               \
                                        func = n ## _generic;                                   \
                                }                                                               \
                        }                                                                       \
                }                                                                               \
                                                                                                \
                return func;                                                                    \
        }

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
