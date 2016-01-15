#include <unistd.h>
#include <stdint.h>

#if HAVE_CONFIG_H
# include "config.h"
#endif

#define PROTO_RETURN size_t
#define PROTO_ARGS                              \
        const uint8_t low[16],                  \
        const uint8_t high[16],                 \
        const uint8_t *restrict const in,       \
        uint8_t *restrict const out,            \
        const size_t len
#define PROTO(name)                     \
        PROTO_RETURN                    \
        __attribute__((nonnull))        \
        name (PROTO_ARGS)

#if RS_HAVE_AVX2
PROTO(reedsolomon_gal_mul_avx2);
PROTO(reedsolomon_gal_mul_xor_avx2);
#endif
#if RS_HAVE_AVX
PROTO(reedsolomon_gal_mul_avx);
PROTO(reedsolomon_gal_mul_xor_avx);
#endif
#if RS_HAVE_SSSE3
PROTO(reedsolomon_gal_mul_ssse3);
PROTO(reedsolomon_gal_mul_xor_ssse3);
#endif
#if RS_HAVE_SSE2
PROTO(reedsolomon_gal_mul_sse2);
PROTO(reedsolomon_gal_mul_xor_sse2);
#endif
#if RS_HAVE_GENERIC
PROTO(reedsolomon_gal_mul_generic);
PROTO(reedsolomon_gal_mul_xor_generic);
#endif

PROTO(reedsolomon_gal_mul);
PROTO(reedsolomon_gal_mul_xor);

typedef enum {
        REEDSOLOMON_CPU_GENERIC = 0,
        REEDSOLOMON_CPU_SSE2 = 1,
        REEDSOLOMON_CPU_SSSE3 = 2,
        REEDSOLOMON_CPU_AVX = 3,
        REEDSOLOMON_CPU_AVX2 = 4,
} reedsolomon_cpu_support;

reedsolomon_cpu_support reedsolomon_determine_cpu_support(void);
