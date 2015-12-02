#include <unistd.h>
#include <stdint.h>

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

PROTO(reedsolomon_gal_mul_avx2);
PROTO(reedsolomon_gal_mul_xor_avx2);
PROTO(reedsolomon_gal_mul_avx);
PROTO(reedsolomon_gal_mul_xor_avx);
PROTO(reedsolomon_gal_mul_ssse3);
PROTO(reedsolomon_gal_mul_xor_ssse3);
PROTO(reedsolomon_gal_mul_sse2);
PROTO(reedsolomon_gal_mul_xor_sse2);
PROTO(reedsolomon_gal_mul_generic);
PROTO(reedsolomon_gal_mul_xor_generic);

PROTO(reedsolomon_gal_mul);
PROTO(reedsolomon_gal_mul_xor);
