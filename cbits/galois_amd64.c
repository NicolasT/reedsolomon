/* Copyright 2015, Nicolas Trangez, see LICENSE for details. */

#include <unistd.h>
#include <inttypes.h>
#include <sys/types.h>
#include <x86intrin.h>
#include <cpuid.h>

#if defined(__GNUC__) && !defined(__clang__)
# define TARGET(tgt) __attribute__((target(tgt)))
#else
# define TARGET(tgt)
#endif

//+build !noasm !appengine

// Copyright 2015, Klaus Post, see LICENSE for details.

// Based on http://www.snia.org/sites/default/files2/SDC2013/presentations/NewThinking/EthanMiller_Screaming_Fast_Galois_Field%20Arithmetic_SIMD%20Instructions.pdf
// and http://jerasure.org/jerasure/gf-complete/tree/master

/*
// func galMulSSSE3Xor(low, high, in, out []byte)
TEXT ·galMulSSSE3Xor(SB), 7, $0
    MOVQ    low+0(FP),SI        // SI: &low
    MOVQ    high+24(FP),DX      // DX: &high
    MOVOU  (SI), X6             // X6 low
    MOVOU  (DX), X7             // X7: high
    MOVQ    $15, BX             // BX: low mask
    MOVQ    BX, X8
    PXOR    X5, X5    
    MOVQ    in+48(FP),SI        // R11: &in
    MOVQ    in_len+56(FP),R9    // R9: len(in)
    MOVQ    out+72(FP), DX      // DX: &out
    PSHUFB  X5, X8              // X8: lomask (unpacked)
    SHRQ    $4, R9              // len(in) / 16
    CMPQ    R9 ,$0
    JEQ     done_xor
loopback_xor:
    MOVOU  (SI),X0   // in[x]
    MOVOU  (DX),X4   // out[x]
    MOVOU   X0, X1   // in[x]
    MOVOU   X6, X2   // low copy
    MOVOU   X7, X3   // high copy
    PSRLQ   $4, X1   // X1: high input
    PAND    X8, X0   // X0: low input
    PAND    X8, X1   // X0: high input
    PSHUFB  X0, X2   // X2: mul low part
    PSHUFB  X1, X3   // X3: mul high part
    PXOR    X2, X3   // X3: Result
    PXOR    X4, X3   // X3: Result xor existing out
    MOVOU   X3, (DX) // Store
    ADDQ    $16, SI  // in+=16
    ADDQ    $16, DX  // out+=16 
    SUBQ    $1, R9
    JNZ     loopback_xor
done_xor:
    RET
*/

#define PROTO_RETURN size_t
#define PROTO_ARGS                              \
        const uint8_t _low[16],                 \
        const uint8_t _high[16],                \
        const uint8_t *restrict const _in,      \
        uint8_t *restrict const _out,           \
        const size_t len
#define PROTO(target, name) \
        PROTO_RETURN \
        TARGET(#target) \
        __attribute__((nonnull)) \
        name (PROTO_ARGS)

#define TEMPLATE                                                        \
        const V128 * const low = (const V128 *)_low,                    \
                   * const high = (const V128 *)_high;                  \
        const V * in = (const V *)_in;                                  \
        V *out = (V *)_out;                                             \
                                                                        \
        const V low_mask_unpacked = LOW_MASK_UNPACKED;                  \
        const V128 low_vector128 = LOAD_V128(low),                      \
                   high_vector128 = LOAD_V128(high);                    \
        const V low_vector = REPLICATE_V128(low_vector128),             \
                high_vector = REPLICATE_V128(high_vector128);           \
        V in_x = { 0 },                                                 \
          high_input = { 0 },                                           \
          low_input = { 0 },                                            \
          mul_low_part = { 0 },                                         \
          mul_high_part = { 0 },                                        \
          result = { 0 };                                               \
        size_t x = len / sizeof(V);                                     \
                                                                        \
        while(x > 0) {                                                  \
                in_x = LOAD_V(in);                                      \
                high_input = SHIFT_RIGHT_V(in_x, 4);                    \
                low_input = AND_V(in_x, low_mask_unpacked);             \
                high_input = AND_V(high_input, low_mask_unpacked);      \
                mul_low_part = SHUFFLE_V(low_vector, low_input);        \
                mul_high_part = SHUFFLE_V(high_vector, high_input);     \
                result = XOR_V(mul_low_part, mul_high_part);            \
                result = POSTPROCESS(LOAD_V(out), result);              \
                STORE_V(out, result);                                   \
                in++;                                                   \
                out++;                                                  \
                x--;                                                    \
        }                                                               \
                                                                        \
        return ((const uint8_t *)in - _in);                             \

/*
// func galMulSSSE3(low, high, in, out []byte)
TEXT ·galMulSSSE3(SB), 7, $0
    MOVQ    low+0(FP),SI        // SI: &low
    MOVQ    high+24(FP),DX      // DX: &high
    MOVOU   (SI), X6            // X6 low
    MOVOU   (DX), X7            // X7: high
    MOVQ    $15, BX             // BX: low mask
    MOVQ    BX, X8
    PXOR    X5, X5    
    MOVQ    in+48(FP),SI        // R11: &in
    MOVQ    in_len+56(FP),R9    // R9: len(in)
    MOVQ    out+72(FP), DX      // DX: &out
    PSHUFB  X5, X8              // X8: lomask (unpacked)
    SHRQ    $4, R9              // len(in) / 16
    CMPQ    R9 ,$0
    JEQ     done
loopback:
    MOVOU  (SI),X0   // in[x]
    MOVOU   X0, X1   // in[x]
    MOVOU   X6, X2   // low copy
    MOVOU   X7, X3   // high copy
    PSRLQ   $4, X1   // X1: high input
    PAND    X8, X0   // X0: low input
    PAND    X8, X1   // X0: high input
    PSHUFB  X0, X2   // X2: mul low part
    PSHUFB  X1, X3   // X3: mul high part
    PXOR    X2, X3   // X3: Result
    MOVOU   X3, (DX) // Store
    ADDQ    $16, SI  // in+=16
    ADDQ    $16, DX  // out+=16 
    SUBQ    $1, R9
    JNZ     loopback
done:
    RET

*/

/* Hand-rolled AVX-based implementations */
#define V                       __m128i
#define V128                    __m128i
#define LOAD_V(a)               _mm_loadu_si128(a)
#define LOAD_V128(a)            _mm_loadu_si128(a)
#define LOW_MASK_UNPACKED       _mm_set1_epi8(0x0f)
#define SHIFT_RIGHT_V(v, n)     _mm_srli_epi64(v, n)
#define AND_V(a, b)             _mm_and_si128(a, b)
#define SHUFFLE_V(v, o)         _mm_shuffle_epi8(v, o)
#define XOR_V(a, b)             _mm_xor_si128(a, b)
#define STORE_V(l, v)           _mm_storeu_si128(l, v)
#define REPLICATE_V128(v)       (v)
__attribute__((hot)) PROTO(avx, reedsolomon_gal_mul_avx_opt) {
#define POSTPROCESS(old, result)        result
        TEMPLATE
#undef POSTPROCESS
}
__attribute__((hot)) PROTO(avx, reedsolomon_gal_mul_xor_avx_opt) {
#define POSTPROCESS(old, result)        _mm_xor_si128(old, result)
        TEMPLATE
#undef POSTPROCESS
}
#undef V
#undef V128
#undef LOAD_V
#undef LOAD_V128
#undef LOW_MASK_UNPACKED
#undef SHIFT_RIGHT_V
#undef AND_V
#undef SHUFFLE_V
#undef XOR_V
#undef STORE_V
#undef REPLICATE_V128

/* We only support the AVX version for now on Windows.
 * Rationale: MinGHC comes with GCC 4.5.2, which is... rather old.
 * Instead of turning the code below into a conditional-compilation-mess, keep
 * things easy (well, easier) by only providing one version.
 */
#if !defined(_WIN32) && !defined(__clang__)

/* Hand-rolled AVX2-based implementation */
#define V                       __m256i
#define V128                    __m128i
#define LOAD_V(a)               _mm256_loadu_si256(a)
#define LOAD_V128(a)            _mm_loadu_si128(a)
#define LOW_MASK_UNPACKED       _mm256_set1_epi8(0x0f)
#define SHIFT_RIGHT_V(v, n)     _mm256_srli_epi64(v, n)
#define AND_V(a, b)             _mm256_and_si256(a, b)
#define SHUFFLE_V(v, o)         _mm256_shuffle_epi8(v, o)
#define XOR_V(a, b)             _mm256_xor_si256(a, b)
#define STORE_V(l, v)           _mm256_storeu_si256(l, v)
#define REPLICATE_V128(v)       _mm256_broadcastsi128_si256(v)
__attribute__((hot)) PROTO(avx2, reedsolomon_gal_mul_avx2_opt) {
#define POSTPROCESS(old, result)        result
        TEMPLATE
#undef POSTPROCESS
}
__attribute__((hot)) PROTO(avx2, reedsolomon_gal_mul_xor_avx2_opt) {
#define POSTPROCESS(old, result)        _mm256_xor_si256(old, result)
        TEMPLATE
#undef POSTPROCESS
}
#undef V
#undef V128
#undef LOAD_V
#undef LOAD_V128
#undef LOW_MASK_UNPACKED
#undef SHIFT_RIGHT_V
#undef AND_V
#undef SHUFFLE_V
#undef XOR_V
#undef STORE_V
#undef REPLICATE_V128

/* Compiler-generated implementations for various targets */
typedef uint8_t v16uc __attribute__((vector_size(16)));

#define V                       v16uc
#define V128                    v16uc
#define LOAD_V(a)               (*a)
#define LOAD_V128(a)            (*a)
#define LOW_MASK_UNPACKED       { 0x0f, 0x0f, 0x0f, 0x0f,       \
                                  0x0f, 0x0f, 0x0f, 0x0f,       \
                                  0x0f, 0x0f, 0x0f, 0x0f,       \
                                  0x0f, 0x0f, 0x0f, 0x0f }
#define SHIFT_RIGHT_V(v, n)     (v >> n)
#define AND_V(a, b)             (a & b)
#define SHUFFLE_V(v, o)         __builtin_shuffle(v, o)
#define XOR_V(a, b)             (a ^ b)
#define STORE_V(l, v)           (*l = v)
#define REPLICATE_V128(v)       (v)

#define POSTPROCESS(old, result)        result
PROTO(avx, reedsolomon_gal_mul_avx) { TEMPLATE }
PROTO(sse4.1, reedsolomon_gal_mul_sse_4_1) { TEMPLATE }
PROTO(default, reedsolomon_gal_mul_generic) { TEMPLATE }
#undef POSTPROCESS

#define POSTPROCESS(old, result)        (old ^ result)
PROTO(avx, reedsolomon_gal_mul_xor_avx) { TEMPLATE }
PROTO(sse4.1, reedsolomon_gal_mul_xor_sse_4_1) { TEMPLATE }
PROTO(default, reedsolomon_gal_mul_xor_generic) { TEMPLATE }
#undef POSTPROCESS

#undef V
#undef LOAD_V
#undef LOW_MASK_UNPACKED
#undef SHIFT_RIGHT_V
#undef AND_V
#undef SHUFFLE_V
#undef XOR_V
#undef STORE_V
#undef REPLICATE_V128

PROTO_RETURN reedsolomon_gal_mul(PROTO_ARGS)
        __attribute__((ifunc("reedsolomon_gal_mul_ifunc")));
PROTO_RETURN reedsolomon_gal_mul_xor(PROTO_ARGS)
        __attribute__((ifunc("reedsolomon_gal_mul_xor_ifunc")));

#define LOG(s)                          \
        do {                            \
                access(s, F_OK);        \
        } while(0)

static inline __attribute__((always_inline)) int __get_cpuid_count(
        const unsigned int level,
        const unsigned int count,
        unsigned int *eax,
        unsigned int *ebx,
        unsigned int *ecx,
        unsigned int *edx) {
        unsigned int ext = level & 0x80000000;

        if(__get_cpuid_max(ext, 0) < level) {
                return 0;
        }

        __cpuid_count(level, count, *eax, *ebx, *ecx, *edx);
        return 1;
}

#define IFUNC(n, proto)                                                                                         \
        proto n ## _ifunc(void) {                                                                               \
                struct {                                                                                        \
                        unsigned int eax, ebx, ecx, edx;                                                        \
                } cpuid1 = { 0, 0, 0, 0 },                                                                      \
                  cpuid7 = { 0, 0, 0, 0 };                                                                      \
                int rc = 0;                                                                                     \
                proto func = NULL;                                                                              \
                                                                                                                \
                rc = __get_cpuid(1, &cpuid1.eax, &cpuid1.ebx, &cpuid1.ecx, &cpuid1.edx);                        \
                if(rc == 0) {                                                                                   \
                        LOG("reedsolomon: cpuid failed, using " #n "_generic");                                 \
                        func = n ## _generic;                                                                   \
                }                                                                                               \
                else {                                                                                          \
                        rc = __get_cpuid_count(7, 0, &cpuid7.eax, &cpuid7.ebx, &cpuid7.ecx, &cpuid7.edx);       \
                                                                                                                \
                        if(rc != 0 && (cpuid7.ebx & bit_AVX2) != 0) {                                           \
                                LOG("reedsolomon: using " #n "_avx2_opt");                                      \
                                func = n ## _avx2_opt;                                                          \
                        }                                                                                       \
                        else if((cpuid1.ecx & bit_AVX) != 0) {                                                  \
                                LOG("reedsolomon: using " #n "_avx_opt");                                       \
                                func = n ## _avx_opt;                                                           \
                        }                                                                                       \
                        else if((cpuid1.ecx & bit_SSE4_1) != 0) {                                               \
                                LOG("reedsolomon: using " #n "_sse_4_1");                                       \
                                func = n ## _sse_4_1;                                                           \
                        }                                                                                       \
                        else {                                                                                  \
                                LOG("reedsolomon: using " #n "_generic");                                       \
                                func = n ## _generic;                                                           \
                        }                                                                                       \
                }                                                                                               \
                                                                                                                \
                return func;                                                                                    \
        }

typedef PROTO_RETURN(*gal_mul_proto)(PROTO_ARGS);
typedef PROTO_RETURN(*gal_mul_xor_proto)(PROTO_ARGS);

IFUNC(reedsolomon_gal_mul, gal_mul_proto)
IFUNC(reedsolomon_gal_mul_xor, gal_mul_xor_proto)
#else /* if !defined(_WIN32) && !defined(__clang__) */
PROTO_RETURN reedsolomon_gal_mul(PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}
PROTO_RETURN reedsolomon_gal_mul_avx2_opt(PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}
PROTO_RETURN reedsolomon_gal_mul_avx(PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}
PROTO_RETURN reedsolomon_gal_mul_sse_4_1(PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}
PROTO_RETURN reedsolomon_gal_mul_generic(PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}

PROTO_RETURN reedsolomon_gal_mul_xor(PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
PROTO_RETURN reedsolomon_gal_mul_xor_avx2_opt(PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
PROTO_RETURN reedsolomon_gal_mul_xor_avx(PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
PROTO_RETURN reedsolomon_gal_mul_xor_sse_4_1(PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
PROTO_RETURN reedsolomon_gal_mul_xor_generic(PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
#endif
