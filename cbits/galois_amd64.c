#include <inttypes.h>
#include <immintrin.h>

#include "galois_amd64.h"

#if HAVE_FUNC_ATTRIBUTE_HOT
# define HOT_FUNCTION   __attribute__((hot))
#else
# define HOT_FUNCTION
#endif

#if HAVE_FUNC_ATTRIBUTE_CONST
# define CONST_FUNCTION __attribute__((const))
#else
# define CONST_FUNCTION
#endif

#if HAVE_FUNC_ATTRIBUTE_ALWAYS_INLINE
# define ALWAYS_INLINE  inline __attribute__((always_inline))
#else
# define ALWAYS_INLINE  inline
#endif

#define CONCAT_HELPER(a, b)     a ## b
#define CONCAT(a, b)            CONCAT_HELPER(a, b)

typedef uint8_t v16u8v __attribute__((vector_size(16)));
typedef uint64_t v2u64v __attribute__((vector_size(16)));

#define T(t, n) t n[128 / 8 / sizeof(t)]
#define T1(t, n) t n
typedef union {
        T(uint8_t, u8);
        T(uint64_t, u64);
#if __SSE2__
        T1(__m128i, m128i);
#endif
        T1(v16u8v, v16u8);
        T1(v2u64v, v2u64);
} v128 __attribute__((aligned(16)));
# undef T
# undef T1

#ifdef __AVX2__
typedef __m256i v;
#else
typedef v128 v;
#endif

static ALWAYS_INLINE v128 loadu_v128(const uint8_t *in) {
#if defined(__SSE2__)
        v128 result = { .m128i = _mm_loadu_si128((const __m128i *)in) };
#else
        const uint64_t *in64 = (const uint64_t *)in;
        v128 result = { .u64 = { in64[0]
                               , in64[1]
                               }
                      };
#endif

        return result;
}

static ALWAYS_INLINE v loadu_v(const uint8_t *in) {
#if defined(__AVX2__)
        v result = _mm256_loadu_si256((const v *)in);
#else
        v result = loadu_v128(in);
#endif

        return result;
}

static ALWAYS_INLINE CONST_FUNCTION v set1_epi8_v(const uint8_t c) {
#if defined(__AVX2__)
        v result = _mm256_set1_epi8(c);
#elif defined(__SSE2__)
        v result = { .m128i = _mm_set1_epi8(c) };
#else
        uint64_t c2 = c,
                 tmp = (c2 << (7 * 8)) |
                       (c2 << (6 * 8)) |
                       (c2 << (5 * 8)) |
                       (c2 << (4 * 8)) |
                       (c2 << (3 * 8)) |
                       (c2 << (2 * 8)) |
                       (c2 << (1 * 8)) |
                       (c2 << (0 * 8));
        v result = { .u64 = { tmp, tmp } };
#endif

        return result;
}

static ALWAYS_INLINE CONST_FUNCTION v srli_epi64_v(const v in, const unsigned int n) {
#if defined(__AVX2__)
        v result = _mm256_srli_epi64(in, n);
#elif defined(__SSE2__)
        v result = { .m128i = _mm_srli_epi64(in.m128i, n) };
#else
        v result = { .u64 = { in.u64[0] >> n
                            , in.u64[1] >> n
                            }
                   };
#endif

        return result;
}

static ALWAYS_INLINE CONST_FUNCTION v and_v(const v a, const v b) {
#if defined(__AVX2__)
        v result = _mm256_and_si256(a, b);
#elif defined(__SSE2__)
        v result = { .m128i = _mm_and_si128(a.m128i, b.m128i) };
#else
        v result = { .v2u64 = a.v2u64 & b.v2u64 };
#endif

        return result;
}

static ALWAYS_INLINE CONST_FUNCTION v xor_v(const v a, const v b) {
#if defined(__AVX2__)
        v result = _mm256_xor_si256(a, b);
#elif defined(__SSE2__)
        v result = { .m128i = _mm_xor_si128(a.m128i, b.m128i) };
#else
        v result = { .v2u64 = a.v2u64 ^ b.v2u64 };
#endif

        return result;
}

static ALWAYS_INLINE CONST_FUNCTION v shuffle_epi8_v(const v vec, const v mask) {
#if defined(__AVX2__)
        v result = _mm256_shuffle_epi8(vec, mask);
#elif defined(__SSSE3__)
        v result = { .m128i = _mm_shuffle_epi8(vec.m128i, mask.m128i) };
#else
        v result = { .u64 = { 0, 0 } };

# define DO_BYTE(i) \
        result.u8[i] = mask.u8[i] & 0x80 ? 0 : vec.u8[mask.u8[i] & 0x0F];

        DO_BYTE( 0); DO_BYTE( 1); DO_BYTE( 2); DO_BYTE( 3);
        DO_BYTE( 4); DO_BYTE( 5); DO_BYTE( 6); DO_BYTE( 7);
        DO_BYTE( 8); DO_BYTE( 9); DO_BYTE(10); DO_BYTE(11);
        DO_BYTE(12); DO_BYTE(13); DO_BYTE(14); DO_BYTE(15);
#endif

        return result;
}

static ALWAYS_INLINE void storeu_v(uint8_t *out, const v vec) {
#if defined(__AVX2__)
        _mm256_storeu_si256((__m256i *)out, vec);
#elif defined(__SSE2__)
        _mm_storeu_si128((__m128i *)out, vec.m128i);
#else
        uint64_t *out64 = (uint64_t *)out;

        out64[0] = vec.u64[0];
        out64[1] = vec.u64[1];
#endif
}

static ALWAYS_INLINE CONST_FUNCTION v replicate_v128_v(const v128 vec) {
#if defined(__AVX2__)
        return _mm256_broadcastsi128_si256(vec.m128i);
#else
        return vec;
#endif
}


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

#ifdef HOT
HOT_FUNCTION
#endif
PROTO(CONCAT(reedsolomon_gal_mul_, TARGET)) {
        const v low_mask_unpacked = set1_epi8_v(0x0f);

        const v128 low_vector128 = loadu_v128(low),
                   high_vector128 = loadu_v128(high);
        const v low_vector = replicate_v128_v(low_vector128),
                high_vector = replicate_v128_v(high_vector128);

        const uint8_t *curr_in = in;
        uint8_t *curr_out = out;

        v in_x,
          high_input,
          low_input,
          mul_low_part,
          mul_high_part,
          result;
        size_t x = len / sizeof(v);

        while(x > 0) {
                in_x = loadu_v(curr_in);

                low_input = and_v(in_x, low_mask_unpacked);
                high_input = and_v(srli_epi64_v(in_x, 4), low_mask_unpacked);

                mul_low_part = shuffle_epi8_v(low_vector, low_input);
                mul_high_part = shuffle_epi8_v(high_vector, high_input);

                result = xor_v(mul_low_part, mul_high_part);

                storeu_v(curr_out, result);

                curr_in += sizeof(v);
                curr_out += sizeof(v);
                x--;
        }

        return (curr_in - in);
}

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

#ifdef HOT
HOT_FUNCTION
#endif
PROTO(CONCAT(reedsolomon_gal_mul_xor_, TARGET)) {
        const v low_mask_unpacked = set1_epi8_v(0x0f);

        const v128 low_vector128 = loadu_v128(low),
                   high_vector128 = loadu_v128(high);
        const v low_vector = replicate_v128_v(low_vector128),
                high_vector = replicate_v128_v(high_vector128);

        const uint8_t *curr_in = in;
        uint8_t *curr_out = out;

        v in_x,
          high_input,
          low_input,
          mul_low_part,
          mul_high_part,
          result;
        size_t x = len / sizeof(v);

        while(x > 0) {
                in_x = loadu_v(curr_in);

                low_input = and_v(in_x, low_mask_unpacked);
                high_input = and_v(
                                srli_epi64_v(in_x, 4),
                                low_mask_unpacked);

                mul_low_part = shuffle_epi8_v(low_vector, low_input);
                mul_high_part = shuffle_epi8_v(high_vector, high_input);

                result = xor_v(
                                loadu_v(curr_out),
                                xor_v(mul_low_part, mul_high_part));

                storeu_v(curr_out, result);

                curr_in += sizeof(v);
                curr_out += sizeof(v);
                x--;
        }

        return (curr_in - in);
}
