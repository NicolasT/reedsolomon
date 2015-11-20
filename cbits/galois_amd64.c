/* Copyright 2015, Nicolas Trangez, see LICENSE for details. */

#include <unistd.h>
#include <inttypes.h>
#include <sys/types.h>
#include <x86intrin.h>
#include <cpuid.h>

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

#define GAL_MUL_XOR_PROTO_RETURN size_t
#define GAL_MUL_XOR_PROTO_ARGS                  \
        const uint8_t _low[16],                 \
        const uint8_t _high[16],                \
        const uint8_t *restrict const _in,      \
        uint8_t *restrict const _out,           \
        const size_t len                        \

#if defined(__GNUC__) && !defined(__clang__)
# define TARGET(tgt) __attribute__((target(tgt)))
#else
# define TARGET(tgt)
#endif

#define GAL_MUL_XOR_PROTO(tgt, name)                                    \
        GAL_MUL_XOR_PROTO_RETURN                                        \
        TARGET(#tgt)                                                    \
        __attribute__((nonnull))                                        \
        reedsolomon_gal_mul_xor_ ##name (GAL_MUL_XOR_PROTO_ARGS)

#define GAL_MUL_XOR_IMPL                                                \
        const V * const low = (const V *)_low,                          \
                * const high = (const V *)_high,                        \
                * in = (const V *)_in;                                  \
        V *out = (V *)_out;                                             \
                                                                        \
        const V low_mask_unpacked = LOW_MASK_UNPACKED,                  \
                low_vector = LOAD_V(low),                               \
                high_vector = LOAD_V(high);                             \
        V in_x = { 0 },                                                 \
          high_input = { 0 },                                           \
          low_input = { 0 },                                            \
          mul_low_part = { 0 },                                         \
          mul_high_part = { 0 },                                        \
          result = { 0 },                                               \
          out_x = { 0 };                                                \
        size_t x = len / 16;                                            \
                                                                        \
        while(x > 0) {                                                  \
                in_x = LOAD_V(in);                                      \
                high_input = SHIFT_RIGHT_V(in_x, 4);                    \
                low_input = AND_V(in_x, low_mask_unpacked);             \
                high_input = AND_V(high_input, low_mask_unpacked);      \
                mul_low_part = SHUFFLE_V(low_vector, low_input);        \
                mul_high_part = SHUFFLE_V(high_vector, high_input);     \
                result = XOR_V(mul_low_part, mul_high_part);            \
                out_x = LOAD_V(out);                                    \
                result = XOR_V(out_x, result);                          \
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

#define GAL_MUL_PROTO_RETURN size_t
#define GAL_MUL_PROTO_ARGS                      \
        const uint8_t _low[16],                 \
        const uint8_t _high[16],                \
        const uint8_t *restrict const _in,      \
        uint8_t *restrict const _out,           \
        const size_t len

#define GAL_MUL_PROTO(tgt, name)                                \
        GAL_MUL_PROTO_RETURN                                    \
        TARGET(#tgt)                                            \
        __attribute__((nonnull))                                \
        reedsolomon_gal_mul_ ##name (GAL_MUL_PROTO_ARGS)

#define GAL_MUL_IMPL                                                    \
        const V * const low = (const V *)_low,                          \
                * const high = (const V *)_high,                        \
                * in = (const V *)_in;                                  \
        V *out = (V *)_out;                                             \
                                                                        \
        const V low_mask_unpacked = LOW_MASK_UNPACKED,                  \
                low_vector = LOAD_V(low),                               \
                high_vector = LOAD_V(high);                             \
        V in_x = { 0 },                                                 \
          high_input = { 0 },                                           \
          low_input = { 0 },                                            \
          mul_low_part = { 0 },                                         \
          mul_high_part = { 0 },                                        \
          result = { 0 };                                               \
        size_t x = len / 16;                                            \
                                                                        \
        while(x > 0) {                                                  \
                in_x = LOAD_V(in);                                      \
                high_input = SHIFT_RIGHT_V(in_x, 4);                    \
                low_input = AND_V(in_x, low_mask_unpacked);             \
                high_input = AND_V(high_input, low_mask_unpacked);      \
                mul_low_part = SHUFFLE_V(low_vector, low_input);        \
                mul_high_part = SHUFFLE_V(high_vector, high_input);     \
                result = XOR_V(mul_low_part, mul_high_part);            \
                STORE_V(out, result);                                   \
                in++;                                                   \
                out++;                                                  \
                x--;                                                    \
        }                                                               \
                                                                        \
        return ((const uint8_t *)in - _in);                             \

/* Hand-rolled AVX-based implementations */
#define V                       __m128i
#define LOAD_V(a)               _mm_loadu_si128(a)
#define LOW_MASK_UNPACKED       _mm_set1_epi8(0x0f)
#define SHIFT_RIGHT_V(v, n)     _mm_srli_epi64(v, n)
#define AND_V(a, b)             _mm_and_si128(a, b)
#define SHUFFLE_V(v, o)         _mm_shuffle_epi8(v, o)
#define XOR_V(a, b)             _mm_xor_si128(a, b)
#define STORE_V(l, v)           _mm_store_si128(l, v)
__attribute__((hot)) GAL_MUL_PROTO(avx, avx_opt) {
        GAL_MUL_IMPL
}
__attribute__((hot)) GAL_MUL_XOR_PROTO(avx, avx_opt) {
        GAL_MUL_XOR_IMPL
}
#undef V
#undef LOAD_V
#undef LOW_MASK_UNPACKED
#undef SHIFT_RIGHT_V
#undef AND_V
#undef SHUFFLE_V
#undef XOR_V
#undef STORE_V


/* We only support the AVX version for now on Windows.
 * Rationale: MinGHC comes with GCC 4.5.2, which is... rather old.
 * Instead of turning the code below into a conditional-compilation-mess, keep
 * things easy (well, easier) by only providing one version.
 */
#if !defined(_WIN32) && !defined(__clang__)

/* Compiler-generated implementations for various targets */
typedef uint8_t v16uc __attribute__((vector_size(16)));

#define V                       v16uc
#define LOAD_V(a)               (*a)
#define LOW_MASK_UNPACKED       { 0x0f, 0x0f, 0x0f, 0x0f,       \
                                  0x0f, 0x0f, 0x0f, 0x0f,       \
                                  0x0f, 0x0f, 0x0f, 0x0f,       \
                                  0x0f, 0x0f, 0x0f, 0x0f }
#define SHIFT_RIGHT_V(v, n)     (v >> n)
#define AND_V(a, b)             (a & b)
#define SHUFFLE_V(v, o)         __builtin_shuffle(v, o)
#define XOR_V(a, b)             (a ^ b)
#define STORE_V(l, v)           (*l = v)

GAL_MUL_PROTO(avx, avx) { GAL_MUL_IMPL }
GAL_MUL_XOR_PROTO(avx, avx) { GAL_MUL_XOR_IMPL }
GAL_MUL_PROTO(sse4.1, sse_4_1) { GAL_MUL_IMPL }
GAL_MUL_XOR_PROTO(sse4.1, sse_4_1) { GAL_MUL_XOR_IMPL }
GAL_MUL_PROTO(default, generic) { GAL_MUL_IMPL }
GAL_MUL_XOR_PROTO(default, generic) { GAL_MUL_XOR_IMPL }

#undef V
#undef LOAD_V
#undef LOW_MASK_UNPACKED
#undef SHIFT_RIGHT_V
#undef AND_V
#undef SHUFFLE_V
#undef XOR_V
#undef STORE_V


GAL_MUL_PROTO_RETURN reedsolomon_gal_mul(GAL_MUL_PROTO_ARGS)
        __attribute__((ifunc("reedsolomon_gal_mul_ifunc")));
GAL_MUL_XOR_PROTO_RETURN reedsolomon_gal_mul_xor(GAL_MUL_XOR_PROTO_ARGS)
        __attribute__((ifunc("reedsolomon_gal_mul_xor_ifunc")));

#define LOG(s)                          \
        do {                            \
                access(s, F_OK);        \
        } while(0)

#define IFUNC(n, proto)                                                         \
        proto n ## _ifunc(void) {                                               \
                unsigned int eax = 0,                                           \
                             ebx = 0,                                           \
                             ecx = 0,                                           \
                             edx = 0;                                           \
                int rc = 0;                                                     \
                proto func = NULL;                                         \
                                                                                \
                rc = __get_cpuid(1, &eax, &ebx, &ecx, &edx);                    \
                if(rc == 0) {                                                   \
                        LOG("reedsolomon: cpuid failed, using " #n "_generic"); \
                        func = n ## _generic;                                   \
                }                                                               \
                else {                                                          \
                        if((ecx & bit_AVX) != 0) {                              \
                                LOG("reedsolomon: using " #n "_avx_opt");       \
                                func = n ## _avx_opt;                           \
                        }                                                       \
                        else if((ecx & bit_SSE4_1) != 0) {                      \
                                LOG("reedsolomon: using " #n "_sse_4_1");       \
                                func = n ## _sse_4_1;                           \
                        }                                                       \
                        else {                                                  \
                                LOG("reedsolomon: using " #n "_generic");       \
                                func = n ## _generic;                           \
                        }                                                       \
                }                                                               \
                                                                                \
                return func;                                                    \
        }

typedef GAL_MUL_PROTO_RETURN(*gal_mul_proto)(GAL_MUL_PROTO_ARGS);
typedef GAL_MUL_XOR_PROTO_RETURN(*gal_mul_xor_proto)(GAL_MUL_XOR_PROTO_ARGS);

IFUNC(reedsolomon_gal_mul, gal_mul_proto)
IFUNC(reedsolomon_gal_mul_xor, gal_mul_xor_proto)
#else /* if !defined(_WIN32) && !defined(__clang__) */
GAL_MUL_PROTO_RETURN reedsolomon_gal_mul(GAL_MUL_PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}
GAL_MUL_PROTO_RETURN reedsolomon_gal_mul_avx(GAL_MUL_PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}
GAL_MUL_PROTO_RETURN reedsolomon_gal_mul_sse_4_1(GAL_MUL_PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}
GAL_MUL_PROTO_RETURN reedsolomon_gal_mul_generic(GAL_MUL_PROTO_ARGS) {
        return reedsolomon_gal_mul_avx_opt(_low, _high, _in, _out, len);
}

GAL_MUL_XOR_PROTO_RETURN reedsolomon_gal_mul_xor(GAL_MUL_XOR_PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
GAL_MUL_XOR_PROTO_RETURN reedsolomon_gal_mul_xor_avx(GAL_MUL_XOR_PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
GAL_MUL_XOR_PROTO_RETURN reedsolomon_gal_mul_xor_sse_4_1(GAL_MUL_XOR_PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
GAL_MUL_XOR_PROTO_RETURN reedsolomon_gal_mul_xor_generic(GAL_MUL_XOR_PROTO_ARGS) {
        return reedsolomon_gal_mul_xor_avx_opt(_low, _high, _in, _out, len);
}
#endif
