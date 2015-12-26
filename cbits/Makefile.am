ACLOCAL_AMFLAGS = -I m4

noinst_LIBRARIES = libreedsolomon.a \
 		   libreedsolomon_avx.a \
		   libreedsolomon_ssse3.a \
		   libreedsolomon_sse2.a \
		   libreedsolomon_generic.a

libreedsolomon_a_SOURCES = reedsolomon_dispatch.c \
		           reedsolomon.h

SIMD_SRC = reedsolomon.c \
	   reedsolomon.h

libreedsolomon_avx_a_SOURCES = $(SIMD_SRC)
libreedsolomon_avx_a_CPPFLAGS = -DTARGET=avx -DHOT
libreedsolomon_avx_a_CFLAGS = -mavx

libreedsolomon_ssse3_a_SOURCES = $(SIMD_SRC)
libreedsolomon_ssse3_a_CPPFLAGS = -DTARGET=ssse3
libreedsolomon_ssse3_a_CFLAGS = -mssse3

libreedsolomon_sse2_a_SOURCES = $(SIMD_SRC)
libreedsolomon_sse2_a_CPPFLAGS = -DTARGET=sse2
libreedsolomon_sse2_a_CFLAGS = -msse2

libreedsolomon_generic_a_SOURCES = $(SIMD_SRC)
libreedsolomon_generic_a_CPPFLAGS = -DTARGET=generic

if RS_HAVE_AVX2
noinst_LIBRARIES += libreedsolomon_avx2.a

libreedsolomon_avx2_a_SOURCES = $(SIMD_SRC)
libreedsolomon_avx2_a_CPPFLAGS = -DTARGET=avx2 -DHOT
libreedsolomon_avx2_a_CFLAGS = -mavx2
endif
