ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libreedsolomon.la
noinst_LTLIBRARIES = libreedsolomon_avx.la \
		     libreedsolomon_ssse3.la \
		     libreedsolomon_sse2.la \
		     libreedsolomon_generic.la

libreedsolomon_la_SOURCES = reedsolomon_dispatch.c \
			    reedsolomon.h
libreedsolomon_la_LIBADD = $(noinst_LTLIBRARIES)

SIMD_SRC = reedsolomon.c \
	   reedsolomon.h

libreedsolomon_avx_la_SOURCES = $(SIMD_SRC)
libreedsolomon_avx_la_CPPFLAGS = -DTARGET=avx -DHOT
libreedsolomon_avx_la_CFLAGS = -mavx

libreedsolomon_ssse3_la_SOURCES = $(SIMD_SRC)
libreedsolomon_ssse3_la_CPPFLAGS = -DTARGET=ssse3
libreedsolomon_ssse3_la_CFLAGS = -mssse3

libreedsolomon_sse2_la_SOURCES = $(SIMD_SRC)
libreedsolomon_sse2_la_CPPFLAGS = -DTARGET=sse2
libreedsolomon_sse2_la_CFLAGS = -msse2

libreedsolomon_generic_la_SOURCES = $(SIMD_SRC)
libreedsolomon_generic_la_CPPFLAGS = -DTARGET=generic

if RS_HAVE_AVX2
noinst_LTLIBRARIES += libreedsolomon_avx2.la

libreedsolomon_avx2_la_SOURCES = $(SIMD_SRC)
libreedsolomon_avx2_la_CPPFLAGS = -DTARGET=avx2 -DHOT
libreedsolomon_avx2_la_CFLAGS = -mavx2
endif
