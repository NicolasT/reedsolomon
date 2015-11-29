ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libgalois_amd64.la
noinst_LTLIBRARIES = libgalois_amd64_avx.la \
		     libgalois_amd64_ssse3.la \
		     libgalois_amd64_sse2.la \
                     libgalois_amd64_generic.la

libgalois_amd64_la_SOURCES = galois_amd64_dispatch.c \
		 	     galois_amd64.h
libgalois_amd64_la_LIBADD = $(noinst_LTLIBRARIES)

SIMD_SRC = galois_amd64.c \
	   galois_amd64.h

libgalois_amd64_avx_la_SOURCES = $(SIMD_SRC)
libgalois_amd64_avx_la_CFLAGS = -mavx -DTARGET=avx -DHOT

libgalois_amd64_ssse3_la_SOURCES = $(SIMD_SRC)
libgalois_amd64_ssse3_la_CFLAGS = -mssse3 -DTARGET=ssse3

libgalois_amd64_sse2_la_SOURCES = $(SIMD_SRC)
libgalois_amd64_sse2_la_CFLAGS = -msse2 -DTARGET=sse2

libgalois_amd64_generic_la_SOURCES = $(SIMD_SRC)
libgalois_amd64_generic_la_CFLAGS = -DTARGET=generic

if RS_HAVE_AVX2
noinst_LTLIBRARIES += libgalois_amd64_avx2.la

libgalois_amd64_avx2_la_SOURCES = $(SIMD_SRC)
libgalois_amd64_avx2_la_CFLAGS = -mavx2 -DTARGET=avx2 -DHOT
endif
