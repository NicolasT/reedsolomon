ACLOCAL_AMFLAGS = -I m4

@CODE_COVERAGE_RULES@

libreedsolomon_a_SOURCES = src/reedsolomon-dispatch.c \
		           src/reedsolomon.h
libreedsolomon_a_CPPFLAGS = $(CODE_COVERAGE_CPPFLAGS)
libreedsolomon_a_CFLAGS = $(CODE_COVERAGE_CFLAGS)
noinst_LIBRARIES = libreedsolomon.a

SIMD_SRC = src/reedsolomon.c \
	   src/reedsolomon.h

if RS_HAVE_GENERIC
libreedsolomon_generic_a_SOURCES = $(SIMD_SRC)
libreedsolomon_generic_a_CPPFLAGS = -DTARGET=generic \
				    $(CODE_COVERAGE_CPPFLAGS)
libreedsolomon_generic_a_CFLAGS = $(CODE_COVERAGE_CFLAGS)
noinst_LIBRARIES += libreedsolomon-generic.a
endif

if RS_HAVE_SSE2
libreedsolomon_sse2_a_SOURCES = $(SIMD_SRC)
libreedsolomon_sse2_a_CPPFLAGS = -DTARGET=sse2 \
				 $(CODE_COVERAGE_CPPFLAGS)
libreedsolomon_sse2_a_CFLAGS = -msse2 $(AM_CFLAGS) \
			       $(CODE_COVERAGE_CFLAGS)
noinst_LIBRARIES += libreedsolomon-sse2.a
endif

if RS_HAVE_SSSE3
libreedsolomon_ssse3_a_SOURCES = $(SIMD_SRC)
libreedsolomon_ssse3_a_CPPFLAGS = -DTARGET=ssse3 \
				  $(CODE_COVERAGE_CPPFLAGS)
libreedsolomon_ssse3_a_CFLAGS = -mssse3 $(AM_CFLAGS) \
				$(CODE_COVERAGE_CFLAGS)
noinst_LIBRARIES += libreedsolomon-ssse3.a
endif

if RS_HAVE_AVX
libreedsolomon_avx_a_SOURCES = $(SIMD_SRC)
libreedsolomon_avx_a_CPPFLAGS = -DTARGET=avx -DHOT \
				$(CODE_COVERAGE_CPPFLAGS)
libreedsolomon_avx_a_CFLAGS = -mavx $(AM_CFLAGS) \
			      $(CODE_COVERAGE_CFLAGS)
noinst_LIBRARIES += libreedsolomon-avx.a
endif

if RS_HAVE_AVX2
libreedsolomon_avx2_a_SOURCES = $(SIMD_SRC)
libreedsolomon_avx2_a_CPPFLAGS = -DTARGET=avx2 -DHOT \
				 $(CODE_COVERAGE_CPPFLAGS)
libreedsolomon_avx2_a_CFLAGS = -mavx2 $(AM_CFLAGS) \
			       $(CODE_COVERAGE_CFLAGS)
noinst_LIBRARIES += libreedsolomon-avx2.a
endif

if RS_HAVE_NEON
libreedsolomon_neon_a_SOURCES = $(SIMD_SRC)
libreedsolomon_neon_a_CPPFLAGS = -DTARGET=neon -DHOT \
				 $(CODE_COVERAGE_CPPFLAGS)
libreedsolomon_neon_a_CFLAGS = $(CODE_COVERAGE_CFLAGS)
noinst_LIBRARIES += libreedsolomon-neon.a
endif

if RS_HAVE_ALTIVEC
libreedsolomon_altivec_a_SOURCES = $(SIMD_SRC)
libreedsolomon_altivec_a_CPPFLAGS = -DTARGET=altivec -DHOT \
				    $(CODE_COVERAGE_CPPFLAGS)
libreedsolomon_altivec_a_CFLAGS = $(CODE_COVERAGE_CFLAGS)
noinst_LIBRARIES += libreedsolomon-altivec.a
endif

check_PROGRAMS = reedsolomon-gal-mul-stdio \
		 test-reedsolomon-gal-mul-xor
reedsolomon_gal_mul_stdio_SOURCES = tests/reedsolomon-gal-mul-stdio.c
reedsolomon_gal_mul_stdio_CPPFLAGS = -I$(top_srcdir)/src \
				     $(CODE_COVERAGE_CPPFLAGS)
reedsolomon_gal_mul_stdio_CFLAGS = $(CODE_COVERAGE_CFLAGS)
reedsolomon_gal_mul_stdio_LDADD = libreedsolomon.a \
				  $(LIBOBJS)
reedsolomon_gal_mul_stdio_LIBS = $(CODE_COVERAGE_LIBS)

if RS_HAVE_GENERIC
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-generic.a
endif
if RS_HAVE_SSE2
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-sse2.a
endif
if RS_HAVE_SSSE3
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-ssse3.a
endif
if RS_HAVE_AVX
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-avx.a
endif
if RS_HAVE_AVX2
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-avx2.a
endif
if RS_HAVE_NEON
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-neon.a
endif
if RS_HAVE_ALTIVEC
reedsolomon_gal_mul_stdio_LDADD += libreedsolomon-altivec.a
endif

test_reedsolomon_gal_mul_xor_SOURCES = tests/test-reedsolomon-gal-mul-xor.c
test_reedsolomon_gal_mul_xor_CPPFLAGS = -I$(top_srcdir)/src \
					$(CODE_COVERAGE_CPPFLAGS)
test_reedsolomon_gal_mul_xor_CFLAGS = $(CODE_COVERAGE_CFLAGS)
test_reedsolomon_gal_mul_xor_LDADD = $(reedsolomon_gal_mul_stdio_LDADD)
test_reedsolomon_gal_mul_xor_LIBS = $(CODE_COVERAGE_LIBS)

ALL_TEST_CASES = \
	tests/zeros.test \
	tests/quickcheck-0.test \
	tests/quickcheck-1.test \
	tests/quickcheck-2.test \
	tests/quickcheck-3.test \
	tests/quickcheck-4.test \
	tests/quickcheck-5.test \
	tests/quickcheck-6.test \
	tests/quickcheck-7.test \
	tests/quickcheck-8.test \
	tests/quickcheck-9.test \
	tests/quickcheck-10.test \
	tests/quickcheck-11.test \
	tests/quickcheck-12.test \
	tests/quickcheck-13.test \
	tests/quickcheck-14.test \
	tests/quickcheck-15.test \
	tests/quickcheck-16.test \
	tests/quickcheck-17.test \
	tests/quickcheck-18.test \
	tests/quickcheck-19.test \
	tests/quickcheck-20.test \
	tests/quickcheck-21.test \
	tests/quickcheck-22.test \
	tests/quickcheck-23.test \
	tests/quickcheck-24.test \
	tests/quickcheck-25.test \
	tests/quickcheck-26.test \
	tests/quickcheck-27.test \
	tests/quickcheck-28.test \
	tests/quickcheck-29.test \
	tests/quickcheck-30.test \
	tests/quickcheck-31.test \
	tests/quickcheck-32.test \
	tests/quickcheck-33.test \
	tests/quickcheck-34.test \
	tests/quickcheck-35.test \
	tests/quickcheck-36.test \
	tests/quickcheck-37.test \
	tests/quickcheck-38.test \
	tests/quickcheck-39.test \
	tests/quickcheck-40.test \
	tests/quickcheck-41.test \
	tests/quickcheck-42.test \
	tests/quickcheck-43.test \
	tests/quickcheck-44.test \
	tests/quickcheck-45.test \
	tests/quickcheck-46.test \
	tests/quickcheck-47.test \
	tests/quickcheck-48.test \
	tests/quickcheck-49.test \
	tests/quickcheck-50.test \
	tests/quickcheck-51.test \
	tests/quickcheck-52.test \
	tests/quickcheck-53.test \
	tests/quickcheck-54.test \
	tests/quickcheck-55.test \
	tests/quickcheck-56.test \
	tests/quickcheck-57.test \
	tests/quickcheck-58.test \
	tests/quickcheck-59.test \
	tests/quickcheck-60.test \
	tests/quickcheck-61.test \
	tests/quickcheck-62.test \
	tests/quickcheck-63.test \
	tests/quickcheck-64.test \
	tests/quickcheck-65.test \
	tests/quickcheck-66.test \
	tests/quickcheck-67.test \
	tests/quickcheck-68.test \
	tests/quickcheck-69.test \
	tests/quickcheck-70.test \
	tests/quickcheck-71.test \
	tests/quickcheck-72.test \
	tests/quickcheck-73.test \
	tests/quickcheck-74.test \
	tests/quickcheck-75.test \
	tests/quickcheck-76.test \
	tests/quickcheck-77.test \
	tests/quickcheck-78.test \
	tests/quickcheck-79.test \
	tests/quickcheck-80.test \
	tests/quickcheck-81.test \
	tests/quickcheck-82.test \
	tests/quickcheck-83.test \
	tests/quickcheck-84.test \
	tests/quickcheck-85.test \
	tests/quickcheck-86.test \
	tests/quickcheck-87.test \
	tests/quickcheck-88.test \
	tests/quickcheck-89.test \
	tests/quickcheck-90.test \
	tests/quickcheck-91.test \
	tests/quickcheck-92.test \
	tests/quickcheck-93.test \
	tests/quickcheck-94.test \
	tests/quickcheck-95.test \
	tests/quickcheck-96.test \
	tests/quickcheck-97.test \
	tests/quickcheck-98.test \
	tests/quickcheck-99.test

TEST_LOG_DRIVER = env AM_TAP_AWK='$(AWK)' \
		  INTERPRETER='$(INTERPRETER)' \
		  $(SHELL) $(top_srcdir)/build-aux/tap-driver.sh
AM_TEST_LOG_FLAGS = $(SHELL) $(top_srcdir)/tests/run-test.sh

LOG_COMPILER = $(INTERPRETER)

TESTS = test-reedsolomon-gal-mul-xor \
	$(ALL_TEST_CASES)

EXTRA_DIST = tests/run-test.sh \
	     $(ALL_TEST_CASES)
