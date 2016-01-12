#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([reedsolomon],
        m4_esyscmd_s([grep ^Version ../reedsolomon.cabal | sed -e 's/^Version\:[[:space:]]*//']),
        [https://github.com/NicolasT/reedsolomon/issues],
        [reedsolomon],
        [https://github.com/NicolasT/reedsolomon])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR([reedsolomon.h])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([1.11 foreign -Wall -Werror])
AM_MAINTAINER_MODE([enable])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CC_C99
AC_PROG_CPP
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AC_PROG_RANLIB

AX_APPEND_COMPILE_FLAGS([-ggdb3])
AX_APPEND_COMPILE_FLAGS([-Wall -Wextra])
AX_APPEND_COMPILE_FLAGS([-O3])
AX_APPEND_COMPILE_FLAGS([-floop-parallelize-all -funroll-loops -ftree-vectorize -fprefetch-loop-arrays])
AX_APPEND_COMPILE_FLAGS([-frecord-gcc-switches -grecord-gcc-switches])

AX_CHECK_COMPILE_FLAG([-msse2], [rs_sse2=1], [rs_sse2=0])
AX_CHECK_COMPILE_FLAG([-mssse3], [rs_ssse3=1], [rs_ssse3=0])
AX_CHECK_COMPILE_FLAG([-mavx], [rs_avx=1], [rs_avx=0])
AX_CHECK_COMPILE_FLAG([-mavx2], [rs_avx2=1], [rs_avx2=0])

AM_CONDITIONAL(RS_HAVE_SSE2, [test x$rs_sse2 = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_SSE2],
    [$rs_sse2],
    [Define to 1 if AVX2 works])
AM_CONDITIONAL(RS_HAVE_SSSE3, [test x$rs_ssse3 = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_SSSE3],
    [$rs_ssse3],
    [Define to 1 if SSSE3 works])
AM_CONDITIONAL(RS_HAVE_AVX, [test x$rs_avx = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_AVX],
    [$rs_avx],
    [Define to 1 if AVX works])
AM_CONDITIONAL(RS_HAVE_AVX2, [test x$rs_avx2 = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_AVX2],
    [$rs_avx2],
    [Define to 1 if AVX2 works])

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([stddef.h stdint.h unistd.h])
AC_CHECK_HEADERS([cpuid.h])
rs_simd_headers_save_flags=$[]_AC_LANG_PREFIX[]FLAGS
if test x$rs_sse2 = x1; then
    _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS -msse2"
fi
if test x$rs_ssse3 = x1; then
    _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS -mssse3"
fi
if test x$rs_avx = x1; then
    _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS -mavx"
fi
if test x$rs_avx2 = x1; then
    _AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS -mavx2"
fi
AC_CHECK_HEADERS([emmintrin.h])
AC_CHECK_HEADERS([tmmintrin.h])
AC_CHECK_HEADERS([immintrin.h])
_AC_LANG_PREFIX[]FLAGS=$rs_simd_headers_save_flags

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_UINT8_T
AC_TYPE_UINT64_T

# Checks for library functions.

# Work-around compiler emitting warnings for unrecognized flags added to CFLAGS
# before, which causes the attribute checks to fail.
rs_gcc_func_attribute_save_flags=$[]_AC_LANG_PREFIX[]FLAGS
_AC_LANG_PREFIX[]FLAGS=""
AX_GCC_FUNC_ATTRIBUTE([always_inline])
AX_GCC_FUNC_ATTRIBUTE([const])
AX_GCC_FUNC_ATTRIBUTE([hot])
AX_GCC_FUNC_ATTRIBUTE([ifunc])
AX_GCC_FUNC_ATTRIBUTE([force_align_arg_pointer])
_AC_LANG_PREFIX[]FLAGS=$rs_gcc_func_attribute_save_flags

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
