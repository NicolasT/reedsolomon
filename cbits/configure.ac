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
AC_PROG_CPP
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AC_PROG_RANLIB

AX_APPEND_COMPILE_FLAGS([-std=c99])
AX_APPEND_COMPILE_FLAGS([-ggdb3])
AX_APPEND_COMPILE_FLAGS([-Wall -Wextra])
AX_APPEND_COMPILE_FLAGS([-O3 -funroll-all-loops])
AX_APPEND_COMPILE_FLAGS([-frecord-gcc-switches -grecord-gcc-switches])

# Check CC has AVX2 support
AC_MSG_CHECKING([whether C compiler supports AVX2])
rs_avx2_save_flags=$[]_AC_LANG_PREFIX[]FLAGS
_AC_LANG_PREFIX[]FLAGS="$[]_AC_LANG_PREFIX[]FLAGS -mavx2"
rs_avx2=0
AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM(
        [[#include <immintrin.h>]],
        [[
            __m256i a = _mm256_set1_epi8(1),
                    b = _mm256_set1_epi8(2),
                    c = _mm256_shuffle_epi8(a, b);
            (void)c;
        ]])
    ],
    [
        AC_MSG_RESULT(yes)
        rs_avx2=1
    ],
    [
        AC_MSG_RESULT(no)
        rs_avx2=0
    ])
_AC_LANG_PREFIX[]FLAGS=$rs_avx2_save_flags

AC_DEFINE_UNQUOTED(
    [RS_HAVE_AVX2],
    [$rs_avx2],
    [Define to 1 if AVX2 works])
AM_CONDITIONAL(RS_HAVE_AVX2, [test x$rs_avx2 = x1])

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([stddef.h stdint.h unistd.h])
AC_CHECK_HEADERS([cpuid.h immintrin.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_UINT8_T
AC_TYPE_UINT64_T

# Checks for library functions.

AX_GCC_FUNC_ATTRIBUTE([always_inline])
AX_GCC_FUNC_ATTRIBUTE([const])
AX_GCC_FUNC_ATTRIBUTE([hot])
AX_GCC_FUNC_ATTRIBUTE([ifunc])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
