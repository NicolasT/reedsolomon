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

AC_CANONICAL_BUILD
AC_CANONICAL_HOST

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CC_C99
AC_PROG_CPP
AC_PROG_AWK
AC_PROG_SED
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
AC_PROG_RANLIB

# Checks for libraries.

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_TYPE_UINT8_T
AC_TYPE_UINT64_T

## Checks for supported function attributes.
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

## Checks for supported generic compiler options.
AX_APPEND_COMPILE_FLAGS([-ggdb3])
AX_APPEND_COMPILE_FLAGS([-Wall -Wextra])
AX_APPEND_COMPILE_FLAGS([-O3])
dnl Handle "sorry, unimplemented: Graphite loop optimizations can only be used if the libcloog-ppl0 package is installed"
dnl The error above is only triggered when the Graphite optimization is actually
dnl used, i.e. not for the 'default' program source compiled to determine
dnl compiler support for some flag. The snippet below should trigger it though.
dnl See also: https://lists.debian.org/debian-gcc/2015/06/msg00164.html
AX_APPEND_COMPILE_FLAGS([-floop-parallelize-all], [], [], [
    AC_LANG_PROGRAM([[
            extern int argc;
            extern char **argv;
        ]], [[
            int x = 0, result = 0;
            for(x = 0; x < argc; x++) {
                result |= (int)argv[x][0];
            }
            return result;
        ]])])
AX_APPEND_COMPILE_FLAGS([-funroll-loops -ftree-vectorize -fprefetch-loop-arrays])
AX_APPEND_COMPILE_FLAGS([-frecord-gcc-switches -grecord-gcc-switches])

## Checks for supported ISA compiler options.
AX_CHECK_COMPILE_FLAG([-msse2], [rs_sse2=1], [rs_sse2=0])
AX_CHECK_COMPILE_FLAG([-mssse3], [rs_ssse3=1], [rs_ssse3=0])
AX_CHECK_COMPILE_FLAG([-mavx], [rs_avx=1], [rs_avx=0])
AX_CHECK_COMPILE_FLAG([-mavx2], [rs_avx2=1], [rs_avx2=0])

# Checks for header files.
AC_CHECK_HEADERS([stddef.h stdint.h unistd.h])
AC_CHECK_HEADERS([cpuid.h])

## Checks for ISA-specific header files.
rs_simd_headers_save_flags=$CPPFLAGS
if test x$rs_sse2 = x1; then
    CPPFLAGS="$CPPFLAGS -msse2"
fi
if test x$rs_ssse3 = x1; then
    CPPFLAGS="$CPPFLAGS -mssse3"
fi
if test x$rs_avx = x1; then
    CPPFLAGS="$CPPFLAGS -mavx"
fi
if test x$rs_avx2 = x1; then
    CPPFLAGS="$CPPFLAGS -mavx2"
fi
AC_CHECK_HEADERS([emmintrin.h])
AC_CHECK_HEADERS([tmmintrin.h])
AC_CHECK_HEADERS([immintrin.h])
CPPFLAGS=$rs_simd_headers_save_flags

AC_CHECK_HEADERS([arm_neon.h], [rs_neon=1], [rs_neon=0])

# Checks for library functions.
AC_FUNC_MALLOC

# Definitions based on discovered compiler or ISA characteristics.
if test x$rs_neon = x1; then
    # When building with ARM NEON support, we don't build generic
    rs_generic=0
else
    rs_generic=1
fi
AM_CONDITIONAL(RS_HAVE_GENERIC, [test x$rs_generic = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_GENERIC],
    [$rs_generic],
    [Define to 1 if the generic routine works])
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
AM_CONDITIONAL(RS_HAVE_NEON, [test x$rs_neon = x1])
AC_DEFINE_UNQUOTED(
    [RS_HAVE_NEON],
    [$rs_neon],
    [Define to 1 if NEON works])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

rs_backends=""
if test x$rs_generic = x1; then
    rs_backends="$rs_backends, generic"
fi
if test x$rs_sse2 = x1; then
    rs_backends="$rs_backends, sse2"
fi
if test x$rs_ssse3 = x1; then
    rs_backends="$rs_backends, ssse3"
fi
if test x$rs_avx = x1; then
    rs_backends="$rs_backends, avx"
fi
if test x$rs_avx2 = x1; then
    rs_backends="$rs_backends, avx2"
fi
if test x$rs_neon = x1; then
    rs_backends="$rs_backends, neon"
fi

rs_backends=`echo $rs_backends | $SED s/^,\ //`

echo ["
----------------------------------------------------------------------
Configure completed successfully.

    Version: $PACKAGE_VERSION
    Compiler: $CC

    Build platform: $build
    Host platform: $host
    Enabled backends: $rs_backends
"]

case $host_cpu in
    arm*)
        if test x$rs_neon = x0; then
            echo "    WARNING: Not using ARM NEON, performance will be degraded."
        fi;;
    i[3456]86*|x86_64*)
        # Assume a compiler which supports AVX/AVX2 also supports SSSE3
        if test x$rs_ssse3 = x0; then
            echo "    WARNING: Not using Intel SSSE3, performance will be degraded."
        fi;;
esac
