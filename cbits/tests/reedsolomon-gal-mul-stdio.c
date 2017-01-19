/* reedsolomon-gal-mul-stdio.c - Galois-field multiplication routine driver
 *
 * Copyright (c) 2016 Nicolas Trangez
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE
 */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <strings.h>

#if defined(_WIN32) && _WIN32
# define USE_WIN32 1
# include <io.h>
# include <fcntl.h>
#else
# define USE_WIN32 0
#endif

#include "reedsolomon.h"

#ifndef PRIsize_t
# if USE_WIN32
#  define PRIsize_t "I"
# else
#  define PRIsize_t "z"
# endif
#endif
#ifndef SCNsize_t
# if USE_WIN32
#  define SCNsize_t "I"
# else
#  define SCNsize_t "z"
# endif
#endif

static int read_all(const int fd, uint8_t *vec, size_t count) {
        ssize_t rc = 0;

        while(count > 0) {
                rc = read(fd, vec, count);

                if(rc < 0) {
                        perror("read");
                        return -1;
                }
                if(rc == 0) {
                        fprintf(stderr, "Unexpected EOF while reading\n");
                        return -1;
                }

                count -= rc;
                vec += rc;
        }

        return 0;
}

static int write_all(const int fd, const uint8_t *vec, size_t count) {
        ssize_t rc = 0;

        while(count > 0) {
                rc = write(fd, vec, count);

                if(rc < 0) {
                        perror("write");
                        return -1;
                }
                if(rc == 0) {
                        fprintf(stderr, "Unexpected EOF while writing\n");
                        return -1;
                }

                count -= rc;
                vec += rc;
        }

        return 0;
}

typedef PROTO_RETURN(*reedsolomon_gal_mul_fun)(PROTO_ARGS);

static reedsolomon_gal_mul_fun get_impl(reedsolomon_cpu_support type) {
        reedsolomon_gal_mul_fun res = NULL;

#define CASE(selector, impl) \
        case selector: \
                res = impl; \
                break;

        switch(type) {
#if RS_HAVE_GENERIC
                CASE(REEDSOLOMON_CPU_GENERIC, reedsolomon_gal_mul_generic)
#endif
#if RS_HAVE_SSE2
                CASE(REEDSOLOMON_CPU_SSE2, reedsolomon_gal_mul_sse2)
#endif
#if RS_HAVE_SSSE3
                CASE(REEDSOLOMON_CPU_SSSE3, reedsolomon_gal_mul_ssse3)
#endif
#if RS_HAVE_AVX
                CASE(REEDSOLOMON_CPU_AVX, reedsolomon_gal_mul_avx)
#endif
#if RS_HAVE_AVX2
                CASE(REEDSOLOMON_CPU_AVX2, reedsolomon_gal_mul_avx2)
#endif
#if RS_HAVE_NEON
                CASE(REEDSOLOMON_CPU_NEON, reedsolomon_gal_mul_neon)
#endif
#if RS_HAVE_ALTIVEC
                CASE(REEDSOLOMON_CPU_ALTIVEC, reedsolomon_gal_mul_altivec)
#endif
                default:
                        res = NULL;
                        break;
        }
#undef CASE

        return res;
}

static const char * impl_name(reedsolomon_cpu_support type) {
        const char * res = NULL;

        switch(type) {
                case REEDSOLOMON_CPU_GENERIC:
                        res = "Generic";
                        break;
                case REEDSOLOMON_CPU_SSE2:
                        res = "SSE2";
                        break;
                case REEDSOLOMON_CPU_SSSE3:
                        res = "SSSE3";
                        break;
                case REEDSOLOMON_CPU_AVX:
                        res = "AVX";
                        break;
                case REEDSOLOMON_CPU_AVX2:
                        res = "AVX2";
                        break;
                case REEDSOLOMON_CPU_NEON:
                        res = "NEON";
                        break;;
                case REEDSOLOMON_CPU_ALTIVEC:
                        res = "AltiVec";
                        break;
        }

        return res;
}

static int parse_backend(const char *name, reedsolomon_cpu_support *id) {
        int rc = -1;

#define CASE(_name) \
        if(strcasecmp(name, #_name) == 0) { \
                *id = REEDSOLOMON_CPU_ ## _name; \
                rc = 0; \
        }

        CASE(GENERIC)
        else CASE(SSE2)
        else CASE(SSSE3)
        else CASE(AVX)
        else CASE(AVX2)
        else CASE(NEON)
        else CASE(ALTIVEC)

        return rc;
}

int main(int argc, char **argv) {
        int rc = 1,
            opt = -1;
        size_t size = 0,
               cnt = 0;
        uint8_t *data = NULL,
                *out = NULL,
                low_vector[16] = { 0 },
                high_vector[16] = { 0 };
        reedsolomon_gal_mul_fun impl = NULL;
        reedsolomon_cpu_support backend_id = REEDSOLOMON_CPU_GENERIC,
                                supported = REEDSOLOMON_CPU_GENERIC;
        const char *backend = "native";

        while((opt = getopt(argc, argv, "b:")) != -1) {
                switch(opt) {
                        case 'b':
                                backend = optarg;
                                break;
                        case '?': {
                                rc = 99;
                                goto out;
                        } break;
                        default:
                                abort();
                                break;
                }
        }

        if(argc - optind != 1) {
                fprintf(stderr, "Usage: %s [-b backend] SIZE\n", argv[0]);
                rc = 99;
                goto out;
        }

        supported = reedsolomon_determine_cpu_support();

        if(strcasecmp(backend, "native") == 0) {
                fprintf(stderr, "Using native backend, which is %s\n", impl_name(supported));
                impl = reedsolomon_gal_mul;
        } else {
                rc = parse_backend(backend, &backend_id);
                if(rc != 0) {
                        fprintf(stderr, "Unknown backend: %s\n", backend);
                        rc = 99;
                        goto out;
                }

                if(backend_id > supported) {
                        fprintf(stderr, "Requested backend not supported on system CPU: %s > %s\n",
                                impl_name(backend_id), impl_name(supported));
                        rc = 77;
                        goto out;
                }
                impl = get_impl(backend_id);
        }
        if(impl == NULL) {
                fprintf(stderr, "Failed to find requested implementation\n");
                rc = 77;
                goto out;
        }

        rc = sscanf(argv[optind], "%" SCNsize_t "u", &size);
        if(rc == EOF) {
                perror("sscanf");
                rc = 99;
                goto out;
        }

        if(size == 0) {
                fprintf(stderr, "Invalid size: %" PRIsize_t "u\n", size);
                rc = 99;
                goto out;
        }

#if USE_WIN32
        rc = _setmode(STDIN_FILENO, _O_BINARY);
        if(rc == -1) {
                perror("_setmode");
                rc = 99;
                goto out;
        }

        rc = _setmode(STDOUT_FILENO, _O_BINARY);
        if(rc == -1) {
                perror("_setmode");
                rc = 99;
                goto out;
        }
#endif

        rc = read_all(STDIN_FILENO, low_vector, sizeof(low_vector));
        if(rc != 0) {
                rc = 99;
                goto out;
        }

        rc = read_all(STDIN_FILENO, high_vector, sizeof(high_vector));
        if(rc != 0) {
                rc = 99;
                goto out;
        }

        data = malloc(size);
        if(data == NULL) {
                perror("malloc");
                rc = 99;
                goto out;
        }

        rc = read_all(STDIN_FILENO, data, size);
        if(rc != 0) {
                rc = 99;
                goto out;
        }

        out = malloc(size);
        if(out == NULL) {
                perror("malloc");
                rc = 99;
                goto out;
        }

        cnt = impl(low_vector, high_vector, data, out, size);
        if(cnt != size) {
                fprintf(stderr, "Count mismatch: size=%" PRIsize_t "u, cnt=%" PRIsize_t "u\n", size, cnt);
                rc = 99;
                goto out;
        }

        rc = write_all(STDOUT_FILENO, out, size);
        if(rc != 0) {
                rc = 99;
                goto out;
        }

        rc = 0;

out:
        free(data);
        free(out);

        return rc;
}
