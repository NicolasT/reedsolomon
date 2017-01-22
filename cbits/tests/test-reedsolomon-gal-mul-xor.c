#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif
#include "reedsolomon.h"

#define LEN (64)
#define SEED (42)

#if defined(HAVE_VAR_ATTRIBUTE_ALIGNED) && HAVE_VAR_ATTRIBUTE_ALIGNED
# define ALIGN __attribute__((aligned(RS_ASSUMED_ALIGNMENT)))
#else
# warning No variable alignment attribute support, subtle test failures may arise
# define ALIGN
#endif

/* Simple Lehmer PRNG */
static inline uint32_t next(uint32_t rnd) {
    return ((uint64_t)rnd * 279470273UL) % 4294967291UL;
}

static uint32_t fill_random(void *ptr, size_t len, uint32_t seed) {
        uint32_t rnd = seed;
        size_t i = 0;

        for(i = 0; i < len / 4; i++) {
                rnd = next(rnd);
                memcpy((char *)ptr + (i * 4), &rnd, 4);
        }

        rnd = next(rnd);
        memcpy((char *)ptr + (i * 4), &rnd, len - (i * 4));

        return rnd;
}

int main() {
        uint8_t low[16] ALIGN = { 0 },
                high[16] ALIGN = { 0 },
                data[LEN] ALIGN = { 0 },
                out[LEN] ALIGN = { 0 },
                initial_out[LEN] ALIGN = { 0 },
                out_xor[LEN] ALIGN;
        size_t size = 0,
               idx = 0;
        int rc = 1;
        uint32_t rnd = SEED;

        rnd = fill_random(low, sizeof(low), rnd);
        rnd = fill_random(high, sizeof(high), rnd);
        rnd = fill_random(data, sizeof(data), rnd);
        fill_random(initial_out, sizeof(initial_out), rnd);

        memcpy(out_xor, initial_out, LEN);

        size = reedsolomon_gal_mul_xor(low, high, data, out_xor, sizeof(data));
        if(size != LEN) {
                abort();
        }

        size = reedsolomon_gal_mul(low, high, data, out, sizeof(data));
        if(size != LEN) {
                abort();
        }

        for(idx = 0; idx < sizeof(out_xor); idx++) {
                if(out_xor[idx] != (out[idx] ^ initial_out[idx])) {
                        rc = 1;
                        goto out;
                }
        }

        rc = 0;
out:
        return rc;
}
