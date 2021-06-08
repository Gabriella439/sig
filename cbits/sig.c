/*  This file implements most of the algorithm described by the following paper:

    > Mytkowicz, Todd, Madanlal Musuvathi, and Wolfram Schulte. "Data-parallel
    > finite-state machines." ACM SIGARCH Computer Architecture News. Vol. 42.
    > No. 1. ACM, 2014.

    The comments in this file will reference concepts from that paper.
*/

#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

typedef char v16qi __attribute__ ((vector_size (16)));
typedef char v32qi __attribute__ ((vector_size (32)));
typedef char v64qi __attribute__ ((vector_size (64)));

typedef union {
    v32qi x32;
    v16qi x16[2];
} v32;

typedef union {
    v64qi x64;
    v32qi x32[2];
} v64;

/* All of the following shuffle{m}x{n} functions implement the ⊗ₘₙ operator from
   the paper for specific values of m and n.
*/
static inline v16qi shuffle16x16(v16qi m, v16qi n) {
    return __builtin_ia32_pshufb128(n, m);
}

static inline v32qi shuffle32x16(v32qi m, v16qi n) {
    v32 nn = { .x16 = { n, n } };
    return __builtin_ia32_pshufb256(nn.x32, m);
}

static inline v64qi shuffle64x16(v64qi m, v16qi n) {
    v64 m_ = { .x64 = m };
    v32qi lo = shuffle32x16(m_.x32[0], n);
    v32qi hi = shuffle32x16(m_.x32[1], n);
    v64 result = { .x32 = { lo, hi } };
    return result.x64;
}

static inline v16qi shuffle16x32(v16qi m, v32qi n) {
    v32 n_ = { .x32 = n };
    v16qi lo0 = shuffle16x16(m     , n_.x16[0]);
    v16qi lo1 = shuffle16x16(m % 16, n_.x16[1]);
    return __builtin_ia32_pblendvb128(lo1, lo0, m < 16);
}

static inline v32qi shuffle32x32(v32qi m, v32qi n) {
    v32 n_ = { .x32 = n };
    v32qi lo0 = shuffle32x16(m     , n_.x16[0]);
    v32qi lo1 = shuffle32x16(m % 16, n_.x16[1]);
    return __builtin_ia32_pblendvb256(lo1, lo0, m < 16);
}

static inline v64qi shuffle64x32(v64qi m, v32qi n) {
    v64 m_ = { .x64 = m };
    v32qi lo = shuffle32x32(m_.x32[0], n);
    v32qi hi = shuffle32x32(m_.x32[1], n);
    v64 result = { .x32 = { lo, hi } };
    return result.x64;
}

static inline v16qi shuffle16x64(v16qi m, v64qi n) {
    v64 n_ = { .x64 = n };
    v16qi lo0 = shuffle16x32(m     , n_.x32[0]);
    v16qi lo1 = shuffle16x32(m % 32, n_.x32[1]);
    return __builtin_ia32_pblendvb128(lo1, lo0, m < 32);
}

static inline v32qi shuffle32x64(v32qi m, v64qi n) {
    v64 n_ = { .x64 = n };
    v32qi lo0 = shuffle32x32(m     , n_.x32[0]);
    v32qi lo1 = shuffle32x32(m % 32, n_.x32[1]);
    return __builtin_ia32_pblendvb256(lo1, lo0, m < 32);
}

static inline v64qi shuffle64x64(v64qi m, v64qi n) {
    v64 m_ = { .x64 = m };
    v32qi lo = shuffle32x64(m_.x32[0], n);
    v32qi hi = shuffle32x64(m_.x32[1], n);
    v64 result = { .x32 = { lo, hi } };
    return result.x64;
}

typedef struct {
  v64qi left;
  v16qi unique;
  size_t num_unique;
} factorization16x64;

factorization16x64 factor16x64(v64qi s) {
    size_t i;
    char value;
    size_t lookup[64] = { 0 };

    uint64_t bitset = 0;
    factorization16x64 result =
        { .left = { 0 }, .unique = { 0 }, .num_unique = 0 };

    for (i = 0; i < 64; i++) {
        value = s[i];

        if ((bitset >> value) & 0x1ull) {
            result.left[i] = lookup[value];
        } else {
            bitset |= 0x1ull << value;
            result.unique[result.num_unique] = value;
            lookup[value] = result.num_unique;
            result.left[i] = result.num_unique;
            result.num_unique++;
        }
    }

    return result;
}

typedef struct {
  v32qi left;
  v16qi unique;
  size_t num_unique;
} factorization16x32;

factorization16x32 factor16x32(v32qi s) {
    size_t i;
    char value;
    size_t lookup[64] = { 0 };

    uint64_t bitset = 0;
    factorization16x32 result =
        { .left = { 0 }, .unique = { 0 }, .num_unique = 0 };

    for (i = 0; i < 32; i++) {
        value = s[i];

        if ((bitset >> value) & 0x1ull) {
            result.left[i] = lookup[value];
        } else {
            bitset |= 0x1ull << value;
            result.unique[result.num_unique] = value;
            lookup[value] = result.num_unique;
            result.left[i] = result.num_unique;
            result.num_unique++;
        }
    }

    return result;
}

typedef struct {
  v64qi left;
  v32qi unique;
  size_t num_unique;
} factorization32x64;

factorization32x64 factor32x64(v64qi s) {
    size_t i;
    char value;
    size_t lookup[64] = { 0 };

    uint64_t bitset = 0;
    factorization32x64 result =
        { .left = { 0 }, .unique = { 0 }, .num_unique = 0 };

    for (i = 0; i < 64; i++) {
        value = s[i];

        if ((bitset >> value) & 0x1ull) {
            result.left[i] = lookup[value];
        } else {
            bitset |= 0x1ull << value;
            result.unique[result.num_unique] = value;
            lookup[value] = result.num_unique;
            result.left[i] = result.num_unique;
            result.num_unique++;
        }
    }

    return result;
}

int num_unique64(v64qi x) {
    unsigned long long bitset = 0;
    size_t i;

    for (i = 0; i < 64; i++) {
      bitset |= 0x1ull << x[i];
    }

    return __builtin_popcountll(bitset);
}

int num_unique32(v32qi x) {
    uint64_t bitset = 0;
    size_t i;

    for (i = 0; i < 32; i++) {
      bitset |= 0x1ull << x[i];
    }

    return __builtin_popcountll(bitset);
}

void run(char *in, size_t len, unsigned char *tBytes, char *out) {
    unsigned char a, b, c, d, e, f, g;
    int i, j, uniques;

    factorization16x64 factors16x64, t_factors16x64[256];
    factorization32x64 factors32x64, t_factors32x64[256];
    factorization16x32 factors16x32;

    v64qi t[256];
    v64qi s64 =
        {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15
        , 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31
        , 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47
        , 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63
        };
    v64qi l64 = s64;

    v32qi t32[256][256];
    v32qi *t32_current;
    v32qi s32;

    v16qi t16[256][256];
    v16qi *t16_current;
    v16qi s16, s16_0, s16_1, s16_2, s16_3, s16_4, s16_5;

    if (len == 0) {
        goto done;
    }

    int maxUniques = 0;
    for (i = 0; i < 256; i++) {
        for (j = 0; j < 64; j++) {
            t[i][j] = tBytes[64 * i + j];
        }

        uniques = num_unique64(t[i]);
        if (maxUniques < uniques) {
          maxUniques = uniques;
        }
    }

    // TODO: Share work to compute range coalescing tables between threads
    if (maxUniques <= 16) {
        for (i = 0; i < 256; i++) {
            t_factors16x64[i] = factor16x64(t[i]);
        }

        for (i = 0; i < 256; i++) {
            for (j = 0; j < 256; j++) {
                t16[i][j] = shuffle16x64(t_factors16x64[i].unique, t_factors16x64[j].left);
            }
        }

        i = 0;
        a = in[i];
        s64 = t_factors16x64[a].left;
        t16_current = t16[a];
        i++;

        goto loop16x64;
    } else if (maxUniques <= 32) {
        for (i = 0; i < 256; i++) {
            t_factors32x64[i] = factor32x64(t[i]);
        }

        for (i = 0; i < 256; i++) {
            for (j = 0; j < 256; j++) {
                t32[i][j] = shuffle32x64(t_factors32x64[i].unique, t_factors32x64[j].left);
            }
        }

        i = 0;
        a = in[i];
        s64 = t_factors32x64[a].left;
        t32_current = t32[a];
        i++;

        goto loop32x64;
    } else {
        i = 0;
        goto loop64x64;
    }

loop64x64:
    for (; i < len; i++) {
        if (i % 256 == 1) {
            uniques = num_unique64(s64);
            if (uniques <= 16) {
              factors16x64 = factor16x64(s64);
              s16 = factors16x64.unique;
              l64 = shuffle64x64(l64, factors16x64.left);
              goto loop64x16;
            } else if (uniques <= 32) {
              factors32x64 = factor32x64(s64);
              s32 = factors32x64.unique;
              l64 = shuffle64x64(l64, factors32x64.left);
              goto loop64x32;
            }
        }

        a = in[i];
        s64 = shuffle64x64(s64, t[a]);
    }

    s64 = shuffle64x64(l64, s64);

    goto done;

loop64x32:
    for (; i < len; i++) {
        if (i % 256 == 2) {
            uniques = num_unique32(s32);
            if (uniques <= 16) {
              factors16x32 = factor16x32(s32);
              s16 = factors16x32.unique;
              l64 = shuffle64x32(l64, factors16x32.left);
              goto loop64x16;
            }
        }

        a = in[i];
        s32 = shuffle32x64(s32, t[a]);
    }

    s64 = shuffle64x32(l64, s32);

    goto done;

loop64x16:
    for (; i < len; i++) {
        a = in[i];
        s16 = shuffle16x64(s16, t[a]);
    }

    s64 = shuffle64x16(l64, s16);

    goto done;

loop32x64:
   for (; i < len; i++) {
        if (i % 256 == 1) {
            uniques = num_unique64(s64);
            if (uniques <= 16) {
              factors16x64 = factor16x64(s64);
              s16 = factors16x64.unique;
              l64 = shuffle64x64(l64, factors16x64.left);
              goto loop32x16;
            } else if (uniques <= 32) {
              factors32x64 = factor32x64(s64);
              s32 = factors32x64.unique;
              l64 = shuffle64x64(l64, factors32x64.left);
              goto loop32x32;
            }
        }

        a = in[i];
        s64 = shuffle64x32(s64, t32_current[a]);
        t32_current = t32[a];
    }

    a = in[i - 1];
    s64 = shuffle64x32(s64, t_factors32x64[a].unique);
    s64 = shuffle64x64(l64, s64);

    goto done;

loop32x32:
    for (; i < len; i++) {
        if (i % 256 == 2) {
            uniques = num_unique32(s32);
            if (uniques <= 16) {
              factors16x32 = factor16x32(s32);
              s16 = factors16x32.unique;
              l64 = shuffle64x32(l64, factors16x32.left);
              goto loop32x16;
            }
        }

        a = in[i];
        s32 = shuffle32x32(s32, t32_current[a]);
        t32_current = t32[a];
    }

    a = in[i - 1];
    s32 = shuffle32x32(s32, t_factors32x64[a].unique);
    s64 = shuffle64x32(l64, s32);

    goto done;

loop32x16:
    for (; i < len; i++) {
        a = in[i];
        s16 = shuffle16x32(s16, t32_current[a]);
        t32_current = t32[a];
    }

    a = in[i - 1];
    s16 = shuffle16x32(s16, t_factors32x64[a].unique);
    s64 = shuffle64x16(l64, s16);

    goto done;

loop16x64:
   for (; i < len; i++) {
        if (i % 256 == 1) {
            uniques = num_unique64(s64);
            if (uniques <= 16) {
              factors16x64 = factor16x64(s64);
              s16 = factors16x64.unique;
              l64 = shuffle64x64(l64, factors16x64.left);
              goto loop16x16;
            } else if (uniques <= 32) {
              factors32x64 = factor32x64(s64);
              s32 = factors32x64.unique;
              l64 = shuffle64x64(l64, factors32x64.left);
              goto loop16x32;
            }
        }

        a = in[i];
        s64 = shuffle64x16(s64, t16_current[a]);
        t16_current = t16[a];
    }

    a = in[i - 1];
    s64 = shuffle64x16(s64, t_factors16x64[a].unique);
    s64 = shuffle64x64(l64, s64);

    goto done;

loop16x32:
    for (; i < len; i++) {
        if (i % 256 == 2) {
            uniques = num_unique32(s32);
            if (uniques <= 16) {
              factors16x32 = factor16x32(s32);
              s16 = factors16x32.unique;
              l64 = shuffle64x32(l64, factors16x32.left);
              goto loop16x16;
            }
        }

        a = in[i];
        s32 = shuffle32x16(s32, t16_current[a]);
        t16_current = t16[a];
    }

    a = in[i - 1];
    s32 = shuffle32x16(s32, t_factors16x64[a].unique);
    s64 = shuffle64x32(l64, s32);

    goto done;

loop16x16:
    for (; i + 6 < len; i += 7) {
        a = in[i    ];
        b = in[i + 1];
        c = in[i + 2];
        d = in[i + 3];
        e = in[i + 4];
        f = in[i + 5];
        g = in[i + 6];

        s16_0 = shuffle16x16(s16, t16_current[a]);
        s16_1 = shuffle16x16(t16[a][b], t16[b][c]);
        s16_2 = shuffle16x16(t16[c][d], t16[d][e]);
        s16_3 = shuffle16x16(t16[e][f], t16[f][g]);

        s16_4 = shuffle16x16(s16_0, s16_1);
        s16_5 = shuffle16x16(s16_2, s16_3);

        s16 = shuffle16x16(s16_4, s16_5);

        t16_current = t16[g];
    }

    for (; i < len; i++) {
        a = in[i];
        s16 = shuffle16x16(s16, t16_current[a]);
        t16_current = t16[a];
    }

    a = in[i - 1];
    s16 = shuffle16x16(s16, t_factors16x64[a].unique);
    s64 = shuffle64x16(l64, s16);

    goto done;

done:
    for (i = 0; i < 64; i++) {
        out[i] = s64[i];
    }
}
