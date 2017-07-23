#include <stdint.h>
#include <stdlib.h>

typedef uint8_t v16si __attribute__ ((vector_size (16)));

void run(char *in, size_t len, unsigned char *tBytes, char *out) {
    unsigned char a, b, c, d, e, f, g;
    int i, j;
    v16si s = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 };
    v16si s0, s1, s2, s3, s4, s5;
    v16si t[256];

    for (i = 0; i < 256; i++) {
        for (j = 0; j < 16; j++) {
            t[i][j] = tBytes[16 * i + j];
        }
    }

    for (i = 0; i + 6 < len; i += 7) {
        // These can be run in parallel
        a = in[i    ];
        b = in[i + 1];
        c = in[i + 2];
        d = in[i + 3];
        e = in[i + 4];
        f = in[i + 5];
        g = in[i + 6];

        // These can be run in parallel
        s0 = __builtin_shuffle(t[a], s   );
        s1 = __builtin_shuffle(t[c], t[b]);
        s2 = __builtin_shuffle(t[e], t[d]);
        s3 = __builtin_shuffle(t[g], t[f]);

        // These can be run in parallel
        s4 = __builtin_shuffle(s1  , s0  );
        s5 = __builtin_shuffle(s3  , s2  );

        s  = __builtin_shuffle(s5  , s4  );
    }
    for (j = i; j < len; j++) {
        a = in[j];
        s = __builtin_shuffle(t[a], s);
    }

    for (i = 0; i < 16; i++) {
        out[i] = s[i];
    }
}
