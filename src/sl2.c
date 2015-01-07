#include "sl2.h"

void sl2_mul_a(sl2_t b, sl2_t c) {
  // A: {00 = 10, 01 = 01, 10 = 01, 11 = 00}
  c[0][0] = gf2p127_add(gf2p127_mul_10(b[0][0]),
                        gf2p127_mul_01(b[1][0]));
  c[0][1] = gf2p127_add(gf2p127_mul_10(b[0][1]),
                        gf2p127_mul_01(b[1][1]));
  c[1][0] = gf2p127_add(gf2p127_mul_01(b[0][0]),
                        gf2p127_mul_00(b[1][0]));
  c[1][1] = gf2p127_add(gf2p127_mul_01(b[0][1]),
                        gf2p127_mul_00(b[1][1]));
};

void sl2_mul_b(sl2_t a, sl2_t c) {
  // B: {00 = 10, 01 = 11, 10 = 01, 11 = 01}
  c[0][0] = gf2p127_add(gf2p127_mul_10(a[0][0]),
                        gf2p127_mul_11(a[1][0]));
  c[0][1] = gf2p127_add(gf2p127_mul_10(a[0][1]),
                        gf2p127_mul_11(a[1][1]));
  c[1][0] = gf2p127_add(gf2p127_mul_01(a[0][0]),
                        gf2p127_mul_01(a[1][0]));
  c[1][1] = gf2p127_add(gf2p127_mul_01(a[0][1]),
                        gf2p127_mul_01(a[1][1]));
};

void sl2_mul(sl2_t a, sl2_t b, sl2_t c) {
  // Strassen algorithm
  gf2p127_t m0, m1, m2, m3, m4, m5, m6;
  m0 = gf2p127_mul(gf2p127_add(a[0][0], a[1][1]),
                   gf2p127_add(b[0][0], b[1][1]));
  m1 = gf2p127_mul(gf2p127_add(a[1][0], a[1][1]), b[0][0]);
  m2 = gf2p127_mul(a[0][0], gf2p127_add(b[0][1], b[1][1]));
  m3 = gf2p127_mul(a[1][1], gf2p127_add(b[1][0], b[0][0]));
  m4 = gf2p127_mul(gf2p127_add(a[0][0], a[0][1]), b[1][1]);
  m5 = gf2p127_mul(gf2p127_add(a[1][0], a[0][0]),
                   gf2p127_add(b[0][0], b[0][1]));
  m6 = gf2p127_mul(gf2p127_add(a[0][1], a[1][1]),
                   gf2p127_add(b[1][0], b[1][1]));
  c[0][0] = gf2p127_add(gf2p127_add(m0, m3), gf2p127_add(m4, m6));
  c[0][1] = gf2p127_add(m2, m4);
  c[1][0] = gf2p127_add(m1, m3);
  c[1][1] = gf2p127_add(gf2p127_add(m0, m1), gf2p127_add(m2, m5));
};
