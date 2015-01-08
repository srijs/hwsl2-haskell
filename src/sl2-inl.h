#pragma once

#include "sl2.h"

static inline
void sl2_mul_A_b(sl2_t b, sl2_t c) {
  // A: {00 = 10, 01 = 01, 10 = 01, 11 = 00}
  gf2p127_t b00 = b[0][0];
  gf2p127_t b01 = b[0][1];
  gf2p127_t b10 = b[1][0];
  gf2p127_t b11 = b[1][1];
  c[0][0] = gf2p127_add(gf2p127_mul_10(b00),
                        gf2p127_mul_01(b10));
  c[0][1] = gf2p127_add(gf2p127_mul_10(b01),
                        gf2p127_mul_01(b11));
  c[1][0] = gf2p127_add(gf2p127_mul_01(b00),
                        gf2p127_mul_00(b10));
  c[1][1] = gf2p127_add(gf2p127_mul_01(b01),
                        gf2p127_mul_00(b11));
};

static inline
void sl2_mul_B_b(sl2_t b, sl2_t c) {
  // B: {00 = 10, 01 = 11, 10 = 01, 11 = 01}
  gf2p127_t b00 = b[0][0];
  gf2p127_t b01 = b[0][1];
  gf2p127_t b10 = b[1][0];
  gf2p127_t b11 = b[1][1];
  c[0][0] = gf2p127_add(gf2p127_mul_10(b00),
                        gf2p127_mul_11(b10));
  c[0][1] = gf2p127_add(gf2p127_mul_10(b01),
                        gf2p127_mul_11(b11));
  c[1][0] = gf2p127_add(gf2p127_mul_01(b00),
                        gf2p127_mul_01(b10));
  c[1][1] = gf2p127_add(gf2p127_mul_01(b01),
                        gf2p127_mul_01(b11));
};

static inline
void sl2_mul_bit_right(sl2_t a, int bit, sl2_t c) {
  // A: {00 = 10, 01 = 01, 10 = 01, 11 = 00}
  // B: {00 = 10, 01 = 11, 10 = 01, 11 = 01}
  gf2p127_t a00 = a[0][0];
  gf2p127_t a10 = a[1][0];
  gf2p127_t a01 = a[0][1];
  gf2p127_t a11 = a[1][1];
  gf2p127_t m10a00a01 = gf2p127_add(gf2p127_mul_10(a00), a01);
  gf2p127_t m10a10a11 = gf2p127_add(gf2p127_mul_10(a10), a11);
  gf2p127_t c01[2], c11[2];
  c01[0] = a00;
  c11[0] = a10;
  c01[1] = gf2p127_add(a00, m10a00a01);
  c11[1] = gf2p127_add(a10, m10a10a11);
  c[0][0] = m10a00a01;
  c[1][0] = m10a10a11;
  c[0][1] = c01[bit];
  c[1][1] = c11[bit];
}

static inline
void sl2_mul_bits_right(sl2_t a, unsigned char byte) {
  sl2_mul_bit_right(a, (byte >> 7) & 1, a);
  sl2_mul_bit_right(a, (byte >> 6) & 1, a);
  sl2_mul_bit_right(a, (byte >> 5) & 1, a);
  sl2_mul_bit_right(a, (byte >> 4) & 1, a);
  sl2_mul_bit_right(a, (byte >> 3) & 1, a);
  sl2_mul_bit_right(a, (byte >> 2) & 1, a);
  sl2_mul_bit_right(a, (byte >> 1) & 1, a);
  sl2_mul_bit_right(a, (byte >> 0) & 1, a);
}

static inline
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

static inline
void sl2_mul_byte_right(sl2_t a, unsigned char byte, sl2_t m[256]) {
  sl2_mul(a, m[byte], a);
}

static inline
void sl2_init(sl2_t m[2]) {
  m[0][0][0] = gf2p127_from_int(2);
  m[0][0][1] = gf2p127_from_int(1);
  m[0][1][0] = gf2p127_from_int(1);
  m[0][1][1] = gf2p127_from_int(0);
  m[1][0][0] = gf2p127_from_int(2);
  m[1][0][1] = gf2p127_from_int(3);
  m[1][1][0] = gf2p127_from_int(1);
  m[1][1][1] = gf2p127_from_int(1);
}
