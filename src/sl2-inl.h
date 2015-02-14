#pragma once

#include "gf2p127-inl.h"

typedef gf2p127_t sl2_t[2][2];

static inline
_Bool sl2_eq(sl2_t a, sl2_t b) {
  return gf2p127_eq(a[0][0], b[0][0]) &&
         gf2p127_eq(a[0][1], b[0][1]) &&
         gf2p127_eq(a[1][0], b[1][0]) &&
         gf2p127_eq(a[1][1], b[1][1]);
}

static inline
void sl2_copy(sl2_t dst, sl2_t src) {
  dst[0][0] = src[0][0];
  dst[0][1] = src[0][1];
  dst[1][0] = src[1][0];
  dst[1][1] = src[1][1];
}

static inline
void sl2_mul_bit_left(sl2_t b, int bit) {
  // A: {00 = 10, 01 = 01, 10 = 01, 11 = 00}
  // B: {00 = 10, 01 = 11, 10 = 01, 11 = 01}
  gf2p127_t b00 = b[0][0];
  gf2p127_t b10 = b[1][0];
  gf2p127_t b01 = b[0][1];
  gf2p127_t b11 = b[1][1];
  b[0][0] = gf2p127_zero();
  b[0][1] = gf2p127_zero();
  b[1][0] = gf2p127_add(b00, b[bit][0]);
  b[1][1] = gf2p127_add(b01, b[bit][1]);
  b[0][0] = gf2p127_add(b10, gf2p127_mul_10_left(b[1][0]));
  b[0][1] = gf2p127_add(b11, gf2p127_mul_10_left(b[1][1]));
}

static inline
void sl2_mul_bits_left(sl2_t b, unsigned char byte) {
  sl2_mul_bit_left(b, (byte >> 0) & 1);
  sl2_mul_bit_left(b, (byte >> 1) & 1);
  sl2_mul_bit_left(b, (byte >> 2) & 1);
  sl2_mul_bit_left(b, (byte >> 3) & 1);
  sl2_mul_bit_left(b, (byte >> 4) & 1);
  sl2_mul_bit_left(b, (byte >> 5) & 1);
  sl2_mul_bit_left(b, (byte >> 6) & 1);
  sl2_mul_bit_left(b, (byte >> 7) & 1);
}

static inline
void sl2_mul_bit_right(sl2_t a, int bit) {
  // A: {00 = 10, 01 = 01, 10 = 01, 11 = 00}
  // B: {00 = 10, 01 = 11, 10 = 01, 11 = 01}
  gf2p127_t a00 = a[0][0];
  gf2p127_t a10 = a[1][0];
  gf2p127_t a01 = a[0][1];
  gf2p127_t a11 = a[1][1];
  a[0][1] = gf2p127_zero();
  a[1][1] = gf2p127_zero();
  a[0][0] = gf2p127_add(gf2p127_mul_10(a00), a01);
  a[1][0] = gf2p127_add(gf2p127_mul_10(a10), a11);
  a[0][1] = gf2p127_add(a00, a[0][!bit]);
  a[1][1] = gf2p127_add(a10, a[1][!bit]);
}

static inline
void sl2_mul_bits_right(sl2_t a, unsigned char byte) {
  sl2_mul_bit_right(a, (byte >> 7) & 1);
  sl2_mul_bit_right(a, (byte >> 6) & 1);
  sl2_mul_bit_right(a, (byte >> 5) & 1);
  sl2_mul_bit_right(a, (byte >> 4) & 1);
  sl2_mul_bit_right(a, (byte >> 3) & 1);
  sl2_mul_bit_right(a, (byte >> 2) & 1);
  sl2_mul_bit_right(a, (byte >> 1) & 1);
  sl2_mul_bit_right(a, (byte >> 0) & 1);
}

static inline
void sl2_mul(sl2_t c, sl2_t a, sl2_t b) {
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
}

static inline
void sl2_mul_byte_left(sl2_t b, unsigned char byte, sl2_t m[256]) {
  sl2_mul(m[byte], b, b);
}

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

static inline
void sl2_unit(sl2_t a) {
  a[0][0] = gf2p127_from_int(1);
  a[0][1] = gf2p127_from_int(0);
  a[1][0] = gf2p127_from_int(0);
  a[1][1] = gf2p127_from_int(1);
}

static inline
char *sl2_hex(char *buf, sl2_t a) {
  gf2p127_hex(&buf[0],  a[0][0]);
  gf2p127_hex(&buf[32], a[0][1]);
  gf2p127_hex(&buf[64], a[1][0]);
  gf2p127_hex(&buf[96], a[1][1]);
  return buf;
}

static const unsigned char b64[64] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

static const unsigned char unb64[256] = {
  ['A'] =  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12,
          13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
  ['a'] = 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
          39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
  ['0'] = 52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
  ['-'] = 62, ['_'] = 63
};

static inline
void sl2_serialize(sl2_t m, unsigned char buf[86]) {

  int i, j;
  unsigned char a, b, c, *data = (unsigned char *)m;

  for (i = j = 0; i <= 64 - 3; i += 3, j += 4) {
    a = data[i + 0];
    b = data[i + 1];
    c = data[i + 2];
    buf[j + 0] = b64[a >> 2];
    buf[j + 1] = b64[((0x03 & a) << 4) + (b >> 4)];
    buf[j + 2] = b64[((0x0f & b) << 2) + (c >> 6)];
    buf[j + 3] = b64[0x3f & c];
  }

  buf[84] = b64[data[i] >> 2];
  buf[85] = b64[(0x3 & data[i]) << 4];

}

static inline
void sl2_unserialize(sl2_t m, unsigned char buf[86]) {

  int i, j;
  unsigned char a, b, c, d, *data = (unsigned char *)m;

  for (i = j = 0; i <= 86 - 4; i += 4, j += 3) {
    a = unb64[buf[i + 0]];
    b = unb64[buf[i + 1]];
    c = unb64[buf[i + 2]];
    d = unb64[buf[i + 3]];
    data[j + 0] = (a << 2) | (b >> 4);
    data[j + 1] = (b << 4) | (c >> 2);
    data[j + 2] = (c << 6) | (d);
  }

  data[63] = (unb64[buf[i]] << 2) | (unb64[buf[i + 1]] >> 4);

}
