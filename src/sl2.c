#include "sl2-inl.h"

void sl2_mul_bit_buf_left(sl2_t b, unsigned char *buf, size_t n) {
  size_t i;
  for (i = n; i > 0; i--) {
    sl2_mul_bits_left(b, buf[i - 1]);
  }
}

void sl2_mul_buf_left(sl2_t b, unsigned char *buf, size_t n, sl2_t m[256]) {
  size_t i;
  for (i = n; i > 0; i--) {
    sl2_mul_byte_left(b, buf[i - 1], m);
  }
}

void sl2_mul_bit_buf_right(sl2_t a, unsigned char *buf, size_t n) {
  size_t i;
  for (i = 0; i < n; i++) {
    sl2_mul_bits_right(a, buf[i]);
  }
}

void sl2_mul_buf_right(sl2_t a, unsigned char *buf, size_t n, sl2_t m[256]) {
  size_t i;
  for (i = 0; i < n; i++) {
    sl2_mul_byte_right(a, buf[i], m);
  }
}

void sl2_init_256(sl2_t m[256]) {
  sl2_t m2[2];
  sl2_init(m2);
  int i, bit;
  for (i = 0; i < 256; i++) {
    bit = (i >> 7) & 1;
    m[i][0][0] = m2[bit][0][0];
    m[i][0][1] = m2[bit][0][1];
    m[i][1][0] = m2[bit][1][0];
    m[i][1][1] = m2[bit][1][1];
    sl2_mul_bit_right(m[i], (i >> 6) & 1, m[i]);
    sl2_mul_bit_right(m[i], (i >> 5) & 1, m[i]);
    sl2_mul_bit_right(m[i], (i >> 4) & 1, m[i]);
    sl2_mul_bit_right(m[i], (i >> 3) & 1, m[i]);
    sl2_mul_bit_right(m[i], (i >> 2) & 1, m[i]);
    sl2_mul_bit_right(m[i], (i >> 1) & 1, m[i]);
    sl2_mul_bit_right(m[i], (i >> 0) & 1, m[i]);
  }
}

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

void sl2_deserialize(sl2_t m, unsigned char buf[86]) {

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
