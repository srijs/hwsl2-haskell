#include "sl2-inl.h"

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
