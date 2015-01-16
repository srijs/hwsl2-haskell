#include "tillich-zemor.h"
#include "sl2-inl.h"

void tz_hash_unit(tz_hash_t h) {
  sl2_unit(h);
}

void tz_hash_append(tz_hash_t h, unsigned char *buf, size_t n) {
  size_t i;
  unsigned char byte;
  for (i = 0; i < n; i++) {
    byte = buf[i];
    sl2_mul_bit_right(h, (byte >> 7) & 1, h);
    sl2_mul_bit_right(h, (byte >> 6) & 1, h);
    sl2_mul_bit_right(h, (byte >> 5) & 1, h);
    sl2_mul_bit_right(h, (byte >> 4) & 1, h);
    sl2_mul_bit_right(h, (byte >> 3) & 1, h);
    sl2_mul_bit_right(h, (byte >> 2) & 1, h);
    sl2_mul_bit_right(h, (byte >> 1) & 1, h);
    sl2_mul_bit_right(h, (byte >> 0) & 1, h);
  }
}

void tz_hash_prepend(tz_hash_t h, unsigned char *buf, size_t n) {
  size_t i;
  unsigned char byte;
  for (i = n; i > 0; i--) {
    byte = buf[i - 1];
    sl2_mul_bit_left(h, (byte >> 0) & 1, h);
    sl2_mul_bit_left(h, (byte >> 1) & 1, h);
    sl2_mul_bit_left(h, (byte >> 2) & 1, h);
    sl2_mul_bit_left(h, (byte >> 3) & 1, h);
    sl2_mul_bit_left(h, (byte >> 4) & 1, h);
    sl2_mul_bit_left(h, (byte >> 5) & 1, h);
    sl2_mul_bit_left(h, (byte >> 6) & 1, h);
    sl2_mul_bit_left(h, (byte >> 7) & 1, h);
  }
}

void tz_hash_concat(tz_hash_t a, tz_hash_t b, tz_hash_t c) {
  sl2_mul(a, b, c);
}

void tz_hash_serialize(tz_hash_t h, unsigned char buf[83]) {
  sl2_serialize(h, buf);
}

void tz_hash_unserialize(tz_hash_t h, unsigned char buf[83]) {
  sl2_unserialize(h, buf);
}
