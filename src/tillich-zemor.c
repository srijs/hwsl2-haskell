#include "tillich-zemor.h"
#include "sl2-inl.h"

void tz_hash_unit(tz_hash_t h) {
  sl2_unit(h);
}

void tz_hash_append(tz_hash_t h, unsigned char *buf, size_t n) {
  size_t i;
  for (i = 0; i < n; i++) {
    sl2_mul_bits_right(h, buf[i]);
  }
}

void tz_hash_prepend(tz_hash_t h, unsigned char *buf, size_t n) {
  size_t i;
  for (i = n; i > 0; i--) {
    sl2_mul_bits_left(h, buf[i - 1]);
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
