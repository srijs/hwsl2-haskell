#include "tillich-zemor.h"
#include "sl2-inl.h"

struct tz_hash_struct {
  sl2_t sl2;
};

int tz_hash_eq(tz_hash_t a, tz_hash_t b) {
  return sl2_eq(a->sl2, b->sl2);
}

void tz_hash_copy(tz_hash_t dst, tz_hash_t src) {
  sl2_copy(dst->sl2, src->sl2);
}

void tz_hash_unit(tz_hash_t h) {
  sl2_unit(h->sl2);
}

void tz_hash_append(tz_hash_t h, unsigned char *buf, size_t n) {
  size_t i;
  for (i = 0; i < n; i++) {
    sl2_mul_bits_right(h->sl2, buf[i]);
  }
}

void tz_hash_prepend(tz_hash_t h, unsigned char *buf, size_t n) {
  size_t i;
  for (i = n; i > 0; i--) {
    sl2_mul_bits_left(h->sl2, buf[i - 1]);
  }
}

void tz_hash_concat(tz_hash_t a, tz_hash_t b, tz_hash_t c) {
  sl2_mul(a->sl2, b->sl2, c->sl2);
}

void tz_hash_serialize(tz_hash_t h, unsigned char buf[86]) {
  sl2_serialize(h->sl2, buf);
}

void tz_hash_unserialize(tz_hash_t h, unsigned char buf[86]) {
  sl2_unserialize(h->sl2, buf);
}
