#pragma once

#include <smmintrin.h>

typedef __m128i tz_hash_t[2][2];

void tz_hash_unit(tz_hash_t h);

void tz_hash_append(tz_hash_t h, unsigned char *buf, size_t n);
void tz_hash_prepend(tz_hash_t h, unsigned char *buf, size_t n);

void tz_hash_concat(tz_hash_t a, tz_hash_t b, tz_hash_t c);

void tz_hash_serialize(tz_hash_t h, unsigned char[86]);
void tz_hash_unserialize(tz_hash_t h, unsigned char[86]);
