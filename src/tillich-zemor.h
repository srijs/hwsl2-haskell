#pragma once

#include <stddef.h>

#define TZ_HASH_SIZE 64
typedef struct tz_hash_struct *tz_hash_t;

void tz_hash_copy(tz_hash_t dst, tz_hash_t src);

int tz_hash_eq(tz_hash_t a, tz_hash_t b);

void tz_hash_unit(tz_hash_t);

void tz_hash_append(tz_hash_t h, unsigned char *buf, size_t n);
void tz_hash_prepend(tz_hash_t h, unsigned char *buf, size_t n);

void tz_hash_concat(tz_hash_t c, tz_hash_t a, tz_hash_t b);

void tz_hash_serialize(tz_hash_t h, unsigned char str[86]);
void tz_hash_unserialize(tz_hash_t h, unsigned char str[86]);
