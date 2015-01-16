#pragma once

#include "gf2p127.h"

typedef gf2p127_t sl2_t[2][2];

void sl2_unit(sl2_t a);

void sl2_mul_buf_left(sl2_t b, unsigned char *buf, size_t n);
void sl2_mul_buf_right(sl2_t a, unsigned char *buf, size_t n);

void sl2_serialize(sl2_t a, unsigned char buf[86]);
void sl2_deserialize(sl2_t a, unsigned char buf[86]);

char *sl2_hex(char *buf, sl2_t a);
