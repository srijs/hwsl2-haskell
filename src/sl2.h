#pragma once

#include "gf2p127.h"

typedef gf2p127_t sl2_t[2][2];

void sl2_init_256(sl2_t m[256]);
void sl2_mul_buf_right(sl2_t a, unsigned char *buf, size_t n, sl2_t m[256]);

void sl2_mul_bit_buf_right(sl2_t a, unsigned char *buf, size_t n);

char *sl2_hex(char *buf, sl2_t a);
