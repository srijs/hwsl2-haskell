#pragma once

#include "gf2p127.h"

typedef gf2p127_t sl2_t[2][2];

void sl2_mul(sl2_t a, sl2_t b, sl2_t c);
