#include <stdio.h>

#include "greatest.h"
#include "../gf2p127.h"

SUITE(gf2p127);

TEST addition(void) {
  gf2p127_t a = _mm_cvtsi32_si128(0x53);
  gf2p127_t b = _mm_cvtsi32_si128(0xCA);
  gf2p127_t c = _mm_cvtsi32_si128(0x99);
  ASSERT(gf2p127_eq(c, gf2p127_add(a, b)));
  PASS();
}

TEST multiplication(void) {
  gf2p127_t a = _mm_cvtsi32_si128(0x53);
  gf2p127_t b = _mm_cvtsi32_si128(0xCA);
  gf2p127_t c = _mm_cvtsi32_si128(0x3F7E);
  gf2p127_t d = _mm_slli_si128(c, 14);
  gf2p127_t e = _mm_slli_si128(c, 10);
  ASSERT(gf2p127_eq(c, gf2p127_mul(a, b)));
  char buf[1024];
  ASSERT_STR_EQ("000000000fff3ffc000000000aaa2aa8",
                gf2p127_hex(buf, gf2p127_mul(d, e)));
  ASSERT_STR_EQ("000000000fff3ffc000000000aaa2aa8",
                gf2p127_hex(buf, gf2p127_mul(e, d)));
  PASS();
}

GREATEST_SUITE(gf2p127) {
  RUN_TEST(addition);
  RUN_TEST(multiplication);
}
