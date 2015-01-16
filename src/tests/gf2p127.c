#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "greatest.h"
#include "../sl2-inl.h"
#include "rand.h"

SUITE(gf2p127);

TEST addition(void) {
  gf2p127_t a = gf2p127_from_int(0x53);
  gf2p127_t b = gf2p127_from_int(0xCA);
  gf2p127_t c = gf2p127_from_int(0x99);
  ASSERT(gf2p127_eq(c, gf2p127_add(a, b)));
  PASS();
}

TEST mulbit(void) {
  srand(time(NULL));
  char bufa[1024], bufb[1024], bufc[1024];
  gf2p127_t a, b, c;
  int i;
  for (i = 0; i < 1024 * 1024; i++) {
    a = gf2p127_rand();
    b = gf2p127_mul(a, gf2p127_from_int(i % 2));
    c = gf2p127_mul_bit(a, i % 2);
    ASSERT_STR_EQm(gf2p127_hex(bufa, a), gf2p127_hex(bufb, b), gf2p127_hex(bufc, c));
  }
  PASS();
}

TEST mul00(void) {
  srand(time(NULL));
  char bufa[1024], bufb[1024], bufc[1024];
  gf2p127_t a, b, c;
  int i;
  for (i = 0; i < 1024 * 1024; i++) {
    a = gf2p127_rand();
    b = gf2p127_mul(a, gf2p127_from_int(0));
    c = gf2p127_mul_00(a);
    ASSERT_STR_EQm(gf2p127_hex(bufa, a), gf2p127_hex(bufb, b), gf2p127_hex(bufc, c));
  }
  PASS();
}

TEST mul01(void) {
  srand(time(NULL));
  char bufa[1024], bufb[1024], bufc[1024];
  gf2p127_t a, b, c;
  int i;
  for (i = 0; i < 1024 * 1024; i++) {
    a = gf2p127_rand();
    b = gf2p127_mul(a, gf2p127_from_int(1));
    c = gf2p127_mul_01(a);
    ASSERT_STR_EQm(gf2p127_hex(bufa, a), gf2p127_hex(bufb, b), gf2p127_hex(bufc, c));
  }
  PASS();
}

TEST mul10(void) {
  srand(time(NULL));
  char bufa[1024], bufb[1024], bufc[1024];
  gf2p127_t a, b, c;
  int i;
  for (i = 0; i < 1024 * 1024; i++) {
    a = gf2p127_rand();
    b = gf2p127_mul(a, gf2p127_from_int(2));
    c = gf2p127_mul_10(a);
    ASSERT_STR_EQm(gf2p127_hex(bufa, a), gf2p127_hex(bufb, b), gf2p127_hex(bufc, c));
  }
  PASS();
}

TEST mul11(void) {
  srand(time(NULL));
  char bufa[1024], bufb[1024], bufc[1024];
  gf2p127_t a, b, c;
  int i;
  for (i = 0; i < 1024 * 1024; i++) {
    a = gf2p127_rand();
    b = gf2p127_mul(a, gf2p127_from_int(3));
    c = gf2p127_mul_11(a);
    ASSERT_STR_EQm(gf2p127_hex(bufa, a), gf2p127_hex(bufb, b), gf2p127_hex(bufc, c));
  }
  PASS();
}

TEST multiplication(void) {
  gf2p127_t a = gf2p127_from_int(0x53);
  gf2p127_t b = gf2p127_from_int(0xCA);
  gf2p127_t c = gf2p127_from_int(0x3F7E);
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
  RUN_TEST(mulbit);
  RUN_TEST(mul00);
  RUN_TEST(mul01);
  RUN_TEST(mul10);
  RUN_TEST(mul11);
  RUN_TEST(multiplication);
}
