#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "greatest.h"
#include "../sl2-inl.h"
#include "rand.h"

SUITE(sl2);

TEST cmp(void) {
  sl2_t a, b;
  int i;
  for (i = 0; i < 1024; i++) {
    sl2_rand(a);
    sl2_rand(b);
    ASSERT(sl2_cmp(a, a) == 0);
    ASSERT(sl2_cmp(a, b) != 0);
  }
  PASS();
}

TEST multiplication(void) {
  sl2_t a, b, c, d;
  char bufa[1024], bufb[1024];
  int i;
  for (i = 0; i < 1024; i++) {
    sl2_rand(a);
    sl2_rand(b);
    sl2_mul(c, a, b);
    d[0][0] = gf2p127_add(gf2p127_mul(a[0][0], b[0][0]),
                          gf2p127_mul(a[0][1], b[1][0]));
    d[0][1] = gf2p127_add(gf2p127_mul(a[0][0], b[0][1]),
                          gf2p127_mul(a[0][1], b[1][1]));
    d[1][0] = gf2p127_add(gf2p127_mul(a[1][0], b[0][0]),
                          gf2p127_mul(a[1][1], b[1][0]));
    d[1][1] = gf2p127_add(gf2p127_mul(a[1][0], b[0][1]),
                          gf2p127_mul(a[1][1], b[1][1]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, c[0][0]), gf2p127_hex(bufb, d[0][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, c[0][1]), gf2p127_hex(bufb, d[0][1]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, c[1][0]), gf2p127_hex(bufb, d[1][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, c[1][1]), gf2p127_hex(bufb, d[1][1]));
  }
  PASS();
}

TEST multiplication_associativity(void) {
  sl2_t a, b, c, d, e;
  char bufa[1024], bufb[1024];
  int i;
  for (i = 0; i < 1024; i++) {
    sl2_rand(a);
    sl2_rand(b);
    sl2_rand(c);
    sl2_mul(d, a, b);
    sl2_mul(d, d, c);
    sl2_mul(e, b, c);
    sl2_mul(e, a, e);
    ASSERT_STR_EQ(sl2_hex(bufa, d), sl2_hex(bufb, e));
  }
  PASS();
}

TEST multiplication_special_bit_left(void) {
  sl2_t a, b, m[2];
  char bufa[1024], bufb[1024];
  int i;
  sl2_init(m);
  for (i = 0; i < 1024; i++) {
    sl2_rand(a);
    sl2_mul(b, m[i % 2], a);
    sl2_mul_bit_left(&a[0][0], &a[0][1], &a[1][0], &a[1][1], _mm_load_si128(&minmax[i % 2]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[0][0]), gf2p127_hex(bufb, b[0][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[0][1]), gf2p127_hex(bufb, b[0][1]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[1][0]), gf2p127_hex(bufb, b[1][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[1][1]), gf2p127_hex(bufb, b[1][1]));
  }
  PASS();
}

TEST multiplication_special_bit_right(void) {
  sl2_t a, b, m[2];
  char bufa[1024], bufb[1024];
  int i;
  sl2_init(m);
  for (i = 0; i < 1024; i++) {
    sl2_rand(a);
    sl2_mul(b, a, m[i % 2]);
    sl2_mul_bit_right(&a[0][0], &a[0][1], &a[1][0], &a[1][1], _mm_load_si128(&minmax[i % 2]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[0][0]), gf2p127_hex(bufb, b[0][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[0][1]), gf2p127_hex(bufb, b[0][1]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[1][0]), gf2p127_hex(bufb, b[1][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[1][1]), gf2p127_hex(bufb, b[1][1]));
  }
  PASS();
}

TEST serialize(void) {
  sl2_t a, b;
  unsigned char bufa[256] = {0};
  char bufb[256], bufc[256];
  int i;
  for (i = 0; i < 1024; i++) {
    sl2_rand(a);
    sl2_serialize(a, bufa);
    sl2_unserialize(b, bufa);
    ASSERT_STR_EQ(gf2p127_hex(bufb, a[0][0]), gf2p127_hex(bufc, b[0][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufb, a[0][1]), gf2p127_hex(bufc, b[0][1]));
    ASSERT_STR_EQ(gf2p127_hex(bufb, a[1][0]), gf2p127_hex(bufc, b[1][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufb, a[1][1]), gf2p127_hex(bufc, b[1][1]));
  }
  PASS();
}

GREATEST_SUITE(sl2) {
  RUN_TEST(cmp);
  RUN_TEST(multiplication);
  RUN_TEST(multiplication_associativity);
  RUN_TEST(multiplication_special_bit_left);
  RUN_TEST(multiplication_special_bit_right);
  RUN_TEST(serialize);
}
