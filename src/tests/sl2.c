#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "greatest.h"
#include "../sl2-inl.h"
#include "rand.h"

SUITE(sl2);

TEST multiplication(void) {
  sl2_t a, b, c, d;
  char bufa[1024], bufb[1024];
  int i;
  for (i = 0; i < 1024; i++) {
    sl2_rand(a);
    sl2_rand(b);
    sl2_mul(a, b, c);
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
    sl2_mul(a, b, d);
    sl2_mul(d, c, d);
    sl2_mul(b, c, e);
    sl2_mul(a, e, e);
    ASSERT_STR_EQ(sl2_hex(bufa, d), sl2_hex(bufb, e));
  }
  PASS();
}

TEST multiplication_special_bit_right(void) {
  sl2_t a, b, c, m[2];
  char bufa[1024], bufb[1024];
  int i;
  sl2_init(m);
  for (i = 0; i < 1024; i++) {
    sl2_rand(a);
    sl2_mul(a, m[i % 2], b);
    sl2_mul_bit_right(a, i % 2, c);
    ASSERT_STR_EQ(gf2p127_hex(bufa, b[0][0]), gf2p127_hex(bufb, c[0][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, b[0][1]), gf2p127_hex(bufb, c[0][1]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, b[1][0]), gf2p127_hex(bufb, c[1][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, b[1][1]), gf2p127_hex(bufb, c[1][1]));
  }
  PASS();
}

TEST multiplication_special_bits_right(void) {
  sl2_t a, b, m[2], mbyte[256];
  char bufa[1024], bufb[1024];
  int i;
  sl2_init(m);
  sl2_init_256(mbyte);
  for (i = 0; i < 1024 * 1024; i++) {
    sl2_rand(a);
    b[0][0] = a[0][0];
    b[0][1] = a[0][1];
    b[1][0] = a[1][0];
    b[1][1] = a[1][1];
    sl2_mul_bits_right(a, i % 256);
    sl2_mul_byte_right(b, i % 256, mbyte);
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[0][0]), gf2p127_hex(bufb, b[0][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[0][1]), gf2p127_hex(bufb, b[0][1]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[1][0]), gf2p127_hex(bufb, b[1][0]));
    ASSERT_STR_EQ(gf2p127_hex(bufa, a[1][1]), gf2p127_hex(bufb, b[1][1]));
  }
  PASS();
}

GREATEST_SUITE(sl2) {
  RUN_TEST(multiplication);
  RUN_TEST(multiplication_associativity);
  RUN_TEST(multiplication_special_bit_right);
  RUN_TEST(multiplication_special_bits_right);
}
