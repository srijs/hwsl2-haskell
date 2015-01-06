#pragma once

#include <stdio.h>
#include <smmintrin.h>
#include <wmmintrin.h>

typedef __m128i gf2p127_t;

static const inline
_Bool gf2p127_eq(const gf2p127_t a, const gf2p127_t b) {
  _Bool lo = _mm_extract_epi64(a, 0) == _mm_extract_epi64(b, 0);
  _Bool hi = _mm_extract_epi64(a, 1) == _mm_extract_epi64(b, 1);
  return lo && hi;
}

static const inline
gf2p127_t gf2p127_add(const gf2p127_t a, const gf2p127_t b) {
  return _mm_xor_si128(a, b);
}

static const inline
gf2p127_t gf2p127_mul(const gf2p127_t a, const gf2p127_t b) {
  gf2p127_t tmp, lo, hi;

  // Multiplication (Karatsuba):
  //   tmp <- (a0 + a1) * (b0 + b1)
  tmp = _mm_xor_si128(_mm_unpacklo_epi64(a, b), _mm_unpackhi_epi64(a, b));
  tmp = _mm_clmulepi64_si128(tmp, tmp, 0x10);
  //   lo <- a0 * b0
  lo = _mm_clmulepi64_si128(a, b, 0x00);
  //   hi <- a1 * b1
  hi = _mm_clmulepi64_si128(a, b, 0x11);
  //   tmp <- (a0 + a1) * (b0 + b1) + a0 * b0 + a1 * b1
  tmp = _mm_xor_si128(tmp, _mm_xor_si128(lo, hi));
  //   lo <- a0 * b0 + low64[(a0 + a1) * (b0 + b1) + a0 * b0 + a1 * b1] * x^64
  lo = _mm_xor_si128(lo, _mm_slli_si128(tmp, 8));
  //   hi <- a1 * b1 + high64[(a0 + a1) * (b0 + b1) + a0 * b0 + a1 * b1]
  hi = _mm_xor_si128(hi, _mm_srli_si128(tmp, 8));

  // Reduction (modulo f(x) = x^127 + x^63 + 1)
  //   tmp <- low64[hi] * x^64 + high64[lo]
  tmp = _mm_alignr_epi8(hi, lo, 8);
  tmp = _mm_xor_si128(tmp, hi);
  hi = _mm_slli_epi64(hi, 1);
  lo = _mm_xor_si128(lo, hi);
  hi = _mm_unpackhi_epi64(hi, tmp);
  lo = _mm_xor_si128(lo, hi);
  tmp = _mm_srli_epi64(tmp, 63);
  lo = _mm_xor_si128(lo, tmp);
  hi = _mm_unpacklo_epi64(tmp, tmp);
  lo = _mm_xor_si128(lo, _mm_slli_epi64(hi, 63));

  return lo;
}

static
char *gf2p127_show(char *buf, const gf2p127_t m) {
  __uint128_t a = (__uint128_t)m;
  unsigned int k;
  int n;
  char *str = buf;
  if (a & 1) {
    str += sprintf(str, "1");
  } else {
    str += sprintf(str, "0");
  }
  a >>= 1;
  for (k = 1; k < 128; k++) {
    if (a & 1) {
      n = sprintf(str, " + 2^%i", k);
      str += n;
    }
    a >>= 1;
  }
  return buf;
}

static
char *gf2p127_hex(char *str, const gf2p127_t m) {
  sprintf(str, "%.16llx%.16llx", _mm_extract_epi64(m, 1),
                                 _mm_extract_epi64(m, 0));
  return str;
}
