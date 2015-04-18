#pragma once

#include <stdio.h>
#include <stdint.h>
#include <immintrin.h>
#include <smmintrin.h>
#include <wmmintrin.h>

typedef __m128i gf2p127_t;
#ifdef __AVX2__
typedef __m256i gf2p127x2_t;
#endif

static const inline
_Bool gf2p127_valid(const gf2p127_t a) {
  return (_mm_extract_epi64(a, 1) & (UINT64_C(1) << 63)) == 0;
}

static const inline
_Bool gf2p127_eq(const gf2p127_t a, const gf2p127_t b) {
  _Bool lo = _mm_extract_epi64(a, 0) == _mm_extract_epi64(b, 0);
  _Bool hi = _mm_extract_epi64(a, 1) == _mm_extract_epi64(b, 1);
  return lo && hi;
}

static const inline
gf2p127_t gf2p127_zero() {
  return _mm_setzero_si128();
}

static const inline
gf2p127_t gf2p127_from_int(int a) {
  return _mm_cvtsi32_si128(a);
}

static const inline
gf2p127_t gf2p127_mul_bit(const gf2p127_t a, const _Bool bit) {
  return _mm_slli_epi64(a, !bit * 64);
}

static const inline
gf2p127_t gf2p127_add(const gf2p127_t a, const gf2p127_t b) {
  return _mm_xor_si128(a, b);
}

static const inline
gf2p127_t gf2p127_mul_00(const gf2p127_t a) {
  return _mm_setzero_si128();
}

static const inline
gf2p127_t gf2p127_mul_01(const gf2p127_t a) {
  return a;
}

static const gf2p127_t *minmax __attribute__((__aligned__(16))) =
  (gf2p127_t *)(uint64_t [4]){0, 0, UINT64_MAX, UINT64_MAX};

static const gf2p127_t *x127 __attribute__((__aligned__(16))) =
  (gf2p127_t *)(uint32_t [4]){0, 0, 0, 1 << 31};

static const inline
gf2p127_t gf2p127_mul_10(const gf2p127_t a) {
  // Shift lower and upper halves left by one bit,
  // resembling a multiplication by two.
  gf2p127_t sl = _mm_slli_epi64(a, 1);
  // Check for a x^127 overflow, and add the polynom and carry bit.
  gf2p127_t one = _mm_srli_epi64(_mm_alignr_epi8(a, sl, 8), 63);
  gf2p127_t over = _mm_and_si128(sl, _mm_load_si128(x127));
  gf2p127_t x127x63 = _mm_unpackhi_epi64(over, over);
  return _mm_xor_si128(_mm_xor_si128(sl, one), x127x63);
}

#ifdef __AVX2__
static const inline
gf2p127x2_t gf2p127x2_mul_10(const gf2p127x2_t ab) {
  // Shift lower and upper halves left by one bit,
  // resembling a multiplication by two.
  gf2p127x2_t sl = _mm256_slli_epi64(ab, 1);
  // Check for the x^63 carry bit, and the x^127 overflow, and construct x^64 + 1 polynomial.
  gf2p127x2_t carry = _mm256_srli_epi64(_mm256_alignr_epi8(ab, sl, 8), 63);
  gf2p127x2_t carried = _mm256_xor_si256(sl, carry);
  // Check for the x^127 overflow, and construct the x^127 + x^63 polymomial.
  gf2p127x2_t over1 = _mm256_srli_epi64(sl, 63);
  gf2p127x2_t over2 = _mm256_slli_epi64(over1, 63);
  gf2p127x2_t overhi = _mm256_unpackhi_epi64(over2, over2);
  return _mm256_xor_si256(overhi, carried);
}
#endif

static const inline
gf2p127_t gf2p127_mul_11(const gf2p127_t a) {
  gf2p127_t mul01 = gf2p127_mul_01(a);
  gf2p127_t mul10 = gf2p127_mul_10(a);
  return _mm_xor_si128(mul01, mul10);
};

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

static inline
char *gf2p127_hex(char *str, const gf2p127_t m) {
  sprintf(str, "%.16llx%.16llx", _mm_extract_epi64(m, 1),
                                 _mm_extract_epi64(m, 0));
  return str;
}
