#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "../sl2-inl.h"

static long long ustime(void) {
  struct timeval tv;
  long long ust;
  gettimeofday(&tv, NULL);
  ust = ((long)tv.tv_sec)*1000000;
  ust += tv.tv_usec;
  return ust;
}

int main(void) {
  size_t size = 1024 * 1024 * 4;
  unsigned char *buf = malloc(size);
  int fd = open("/dev/urandom", O_RDONLY);
  read(fd, buf, size);
  char str[1024];
  long long start, end;
  sl2_t a, mbyte[256];
  printf("Benchmarking per-byte left multiplication...\n");
  start = ustime();
  sl2_init_256(mbyte);
  a[0][0] = mbyte[buf[size - 1]][0][0];
  a[0][1] = mbyte[buf[size - 1]][0][1];
  a[1][0] = mbyte[buf[size - 1]][1][0];
  a[1][1] = mbyte[buf[size - 1]][1][1];
  sl2_mul_buf_left(a, buf, size - 1, mbyte);
  end = ustime();
  printf("Took %lli nanoseconds.\n", end - start);
  printf("Result: %s\n", sl2_hex(str, a));
  printf("Benchmarking per-bit left multiplication...\n");
  start = ustime();
  a[0][0] = mbyte[buf[size - 1]][0][0];
  a[0][1] = mbyte[buf[size - 1]][0][1];
  a[1][0] = mbyte[buf[size - 1]][1][0];
  a[1][1] = mbyte[buf[size - 1]][1][1];
  sl2_mul_bit_buf_left(a, buf, size - 1);
  end = ustime();
  printf("Took %lli nanoseconds.\n", end - start);
  printf("Result: %s\n", sl2_hex(str, a));
  printf("Benchmarking per-byte right multiplication...\n");
  start = ustime();
  sl2_init_256(mbyte);
  a[0][0] = mbyte[buf[0]][0][0];
  a[0][1] = mbyte[buf[0]][0][1];
  a[1][0] = mbyte[buf[0]][1][0];
  a[1][1] = mbyte[buf[0]][1][1];
  sl2_mul_buf_right(a, buf + 1, size - 1, mbyte);
  end = ustime();
  printf("Took %lli nanoseconds.\n", end - start);
  printf("Result: %s\n", sl2_hex(str, a));
  printf("Benchmarking per-bit right multiplication...\n");
  start = ustime();
  a[0][0] = mbyte[buf[0]][0][0];
  a[0][1] = mbyte[buf[0]][0][1];
  a[1][0] = mbyte[buf[0]][1][0];
  a[1][1] = mbyte[buf[0]][1][1];
  sl2_mul_bit_buf_right(a, buf + 1, size - 1);
  end = ustime();
  printf("Took %lli nanoseconds.\n", end - start);
  printf("Result: %s\n", sl2_hex(str, a));
}
