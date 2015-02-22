#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>

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
  unsigned char str[1024];
  long long start, end;
  sl2_t a;
  printf("Benchmarking prepend...\n");
  start = ustime();
  sl2_unit(a);
  sl2_mul_buf_left(a, buf, size);
  end = ustime();
  printf("Took %lli nanoseconds.\n", end - start);
  memset(str, 0, 1024);
  sl2_serialize(a, str);
  printf("Result: %s\n", str);
  printf("Benchmarking append...\n");
  start = ustime();
  sl2_unit(a);
  sl2_mul_buf_right(a, buf, size);
  end = ustime();
  printf("Took %lli nanoseconds.\n", end - start);
  memset(str, 0, 1024);
  sl2_serialize(a, str);
  printf("Result: %s\n", str);
}
