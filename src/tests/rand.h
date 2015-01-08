static gf2p127_t gf2p127_rand() {
  return *(gf2p127_t *)(unsigned char[16]){
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 256,
    rand() % 128
  };
}

static void sl2_rand(sl2_t a) {
  a[0][0] = gf2p127_rand();
  a[0][1] = gf2p127_rand();
  a[1][0] = gf2p127_rand();
  a[1][1] = gf2p127_rand();
}
