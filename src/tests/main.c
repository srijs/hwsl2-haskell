#include "greatest.h"

extern SUITE(gf2p127);

GREATEST_MAIN_DEFS();

int main(int argc, char **argv) {
  GREATEST_MAIN_BEGIN();
  RUN_SUITE(gf2p127);
  GREATEST_MAIN_END();
}
