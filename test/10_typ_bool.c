#include <stdbool.h>

bool GLOBAL = true;
bool DEFAULT;

bool id(bool b) {
  return b;
}

int main() {
  GLOBAL = false;
  GLOBAL = id(DEFAULT);
  return 0;
}
