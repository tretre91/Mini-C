#include <stdbool.h>

int GLOBAL = 2;

bool lt0(int i) {
  return i < 0;
}

int main() {
  int i = 5;
  bool b = lt0(i);
  bool t = 2 * i < GLOBAL + 2;
  1 < 0;
  return 0;
}
