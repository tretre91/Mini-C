#include <stdbool.h>

int max(int a, int b) {
  if (a >= b) {
    return a;
  } else {
    return b;
  }
}

int abs(int x) {
  if (0 > x) {
    return x - 2 * x;
  } else {
    return x;
  }
}

int main() {
  int i = max(4, -3);
  bool b = i >= 0;
  if (b) {
    return 0;
  } else {
    return 1;
  }
}
