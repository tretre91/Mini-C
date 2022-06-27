#include <stdbool.h>

int main() {
  bool b = true || 1 < 0;
  bool b2 = b && true && (b == b || false);
  if (b && b2) {
    return 0;
  } else {
    return 1;
  }
}
