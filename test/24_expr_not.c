#include <stdbool.h>

bool not(bool b) {
  return !b;
}

int main() {
  bool b = !!!true;
  if (not(!b)) {
    return 1;
  } else {
    return 0;
  }
}
