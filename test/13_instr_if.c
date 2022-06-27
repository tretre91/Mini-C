#include <stdbool.h>

int GLOBAL = 5;

bool not(bool b) {
  if (b) {
    return false;
  } else {
    return true;
  }
}

int main() {
  int i;
  bool b;
  if (GLOBAL < 4) {
    i = 5;
    return i;
  } else {
    b = not(true);
    if (b) {
      return 1;
    } else {
      return 0;
    }
  }
}
