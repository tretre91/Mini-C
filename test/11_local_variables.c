#include <stdbool.h>

int GLOBAL;

int step(int i) {
  int step = 2;
  return i + step;
}

int main() {
  int local = 5;
  int default_value;
  bool b = false;

  GLOBAL = step(local);
  return default_value;
}