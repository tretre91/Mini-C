#include <stdio.h>

int main() {
  int c;
  int i = 50;

  for (int i = 0; i < 10; i = i + 1) {
    int c = 48 + i;
    putchar(c);
    putchar(10);
  }

  for (i = 10, c = 46;; i = i - 1, c = 48 + i) {
    if (i < 0) {
      return i + 1;
    } else {
      putchar(c);
      putchar(10);
    }
  }

  return i + 1;
}
