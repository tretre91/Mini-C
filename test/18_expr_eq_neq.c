#include <stdio.h>
#include <stdbool.h>

int mod(int a, int b) {
  while(b < a + 1) {
    a = a - b;
  }
  return a;
}

void halfabet() {
  int c = 97;
  while (c != 123) {
    if (mod(c, 2) == 1) {
      putchar(c);
    } else {

    }
    c = c + 1;
  }
}

int main() {
  int a = 5;
  bool b1 = a != a;

  if (b1 == true) {
    putchar(0 + 48);
  } else {
    if (a == a) {
      putchar(1 + 48);
    } else {
      putchar(2 + 48);
    }
  }

  halfabet();
  putchar(10);
  return 0;
}
