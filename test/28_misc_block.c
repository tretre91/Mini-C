#include <stdio.h>

int foo(int i) {
  return i * 2;
}

int main() {
  int i = 0;
  while (i < 10) {
    int c = 48 + i;
    i = i + 1;
    putchar(c);
  }
  {
    putchar(10);
  }
  return foo(i) - 20;
}
