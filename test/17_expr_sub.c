#include <stdio.h>

int five = 5;

void reverse_alphabet() {
  int c = 122;
  while(96 < c) {
    putchar(c);
    c = c - 1;
  }
}

int main() {
  int x = 5 - five;
  int z = 4 - -2;
  int y = 4 - 2 + 1;
  reverse_alphabet();
  putchar(10);
  putchar(z + 48);
  putchar(y + 48);
  putchar(10);
  return x;
}
