#include <stdio.h>

int linefeed() {
  return 10;
}

void alphabet() {
  int c = 97;
  while (c < 123) {
    putchar(c);
    c = c + 1;
  }
  putchar(linefeed());
}

int main() {
  alphabet();
  return 0;
}
