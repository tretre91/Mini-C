#include <stdio.h>

void print_int_aux(int i) {
  int div = i / 10;
  int left = i - div * 10;
  if (div >= 1) {
    print_int_aux(div);
  } else {}
  putchar(left + 48);
}

void print_int(int i) {
  if (i < 0) {
    putchar(45);
    print_int_aux(-i);
  } else {
    print_int_aux(i);
  }
}

int main() {
  int i = 4 + -(-2);
  print_int(i);
  putchar(10);
  print_int(4 - 2);
  putchar(10);
  print_int(- 4 + 2);
  putchar(10);
  print_int(4 - +-(-5));
  putchar(10);
  return -(i - 5) + 1;
}
