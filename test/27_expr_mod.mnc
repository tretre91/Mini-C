#include <stdio.h>

void print_int_aux(int i) {
  if (i < 10) {
    putchar(48 + i);
  } else {
    print_int_aux(i / 10);
    putchar(48 + i % 10);
  }
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
  int i = 0;
  while (i < 20) {
    if (i % 3 != 0) {
      print_int(i);
    } else {
      putchar(95);
    }
    putchar(32);
    i = i + 1;
  }
  putchar(10);
  return 0;
}
