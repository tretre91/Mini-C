#include <stdio.h>

void print_long_aux(long i) {
  if (i < 10) {
    putchar('0' + i);
  } else {
    print_long_aux(i / 10);
    putchar('0' + i % 10);
  }
}

void print_long(long i) {
  if (i < 0) {
    putchar('-');
    print_long_aux(-i);
  } else {
    print_long_aux(i);
  }
}

long arr[3] = { 1, 2147483648, -234454543545566 };

int main() {
    long i = arr[0];
    long b = arr[i] - 30;
    i = arr[1];
    int a = i;
    print_long(arr[2]);
    putchar('\n');
    if (a == -2147483648) {
        putchar('y');
    } else {
        putchar('n');
    }
    putchar('\n');
    return 0;
}
