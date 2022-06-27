#include <stdio.h>

int a[10];
int b[10];

void print_int(int i) {
  if (i < 10) {
    putchar('0' + i);
  } else {
    print_int(i / 10);
    putchar('0' + i % 10);
  }
}

void init() {
    for (int i = 0; i < 10; i = i + 1) {
        a[i] = i;
        b[i] = 2 * i;
    }
}

int prod_sum() {
    int c[10];
    for (int i = 0; i < 10; i = i + 1) {
        c[i] = a[i] * b[i];
    }
    int sum = 0;
    for (int i = 0; i < 10; i = i + 1) {
        sum = sum + c[i];
    }
    return sum;
}

int main() {
    init();
    print_int(prod_sum());
    putchar('\n');
    return prod_sum();
}
