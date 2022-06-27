#include <stdio.h>
#include <stdbool.h>

int i1 = 2, i2 = 5;
int espace;
int newline;

void print_2_digits(int i1, int i2) {
  putchar(i1);
  putchar(espace);
  putchar(i2);
  putchar(newline);
}

bool b = false;

int main() {
  int zero, neuf;
  zero = 48;
  neuf = zero + 9;

  espace = 32;
  newline = 10;
  
  for (int i = zero, j = neuf; i <= neuf && j >= zero; i = i + 1, j = j - 1) {
    print_2_digits(i, j); 
  }
  putchar(newline);

  for (i1 = zero, i2 = neuf; i1 <= neuf && i2 >= zero; i1 = i1 + 1, i2 = i2 - 1) {
    print_2_digits(i1, i2);
  }
  putchar(newline);

  for (; zero <= 57 && neuf >= 48; zero = zero + 1, neuf = neuf - 1) {
    print_2_digits(zero, neuf);
  }

  return 0;
}
