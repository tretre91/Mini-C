#include <stdio.h>

// affiche un entier de 0 à 9
void put_digit(int i) {
  putchar(48 + i);
}

/* affiche un entier positif sous forme binaire */
void print_binary(int i) {
  if (i < 2) {
    put_digit(i);
  } else {
    print_binary(i >> 1);
    put_digit(i & 1);
  }
}

int main() {
  print_binary(1 << 8);
  putchar(10);
  print_binary(224865);
  putchar(10);
  return 0;
}
