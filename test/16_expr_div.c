#include <stdio.h>

int y = 44;

void print_int(int i) {
  int div = i / 10;
  int left = i + div * -10;
  if (div < 1) {
    
  } else {
    print_int(div);
  }
  putchar(left + 48);
}

int main() {
  int z = y / 4 * 2;
  print_int(z);
  putchar(10);
  print_int(1000);
  return 0;
}
