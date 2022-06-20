#include <stdio.h>
#include <stdlib.h>

int tab[5] = { 0xFF45, 5, 2, 3, 1 };

int int_to_string_aux(int n, char* buffer) {
  if (n < 10) {
    buffer[0] = '0' + n;
    return 1;
  } else {
    int pos = int_to_string_aux(n / 10, buffer);
    buffer[pos] = '0' + (n % 10);
    return pos + 1;
  }
}

char* int_to_string(int i) {
  char* buffer = malloc(12);
  int end;
  if (i < 0) {
    buffer[0] = '-';
    end = int_to_string_aux(-i, buffer + 1);
    buffer[end + 1] = '\0';
  } else {
    end = int_to_string_aux(i, buffer);
    buffer[end] = '\0';
  }
  return buffer;
}

int main() {
  int* ptr = NULL;
  int a = -492;
  char* s = int_to_string(a);
  puts(s);
  free(s);
  return 0;
}
