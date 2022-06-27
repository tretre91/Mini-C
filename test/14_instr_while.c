#include <stdbool.h>

bool not(bool b) {
  if (b) {
    return false;
  } else {
    return true;
  }
}

bool pair(int n) {
  int i = 0;
  bool pair = true;
  if (n < 0) {
    while (n < i) {
      i = i + -1;
      pair = not(pair);
    }
    return pair;
  } else {
    while (i < n) {
      i = i + 1;
      pair = not(pair);
    }
    return pair;
  }
}

int main() {
  int i = 1;
  while(true) {
    while(i < 10) {
      i = i * 2; 
    }
    return i;
  }
}
