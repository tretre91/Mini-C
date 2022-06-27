void f(int i) {
  i = 5;
}

void foo() {}

int main() {
  f(5);
  foo();
  return 0;
}
