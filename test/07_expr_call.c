int PARAM = 5;

int add(int a, int b) {
  return a + b;
}

int double_(int x) {
  return add(x, x);
}

int main() {
  return add(double_(PARAM), double_(-5));
}
