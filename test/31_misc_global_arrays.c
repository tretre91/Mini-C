int a[10];
int b[10];
int c[10];

void square() {
    for (int i = 0; i < 10; i = i + 1) {
        a[i] = i;
        b[i] = i;
        c[i] = a[i] * b[i];
    }
}

int i = 5;

int index() {
    return i;
}

int main() {
    square();
    return c[index() + 3];
}
