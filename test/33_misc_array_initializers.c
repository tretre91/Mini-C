#include <stdbool.h>

int a[5] = { 0, 1 };
int x = 18;

bool b(bool third) {
    bool tab[4] = { true, true, third, true };
    bool res = tab[0];
    for (int i = 1; i < 4; i = i + 1) {
        res = res && tab[i];
    }
    return res;
}

bool test_b() {
    return b(false);
}

int main() {
    int t[4] = { 0, 1, 2, 3, 4 + x, 5, 6 };
    return t[4];
    {
        int z[5] = { 25, 32, -12, 1, 42 };
        z[2] = 1;
        return z[0];
    }
    int x = 6;
    return 0;
}