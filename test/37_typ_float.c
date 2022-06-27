#include <stdio.h>

float arr[4] = { .5, 3.2f, 1., 34 };

int main() {
    int i = arr[2] + 1.75f;
    arr[0] = i;
    float f = (arr[1] * 3) / 3.4; 
    if (i == 2) {
        putchar('y');
    } else {
        putchar('n');
    }
    putchar('\n');
    return 0;
}
