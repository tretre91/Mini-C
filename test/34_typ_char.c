#include <stdio.h>
#include <stdbool.h>

char tab[6] = { 'h', 'e', 'l', 'l', 'o', '!' };

char foo() {
    int c = 'a' * tab[true];
    tab[0] = 1;
    return tab[2];
}

int main() {
    int k = 'A';
    char c = 'a';
    int a = 'a' + 'a';

    for (int i = 0; i < 6; i = i + 1) {
        putchar(tab[i]);
    }
    char newline = '\n';
    putchar(newline);
    return a;
}