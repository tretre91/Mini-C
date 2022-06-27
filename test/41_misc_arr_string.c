#include <stdio.h>
#include <string.h>

int main() {
    const char* orig = "Bonjour, monde!";
    char tab[] = "Bonjour, monde!";
    puts(tab);
    int s = strcmp(tab, "Hello");
    if (s < 0) {
        puts("<");
    } else if (s == 0) {
        puts("==");
    } else {
        puts(">");
    }
    puts("Hello, world!");
    tab[0] = 'b';
    puts(tab);
    puts(orig);
    return 0;
}
