#include <stdio.h>
#include <string.h>
#include <stdbool.h>

bool equal(const char* s1, const char* s2) {
    return strcmp(s1, s2) == 0;
}

int main() {
    const char* str = "Hello, world!";
    puts(str);
    if (equal(str, "Hello, worl!d")) {
        puts("equal!");
    } else {
        puts("not equal :(");
    }
    return 0;
}
