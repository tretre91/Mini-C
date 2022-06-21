#ifndef __STDIO_H
#define __STDIO_H

[[import::std]] extern int putchar(int c);

int puts(const char* str) {
    for (int i = 0; str[i] != '\0'; i = i + 1) {
        putchar(str[i]);
    }
    return putchar('\n');
}

#endif // __STDIO_H
