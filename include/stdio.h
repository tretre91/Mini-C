[[std]] extern int putchar(int c);

int puts(char* str) {
    for (int i = 0; str[i] != '\0'; i = i + 1) {
        putchar(str[i]);
    }
    return putchar('\n');
}
