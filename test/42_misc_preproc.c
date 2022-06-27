const char* hello = "hello\
, wor\
ld!";

#include <stdio.h>

in\
t foo = 4; // line comment

#define A
#ifdef A // if a is defined

int k = 6;

#endif // A
#undef A

int/* block comment */main() {
    puts(hello);
    return k + foo - 10;
}
