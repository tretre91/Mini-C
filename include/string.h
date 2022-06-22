#ifndef __STRING_H
#define __STRING_H

#include <stddef.h>

size_t strlen(const char* str) {
    size_t len = 0;
    while (str[len] != '\0') {
        len = len + 1;
    }
    return len;
}

int strcmp(const char* lhs, const char* rhs) {
    size_t i = 0;
    while (lhs[i] != '\0' && rhs[i] != '\0' && lhs[i] == rhs[i]) {
        i = i + 1;
    }
    return lhs[i] - rhs[i];
}

#endif // __STRING_H
