#include <stdlib.h>
#include <stdio.h>

void print_int_aux(int i) {
    if (i < 10) {
        putchar('0' + i);
    } else {
        print_int_aux(i / 10);
        putchar('0' + i % 10);
    }
}

void print_int(int i) {
    if (i < 0) {
        putchar('-');
        print_int_aux(-i);
    } else {
        print_int_aux(i);
    }
}

void print_arr(int* arr, int size) {
    for (int i = 0; i < size; i = i + 1) {
        print_int(arr[i]);
        putchar(' ');
    }
    putchar('\n');
}

void copy(int* source, int* dest, int n) {
    for (int i = 0; i < n; i = i + 1) {
        dest[i] = source[i];
    }
}

int* merge(int* arr1, int* arr2, int size) {
    int* result = malloc(2 * size * 4);
    int it1 = 0, it2 = 0;
    for (int i = 0; i < 2 * size; i = i + 1) {
        if (it1 < size && (it2 >= size || arr1[it1] < arr2[it2])) {
            result[i] = arr1[it1];
            it1 = it1 + 1;
        } else {
            result[i] = arr2[it2];
            it2 = it2 + 1;
        }
    }
    free(arr1);
    free(arr2);
    return result;
} 

int* merge_sort_aux(int* arr1, int* arr2, int size) {
    if (size == 1) {
        return merge(arr1, arr2, size);
    } else {
        int new_size = size / 2;

        int* arr11 = malloc(new_size * 4);
        int* arr12 = malloc(new_size * 4);
        int* arr21 = malloc(new_size * 4);
        int* arr22 = malloc(new_size * 4);

        copy(arr1, arr11, new_size);
        copy(arr1 + new_size, arr12, new_size);
        copy(arr2, arr21, new_size);
        copy(arr2 + new_size, arr22, new_size);

        free(arr1);
        free(arr2);
        arr1 = merge_sort_aux(arr11, arr12, new_size);
        arr2 = merge_sort_aux(arr21, arr22, new_size);
        return merge(arr1, arr2, size);
    }
}

int* merge_sort(int* arr, int size) {
    int new_size = size / 2;
    int* arr1 = malloc(4 * new_size);
    int* arr2 = malloc(4 * new_size);
    copy(arr, arr1, new_size);
    copy(arr + new_size, arr2, new_size);
    int* res = merge_sort_aux(arr1, arr2, new_size);
    return res;
}

#define SIZE 131072

int main() {
    int tab[SIZE] =
#include "values131072.txt"
    ;
    int* res = merge_sort(tab, SIZE);
    if (SIZE < 1 << 10) {
        puts("before sorting:");
        print_arr(tab, SIZE);
        puts("after sorting:");
        print_arr(res, SIZE);
    }
    free(res);
    return 0;
}
