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

void merge(int* arr1, int* arr2, int* out, int size) {
    int* c_arr1 = malloc(4 * size);
    int* c_arr2 = malloc(4 * size);
    copy(arr1, c_arr1, size);
    copy(arr2, c_arr2, size);

    int it1 = 0, it2 = 0;
    for (int i = 0; i < 2 * size; i = i + 1) {
        if (it1 < size && (it2 >= size || c_arr1[it1] < c_arr2[it2])) {
            out[i] = c_arr1[it1];
            it1 = it1 + 1;
        } else {
            out[i] = c_arr2[it2];
            it2 = it2 + 1;
        }
    }
    free(c_arr1);
    free(c_arr2);
} 

void merge_sort_aux(int* arr1, int* arr2, int size) {
    if (size == 1) {
        merge(arr1, arr2, arr1, size);
    } else {
        int new_size = size / 2;
        merge_sort_aux(arr1, arr1 + new_size, new_size);
        merge_sort_aux(arr2, arr2 + new_size, new_size);
        merge(arr1, arr2, arr1, size);
    }
}

void merge_sort(int* arr, int size) {
    int new_size = size / 2;
    merge_sort_aux(arr, arr + new_size, new_size);
}

int main() {
    int tab[8] = { 6, 5, 3, 1, 8, 7, 2, 4 };
    puts("before sorting:");
    print_arr(tab, 8);
    merge_sort(tab, 8);
    puts("after sorting:");
    print_arr(tab, 8);
    return 0;
}
