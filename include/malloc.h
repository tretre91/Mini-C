#ifndef __MALLOC_H
#define __MALLOC_H

#include <stddef.h>

#define BYTE char
#define BLOCK BYTE*
#define HEADER size_t

void* __heap_start;
void* __get_heap_start() { return __heap_start; } // TODO
void free(void* ptr);

extern void* __sbrk();
extern void* __heap_end();
extern void __malloc_h_init(int heap_start);

BLOCK __first_block = NULL;

#ifdef __MINIC_GC

// attribute used by the compiler to exclude a function from being gc'ed
#define __NO_GC [[gc_exclude]]

__NO_GC size_t __header_size(HEADER h) {
    return h & 0xFFFFFFFFF8;
}

__NO_GC size_t __header_ref_count(HEADER h) {
    return h >> 40;
}

__NO_GC HEADER __set_header_size(HEADER h, size_t size) {
    return (h & 0xFFFFFF0000000008) | size; 
}

__NO_GC HEADER __set_header_ref_count(HEADER h, size_t ref_count) {
    return (h & 0xFFFFFFFFFF) | (ref_count << 40);
}

__NO_GC HEADER __make_header(size_t ref_count, size_t size, int is_free) {
    return is_free | size | (ref_count << 40);
}

__NO_GC size_t __block_ref_count(BLOCK b) {
    HEADER* ptr = b;
    return __header_ref_count(ptr[-1]);
}

__NO_GC void __set_block_ref_count(BLOCK b, size_t ref_count) {
    HEADER* ptr = b;
    ptr[-1] = __set_header_ref_count(ptr[-1], ref_count);
}

__NO_GC void __increment_ref(void* ptr) {
    if (ptr > __heap_start) {
        BLOCK b = ptr;
        __set_block_ref_count(b, __block_ref_count(b) + 1);
    }
}

__NO_GC void __decrement_ref(void* ptr) {
    if (ptr > __heap_start) {
        BLOCK b = ptr;
        __set_block_ref_count(b, __block_ref_count(b) - 1);
        if (__block_ref_count(b) <= 0) {
            free(b);
        }
    }
}

#else

#define __NO_GC

__NO_GC size_t __header_size(HEADER h) {
    return h & ~0b111;
}

__NO_GC HEADER __set_header_size(HEADER h, size_t size) {
    return (h & 0b111) | size; 
}

__NO_GC HEADER __make_header(size_t _unused, size_t size, int is_free) {
    return size | (is_free & 0b111);
}

#endif // __MINIC_GC

__NO_GC HEADER __get_header(BLOCK b) {
    HEADER* ptr = b;
    return ptr[-1];
}

__NO_GC void __set_header(BLOCK b, HEADER h) {
    HEADER* ptr = b;
    ptr[-1] = h;
}

__NO_GC int __header_status(HEADER h) {
    return h & 0x1;
}

__NO_GC HEADER __set_header_free(HEADER h, int is_free) {
    return (h & ~0b111) | is_free;
}

/***************************************/
/*                                     */
/*         Functions on blocks         */
/*                                     */
/***************************************/

__NO_GC size_t __block_size(BLOCK b) {
    HEADER* ptr = b;
    return __header_size(ptr[-1]);
}

__NO_GC void __set_block_size(BLOCK b, size_t size) {
    HEADER* ptr = b;
    ptr[-1] = __set_header_size(ptr[-1], size);
}

__NO_GC _Bool __block_is_free(BLOCK b) {
    HEADER* ptr = b;
    return ptr[-1] & 0x1;
}

__NO_GC void __set_block_free(BLOCK b) {
    HEADER* ptr = b;
    ptr[-1] = ptr[-1] | 0x1;
}

__NO_GC BLOCK __get_previous_block(BLOCK b) {
    if (b != NULL) {
        BLOCK* ptr = b;
        return ptr[0];
    } else {
        return NULL;
    }
}

__NO_GC void __set_previous_block(BLOCK b, BLOCK prev) {
    if (b != NULL) {
        BLOCK* ptr = b;
        ptr[0] = prev;
    }
}

__NO_GC BLOCK __get_next_block(BLOCK b) {
    if (b != NULL) {
        BLOCK* ptr = b;
        return ptr[1];
    } else {
        return NULL;
    }
}

__NO_GC void __set_next_block(BLOCK b, BLOCK next) {
    if (b != NULL) {
        BLOCK* ptr = b;
        ptr[1] = next;
    }
}

__NO_GC BLOCK __make_block(BYTE* address, size_t size) {
    size_t real_size = size - 8;
    HEADER* header = address;
    *header = __make_header(0, real_size, 0);
    return header + 1;
}

__NO_GC BLOCK __extend_heap(size_t size) {
    BYTE* beginning = __heap_end();
    BYTE* end = beginning;
    while (end - beginning < size + 8) {
        __sbrk();
        end = __heap_end();
    }
    return __make_block(beginning, end - beginning);
}

__NO_GC void __add_free_block(BLOCK b) {
    if (__first_block == NULL) {
        __first_block = b;
        __set_previous_block(b, NULL);
        __set_next_block(b, NULL);
    } else {
        __set_previous_block(b, __get_previous_block(__first_block));
        __set_next_block(b, __first_block);
        __set_previous_block(__first_block, b);
        __first_block = b;
    }
}

__NO_GC void __allocate_block(BLOCK b) {
    BLOCK prev = __get_previous_block(b);
    BLOCK next = __get_next_block(b);
    __set_next_block(prev, next);
    __set_previous_block(next, prev);
    HEADER* ptr = b;
    ptr[-1] = ptr[-1] & ~0x1;
    if (b == __first_block) {
        __first_block = next;
    }
}

__NO_GC BLOCK __find_block(size_t size) {
    BLOCK b = __first_block;
    while (b != NULL && __block_size(b) < size) {
        b = __get_next_block(b);
    }

    if (b == NULL) {
        b = __extend_heap(size);
    }

    __allocate_block(b);
    return b;
}

__NO_GC BLOCK __divide(BLOCK b, size_t allocated_size) {
    size_t size = __block_size(b);
    if (allocated_size < size / 2 - 8) {
        BLOCK next = __make_block(b + allocated_size, size - allocated_size);
        __set_block_free(next);
        __add_free_block(next);
        return __make_block(b - 8, allocated_size + 8);
    }
    return b;
}

__NO_GC size_t __max(size_t a, size_t b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}

__NO_GC size_t __alloc_size(size_t size) {
    size = __max(size, 8);
    size_t aligned_size = 0;
    while (aligned_size < size) {
        aligned_size = aligned_size + 8;
    }
    return aligned_size;
}

__NO_GC void* malloc(size_t size) {
    size = __alloc_size(size);
    BLOCK b = __find_block(size);
    if (b != NULL) {
        b = __divide(b, size);
    }
    return b;
}

__NO_GC void free(void* ptr) {
    BLOCK b = ptr;
    if (b != NULL && !__block_is_free(b)) {
#ifdef __MINIC_GC
        __set_block_ref_count(b, 0);
#endif
        __set_block_free(b);
        __add_free_block(b);
    }
}

#endif // __MALLOC_H
