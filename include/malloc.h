#define NULL 0

#define size_t long
#define BYTE char
#define BLOCK BYTE*
#define HEADER size_t

[[internal]] extern void* __sbrk();
[[internal]] extern void* __heap_end();

BLOCK __first_block = NULL;

BLOCK __get_previous_block(BLOCK b) {
    if (b != NULL) {
        BLOCK* ptr = b;
        return ptr[0];
    } else {
        return NULL;
    }
}

void __set_previous_block(BLOCK b, BLOCK prev) {
    if (b != NULL) {
        BLOCK* ptr = b;
        ptr[0] = prev;
    }
}

BLOCK __get_next_block(BLOCK b) {
    if (b != NULL) {
        BLOCK* ptr = b;
        return ptr[1];
    } else {
        return NULL;
    }
}

void __set_next_block(BLOCK b, BLOCK next) {
    if (b != NULL) {
        BLOCK* ptr = b;
        ptr[1] = next;
    }
}

BLOCK __make_block(BYTE* address, size_t size) {
    size_t real_size = size - 16;
    size_t* header = address;
    *header = real_size;
    return header + 1;
}

size_t __block_size(BLOCK b) {
    HEADER* ptr = b;
    return ptr[-1] & ~0x7;
}

bool __block_is_free(BLOCK b) {
    HEADER* ptr = b;
    return ptr[-1] & 0x1;
}

void __set_free(BLOCK b) {
    HEADER* ptr = b;
    ptr[-1] = ptr[-1] | 0x1;
}

BLOCK __extend_heap(size_t size) {
    BYTE* beginning = __heap_end();
    BYTE* end = beginning;
    while (end - beginning < size + 16) {
        end = __sbrk();
    }
    return __make_block(beginning, end - beginning);
}

void __add_free_block(BLOCK b) {
    if (__first_block == NULL) {
        __first_block = b;
        __set_previous_block(b, NULL);
        __set_next_block(b, NULL);
    } else {
        __set_previous_block(b, __get_previous_block(__first_block));
        __set_next_block(b, __first_block);
        __first_block = b;
    }
}

void __allocate_block(BLOCK b) {
    BLOCK prev = __get_previous_block(b);
    BLOCK next = __get_next_block(b);
    __set_next_block(prev, next);
    __set_previous_block(next, prev);
    HEADER* ptr = b;
    ptr[-1] = ptr[-1] & ~0x1;
    // TODO : gerer le cas first block
    if (__first_block == b) {
        __first_block = NULL;
    }
}

BLOCK __find_block(size_t size) {
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

void* malloc(size_t size) {
    // TODO : diviser les blocs trop grands
    return __find_block(size);
}

void free(void* ptr) {
    BLOCK b = ptr;
    if (b != NULL && !__block_is_free(b)) {
        __set_free(b);
        __add_free_block(b);
    }
}
