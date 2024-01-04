/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_RESIZABLE_BUFFER_H_fb59542b
#define INC_RESIZABLE_BUFFER_H_fb59542b

#include <stddef.h>

#define RB_DEFAULT_INIT_SIZE 64u

struct resizeable_buffer {
    size_t index;
    size_t size;
    char *buffer;
};

void rb_enlarge(struct resizeable_buffer *rb);
void rb_init(struct resizeable_buffer *rb);

#endif /* INC_RESIZABLE_BUFFER_H_fb59542b */
