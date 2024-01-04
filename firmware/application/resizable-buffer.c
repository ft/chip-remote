/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/kernel.h>

#include <stddef.h>
#include <stdlib.h>

#include "resizable-buffer.h"

void
rb_enlarge(struct resizeable_buffer *rb)
{
    if (rb->size == 0) {
        rb->size = RB_DEFAULT_INIT_SIZE;
    } else {
        rb->size += rb->size / 2;
    }

    if (rb->buffer == NULL) {
        rb->buffer = malloc(rb->size);
    } else {
        rb->buffer = realloc(rb->buffer, rb->size);
    }

    if (rb->buffer == NULL) {
        printk("Instrumentation is out of memory!\n");
        exit(0);
    }
}

void
rb_init(struct resizeable_buffer *rb)
{
    rb->index = 0u;
    rb->size = 0u;
    rb->buffer = NULL;
    rb_enlarge(rb);
}
