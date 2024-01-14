/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#ifndef INC_NATIVE_INSTRUMENTATION_H_b3464bde
#define INC_NATIVE_INSTRUMENTATION_H_b3464bde

#include <zephyr/device.h>

#include <ufw/sx.h>

#include "resizable-buffer.h"

extern struct resizeable_buffer ni_buffer;
extern const struct device *uart1;

void ni_dispatch(struct sx_node *node);
void ni_toplevel(struct resizeable_buffer *rb, char ch,
                 const struct device *uart);

#endif /* INC_NATIVE_INSTRUMENTATION_H_b3464bde */
