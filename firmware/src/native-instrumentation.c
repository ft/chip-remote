/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/kernel.h>

#include <zephyr/device.h>
#include <zephyr/drivers/uart.h>

#include <string.h>

#include <ufw/sx.h>

#include "native-instrumentation.h"
#include "resizable-buffer.h"

struct resizeable_buffer ni_buffer;
struct sx_node *rxring = NULL;

void
cr_uart_send(const struct device *tty, const char *str)
{
    const size_t len = strlen(str);
    for (size_t i = 0u; i < len; ++i) {
        uart_poll_out(tty, str[i]);
    }
}

void
cr_spi_text_load(struct sx_node *node)
{
    if (rxring == NULL) {
        rxring = node;
    } else {
        rxring = sx_append(rxring, node);
    }
}

void
ni_dispatch(struct sx_node *node)
{
    if (sx_is_list(node)) {
        if (sx_is_the_symbol(sx_car(node), "load-spi")) {
            struct sx_node *car = sx_pop(&node);
            sx_destroy(&car);
            if (sx_is_list(node))
                cr_spi_text_load(node);
            else
                sx_destroy(&node);
        }
    } else {
        sx_destroy(&node);
    }
}

void
ni_toplevel(struct resizeable_buffer *rb, const char ch,
            const struct device *uart)
{
    if (rb->index > rb->size) {
        rb_enlarge(rb);
    }

    if (ch == '\n') {
        rb->buffer[rb->index] = '\0';
        rb->index = 0u;
        struct sx_parse_result p = sx_parse_string(rb->buffer);
        if (p.status == SXS_SUCCESS) {
            printk("(instrumentation %s)\n", rb->buffer);
            ni_dispatch(p.node);
        } else {
            printk("(sx-error %d %zu)\n", p.status, p.position);
        }
        /* Acknowledge processing of instrumentation request. This ensures a
         * client can block until this happens to avoid race conditions. */
        cr_uart_send(uart, "ok\n");
    } else {
        rb->buffer[rb->index] = ch;
        rb->index++;
    }
}
