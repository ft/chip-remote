/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/kernel.h>

#include <zephyr/device.h>
#include <zephyr/drivers/uart.h>

#include <string.h>

#include <ufw/compiler.h>
#include <ufw/sx.h>
#include <ufwz/spi-text.h>

#include "server.h"

struct sx_node *rxring = NULL;

static ssize_t
ni_send(struct cr_tcp_client *client, const void *buf, const size_t n)
{
    if (n > SSIZE_MAX) {
        return -EINVAL;
    }

    const unsigned char *src = buf;
    size_t rest = n;

    while (rest > 0) {
        const size_t offset = n - rest;
        const ssize_t done = zsock_send(client->socket, src + offset, rest, 0);
        if (done < 0) {
            switch (errno) {
            case EAGAIN:
            case EINTR:
                continue;
            default:
                return done;
            }
        }
        rest -= done;
    }

    return n;
}

static int
ni_dispatch_spi(const size_t idx, struct sx_node *node)
{
    return 0;
}

static int
ni_dispatch(struct cr_tcp_client *client, struct sx_node *node)
{
    printk("Processing expression from client %d: %d...\n",
           client->socket, node->type);

    if (sx_is_list(node) == false) {
        goto done;
    }

    if (sx_is_the_symbol(sx_car(node), "load-spi")) {
        struct sx_node *car = sx_pop(&node);
        sx_destroy(&car);
        if (sx_is_the_symbol(sx_car(node), "spi-0")) {
            car = sx_pop(&node);
            sx_destroy(&car);
            ni_dispatch_spi(0, node);
            goto done;
        }
    }

done:
    sx_destroy(&node);
    return 0;
}

static int
ni_process(struct cr_tcp_server *srv,
           struct cr_tcp_client *client)
{
   /* Why's this loop here? Because the RX buffer may contain more than one
     * s-expression and we want to process all of them. */
    for (;;) {
        const void *buf = byte_buffer_readptr(&client->rx);
        if (buf == NULL) {
            return -ENOMEM;
        }

        const size_t rest = byte_buffer_rest(&client->rx);
        struct sx_parse_result pr = sx_parse_stringn(buf, rest);

        if (pr.status == SXS_SUCCESS) {
            const bool withnode = pr.node != NULL;
            if (withnode) {
                const int drc = ni_dispatch(client, pr.node);
                if (drc < 0) {
                    return drc;
                }
                ni_send(client, "ok\n", 3);
            }
            const int mrc = byte_buffer_markread(&client->rx, pr.position);
            byte_buffer_rewind(&client->rx);
            if (mrc < 0 || byte_buffer_rest(&client->rx) == 0u) {
                printk("Done.\n");
                return 0;
            }
        } else if (pr.status == SXS_UNEXPECTED_END) {
            printk("Not quite all data.\n");
            return 0;
        } else {
            printk("instrumentation: Invalid input (%d)\n", pr.status);
            return -EINVAL;
        }
    }
}

#define CR_NATIVE_INSTRUMENTATION_TCP_PORT 12345u

void
ni_thread_cb(UNUSED void *a, UNUSED void *b, UNUSED void *c)
{
    struct cr_tcp_server srv;
    crs_init(&srv, CR_NATIVE_INSTRUMENTATION_TCP_PORT, ni_process, NULL);
    const int rc = crs_setup(&srv);
    if (rc < 0) {
        printk("Setting up TCP server failed: %s (%d)\n",
               strerror(-rc), -rc);
        return;
    }
    printk("(ni-server-port %d)\n", CR_NATIVE_INSTRUMENTATION_TCP_PORT);
    for (;;) {
        crs_run(&srv);
    }
}

#define CR_NATIVE_INSTRUMENTATION_STACK_SIZE 4096u
#define CR_NATIVE_INSTRUMENTATION_PRIORITY      4u

K_THREAD_DEFINE(ni_thread,
                CR_NATIVE_INSTRUMENTATION_STACK_SIZE,
                ni_thread_cb,
                NULL, NULL, NULL,
                CR_NATIVE_INSTRUMENTATION_PRIORITY,
                0, 0);
