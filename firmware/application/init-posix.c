#include <zephyr/kernel.h>

#include <zephyr/drivers/uart.h>
#include <zephyr/drivers/console/posix_arch_console.h>

#include <sys/types.h>

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sx-parser.h>
#include <ufw/compat.h>

#include "init-common.h"

struct sx_node *rxring = NULL;

void
cr_spi_text_load(struct sx_node *node)
{
    if (rxring == NULL) {
        rxring = node;
    } else {
        rxring = sx_append(rxring, node);
    }
}

const struct device *uart1;
const struct device *uart0;

void
cr_uart_send(const struct device *tty, const char *str)
{
    const size_t len = strlen(str);
    for (size_t i = 0u; i < len; ++i) {
        uart_poll_out(tty, str[i]);
    }
}

#define RB_DEFAULT_INIT_SIZE 64u

struct resizeable_buffer {
    size_t index;
    size_t size;
    char *buffer;
};

struct resizeable_buffer ni_buffer;

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
ni_toplevel(struct resizeable_buffer *rb, const char ch)
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
        cr_uart_send(uart1, "ok\n");
    } else {
        rb->buffer[rb->index] = ch;
        rb->index++;
    }
}

void
main(void)
{
    uart0 = DEVICE_DT_GET(DT_NODELABEL(uart0));
    if (uart0 == NULL) {
        printk("Could not access uart0. Giving up.\n");
        return;
    }

    uart1 = DEVICE_DT_GET(DT_NODELABEL(uart1));
    if (uart1 == NULL) {
        printk("Could not access uart1. Giving up.\n");
        return;
    }

    struct resizeable_buffer nirb;
    rb_init(&nirb);

    printk("ChipRemoteFirmware running on %s\n", CONFIG_BOARD);
    printk("(activated!)\n");
    printk("(firmware-pid %u)\n", getpid());
    posix_flush_stdout();
    /* Disable stderr output */
    close(STDERR_FILENO);

    char ch0 = 0;
    char ch1 = 0;
    for (;;) {
        /* Poll controlling UART port and feed fifo */
        const int rc0 = uart_poll_in(uart0, &ch0);
        const int rc1 = uart_poll_in(uart1, &ch1);

        if (rc0 == 0) {
        }
        if (rc1 == 0) {
            ni_toplevel(&nirb, ch1);
        }

        if (rc1 != 0 && rc1 != 0) {
            k_usleep(1000);
        }
    }
}
