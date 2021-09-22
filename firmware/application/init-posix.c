#include <device.h>
#include <kernel.h>

#include <drivers/uart.h>
#include <drivers/console/native_posix_console.h>

#include <sys/types.h>
#include <unistd.h>

#include <stdlib.h>
#include <string.h>
#include <c/compat.h>

#include <cr-process.h>
#include <sx-parser.h>

#include "init-common.h"
#include "ifc/text/spi.h"
#include "sys/time_units.h"

const struct device *uart1;

#define MAX_BOARD_NAME_LENGTH 32u

uint32_t port00_spi_state = 0u;

struct cr_port port00_spi = {
    .name = "port00-spi",
    .type = CR_PORT_TYPE_SPI,
    .api  = &cr_port_impl_spi_text,
    .data = &port00_spi_state,
    .cfg.spi = {
        .frame_length = 16u,
        .bit_order = CR_BIT_MSB_FIRST,
        .cs = {
            .number = 1u,
            .polarity = CR_LOGIC_INVERTED
        },
        .clk = {
            .rate = 0u,
            .edge = CR_EDGE_RISING,
            .phase_delay = false
        }
    },
    .lines = 0u,
    .line = 0u,
    .initialised = false
};

void
cr_uart_send(const struct device *tty, const char *str)
{
    const size_t len = strlen(str);
    for (size_t i = 0u; i < len; ++i) {
        uart_poll_out(tty, str[i]);
    }
}

void
uart_sink(const char *str)
{
    cr_uart_send(uart0, str);
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
    port00_spi.api->init(&port00_spi);

    uart0 = device_get_binding(DT_LABEL(DT_NODELABEL(uart0)));
    if (uart0 == NULL) {
        printk("Could not access uart0. Giving up.\n");
        return;
    }

    uart1 = device_get_binding(DT_LABEL(DT_NODELABEL(uart1)));
    if (uart1 == NULL) {
        printk("Could not access uart1. Giving up.\n");
        return;
    }

    struct resizeable_buffer nirb;
    rb_init(&nirb);

    char board[MAX_BOARD_NAME_LENGTH];
    strlcpy(board, CONFIG_BOARD, sizeof(board));
    printk("ChipRemoteFirmware running on %s\n", board);
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
            cr_toplevel(&proto, ch0);
        }
        if (rc1 == 0) {
            ni_toplevel(&nirb, ch1);
        }

        if (rc1 != 0 && rc1 != 0) {
            k_usleep(1000);
        }
    }
}
