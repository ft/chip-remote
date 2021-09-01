#include <device.h>
#include <kernel.h>

#include <drivers/uart.h>
#include <drivers/console/native_posix_console.h>

#include <sys/types.h>
#include <unistd.h>

#include <string.h>
#include <c/compat.h>

#include <cr-process.h>

#include "init-common.h"
#include "ifc/text/spi.h"

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
uart_sink(const char *str)
{
    const size_t len = strlen(str);
    for (size_t i = 0u; i < len; ++i) {
        uart_poll_out(uart0, str[i]);
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
    char board[MAX_BOARD_NAME_LENGTH];
    strlcpy(board, CONFIG_BOARD, sizeof(board));
    printk("ChipRemoteFirmware running on %s\n", board);
    printk("(activated!)\n");
    printk("(firmware-pid %u)\n", getpid());
    posix_flush_stdout();
    /* Disable stderr output */
    close(STDERR_FILENO);

    char ch = 0;
    for (;;) {
        /* Poll controlling UART port and feed fifo */
        const int rc = uart_poll_in(uart0, &ch);
        if (rc == 0) {
            cr_toplevel(&proto, ch);
        } else {
            k_usleep(1000);
        }
    }
}
