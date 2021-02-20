#include <device.h>
#include <kernel.h>

#include <drivers/uart.h>

#include <string.h>
#include <c/compat.h>

#include <cr-process.h>

#include "init-common.h"

#define MAX_BOARD_NAME_LENGTH 32u

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
    uart0 = device_get_binding(DT_LABEL(DT_NODELABEL(uart0)));
    if (uart0 == NULL) {
        printk("Could not access uart0. Giving up.\n");
        return;
    }
    char board[MAX_BOARD_NAME_LENGTH];
    strlcpy(board, CONFIG_BOARD, sizeof(board));
    printk("ChipRemoteFirmware running on %s\n", board);

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
