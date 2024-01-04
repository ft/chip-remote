/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file init-posix.c
 * @brief Native POSIX firmware build initialisation
 *
 * The purpose of this firmware build is mostly testing without actual
 * hardware. The register-protocol works via a UART interface, that an external
 * process can communicate with.
 *
 * This firmware can be instrumented using an s-expression based protocol via a
 * second UART interface. The process will print a number of execution details
 * to its stdout output, for automation runners to read.
 */

#include <zephyr/kernel.h>

#include <zephyr/drivers/uart.h>
#include <zephyr/drivers/console/posix_arch_console.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <sx-parser.h>
#include <ufw/endpoints.h>

#include "native-instrumentation.h"

Source regpsource;
Sink regpsink;

const struct device *uart0;

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
