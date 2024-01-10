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

#include <zephyr/drivers/console/posix_arch_console.h>
#include <zephyr/drivers/uart.h>

#include <unistd.h>

#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>
#include <ufwz/endpoint-uart-poll.h>

#include "chip-remote.h"
#include "native-instrumentation.h"
#include "peripherals.h"
#include "registers.h"

void
main(void)
{
    const struct device *uart0 = DEVICE_DT_GET(DT_NODELABEL(uart0));
    if (uart0 == NULL) {
        printk("Could not access uart0. Giving up.\n");
        return;
    }

    const struct device *uart1 = DEVICE_DT_GET(DT_NODELABEL(uart1));
    if (uart1 == NULL) {
        printk("Could not access uart1. Giving up.\n");
        return;
    }

    if (peripheral_check() < 0) {
        return;
    }

    RegP protocol;
    Source regpsource = UFWZ_UART_POLL_SOURCE(uart0);
    Sink regpsink = UFWZ_UART_POLL_SINK(uart0);

    if (chip_remote_init(&protocol, regpsource, regpsink, &registers) < 0) {
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

    char ch1 = 0;
    for (;;) {
        const int rc0 = chip_remote_process(&protocol);
        const int rc1 = uart_poll_in(uart1, &ch1);

        if (rc1 == 0) {
            ni_toplevel(&nirb, ch1, uart1);
        }
        if (rc1 != 0 && rc0 < 0) {
            k_usleep(1000);
        }
    }
}
