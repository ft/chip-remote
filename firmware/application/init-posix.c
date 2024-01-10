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

#include <zephyr/drivers/spi.h>
#include <zephyr/drivers/uart.h>
#include <zephyr/drivers/console/posix_arch_console.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <ufw/binary-format.h>
#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>
#include <ufw/register-table.h>

#include <sx-parser.h>

#include "chip-remote.h"
#include "native-instrumentation.h"
#include "peripherals.h"
#include "registers.h"

const struct device *uart0;

static int
uart_octet_source(void *driver, void *value)
{
    const int rc = uart_poll_in(driver, value);
    return rc < 0 ? -EAGAIN : 1;
}

static int
uart_octet_sink(void *driver, unsigned char value)
{
    uart_poll_out(driver, value);
    return 1;
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

    if (peripheral_check() < 0) {
        return;
    }

    RegP protocol;
    Source regpsource = OCTET_SOURCE_INIT(uart_octet_source, (void*)uart0);
    Sink   regpsink   = OCTET_SINK_INIT(  uart_octet_sink,   (void*)uart0);

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
            ni_toplevel(&nirb, ch1);
        }
        if (rc1 != 0 && rc0 < 0) {
            k_usleep(1000);
        }
    }
}
