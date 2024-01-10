/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <zephyr/kernel.h>

#include <zephyr/drivers/gpio.h>
#include <zephyr/drivers/uart.h>
#include <zephyr/usb/usb_device.h>

#include <stdlib.h>

#include <ufw/compiler.h>
#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>
#include <ufwz/endpoint-uart-poll.h>

#include "chip-remote.h"
#include "peripherals.h"
#include "registers.h"

int
main(void)
{
    const struct device *uart0 = DEVICE_DT_GET(DT_NODELABEL(cdc_acm_uart0));
    if (uart0 == NULL) {
        printk("Could not access usb. Giving up.\n");
        return EXIT_FAILURE;
    }

    if (usb_enable(NULL) != 0) {
        printk("Could not enable usb. Giving up.\n");
        return EXIT_FAILURE;
    }

    if (peripheral_check() < 0) {
        return EXIT_FAILURE;
    }

    RegP protocol;
    Source regpsource = UFWZ_UART_POLL_SOURCE(uart0);
    Sink regpsink = UFWZ_UART_POLL_SINK(uart0);

    if (chip_remote_init(&protocol, regpsource, regpsink, &registers) < 0) {
        return EXIT_FAILURE;
    }

    printk("ChipRemoteFirmware running on %s\n", CONFIG_BOARD);

    for (;;) {
        const int rc = chip_remote_process(&protocol);
        if (rc < 0) {
            k_usleep(1000);
        }
    }

    /* NOTREACHED */
    return EXIT_SUCCESS;
}
