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
#include <ufwz/endpoint-uart-fifo.h>

#include "chip-remote.h"
#include "peripherals.h"
#include "registers.h"

struct uart_config uart_cfg = {
    .baudrate  = 921600u,
    .parity    = UART_CFG_PARITY_NONE,
    .stop_bits = UART_CFG_STOP_BITS_1,
    .flow_ctrl = UART_CFG_FLOW_CTRL_NONE,
    .data_bits = UART_CFG_DATA_BITS_8
};

int
main(void)
{
    const struct device *uart0 = DEVICE_DT_GET(DT_NODELABEL(usart2));
    if (uart0 == NULL || device_is_ready(uart0) == false) {
        printk("Could not access uart-2. Giving up.\n");
        return EXIT_FAILURE;
    }

    if (peripheral_check() < 0) {
        return EXIT_FAILURE;
    }

    if (uart_configure(uart0, &uart_cfg) < 0) {
        printk("Could not configure uart-2. Giving up.\n");
        return EXIT_FAILURE;
    }

    DEFINE_UART_FIFO_SOURCE_DATA(uart0data, 128u);

    if (ufwz_uart_fifo_source_init(uart0, &uart0data) < 0) {
        printk("Could not setup uart-2 fifo. Giving up.\n");
        return EXIT_FAILURE;
    }

    Source regpsource = UFWZ_UART_FIFO_SOURCE(&uart0data);
    Sink regpsink = UFWZ_UART_POLL_SINK(uart0);
    RegP protocol;

    if (chip_remote_init(&protocol, regpsource, regpsink, &registers) < 0) {
        printk("Could not setup chip-remote processor. Giving up.\n");
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
