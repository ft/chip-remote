/*
 * Copyright (c) 2024 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file main.c
 * @brief Main entry-point for the chip-remote firmware
 *
 * This file contains the initialisation code and the default Zephyr thread,
 * running the chip-remote control protocol.
 *
 * The devicetree and kconfig systems decide the behaviour of the code in this
 * file.
 *
 * In kconfig, the CR_INTERFACE_TYPE choice configures the type of interface
 * the code should use for the control protocol interface. Every board port of
 * the firmware has to make this choice. If this is not done, the code will
 * cause the toolchain to error out.
 *
 * In devicetree, the code uses the "chipremote" namespace in the "chosen"
 * subtree. The following nodes are used:
 *
 *   - chipremote,proto,serial:
 *        Selects the device to use for control protocol communication,
 *        if CR_INTERFACE_TYPE is one of the serial device types.
 *
 *  - chipremote,spi[0-9]+:
 *       Assigns a SPI port for chip-remote to use.
 *
 *  - chipremote,i2c[0-9]+:
 *       Assigns a I2C port for chip-remote to use.
 *
 *  - chipremote,heartbeat:
 *       Selects a GPIO to use as its heartbeat LED.
 *
 * When a TCP port is used for the protocol, the firmware ignores any peri-
 * pheral selections done in the chosen node. In fact, these peripherals are
 * free and can be used for other purposes.
 */

#include <zephyr/kernel.h>

#ifdef CONFIG_BOARD_NATIVE_SIM
#include <zephyr/drivers/console/posix_arch_console.h>
#endif /* CONFIG_BOARD_NATIVE_SIM */

#include <zephyr/devicetree.h>
#include <zephyr/drivers/gpio.h>
#include <zephyr/drivers/uart.h>
#include <zephyr/usb/usb_device.h>

#include <stdlib.h>

#ifdef CONFIG_BOARD_NATIVE_SIM
#include <unistd.h>
#endif /* CONFIG_BOARD_NATIVE_SIM */

#include <ufw/byte-buffer.h>
#include <ufw/compiler.h>
#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>
#include <ufwz/endpoint-uart-poll.h>
#include <ufwz/endpoint-uart-fifo.h>
#include <ufwz/shell-addons.h>

#include "chip-remote.h"
#include "peripherals.h"
#include "registers.h"

#include "server.h"

#ifdef CONFIG_CR_INTERFACE_NONE
#error Specify the chip-remote interface type in zephyr config of board!
#endif

#define CR_PROTO_NODE DT_CHOSEN(chipremote_proto_serial)
#define CR_PROTO_IFC  DEVICE_DT_GET(CR_PROTO_NODE)

#ifdef CONFIG_CR_INTERFACE_SMART_SERIAL
#if DT_NODE_HAS_COMPAT(CR_PROTO_NODE, zephyr_cdc_acm_uart) == 1
#define CR_DO_ENABLE_USB 1
#endif /* chipremote_proto_ifc is type zephyr_cdc_acm_uart */
#endif /* CONFIG_CR_INTERFACE_SMART_SERIAL */

#ifdef CONFIG_CR_INTERFACE_SIMPLE_SERIAL
struct uart_config uart_cfg = {
    .baudrate  = CONFIG_CR_PROTOCOL_SERIAL_BAUDRATE,
    .parity    = UART_CFG_PARITY_NONE,
    .stop_bits = UART_CFG_STOP_BITS_1,
    .flow_ctrl = UART_CFG_FLOW_CTRL_NONE,
    .data_bits = UART_CFG_DATA_BITS_8
};
#endif /* CONFIG_CR_INTERFACE_SIMPLE_SERIAL */

int
main(void)
{
    if (peripheral_check() < 0) {
        printk("System perpherals could not be initialised.\n");
        return EXIT_FAILURE;
    }
    printk("System perpherals online.\n");

#ifdef CONFIG_CR_WITH_SERIAL
    const struct device *pifc = CR_PROTO_IFC;
    if (pifc == NULL || device_is_ready(pifc) == false) {
        printk("Could not access protocol interface. Giving up.\n");
        return EXIT_FAILURE;
    }
    printk("Control protocol interface online.\n");
#endif /* CONFIG_CR_WITH_SERIAL */

#ifdef CR_DO_ENABLE_USB
    if (usb_enable(NULL) != 0) {
        printk("Could not enable usb. Giving up.\n");
        return EXIT_FAILURE;
    }
    printk("USB interface online.\n");
#endif /* CR_DO_ENABLE_USB */

#ifdef CONFIG_CR_INTERFACE_SIMPLE_SERIAL
    if (uart_configure(pifc, &uart_cfg) < 0) {
        printk("Could not configure %s. Giving up.\n", pifc->name);
        return EXIT_FAILURE;
    }
    printk("Serial device configured: %s\n", pifc->name);

    DEFINE_UART_FIFO_SOURCE_DATA(pifcdata, 128u);

    if (ufwz_uart_fifo_source_init(pifc, &pifcdata) < 0) {
        printk("Could not setup %s fifo. Giving up.\n", pifc->name);
        return EXIT_FAILURE;
    }

    Source regpsource = UFWZ_UART_FIFO_SOURCE(&pifcdata);
    Sink regpsink = UFWZ_UART_POLL_SINK(pifc);
#endif /* CONFIG_CR_INTERFACE_SIMPLE_SERIAL */

#ifdef CONFIG_CR_INTERFACE_SMART_SERIAL
    Source regpsource = UFWZ_UART_POLL_SOURCE(pifc);
    Sink regpsink = UFWZ_UART_POLL_SINK(pifc);
#endif /* CONFIG_CR_INTERFACE_SMART_SERIAL */

#ifdef CONFIG_CR_WITH_SERIAL
    RegP protocol;

    if (chip_remote_init(&protocol, regpsource, regpsink, &registers) < 0) {
        printk("Could not setup chip-remote processor. Giving up.\n");
        return EXIT_FAILURE;
    }
#endif /* CONFIG_CR_WITH_SERIAL */

    printk("(activated!)\n");
    printk("(board %s)\n", CONFIG_BOARD);
#ifdef CONFIG_BOARD_NATIVE_SIM
    printk("(firmware-pid %u)\n", getpid());
#endif /* CONFIG_BOARD_NATIVE_SIM */

#ifdef CONFIG_CR_WITH_SERIAL
    for (;;) {
        const int rc = chip_remote_process(&protocol);
        if (rc < 0) {
            k_usleep(1000);
        }
    }
#endif /* CONFIG_CR_WITH_SERIAL */

#ifdef CONFIG_CR_INTERFACE_TCPIP
    ufw_shell_reg_init(&registers);

    struct cr_tcp_server srv;
    {
        const int rc = chip_remote_tcp_boot(&srv);
        if (rc < 0) {
            return EXIT_FAILURE;
        }
    }
#endif /* CONFIG_CR_INTERFACE_TCPIP */

#ifdef CONFIG_BOARD_NATIVE_SIM
    posix_flush_stdout();
    close(STDERR_FILENO);
#endif /* CONFIG_BOARD_NATIVE_SIM */

#ifdef CONFIG_CR_INTERFACE_TCPIP
    printk("(cr-server-port %d)\n", srv.port);
    crs_run(&srv);
#endif /* CONFIG_CR_INTERFACE_TCPIP */

    return EXIT_SUCCESS;
}
