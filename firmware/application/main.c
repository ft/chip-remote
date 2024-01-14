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
 *   - chipremote,proto-ifc: Selects the device to use for control protocol
 *                           communication.
 *   - chipremote,instr-ifc: Selects the device to use for firmware
 *                           instrumentation communication. This is only
 *                           useful with the native firmware builds.
 *
 * Ports must choose "chipremote,proto-ifc". There are more nodes in the
 * "chip-remote" subtree, that the firmware uses, but those are used by the
 * peripheral access code an the "interfaces.h" file. Thus they are not
 * discussed here.
 */

#include <zephyr/kernel.h>

#ifdef CONFIG_BOARD_NATIVE_POSIX
#include <zephyr/drivers/console/posix_arch_console.h>
#endif /* CONFIG_BOARD_NATIVE_POSIX */

#include <zephyr/devicetree.h>
#include <zephyr/drivers/gpio.h>
#include <zephyr/drivers/uart.h>
#include <zephyr/usb/usb_device.h>

#include <stdlib.h>

#ifdef CONFIG_BOARD_NATIVE_POSIX
#include <unistd.h>
#endif /* CONFIG_BOARD_NATIVE_POSIX */

#include <ufw/compiler.h>
#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>
#include <ufwz/endpoint-uart-poll.h>
#include <ufwz/endpoint-uart-fifo.h>

#include "chip-remote.h"
#include "peripherals.h"
#include "registers.h"

#ifdef CONFIG_CR_INTERFACE_NONE
#error Specify the chip-remote interface type in zephyr config of board!
#endif

#define CR_PROTO_NODE DT_CHOSEN(chipremote_proto_ifc)
#define CR_PROTO_IFC  DEVICE_DT_GET(CR_PROTO_NODE)

#if DT_NODE_EXISTS(DT_CHOSEN(chipremote_instr_ifc))
#include "native-instrumentation.h"
#define CR_INSTRUMENTATION_NODE DT_CHOSEN(chipremote_instr_ifc)
#define CR_INSTRUMENTATION_IFC  DEVICE_DT_GET(CR_INSTRUMENTATION_NODE)
#endif /* DT_NODE_EXISTS(DT_CHOSEN(chipremote_instr_ifc)) */

#ifdef CONFIG_CR_INTERFACE_SMART_SERIAL
#if DT_NODE_HAS_COMPAT(CR_PROTO_NODE, zephyr_cdc_acm_uart) == 1
#define CR_DO_ENABLE_USB 1
#endif /* chipremote_proto_ifc is type zephyr_cdc_acm_uart */
#endif /* CONFIG_CR_INTERFACE_SMART_SERIAL */

#ifdef CONFIG_CR_INTERFACE_SIMPLE_SERIAL
struct uart_config uart_cfg = {
    .baudrate  = 921600u,
    .parity    = UART_CFG_PARITY_NONE,
    .stop_bits = UART_CFG_STOP_BITS_1,
    .flow_ctrl = UART_CFG_FLOW_CTRL_NONE,
    .data_bits = UART_CFG_DATA_BITS_8
};
#endif /* CONFIG_CR_INTERFACE_SIMPLE_SERIAL */

int
main(void)
{
    const struct device *pifc = CR_PROTO_IFC;
    if (pifc == NULL || device_is_ready(pifc) == false) {
        printk("Could not access protocol interface. Giving up.\n");
        return EXIT_FAILURE;
    }

#ifdef CR_DO_ENABLE_USB
    if (usb_enable(NULL) != 0) {
        printk("Could not enable usb. Giving up.\n");
        return EXIT_FAILURE;
    }
#endif /* CR_DO_ENABLE_USB */

#ifdef CR_INSTRUMENTATION_IFC
    const struct device *instrifc = CR_INSTRUMENTATION_IFC;
    if (instrifc == NULL || device_is_ready(instrifc) == false) {
        printk("Could not access protocol interface. Giving up.\n");
        return EXIT_FAILURE;
    }
#endif /* CR_INSTRUMENTATION_IFC */

    if (peripheral_check() < 0) {
        return EXIT_FAILURE;
    }

#ifdef CONFIG_CR_INTERFACE_SIMPLE_SERIAL
    if (uart_configure(pifc, &uart_cfg) < 0) {
        printk("Could not configure %s. Giving up.\n", pifc->name);
        return EXIT_FAILURE;
    }

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

#ifdef CONFIG_CR_INTERFACE_TCPIP
#error The tcp/ip based interface type is not implemented yet.
#endif /* CONFIG_CR_INTERFACE_TCPIP */

    RegP protocol;

    if (chip_remote_init(&protocol, regpsource, regpsink, &registers) < 0) {
        printk("Could not setup chip-remote processor. Giving up.\n");
        return EXIT_FAILURE;
    }

#ifdef CR_INSTRUMENTATION_IFC
    struct resizeable_buffer nirb;
    rb_init(&nirb);
#endif /* CR_INSTRUMENTATION_IFC */

    printk("ChipRemoteFirmware running on %s\n", CONFIG_BOARD);

#ifdef CONFIG_BOARD_NATIVE_POSIX
    printk("(activated!)\n");
    printk("(firmware-pid %u)\n", getpid());
    posix_flush_stdout();
    /* Disable stderr output */
    close(STDERR_FILENO);
#endif /* CONFIG_BOARD_NATIVE_POSIX */

    for (;;) {
        const int rc0 = chip_remote_process(&protocol);

#ifdef CR_INSTRUMENTATION_IFC
        char ch1;
        const int rc1 = uart_poll_in(instrifc, &ch1);
        if (rc1 == 0) {
            ni_toplevel(&nirb, ch1, instrifc);
        }
#else
        const int rc1 = -1;
#endif /* CR_INSTRUMENTATION_IFC */

        if (rc1 != 0 && rc0 < 0) {
            k_usleep(1000);
        }
    }

    /* NOTREACHED */
    return EXIT_SUCCESS;
}
