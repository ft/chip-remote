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

#include <ufw/endpoints.h>
#include <ufw/register-protocol.h>
#include <ufw/register-table.h>
#include <ufw/register-utilities.h>

#include <sx-parser.h>

#include "native-instrumentation.h"
#include "registers.h"

const struct device *uart0;

static size_t cnt = 0u;

static int
uart_octet_source(void *driver, void *value)
{
    const int rc = uart_poll_in(driver, value);
    if (rc >= 0) {
        cnt++;
    }
#if 0
    if (rc >= 0) {
        printk("uart0: 0x%02x (rc: %d)\n", *(unsigned char*)value, rc);
    }
#endif
    return rc < 0 ? -EAGAIN : 1;
}

static int
uart_octet_sink(void *driver, unsigned char value)
{
    uart_poll_out(driver, value);
    return 1;
}

static RPBlockAccess
regread(uint32_t address, size_t n, uint16_t *value)
{
    return regaccess2blockaccess(
        register_block_read(&registers, address, n, value));
}

static RPBlockAccess
regwrite(uint32_t address, size_t n, const uint16_t *value)
{
    return regaccess2blockaccess(
        register_block_write(&registers, address, n, (void*)value));
}

#define PROTO_SLAB_SLOTS 4u
#define PROTO_SLAB_SIZE  (128u + sizeof(RPFrame))

static int
proto_alloc(void *driver, void **memory)
{
    return k_mem_slab_alloc(driver, memory, K_NO_WAIT);
}

static void
proto_free(void *driver, void *memory)
{
    k_mem_slab_free(driver, memory);
}

K_MEM_SLAB_DEFINE_STATIC(proto_slab, PROTO_SLAB_SIZE, PROTO_SLAB_SLOTS, 4);
BlockAllocator palloc = MAKE_SLAB_BLOCKALLOC(
    &proto_slab, proto_alloc, proto_free, PROTO_SLAB_SIZE);

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

    register_make_bigendian(&registers, true);
    const RegisterInit regtabrc = register_init(&registers);

    if (regtabrc.code != REG_INIT_SUCCESS) {
        register_table_print(stdout, "# ", &registers);
        printf("#\n");
        register_init_print(stdout, "# ", regtabrc);
        return;
    }

    Source regpsource = OCTET_SOURCE_INIT(uart_octet_source, (void*)uart0);
    Sink   regpsink   = OCTET_SINK_INIT(  uart_octet_sink,   (void*)uart0);

    RegP protocol;
    regp_init(&protocol);
    regp_use_allocator(&protocol, &palloc);
    regp_use_memory16(&protocol, regread, regwrite);
    regp_use_channel(&protocol, RP_EP_SERIAL, regpsource, regpsink);

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
        RPMaybeFrame mf;
        const int recvrc = regp_recv(&protocol, &mf);
        if (recvrc < 0 && recvrc != -EAGAIN) {
            printk("# Error in regp_recv(): %d\n", recvrc);
        }
        const int procrc = regp_process(&protocol, &mf);
        if (procrc < 0) {
            printk("# Error in regp_process(): %d\n", procrc);
        }
        regp_free(&protocol, mf.frame);

        const int rc1 = uart_poll_in(uart1, &ch1);
        if (rc1 == 0) {
            ni_toplevel(&nirb, ch1);
        }
        if (rc1 != 0 && recvrc < 0) {
            k_usleep(1000);
        }
    }
}
