#include <device.h>
#include <kernel.h>

#include <drivers/uart.h>

#include <string.h>

#include <cr-process.h>
#include <parse-string.h>

#include "init-common.h"

#ifndef CONFIG_ARCH_POSIX
#include "bitbang-spi.h"
#endif /* CONFIG_ARCH_POSIX */

#define CR_QUEUE_SIZE 16u
K_MSGQ_DEFINE(cr_charqueue, sizeof(char), CR_QUEUE_SIZE, 4u);

#define CR_STACK_SIZE 512u
#define CR_PRIORITY 5u

char cr_input[CR_MAX_LINE_SIZE];

const struct device *uart0;

#ifdef CONFIG_ARCH_POSIX

int
text_transmit(const struct cr_port *port, const uint32_t tx, uint32_t *rx)
{
    static uint32_t state = 0u;
    printk("cr>> 0x%08x\n", tx);
    printk("cr<< 0x%08x\n", state);
    *rx = state;
    state++;
    return 0;
}

#else

int
spi_transmit(const struct cr_port *port, const uint32_t tx, uint32_t *rx)
{
    printk("cr>> 0x%08x\n", tx);
    *rx = cr_spi_xfer(&bbspi, tx);
    printk("cr<< 0x%08x\n", *rx);
    return 0;
}

#endif /* CONFIG_ARCH_POSIX */

void
cr_run(void *a, void *b, void *c)
{
    struct cr_protocol proto;
    char ch = 0;

    printk("ChipRemote Command Processor online!\n");

#ifdef CONFIG_ARCH_POSIX
    cr_process_init(&proto, cr_input, CR_MAX_LINE_SIZE,
                    text_transmit, uart_sink);
#else
    cr_spi_init(&bbspi);
    cr_process_init(&proto, cr_input, CR_MAX_LINE_SIZE,
                    spi_transmit, uart_sink);
#endif /* CONFIG_ARCH_POSIX */
    for (;;) {
        k_msgq_get(&cr_charqueue, &ch, K_FOREVER);
        cr_toplevel(&proto, ch);
    }
}

K_THREAD_DEFINE(cr_run_thread, CR_STACK_SIZE,
                cr_run, NULL, NULL, NULL,
                CR_PRIORITY, 0, 0);
