#include <cr-process.h>

#include "init-common.h"

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
    int rv = port00_spi.api->xfer(&port00_spi, tx, rx);
    printk("cr<< 0x%08x\n", *rx);
    return rv;
}

#endif /* CONFIG_ARCH_POSIX */

struct cr_protocol proto = {
    .state.protocol = CR_PROTO_STATE_IDLE,
    .state.input = CR_INPUT_PROCESS,
    .in.buffer = cr_input,
    .in.size = CR_MAX_LINE_SIZE,
    .in.idx = 0,
#ifdef CONFIG_ARCH_POSIX
    .transmit = text_transmit,
#else
    .transmit = spi_transmit,
#endif /* CONFIG_ARCH_POSIX */
    .reply = uart_sink
};

char cr_input[CR_MAX_LINE_SIZE];
const struct device *uart0;
