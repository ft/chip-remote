#include <cr-port.h>
#include <cr-process.h>

#include "init-common.h"

char cr_input[CR_MAX_LINE_SIZE];
const struct device *uart0;

struct cr_protocol proto = {
    .ports = {
        .table = (struct cr_port*[]){
            [0] = &port00_spi
#ifdef CONFIG_SPI
            ,
            [1] = &port01_spi
#endif /* CONFIG_SPI */
        },

#ifdef CONFIG_SPI
        .tablesize = 2u,
#else
        .tablesize = 1u,
#endif /* CONFIG_SPI */
        .current = 0u,
        .focused = true
    },
    .state.input = CR_INPUT_PROCESS,
    .in.buffer = cr_input,
    .in.size = CR_MAX_LINE_SIZE,
    .in.idx = 0,
    .reply = uart_sink
};
