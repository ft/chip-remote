/*
 * Copyright (c) 2021 Frank Terbeck <ft@bewatermyfriend.org>
 * All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include <common/compiler.h>

#include <cr-port.h>
#include <sx-parser.h>

#include "spi.h"

struct sx_node *rxring = NULL;

void
cr_spi_text_load(struct sx_node *node)
{
    if (rxring == NULL) {
        rxring = node;
    } else {
        rxring = sx_append(rxring, node);
    }
}

int
cr_spi_text_init(struct cr_port *port)
{
    port->initialised = true;
    return 0;
}

int
cr_spi_text_xfer(struct cr_port *port, uint32_t tx, uint32_t *rx)
{
    uint32_t *state = port->data;
    bool fromstate = true;

    if (rxring != NULL && sx_is_list(rxring)) {
        struct sx_node *n = sx_pop(&rxring);
        if (sx_is_integer(n)) {
            fromstate = false;
            *rx = n->data.u64;
        } else if (sx_is_null(n)) {
            rxring = NULL;
        }
        sx_destroy(&n);
    }

    if (fromstate)
        *rx = *state;

    printf("(spi-tx #x%08x)\n", tx);
    printf("(spi-rx #x%08x)\n", *rx);

    if (fromstate)
        (*state)++;

    return 0;
}

int
cr_spi_text_set(UNUSED struct cr_port *port,
                UNUSED const char *key,
                UNUSED const char *value)
{
    return 0;
}

struct cr_port_api cr_port_impl_spi_text = {
    .init = cr_spi_text_init,
    .xfer = cr_spi_text_xfer,
    .address = NULL,
    .set = cr_spi_text_set
};
