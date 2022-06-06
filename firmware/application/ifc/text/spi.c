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

#include <chip-remote.h>
#include <cr-port.h>
#include <cr-utilities.h>
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

static int
cr_spi_text_init(UNUSED struct cr_protocol *proto, struct cr_port *port)
{
    port->initialised = true;
    return 0;
}

static int
cr_spi_text_boot(struct cr_protocol *proto, struct cr_port *port)
{
    return cr_spi_text_init(proto, port);
}

static int
cr_spi_text_xfer(struct cr_protocol *proto, struct cr_port *port,
                 unsigned int n, struct cr_value *args)
{
    cr_number *state = port->data;
    bool fromstate = true;
    cr_number tx, rx;

    if (n != 1) {
        return CR_PORTVAL_INVALID_NUMBER_OF_ARGS;
    }

    if (args[0].type != CR_PROTO_ARG_TYPE_INTEGER) {
        return CR_PORTVAL_INVALID_TYPE_OF_ARG;
    }

    tx = args[0].data.number;
    rx = 0;

    if (rxring != NULL && sx_is_list(rxring)) {
        struct sx_node *node = sx_pop(&rxring);
        if (sx_is_integer(node)) {
            fromstate = false;
            rx = node->data.u64;
        }
        sx_destroy(&node);
    }

    if (fromstate)
        rx = *state;

    printf("(spi-tx #x%016"PRIxCRN")\n", tx);
    printf("(spi-rx #x%016"PRIxCRN")\n", rx);
    cr_proto_put_number(proto, rx);
    cr_proto_put_newline(proto);

    if (fromstate)
        (*state)++;

    return CR_PORTVAL_REPLY_DONE;
}

static int
cr_spi_text_set(UNUSED struct cr_protocol *proto, UNUSED struct cr_port *port,
                UNUSED unsigned int n, UNUSED struct cr_value *value)
{
    return 0;
}

struct cr_port_api cr_port_impl_spi_text = {
    .boot = cr_spi_text_boot,
    .init = cr_spi_text_init,
    .xfer = cr_spi_text_xfer,
    .address = NULL,
    .set = cr_spi_text_set
};
