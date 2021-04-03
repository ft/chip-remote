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

#include "spi.h"

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

    *rx = *state;
    printf("cr>> 0x%08x\n", tx);
    printf("cr<< 0x%08x\n", *state);
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
