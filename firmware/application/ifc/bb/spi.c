/*
 * Copyright (c) 2021 Frank Terbeck <ft@bewatermyfriend.org>
 * All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <drivers/gpio.h>

#include <stdbool.h>
#include <stdint.h>

#include <common/bit-operations.h>
#include <cr-port.h>

#include "spi.h"

static inline void
cr_spi_line_set(const struct cr_line *line, int value)
{
    gpio_pin_set(line->port, line->pin, value);
}

static inline int
cr_spi_line_get(const struct cr_line *line)
{
    return gpio_pin_get(line->port, line->pin);
}

static int
cr_spi_bb_set_gpio(struct cr_line *line,
                   gpio_flags_t flags,
                   enum cr_line_mode mode)
{
    int rv = gpio_pin_configure(line->port, line->pin, flags);

    if (rv < 0)
        return rv;

    line->mode = mode;
    return rv;
}

static int
cr_spi_bb_output(struct cr_line *line)
{
    return cr_spi_bb_set_gpio(line, GPIO_OUTPUT_ACTIVE, CR_LINE_OUTPUT_PUSHPULL);
}

static int
cr_spi_bb_input(struct cr_line *line)
{
    return cr_spi_bb_set_gpio(line, GPIO_INPUT, CR_LINE_INPUT_PULLDOWN);
}

int
cr_spi_bb_init(struct cr_port *port)
{
    const struct cr_port_spi_bb *spi = port->data;

    if (spi == NULL)
        return -1;

    int rv = 0;

    rv = cr_spi_bb_output(spi->clk);
    if (rv < 0)
        return rv;
    rv = cr_spi_bb_output(spi->cs);
    if (rv < 0)
        return rv;
    rv = cr_spi_bb_output(spi->mosi);
    if (rv < 0)
        return rv;
    rv = cr_spi_bb_input(spi->miso);

    port->initialised = true;
    return rv;
}

int
cr_spi_bb_xfer(struct cr_port *port, uint32_t tx, uint32_t *rx)
{
    const struct cr_port_spi_bb *spi = port->data;
    uint32_t rv = 0ull;
    const size_t len = 16;

    *rx = 0u;
    cr_spi_line_set(spi->cs, 0u);
    for (size_t i = 0ull; i < len; ++i) {
        const size_t idx = len - i - 1;
        cr_spi_line_set(spi->mosi, BITLL_GET(tx, 1u, idx));
        cr_spi_line_set(spi->clk, 1u);
        if (cr_spi_line_get(spi->miso) > 0) {
            *rx |= BITLL(idx);
        }
        cr_spi_line_set(spi->clk, 0u);
    }
    cr_spi_line_set(spi->cs, 1u);

    return rv;
}

struct cr_port_api cr_port_impl_spi_bb = {
    .init = cr_spi_bb_init,
    .xfer = cr_spi_bb_xfer,
    .address = NULL
};
