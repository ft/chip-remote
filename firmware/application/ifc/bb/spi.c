/*
 * Copyright (c) 2021 Frank Terbeck <ft@bewatermyfriend.org>
 * All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <drivers/gpio.h>

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include <common/bit-operations.h>
#include <cr-port.h>
#include <cr-utilities.h>

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
cr_spi_bb_set(struct cr_port *port, const char *key, const char *value)
{
    int err;

    if (strcmp(key, "rate") == 0) {
        (void)cr_parse_u32(value, &err);
        if (err > 0) {
            return -3;
        }
        /* Accept any and all rates; this bit-bang implementation will use its
         * native rate in any case. */
        port->cfg.spi.clk.rate = 0;
    } else if (strcmp(key, "clk-phase-delay") == 0) {
        if (string_bool_true(value)) {
            port->cfg.spi.clk.phase_delay = true;
        } else if (string_bool_false(value)) {
            port->cfg.spi.clk.phase_delay = false;
        } else {
            return -3;
        }
    } else if (strcmp(key, "frame-length") == 0) {
        const uint32_t n = cr_parse_u32(value, &err);
        if (err > 0) {
            return -3;
        }
        if (n < 1 || n > 32) {
            return -1;
        }
        port->cfg.spi.frame_length = n;
    } else if (strcmp(key, "bit-order") == 0) {
        if (strcmp(value, "lsb-first") == 0) {
            port->cfg.spi.bit_order = CR_BIT_LSB_FIRST;
        } else if (strcmp(value, "msb-first") == 0) {
            port->cfg.spi.bit_order = CR_BIT_MSB_FIRST;
        } else {
            return -3;
        }
    } else if (strcmp(key, "cs-polarity") == 0) {
        if (strcmp(value, "active-high") == 0) {
            port->cfg.spi.cs.polarity = CR_LOGIC_DIRECT;
        } else if (strcmp(value, "active-low") == 0) {
            port->cfg.spi.cs.polarity = CR_LOGIC_INVERTED;
        } else {
            return -3;
        }
    } else if (strcmp(key, "clk-polarity") == 0) {
        if (strcmp(value, "rising-edge") == 0) {
            port->cfg.spi.clk.edge = CR_EDGE_RISING;
        } else if (strcmp(value, "falling-edge") == 0) {
            port->cfg.spi.clk.edge = CR_EDGE_FALLING;
        } else {
            return -3;
        }
    } else if (strcmp(key, "mode") == 0) {
        return 1;
    } else {
        return -2;
    }

    port->initialised = false;
    return 0;
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

    if (port->cfg.spi.clk.edge == CR_EDGE_RISING) {
        cr_spi_line_set(spi->clk, 0u);
    } else {
        cr_spi_line_set(spi->clk, 1u);
    }

    port->initialised = true;
    return rv;
}

inline static uint32_t
cr_spi_bb_xfer_bit(const struct cr_port *port,
                   const struct cr_port_spi_bb *spi,
                   const size_t bit,
                   const uint32_t tx,
                   uint32_t rx)
{
    cr_spi_line_set(spi->mosi, BITLL_GET(tx, 1u, bit));

    if (port->cfg.spi.clk.edge == CR_EDGE_RISING) {
        cr_spi_line_set(spi->clk, 1u);
    } else {
        cr_spi_line_set(spi->clk, 0u);
    }

    rx |= (cr_spi_line_get(spi->miso) > 0) ? BITLL(bit) : 0u;

    if (port->cfg.spi.clk.edge == CR_EDGE_RISING) {
        cr_spi_line_set(spi->clk, 0u);
    } else {
        cr_spi_line_set(spi->clk, 1u);
    }

    return rx;
}

int
cr_spi_bb_xfer(struct cr_port *port, uint32_t tx, uint32_t *rx)
{
    const struct cr_port_spi_bb *spi = port->data;
    uint32_t rv = 0ull;
    const size_t len = port->cfg.spi.frame_length;

    *rx = 0u;
    if (port->cfg.spi.cs.polarity == CR_LOGIC_DIRECT) {
        cr_spi_line_set(spi->cs, 1u);
    } else {
        cr_spi_line_set(spi->cs, 0u);
    }

    if (port->cfg.spi.bit_order == CR_BIT_MSB_FIRST) {
        for (size_t i = 0ull; i < len; ++i) {
            const size_t idx = len - i - 1;
            *rx = cr_spi_bb_xfer_bit(port, spi, idx, tx, *rx);
        }
    } else {
        for (size_t idx = 0ull; idx < len; ++idx) {
            *rx = cr_spi_bb_xfer_bit(port, spi, idx, tx, *rx);
        }
    }

    if (port->cfg.spi.cs.polarity == CR_LOGIC_DIRECT) {
        cr_spi_line_set(spi->cs, 0u);
    } else {
        cr_spi_line_set(spi->cs, 1u);
    }

    return rv;
}

struct cr_port_api cr_port_impl_spi_bb = {
    .init = cr_spi_bb_init,
    .xfer = cr_spi_bb_xfer,
    .address = NULL,
    .set = cr_spi_bb_set
};
