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
#include <common/compiler.h>
#include <cr-port.h>
#include <cr-utilities.h>

#include "chip-remote.h"
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
cr_spi_bb_set(struct cr_protocol *proto, struct cr_port *port,
              UNUSED unsigned int n, struct cr_value *value)
{
    const char *key = value->data.symbol;

    if (strcmp(key, "rate") == 0) {
        /* Accept any and all rates; this bit-bang implementation will use its
         * native rate in any case. */
        port->cfg.spi.clk.rate = 0;
    } else if (strcmp(key, "clk-phase-delay") == 0) {
        if (REQUIRE_ARG_TYPE(proto, value, 1, BOOLEAN) == false) {
            return CR_PORTVAL_INVALID_TYPE_OF_ARG;
        }
        port->cfg.spi.clk.phase_delay = value[1].data.boolean;
    } else if (strcmp(key, "frame-length") == 0) {
        if (cr_value_max(proto, value, 1, 64u) == false) {
            return CR_PORTVAL_REPLY_DONE;
        }
        port->cfg.spi.frame_length = n;
    } else if (strcmp(key, "bit-order") == 0) {
        if (REQUIRE_ARG_TYPE(proto, value, 1, SYMBOL) == false) {
            return CR_PORTVAL_INVALID_TYPE_OF_ARG;
        }
        const char *sym = value[1].data.symbol;
        if (strcmp(sym, "lsb-first") == 0) {
            port->cfg.spi.bit_order = CR_BIT_LSB_FIRST;
        } else if (strcmp(sym, "msb-first") == 0) {
            port->cfg.spi.bit_order = CR_BIT_MSB_FIRST;
        } else {
            proto->reply("Invalid symbol: ");
            proto->reply(sym);
            cr_proto_put_newline(proto);
            return CR_PORTVAL_REPLY_DONE;
        }
    } else if (strcmp(key, "cs-polarity") == 0) {
        if (REQUIRE_ARG_TYPE(proto, value, 1, SYMBOL) == false) {
            return CR_PORTVAL_INVALID_TYPE_OF_ARG;
        }
        const char *sym = value[1].data.symbol;
        if (strcmp(sym, "active-high") == 0) {
            port->cfg.spi.cs.polarity = CR_LOGIC_DIRECT;
        } else if (strcmp(sym, "active-low") == 0) {
            port->cfg.spi.cs.polarity = CR_LOGIC_INVERTED;
        } else {
            proto->reply("Invalid symbol: ");
            proto->reply(sym);
            cr_proto_put_newline(proto);
            return CR_PORTVAL_REPLY_DONE;
        }
    } else if (strcmp(key, "clk-polarity") == 0) {
        if (REQUIRE_ARG_TYPE(proto, value, 1, SYMBOL) == false) {
            return CR_PORTVAL_INVALID_TYPE_OF_ARG;
        }
        const char *sym = value[1].data.symbol;
        if (strcmp(sym, "rising-edge") == 0) {
            port->cfg.spi.clk.edge = CR_EDGE_RISING;
        } else if (strcmp(sym, "falling-edge") == 0) {
            port->cfg.spi.clk.edge = CR_EDGE_FALLING;
        } else {
            proto->reply("Invalid symbol: ");
            proto->reply(sym);
            cr_proto_put_newline(proto);
            return CR_PORTVAL_REPLY_DONE;
        }
    } else if (strcmp(key, "mode") == 0) {
    } else {
        proto->reply("Invalid setting: ");
        proto->reply(key);
        cr_proto_put_newline(proto);
        return CR_PORTVAL_REPLY_DONE;
    }

    port->initialised = false;
    return CR_PORTVAL_OK;
}

static int
cr_spi_bb_init(UNUSED struct cr_protocol *proto, struct cr_port *port)
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

inline static cr_number
cr_spi_bb_xfer_bit(const struct cr_port *port,
                   const struct cr_port_spi_bb *spi,
                   const size_t bit,
                   const cr_number tx,
                   cr_number rx)
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

static int
cr_spi_bb_xfer(struct cr_protocol *proto, struct cr_port *port,
               unsigned int n, struct cr_value *args)
{
    const struct cr_port_spi_bb *spi = port->data;
    const size_t len = port->cfg.spi.frame_length;
    cr_number tx, rx;

    if (n != 1) {
        return CR_PORTVAL_INVALID_NUMBER_OF_ARGS;
    }

    if (args[0].type != CR_PROTO_ARG_TYPE_INTEGER) {
        return CR_PORTVAL_INVALID_TYPE_OF_ARG;
    }

    tx = args[0].data.number;
    rx = 0;

    if (port->cfg.spi.cs.polarity == CR_LOGIC_DIRECT) {
        cr_spi_line_set(spi->cs, 1u);
    } else {
        cr_spi_line_set(spi->cs, 0u);
    }

    if (port->cfg.spi.bit_order == CR_BIT_MSB_FIRST) {
        for (size_t i = 0ull; i < len; ++i) {
            const size_t idx = len - i - 1;
            rx = cr_spi_bb_xfer_bit(port, spi, idx, tx, rx);
        }
    } else {
        for (size_t idx = 0ull; idx < len; ++idx) {
            rx = cr_spi_bb_xfer_bit(port, spi, idx, tx, rx);
        }
    }

    if (port->cfg.spi.cs.polarity == CR_LOGIC_DIRECT) {
        cr_spi_line_set(spi->cs, 0u);
    } else {
        cr_spi_line_set(spi->cs, 1u);
    }

    cr_proto_put_number(proto, rx);

    return CR_PORTVAL_REPLY_DONE;
}

struct cr_port_api cr_port_impl_spi_bb = {
    .init = cr_spi_bb_init,
    .xfer = cr_spi_bb_xfer,
    .address = NULL,
    .set = cr_spi_bb_set
};
