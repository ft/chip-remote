/*
 * Copyright (c) 2021 Frank Terbeck <ft@bewatermyfriend.org>
 * All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#include <chip-remote.h>
#include <cr-port.h>
#include <cr-utilities.h>

#include "spi.h"

#include <ufw/bit-operations.h>
#include <ufw/compiler.h>

static inline struct spi_config*
cr_spi_get_config(struct cr_port_spi_os *p)
{
    return (p->cfg.mux != 0) ? &p->cfg.b : &p->cfg.a;
}

static inline void
cr_spi_toggle_mux(struct cr_port_spi_os *p)
{
    p->cfg.mux = !p->cfg.mux;
}

int
cr_spi_os_set(struct cr_protocol *proto, struct cr_port *port,
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

static void
cr_spi_set_config(struct spi_config *cfg, const struct cr_port *port)
{
    const uint16_t full_duplex = BIT(11u);
    cfg->slave = 0;
    cfg->frequency = port->cfg.spi.clk.rate;
    cfg->operation =
        SPI_OP_MODE_MASTER                                                   |
        (port->cfg.spi.bit_order == CR_BIT_MSB_FIRST ? SPI_TRANSFER_MSB : 0) |
        SPI_WORD_SET(port->cfg.spi.frame_length)                             |
        full_duplex;
}

static int
cr_spi_os_init(UNUSED struct cr_protocol *proto, struct cr_port *port)
{
    struct cr_port_spi_os *spi = port->data;

    if (spi == NULL)
        return CR_PORTVAL_INTERNAL_ERROR;

    if (spi->api_used)
        cr_spi_toggle_mux(spi);

    struct spi_config *cfg = cr_spi_get_config(spi);
    cr_spi_set_config(cfg, port);
    port->initialised = true;

    return CR_PORTVAL_OK;
}

static int
cr_spi_os_boot(struct cr_protocol *proto, struct cr_port *port)
{
    return cr_spi_os_init(proto, port);
}

static int
cr_spi_os_xfer(struct cr_protocol *proto, struct cr_port *port,
               unsigned int n, struct cr_value *args)
{
    if (n != 1) {
        return CR_PORTVAL_INVALID_NUMBER_OF_ARGS;
    }

    if (args[0].type != CR_PROTO_ARG_TYPE_INTEGER) {
        return CR_PORTVAL_INVALID_TYPE_OF_ARG;
    }

    uint8_t txbytes[sizeof(cr_number)];
    uint8_t rxbytes[sizeof(cr_number)];
    cr_number_to_bytes(args[0].data.number, txbytes);

    struct spi_buf txb = {
        .buf = txbytes,
        .len = sizeof(txbytes)/sizeof(*txbytes)
    };

    struct spi_buf rxb = {
        .buf = rxbytes,
        .len = sizeof(rxbytes)/sizeof(*rxbytes)
    };

    struct spi_buf_set tx = {
        .buffers = &txb,
        .count = 1u,
    };

    struct spi_buf_set rx = {
        .buffers = &rxb,
        .count = 1u,
    };

    struct cr_port_spi_os *spi = port->data;
    struct spi_config *cfg = cr_spi_get_config(spi);
    spi->api_used = true;
    int rc = spi_transceive(spi->bus, cfg, &tx, &rx);
    if (rc < 0) {
        return CR_PORTVAL_ERRNO;
    }
    cr_number rv = cr_number_from_bytes(rx.buffers[0].buf);

    cr_proto_put_number(proto, rv);
    cr_proto_put_newline(proto);

    return CR_PORTVAL_REPLY_DONE;
}

struct cr_port_api cr_port_impl_spi_os = {
    .boot = cr_spi_os_boot,
    .init = cr_spi_os_init,
    .xfer = cr_spi_os_xfer,
    .address = NULL,
    .set = cr_spi_os_set
};
