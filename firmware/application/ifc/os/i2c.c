/*
 * Copyright (c) 2022 Frank Terbeck <ft@bewatermyfriend.org>
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

#include "i2c.h"

#include <common/bit-operations.h>
#include <common/compiler.h>

static struct {
    uint8_t tx[CONFIG_OS_I2C_TX_BUFFER_SIZE];
    uint8_t rx[CONFIG_OS_I2C_RX_BUFFER_SIZE];
} buffer;

int
cr_i2c_os_set(struct cr_protocol *proto, UNUSED struct cr_port *port,
              UNUSED unsigned int n, UNUSED struct cr_value *value)
{
    port->initialised = false;
    return CR_PORTVAL_OK;
}

static uint32_t
cr_i2c_set_config(UNUSED const struct cr_port *port)
{
    return 0u;
}

static int
cr_i2c_os_init(UNUSED struct cr_protocol *proto, struct cr_port *port)
{
    struct cr_port_i2c_os *i2c = port->data;

    if (i2c == NULL)
        return CR_PORTVAL_INTERNAL_ERROR;

    i2c_configure(i2c->bus, cr_i2c_set_config(port));
    port->initialised = true;

    return CR_PORTVAL_OK;
}

static int
cr_i2c_os_boot(struct cr_protocol *proto, struct cr_port *port)
{
    return cr_i2c_os_init(proto, port);
}

static int
cr_i2c_os_xfer(struct cr_protocol *proto, struct cr_port *port,
               unsigned int n, struct cr_value *args)
{
    if (n != 1) {
        return CR_PORTVAL_INVALID_NUMBER_OF_ARGS;
    }

    if (args[0].type != CR_PROTO_ARG_TYPE_INTEGER) {
        return CR_PORTVAL_INVALID_TYPE_OF_ARG;
    }

    cr_proto_put_number(proto, 0);
    cr_proto_put_newline(proto);

    return CR_PORTVAL_REPLY_DONE;
}

struct cr_port_api cr_port_impl_i2c_os = {
    .boot = cr_i2c_os_boot,
    .init = cr_i2c_os_init,
    .xfer = cr_i2c_os_xfer,
    .address = NULL,
    .set = cr_i2c_os_set
};
