/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-transmit.c
 * @brief Implemenation of TRANSMIT command
 */

#include <common/compiler.h>

#include <commands.h>
#include <commands-private.h>
#include <chip-remote.h>
#include <cr-port.h>
#include <cr-utilities.h>

#define CR_DATA_MAX 8u

struct cr_buf64 {
    cr_number data[CR_DATA_MAX];
    size_t n;
};

struct cr_transmission {
    struct cr_buf64 tx;
    struct cr_buf64 rx;
};

static inline struct cr_port*
current_port(const struct cr_protocol *proto)
{
    return proto->ports.table[proto->ports.current];
}

void
cr_handle_transmit(const struct cr_protocol *proto,
                   const struct cr_proto_parse *cmd)
{
    cr_number rx, n;
    unsigned int error;
    char *stop;
    struct cr_port *p = current_port(proto);

    if (p->initialised == false) {
        proto->reply("wtf Focused port is not initialised!\n");
        return;
    }
    n = cr_parse_number(cmd->args[0].data.symbol,
                        16u, &stop, &error);
    if (error != 0u) {
        proto->reply("wtf Broken datum to transmit!\n");
        return;
    }

    cr_transmit(p, n, &rx);
    cr_proto_put_number(proto, rx);
    cr_proto_put_newline(proto);
}
