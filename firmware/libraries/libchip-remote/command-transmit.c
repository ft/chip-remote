/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
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
#include <cr-port.h>
#include <cr-utilities.h>

struct cr_argument transmit_arguments[] = {
    { .optional = false, .type = CR_PROTO_ARG_TYPE_INTEGER },
    { .optional = false, .type = CR_PROTO_ARG_TYPE_VOID }
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
    uint32_t rx;
    struct cr_port *p = current_port(proto);
    if (p->initialised == false) {
        proto->reply("WTF Focused port is not initialised!\n");
        return;
    }
    cr_transmit(p, cmd->args[0].data.u32, &rx);
    cr_proto_put_u32(proto, rx);
    cr_proto_put_newline(proto);
}
