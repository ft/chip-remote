/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-init.c
 * @brief Implemenation of PORTS command
 */

#include <stdint.h>

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

struct cr_argument init_arguments[] = {
    { .optional = false, .type = CR_PROTO_ARG_TYPE_INTEGER },
    { .optional = false, .type = CR_PROTO_ARG_TYPE_VOID }
};

void
cr_handle_init(const struct cr_protocol *proto,
               const struct cr_proto_parse *cmd)
{
    uint32_t idx = cmd->args[0].data.u32;
    if (idx >= proto->ports.tablesize) {
        proto->reply("VALUE-OUT-OF-RANGE ");
        cr_proto_put_u32(proto, idx);
        proto->reply(" > ");
        cr_proto_put_u32(proto, proto->ports.tablesize - 1);
        cr_proto_put_newline(proto);
    } else {
        struct cr_port *p = proto->ports.table[proto->ports.current];
        int rv = p->api->init(p);
        if (rv == 0)
            proto->reply("OK\n");
        else
            proto->reply("WTF Port initialisation failed!\n");
    }
}
