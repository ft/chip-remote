/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-address.c
 * @brief Implemenation of ADDRESS command
 */

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

struct cr_argument address_arguments[] = {
    { .optional = false, .type = CR_PROTO_ARG_TYPE_INTEGER },
    { .optional = false, .type = CR_PROTO_ARG_TYPE_VOID }
};

void
cr_handle_address(const struct cr_protocol *proto,
                  const struct cr_proto_parse *cmd)
{
    struct cr_port *p = proto->ports.table[proto->ports.current];
    if (p->api->address == NULL) {
        proto->reply("WTF Focused port does not support ADDRESS!\n");
        return;
    }

    int rv = p->api->address(p, cmd->args[0].data.u32);

    if (rv == 0) {
        proto->reply("OK\n");
    } else {
        proto->reply("WTF Could not ADDRESS desired device!\n");
    }
}
