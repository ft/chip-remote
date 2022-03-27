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

enum cr_argument_type address_args[] = { CR_PROTO_ARG_TYPE_INTEGER };

static void
address_help(string_sink reply)
{
    reply("wtf usage: address INDEX\n");
}

void
cr_handle_address(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
                  struct cr_value *t, unsigned int n)
{
    if (n != 2u) {
        address_help(proto->reply);
        return;
    }

    struct cr_port *p = proto->ports.table[proto->ports.current];
    if (p->api->address == NULL) {
        proto->reply("wtf Focused port does not support address!\n");
        return;
    }

    int rv = p->api->address(p, t[1].data.number);

    if (rv == 0) {
        proto->reply("ok\n");
    } else {
        proto->reply("wtf Could not address desired device!\n");
    }
}
