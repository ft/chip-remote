/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-ports.c
 * @brief Implemenation of PORTS command
 */

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

void
cr_handle_ports(const struct cr_protocol *proto,
                UNUSED const struct cr_proto_parse *cmd)
{
    proto->reply("PORTS ");
    cr_proto_put_u32(proto, proto->ports.tablesize);
    proto->reply(";FOCUS ");
    if (proto->ports.focused) {
        cr_proto_put_u32(proto, proto->ports.current);
    } else {
        proto->reply("NONE");
    }
    cr_proto_put_newline(proto);
}
