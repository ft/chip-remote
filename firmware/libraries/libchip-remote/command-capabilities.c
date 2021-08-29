/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-ports.c
 * @brief Implemenation of CAPABILITIES command
 */

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

void
cr_handle_capabilities(const struct cr_protocol *proto,
                       UNUSED const struct cr_proto_parse *cmd)
{
    proto->reply("rx-buffer-size ");
    cr_proto_put_u32(proto, proto->in.size);
    proto->reply(";maximum-arguments ");
    cr_proto_put_u32(proto, CR_PROTO_MAX_ARGS);
    for (size_t i = 0u; cr_commands[i].id != CR_PROTO_CMD_UNKNOWN; ++i) {
        if (cr_commands[i].name != NULL && cr_commands[i].name[0] == '+') {
            proto->reply(";");
            proto->reply(cr_commands[i].name);
        }
    }
    cr_proto_put_newline(proto);
}
