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
cr_handle_capabilities(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
                       UNUSED struct cr_value *t, UNUSED unsigned int n)
{
    proto->reply("rx-buffer-size ");
    cr_proto_put_number(proto, proto->in.size);
    proto->reply(";maximum-arguments ");
    cr_proto_put_number(proto, CR_PROTOCOL_MAX_TOKENS);
    for (size_t i = 0u; cr_commands[i].id != CR_PROTO_CMD_UNKNOWN; ++i) {
        if (cr_commands[i].name != NULL && cr_commands[i].name[0] == '+') {
            proto->reply(";");
            proto->reply(cr_commands[i].name);
        }
    }
    cr_proto_put_newline(proto);
}
