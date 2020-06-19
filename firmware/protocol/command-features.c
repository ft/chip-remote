/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-features.c
 * @brief Implemenation of FEATURES command
 */

#include <stdio.h>

#include <common/compiler.h>

#include <commands.h>
#include <commands-private.h>

enum cr_proto_state
cr_handle_features(const struct cr_protocol *proto,
                   const struct cr_command *cmd,
                   UNUSED const struct cr_value *arg,
                   UNUSED unsigned int argn)
{
    static unsigned int idx = 0u;

    if (cmd->id == CR_PROTO_CMD_FEATURES) {
        idx = 0u;
    }

    for (; cr_commands[idx].id != CR_PROTO_CMD_UNKNOWN; ++idx) {
        if (cr_commands[idx].id == CR_PROTO_CMD_MORE) {
            continue;
        }

        if (cr_commands[idx].cb != NULL) {
            proto->reply(cr_commands[idx].name);
            proto->reply("\n");
            idx++;
            break;
        }
    }

    if (cr_commands[idx].id == CR_PROTO_CMD_UNKNOWN) {
        proto->reply("DONE\n");
        return CR_PROTO_STATE_ACTIVE;
    }

    return CR_PROTO_STATE_MULTILINE;
}
