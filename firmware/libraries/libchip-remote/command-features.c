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

cr_callback_value
cr_handle_features(const struct cr_protocol *proto,
                   UNUSED const struct cr_proto_parse *cmd)
{
    for (unsigned int idx = 0u; ; ++idx) {
        if (cr_commands[idx].cb != NULL) {
            proto->reply(cr_commands[idx].name);
            if (cr_commands[idx+1].id != CR_PROTO_CMD_UNKNOWN) {
                proto->reply(";");
            } else {
                break;
            }
        }
    }

    proto->reply("\n");
    return CR_CB_OK;
}
