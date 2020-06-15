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

struct cr_command_result
cr_handle_features(const struct cr_command *cmd,
                   UNUSED const struct cr_value *arg,
                   unsigned int argn)
{
    static unsigned int idx = 0u;
    struct cr_command_result rv = {
        .result = (argn == 0u) ? CR_PROTO_RESULT_OK
                               : CR_PROTO_RESULT_MALFORMED,
        .next_state = CR_PROTO_STATE_MULTILINE };

    if (rv.result == CR_PROTO_RESULT_MALFORMED)
        return rv;

    if (cmd->id == CR_PROTO_CMD_FEATURES) {
        idx = 0u;
    }

    for (; cr_commands[idx].id != CR_PROTO_CMD_UNKNOWN; ++idx) {
        if (cr_commands[idx].id == CR_PROTO_CMD_MORE) {
            continue;
        }

        if (cr_commands[idx].cb != NULL) {
            printf("feature: %s\n", cr_commands[idx].name);
            idx++;
            break;
        }
    }

    if (cr_commands[idx].id == CR_PROTO_CMD_UNKNOWN) {
        printf("feature: DONE\n");
        rv.next_state = CR_PROTO_STATE_ACTIVE;
    }

    return rv;
}
