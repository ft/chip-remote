/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-set.c
 * @brief Implemenation of SET command
 */

#include <stdint.h>

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands-private.h>
#include <cr-utilities.h>

struct cr_argument set_arguments[] = {
    { .optional = false, .type = CR_PROTO_ARG_TYPE_INTEGER },
    { .optional = false, .type = CR_PROTO_ARG_TYPE_STRING },
    { .optional = false, .type = CR_PROTO_ARG_TYPE_STRING },
    { .optional = false, .type = CR_PROTO_ARG_TYPE_VOID }
};

void
cr_handle_set(const struct cr_protocol *proto,
              const struct cr_proto_parse *cmd)
{
    const uint32_t idx = cmd->args[0].data.u32;
    proto->reply("WTF Not implemented yet: SET ");
    cr_proto_put_u32(proto, idx);
    cr_proto_put_space(proto);
    proto->reply(cmd->args[1].data.string);
    cr_proto_put_space(proto);
    proto->reply(cmd->args[2].data.string);
    cr_proto_put_newline(proto);
}
