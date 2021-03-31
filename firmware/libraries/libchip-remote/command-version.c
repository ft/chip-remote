/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-features.c
 * @brief Implemenation of FEATURES command
 */

#include <common/compiler.h>

#include <commands.h>
#include <commands-private.h>
#include <cr-utilities.h>

void
cr_handle_version(const struct cr_protocol *proto,
                  UNUSED const struct cr_proto_parse *cmd)
{
    proto->reply("VERSION ");
    cr_proto_put_u32(proto, CR_PROTOCOL_VERSION_MAJOR);
    cr_proto_put_space(proto);
    cr_proto_put_u32(proto, CR_PROTOCOL_VERSION_MINOR);
    cr_proto_put_space(proto);
    cr_proto_put_u32(proto, CR_PROTOCOL_VERSION_PATCHLEVEL);
    cr_proto_put_newline(proto);
}
