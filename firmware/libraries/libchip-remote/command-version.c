/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-features.c
 * @brief Implemenation of VERSION command
 */

#include <ufw/compiler.h>

#include <commands.h>
#include <commands-private.h>
#include <cr-utilities.h>

void
cr_handle_version(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
                  UNUSED struct cr_value *t, UNUSED unsigned int n)
{
    proto->reply("VERSION ");
    cr_proto_put_number(proto, CR_PROTOCOL_VERSION_MAJOR);
    cr_proto_put_space(proto);
    cr_proto_put_number(proto, CR_PROTOCOL_VERSION_MINOR);
    cr_proto_put_space(proto);
    cr_proto_put_number(proto, CR_PROTOCOL_VERSION_PATCHLEVEL);
    cr_proto_put_newline(proto);
}
