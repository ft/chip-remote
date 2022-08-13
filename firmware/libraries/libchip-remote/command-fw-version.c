/*
 * Copyright (c) 2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file command-features.c
 * @brief Implemenation of extended firmware VERSION command
 */

#include <ufw/compiler.h>

#include <commands.h>
#include <commands-private.h>
#include <cr-utilities.h>

#define FIRMWARE_VERSION_MAJOR 0u
#define FIRMWARE_VERSION_MINOR 0u
#define FIRMWARE_VERSION_MICRO 1u

void
cr_handle_fw_version(struct cr_protocol *proto, UNUSED struct cr_command *cmd,
                     UNUSED struct cr_value *t, UNUSED unsigned int n)
{
    proto->reply("chip-remote firmware version ");
    cr_proto_put_number(proto, FIRMWARE_VERSION_MAJOR);
    cr_proto_put_space(proto);
    cr_proto_put_number(proto, FIRMWARE_VERSION_MINOR);
    cr_proto_put_space(proto);
    cr_proto_put_number(proto, FIRMWARE_VERSION_MICRO);
    cr_proto_put_newline(proto);
}
