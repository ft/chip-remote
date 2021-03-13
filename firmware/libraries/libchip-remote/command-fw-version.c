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

#define FIRMWARE_VERSION_MAJOR 0u
#define FIRMWARE_VERSION_MINOR 0u
#define FIRMWARE_VERSION_MICRO 1u

cr_callback_value
cr_handle_fw_version(const struct cr_protocol *proto,
                     UNUSED const struct cr_proto_parse *cmd)
{
    cr_proto_put_u32(proto, FIRMWARE_VERSION_MAJOR);
    cr_proto_put_space(proto);
    cr_proto_put_u32(proto, FIRMWARE_VERSION_MINOR);
    cr_proto_put_space(proto);
    cr_proto_put_u32(proto, FIRMWARE_VERSION_MICRO);
    cr_proto_put_newline(proto);
    return CR_CB_OK;
}
