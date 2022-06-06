/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file commands.c
 * @brief Chip-remote protocol command table and lookup
 */

#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#include <common/compiler.h>

#include <chip-remote.h>
#include <commands.h>
#include <commands-private.h>

#define COMMAND(_id, _eprefix, _nprefix, _name, _callback)  \
    [CR_PROTO_CMD_ ## _eprefix ## _id]  = {                 \
        .id = CR_PROTO_CMD_ ## _eprefix ## _id,             \
        .name = (#_nprefix #_name),                         \
        .cb = _callback }

#define CR_COMMAND(_id, _name, _callback)               \
    COMMAND(_id, /*none*/, /*none*/, _name, _callback)

#define FW_COMMAND(_id, _name, _callback)       \
    COMMAND(_id, FW_, +, _name, _callback)

#define MISSING_COMMAND(_id)                    \
    [CR_PROTO_CMD_ ## _id]  = {                 \
        .id = CR_PROTO_CMD_ ## _id,             \
        .name = NULL,                           \
        .cb = NULL }

#define END_OF_COMMAND_TABLE CR_COMMAND(UNKNOWN, unknown, NULL)

struct cr_command cr_commands[] = {
    CR_COMMAND(BYE,          bye,          cr_handle_bye),
    CR_COMMAND(CAPABILITIES, capabilities, cr_handle_capabilities),
    CR_COMMAND(FOCUS,        focus,        cr_handle_focus),
    CR_COMMAND(HI,           hi,           cr_handle_hi),
    CR_COMMAND(INIT,         init,         cr_handle_init),
    MISSING_COMMAND(LINES),
    MISSING_COMMAND(LINE),
    MISSING_COMMAND(MODES),
    MISSING_COMMAND(MODE),
    CR_COMMAND(PORTS,        ports,        cr_handle_ports),
    MISSING_COMMAND(PORT),
    CR_COMMAND(SET,          set,          cr_handle_set),
    CR_COMMAND(TRANSMIT,     transmit,     cr_handle_transmit),
    CR_COMMAND(VERSION,      version,      cr_handle_version),
    CR_COMMAND(UVERSION,     VERSION,      cr_handle_version),     /* Backward comp. */
    FW_COMMAND(VERSION,      version,      cr_handle_fw_version),
    END_OF_COMMAND_TABLE
};

struct cr_command*
cr_lookup_command(const char *name)
{
    size_t i;
    for (i = 0u; cr_commands[i].id != CR_PROTO_CMD_UNKNOWN; ++i) {
        if (cr_commands[i].name == NULL)
            continue;
        if (strcmp(cr_commands[i].name, name) == 0)
            return cr_commands + i;
    }

    return cr_commands + i;
}
