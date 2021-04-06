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

#define CMD(x) (CR_PROTO_CMD_ ## x)

struct cr_argument no_arguments[] = {
    { .optional = false, .type = CR_PROTO_ARG_TYPE_VOID }
};

#define COMMAND(_eprefix, _nprefix, _name, _callback, _arguments)       \
    [CMD(_eprefix ## _name)]  = {                                       \
        .id = CMD(_name),                                               \
        .name = (#_nprefix #_name),                                     \
        .cb = _callback,                                                \
        .args = _arguments }

#define CRaCOMMAND(_name, _callback, _arguments)                \
    COMMAND(/*none*/, /*none*/, _name, _callback, _arguments)

#define CRsCOMMAND(_name, _callback)            \
    CRaCOMMAND(_name, _callback, no_arguments)

#define FWaCOMMAND(_name, _callback, _arguments)        \
    COMMAND(FW_, +, _name, _callback, _arguments)

#define FWsCOMMAND(_name, _callback)            \
    FWaCOMMAND(_name, _callback, no_arguments)

#define MISSING_COMMAND(_name)                  \
    [CMD(_name)]  = {                           \
        .id = CMD(_name),                       \
        .name = NULL,                           \
        .cb = NULL,                             \
        .args = NULL }

#define END_OF_COMMAND_TABLE CRaCOMMAND(UNKNOWN, NULL, NULL)

struct cr_command cr_commands[] = {
    CRaCOMMAND(ADDRESS,  cr_handle_address,  address_arguments),
    CRsCOMMAND(BYE,      cr_handle_bye),
    CRsCOMMAND(FEATURES, cr_handle_features),
    MISSING_COMMAND(FOCUS),
    MISSING_COMMAND(HASHED),
    CRsCOMMAND(HI,       cr_handle_hi),
    CRaCOMMAND(INIT,     cr_handle_init,     init_arguments),
    MISSING_COMMAND(LINES),
    MISSING_COMMAND(LINE),
    MISSING_COMMAND(MODES),
    CRsCOMMAND(PORTS,    cr_handle_ports),
    MISSING_COMMAND(PORT),
    CRaCOMMAND(SET,      cr_handle_set,      set_arguments),
    CRaCOMMAND(TRANSMIT, cr_handle_transmit, transmit_arguments),
    CRsCOMMAND(VERSION,  cr_handle_version),
    FWsCOMMAND(VERSION,  cr_handle_fw_version),
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
