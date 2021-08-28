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

struct cr_argument no_arguments[] = {
    { .optional = false, .type = CR_PROTO_ARG_TYPE_VOID }
};

#define COMMAND(_id, _eprefix, _nprefix, _name, _callback, _arguments)  \
    [CR_PROTO_CMD_ ## _eprefix ## _id]  = {                             \
        .id = CR_PROTO_CMD_ ## _eprefix ## _id,                         \
        .name = (#_nprefix #_name),                                     \
        .cb = _callback,                                                \
        .args = _arguments }

#define CRaCOMMAND(_id, _name, _callback, _arguments)                   \
    COMMAND(_id, /*none*/, /*none*/, _name, _callback, _arguments)

#define CRsCOMMAND(_id, _name, _callback)               \
    CRaCOMMAND(_id, _name, _callback, no_arguments)

#define FWaCOMMAND(_id, _name, _callback, _arguments)   \
    COMMAND(_id, FW_, +, _name, _callback, _arguments)

#define FWsCOMMAND(_id, _name, _callback)               \
    FWaCOMMAND(_id, _name, _callback, no_arguments)

#define MISSING_COMMAND(_id)                    \
    [CR_PROTO_CMD_ ## _id]  = {                 \
        .id = CR_PROTO_CMD_ ## _id,             \
        .name = NULL,                           \
        .cb = NULL,                             \
        .args = NULL }

#define END_OF_COMMAND_TABLE CRaCOMMAND(UNKNOWN, unknown, NULL, NULL)

struct cr_command cr_commands[] = {
    CRaCOMMAND(ADDRESS,  address,  cr_handle_address,  address_arguments),
    CRsCOMMAND(BYE,      bye,      cr_handle_bye),
    CRsCOMMAND(FEATURES, features, cr_handle_features),
    MISSING_COMMAND(FOCUS),
    CRsCOMMAND(HI,       hi,       cr_handle_hi),
    CRaCOMMAND(INIT,     init,     cr_handle_init,     init_arguments),
    MISSING_COMMAND(LINES),
    MISSING_COMMAND(LINE),
    MISSING_COMMAND(MODES),
    CRsCOMMAND(PORTS,    ports,    cr_handle_ports),
    MISSING_COMMAND(PORT),
    CRaCOMMAND(SET,      set,      cr_handle_set,      set_arguments),
    CRaCOMMAND(TRANSMIT, transmit, cr_handle_transmit, transmit_arguments),
    CRsCOMMAND(VERSION,  version,  cr_handle_version),
    CRsCOMMAND(UVERSION, VERSION,  cr_handle_version), /* Backward compat. */
    FWsCOMMAND(VERSION,  version,  cr_handle_fw_version),
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
