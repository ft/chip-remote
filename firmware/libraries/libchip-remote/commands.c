/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
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

#define COMMAND(_name, _callback, _arguments)   \
    [CMD(_name)]  = {                                   \
        .id = CMD(_name),                               \
        .name = #_name,                                 \
        .cb = _callback,                                \
        .args = _arguments }

#define MISSING_COMMAND(_name)                  \
    [CMD(_name)]  = {                           \
        .id = CMD(_name),                       \
        .name = NULL,                           \
        .cb = NULL,                             \
        .args = NULL }

#define END_OF_COMMAND_TABLE COMMAND(UNKNOWN, NULL, NULL)

struct cr_command cr_commands[] = {
    MISSING_COMMAND(ADDRESS),
    COMMAND(BYE,      cr_handle_bye,      no_arguments),
    COMMAND(FEATURES, cr_handle_features, no_arguments),
    MISSING_COMMAND(FOCUS),
    MISSING_COMMAND(HASHED),
    COMMAND(HI,       cr_handle_hi,       no_arguments),
    MISSING_COMMAND(INIT),
    MISSING_COMMAND(LINES),
    MISSING_COMMAND(LINE),
    MISSING_COMMAND(MODES),
    MISSING_COMMAND(PORT),
    MISSING_COMMAND(SET),
    COMMAND(TRANSMIT, cr_handle_transmit, transmit_arguments),
    COMMAND(VERSION,  cr_handle_version,  no_arguments),
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
