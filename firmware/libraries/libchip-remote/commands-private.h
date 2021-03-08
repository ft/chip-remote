/*
 * Copyright (c) 2020 chip-remote workers, All rights reserved.
 *
 * Terms for redistribution and use can be found in LICENCE.
 */

/**
 * @file commands-private.h
 * @brief Function declarations for all implemented RCCEP commands
 */

#include <chip-remote.h>
#include <cr-process.h>

extern struct cr_command cr_commands[];

#define RETURN_TYPE int

#define ARGUMENTS (const struct cr_protocol*,   \
                   const struct cr_command*,    \
                   const struct cr_value*,      \
                   unsigned int)

#define DECLARE_COMMAND(name)                   \
    RETURN_TYPE cr_handle_ ## name ARGUMENTS

#define DECLARE_ARGS_COMMAND(name)                   \
    extern struct cr_argument name ## _arguments[];  \
    DECLARE_COMMAND(name)

DECLARE_COMMAND(bye);
DECLARE_COMMAND(features);
DECLARE_COMMAND(hi);
DECLARE_ARGS_COMMAND(transmit);
DECLARE_COMMAND(version);

#undef DECLARE_ARGS_COMMAND
#undef DECLARE_COMMAND
#undef ARGUMENTS
#undef RETURN_TYPE
