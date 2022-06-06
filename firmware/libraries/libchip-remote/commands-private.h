/*
 * Copyright (c) 2020-2021 chip-remote workers, All rights reserved.
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

#define RETURN_TYPE void

#define ARGUMENTS (struct cr_protocol*,          \
                   struct cr_command*,           \
                   struct cr_value*,             \
                   unsigned int)

#define xDECLARE_COMMAND(prefix, name)                  \
    RETURN_TYPE cr_handle_ ## prefix ## name ARGUMENTS

#define DECLARE_COMMAND(name)    xDECLARE_COMMAND(/*none*/, name)
#define DECLARE_FW_COMMAND(name) xDECLARE_COMMAND(fw_,      name)

DECLARE_COMMAND(address);
DECLARE_COMMAND(bye);
DECLARE_COMMAND(capabilities);
DECLARE_COMMAND(focus);
DECLARE_COMMAND(hi);
DECLARE_COMMAND(init);
DECLARE_COMMAND(ports);
DECLARE_COMMAND(set);
DECLARE_COMMAND(transmit);
DECLARE_COMMAND(version);
DECLARE_FW_COMMAND(version);

#undef DECLARE_FW_COMMAND
#undef DECLARE_COMMAND
#undef xDECLARE_COMMAND
#undef ARGUMENTS
#undef RETURN_TYPE
