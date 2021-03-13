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

#define RETURN_TYPE void

#define ARGUMENTS (const struct cr_protocol*, const struct cr_proto_parse*)

#define xDECLARE_COMMAND(prefix, name)                  \
    RETURN_TYPE cr_handle_ ## prefix ## name ARGUMENTS

#define xDECLARE_ARGS_COMMAND(prefix, name)                     \
    extern struct cr_argument prefix ## name ## _arguments[];  \
    xDECLARE_COMMAND(prefix, name)

#define DECLARE_COMMAND(name) xDECLARE_COMMAND(/*none*/, name)
#define DECLARE_ARGS_COMMAND(name) xDECLARE_ARGS_COMMAND(/*none*/, name)
#define FW_DECLARE_COMMAND(name) xDECLARE_COMMAND(fw_, name)
#define FW_DECLARE_ARGS_COMMAND(name) xDECLARE_ARGS_COMMAND(fw_, name)

DECLARE_COMMAND(bye);
DECLARE_COMMAND(features);
DECLARE_COMMAND(hi);
DECLARE_ARGS_COMMAND(transmit);
DECLARE_COMMAND(version);
FW_DECLARE_COMMAND(version);

#undef DECLARE_ARGS_COMMAND
#undef DECLARE_COMMAND
#undef ARGUMENTS
#undef RETURN_TYPE
