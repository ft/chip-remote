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

#define RETURN_TYPE enum cr_proto_result

#define ARGUMENTS (const struct cr_command*,    \
                   const struct cr_value*,      \
                   unsigned int)

#define DECLARE_COMMAND(name) RETURN_TYPE cr_handle_ ## name ARGUMENTS

DECLARE_COMMAND(bye);
DECLARE_COMMAND(hi);

#undef ARGUMENTS
#undef RETURN_TYPE
#undef DECLARE_COMMAND
